import sys
from datetime import datetime
from dateutil import relativedelta
from pytz import timezone
import calendar
import numpy as np
from collections import defaultdict
from scipy import stats
import pymongo
from multiprocessing import Process, JoinableQueue, Manager, Pool
import tools
import random
import re
sys.path.append("../lib/ip2asn/")

import ip2asn


asn_regex = re.compile("^AS([0-9]*)\s(.*)$")

db = None

def processInit():
    global db
    client = pymongo.MongoClient("localhost",27017,connect=True)
    db = client.atlas


def readOneTraceroute(trace, diffRtt, metric=np.nanmedian):
    """
    Read a single traceroute instance and compute the corresponding differential RTTs.
    """

    if trace is None or "error" in trace["result"][0] or "err" in trace["result"][0]["result"]:
        return diffRtt

    probeIp = trace["from"]
    probeId = None
    msmId = None
    if "prb_id" in trace:
        probeId = trace["prb_id"]
    if "msm_id" in trace:
        msmId = trace["msm_id"]
    prevRttMed = {}

    for hopNb, hop in enumerate(trace["result"]):
        try:
            # TODO: clean that workaround results containing no IP, e.g.:
            # {u'result': [{u'x': u'*'}, {u'x': u'*'}, {u'x': u'*'}], u'hop': 6}, 

            if "result" in hop :

                rttList = defaultdict(list) 
                rttMed = {}

                for res in hop["result"]:
                    if not "from" in res  or tools.isPrivateIP(res["from"]) or not "rtt" in res or res["rtt"] <= 0.0:
                        continue

                    # assert hopNb+1==hop["hop"] or hop["hop"]==255 

                    rttList[res["from"]].append(res["rtt"])

                for ip2, rtts in rttList.iteritems():
                    rttAgg = np.median(rtts)
                    rttMed[ip2] = rttAgg

                    # Differential rtt
                    if len(prevRttMed):
                        for ip1, pRttAgg in prevRttMed.iteritems():
                            if ip1 == ip2 :
                                continue

                            # data for (ip1, ip2) and (ip2, ip1) are put
                            # together in mergeRttResults
                            if not (ip1, ip2) in diffRtt:
                                diffRtt[(ip1,ip2)] = {"rtt": [], "probe": [], "msmId": defaultdict(set)}

                            i = diffRtt[(ip1,ip2)]
                            i["rtt"].append(rttAgg-pRttAgg)
                            i["probe"].append(probeIp)
                            i["msmId"][msmId].add(probeId)
        finally:
            prevRttMed = rttMed
            # TODO we miss 2 inferred links if a router never replies
    return diffRtt


def computeRtt( (af, start, end, skip, limit) ):
    """
    Read traceroutes from a cursor. Used for multi-processing.

    Assume start and end are less than 24h apart
    """
    s = datetime.utcfromtimestamp(start)
    e = datetime.utcfromtimestamp(end)
    collectionNames = set(["traceroute%s_%s_%02d_%02d" % (af, d.year, d.month, d.day) for d in [s,e]])


    nbRow = 0
    diffRtt = defaultdict(dict)
    for col in collectionNames:
        collection = db[col]

        cursor = collection.find( { "timestamp": {"$gte": start, "$lt": end}},
                projection={"result":1, "from":1, "prb_id":1, "msm_id":1} ,
                skip = skip,
                limit = limit,
                #no_cursor_timeout=True,
                #cursor_type=pymongo.cursor.CursorType.EXHAUST)
                batch_size=int(10e6))
        for trace in cursor: 
            readOneTraceroute(trace, diffRtt)
            nbRow += 1
        cursor.close()
    return diffRtt, nbRow


def mergeRttResults(rttResults, currDate, nbBins):

        diffRtt = defaultdict(dict)
        nbRow = 0 
        for i, (iRtt, compRows) in enumerate(rttResults):
            if compRows==0:
                continue

            if not iRtt is None:
                for k, v in iRtt.iteritems():

                    # put together data for (ip1, ip2) and (ip2, ip1)
                    ipPair = tuple(sorted(k))
                    if ipPair in diffRtt:
                        inf = diffRtt[ipPair]
                        inf["rtt"].extend(v["rtt"])
                        inf["probe"].extend(v["probe"])
                        for msmId, probes in v["msmId"].iteritems():
                            inf["msmId"][msmId].update(probes)
                    else:
                        diffRtt[ipPair] = v

            nbRow += compRows
        return diffRtt, nbRow


def rttRevise(sampleDistributions, param, ts, probe2asn, i2a, collection=None, probeip2asn=None):

    if sampleDistributions is None:
        return
    ipPairtts = []
    minAsn = param["minASN"]
    minASNEntropy = param["minASNEntropy"]

    for ipPair, data in sampleDistributions.iteritems():

        dist = np.array(data["rtt"])
        probes = np.array(data["probe"])
        mask = np.array([True]*len(dist))
        asn = defaultdict(int)
        asnProbeIdx = defaultdict(list)

        for idx, ip in enumerate(data["probe"]):
            if not ip in probe2asn:
                a = i2a.ip2asn(ip)
                probe2asn[ip] = a 
            else:
                a = probe2asn[ip]
            asn[a] += 1
            asnProbeIdx[a].append(idx)

        if len(asn)==1:
            asnEntropy = 0
        else:
            asnEntropy = stats.entropy(asn.values())/np.log(len(asn))
        trimDist = False
       
        # trim the distribution if needed
        while asnEntropy < minASNEntropy and len(asn) > minAsn:

            #remove one sample from the most prominent AS
            maxAsn = max(asn, key=asn.get)
            remove = random.randrange(0,len(asnProbeIdx[maxAsn]))
            rmIdx = asnProbeIdx[maxAsn].pop(remove)
            mask[rmIdx] = False
            asn[maxAsn] -= 1
            #remove the AS if we removed all its probes
            if asn[maxAsn] <= 0:
                asn.pop(maxAsn, None)
        
            # recompute the entropy
            asnEntropy = stats.entropy(asn.values())/np.log(len(asn))
            trimDist = True 

        # Compute the distribution median
        if len(asn) < minAsn or asnEntropy < minASNEntropy:
            continue

        # if trimmed then update the sample dist and probes
        if trimDist:
            dist = dist[mask]
            probes = probes[mask]

        nbProbes = len(probes)
        n = len(dist) 
        med = np.median(dist)

        nosetMsmId = {str(k): list(v) for k, v in data["msmId"].iteritems()}
        '''
        ipPairtt = {"timeBin": ts, "ipPair": ipPair, "median": med, "nbSamples": n, "nbProbes": nbProbes, "expId": expId,
                    "samplePerASN": list(asn), "nbASN": len(asn), "asnEntropy": asnEntropy, "msmId": nosetMsmId}
        '''
        ipPairtt = {"timeBin": ts, "ipPair": ipPair, "median": med}
        if not collection is None:
            ipPairtts.append(ipPairtt)

    # Insert all ipPairtts to the database
    if len(ipPairtts) and not collection is None:
        collection.insert_many(ipPairtts)

    return

def ipPairAgg(start,end,af,duration):
    #At least 32 probe results are returned per day on 48 probe times
    num = 32*duration
    client = pymongo.MongoClient("localhost", 27017)
    db = client.atlas
    rttCollection = "rttCollection%s_%s%02d%02d_%s%02d%02d" % (af, start.year, start.month, start.day, end.year, end.month, end.day)
    rttCollection = db[rttCollection]


    ipPairtts = "ipPairtts%s_%s%02d%02d_%s%02d%02d" % (af, start.year, start.month, start.day, end.year, end.month, end.day)
    ipPairtts = db[ipPairtts]

    pipline = [{ "$group" : { "_id" : "$ipPair","count":{"$sum":1},"rtt": { "$push" : "$median" },"timebin":{"$push" :"$timeBin"}}},
               {"$match" : {"count" : {"$gt":num}}}]
    aggresults = rttCollection.aggregate(pipline)
    results = []
    for d in list(aggresults):
        d.update(ipPairs=d.pop('_id'))
        results.append(d)
    ipPairtts.insert_many(results)


def dataCleansing(start,end,af,i2aIndex):

    nbProcesses = 12
    binMult = 3 # number of bins = binMult*nbProcesses 
    pool = Pool(nbProcesses,initializer=processInit)
    client = pymongo.MongoClient("localhost",27017)
    db = client.atlas
    #detectionExperiments = db.rttExperiments
    collection = "rttCollection%s_%s%02d%02d_%s%02d%02d" % (af,start.year,start.month,start.day,end.year,end.month,end.day)
    rttCollection = db[collection]

    expParam = {
                "timeWindow": 30*60, # time bin per half hour
                "start": start,
                "end":   end,
                "minASN": 3,
                "minASNEntropy": 0.5,
                "experimentDate": datetime.now(),
                "af": af
                }

    #expId = detectionExperiments.insert_one(expParam).inserted_id

    probe2asn = {}
    probeip2asn = {}
    i2a = ip2asn.ip2asn("../lib/ip2asn/db/rib."+i2aIndex+".pickle")
    start = int(calendar.timegm(expParam["start"].timetuple()))
    end = int(calendar.timegm(expParam["end"].timetuple()))

    for currDate in range(start,end,int(expParam["timeWindow"])):
        sys.stdout.write("Rtt analysis %s\n" % datetime.utcfromtimestamp(currDate))

        # Get distributions for the current time bin
        c = datetime.utcfromtimestamp(currDate)
        col = "traceroute%s_%s_%02d_%02d" % (expParam["af"], c.year, c.month, c.day)
        totalRows = db[col].count({ "timestamp": {"$gte": currDate, "$lt": currDate+expParam["timeWindow"]}})
        if not totalRows:
            print "\nNo data for that time bin!"
            continue
        params = []
        limit = int(totalRows/(nbProcesses*binMult-1))
        if not limit:
            print "\nToo few data packets!"
            continue
        skip = range(0, totalRows, limit)
        for i, val in enumerate(skip):
            params.append( (expParam["af"], currDate, currDate+expParam["timeWindow"], val, limit) )

        #diffRtt = defaultdict(dict)
        #nbRow = 0
        rttResults = pool.imap_unordered(computeRtt, params)
        diffRtt, nbRow = mergeRttResults(rttResults, currDate, nbProcesses*binMult)

        # rtt Revise
        rttRevise(diffRtt, expParam, datetime.utcfromtimestamp(currDate), probe2asn, i2a, rttCollection, probeip2asn)
    
    pool.close()
    pool.join()

if __name__ == "__main__":

    if len(sys.argv) < 7:
        print "usage: %s year month day af duration ip2asn(date)" % sys.argv[0]
        sys.exit()

    start = datetime(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]),tzinfo=timezone("UTC"))
    end = start + relativedelta.relativedelta(days=int(sys.argv[5]))
    af = sys.argv[4]
    duration = int(sys.argv[5])
    i2aIndex = sys.argv[6]

    dataCleansing(start,end,af,i2aIndex)
    ipPairAgg(start,end,af,duration)