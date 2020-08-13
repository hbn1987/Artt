import sys
import os 
from datetime import datetime
from datetime import timedelta
from dateutil import relativedelta
from ripe.atlas.cousteau import AtlasResultsRequest 
import json
import pymongo
import gzip
import gc


def downloadData(start, end, msmTypes = ["builtin", "anchor"], afs = [4, 6], reverse=False, split=0, timeWindow = timedelta(minutes=3*60) ):
    errors = []

    storage = "mongo"
    if storage == "mongo":
        client = pymongo.MongoClient("localhost",27017)
        db = client.atlas

    # Get measurments results
    for af in afs:
        for msmType in msmTypes:
            # Read the corresponding id file
            if reverse:
                msmIds = reversed(open("./%sMsmIdsv%s.txt" % (msmType, af), "r").readlines())
            else:
                msmIds = open("./%sMsmIdsv%s.txt" % (msmType, af), "r").readlines()

            msmIds = msmIds[int(len(msmIds)*split):]

            for line in msmIds:
                msmId = int(line.partition(":")[2])

                currDate = start
                while currDate+timeWindow<=end:
                    path = "../data/ipv%s/%s/%s/%s" % (af,msmType,currDate.year, currDate.month)
                    try:
                        print("%s:  measurement id %s" % (currDate, msmId) )
                        if not os.path.exists(path):
                            os.makedirs(path)
                        if os.path.exists("%s/%s_msmId%s.json.gz" % (path, currDate.strftime(u'%Y-%m-%d-%H'), msmId)):
                            continue

                        kwargs = {
                            "msm_id": msmId,
                            "start": currDate,
                            "stop": currDate+timeWindow,
                            # "probe_ids": [1,2,3,4]
                        }

                        is_success, results = AtlasResultsRequest(**kwargs).create()
                        if is_success :
                                # results = list(results)
                            # else:
                                # Output file
                                fi = gzip.open("%s/%s_msmId%s.json.gz" % (path, currDate.strftime(u'%Y-%m-%d-%H'), msmId) ,"wb")
                                print("Storing data for %s measurement id %s" % (currDate.strftime(u'%Y-%m-%d-%H'), msmId) )
                                json.dump(results, fi)
                                fi.close()

                            # if storage == "mongo":
                                if len(results)==0:
                                    continue
                                print("Sending data to Mongodb server")
                                if af == 4:
                                    #TODO store data in traceroute4
                                    collection = "traceroute4_%s_%02d_%02d" % (currDate.year,
                                                                currDate.month, currDate.day)
                                elif af == 6:
                                    collection = "traceroute6_%s_%02d_%02d" % (currDate.year, 
                                                                currDate.month, currDate.day)
                                else:
                                    None

                                col = db[collection]
                                col.insert_many(results)
                                #col.create_index("timestamp", background=True)

                                #print 'gc.collect\n'
                                del col
                                del is_success
                                del results
                                gc.collect()

                        else:
                            print "fetch results not success: %s: msmId=%s" % (currDate, msmId)

                    except ValueError:
                        errors.append("%s: msmId=%s" % (currDate, msmId))
                        print "error: %s: msmId=%s" % (currDate, msmId)

                    finally:
                        currDate += timeWindow


    if errors:
        print("Errors with the following parameters:")
        print(errors)


if __name__ == "__main__":

    if len(sys.argv) < 7:
        print "usage: %s year month day (builtin, anchor) af duration" % sys.argv[0]
        sys.exit()

    start = datetime(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]))

    end = start + relativedelta.relativedelta(days=int(sys.argv[6]))

    downloadData(start, end, [sys.argv[4]], [int(sys.argv[5])])