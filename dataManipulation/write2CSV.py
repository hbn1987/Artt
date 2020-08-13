import missvalueHandling as hm
import os
import pymongo
import sys
from datetime import datetime
from dateutil import relativedelta
import pandas as pd

def getts(id):
    client = pymongo.MongoClient("localhost", 27017)
    db = client.atlas
    ipPairtts = "ipPairtts"+id
    ipPairtts = db[ipPairtts]
    results = ipPairtts.find({})
    return results

def write2CSV(start, end, day, af, rtt_alyz_dir="../data/rtt_analysis/"):

    id = "%s_%s%02d%02d_%s%02d%02d" % (af, start.year, start.month, start.day, end.year, end.month, end.day)
    origid = id
    # Skip if already done
    changeid="changepoint"+id+".json"
    if os.path.exists(os.path.join(rtt_alyz_dir, changeid)):
        print "%r already treated, thus skipped." % changeid
        return

    mes = getts(id)
    outputlist = list()
    outputcols = list()
    count = 0
    char = ' - '
    for me in mes:

        # Missing value handing
        d = {"data": me["rtt"],
             "date": me["timebin"]}
        ts = pd.DataFrame(d)
        hts = hm.data_Full(start,ts,day)

        rtt_mes = hts["data"].tolist()

        #Time series data output

        if count == 0:
            outputlist.append(hts["date"].tolist())
            outputcols.append("date")
        else:
            outputlist.append(rtt_mes)
            outputcols.append(char.join(me["ipPairs"]))
        count += 1
    dics = dict(zip(outputcols, outputlist))
    df = pd.DataFrame(dics)
    path = "../data/TSdata"
    df.to_csv(path + "%s.csv" % (origid))


if __name__ == '__main__':
    if len(sys.argv) < 6:
        print "usage: %s year month day af duration" % sys.argv[0]
        sys.exit()

    start = datetime(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]))
    day = int(sys.argv[5])
    end = start + relativedelta.relativedelta(days=day)
    af = sys.argv[4]

    write2CSV(start, end, day, af)
