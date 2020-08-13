import pandas as pd
from datetime import datetime
from datetime import timedelta


def datetime_toString(dt):
    return dt.strftime("%Y-%m-%d %H:%M:%S")


def string_toDatetime(string):
    return datetime.strptime(string, "%Y-%m-%d %H:%M:%S")


def data_Full(start, ts, day):
    date = ts['date'].tolist()
    data = ts['data'].tolist()
    new = max(data)*3#Insert 3 * maximum
    tshanded = ts
    act = 48*day
    date0 = start
    date_s = datetime_toString(date0)

    #Adding head and middle missing values
    for j in range(0, len(date)-1):
        if len(date) < act:
            date_is = datetime_toString(date[j])
            while date_is != date_s:
                nada = new
                adda = [nada, date0]
                date_da = pd.DataFrame(adda).T
                date_da.columns = tshanded.columns
                tshanded = pd.concat([tshanded, date_da])
                date0 += timedelta(minutes=30)
                date_s = datetime_toString(date0)
            date0 += timedelta(minutes=30)
            date_s = datetime_toString(date0)

    #Adding tail missing values
    while len(tshanded['date']) < act:
        taildate = tshanded['date'].tolist()[-1]
        date0 = taildate+timedelta(minutes=30)
        nada = new
        adda = [nada, date0]
        date_da = pd.DataFrame(adda).T
        date_da.columns = tshanded.columns
        tshanded = pd.concat([tshanded, date_da])

    tshanded = tshanded.sort_values(by=['date'])
    tshanded.index = range(len(tshanded))
    return tshanded

if __name__ == '__main__':
    date = ["2015-11-29 01:00:00", "2015-11-29 01:30:00", "2015-11-29 02:00:00", "2015-11-29 02:30:00", "2015-11-29 03:00:00", "2015-11-29 03:30:00", "2015-11-29 04:00:00", "2015-11-29 05:00:00", "2015-11-29 05:30:00", "2015-11-29 06:00:00", "2015-11-29 06:30:00", "2015-11-29 07:00:00", "2015-11-29 07:30:00", "2015-11-29 08:00:00", "2015-11-29 08:30:00", "2015-11-29 09:00:00", "2015-11-29 09:30:00", "2015-11-29 10:00:00", "2015-11-29 10:30:00", "2015-11-29 11:00:00", "2015-11-29 11:30:00", "2015-11-29 12:00:00", "2015-11-29 13:00:00", "2015-11-29 13:30:00", "2015-11-29 14:00:00", "2015-11-29 14:30:00", "2015-11-29 15:00:00", "2015-11-29 15:30:00", "2015-11-29 16:00:00", "2015-11-29 16:30:00", "2015-11-29 17:00:00", "2015-11-29 17:30:00", "2015-11-29 18:00:00", "2015-11-29 18:30:00", "2015-11-29 19:00:00", "2015-11-29 19:30:00", "2015-11-29 20:00:00", "2015-11-29 21:00:00", "2015-11-29 21:30:00", "2015-11-29 22:00:00", "2015-11-29 22:30:00", "2015-11-29 23:00:00", "2015-11-29 23:30:00", "2015-11-30 00:00:00", "2015-11-30 00:30:00", "2015-11-30 01:00:00", "2015-11-30 01:30:00", "2015-11-30 02:00:00", "2015-11-30 02:30:00", "2015-11-30 03:00:00", "2015-11-30 03:30:00", "2015-11-30 04:00:00", "2015-11-30 05:00:00", "2015-11-30 05:30:00"]
    data = [15.405999999999995, 15.465999999999998, 15.983, 15.621999999999996, 15.934000000000001, 15.653, 15.4655, 15.469, 15.691, 15.549000000000001, 15.862999999999996, 15.604000000000003, 15.6235, 14.916, 14.942999999999998, 16.801000000000002, 16.064, 15.655000000000001, 15.626999999999999, 15.486999999999998, 15.490999999999998, 15.696500000000004, 15.531000000000002, 15.836, 16.044, 15.780000000000001, 15.5135, 15.543000000000003, 14.472999999999999, 15.442, 15.550999999999998, 15.718, 16.333, 15.361, 15.458000000000002, 15.779, 15.795999999999996, 15.316999999999997, 16.003, 15.995, 15.527999999999999, 15.797500000000001, 16.002, 16.003999999999998, 15.661999999999999, 15.861999999999998, 15.61, 15.839500000000003, 15.320999999999998, 15.254999999999999, 15.899999999999999, 15.051000000000002, 16.025000000000002, 16.032]
    d = {"data": data,
         "date": date}
    ts = pd.DataFrame(d)
    start = "2015-11-29 00:00:00"
    day = 2
    print data_Full(start,ts,day)

