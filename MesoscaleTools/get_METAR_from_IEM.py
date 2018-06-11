import requests
import datetime
import re
import numpy as np
import pandas as pd
import sys
from metar import Metar
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO




class Parser(object):
    STATION_RE = r"^[A-Z]{4}$"
    URLTEMPLATE="https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station={station}&data=all&year1={year_start}&month1={month_start}&day1={day_start}&year2={year_end}&month2={month_end}&day2={day_end}&tz=Etc%2FUTC&format=onlycomma&latlon=yes&direct=no&report_type=1&report_type=2"
    def __init__(self, station, date_start, date_end):
        self.station = station
        if not re.search(self.STATION_RE, self.station):
            raise ValueError("Please use a proper station...")
        self.date_start = date_start
        self.date_end = date_end
        if not isinstance(self.date_start, datetime.datetime) or not isinstance(self.date_end, datetime.datetime):
            raise ValueError("Please use Datetime objects to initialize the Parser")
        self._year_start = self.date_start.year
        self._month_start = self.date_start.month
        self._day_start = self.date_start.day
        self._year_end = self.date_end.year
        self._month_end = self.date_end.month
        self._day_end = self.date_end.day
        self._url = self.URLTEMPLATE.format(station=self.station, year_start=self._year_start, month_start=self._month_start, day_start = self._day_start,
            year_end = self._year_end, month_end=self._month_end, day_end = self._day_end)
        print(self._url)
    
    def parse(self):
        reply = requests.get(self._url)
        if not reply.status_code == 200:
            raise ValueError("Somethings wrong with your URL....")
        self.text = StringIO(reply.text)
        line = str.splitlines(str(reply.text))
        if len(line) == 1:
            raise ValueError("No Data available with this parametrization")
        df = pd.read_csv(self.text,delimiter=",", header=0)
        df.rename(columns=lambda x: x.strip(), inplace=True)
        df = self.cleanup_dataframe(df)
        return df

    @staticmethod
    def kn2ms(val):
        return val * 463/900.0
    
    @staticmethod
    def mi2me(val):
        return val * 1609.344
    
    @staticmethod
    def f2m(val):
        return val * 0.3048
    
    @staticmethod
    def f2c(val):
        return 5/ 9.0 * (val - 32)
    
    @staticmethod
    def in2mm(val):
        return val * 25.4
    


    def cleanup_dataframe(self, df):
        df.replace("M", np.nan, inplace=True)
        df.valid = pd.to_datetime(df["valid"])
        df.index = df["valid"]
        df["tmpc"] = Parser.f2c(df["tmpf"])
        df["dwpc"] = Parser.f2c(df["dwpf"])
        df["sms"] = Parser.kn2ms(df["sknt"])
        df["vs"] = Parser.mi2me(df["vsby"])
        df["p01imm"] = Parser.in2mm(df["p01i"])
        df["skyl1"] = Parser.f2m(pd.to_numeric(df["skyl1"]))
        df["skyl2"] = Parser.f2m(pd.to_numeric(df["skyl2"]))
        df["skyl3"] = Parser.f2m(pd.to_numeric(df["skyl3"]))
        df["skyl4"] = Parser.f2m(pd.to_numeric(df["skyl4"]))
        df.drop(["station", "valid", "gust"],inplace=True, axis=1)
        df = extract_cloud_layers(df)
        return df


def extract_cloud_layers(df):
    FEW = 2/8.0
    SCT = 4/8.0
    BKN = 6/8.0
    OVC = 1
    df["cloudiness"] = 0
    df["significant_level"] = 0
    for i in range(4):
        cld = df["cloudiness"].values
        siglev = df["significant_level"].values
        activelev = df["skyl{}".format(i+1)]
        cld = np.where((df["skyc{}".format(i+1)] == "FEW") & (cld <= FEW), FEW, cld)
        siglev = np.where((df["skyc{}".format(i+1)] == "FEW") & (cld <= FEW), activelev, siglev)
        cld = np.where((df["skyc{}".format(i+1)] == "SCT") & (cld <= SCT), SCT, cld)
        siglev = np.where((df["skyc{}".format(i+1)] == "SCT") & (cld <= SCT), activelev, siglev)
        cld = np.where((df["skyc{}".format(i+1)] == "BKN") & (cld <= BKN), BKN, cld)
        siglev = np.where((df["skyc{}".format(i+1)] == "BKN") & (cld <= BKN), activelev, siglev)
        cld = np.where((df["skyc{}".format(i+1)] == "OVC") & (cld <= OVC), OVC, cld)
        siglev = np.where((df["skyc{}".format(i+1)] == "OVC") & (cld <= OVC), activelev, siglev)
        df["cloudiness"] = cld
        df["significant_level"]  = siglev
    return df


def extract_cloud_layers_old(df):
    df["cloudiness"] = 0
    for i in range(4):
        cld = df["cloudiness"].values
        cld = np.where(df["skyc{}".format(i+1)] == "FEW", cld+((1/(i+1)) * (2/8.0)), cld)
        cld = np.where(df["skyc{}".format(i+1)] == "SCT", cld+((1/(i+1)) *(4/8.0)), cld)
        cld = np.where(df["skyc{}".format(i+1)] == "BKN", cld+((1/(i+1)) *(6/8.0)), cld)
        cld = np.where(df["skyc{}".format(i+1)] == "OVC", cld+(1/(i+1)), cld)
        df["cloudiness"] = cld
    df["cloudiness"] /= (1 + 1/2.0 + 1/3.0 + 1/4.0)
    return df        
    


#p = Parser("XXXX", datetime.datetime(2012,2,2,1,1), datetime.datetime(2012,2,3,1,1))
#p = Parser("WSSS", datetime.datetime(2012,2,2,1,1), datetime.datetime(2012,2,3,1,1))
#res = p.parse()
#print(res["skyc1"], res["skyc2"], res["skyc3"], res["skyc4"])
#print(res.cloudiness)

#print(p.text)

# p = Parser("NSIA",  "KJhsakdjfhsdf", datetime.datetime(2012,2,2,1,1),)