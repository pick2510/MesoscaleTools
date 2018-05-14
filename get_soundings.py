import requests
from fortranformat import FortranRecordReader
import datetime
import re
import pandas as pd
import sys


class Sounding(object):
    COLUMNS = ["TYPE", "PRESSURE", "HEIGHT",
               "TEMP", "DEWPT", "WINDDIR", "WINDSPD"]

    def __init__(self, **kwds):
        self.__dict__.update(kwds)


class Parser(object):
    URLTEMPLATE = "https://ruc.noaa.gov/raobs/GetRaobs.cgi?shour=All+Times&ltype=All+Levels&wunits=Knots&bdate={bdate}&edate={edate}&access=WMO+Station+Identifier&view=NO&StationIDs={stationID}&osort=Station+Series+Sort&oformat=FSL+format+(ASCII+text)"
    HEADER = FortranRecordReader("(3i7,6x,a4,i7)")
    IDENT = FortranRecordReader("(3i7,f7.2,a1,f6.2,a1,i6,i7)")
    IDENT2 = FortranRecordReader("(i7,10x,a4,14x,i7,5x,a2)")
    VALS = FortranRecordReader("(7i7)")
    LINEIDENT = FortranRecordReader("(i7)")
    SPLIT_REGEX = re.compile("^    254", re.M)

    def __init__(self, bdate, edate, stationID):
        self.bdate = bdate
        self.edate = edate
        self.stationID = stationID
        self.url = self.URLTEMPLATE.format(
            bdate=self.bdate, edate=self.edate, stationID=self.stationID)
        self.soundings = []

    def parse(self):
        req = requests.get(self.url)
        if "Sorry" in req.text or "ERROR" in req.text:
            print("ERROR, Something is wrong in your request")
            print(req.text)
            return
        content  = ["    254" + x for x in re.split(self.SPLIT_REGEX, req.text)][1:]
        #content = ["    254" + x for x in req.text.split("    254")][1:]
        for snd in content:
            snd_obj = Sounding()
            data = []
            for line in snd.splitlines():
                linecode = self.LINEIDENT.read(line)
                if linecode[0] == 254:
                    header = self.HEADER.read(line)
                    try:
                        date = datetime.datetime.strptime("{}-{}-{} {}:00:00".format(
                            header[4], header[3].strip(), header[2], header[1]), "%Y-%b-%d %H:%M:%S")
                    except ValueError:
                        print("Something has gone wrong with datetime parsing....")
                    snd_obj.datetime = date
                elif linecode[0] == 1:
                    ident = self.IDENT.read(line)
                    snd_obj.wban, snd_obj.wmo, snd_obj.lat, snd_obj.lat_dir, snd_obj.lon, snd_obj.lon_dir, snd_obj_elev, snd_obj.rtime = ident[
                        1], ident[2], ident[3], ident[4], ident[5], ident[6], ident[7], ident[8]
                elif linecode[0] == 2:
                    checks = self.VALS.read(line)
                    snd_obj.hydro, snd_obj.mxwd, snd_obj.tropl, snd_obj.lines, snd_obj.tindex, snd_obj.source = checks[
                        1], checks[2], checks[3], checks[4], checks[5], checks[6]
                elif linecode[0] == 3:
                    ident2 = self.IDENT2.read(line)
                    snd_obj.staid, snd_obj.sonde, snd_obj.wsunits = ident2[1], ident2[2], ident2[3]
                elif 4 <= linecode[0] <= 9:
                    vals = self.VALS.read(line)
                    data.append(vals)
                else:
                    raise ValueError
            snd_obj.data = pd.DataFrame(data=data, columns=Sounding.COLUMNS)
            print(snd_obj.data)
            self.soundings.append(snd_obj)


"""
res = URLTEMPLATE.format(bdate="2002060600", edate="2002060712", stationID="10868")
req = requests.get(res)


sounding_dic = {} 

for snd in content:
    df = pd.DataFrame()
    for line in snd.splitlines():
       currentline = lineident.read(line)
"""

p = Parser("2002060600", "2002070810", "10868")
print(p.url)
p.parse()
snd = p.soundings

print(snd)
