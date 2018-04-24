#!/usr/bin/env python


from multiprocessing import Pool
try:
    from gribapi import *
    GRIBAPI = True
except ImportError:
    from eccodes import *
    GRIBAPI = False

import logging
import argparse
import sys
import datetime
import os


from ecmwfapi import ECMWFDataServer

logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)


grid = "5/100/-2/108"
res = "0.25/0.25"
date = "2014-02-01/to/2014-02-28"
sfc_params = "039/040/041/042/043/129/134/139/141/170/172/183/198/235/236/238"
ml_params = "075/076/130/131/132/133/152/203.200/246/247"
pl_params = "129"
mod_levs = "all"
sfc_file = "ERA5_cosmo_sfc.grb"
ml_file = "ERA5_cosmo_ml.grb"
pl_file = "ERA5_cosmo_pl.grb"
out_file = "ERA5_cosmo_merged.grb"


infile_list = [sfc_file, ml_file, pl_file]

sfc_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "sfc",
        'param': sfc_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': sfc_file
}
pl_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "pl",
        'param': pl_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': pl_file
}
ml_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "ml",
        'levelist': mod_levs,
        'param': ml_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': ml_file
}


dic_list = [sfc_dic, ml_dic, pl_dic]


def setupArgParser():
    parser = argparse.ArgumentParser(description="Fetch ERA5 gribs from ECMWF")
    parser.add_argument(
            '-startdate', type=str,
            help="Enter Startdate like '20140201'", required=True)
    parser.add_argument('-enddate', type=str,
                        help="Enter Enddate like '20140230'", required=True)
    parser.add_argument('-grid', type=str, required=True,
                        help="Please enter grid in the form 'N/W/S/E'")
    parser.add_argument(
            '-res',
            type=str,
            help="Please enter resolution in the dorm 'dx/dy'",
            default="0.25/0.25)")
    parser.add_argument(
            '-interval',
            type=int,
            choices=[
                    1,
                    3,
                    6],
            default=3,
            help="Enter analysis interval between forecast/analysis in hours")
    return parser


def parseArgs(parser):
    return parser.parse_args()


def setArguments(timesteps, args):
    for dic in dic_list:
        dic['time'] = timesteps
        dic['area'] = args.grid
        dic['grid'] = args.res


def selectInterval(args):
    if args.interval == 1:
        timesteps = "00:00:00/01:00:00/02:00:00/03:00:00/04:00:00/05:00:00/" \
                "06:00:00/07:00:00/08:00:00/09:00:00/10:00:00/11:00:00/12:00:00/13:00:00/" \
                "14:00:00/15:00:00/16:00:00/17:00:00/18:00:00/19:00:00/20:00:00/" \
                "21:00:00/22:00:00/23:00:00"
    elif args.interval == 3:
        timesteps = "00:00:00/03:00:00/06:00:00/09:00:00/12:00:00/15:00:00/" \
                "18:00:00/21:00:00"
    else:
        timesteps = "00:00:00/06:00:00/12:00:00/18:00:00/21:00:00"
        return timesteps


def sanityCheck(args):
    try:
        start = datetime.datetime.strptime(args.startdate, "%Y%m%d")
    except ValueError:
        logging.error(
                "ERROR: Wrongly formatted Startdate: {}".format(
                        args.startdate))
        sys.exit(-1)
        try:
            end = datetime.datetime.strptime(args.enddate, "%Y%m%d")
        except ValueError:
            logging.error(
                    "ERROR: Wrongly formatted Enddate: {}".format(
                            args.enddate))
            delta = end - start
            if delta < datetime.timedelta(0):
                logging.error("ERROR: Enddate earlier than Startdate: {} -> {}".format(
                        args.startdate, args.enddate))
                sys.exit(-1)
                if end.month != start.month:
                    logging.error(
                            "ERROR: Only dates in the same month are yet supported at the moment.")
                    logging.error("You can execute the script multiple times")
                    sys.exit(-1)


def catBinaryOutput(outfile, infiles):
    BLOCKSIZE = 4096
    BLOCKS = 1024
    CHUNK = BLOCKS * BLOCKSIZE
    with open(outfile, "wb") as out:
        for fname in infiles:
            with open(fname, "rb") as inf:
                while True:
                    read_bytes = inf.read(CHUNK)
                    if not read_bytes:
                        break
                    out.write(read_bytes)


def fetchECMWF(dic):
    server = ECMWFDataServer()
    logging.info("MARS Request: {}".format(dic))
    try:
        server.retrieve(dic)
    except BaseException:
        logging.error(
            "ERROR: Something of your request is not working, either "
            "ecmwf or in the request itself")
        sys.exit(-1)


def splitGRIBSgribapi(ifile):
    index_keys = ["dataDate", "dataTime"]
    logging.info("Creating index for grib file")
    iid = grib_index_new_from_file(ifile, index_keys)
    date_vals, time_vals = grib_index_get(
            iid, "dataDate"), grib_index_get(
                    iid, "dataTime")
    logging.info("Splitting grib")
    for date in date_vals:
        grib_index_select(iid, index_keys[0], date)
        for time in time_vals:
            grib_index_select(iid, index_keys[1], time)
            if time == "0":
                time = "00"
            else:
                time = "{:02}".format(int(time)/100)
            with open("eas{}{}".format(date, time), "ab") as out:
                while True:
                    gid = grib_new_from_index(iid)
                    if gid is None:
                        break
                    grib_write(gid, out)
                    grib_release(gid)


def splitGRIBSeccodes(ifile):
    index_keys = ["dataDate", "dataTime"]
    logging.info("Creating index for grib file")
    iid = codes_index_new_from_file(ifile, index_keys)
    date_vals, time_vals = codes_index_get(
            iid, "dataDate"), codes_index_get(
                    iid, "dataTime")
    logging.info("Splitting grib")
    for date in date_vals:
        codes_index_select(iid, index_keys[0], date)
        for time in time_vals:
            codes_index_select(iid, index_keys[1], time)
            if time == "0":
                time = "00"
            else:
                time = "{:02}".format(int(time)/100)
            with open("eas{}{}".format(date, time), "ab") as out:
                while True:
                    gid = codes_new_from_index(iid)
                    if gid is None:
                        break
                    codes_write(gid, out)
                    codes_release(gid)


def cleanup(files):
    for f in files:
        if os.path.isfile(f):
            os.remove(r)


if __name__ == "__main__":
    parser = setupArgParser()
    args = parseArgs(parser)
    logging.info("******************************************")
    logging.info(" ERA5 for mesoscale simulations fetcher   ")
    logging.info("******************************************")
    logging.info("")
    logging.info("******************************************")
    logging.info(" The following arguments were given:")
    logging.info("{}".format(args))
    logging.info("******************************************")
    logging.info(" Sanity check of arguments")
    logging.info("******************************************")
    sanityCheck(args)
    timesteps = selectInterval(args)
    logging.info("Timesteps = {}".format(timesteps))
    logging.info("******************************************")
    logging.info(
        "Set grid to {} and resolution to {}".format(
            args.grid, args.res))
    setArguments(timesteps, args)
    logging.info("Starting ecmwf mars request")
    #p = Pool(3)
    #p.map(fetchECMWF, [sfc_dic, pl_dic, ml_dic])
    logging.info("Ecmwf request finished....")
    logging.info("******************************************")
    logging.info("Concat gribs")
    #catBinaryOutput(out_file, infile_list)
    logging.info("Split gribs and name them for cosmo")
    if GRIBAPI:
        splitGRIBSgribapi(out_file)
    else:
        splitGRIBSeccodes(out_file)
    cleanup(infile_list)
    logging.info("Cleaning directory...")
