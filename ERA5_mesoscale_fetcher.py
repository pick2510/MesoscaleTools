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
from ERA5_dataset_template import returnModelData




logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)


def setupArgParser():
    parser = argparse.ArgumentParser(description="Fetch ERA5 gribs from ECMWF")
    parser.add_argument('-model', type=str, required=True, choices=["cosmo", "wrf"])
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
        help="Please enter resolution in the form 'dx/dy'",
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


def setArguments(timesteps, args, dic_list):
    for dic in dic_list:
        dic['date'] = "{}/to/{}".format(args.startdate.strftime("%Y-%m-%d"),
                                        args.enddate.strftime("%Y-%m-%d"))
        dic['time'] = timesteps
        dic['area'] = args.grid
        dic['grid'] = args.res
    return dic_list


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
        sys.exit(-1)
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
    return start, end


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


def splitGRIBSCOSMOgribapi(ifile):
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


def splitGRIBSCOSMOeccodes(ifile):
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


def fetchCOSMO(args):
    dic_list, infile_list, out_file = returnModelData(args.model)
    timesteps = selectInterval(args)
    logging.info("Timesteps = {}".format(timesteps))
    logging.info("******************************************")
    logging.info(
        "Set grid to {} and resolution to {}".format(
            args.grid, args.res))
    dic_list=setArguments(timesteps, args, dic_list)
    logging.info("Starting ecmwf mars request")
    #p = Pool(len(dic_list))
    #p.map(fetchECMWF, [sfc_dic, pl_dic, ml_dic])
    logging.info("Ecmwf request finished....")
    logging.info("******************************************")
    logging.info("Concat gribs")
    #catBinaryOutput(out_file, infile_list)
    logging.info("Split gribs and name them for cosmo")
    if GRIBAPI:
        splitGRIBSCOSMOgribapi(out_file)
    else:
        splitGRIBSCOSMOeccodes(out_file)
    cleanup(infile_list)
    logging.info("Cleaning directory...")


def fetchWRF(args):
    dic_list, infile_list, out_file = returnModelData(args.model)
    timesteps = selectInterval(args)
    logging.info("Timesteps = {}".format(timesteps))
    logging.info("******************************************")
    logging.info(
        "Set grid to {} and resolution to {}".format(
            args.grid, args.res))
    dic_list=setArguments(timesteps, args, dic_list)
    print(dic_list, timesteps
          )
    logging.info("Starting ecmwf mars request")
    p = Pool(len(dic_list))
    p.map(fetchECMWF, dic_list)
    logging.info("Ecmwf request finished....")
    logging.info("******************************************")
    logging.info("Concat gribs")
    catBinaryOutput(out_file, infile_list)
    """
    logging.info("Split gribs and name them for cosmo")
    if GRIBAPI:
        splitGRIBSCOSMOgribapi(out_file)
    else:
        splitGRIBSCOSMOeccodes(out_file)
    cleanup(infile_list)
    logging.info("Cleaning directory...")
    """



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
    args.startdate, args.enddate = sanityCheck(args)
    logging.info("Selected model {}".format(args.model))
    logging.info("******************************************")
    if args.model == "cosmo":
        fetchCOSMO(args)
    else:
        fetchWRF(args)
    logging.info("Done fetching.")
    logging.info("Generating Envrionment")
