#############################################################
# COSMO/int2lm Job submitter for PBS TORQUE
# may be used for things as well
# by Dominik Strebel, dominik.strebel@empa.ch
#############################################################

#############################################################
# Imports
#############################################################

import math
import argparse
import os
import random
import re
import subprocess

##############################################################
# Config Section
# ---------------
# Set your environment below
# #############################################################
conf = {
    "DEBUG": False,  # DEBUG switch
    "envargs": ["LD_LIBRARY_PATH='/mnt/data/std/COSMO/lib'"],
    "runpathops": ["rm -f YU*"],
    "email": "dominik.strebel@empa.ch",
    "jobname": None,  # if None, generate one for you
    "modules": ["pgi/17.10",
                "openmpi/2.1.2/pgi/17.10"],
    "runpath": None,  # if None, os.getcwd()
    "mpirunner": "mpirun",
    "epath": None,  # if None, runpath
    "opath": None,  # if None, runpath
    "queue":  None,  # if None and not defined in args, use best, tbd. ;-)
    "walltime": None,  # if None and not defined in args, use max
    "batch": "qsub"
}
#############################################################
# Templates
# ---------
# Edit if you know what you do ;-)
#############################################################

jobtemplate = """#!/bin/bash
#PBS -l nodes={nodes}:ppn={procs}
#PBS -l walltime={walltime}
#PBS -q {queue}
#PBS -N {jobname}
#PBS -o {opath}
#PBS -e {epath}
#PBS -k oe
#PBS -m bea
#PBS -M {email}
#PBS -V
#

cd {runpath}
{runpathops}

{envargs}

{modules}

{mpirunner} -np {np} {prog}
"""

queues = {"short":
          {"maxnodes": 8,
           "cores": 16,
           "maxwalltime": 24},
          "long":
          {"maxnodes": 4,
           "cores": 16,
           "maxwalltime": 168},
          "medium":
          {"maxnodes": 4,
           "cores": 16,
           "maxwalltime": 96},
          "huge":
          {"maxnodes": 16,
           "cores": 16,
           "maxwalltime": 48},
          "newshort":
          {"maxnodes": 16,
           "cores": 20,
           "maxwalltime": 72}
          }

progs = {
    "cosmo": "./cclm",
    "int2lm": "./int2lm"
}

namelist = {
    "cosmo": "INPUT_ORG",
    "int2lm": "INPUT"
}

nprocx_regex = re.compile(r"nprocx.*$", re.M)
nprocy_regex = re.compile(r"nprocy.*$", re.M)


#############################################################
# Procedures
#############################################################


def divisorGenerator(n):
    large_divisors = []
    for i in range(1, int(math.sqrt(n) + 1)):
        if n % i == 0:
            yield i
            if i*i != n:
                large_divisors.append(n / i)
    for divisor in reversed(large_divisors):
        yield divisor


def getNProcXY(nproc):
    root = int(math.sqrt(nproc))
    if root ** 2 == nproc:
        return root, root
    else:
        divs = list(divisorGenerator(nproc))
        return int(divs[len(divs)//2]), int(divs[len(divs)//2-1])


def setupArgParser():
    parser = argparse.ArgumentParser(description='Submit cosmo/int2lm job')
    parser.add_argument('-q', type=str,
                        help='Specifiy the queue to use, If None and not '
                        'defined, use max')
    parser.add_argument('-s', choices=['cosmo', 'int2lm'],
                        help='Specify the application to use', required=True)
    parser.add_argument('-n', type=int,
                        help='Specify node count', required=True)
    parser.add_argument('-w', type=str,
                        help='Specify walltime [HH], if None and not '
                        'defined, use max')
    return parser.parse_args()


def setDefaultArgs(args):
    if args.q == None:
        if conf["queue"] == None:
            args.q = "newshort"
        elif conf["queue"] not in queues.keys():
            args.q = "newshort"
        else:
            args.q = conf["queue"]
    elif args.q not in queues.keys():
        args.q = "newshort"
    if args.w == None:
        if conf["walltime"] == None:
            args.w = queues[args.q]['maxwalltime']
        elif conf["walltime"] > queues[args.q]['maxwalltime']:
            args.w = queues[args.q]['maxwalltime']
        else:
            args.w = conf["walltime"]
    elif int(args.w) > queues[args.q]['maxwalltime']:
        args.w = queues[args.q]['maxwalltime']
    if args.n > queues[args.q]['maxnodes']:
        args.n = queues[args.q]['maxnodes']
    conf["nodes"] = args.n
    conf["walltime"] = str(args.w) + ":00:00"
    conf["queue"] = args.q
    conf["simulation"] = args.s


def setConf():
    if conf["runpath"] == None:
        conf["runpath"] = os.getcwd()
    if conf["epath"] == None:
        conf["epath"] = conf["runpath"]
    if conf["opath"] == None:
        conf["opath"] = conf["runpath"]
    if conf["jobname"] == None:
        conf["jobname"] = "{}-{}-{}".format(os.getlogin(),
                                            conf["simulation"], random.randint(0, 10000))
    if conf["DEBUG"]:
        print(conf)


def generateJob():
    return jobtemplate.format(
        nodes=conf["nodes"],
        procs=queues[conf["queue"]]["cores"],
        walltime=conf["walltime"],
        queue=conf["queue"],
        jobname=conf["jobname"],
        opath=conf["opath"],
        epath=conf["epath"],
        email=conf["email"],
        runpath=conf["runpath"],
        runpathops="".join(["{}\n".format(item)
                            for item in conf["runpathops"]]),
        envargs="".join(["export {}\n".format(item)
                         for item in conf["envargs"]]),
        modules="".join(["module load {}\n".format(item)
                         for item in conf["modules"]]),
        mpirunner=conf["mpirunner"],
        np=conf["nproc"],
        prog=progs[conf["simulation"]]
    )


def changeNamelist():
    with open(namelist[conf["simulation"]], "r") as f:
        nl = f.read()
        nl = re.sub(nprocx_regex, "nprocx = {},".format(conf["nprocx"]), nl)
        nl = re.sub(nprocy_regex, "nprocy = {},".format(conf["nprocy"]), nl)
        if conf["DEBUG"]:
            print(nl)
    with open(namelist[conf["simulation"]], "w") as f:
        f.write(nl)


def submitJob(job):
    proc = subprocess.Popen(
        [conf["batch"]], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    outs, errs = proc.communicate(job)
    return outs


def main():
    setDefaultArgs(setupArgParser())
    if conf["DEBUG"]:
        print(conf)
    setConf()
    conf["nproc"] = conf["nodes"] * queues[conf["queue"]]['cores']
    conf["nprocx"], conf["nprocy"] = getNProcXY(conf["nproc"])
    setConf()
    changeNamelist()
    job = generateJob()
    print("JOBFILE:")
    print(job)
    print("SCRIPT OUT:")
    print(submitJob(job.encode()))


if __name__ == "__main__":
    main()
