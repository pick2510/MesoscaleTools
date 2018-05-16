import os
import sys
import setuptools

import numpy.distutils.core
ext1 = numpy.distutils.core.Extension(
    name = "MesoscaleTools._extfortran",
    sources = ["MesoscaleTools/ext/interp.f90"]
)
ext_modules = [ext1]

with open("requirements.txt") as f:
    requirements = f.read().strip().splitlines()


numpy.distutils.core.setup( 
    author = "Dominik Strebel",
    author_email = "dominik.strebel@empa.ch",
    description = "Routines for working with mesoscale simulations at Empa",
    url = "https://github.com/pick2510/MesoscaleTools",
    keywords = ["python", "wrf", "forecast", "model",
                "weather research and forecasting", "interpolation", 
                "plotting", "plots", "meteorology", "nwp", 
                "numerical weather prediction", "diagnostic", "cosmo",
                "science", "numpy", "urban climate"],
    install_requires = requirements,
    classifiers = ["Development Status :: 5 - Production/Stable",
                "Intended Audience :: Science/Research",
                "Intended Audience :: Developers",
                "License :: OSI Approved",
                "Programming Language :: Fortran",
                "Programming Language :: Python :: 2.7",
                "Programming Language :: Python :: 3.4",
                "Programming Language :: Python :: 3.5",
                "Programming Language :: Python :: 3.6",
                "Topic :: Scientific/Engineering :: Atmospheric Science",
                "Topic :: Software Development",
                "Operating System :: POSIX",
                "Operating System :: Unix",
                "Operating System :: MacOS",
                "Operating System :: Microsoft :: Windows"],
    name = "MesoscaleTools",
    platforms = ["any"],
    license = "OSI Approved",
    version =  "0.1 alpha",
    packages = ['MescoscaleTools'],
    ext_modules = ext_modules,
    package_dir = {"MescoscaleTools" : ""},
    download_url = "http://python.org/pypi/wrf-python",
    scripts=[],
)  
