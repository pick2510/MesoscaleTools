mport xarray
import glob
import logging
import numpy as np
import numpy.ma as ma
import pandas as pd
from grids import RotatedGrid


LAT = 47.37008
LON = 8.54102
#GLOB_PATTERN = "/home/strebdom/std-srv/run_23_08_19/lfff000*.nc"
GLOB_PATTERN = "/home/strebdom/std-srv/results_total_adj_prop/lff*.nc"


OUT_PATH = "/home/strebdom/test/"
FILE_TEMPLATE = OUT_PATH + "{}.csv"


VARS = [
    "T",
    "SW_GROUND",
    "SW_WALL",
    "SW_ROOF",
    "T_WALL",
    "T_ROOF",
    "T_GROUND"
]


def get_grid(pollat=43, pollon=-170):
    return RotatedGrid(pollat=pollat, pollon=pollon)


def get_nc_files():
    files = glob.glob(GLOB_PATTERN)
    files.sort()
    return files


def get_timesteps(ds):
    return ds.time.values


def get_subset(ds, rlat, rlon):
    return ds.sel(rlat=rlat, rlon=rlon,  method='nearest')


def get_srlon_subset(ds, rlat, srlon):
    return ds.sel(rlat=rlat, srlon=srlon,  method='nearest')

def get_srlat_subset(ds, srlat, rlon):
    return ds.sel(srlat=srlat, rlon=rlon,  method='nearest')



def get_general_1d(subset, variable, descr):
    ts = get_timesteps(subset)
    data = subset[variable].values
    return pd.DataFrame(data=data, columns=[descr], index=ts)


def get_var_lev(subset, variable):
    levs = subset[variable].level.values
    ts = get_timesteps(subset)
    data = subset[variable].values
    return pd.DataFrame(data=data, columns=levs, index=ts)


def get_lowest_lev_var(subset, variable, descr):
    lowest_lev = subset[variable].level.values[-1]
    ts = get_timesteps(subset)
    data = subset[variable][:, lowest_lev].values
    return pd.DataFrame(data=data, columns=[descr], index=ts)


def get_roof_t(subset):
    ts = get_timesteps(subset)
    mask = ~(subset.FR_ROOF[0, 0].values > 0)
    mask = np.repeat(mask[np.newaxis, :], len(ts), axis=0)
    vals = subset.T_ROOF_S[:, 0, :, :].values
    vals_masked = ma.masked_array(vals, mask)
    mean = vals_masked.mean(axis=2)[:, [1, 3]]
    df = pd.DataFrame(data=mean, columns=["northSouth", "eastWest"], index=ts)
    return df


def get_two_dir_variable(subset, variable):
    ts = get_timesteps(subset)
    data = subset[variable][:, 0, [1, 3]].values
    df = pd.DataFrame(data=data, columns=["northSouth", "eastWest"], index=ts)
    return df


def get_four_dir_variable(subset, variable):
    ts = get_timesteps(subset)
    vals = subset[variable][:, 0, [6, 3, 7, 2]].values
    # print(vals.shape)
    mask = ~(subset.FR_UDIR[0, 0].values > 0)
    mask = np.repeat(mask[np.newaxis, :], len(ts), axis=0)
    mask = np.repeat(mask[:, :, np.newaxis], len(
        subset.FR_ROOF.uheight1)-1, axis=2)
    # print(mask.shape)
    vals_masked = ma.masked_array(vals, mask)
    mean = vals_masked.mean(axis=2)
    # print(mean.shape)
    df = pd.DataFrame(data=mean, columns=[
                      "north", "east", "south", "west"], index=ts)
    return df


def write_file_ts(df, name):
    df.to_csv(FILE_TEMPLATE.format(name), index_label="time")


def write_file_lev(df, name):
    df.to_csv(FILE_TEMPLATE.format(name), header=False)


def get_levs(ds):
    levs = ds.vcoord[0].to_dataframe()
    levs = levs.drop(["height_2m", "height_10m", "height_toa", "time"],
                     axis=1)
    return levs


def main():
    logging.basicConfig(level=logging.INFO)
    logging.info("START EXTRACTION")
    files = get_nc_files()
    logging.info("Files: {}".format(files))
    ds = xarray.open_mfdataset(files, parallel=True)
    timesteps = get_timesteps(ds)
    grid = get_grid()
    logging.info("reg lat: {}, reg lon: {}".format(LAT, LON))
    rlat, rlon = grid.transformToRot(lats=LAT, lons=LON)
    rlat, rlon = rlat[0], rlon[0]
    logging.info("rot lat: {}, rot lon: {}".format(rlat, rlon))
    subset_rot = get_subset(ds, rlat, rlon)
    ts = get_var_lev(subset_rot, "T")
    write_file_ts(ts, "T")
    levs = get_levs(ds)
    write_file_lev(levs, "LEVELS")
    t_sfc = get_two_dir_variable(subset_rot, "T_GROUND_S")
    write_file_ts(t_sfc, "T_GROUND")
    t_roof = get_roof_t(subset_rot)
    write_file_ts(t_roof, "T_ROOF")
    asob_s = get_general_1d(subset_rot, "ASOB_S", "Solarradiation")
    write_file_ts(asob_s, "RADIATION")
    t_wall = get_four_dir_variable(subset_rot, "T_WALL_S")
    write_file_ts(t_wall, "T_WALL")
    rel_hum = get_var_lev(subset_rot, "RELHUM")
    write_file_ts(rel_hum, "RELHUM")
    subset_U = get_srlon_subset(ds, rlat, rlon)
    subset_V = get_srlat_subset(ds, rlat, rlon)
    U = get_var_lev(subset_U, "U")
    V = get_var_lev(subset_V, "V")
    write_file_ts(U, "WIND_U")
    write_file_ts(V, "WIND_V")
    mag = (U**2 + V**2)
    mag = mag.apply(lambda x: np.sqrt(x))
    write_file_ts(mag, "WIND_MAG")

     


if __name__ == "__main__":
    main()

