from .extensions import  interp1d, interp2d, interp3d
import xarray


def interpolate_lat_lon_height(data, height, interplevs, lon, lat):
    res_data = []
    for level in interplevs:
        temp_dat = interp3d(data, height, level)
        rlat,rlon = temp_dat.coords["rlat"], temp_dat.coords["rlon"]
        res_data.append(interp2d(temp_dat.data, rlon, rlat, lon, lat))
    return xarray.DataArray(res_data,coords = {"height": interplevs}, dims="height", name="var")


def interpolate_heightlevels(data, height, interplevs):
    res_data = []
    for level in interplevs:
        res_data.append(interp1d(data, height, level))
    return xarray.DataArray(res_data, coords = {"height": interplevs}, dims= "height", name = "var")