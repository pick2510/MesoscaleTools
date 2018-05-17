import MesoscaleTools._fortranext as ext
import numpy as np
import xarray

def interp3d(field3d,  zfield, desiredloc, missingval=-9999, is_cosmo = True):
    transp_field3d = np.transpose(field3d.data,(2,1,0))
    transp_zfield = np.transpose(zfield.data, (2,1,0))
    outview = np.empty(transp_field3d.shape[0:2], np.float64, order="F")
    res = np.transpose(ext.utils.dinterp3dz(transp_field3d, outview, transp_zfield, desiredloc, missingval), (1,0))
    if is_cosmo:
        rlat, rlon = field3d.coords["rlat"], field3d.coords["rlon"]
        return xarray.DataArray(res, coords=[rlat, rlon], dims=['rlat', 'rlon'])


def interp4d(field4d, zfield3d, desiredloc, missinval=-9999, is_cosmo = True):
    steps = len(field4d.coords["time"])
    print(steps)


def interp2d(field2d, xcoords, ycoords, x, y):
    num, a =  ext.utils.dinterp2d(field2d.T, xcoords, ycoords, x, y)
    if a == -1:
        raise ValueError("ERROR: x or y value out of bounds")
    else:
        return num

    
