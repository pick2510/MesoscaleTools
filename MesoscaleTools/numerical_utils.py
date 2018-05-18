import xarray
import numpy as np
import scipy

def destagger_dimension(data, stagger_dim, name):
    var_shape = data.shape
    num_dims = data.ndim
    stagger_dim_size = var_shape[stagger_dim]

    full_slice = slice(None)
    slice1 = slice(0, stagger_dim_size - 1, 1)
    slice2 = slice(1, stagger_dim_size, 1)
    
    # default to full slices
    dim_ranges_1 = [full_slice] * num_dims
    dim_ranges_2 = [full_slice] * num_dims
    
    # for the stagger dim, insert the appropriate slice range
    dim_ranges_1[stagger_dim] = slice1
    dim_ranges_2[stagger_dim] = slice2
     
    if isinstance(data, xarray.core.dataarray.DataArray):
        coords = data.indexes
        res =  .5*(data.data[dim_ranges_1] + data.data[dim_ranges_2])
        shape = res.shape
        dims = ["level"]
        dims.extend(coords.keys())
        result = xarray.DataArray(res, coords=coords, dims=dims)
    else:
        result =  .5*(data[dim_ranges_1] + data[dim_ranges_2])
    return result

