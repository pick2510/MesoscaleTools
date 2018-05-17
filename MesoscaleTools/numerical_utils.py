import xarray
import numpy as np
import scipy

def destagger_dimension(data, stagger_dim):
    var_shape = data.shape
    num_dims = data.ndim
    stagger_dim_size = var_shape[stagger_dim]
    
    # Dynamically building the range slices to create the appropriate 
    # number of ':'s in the array accessor lists.
    # For example, for a 3D array, the calculation would be 
    # result = .5 * (var[:,:,0:stagger_dim_size-2] 
    #                    + var[:,:,1:stagger_dim_size-1])
    # for stagger_dim=2.  So, full slices would be used for dims 0 and 1, but 
    # dim 2 needs the special slice.  
    full_slice = slice(None)
    slice1 = slice(0, stagger_dim_size - 1, 1)
    slice2 = slice(1, stagger_dim_size, 1)
    
    # default to full slices
    dim_ranges_1 = [full_slice] * num_dims
    dim_ranges_2 = [full_slice] * num_dims
    
    # for the stagger dim, insert the appropriate slice range
    dim_ranges_1[stagger_dim] = slice1
    dim_ranges_2[stagger_dim] = slice2
    
    result = .5*(data[dim_ranges_1] + data[dim_ranges_2])
    
    return result

