"""
*******************************************************************************
Data Structures for COSMO and WRF ecmwfapi requests
Dominik Strebel, Empa
*******************************************************************************
MARS default params.
*******************************************************************************
"""
grid = "5/100/-2/108"
res = "0.25/0.25"
date = "2014-02-01/to/2014-02-28"
cosmo_sfc_params = "039/040/041/042/043/129/134/139/141/170/172/183/198/235/236/238"
cosmo_ml_params = "075/076/130/131/132/133/152/203.200/246/247"
cosmo_pl_params = "129"
wrf_sfc_params = "msl/skt/2t/10u/10v/2d/z/lsm/sst/ci/sd/stl1/stl2/stl3/stl4/swvl1/swvl2/swvl3/swvl4"
wrf_ml_params = "129/130/131/132/133/152"
mod_levs = "all"
cosmo_sfc_file = "ERA5_cosmo_sfc.grb"
cosmo_ml_file = "ERA5_cosmo_ml.grb"
cosmo_pl_file = "ERA5_cosmo_pl.grb"
cosmo_out_file = "ERA5_cosmo_merged.grb"
wrf_sfc_file = "ERA5_wrf_sfc.grb"
wrf_ml_file = "ERA5_wrf_ml.grb"
wrf_out_file = "ERA5_wrf_merged.grb"

cosmo_infile_list = [cosmo_sfc_file, cosmo_ml_file, cosmo_pl_file]
wrf_infile_list = [wrf_sfc_file, wrf_ml_file]

"""
*******************************************************************************
COSMO DICS
*******************************************************************************
"""


cosmo_sfc_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "sfc",
        'param': cosmo_sfc_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': cosmo_sfc_file
}
cosmo_pl_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "pl",
        'param': cosmo_pl_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': cosmo_pl_file
}
cosmo_ml_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "ml",
        'levelist': mod_levs,
        'param': cosmo_ml_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': cosmo_ml_file
}


cosmo_dic_list = [cosmo_sfc_dic, cosmo_ml_dic, cosmo_pl_dic]
"""
******************************************************************************
WRF DICS
******************************************************************************
"""


wrf_sfc_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "sfc",
        'param': wrf_sfc_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': wrf_sfc_file
}

wrf_ml_dic = {
        'stream': 'oper',
        'class': 'ea',
        'dataset': 'era5',
        'date': date,
        'levtype': "ml",
        'levelist': mod_levs,
        'param': wrf_ml_params,
        'area': grid,
        'grid': res,
        'type': "an",
        'expver': "1",
        'time': "",
        'target': wrf_ml_file
}

wrf_dic_list = [wrf_sfc_dic, wrf_ml_dic]

"""
******************************************************************************
Return correct data for the selected model
******************************************************************************
"""


def returnModelData(selectedModel):
    if selectedModel == "cosmo":
        return cosmo_dic_list, cosmo_infile_list, cosmo_out_file
    else:
        return wrf_dic_list, wrf_infile_list, wrf_out_file

