import numpy as np
import scipy


def rh_pressure(t, td):
    return (saturated_water_pressure(td) / saturated_water_pressure(t)) * 100

def saturated_water_pressure(t):
    return  6.10708 * np.exp((17.08085 * t)/(234.12 + t))

def rh_specific_hum(qv, p, t, t0=273.15):
    return 0.263 * qv * p * (np.exp((17.67*(t-t0))/(t-29.65)))**-1     