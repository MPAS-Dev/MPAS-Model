#!/usr/bin/env python
#  $Id$
"""
Make gsw_mod_check_data.c from the current gsw_data_v3_0.nc. This is a developer
utility and not a part of the public distribution, but its end-product is. Note
that it generates gsw_mod_check_data.c but will not overwrite it if it exists.
General concept: we don't want end-users of this distribution to require having
netcdf installed, nor do we want to incur the I/O overhead every time this
library is used. So we simply generate static data from the netcdf file.
"""
import math, os, sys, numpy, re
from netCDF4 import Dataset

nan = "9e90_r8"

pzeros = re.compile("0{7,}[1-9]*")
pnines = re.compile("9{7,}[0-8]*")

def float2string(val, sformat, addcomma):
    if math.isnan(val):
        str_val = nan
    else:
        str_val = sformat % val
#
#       Need to ensure all floats contain a decimal point because gFortran
#       versions prior to 6.3.1 could give an internal compiler error or
#       generate the wrong result internally (see
#       https://gcc.gnu.org/bugzilla/show_bug.cgi?id=80388).
#
        if str_val.find(".") < 0 and str_val.find("e") < 0 :
            str_val += "."
#
#       Prettify the result by rounding down long sequences of zeros and
#       rounding up long sequences of nines (this is just a machine
#       precision issue that won't affect any GSW results), eg.
#       35.784000000000002 = 35.784  (this is easy - just truncate)
#       34.456999999999995 = 34.457  (a bit harder - truncate and add +1
#                                     to last digit)
#
        (str_val, nsubs) = pnines.subn("",str_val)
        if nsubs > 0 :
            last_digit = "%d" % (int(str_val[-1]) + 1)
            str_val = str_val[:-1] + last_digit
        else:
            str_val = pzeros.sub("",str_val)
        str_val += "_r8"
    if addcomma:
        str_val += ", "
    return str_val

def write_variable(var_name, dims, v):
    ndims = len(dims)
    if ndims == 1:
        if dims[0] == 1:
            fortran_dims = ""
        else:
            fortran_dims = (", dimension(" + work_dims[""][2] + ")")
    else:
        fortran_dims = (", dimension(" + work_dims[v.dimensions[1]][1] + ","
                        + work_dims[v.dimensions[1]][2] + ")")

    out.write("real (r8)%s :: %s\n" % (fortran_dims, var_name))
    out.write("data %s / &\n" % var_name)

    buf = "  "
    maxlen = 78
    num_format = "%.17g"
    if ndims == 1:
        lastx = dims[0]-1
#
#       The following construct (and variations below) transfer the
#       netcdf variable into a memory-resident buffer all at once.
#       Anything else is not advised.
#
        vv = v[:]
        for val, x in [(vv[cx],cx) for cx in range(dims[0])]:
            sval = float2string(val,num_format,(x != lastx))
            if len(buf)+len(sval) > maxlen:
                out.write(buf+"&\n")
                buf = "  "
            buf += sval
    elif ndims == 2:
        lastx = dims[0]-1
        lasty = dims[1]-1
        vv = v[:][:]
        for x in range(dims[0]):
            for val,y in [(vv[x][cy],cy) for cy in range(dims[1])]:
                sval = float2string(val,num_format,(x != lastx or y != lasty))
                if len(buf)+len(sval) > maxlen:
                    out.write(buf+"&\n")
                    buf = "  "
                buf += sval
    else:
        lastx = dims[0]-1
        lasty = dims[1]-1
        lastz = dims[2]-1
        vv = v[:][:][:]
        for x in range(dims[0]):
            for y in range(dims[1]):
                for val,z in [(vv[x][y][cz],cz) for cz in range(dims[2])]:
                    sval = float2string(val,num_format, 
                                (x != lastx or y != lasty or z != lastz))
                    if len(buf)+len(sval) > maxlen:
                        out.write(buf+"&\n")
                        buf = "  "
                    buf += sval
    if buf:
        out.write(buf+" /\n\n")

def write_structure(var_name, dims, v):
    ndims = len(dims)
    if ndims == 1:
        gsw_type = work_dims[""][3]
    else:
        gsw_type = work_dims[v.dimensions[1]][3]

    out.write("type(%s) :: %s\n\n" % (gsw_type, var_name.lower()))
    out.write("data %s / %s( &\n" % (var_name.lower(), gsw_type))

    if math.isnan(v.computation_accuracy):
        str_ca = nan
    else:
        str_ca = "%.5g_r8" % v.computation_accuracy

    out.write("  \"%s\", %s, (/ &\n" % (var_name, str_ca))

    vmax = numpy.nanmax(v)
    if vmax != 0.:
        precision = v.computation_accuracy/abs(vmax)
    else:
        precision = v.computation_accuracy

    if precision < 1e-12:
        num_format = "%.16g"
    elif precision < 1e-10:
        num_format = "%.14g"
    else:
        num_format = "%.12g"

    buf = "  "
    maxlen = 78
    if ndims == 1:
        lastx = dims[0]-1
#
#       The following construct (and variations below) transfer the
#       netcdf variable into a memory-resident buffer all at once.
#       Anything else is not advised.
#
        vv = v[:]
        for val, x in [(vv[cx],cx) for cx in range(dims[0])]:
            sval = float2string(val,num_format,(x != lastx))
            if len(buf)+len(sval) > maxlen:
                out.write(buf+"&\n")
                buf = "  "
            buf += sval
    elif ndims == 2:
        lastx = dims[0]-1
        lasty = dims[1]-1
        vv = v[:][:]
        for x in range(dims[0]):
            for val,y in [(vv[x][cy],cy) for cy in range(dims[1])]:
                sval = float2string(val,num_format,(x != lastx or y != lasty))
                if len(buf)+len(sval) > maxlen:
                    out.write(buf+"&\n")
                    buf = "  "
                buf += sval
    else:
        lastx = dims[0]-1
        lasty = dims[1]-1
        lastz = dims[2]-1
        vv = v[:][:][:]
        for x in range(dims[0]):
            for y in range(dims[1]):
                for val,z in [(vv[x][y][cz],cz) for cz in range(dims[2])]:
                    sval = float2string(val,num_format, 
                                (x != lastx or y != lasty or z != lastz))
                    if len(buf)+len(sval) > maxlen:
                        out.write(buf+"&\n")
                        buf = "  "
                    buf += sval
    if buf:
        out.write(buf+" &\n")
    out.write("  /) ) /\n\n")

work_dims = {
     "test_cast_length":             ["test_cast_number",
              "cast_m",       "cast_n",       "gsw_result"],
     "Arctic_test_cast_length":      ["Arctic_test_cast_number",
              "cast_ice_m",   "cast_ice_n",   "gsw_result_ice"],
     "test_cast_midpressure_length": ["test_cast_midpressure_number",
              "cast_mpres_m", "cast_mpres_n", "gsw_result_mpres"],
     "":     ["",     "",     "cast_n",       "gsw_result_cast"]
}
work_vars = [
    ["ct", "CT_chck_cast"],
    ["rt", "Rt_chck_cast"],
    ["sa", "SA_chck_cast"],
    ["sk", "SK_chck_cast"],
    ["sp", "SP_chck_cast"],
    ["t", "t_chck_cast"],
    ["p", "p_chck_cast"],
    ["delta_p", "delta_p_chck_cast"],
    ["p_shallow", "p_chck_cast_shallow"],
    ["p_deep", "p_chck_cast_deep"],
    ["lat_cast", "lat_chck_cast"],
    ["long_cast", "long_chck_cast"],
    ["ct_arctic", "CT_Arctic"],
    ["sa_arctic", "SA_Arctic"],
    ["t_arctic", "t_Arctic"],
    ["p_arctic", "p_Arctic"],
    ["sa_seaice", "SA_seaice"],
    ["t_seaice", "t_seaice"],
    ["w_seaice", "w_seaice"],
    ["t_ice", "t_ice"],
    ["w_ice", "w_ice"],
    ["sa_bulk", "SA_bulk"],
    ["h_pot_bulk", "h_pot_bulk"],
    ["h_bulk", "h_bulk"],
    ["pref", "pr"]
]
vars = [
    ['C_from_SP', ""],
    ['SP_from_C', ""],
    ['SP_from_SK', ""],
    ['SA_from_SP', ""],
    ['Sstar_from_SP', ""],
    ['CT_from_t', ""],
    ['deltaSA_from_SP', ""],
    ['SR_from_SP', ""],
    ['SP_from_SR', ""],
    ['SP_from_SA', ""],
    ['Sstar_from_SA', ""],
    ['SA_from_Sstar', ""],
    ['SP_from_Sstar', ""],
    ['pt_from_CT', ""],
    ['t_from_CT', ""],
    ['CT_from_pt', ""],
    ['pt0_from_t', ""],
    ['pt_from_t', ""],
    ['z_from_p', ""],
    ['p_from_z', ""],
    ['entropy_from_pt', ""],
    ['pt_from_entropy', ""],
    ['CT_from_entropy', ""],
    ['entropy_from_t', ""],
    ['adiabatic_lapse_rate_from_CT', ""],
    ['specvol', ""],
    ['alpha', ""],
    ['beta', ""],
    ['alpha_on_beta', ""],
    ['v_vab', ""],
    ['alpha_vab', ""],
    ['beta_vab', ""],
    ['v_SA', ""],
    ['v_CT', ""],
    ['v_P', ""],
    ['v_SA_SA', ""],
    ['v_SA_CT', ""],
    ['v_CT_CT', ""],
    ['v_SA_P', ""],
    ['v_CT_P', ""],
    ['v_SA_wrt_h', ""],
    ['v_h', ""],
    ['v_SA_SA_wrt_h', ""],
    ['v_SA_h', ""],
    ['v_h_h', ""],
    ['specvol_anom_standard', ""],
    ['rho', ""],
    ['rho_rab', ""],
    ['alpha_rab', ""],
    ['beta_rab', ""],
    ['rho_SA', ""],
    ['rho_CT', ""],
    ['rho_P', ""],
    ['rho_SA_SA', ""],
    ['rho_SA_CT', ""],
    ['rho_CT_CT', ""],
    ['rho_SA_P', ""],
    ['rho_CT_P', ""],
    ['rho_SA_wrt_h', ""],
    ['rho_h', ""],
    ['rho_SA_SA_wrt_h', ""],
    ['rho_SA_h', ""],
    ['rho_h_h', ""],
    ['sigma0', ""],
    ['sigma1', ""],
    ['sigma2', ""],
    ['sigma3', ""],
    ['sigma4', ""],
    ['sound_speed', ""],
    ['kappa', ""],
    ['cabbeling', ""],
    ['thermobaric', ""],
    ['SA_from_rho', ""],
    ['CT_from_rho', ""],
    ['CT_maxdensity', ""],
    ['internal_energy', ""],
    ['enthalpy', ""],
    ['enthalpy_diff', ""],
    ['CT_from_enthalpy', ""],
    ['dynamic_enthalpy', ""],
    ['h_SA', ""],
    ['h_CT', ""],
    ['h_SA_SA', ""],
    ['h_SA_CT', ""],
    ['h_CT_CT', ""],
    ['CT_SA', ""],
    ['CT_pt', ""],
    ['CT_SA_SA', ""],
    ['CT_SA_pt', ""],
    ['CT_pt_pt', ""],
    ['eta_SA', ""],
    ['eta_CT', ""],
    ['eta_SA_SA', ""],
    ['eta_SA_CT', ""],
    ['eta_CT_CT', ""],
    ['pt_SA', ""],
    ['pt_CT', ""],
    ['pt_SA_SA', ""],
    ['pt_SA_CT', ""],
    ['pt_CT_CT', ""],
    ['CT_freezing', ""],
    ['CT_freezing_poly', ""],
    ['t_freezing', ""],
    ['t_freezing_poly', ""],
    ['pot_enthalpy_ice_freezing', ""],
    ['pot_enthalpy_ice_freezing_poly', ""],
    ['SA_freezing_from_CT', ""],
    ['SA_freezing_from_CT_poly', ""],
    ['SA_freezing_from_t', ""],
    ['SA_freezing_from_t_poly', ""],
    ['CTfreezing_SA', ""],
    ['CTfreezing_P', ""],
    ['CTfreezing_SA_poly', ""],
    ['CTfreezing_P_poly', ""],
    ['tfreezing_SA', ""],
    ['tfreezing_P', ""],
    ['tfreezing_SA_poly', ""],
    ['tfreezing_P_poly', ""],
    ['pot_enthalpy_ice_freezing_SA', ""],
    ['pot_enthalpy_ice_freezing_P', ""],
    ['pot_enthalpy_ice_freezing_SA_poly', ""],
    ['pot_enthalpy_ice_freezing_P_poly', ""],
    ['latentheat_melting', ""],
    ['latentheat_evap_CT', ""],
    ['latentheat_evap_t', ""],
    ['grav', ""],
    ['enthalpy_CT_exact', ""],
    ['h_SA_CT_exact', ""],
    ['h_CT_CT_exact', ""],
    ['h_SA_SA_CT_exact', ""],
    ['h_SA_CT_CT_exact', ""],
    ['h_CT_CT_CT_exact', ""],
    ['rho_t_exact', ""],
    ['pot_rho_t_exact', ""],
    ['alpha_wrt_t_exact', ""],
    ['beta_const_t_exact', ""],
    ['specvol_t_exact', ""],
    ['sound_speed_t_exact', ""],
    ['kappa_t_exact', ""],
    ['enthalpy_t_exact', ""],
    ['CT_SA_wrt_t', ""],
    ['CT_T_wrt_t', ""],
    ['CT_P_wrt_t', ""],
    ['chem_potential_water_t_exact', ""],
    ['t_deriv_chem_potential_water_t_exact', ""],
    ['dilution_coefficient_t_exact', ""],
    ['deltaSA_atlas', ""],
    ['Fdelta', ""],
    ['n2', ""],
    ['p_mid_n2', ""],
    ['Tu', ""],
    ['Rsubrho', ""],
    ['p_mid_TuRsr', ""],
    ['n2min', ""],
    ['n2min_pmid', ""],
    ['n2min_specvol', ""],
    ['n2min_alpha', ""],
    ['n2min_beta', ""],
    ['n2min_dsa', ""],
    ['n2min_dct', ""],
    ['n2min_dp', ""],
    ['IPVfN2', ""],
    ['p_mid_IPVfN2', ""],
    ['n2_lowerlimit', ""],
    ['mlp', ""],
    ['geo_strf_dyn_height', ""],
    ['geo_strf_dyn_height_pc', ""],
    ['geo_strf_dyn_height_pc_p_mid', ""],
    ['rho_ice', ""],
    ['alpha_wrt_t_ice', ""],
    ['specvol_ice', ""],
    ['pressure_coefficient_ice', ""],
    ['sound_speed_ice', ""],
    ['kappa_ice', ""],
    ['kappa_const_t_ice', ""],
    ['internal_energy_ice', ""],
    ['enthalpy_ice', ""],
    ['entropy_ice', ""],
    ['cp_ice', ""],
    ['chem_potential_water_ice', ""],
    ['Helmholtz_energy_ice', ""],
    ['adiabatic_lapse_rate_ice', ""],
    ['pt0_from_t_ice', ""],
    ['pt_from_t_ice', ""],
    ['t_from_pt0_ice', ""],
    ['pot_enthalpy_from_pt_ice', ""],
    ['pt_from_pot_enthalpy_ice', ""],
    ['pot_enthalpy_from_pt_ice_poly', ""],
    ['pt_from_pot_enthalpy_ice_poly', ""],
    ['pressure_freezing_CT', ""],
    ['melting_ice_SA_CT_ratio', ""],
    ['melting_ice_SA_CT_ratio_poly', ""],
    ['melting_ice_equilibrium_SA_CT_ratio', ""],
    ['melting_ice_equilibrium_SA_CT_ratio_poly', ""],
    ['melting_ice_into_seawater_SA_final', ""],
    ['melting_ice_into_seawater_CT_final', ""],
    ['ice_fraction_to_freeze_seawater_SA_freeze', ""],
    ['ice_fraction_to_freeze_seawater_CT_freeze', ""],
    ['ice_fraction_to_freeze_seawater_w_Ih', ""],
    ['dSA_dCT_frazil', ""],
    ['dSA_dP_frazil', ""],
    ['dCT_dP_frazil', ""],
    ['dSA_dCT_frazil_poly', ""],
    ['dSA_dP_frazil_poly', ""],
    ['dCT_dP_frazil_poly', ""],
    ['frazil_properties_potential_SA_final', ""],
    ['frazil_properties_potential_CT_final', ""],
    ['frazil_properties_potential_w_Ih_final', ""],
    ['frazil_properties_potential_poly_SA_final', ""],
    ['frazil_properties_potential_poly_CT_final', ""],
    ['frazil_properties_potential_poly_w_Ih_final', ""],
    ['frazil_properties_SA_final', ""],
    ['frazil_properties_CT_final', ""],
    ['frazil_properties_w_Ih_final', ""],
    ['melting_seaice_SA_CT_ratio', ""],
    ['melting_seaice_SA_CT_ratio_poly', ""],
    ['melting_seaice_equilibrium_SA_CT_ratio', ""],
    ['melting_seaice_equilibrium_SA_CT_ratio_poly', ""],
    ['melting_seaice_into_seawater_SA_final', ""],
    ['melting_seaice_into_seawater_CT_final', ""],
    ['seaice_fraction_to_freeze_seawater_SA_freeze', ""],
    ['seaice_fraction_to_freeze_seawater_CT_freeze', ""],
    ['seaice_fraction_to_freeze_seawater_w_Ih', ""]
]
rootgrp = Dataset('gsw_data_v3_0.nc', 'r')
v = rootgrp.variables
d = rootgrp.dimensions

version_date = rootgrp.version_date
version_number = rootgrp.version_number
try:
    fd = os.open("gsw_mod_check_data.f90", os.O_CREAT|os.O_EXCL|os.O_RDWR, 0644)
except:
    print str(sys.exc_info()[1])
    print "Will not overwrite gsw_mod_check_data.f90 - exiting."
    sys.exit(1)
out = os.fdopen(fd, "w")
out.write("""
!
!**  $Id$
!**  Extracted from gsw_data_v3_0.nc
!
!==========================================================================
module gsw_mod_check_data
!==========================================================================

use gsw_mod_kinds

implicit none

""")

dim_format = "integer, parameter :: %s = %d\n"
for key, value in work_dims.items():
    if key != "":
        out.write(dim_format % (value[1], len(d[key])))
        out.write(dim_format % (value[2], len(d[value[0]])))

out.write("""
type gsw_result
    character (50) :: variable_name
    real (r8) :: computation_accuracy
    real (r8), dimension(cast_m,cast_n) :: values
end type gsw_result

type gsw_result_ice
    character (50) :: variable_name
    real (r8) :: computation_accuracy
    real (r8), dimension(cast_ice_m,cast_ice_n) :: values
end type gsw_result_ice

type gsw_result_mpres
    character (50) :: variable_name
    real (r8) :: computation_accuracy
    real (r8), dimension(cast_mpres_m,cast_mpres_n) :: values
end type gsw_result_mpres

type gsw_result_cast
    character (50) :: variable_name
    real (r8) :: computation_accuracy
    real (r8), dimension(cast_n) :: values
end type gsw_result_cast

""")

for var_label, var_name in [var for var in work_vars]:
    if not var_name:
        var_name = var_label
    dims = [len(d[dname]) for dname in v[var_name].dimensions]
    write_variable(var_label.lower(), dims, v[var_name])

for var_label, var_name in [var for var in vars]:
    if not var_name:
        var_name = var_label
    dims = [len(d[dname]) for dname in v[var_name].dimensions]
    write_structure(var_label, dims, v[var_name])

out.write("""
end module
!--------------------------------------------------------------------------
""")
out.close()
sys.exit(0)
