
;******************************************************************
;   getvars.pro

;   Copyright (c) 2002-2008, Scott D. Peckham 
;   Created:   June 2002
;   Modified:  Feb 2004, July 2005, Oct 2005,
;   Modified:  Dec 2005, Jan 2006, April 2006
;   Modified:  May 2006, July 2006
;   Modified:  Feb 2007 (Qnet -> Qn_SW and Qn_LW)
;   Modified:  Mar 2007 (met_vars, separated)
;   Modified:  Mar 2007 (chan_dt defaults to 0, set later)
;   Modified:  Mar 2008 (Get_Infil_Vars, multi-layer changes)

;******************************************************************

;   Update_Outfile_Names  (7/27/06)
;   Total_Outfile_Size    (July 2005)

;   Get_Run_Vars          (7/10/05)
;   Get_Grid_Vars
;   Get_Stop_Vars
;   Get_Precip_Vars
;   Get_Channel_Vars
;   Get_Met_Vars           (new: 3/13/07)
;   Get_Snow_Vars
;   Get_ET_Vars
;   Get_GW_Vars
;   Get_Infil_Vars
;   Get_Overland_Vars
;   Get_Sediment_Vars
;   Get_Diversion_Vars

;   Read_Var                      (2/24/04)
;   Read_Next_Var
;   Read_Precip_Vars_in_Files
;   Read_Channel_Vars_in_Files    (7/13/05)
;   Read_Met_Vars_in_Files        (new: 3/14/07)
;   Read_Snow_Vars_in_Files
;   Read_ET_Var
;   Read_ET_Vars_in_Files
;   Read_GW_Vars_in_Files
;   Read_Infil_Vars_in_Files

;   Free_Pointers                 (Update when adding new vars)
;   Close_Input_Files

;   Update_Grid_Vars       (10/1/05)
;   Update_Precip_Vars
;   Update_Met_Vars        (new: 3/14/07)
;   Update_Snow_Vars
;   Update_ET_Vars
;   Update_Infil_Vars
;   Update_GW_Vars

;   Var_Setting   (function)

;******************************************************************
;An issue with PTR_NEW and the NO_COPY keyword:
;Make sure to check all the "Read_Gridded" routines,
;and the Precipitation and Snowmelt functions.

;-------------------
;CASE 1: Works fine
;-------------------
;h = ptr_new(1.0)
;*h = *h + 1.0
;print, *h

;--------------------         ----------------------
;CASE 2: Works fine           CASE 3: Doesn't work
;--------------------         ----------------------
;m = 1.0                   
;h = ptr_new(m, /no_copy)     h = ptr_new(1.0, /no_copy)
;*h = *h + 1.0                *h = *h + 1.0
;print, *h                    print, *h
;print, m
;k = indgen(3,3)
;*h = k

;******************************************************************
pro Update_Outfile_Names, run_prefix, mstate

;---------------------------------------------------------------
;Notes:  Whenever the run_prefix changes (e.g. user changes it
;        and clicks the Next button in the "run var" panel),
;        all output filenames should be reset to new defaults.
;---------------------------------------------------------------

;--------------------------
;Channel process filenames
;--------------------------
mstate.channel_vars.Q_rts_file = (run_prefix + '_2D-Q.rts')
mstate.channel_vars.u_rts_file = (run_prefix + '_2D-u.rts')
mstate.channel_vars.d_rts_file = (run_prefix + '_2D-d.rts')
mstate.channel_vars.f_rts_file = (run_prefix + '_2D-f.rts')
;------------------------------------------------------------
mstate.channel_vars.Q_out_file = (run_prefix + '_0D-Q.txt')
mstate.channel_vars.u_out_file = (run_prefix + '_0D-u.txt')
mstate.channel_vars.d_out_file = (run_prefix + '_0D-d.txt')
mstate.channel_vars.f_out_file = (run_prefix + '_0D-f.txt')

;---------------------------
;Snowmelt process filenames
;---------------------------
mstate.snow_vars.mr_rts_file = (run_prefix + '_2D-SMrate.rts')
mstate.snow_vars.hs_rts_file = (run_prefix + '_2D-hsnow.rts')
mstate.snow_vars.sw_rts_file = (run_prefix + '_2D-hswe.rts')
mstate.snow_vars.cc_rts_file = (run_prefix + '_2D-Ecc.rts')
mstate.snow_vars.ea_rts_file = (run_prefix + '_2D-ea.rts')
mstate.snow_vars.es_rts_file = (run_prefix + '_2D-es.rts')
;---------------------------------------------------------------
mstate.snow_vars.mr_out_file = (run_prefix + '_0D-SMrate.txt')
mstate.snow_vars.hs_out_file = (run_prefix + '_0D-hsnow.txt')
mstate.snow_vars.sw_out_file = (run_prefix + '_0D-hswe.txt')
mstate.snow_vars.cc_out_file = (run_prefix + '_0D-Ecc.txt')
mstate.snow_vars.ea_out_file = (run_prefix + '_0D-ea.txt')
mstate.snow_vars.es_out_file = (run_prefix + '_0D-es.txt')

;---------------------
;ET process filenames
;---------------------
mstate.ET_vars.er_rts_file = (run_prefix + '_2D-ETrate.rts')
;-------------------------------------------------------------
mstate.ET_vars.er_out_file = (run_prefix + '_0D-ETrate.txt')

;-------------------------------
;Infiltration process filenames
;-------------------------------
mstate.infil_vars.v0_rts_file = (run_prefix + '_2D-v0.rts')
mstate.infil_vars.q0_rts_file = (run_prefix + '_2D-q0.rts')
mstate.infil_vars.I_rts_file  = (run_prefix + '_2D-I.rts')
mstate.infil_vars.Zw_rts_file = (run_prefix + '_2D-Zw.rts')
;-------------------------------------------------------------
mstate.infil_vars.v0_out_file = (run_prefix + '_0D-v0.txt')
mstate.infil_vars.q0_out_file = (run_prefix + '_0D-q0.txt')
mstate.infil_vars.I_out_file  = (run_prefix + '_0D-I.txt')
mstate.infil_vars.Zw_out_file = (run_prefix + '_0D-Zw.txt')
;-------------------------------------------------------------
mstate.infil_vars.q_stack_file = (run_prefix + '_3D-q.rt3')
mstate.infil_vars.p_stack_file = (run_prefix + '_3D-p.rt3')
mstate.infil_vars.K_stack_file = (run_prefix + '_3D-K.rt3')
mstate.infil_vars.v_stack_file = (run_prefix + '_3D-v.rt3')
;-------------------------------------------------------------
mstate.infil_vars.q_profile_file = (run_prefix + '_1D-q.txt')
mstate.infil_vars.p_profile_file = (run_prefix + '_1D_p.txt')
mstate.infil_vars.K_profile_file = (run_prefix + '_1D_K.txt')
mstate.infil_vars.v_profile_file = (run_prefix + '_1D_v.txt')

;----------------------------------
;Subsurface flow process filenames
;----------------------------------
mstate.GW_vars.ht_rts_file  = (run_prefix + '_2D-htable.rts')
mstate.GW_vars.df_rts_file  = (run_prefix + '_2D-dfreeze.rts')
mstate.GW_vars.dt_rts_file  = (run_prefix + '_2D-dthaw.rts')
;mstate.GW_vars.sm_rts_file = (run_prefix + '_2D-q0.rts')
;---------------------------------------------------------------
mstate.GW_vars.ht_out_file  = (run_prefix + '_0D-htable.txt')
mstate.GW_vars.df_out_file  = (run_prefix + '_0D-dfreeze.txt')
mstate.GW_vars.dt_out_file  = (run_prefix + '_0D-dthaw.txt')
;mstate.GW_vars.sm_out_file = (run_prefix + '_0D-q0.txt')

end;  Update_Outfile_Names
;******************************************************************
function Total_Outfile_Size, mstate

;-------------------------------------------------------
;Notes:  This function computes or estimates the total
;        filesize of all the output files that the user
;        has selected.
;-------------------------------------------------------
FORWARD_FUNCTION Number_of_Samples

;---------------------------
;Abbrevations for some vars
;---------------------------
dt = mstate.channel_vars.dt     ;[seconds]
nx = mstate.grid_vars.ncols
ny = mstate.grid_vars.nrows
cv = mstate.channel_vars
sv = mstate.snow_vars
ev = mstate.et_vars
iv = mstate.infil_vars
gv = mstate.gw_vars

stop_vars = mstate.stop_vars

bpp   = 4UL                    ;(bytes per pixel or sample)
bpg   = ulong(nx * ny * bpp)   ;(bytes per grid layer)
total = 0UL

;--------------------------------------------
;Get number of grid samples for each process
;--------------------------------------------
n_grids_chan  = Number_of_Samples(stop_vars, cv.save_grid_dt, dt)
n_grids_snow  = Number_of_Samples(stop_vars, sv.save_grid_dt, dt)
n_grids_et    = Number_of_Samples(stop_vars, ev.save_grid_dt, dt)
n_grids_infil = Number_of_Samples(stop_vars, iv.save_grid_dt, dt)
n_grids_gw    = Number_of_Samples(stop_vars, gv.save_grid_dt, dt)

;-----------------------------------
;Add up the total size of RTS files
;that user has chosen to create
;-----------------------------------
ngc = n_grids_chan  * (cv.save_Q_grids + cv.save_u_grids + $
                       cv.save_d_grids + cv.save_f_grids)
ngs = n_grids_snow  * (sv.save_mr_grids + sv.save_hs_grids + $
                       sv.save_sw_grids + sv.save_cc_grids + $
                       sv.save_ea_grids + sv.save_es_grids)
nge = n_grids_et    * (ev.save_er_grids)
ngi = n_grids_infil * (iv.save_v0_grids + iv.save_I_grids + $
                       iv.save_q0_grids + iv.save_Zw_grids)
ngg = n_grids_gw    * (gv.save_ht_grids + gv.save_df_grids + $
                       gv.save_dt_grids)

total = total + bpg*(ngc + ngs + nge + ngi + ngg)

;-----------------------------------
;Try to adjust for factor of safety
;This is hard to do dynamically;
;will need to experiment more.
;-----------------------------------
if (stop_vars.method eq 0) then begin
    total = (total / 5UL)
endif
   
;---------------------------------------------
;Get number of pixel samples for each process
;---------------------------------------------
n_pixels_chan  = Number_of_Samples(stop_vars, cv.save_pixels_dt, dt)
n_pixels_snow  = Number_of_Samples(stop_vars, sv.save_pixels_dt, dt)
n_pixels_et    = Number_of_Samples(stop_vars, ev.save_pixels_dt, dt)
n_pixels_infil = Number_of_Samples(stop_vars, iv.save_pixels_dt, dt)
n_pixels_gw    = Number_of_Samples(stop_vars, gv.save_pixels_dt, dt)
 
;--------------------------------------------------------------
;Add up the total size of "pixel profiles" that user has
;chosen to create.  NB!  They are stored as ASCII, not binary.
;--------------------------------------------------------------
npc = n_pixels_chan  * (cv.save_Q_pixels + cv.save_u_pixels + $
                        cv.save_d_pixels + cv.save_f_pixels)
nps = n_pixels_snow  * (sv.save_mr_pixels + sv.save_hs_pixels + $
                        sv.save_sw_pixels + sv.save_cc_pixels + $
                        sv.save_ea_pixels + sv.save_es_pixels)
npe = n_pixels_et    * (ev.save_er_pixels)
npi = n_pixels_infil * (iv.save_v0_pixels + iv.save_I_pixels + $
                        iv.save_q0_pixels + iv.save_Zw_pixels)
npg = n_pixels_gw    * (gv.save_ht_pixels + gv.save_df_pixels + $
                        gv.save_dt_pixels)

;*** nm  =   ;(number of monitored pixels)
nm  = 1UL
fac = 2UL   ;(factor for diff. between ASCII & binary)
  ;*** CHECK THIS ***

total = total + (bpp * nm * fac)*(npc + nps + npe + npi + npg)

RETURN, total

end;  Total_Outfile_Size
;***************************************************************
pro Get_Run_Vars, run_vars

;--------------------------------
;7/13/06.  Try to get DEM prefix
;--------------------------------
directory = Current_Directory()
files = file_search('*_DEM.rtg')   ;(searches current directory)
if (files[0] ne '') then begin
    len    = strlen(files[0])
    prefix = strmid(files[0],0,(len-8)) 
endif else begin
    prefix = 'Null'
endelse

;---------------------------------
;Get default run prefix (7/15/06)
;using files in current directory
;---------------------------------
Get_Run_Prefix, run_prefix 

;-----------------------------------
;Store run variables in a structure
;-----------------------------------
run_vars = {   $
prefix:          prefix,      $
run_prefix:      run_prefix,  $
directory:       directory,   $
log_file:        run_prefix + '_LOG.txt',    $
comment_file:    run_prefix + '_README.txt', $
;---------------------------------------
prefix_ID:       0L, $
run_prefix_ID:   0L, $
directory_ID:    0L, $
log_file_ID:     0L, $
comment_file_ID: 0L, $
comments_ID:0L }

end;  Get_Run_Vars
;******************************************************************
pro Get_Grid_Vars, grid_vars, prefix, directory

;-------------------------------------------------
;Notes:  Prefix will always be 'Null' when this

;        routine is first called, but now default
;        filenames in wizard panels get updated
;        when the prefix is changed in the Run
;        Info dialog.

;        Prefix, run_prefix and directory, etc.
;        are now saved in a run_vars structure.
;-------------------------------------------------

;-------------------------------
;Read grid info from RTI file ?
;-------------------------------
;if (n_elements(prefix) ne 0) AND (prefix ne 'Null') then begin
;    RTI_file = (prefix + '.rti')
;    Read_RTI_File, RTI_file, info
;endif else begin
    ;--------------------------------
    ;Try to get prefix from DEM name
    ;--------------------------------
;    directory = Current_Directory()
    ;*** result = findfile('*_DEM.rtg', count=n)
;    result = file_search('*_DEM.rtg', count=n)
;    if (n ne 0) then begin
;        file = result[0]
;        Get_RTI_Filename, file, dummy, PREFIX=prefix
;    endif else begin
;        prefix = 'Null'
;    endelse

;    info = {ncols:1L, nrows:1L, xres:1d, yres:1d, $
;            data_type:'INTEGER', pixel_geom:0b}
;endelse

info = {ncols:1L, nrows:1L, xres:1d, yres:1d, $
        data_type:'INTEGER', pixel_geom:0b, $
        byte_order:'LSB'}

prefix = 'Null'

;----------------------------------
;Min_dx is set more accurately by
;the Read_Run_Info_Panel procedure
;and is used for Courant condition
;----------------------------------
if (info.pixel_geom eq 0) then begin
    min_dx = info.xres * (92.6 / 3d) * 0.5d   ;[meters]
endif else min_dx = info.xres

grid_vars = { $
RTI_file:       prefix + '.rti', $
ncols:          info.ncols,  $

nrows:          info.nrows,  $
xres:           info.xres,   $
yres:           info.yres,   $
data_type:      info.data_type,  $     ;(of the DEM)
pixel_geom:     info.pixel_geom, $
byte_order:     info.byte_order, $
min_dx:         min_dx, $
;-----------------------------
;Used by the Basin Info panel
;-----------------------------
basin_table_ID: 0L, $
outlet_IDs:     ptr_new(0L), $
basin_areas:    ptr_new(0.0), $
basin_reliefs:  ptr_new(0.0), $
;-----------------------------
;Move these to precip_vars ??
;-----------------------------
RTM_file:       prefix + '_basin.rtm', $
RTM_file_ID:    0L, $
get_pvolume:    1b, $
get_pvolume_ID: 0L  }  ;(use RTM file to compute volume in?)

;----------------------------------------------------
;NB!  Flow_grid_file was moved to the channel vars.
;     structure and DEM_file to gw_vars.  The others
;     here were used for an experimental Grid_Info
;     panel.  Area and order grids are only used for
;     preprocessing
;----------------------------------------------------
;flow_grid_file: prefix + '_flow.rtg',  $
;DEM_file:       prefix + '_DEM.rtg',   $
;slope_file:     prefix + '_slope.rtg', $
;area_file:      prefix + '_area.rtg',  $
;order_file:     prefix + '_order.rtg', $
;-----------------------------------------

;flow_grid_file_ID: 0L, $
;DEM_file_ID:       0L, $
;slope_file_ID:     0L, $
;area_file_ID:      0L, $
;order_file_ID:     0L, $
;---------------------------
;ncols_ID:       0L, $    ;(text box IDs)
;nrows_ID:       0L, $
;xres_ID:        0L, $

;yres_ID:        0L, $
;data_type_ID:   0L, $    ;(droplist IDs)
;pixel_geom_ID:  0L, $
;xres_units_ID:  0L, $
;yres_units_ID:  0L  }

END;  Get_Grid_Vars
;******************************************************************
pro Get_Stop_Vars, stop_vars

stop_vars = { $
method: 0b, method_ID: 0L, $
;----------------------------
Qp_fraction:  0.05d, $
T_stop_model: 20d, $     ;[minutes]
T_stop_real:  20d, $     ;[minutes]
n_steps:      100L }

END;  Get_Stop_Vars
;******************************************************************
pro Get_Precip_Vars, precip_vars

;-------------------------------------------------------------
;Notes:  Duration_sums are computed by Initialize_Precip_Vars
;        7/15/05. Modified to support new GUI_Precip.
;-------------------------------------------------------------
;NB!     If default duration is too small (e.g. 2 min
;        vs. 60), then a run with infiltration turned
;        on may fail since virtually all of the water
;        is absorbed into the soil.  Remember that
;        initial flow depths default to 0 everywhere.
;-----------------------------------------------------
rate0 = (180d / (1000d * 3600d))  ;[mm/hr] -> [m/s]

;-----------------
;Define structure
;-----------------
precip_vars = { $
method:   1b, method_ID: 0L, $
dt:      60d, $      ;(is set to cv.dt by Route_Flow; OBSOLETE?)
max_rate: 0d, $      ;(updated by Precipitation function)
all_scalars: 1b, $   ;(3/19/07)
;-------------------------------------------------
rates:     ptr_new(rate0), rate_file: '',     $
durations: ptr_new(60.0),  duration_file: '', $
;-------------------------------------------------
rate_type:     0b, rate_unit:     0L, $
duration_type: 0b, duration_unit: 0L  }


;-------------------------------
;These are now only in met_vars
;-------------------------------
;T_air:     ptr_new(20.0),  T_air_file: '',    $
;T_air_type:    0b, T_air_unit:    0L  }
;-------------------------------------------
;These were once used by GUI_Uniform_Precip
;-------------------------------------------
;u_rates:     ptr_new(rate0), $
;u_durations: ptr_new(60.0) }
;--------------------------------
;These are now only in snow_vars
;--------------------------------
;h_swe:ptr_new(0.0), h_swe_type:0b, $
;h_swe_file:'', h_swe_unit:0L }

END;  Get_Precip_Vars
;******************************************************************
pro Get_Channel_Vars, channel_vars, prefix

;-----------------------------------------------------------
;Notes:  This is first called without the prefix argument.
;        The grid filenames below are set using the data
;        prefix by the Read_Run_Info_Panel routine.

;NB!     (3/16/07) dt is now set to zero initially, so
;        that Read_Run_Info_Panel can determine if user
;        has set the value (e.g. by loading a saved value).
;        If unset, then it is set by Read_Run_Info_Panel.
;        If GUI is not used, it is set in the input file.
;-----------------------------------------------------------
  
;----------------
;Initialize vars
;----------------
if (n_elements(prefix) eq 0) then prefix = 'Null'

;-------------------------------------
;Get the default filenames since most
;of the variables are static grids 
;-------------------------------------
code_file  = prefix + '_flow.rtg'
slope_file = prefix + '_slope.rtg'
nval_file  = prefix + '_chan-n.rtg'
width_file = prefix + '_chan-w.rtg'
angle_file = prefix + '_chan-a.rtg'
z0val_file = prefix + '_chan-z0.rtg'
d0_file    = prefix + '_d0.rtg'
sinu_file  = prefix + '_sinu.rtg'

;-----------------
;Define structure
;-----------------
channel_vars = { $
method: 1b, method_ID: 0L, $
dt:     0d, $  ;[secs]              (see notes above)
;--------------------------------
;Computed variables (future use)
;--------------------------------
;Q:  ptr_new(0.0), $
;u:  ptr_new(0.0), $
;d:  ptr_new(0.0), $
;f:  ptr_new(1.0), $
;------------------------------
kinematic_wave: 1b, $
diffusive_wave: 0b, $
dynamic_wave:   0b, $
manning:        1b, $
law_of_wall:    0b, $
;-----------------------------------------------------
codes:  ptr_new(0b),     code_file:code_file,   $
slopes: ptr_new(0.001d), slope_file:slope_file, $
nvals:  ptr_new(0.03d),  nval_file:nval_file,   $
widths: ptr_new(3d),     width_file:width_file, $
angles: ptr_new(45d),    angle_file:angle_file, $
d0:     ptr_new(0d),     d0_file:d0_file,       $    ;(initial depth)
sinu:   ptr_new(1d),     sinu_file:sinu_file,   $    ;(sinuosity)
z0vals: ptr_new(0.01d),  z0val_file:z0val_file, $
;----------------------------------------------------
code_type:  2b, code_unit:  0L, $
slope_type: 2b, slope_unit: 0L, $
nval_type:  2b, nval_unit:  0L, $
width_type: 2b, width_unit: 0L, $
angle_type: 2b, angle_unit: 0L, $
d0_type:    0b, d0_unit:    0L, $
sinu_type:  0b, sinu_unit:  0L, $
z0val_type: 2b, z0val_unit: 0L, $
;----------------------------------
save_grid_dt:   60d, $
save_Q_grids:   0b,  Q_rts_file:'', Q_rts_unit:0L, $
save_u_grids:   0b,  u_rts_file:'', u_rts_unit:0L, $
save_d_grids:   0b,  d_rts_file:'', d_rts_unit:0L, $
save_f_grids:   0b,  f_rts_file:'', f_rts_unit:0L, $
;----------------------------------------------------
save_pixels_dt: 60d, $
save_Q_pixels:  1b,  Q_out_file:'', Q_out_unit:0L, $
save_u_pixels:  0b,  u_out_file:'', u_out_unit:0L, $
save_d_pixels:  0b,  d_out_file:'', d_out_unit:0L, $
save_f_pixels:  0b,  f_out_file:'', f_out_unit:0L }

END;  Get_Channel_Vars
;******************************************************************
pro Get_Met_Vars, met_vars

;------------------------------------------------------------
;NOTES:  This procedure returns a structure that contains
;        meteorological variables, and possibly any other
;        variable that is needed by multiple processes.
;        They were previously included redundantly for each
;        process that needed them.

;        **************************************************
;        Do we ever need to distinguish between a surface
;        temperature and snow temperature (in the snow) ?
;        Recall that a separate T_soil_x variable is used
;        to compute Qc.
;        **************************************************

;        Vapor_Pressure function is defined in Qnet_file.pro
;        and accepts temperature argument, in Celsius, and
;        relative humidity.  It returns vapor pressure in
;        units of kPa.

;        Cp_snow is from NCAR CSM Flux Coupler web page

;        uz = wind speed, z = reference height above ground

;        rho_H2O is currently not adjustable with GUI.
;------------------------------------------------------------
FORWARD_FUNCTION Vapor_Pressure, Saturation_Vapor_Pressure

;----------------------
;Compute some defaults
;----------------------
RH     = 0.50  ;(relative humidity)
T_air  = 20.0  ;[deg C]
e_air  = Vapor_Pressure(T_air, RH)    ;[kPa]
e_air  = (e_air * 10.0)               ;[kPa -> mbars]
;-----------------------------------------------------
T_surf = -5.0  ;[deg C]
e_surf = Saturation_Vapor_Pressure(T_surf)  ;[kPa]

;-----------------
;Define structure
;-----------------
met_vars = { $
rho_H2O:  ptr_new(1000d),    $    ;[kg/m^3 at T=????]
rho_air:  ptr_new(1.2614),   $    ;[kg/m^3 at T=280K]
Cp_air:   ptr_new(1005.7),   $    ;[J/kg/K at T=280K]
;-----------------------------------------------------
Qn_SW:    ptr_new(100.0),    Qn_SW_file:'',    $   ;(Qnet SW)
Qn_LW:    ptr_new(10.0),     Qn_LW_file:'',    $   ;(Qnet LW)
T_air:    ptr_new(T_air),    T_air_file:'',    $   ;(air temp.)
T_surf:   ptr_new(T_surf),   T_surf_file:'',   $   ;(surf. temp)
RH:       ptr_new(RH),       RH_file:'',       $   ;(rel. humidity)
p0:       ptr_new(1000.0),   p0_file:'',       $   ;(atm. pressure)
uz:       ptr_new(3.0),      uz_file:'',       $   ;(wind speed at z)
z:        ptr_new(10.0),     z_file:'',        $   ;(wind ref. height)
z0_air:   ptr_new(.02),      z0_air_file:'',   $   ;(roughness height)
;-------------------------------------------------
Qn_SW_type:   0b,      Qn_SW_unit:    0L, $
Qn_LW_type:   0b,      Qn_LW_unit:    0L, $
T_air_type:   0b,      T_air_unit:    0L, $
T_surf_type:  0b,      T_surf_unit:   0L, $
RH_type:      0b,      RH_unit:       0L, $
p0_type:      0b,      p0_unit:       0L, $
uz_type:      0b,      uz_unit:       0L, $
z_type:       0b,      z_unit:        0L, $   ;(always scalar now)
z0_air_type:  0b,      z0_air_unit:   0L, $   ;(always scalar now)
;-------------------
;Computed variables
;----------------------------------------
;See Latent_Heat_Flux and Vapor_Pressure
;functions in formulas.pro.
;----------------------------------------
e_air:    ptr_new(e_air),  $   ;[mbars]
e_surf:   ptr_new(e_surf)  }   ;[mbars]


;--------------------------------------------
;(3/14/07) These are now only in snow_vars
;--------------------------------------------
;Snow variables shared between snowmelt & ET
;--------------------------------------------
;h0_snow:  ptr_new(h0_snow),  h0_snow_file:'',  $    ;(initial value)
;h0_swe:   ptr_new(h0_swe),   h0_swe_file:'',   $    ;(initial value)
;rho_snow: ptr_new(300d),     rho_snow_file:'', $    ;[kg/m^3 at T=???K]
;Cp_snow:  ptr_new(2090.0),   $    ;[J/kg/K at T=???K]
;--------------------------------------------------
;h0_snow_type: 0b,      h0_snow_unit:  0L, $
;h0_swe_type:  0b,      h0_swe_unit:   0L, $
;rho_snow_type: 0b,     rho_snow_unit: 0L  }

end;  Get_Met_Vars
;******************************************************************
pro Get_Snow_Vars, snow_vars

;------------------------------------------------------------
;NOTES:  Vapor_Pressure function is defined in formulas.pro
;        and accepts temperature argument, in Celsius, and
;        relative humidity.  It returns vapor pressure in
;        units of kPa.

;        rho_H2O, Cp_snow, rho_air and Cp_air are currently
;        hardwired (not adjustable with GUI).

;        Cp_snow is from NCAR CSM Flux Coupler web page

;        hs = snow depth
;        sw = snow water equivalent (depth)
;        mr = snow melt rate
;        ea = vapor pressure, air
;        es = vapor pressure, surf (and saturation)
;        cc = cold content     
;------------------------------------------------------------
FORWARD_FUNCTION Vapor_Pressure

;------------------------------
;Set up default h_snow & h_swe
;------------------------------
h0_snow  = 0.5    ;[m]
rho_H2O  = 1000d
rho_snow = 300d
h0_swe   = h0_snow * (rho_snow / rho_H2O)

;-----------------
;Define structure
;-----------------
snow_vars = { $
method:   0b,  method_ID: 0L, $
dt:       3600d, $   ;[sec]
all_scalars: 1b, $   ;(3/19/07)
;-----------------------------------------------------
Cp_snow:  ptr_new(2090.0),   $    ;[J/kg/K at T=???K]
;-----------------------------------------------------
c0:       ptr_new(2.7),      c0_file:'', $
T0:       ptr_new(-0.2),     T0_file:'', $
h0_snow:  ptr_new(h0_snow),  h0_snow_file:'',  $    ;(initial value)
h0_swe:   ptr_new(h0_swe),   h0_swe_file:'',   $    ;(initial value)
rho_snow: ptr_new(rho_snow), rho_snow_file:'', $    ;[kg/m^3 at T=???K]
;-------------------------------------------------
c0_type:      0b,      c0_unit:       0L, $
T0_type:      0b,      T0_unit:       0L, $
h0_snow_type: 0b,      h0_snow_unit:  0L, $
h0_swe_type:  0b,      h0_swe_unit:   0L, $
rho_snow_type: 0b,     rho_snow_unit: 0L, $
;--------------------------------------------
;NB! e_air and e_surf themselves are stored
;in the "met_vars" structure
;--------------------------------------------
save_grid_dt:   60d, $
save_mr_grids:   0b,  mr_RTS_file:'', mr_RTS_unit:0L, $
save_hs_grids:   0b,  hs_RTS_file:'', hs_RTS_unit:0L, $
save_sw_grids:   0b,  sw_RTS_file:'', sw_RTS_unit:0L, $
save_cc_grids:   0b,  cc_RTS_file:'', cc_RTS_unit:0L, $
save_ea_grids:   0b,  ea_RTS_file:'', ea_RTS_unit:0L, $    ;(e_air)
save_es_grids:   0b,  es_RTS_file:'', es_RTS_unit:0L, $    ;(e_surf)
;--------------------------------------------------------
save_pixels_dt: 60d, $
save_mr_pixels:  0b,  mr_out_file:'', mr_out_unit:0L, $
save_hs_pixels:  0b,  hs_out_file:'', hs_out_unit:0L, $
save_sw_pixels:  0b,  sw_out_file:'', sw_out_unit:0L, $
save_cc_pixels:  0b,  cc_out_file:'', cc_out_unit:0L, $
save_ea_pixels:  0b,  ea_out_file:'', ea_out_unit:0L, $    ;(e_air)
save_es_pixels:  0b,  es_out_file:'', es_out_unit:0L, $    ;(e_surf)
;-------------------
;Computed variables
;-------------------
Ecc:      ptr_new(0.0),     $   ;(cold content, [J/m^2])
h_snow:   ptr_new(h0_snow), $   ;(snow depth, [m])
h_swe:    ptr_new(h0_swe)   }   ;(snow water equiv., [m])


END;  Get_Snow_Vars
;******************************************************************
pro Get_ET_Vars, ET_vars

;----------------------------------------------------
;Notes:  h_snow is needed by the Bulk_Exchange_Coeff
;        function to adjust reference height, z.
;        Do we need h0_snow here ??
;----------------------------------------------------

;-----------------
;Define structure
;-----------------
ET_vars = { $
method:   0b,  method_ID: 0L, $
dt:       3600d, $   ;[secs]
all_scalars: 1b, $   ;(3/19/07)
;------------------------------------------------
alpha:    ptr_new(1.2d),   alpha_file:'',     $
Ks:       ptr_new(0.45),   Ks_file:'',        $
soil_x:   ptr_new(0.05),   soil_x_file:'',    $
T_soil_x: ptr_new(0.0),    T_soil_x_file: '', $
;------------------------------------------------
alpha_type:    0b,    alpha_unit:    0L, $
Ks_type:       0b,    Ks_unit:       0L, $
soil_x_type:   0b,    soil_x_unit:   0L, $
T_soil_x_type: 0b,    T_soil_x_unit: 0L, $
;-------------------------------------------
save_grid_dt:   60d, $
save_er_grids:   0b,  er_RTS_file:'', er_RTS_unit:0L, $
;-------------------------------------------------------
save_pixels_dt: 60d, $
save_er_pixels:  0b,  er_out_file:'', er_out_unit:0L }


END;  Get_ET_Vars
;******************************************************************
pro Get_GW_Vars, GW_vars, prefix, run_prefix

;-----------------------------------------------------------
;NOTES:  h0_table = init. elevation of water table [m]
;        d_thaw    = depth to nonfrozen soil [m]
;                  (everything below is thawed)

;NB!     (3/16/07) dt is now set to zero initially, so
;        that Read_Run_Info_Panel can determine if user
;        has set the value (e.g. by loading a saved value).
;        If unset, then it is set by Read_Run_Info_Panel.
;        If GUI is not used, it is set in the input file.
;-----------------------------------------------------------
if (n_elements(prefix) eq 0) then prefix = 'Null'
if (n_elements(run_prefix) eq 0) then run_prefix='Null'

;------------------------------
;Set max number of soil layers
;------------------------------
;** nlayers = 10
nlayers = 6

;-------------------------------------
;Get the default filenames since elev
;and h0_table are both grids
;-------------------------------------
elev_file      = (prefix + '_DEM.rtg')
;-----------------------------------------------
;d_bedrock_file = (prefix + '_dbedrock.rtg')
;h0_table_file  = (run_prefix + '_H2Otable.rtg') 
;d_freeze_file = (run_prefix + '_dfreeze.rtg')
;d_thaw_file   = (run_prefix + '_dthaw.rtg')

;---------------------------------------------------
;Get default filenames for new, more general method
;---------------------------------------------------
Ks_file = strarr(nlayers)
qs_file = strarr(nlayers)
th_file = strarr(nlayers)
for j=0,(nlayers-1) do begin
    jstr = strtrim(string(j), 2)
    Ks_file[j] = (run_prefix + '_Ks' + jstr + '.rtg')
    qs_file[j] = (run_prefix + '_qs' + jstr + '.rtg')
    th_file[j] = (run_prefix + '_th' + jstr + '.rtg')
endfor

;-----------------------------------
;Get pointer arrays for soil layers
;and assign initial, scalar values
;-----------------------------------
Ks_ptr = ptrarr(nlayers, /ALLOCATE_HEAP)
qs_ptr = ptrarr(nlayers, /ALLOCATE_HEAP)
th_ptr = ptrarr(nlayers, /ALLOCATE_HEAP)

for j=0,(nlayers-1) do begin
    if (j eq 0) then Kval=5e-5 else Kval=0d
    *(Ks_ptr[j]) = Kval

    *(qs_ptr[j]) = 0.5d
    *(th_ptr[j]) = 0.1d
endfor

;-----------------
;Define structure
;-----------------
GW_vars = { $
method:     0b,  method_ID: 0L, $
dt:         0d,   $   ;[secs]        ;(see note above)
nlayers:    nlayers, $
h_table:    ptr_new(1.0), $   ;[meters]
;-------------------------------------------------------------
elev:       ptr_new(1.0),    elev_file:elev_file, $
h0_table:   ptr_new(0.0),    h0_table_file:'',  $
d_bedrock:  ptr_new(0.0),    d_bedrock_file:'', $
d_freeze:   ptr_new(0.0),    d_freeze_file:'',  $
d_thaw:     ptr_new(9999.0), d_thaw_file:'',    $
;-------------------------------------------------------------
elev_type:      2b,    elev_unit:      0L, $
h0_table_type:  0b,    h0_table_unit:  0L, $
d_bedrock_type: 0b,    d_bedrock_unit: 0L, $
d_freeze_type:  0b,    d_freeze_unit:  0L, $  ;***************
d_thaw_type:    0b,    d_thaw_unit:    0L, $
;---------------------------------------------------------
Ks:        Ks_ptr,           Ks_file:Ks_file,         $
Ks_type:   bytarr(nlayers),  Ks_unit:lonarr(nlayers), $
;---------------------------------------------------------
qs:        qs_ptr,           qs_file:qs_file,         $
qs_type:   bytarr(nlayers),  qs_unit:lonarr(nlayers), $
;---------------------------------------------------------
th:        th_ptr,           th_file:th_file,         $
th_type:   bytarr(nlayers),  th_unit:lonarr(nlayers), $
;---------------------------------------------------------
save_grid_dt:   60d, $
save_ht_grids:   0b,  ht_RTS_file:'', ht_RTS_unit:0L, $
save_sm_grids:   0b,  sm_RTS_file:'', sm_RTS_unit:0L, $
save_df_grids:   0b,  df_RTS_file:'', df_RTS_unit:0L, $
save_dt_grids:   0b,  dt_RTS_file:'', dt_RTS_unit:0L, $
;--------------------------------------------------------
save_pixels_dt: 60d, $
save_ht_pixels:  0b,  ht_out_file:'', ht_out_unit:0L, $
save_sm_pixels:  0b,  sm_out_file:'', sm_out_unit:0L, $
save_df_pixels:  0b,  df_out_file:'', df_out_unit:0L, $
save_dt_pixels:  0b,  dt_out_file:'', dt_out_unit:0L  }

END;  Get_GW_Vars
;******************************************************************
pro Get_Infil_Vars, infil_vars, run_prefix

;---------------------------------------------------------
;Notes:  Default settings are average for 'Loam', as
;        returned by the Get_Soil_Params routine.
;        Vars in 2nd line are set fairly arbitrary.

;        eta = (2 + (3*lambda)) and needs to be
;        updated whenever lambda is updated, as in
;        Update_Infil_Vars.  We want to avoid
;        recomputing it everywhere it is used in
;        order to save time.

;        The vars computed by Richards' method are
;        set to scalar 0d below, but they are set to
;        arrays by Route_Flow according to user choices.
;---------------------------------------------------------
;NB!     soil types are only used to pass defaults to
;        the droplists in the GUI.  "soil_type" field
;        is only used by the non-Richards routines.

;NB!     dz is a pointer, but dz1, dz2 & dz3 are scalars.
;        For multiple soil layers, we build a 1D array
;        for dz from the others.
;---------------------------------------------------------
if (n_elements(run_prefix) eq 0) then run_prefix='Null'

;------------------------------
;Set max number of soil layers
;------------------------------
n_layers = 3

soil_types = strarr(n_layers) + 'clay_loam'
soil_types[0] = 'silt_loam'
soil_types[1] = 'loam'

;------------------------------------
;Get default params for each layer
;Get_Soil_Params is in GUI_infil.pro
;------------------------------------
Get_Soil_Params, soil_types[0], phi1, Ks1, pB1, b1, lam1, eta1, qs1, G1, $
                 qi1, Ki1, qr1, c1, pA1, dz1, nz1
Get_Soil_Params, soil_types[1], phi2, Ks2, pB2, b2, lam2, eta2, qs2, G2, $
                 qi2, Ki2, qr2, c2, pA2, dz2, nz2
Get_Soil_Params, soil_types[2], phi3, Ks3, pB3, b3, lam3, eta3, qs3, G3, $
                 qi3, Ki3, qr3, c3, pA3, dz3, nz3

;---------------------------------------
;Compute min soil moisture from min psi
;-------------------------------------------------
;Don’t do this here.  It will just be overwritten
;by the Initialize_Infil_Vars routine.
;-------------------------------------------------
;** qH1 = Theta_Min(qs1, qr1, pB1, pA1, c1, lam1)
;** qH2 = Theta_Min(qs2, qr2, pB2, pA2, c2, lam2)
;** qH3 = Theta_Min(qs3, qr3, pB3, pA3, c3, lam3)

;---------------------------------------------------
;Get default filenames for new, more general method
;---------------------------------------------------
Ks_file  = strarr(n_layers)
Ki_file  = strarr(n_layers)
qs_file  = strarr(n_layers)
qi_file  = strarr(n_layers)
qr_file  = strarr(n_layers)
pB_file  = strarr(n_layers)
pA_file  = strarr(n_layers)
lam_file = strarr(n_layers)
c_file   = strarr(n_layers)
;------------------------------
for j=0, (n_layers-1) do begin
    jstr = strtrim(string(j+1), 2)
    Ks_file[j]  = (run_prefix + '_Ks'  + jstr + '.rtg')
    Ki_file[j]  = (run_prefix + '_Ki'  + jstr + '.rtg')
    qs_file[j]  = (run_prefix + '_qs'  + jstr + '.rtg')
    qi_file[j]  = (run_prefix + '_qi'  + jstr + '.rtg')
    qr_file[j]  = (run_prefix + '_qr'  + jstr + '.rtg')
    pB_file[j]  = (run_prefix + '_pB'  + jstr + '.rtg')
    pA_file[j]  = (run_prefix + '_pA'  + jstr + '.rtg')
    lam_file[j] = (run_prefix + '_lam' + jstr + '.rtg')
    c_file[j]   = (run_prefix + '_c'   + jstr + '.rtg')
endfor

;-----------------------------------
;Get pointer arrays for soil layers
;and assign initial, scalar values
;-----------------------------------
Ks_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
Ki_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
qs_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
qi_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
qr_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
pB_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
pA_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)
lam_ptr = ptrarr(n_layers, /ALLOCATE_HEAP)
c_ptr   = ptrarr(n_layers, /ALLOCATE_HEAP)
;----------------------------------------------
;Note:  These two are computed from the others
;----------------------------------------------
eta_ptr = ptrarr(n_layers, /ALLOCATE_HEAP)
qH_ptr  = ptrarr(n_layers, /ALLOCATE_HEAP)

;---------------------------------------
;Get corresponding arrays for dz and nz
;---------------------------------------
dz_vals = dblarr(n_layers) + dz3
dz_vals[0] = dz1
dz_vals[1] = dz2
;---------------------------------
nz_vals = intarr(n_layers) + nz3
nz_vals[0] = nz1
nz_vals[1] = nz2
nz_total = fix(total(nz_vals))      ;***********

;---------------------------------------
;Initialize all layers same as layer 3
;---------------------------------------
for j=0,(n_layers-1) do begin
    *(Ks_ptr[j])  = Ks3
    *(Ki_ptr[j])  = Ki3
    *(qs_ptr[j])  = qs3
    *(qi_ptr[j])  = qi3
    *(qr_ptr[j])  = qr3
    *(pB_ptr[j])  = pB3
    *(pA_ptr[j])  = pA3
    *(lam_ptr[j]) = lam3
    *(c_ptr[j])   = c3
endfor
;------------------------
;Would this also work ?
;------------------------
;*(Ks_ptr)  = Ks3
;*(Ki_ptr)  = Ki3
;*(qs_ptr)  = qs3
;*(qi_ptr)  = qi3
;*(qr_ptr)  = qr3
;*(pB_ptr)  = pB3
;*(pA_ptr)  = pA3
;*(lam_ptr) = lam3
;*(c_ptr)   = c3
;------------------------------
;Set top 2 layers to defaults
;------------------------------
*(Ks_ptr[0])  = Ks1
*(Ki_ptr[0])  = Ki1
*(qs_ptr[0])  = qs1
*(qi_ptr[0])  = qi1
*(qr_ptr[0])  = qr1
*(pB_ptr[0])  = pB1
*(pA_ptr[0])  = pA1
*(lam_ptr[0]) = lam1
*(c_ptr[0])   = c1
;---------------------
*(Ks_ptr[1])  = Ks2
*(Ki_ptr[1])  = Ki2
*(qs_ptr[1])  = qs2
*(qi_ptr[1])  = qi2
*(qr_ptr[1])  = qr2
*(pB_ptr[1])  = pB2
*(pA_ptr[1])  = pA2
*(lam_ptr[1]) = lam2
*(c_ptr[1])   = c2

;----------------
;Vars used below
;----------------
byte_zeros = bytarr(n_layers)
long_zeros = lonarr(n_layers)

;-----------------
;Define structure
;-----------------
infil_vars = { $
method:  0b,   method_ID: 0L, $
dt:      60d,  $       ;[sec]
all_scalars: 1b, $     ;(3/19/07)

;******************************************************
;NOTE:  Some non-Richards infil routines may still
;       expect that Ks_unit, etc. are scalars !!
;******************************************************

;--------------------------------------------
;Vars used only by non-Richards infiltration
;Note that Ks, Ki, qs and qi are shared.
;--------------------------------------------
G:        ptr_new(G1),     G_file:'',   $  ;[meters]
G_type:   0b,              G_unit:0L,   $
;----------------------------------------------------------
gam:      ptr_new(0.82d),  gam_file:'',    $  ;[unitless]
gam_type: 0b,              gam_unit:  0L,  $
;----------------------------------------------------------
tp:       ptr_new(-1d),                $  ;[seconds]  (time to ponding)
fp:       ptr_new(0d),                 $  ;[m/s]      (infil. rate @ ponding)
I:        ptr_new(0.000001d),          $  ;[m]        (tot. infil. depth)
;----------------------------------
;Vars for Richards equation method
;----------------------------------
n_layers: n_layers, $
Zw:      ptr_new(0d),      $   ;[m]    (wetting front depth)
z:       ptr_new(0d),      $   ;[m]    (z-values; set by Route_Flow)
;-------------------------------------------------------------------
q:       ptr_new(qi),      $   ;[unitless]  (soil moisture, theta)
p:       ptr_new(0d),      $   ;[unitless]  (pressure head, psi)
K:       ptr_new(Ki),      $   ;[m/s]
v:       ptr_new(0d),      $   ;[m/s]
;---------------------------------------------------------
;For Richards method, these will be assembled from data
;for separate layers by Initialize_Infil_Vars.
;For other infil methods they may be used directly.
;----------------------------------------------------------
;Note:   Initialize_Infil_Vars will take care of qH later
;----------------------------------------------------------
Ks:      ptr_new(Ks1),     $   ;[m/s]
Ki:      ptr_new(Ki1),     $   ;[m/s]
qs:      ptr_new(qs1),     $   ;[unitless]
qi:      ptr_new(qi1),     $   ;[unitless]
qr:      ptr_new(qr1),     $   ;[unitless]
qH:      ptr_new(0d),      $   ;[unitless]
pB:      ptr_new(pB1),     $   ;[meters]
pA:      ptr_new(pA1),     $   ;[meters]
lam:     ptr_new(lam1),    $   ;[unitless]
c:       ptr_new(c1),      $   ;[unitless]
eta:     ptr_new(eta1),    $   ;[unitless]
dz:      ptr_new(dz1),     $   ;[m]          (node spacing, PTR)
nz:      nz_total,         $   ;[unitless]   (total # of nodes)
;----------------------------------------------------------
Ks_val:    Ks_ptr,       Ks_file:Ks_file,    $   ;[m/s]
Ks_type:   byte_zeros,   Ks_unit:long_zeros, $
;----------------------------------------------------------
Ki_val:    Ki_ptr,       Ki_file:Ki_file,    $   ;[m/s]
Ki_type:   byte_zeros,   Ki_unit:long_zeros, $
;----------------------------------------------------------
qs_val:    qs_ptr,       qs_file:qs_file,    $   ;[unitless]
qs_type:   byte_zeros,   qs_unit:long_zeros, $
;----------------------------------------------------------
qi_val:    qi_ptr,       qi_file:qi_file,    $   ;[unitless]
qi_type:   byte_zeros,   qi_unit:long_zeros, $
;----------------------------------------------------------
qr_val:    qr_ptr,       qr_file:qr_file,    $   ;[unitless]
qr_type:   byte_zeros,   qr_unit:long_zeros, $
;----------------------------------------------------------
pB_val:    pB_ptr,       pB_file:pB_file,    $   ;[meters]
pB_type:   byte_zeros,   pB_unit:long_zeros, $
;----------------------------------------------------------
pA_val:    pA_ptr,       pA_file:pA_file,    $   ;[meters]
pA_type:   byte_zeros,   pA_unit:long_zeros, $
;----------------------------------------------------------
lam_val:   lam_ptr,      lam_file:lam_file,   $  ;[unitless]
lam_type:  byte_zeros,   lam_unit:long_zeros, $
;----------------------------------------------------------
c_val:     c_ptr,        c_file:c_file,       $  ;[unitless]
c_type:    byte_zeros,   c_unit:long_zeros,   $
;----------------------------------------------------------
;These next two are computed from others and therefore
;don’t need file or unit.  What about type ?
;----------------------------------------------------------
qH_val:    qH_ptr,      $   ;[unitless]
;** qH_type:   byte_zeros,  $
;----------------------------------------
eta_val:   eta_ptr,     $  ;[unitless]
;** eta_type:  byte_zeros,  $
;-------------------------------------------------------------------
dz_val:    dz_vals,     $   ;[m]          (node spacing, doubles)
nz_val:    nz_vals,     $   ;[unitless]   (total # of nodes)
soil_type: soil_types,  $

;-------------------------
;Vars for saving output
;--------------------------------
;v0 = surface infiltration rate
;q0 = surface soil moisture
;I  = total infiltrated depth
;Zw = wetting front depth
;q  = soil moisture (theta)
;p  = pressure head (psi)
;K  = hydraulic conductivity
;v  = vertical flow rate
;--------------------------------
save_grid_dt:   60d, $
save_v0_grids:      0b,  v0_rts_file:'', v0_rts_unit:0L, $
save_q0_grids:      0b,  q0_rts_file:'', q0_rts_unit:0L, $
save_I_grids:       0b,  I_rts_file:'',  I_rts_unit:0L,  $
save_Zw_grids:      0b,  Zw_rts_file:'', Zw_rts_unit:0L, $ 
;-----------------------------------------------------------
save_pixels_dt: 60d, $
save_v0_pixels:     0b,  v0_out_file:'', v0_out_unit:0L, $
save_q0_pixels:     0b,  q0_out_file:'', q0_out_unit:0L, $
save_I_pixels:      0b,  I_out_file:'',  I_out_unit:0L,  $
save_Zw_pixels:     0b,  Zw_out_file:'', Zw_out_unit:0L, $
;--------------------------------------------------------------
save_stack_dt: 60d, $
save_q_stacks:      0b,  q_stack_file:'',  q_stack_unit:0L, $
save_p_stacks:      0b,  p_stack_file:'',  p_stack_unit:0L, $
save_K_stacks:      0b,  K_stack_file:'',  K_stack_unit:0L, $ 
save_v_stacks:      0b,  v_stack_file:'',  v_stack_unit:0L, $
;-----------------------------------------------------------------
save_profile_dt: 60d, $
save_q_profiles:    0b,  q_profile_file:'', q_profile_unit:0L, $
save_p_profiles:    0b,  p_profile_file:'', p_profile_unit:0L, $
save_K_profiles:    0b,  K_profile_file:'', K_profile_unit:0L, $
save_v_profiles:    0b,  v_profile_file:'', v_profile_unit:0L  }

END;  Get_Infil_Vars
;******************************************************************
pro Get_Overland_Vars, overland_vars

;-----------------
;Define structure
;-----------------
overland_vars = { $
method:  0b,  method_ID: 0L, $
dt:      3600d, $      ;[1 hour]
rate:    0d  }

END;  Get_Overland_Vars
;******************************************************************
pro Get_Sediment_Vars, sed_vars

;----------------------------------------
;NOTES: 
;----------------------------------------

;-----------------
;Define structure
;-----------------
sed_vars = { $
method:   0b,  method_ID: 0L, $
dt:       60d, $   ;[sec]
;------------------------------------------
E:        ptr_new(0.03), E_file:'', $
area:     ptr_new(1.0),  area_file:'', $
p1:       ptr_new(1.0),  $
slope:    ptr_new(1.0),  slope_file:'', $
p2:       ptr_new(2.0),  $
;------------------------------------------
relief:   ptr_new(1.0),  relief_file:'', $
temp:     ptr_new(15.0), temp_file:'',   $
;------------------------------------------
E_type:      0b,    E_unit:      0L, $
area_type:   0b,    area_unit:   0L, $
slope_type:  0b,    slope_unit:  0L, $
relief_type: 0b,    relief_unit: 0L, $
temp_type:   0b,    temp_unit:   0L }

END;  Get_Sediment_Vars
;******************************************************************
pro Get_Diversion_Vars, diversion_vars, prefix 

if (n_elements(prefix) eq 0) then prefix = 'Null'

;-----------------
;Define structure
;-----------------
diversion_vars = { $
method:   0b,  method_ID: 0L, $
;-------------------------------
use_sources: 0b, $
use_sinks:   0b, $
use_canals:  0b, $
;---------------------------------------
source_file: prefix + '_sources.txt', $
sink_file:   prefix + '_sinks.txt',   $
canal_file:  prefix + '_canals.txt'   }

END;  Get_Diversion_Vars
;******************************************************************
pro Read_Var, ptr, type, file, nx, ny, UNIT=unit, $
              GRID_TYPE=grid_type, FACTOR=FACTOR

if NOT(keyword_set(FACTOR)) then FACTOR=1d

case (type) of
    0 : begin
        ;-----------------------------------
        ;Scalar: Was entered by user in GUI
        ;-----------------------------------
        unit = -1L
        end
    1 : begin
        ;-----------------------------------------
        ;Time series: Read first scalar from file
        ;-----------------------------------------
        value = 0.0
        TF_Get_LUN, unit, file
        openr, unit, file 
        readf, unit, value   ;(ASCII, one per line)

        if (FACTOR ne 1d) then begin
            *ptr = value * FACTOR
        endif else begin
            *ptr = value
        endelse
        end
    2 : begin
        ;----------------------------------
        ;Single grid: Read grid from file
        ;Read_Grid accounts for byte order
        ;--------------------------------------------
        ;NB!  GRID_TYPE keyword allows DEM to be
        ;read for GW vars, which might not be FLOAT'
        ;--------------------------------------------
        if NOT(keyword_set(GRID_TYPE)) then grid_type='FLOAT'
        Read_Grid, grid, file, TYPE=grid_type, /SILENT
        if (FACTOR ne 1d) then begin
            *ptr = grid * FACTOR
        endif else begin
            *ptr = grid
        endelse
        unit = -1L    ;(nothing left to read)
        end
    3 : begin
        ;-----------------------------------------
        ;Grid sequence: Read first grid from file
        ;Need RTI file for the byte order issue.
        ;-----------------------------------------
        Get_RTI_Filename, file, RTI_file
        Read_RTI_File, RTI_file, info
        ;---------------------------------
        grid = fltarr(nx,ny)
        TF_Get_LUN, unit, file
        openr, unit, file, $
               SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
        readu, unit, grid     ;(don't close the file)
        if (FACTOR ne 1d) then begin
            *ptr = grid * FACTOR
        endif else begin
            *ptr = grid
        endelse
        end
endcase

end;  Read_Var
;*****************************************************************
pro Read_Next_Var, ptr, type, file, unit, FACTOR=FACTOR

;---------------------------------------------------------------
;Notes:  Type argument is 0, 1, 2 or 3 for scalar, time series,
;        grid or grid stack.  For scalar and grid, value does
;        not change in time and was set at start, outside of
;        the main loop.  For time series and grid sequence,
;        the first value is read outside of main loop and
;        stored with a pointer.
;---------------------------------------------------------------
if (type eq 0) OR (type eq 2) then RETURN
if NOT(keyword_set(FACTOR)) then FACTOR = 1d

;---------------------------
;Is input file still open ?
;---------------------------
if (n_elements(unit) eq 0) then RETURN

file_info = fstat(unit)
if (file_info.open eq 0b) then RETURN
if (EOF(unit)) then begin
    ;*** free_lun, unit
    RETURN
endif

;-----------------------------------
;Read next scalar or grid from file
;-----------------------------------
;Inherit data type of pointer var,
;so don't need nx and ny.
;-----------------------------------
;(3/20/07) Make sure to read FLOAT,
;especially with RTS file input.
;Do rainrates have type of DOUBLE?
;-----------------------------------
var = float(*ptr)

if (type eq 1) then readf, unit, var    ;(from text file)
if (type eq 3) then readu, unit, var    ;(from binary file)

;------------------------------------------
;Optional factor for unit conversion, etc.
;------------------------------------------
if (FACTOR ne 1d) then var = (var * FACTOR)

;----------------------
;Point to new variable
;----------------------
*ptr = var

end;  Read_Next_Var
;******************************************************************
pro Read_Precip_Vars_in_Files, pv, nx, ny

;-----------------------------------------------
;NOTES:  pv = precip_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.
;-----------------------------------------------
;NB!  PTR_NEW is used with the NO_COPY keyword.
;     This can cause some unexpected behavior;
;     is it what we want here ??
;     Instead of:
;         pv.T = ptr_new(T, /no_copy)
;     should we have:
;         *pv.T = T  ????
;     except twice the memory is used, at least
;     temporarily, for T.
;--------------------------------------------------------
;NB!  Rainrate units are assumed to be stored with units
;     of [mm/hr], but are converted to [m/s] prior to
;     storage in the data structure by Read_Var, via the
;     new FACTOR keyword.  (1/19/07)
;--------------------------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (pv.method eq 0) then RETURN
;----------------------------------------------------
Read_Var, pv.rates, pv.rate_type, pv.rate_file, $
          nx, ny, UNIT=unit, FACTOR=(1d / 3600000d)
pv.rate_unit = unit
;----------------------------------------------------
Read_Var, pv.durations, pv.duration_type,  $
          pv.duration_file, nx, ny, UNIT=unit
pv.duration_unit = unit

;-------------------------------
;(3/14/07) Now only in met_vars
;-------------------------------
;Read_Var, pv.T_air, pv.T_air_type, pv.T_air_file, $
;          nx, ny, UNIT=unit
;pv.T_air_unit = unit

END;  Read_Precip_Vars_in_Files
;******************************************************************
pro Read_Channel_Vars_in_Files, cv, nx, ny

;---------------------------------------
;NOTES:  cv = channel_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.
;-----------------------------------------------
;NB!  PTR_NEW is used with the NO_COPY keyword.
;     This can cause some unexpected behavior;
;     is it what we want here ??
;     Instead of:
;         pv.T = ptr_new(T, /no_copy)
;     should we have:
;         *pv.T = T  ????
;     except twice the memory is used, at least

;     temporarily, for T.
;-----------------------------------------------
FORWARD_FUNCTION File_Found

;-----------------------------
;Return if no method selected

;-----------------------------
if (cv.method eq 0) then RETURN

Read_Var, cv.slopes, cv.slope_type, cv.slope_file, nx, ny, UNIT=unit
cv.slope_unit = unit
Read_Var, cv.nvals,  cv.nval_type,  cv.nval_file,  nx, ny, UNIT=unit
cv.nval_unit  = unit
Read_Var, cv.widths, cv.width_type, cv.width_file, nx, ny, UNIT=unit
cv.width_unit = unit
Read_Var, cv.angles, cv.angle_type, cv.angle_file, nx, ny, UNIT=unit
cv.angle_unit = unit
Read_Var, cv.sinu,   cv.sinu_type, cv.sinu_file, nx, ny, UNIT=unit
cv.sinu_unit = unit
;*** if NOT(File_Found(cv.d0_file, /SILENT)) then RETURN
Read_Var, cv.d0, cv.d0_type, cv.d0_file, nx, ny, UNIT=unit
cv.d0_unit = unit


END;  Read_Channel_Vars_in_Files
;******************************************************************
pro Read_Met_Vars_in_Files, mv, nx, ny

;---------------------------------------
;NOTES:  mv = met_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.
;-----------------------------------------------------------------------
Read_Var, mv.Qn_SW,  mv.Qn_SW_type,  mv.Qn_SW_file,  nx, ny, UNIT=unit
mv.Qn_SW_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.Qn_LW,  mv.Qn_LW_type,  mv.Qn_LW_file,   nx, ny, UNIT=unit
mv.Qn_LW_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.T_air,  mv.T_air_type,  mv.T_air_file,  nx, ny, UNIT=unit
mv.T_air_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.T_surf, mv.T_surf_type, mv.T_surf_file, nx, ny, UNIT=unit
mv.T_surf_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.RH,     mv.RH_type,     mv.RH_file,     nx, ny, UNIT=unit
mv.RH_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.p0,     mv.p0_type,     mv.p0_file,     nx, ny, UNIT=unit
mv.p0_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.uz,     mv.uz_type,     mv.uz_file,     nx, ny, UNIT=unit
mv.uz_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.z,      mv.z_type,      mv.z_file,      nx, ny, UNIT=unit
mv.z_unit = unit
;-----------------------------------------------------------------------
Read_Var, mv.z0_air, mv.z0_air_type, mv.z0_air_file, nx, ny, UNIT=unit
mv.z0_air_unit = unit

END;  Read_Met_Vars_in_Files
;******************************************************************
pro Read_Snow_Vars_in_Files, sv, nx, ny

;---------------------------------------
;NOTES:  sv = snow_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.
;-----------------------------------------------
;NB!  PTR_NEW is used with the NO_COPY keyword.
;     This can cause some unexpected behavior;
;     is it what we want here ??
;     Instead of:
;         sv.T0 = ptr_new(T0, /no_copy)
;     should we have:
;         *sv.T0 = T0  ????
;     except twice the memory is used, at least
;     temporarily, for T0.
;-----------------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (sv.method eq 0) then RETURN
;-----------------------------------------------------------------------
Read_Var, sv.c0,     sv.c0_type,     sv.c0_file,     nx, ny, UNIT=unit
sv.c0_unit = unit
;-----------------------------------------------------------------------
Read_Var, sv.T0,     sv.T0_type,     sv.T0_file,     nx, ny, UNIT=unit
sv.T0_unit = unit
;-----------------------------------------------------------------------
Read_Var, sv.rho_snow, sv.rho_snow_type, sv.rho_snow_file, nx, ny, UNIT=unit
sv.rho_snow_unit = unit   ;(7/20/06  ********)
;-----------------------------------------------------------------------
Read_Var, sv.h0_snow, sv.h0_snow_type, sv.h0_snow_file, nx, ny, UNIT=unit
sv.h0_snow_unit = unit
;-----------------------------------------------------------------------
Read_Var, sv.h0_swe,  sv.h0_swe_type,  sv.h0_swe_file,  nx, ny, UNIT=unit
sv.h0_swe_unit = unit


;------------------------------------
;These are now in met_vars (3/14/07)
;-----------------------------------------------------------------------
;Read_Var, sv.Qn_SW,  sv.Qn_SW_type,  sv.Qn_SW_file,   nx, ny, UNIT=unit
;sv.Qn_SW_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.Qn_LW,  sv.Qn_LW_type,  sv.Qn_LW_file,   nx, ny, UNIT=unit
;sv.Qn_LW_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.T_air,  sv.T_air_type,  sv.T_air_file,  nx, ny, UNIT=unit
;sv.T_air_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.T_surf, sv.T_surf_type, sv.T_surf_file, nx, ny, UNIT=unit
;sv.T_surf_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.RH, sv.RH_type, sv.RH_file, nx, ny, UNIT=unit
;sv.RH_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.p0,     sv.p0_type,     sv.p0_file,     nx, ny, UNIT=unit
;sv.p0_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.uz,     sv.uz_type,     sv.uz_file,     nx, ny, UNIT=unit
;sv.uz_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.z,      sv.z_type,      sv.z_file,     nx, ny, UNIT=unit
;sv.z_unit = unit
;-----------------------------------------------------------------------
;Read_Var, sv.z0_air, sv.z0_air_type, sv.z0_air_file, nx, ny, UNIT=unit
;sv.z0_air_unit = unit

END;  Read_Snow_Vars_in_Files
;******************************************************************
pro Read_ET_Var, ev_ptr, ev_type, ev_file, sv_ptr, sv_file, $
                 nx, ny, UNIT=unit, COPY=copy

Read_Var, ev_ptr, ev_type, ev_file, nx, ny, UNIT=unit

;---------------------------------------------------------
;(3/14/07)  Don't need to consider this now since
;           all met_vars are in one structure
;---------------------------------------------------------
;Since many of the ET vars are also snow vars, later can
;save memory in cases where ET var is already available
;as a snow var.  For now, use duplicates for simpicity.
;Doing this is more complex than indicated below.
;---------------------------------------------------------
;if (ev_type ge 1) AND (ev_file eq sv_file) AND $
;   (ptr_valid(sv_ptr)) then begin
;    ptr_free, ev_ptr
;    copy = 1b
;    ;** ev_ptr = sv_ptr  ;(Does not get updated in ev structure.)
;    unit = -1L   ;******
;endif else begin
;    copy = 0b
;    Read_Var, ev_ptr, ev_type, ev_file, nx, ny, UNIT=unit
;endelse

end;  Read_ET_Var
;******************************************************************
pro Read_ET_Vars_in_Files, ev, sv, nx, ny 

;-----------------------------------
;NOTES:  ev = ET_vars   = structure
;        sv = snow_vars = structure
;-----------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (ev.method eq 0) then RETURN

;------------------------
;These are not snow vars
;------------------------
Read_ET_Var, ev.alpha, ev.alpha_type, ev.alpha_file, $
             0, 0, nx, ny, UNIT=unit
ev.alpha_unit = unit
;---------------------------------------------------------------
Read_ET_Var, ev.Ks, ev.Ks_type, ev.Ks_file, $
             0, 0, nx, ny, UNIT=unit
ev.Ks_unit = unit
;---------------------------------------------------------------
Read_ET_Var, ev.soil_x, ev.soil_x_type, ev.soil_x_file, $
             0, 0, nx, ny, UNIT=unit
ev.soil_x_unit = unit
;---------------------------------------------------------------
Read_ET_Var, ev.T_soil_x, ev.T_soil_x_type, ev.T_soil_x_file, $
             0, 0, nx, ny, UNIT=unit
ev.T_soil_x_unit = unit

;-------------------------------------------
;(3/14/07) These are now stored in met_vars
;--------------------------------------------------
;All of these are also snow vars, so use the snow
;var (via ptr) if possible, to avoid duplications.
;--------------------------------------------------
;NOTE: See Read_ET_Var; snow vars not used now.
;--------------------------------------------------
;Read_ET_Var, ev.Qn_SW,  ev.Qn_SW_type,  ev.Qn_SW_file, $
;             sv.Qn_SW,  sv.Qn_SW_file,  nx, ny, UNIT=unit
;ev.Qn_SW_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.Qn_LW,  ev.Qn_LW_type,  ev.Qn_LW_file, $
;             sv.Qn_LW,  sv.Qn_LW_file,  nx, ny, UNIT=unit
;ev.Qn_LW_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.T_air,  ev.T_air_type,  ev.T_air_file, $
;             sv.T_air,  sv.T_air_file,  nx, ny, UNIT=unit
;ev.T_air_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.T_surf, ev.T_surf_type, ev.T_surf_file, $
;             sv.T_surf, sv.T_surf_file, nx, ny, UNIT=unit
;ev.T_surf_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.RH,     ev.RH_type,     ev.RH_file, $
;             sv.RH,     sv.RH_file,     nx, ny, UNIT=unit
;ev.RH_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.p0,     ev.p0_type,     ev.p0_file, $
;             sv.p0,     sv.p0_file,     nx, ny, UNIT=unit
;ev.p0_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.uz,     ev.uz_type,     ev.uz_file, $
;             sv.uz,     sv.uz_file,     nx, ny, UNIT=unit
;ev.uz_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.z,     ev.z_type,     ev.z_file, $
;             sv.z,     sv.z_file,     nx, ny, UNIT=unit
;ev.z_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.z0_air, ev.z0_air_type, ev.z0_air_file, $
;             sv.z0_air, sv.z0_air_file, nx, ny, UNIT=unit
;ev.z0_air_unit = unit

;-------------------------------------------------
;(3/14/07) No longer necessary with new approach
;----------------------------------------------------------
;Read_ET_Var, ev.h0_snow, ev.h0_snow_type, ev.h0_snow_file, $
;             sv.h0_snow, sv.h0_snow_file, nx, ny, UNIT=unit
;ev.h0_snow_unit = unit
;----------------------------------------------------------
;Read_ET_Var, ev.h0_swe, ev.h0_swe_type, ev.h0_swe_file, $
;             sv.h0_swe, sv.h0_swe_file, nx, ny, UNIT=unit
;ev.h0_swe_unit = unit

END;  Read_ET_Vars_in_Files
;******************************************************************
pro Read_GW_Vars_in_Files, gv, nx, ny 

;-----------------------------------
;NOTES:  gv = gw_vars = structure
;-----------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (gv.method eq 0) then RETURN

;----------------------------------------------------------
;NOTE:  DEM is only RTG file that is currently allowed to
;       have a data type other than 'FLOAT', by using the
;       GRID_TYPE keyword to Read_Var as shown.
;----------------------------------------------------------
;*** TF_Print,'Reading elevation grid...'
if (gv.elev_type eq 2) then begin
    Get_RTI_Filename, gv.elev_file, RTI_file
    Read_RTI_File, RTI_file, info
    grid_type = info.data_type
endif
Read_Var, gv.elev, gv.elev_type, gv.elev_file, $
          nx, ny, UNIT=unit, GRID_TYPE=grid_type
gv.elev_unit = unit
;--------------------------------------------------------------
;*** TF_Print,'Reading initial water table grid...'
Read_Var, gv.h0_table, gv.h0_table_type, gv.h0_table_file, $
          nx, ny, UNIT=unit
gv.h0_table_unit = unit
;--------------------------------------------------------------
Read_Var, gv.d_freeze, gv.d_freeze_type, gv.d_freeze_file, $
          nx, ny, UNIT=unit
gv.d_freeze_unit = unit
;--------------------------------------------------------------
Read_Var, gv.d_thaw, gv.d_thaw_type, gv.d_thaw_file, $
          nx, ny, UNIT=unit
gv.d_thaw_unit = unit
;--------------------------------------------------------------
Read_Var, gv.d_bedrock, gv.d_bedrock_type, gv.d_bedrock_file, $
          nx, ny, UNIT=unit
gv.d_bedrock_unit = unit

;--------------------------------------------------
;These are used by the new, more general GW method
;--------------------------------------------------
for j=0,(gv.nlayers-1) do begin
    Read_Var, gv.Ks[j], gv.Ks_type[j], gv.Ks_file[j], nx, ny, UNIT=unit
    gv.Ks_unit[j] = unit
    ;-------------------------------------------------------------------
    Read_Var, gv.qs[j], gv.qs_type[j], gv.qs_file[j], nx, ny, UNIT=unit
    gv.qs_unit[j] = unit
    ;-------------------------------------------------------------------
    Read_Var, gv.th[j], gv.th_type[j], gv.th_file[j], nx, ny, UNIT=unit
    gv.th_unit[j] = unit
endfor

;----------------------------------------------------
;These are used by Bolton's variable K GW method
;but are now obsolete because the GW method now
;allows multiple layers and any variable can
;vary spatially or temporally, including thickness.
;----------------------------------------------------
;Read_Var, gv.VK1, gv.VK1_type, gv.VK1_file, nx, ny, UNIT=unit
;gv.VK1_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK2, gv.VK2_type, gv.VK2_file, nx, ny, UNIT=unit
;gv.VK2_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK3, gv.VK3_type, gv.VK3_file, nx, ny, UNIT=unit
;gv.VK3_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK4, gv.VK4_type, gv.VK4_file, nx, ny, UNIT=unit
;gv.VK4_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK5, gv.VK5_type, gv.VK5_file, nx, ny, UNIT=unit
;gv.VK5_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK6, gv.VK6_type, gv.VK6_file, nx, ny, UNIT=unit
;gv.VK6_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK7, gv.VK7_type, gv.VK7_file, nx, ny, UNIT=unit
;gv.VK7_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK8, gv.VK8_type, gv.VK8_file, nx, ny, UNIT=unit
;gv.VK8_unit = unit
;--------------------------------------------------------------
;Read_Var, gv.VK9, gv.VK9_type, gv.VK9_file, nx, ny, UNIT=unit
;gv.VK9_unit = unit
;----------------------------------------------------------------
;Read_Var, gv.VK10, gv.VK10_type, gv.VK10_file, nx, ny, UNIT=unit
;gv.VK10_unit = unit
;----------------------------------------------------------------


END;  Read_GW_Vars_in_Files
;******************************************************************
pro Read_Infil_Vars_in_Files, iv, nx, ny 

;-------------------------------------
;NOTES:  iv = infil_vars = structure
;--------------------------------------------------------------
;Note:  This routine is only used for the initial reading of
;       variables that are stored in files.  During a model
;       run, Update_Infil_Vars is called to update variables.
;--------------------------------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (iv.method eq 0) then RETURN

;-------------------------------------
;Vars not used by Richards eqn method
;-------------------------------------
RICHARDS = (iv.method eq 4)
if NOT(RICHARDS) then begin
    Read_Var, iv.Ks, iv.Ks_type[0], iv.Ks_file[0], nx, ny, UNIT=unit
    iv.Ks_unit[0] = unit
    ;-------------------------------------------------------------
    Read_Var, iv.Ki, iv.Ki_type[0], iv.Ki_file[0], nx, ny, UNIT=unit
    iv.Ki_unit[0] = unit
    ;-------------------------------------------------------------
    Read_Var, iv.qs, iv.qs_type[0], iv.qs_file[0], nx, ny, UNIT=unit
    iv.qs_unit[0] = unit
    ;-------------------------------------------------------------
    Read_Var, iv.qi, iv.qi_type[0], iv.qi_file[0], nx, ny, UNIT=unit
    iv.qi_unit[0] = unit
    ;-------------------------------------------------------------
    Read_Var, iv.G, iv.G_type, iv.G_file, nx, ny, UNIT=unit
    iv.G_unit[0] = unit
    ;-------------------------------------------------------------
    Read_Var, iv.gam, iv.gam_type, iv.gam_file, nx, ny, UNIT=unit
    iv.gam_unit[0] = unit
    ;-------------------------------------------------------------
    RETURN
endif

;--------------------------------------------------
;Vars used by Richards method for all soil layers
;--------------------------------------------------
for j=0, (iv.n_layers-1) do begin
    Read_Var, iv.Ks_val[j], iv.Ks_type[j], iv.Ks_file[j], nx, ny, UNIT=unit
    iv.Ks_unit[j] = unit
    ;------------------------------------------------------------------------
    Read_Var, iv.Ki_val[j], iv.Ki_type[j], iv.Ki_file[j], nx, ny, UNIT=unit
    iv.Ki_unit[j] = unit
    ;------------------------------------------------------------------------
    Read_Var, iv.qs_val[j], iv.qs_type[j], iv.qs_file[j], nx, ny, UNIT=unit
    iv.qs_unit[j] = unit
    ;------------------------------------------------------------------------
    Read_Var, iv.qi_val[j], iv.qi_type[j], iv.qi_file[j], nx, ny, UNIT=unit
    iv.qi_unit[j] = unit
    ;------------------------------------------------------------------------
    Read_Var, iv.qr_val[j], iv.qr_type[j], iv.qr_file[j], nx, ny, UNIT=unit
    iv.qr_unit[j] = unit
    ;------------------------------------------------------------------------
    Read_Var, iv.pB_val[j], iv.pB_type[j], iv.pB_file[j], nx, ny, UNIT=unit
    iv.pB_unit[j] = unit
    ;---------------------------------------------------------------------------
    Read_Var, iv.pA_val[j], iv.pA_type[j], iv.pA_file[j], nx, ny, UNIT=unit
    iv.pA_unit[j] = unit
    ;---------------------------------------------------------------------------
    Read_Var, iv.lam_val[j], iv.lam_type[j], iv.lam_file[j], nx, ny, UNIT=unit
    iv.lam_unit[j] = unit
    ;---------------------------------------------------------------------
    Read_Var, iv.c_val[j], iv.c_type[j], iv.c_file[j], nx, ny, UNIT=unit
    iv.c_unit[j] = unit

    ;-------------------------------------------------------
    ;If we read a lambda value from a file, then we need to
    ;compute and save corresponding eta = [2 + (3*lambda)]
    ;-------------------------------------------------------
    if (iv.lam_unit[j] ne 0L) then begin
        *iv.eta_val[j] = 2d + (3d * (*iv.lam_val[j]))
    endif
endfor


END;  Read_Infil_Vars_in_Files
;******************************************************************
pro Free_Pointers, state

;---------------------------------------
;Destroy all existing pointer heap vars
;---------------------------------------
;This is okay, but may affect other
;programs, including RiverTools
;---------------------------------------
;ptr_free, ptr_valid()

;RETURN

;---------------------------
;Free the grid var pointers
;---------------------------
ptr_free, state.grid_vars.outlet_IDs
ptr_free, state.grid_vars.basin_areas
ptr_free, state.grid_vars.basin_reliefs

;------------------------------
;Free the channel var pointers
;------------------------------
ptr_free, state.channel_vars.codes
ptr_free, state.channel_vars.slopes
ptr_free, state.channel_vars.nvals
ptr_free, state.channel_vars.z0vals
ptr_free, state.channel_vars.widths
ptr_free, state.channel_vars.angles
ptr_free, state.channel_vars.sinu    ;(sinuosity)
ptr_free, state.channel_vars.d0      ;(initial depths)
;-------------------------------
;For future use
;-------------------------------
;ptr_free, state.channel_vars.Q
;ptr_free, state.channel_vars.u
;ptr_free, state.channel_vars.d
;ptr_free, state.channel_vars.f

;-----------------------------
;Free the precip var pointers
;-----------------------------
ptr_free, state.precip_vars.rates
ptr_free, state.precip_vars.durations
;-------------------------------------
;** ptr_free, state.precip_vars.T_air   ;(now in met_vars)
;** ptr_free, state.precip_vars.h_swe

;--------------------------
;Free the met var pointers
;--------------------------
ptr_free, state.met_vars.Qn_SW
ptr_free, state.met_vars.Qn_LW
ptr_free, state.met_vars.T_air
ptr_free, state.met_vars.T_surf
ptr_free, state.met_vars.RH
ptr_free, state.met_vars.p0
ptr_free, state.met_vars.uz
ptr_free, state.met_vars.z
ptr_free, state.met_vars.z0_air
ptr_free, state.met_vars.rho_air
ptr_free, state.met_vars.Cp_air
ptr_free, state.met_vars.rho_H2O

;---------------------------
;Free the snow var pointers
;---------------------------
ptr_free, state.snow_vars.c0
ptr_free, state.snow_vars.T0
ptr_free, state.snow_vars.h0_snow
ptr_free, state.snow_vars.h0_swe
ptr_free, state.snow_vars.rho_snow
ptr_free, state.snow_vars.Cp_snow
;---------------------------------
ptr_free, state.snow_vars.Ecc
ptr_free, state.snow_vars.h_snow    ;(computed vars)
ptr_free, state.snow_vars.h_swe
;---------------------------------
;ptr_free, state.snow_vars.Qn_SW    ;(now in met_vars)
;ptr_free, state.snow_vars.Qn_LW
;ptr_free, state.snow_vars.T_air
;ptr_free, state.snow_vars.T_surf
;ptr_free, state.snow_vars.RH
;ptr_free, state.snow_vars.p0
;ptr_free, state.snow_vars.uz
;ptr_free, state.snow_vars.z
;ptr_free, state.snow_vars.z0_air
;ptr_free, state.snow_vars.rho_air
;ptr_free, state.snow_vars.Cp_air
;ptr_free, state.snow_vars.rho_H2O

;-------------------------
;Free the ET var pointers
;-------------------------
ptr_free, state.ET_vars.alpha
ptr_free, state.ET_vars.Ks
ptr_free, state.ET_vars.soil_x
ptr_free, state.ET_vars.T_soil_x
;--------------------------------
;ptr_free, state.ET_vars.Qn_SW    ;(now in met_vars)
;ptr_free, state.ET_vars.Qn_LW
;ptr_free, state.ET_vars.T_air
;ptr_free, state.ET_vars.T_surf
;ptr_free, state.ET_vars.RH
;ptr_free, state.ET_vars.p0
;ptr_free, state.ET_vars.uz
;ptr_free, state.ET_vars.z
;ptr_free, state.ET_vars.z0_air
;ptr_free, state.ET_vars.rho_air
;ptr_free, state.ET_vars.Cp_air
;----------------------------------
;(3/14/07) No longer duplicated
;----------------------------------
;ptr_free, state.ET_vars.Ecc
;ptr_free, state.ET_vars.rho_snow
;ptr_free, state.ET_vars.Cp_snow
;ptr_free, state.ET_vars.h0_snow 
;ptr_free, state.ET_vars.h0_swe   ;*** NOT USED YET
;-------------------------------
;;ptr_free, state.ET_vars.h_snow
;;ptr_free, state.ET_vars.h_swe 

;----------------------------
;Free the infil var pointers
;----------------------------
ptr_free, state.infil_vars.G
ptr_free, state.infil_vars.gam
ptr_free, state.infil_vars.tp
ptr_free, state.infil_vars.fp
ptr_free, state.infil_vars.I
;--------------------------------
ptr_free, state.infil_vars.Zw
ptr_free, state.infil_vars.z
;--------------------------------
ptr_free, state.infil_vars.q
ptr_free, state.infil_vars.p
ptr_free, state.infil_vars.K
ptr_free, state.infil_vars.v
;--------------------------------
ptr_free, state.infil_vars.Ks
ptr_free, state.infil_vars.Ki
ptr_free, state.infil_vars.qs
ptr_free, state.infil_vars.qi
ptr_free, state.infil_vars.qr
ptr_free, state.infil_vars.pB
ptr_free, state.infil_vars.pA
ptr_free, state.infil_vars.lam
ptr_free, state.infil_vars.c
;-------------------------------
ptr_free, state.infil_vars.eta
ptr_free, state.infil_vars.qH
ptr_free, state.infil_vars.dz    ;(special case)
;--------------------------------
for j=0,(state.infil_vars.n_layers-1) do begin
    ptr_free, state.infil_vars.Ks_val[j]
    ptr_free, state.infil_vars.Ki_val[j]
    ptr_free, state.infil_vars.qs_val[j]
    ptr_free, state.infil_vars.qi_val[j]
    ptr_free, state.infil_vars.qr_val[j]
    ptr_free, state.infil_vars.qH_val[j]   ;*******************
    ptr_free, state.infil_vars.pB_val[j]
    ptr_free, state.infil_vars.pA_val[j]
    ptr_free, state.infil_vars.lam_val[j]
    ptr_free, state.infil_vars.c_val[j]
    ptr_free, state.infil_vars.eta_val[j]
endfor

;-------------------------
;Free the GW var pointers
;-------------------------
ptr_free, state.gw_vars.elev
ptr_free, state.gw_vars.h0_table
ptr_free, state.gw_vars.d_freeze
ptr_free, state.gw_vars.d_thaw
ptr_free, state.gw_vars.d_bedrock
;-------------------------------------------
for j=0,(state.gw_vars.nlayers-1) do begin
    ptr_free, state.gw_vars.Ks[j]
    ptr_free, state.gw_vars.qs[j]
    ptr_free, state.gw_vars.th[j]
endfor

;-------------------------------
;Free the sediment var pointers
;-------------------------------
ptr_free, state.sed_vars.E
ptr_free, state.sed_vars.area
ptr_free, state.sed_vars.p1
ptr_free, state.sed_vars.slope
ptr_free, state.sed_vars.p2
ptr_free, state.sed_vars.relief
ptr_free, state.sed_vars.temp

END;  Free_Pointers
;*****************************************************************
pro Close_Input_Files, state

;---------------------------------------------------------
;Note:  Could save all open file unit numbers in a common
;       block, from the Read_Next_Value procedure.
;---------------------------------------------------------

;--------------------------------
;Option to close all files
;which may affect other programs
;--------------------------------
close, /all
RETURN
;*****************************************

;---------------------------
;Close the precip var files
;---------------------------
f = fstat(state.precip_vars.rate_unit)
if (f.open eq 1b) then free_lun, state.precip_vars.rate_unit

;----------------------------------------------------------------
f = fstat(state.precip_vars.duration_unit)
if (f.open eq 1b) then free_lun, state.precip_vars.duration_unit
;----------------------------------------------------------------
;f = fstat(state.precip_vars.T_air_unit)
;if (f.open eq 1b) then free_lun, state.precip_vars.T_air_unit

;----------------------------
;Close the channel var files
;----------------------------
f = fstat(state.channel_vars.slope_unit)
if (f.open eq 1b) then free_lun, state.channel_vars.slope_unit
;----------------------------------------------------------------
f = fstat(state.channel_vars.nval_unit)
if (f.open eq 1b) then free_lun, state.channel_vars.nval_unit
;----------------------------------------------------------------
f = fstat(state.channel_vars.z0val_unit)
if (f.open eq 1b) then free_lun, state.channel_vars.z0val_unit
;----------------------------------------------------------------
f = fstat(state.channel_vars.width_unit)
if (f.open eq 1b) then free_lun, state.channel_vars.width_unit
;----------------------------------------------------------------
f = fstat(state.channel_vars.angle_unit)
if (f.open eq 1b) then free_lun, state.channel_vars.angle_unit

;------------------------
;Close the met var files
;------------------------
f = fstat(state.met_vars.Qn_SW_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.Qn_SW_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.Qn_LW_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.Qn_LW_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.T_air_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.T_air_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.T_surf_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.T_surf_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.RH_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.RH_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.p0_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.p0_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.uz_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.uz_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.z_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.z_unit
;-------------------------------------------------------------
f = fstat(state.met_vars.z0_air_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.z0_air_unit

;-------------------------
;Close the snow var files
;-------------------------
f = fstat(state.snow_vars.c0_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.c0_unit
;--------------------------------------------------------------
f = fstat(state.snow_vars.T0_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.T0_unit
;--------------------------------------------------------------
f = fstat(state.snow_vars.h0_snow_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.h0_snow_unit
;--------------------------------------------------------------
f = fstat(state.snow_vars.h0_swe_unit)
if (f.open eq 1b) then free_lun, state.snow_vars.h0_swe_unit

;-----------------------
;Close the ET var files   ;****************************************
;-----------------------

;-----------------------
;Close the GW var files
;-----------------------
for j=0,(state.gw_vars.nlayers-1) do begin
    f = fstat(state.gw_vars.Ks_unit[j])
    if (f.open eq 1b) then free_lun, state.gw_vars.Ks_unit[j]
    ;----------------------------------------------------------
    f = fstat(state.gw_vars.qs_unit[j])
    if (f.open eq 1b) then free_lun, state.gw_vars.qs_unit[j]
    ;----------------------------------------------------------
    f = fstat(state.gw_vars.th_unit[j])
    if (f.open eq 1b) then free_lun, state.gw_vars.th_unit[j]
endfor

;--------------------------
;Close the infil var files
;--------------------------
f = fstat(state.infil_vars.Ks_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.Ks_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.Ki_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.Ki_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.qs_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.qs_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.qi_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.qi_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.qr_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.qr_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.G_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.G_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.gam_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.gam_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.pB_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.pB_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.pA_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.pA_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.lam_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.lam_unit
;------------------------------------------------------------
f = fstat(state.infil_vars.c_unit)
if (f.open eq 1b) then free_lun, state.infil_vars.c_unit

end;  Close_Input_Files
;*****************************************************************
pro Update_Grid_Vars, prefix, state

;-------------------------------------------------------
;Notes:  This routine is called by Read_Run_Info_Panel,
;        in GUI_main.pro and by 'BROWSE_DIR' event.
;-------------------------------------------------------

;----------------------------
;Read DEM info from RTI file
;----------------------------
RTI_file = (prefix + '.rti')
Read_RTI_File, RTI_file, info
if (n_elements(info) eq 0) then RETURN

;--------------------
;Get additional info
;--------------------
basin_RTM_file = prefix + '_basin.rtm'
Get_Pixel_Sizes, dx_vec, dy,dd,da, RTI_file, /METERS
dx = min(dx_vec)   ;[meters]

;-----------------------------------
;Update grid_var variables in state
;-----------------------------------
state.grid_vars.ncols       = info.ncols
state.grid_vars.nrows       = info.nrows
state.grid_vars.xres        = info.xres
state.grid_vars.yres        = info.yres
;---------------------------------------------
state.grid_vars.data_type   = info.data_type
state.grid_vars.pixel_geom  = info.pixel_geom
state.grid_vars.byte_order  = info.byte_order
state.grid_vars.RTI_file    = RTI_file
state.grid_vars.min_dx      = dx
state.grid_vars.RTM_file    = basin_RTM_file

;--------------------------------------------
;Change the RTM filename in basin info panel
;--------------------------------------------
widget_control, state.grid_vars.RTM_file_ID, $
                set_value=basin_RTM_file 

end;  Update_Grid_Vars
;*****************************************************************
pro Update_Precip_Vars, pv

;------------------------------------------
;NOTES:  pv = precip_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.

;        Rainrate units are always [mm/hr]
;        for input, but internally stored
;        and used as [m/s].
;------------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (pv.method eq 0) then RETURN

;----------------------------------------------------------------
;3/14/08.  The next two calls will return without doing anything
;          when (pv.method eq 1), as long as pv.rate_type and
;          pv.duration_type have been set back to 0b.  So next
;          line really isn't necessary.
;----------------------------------------------------------------
if (pv.method eq 1) then RETURN

Read_Next_Var, pv.rates, pv.rate_type, pv.rate_file, $
               pv.rate_unit, FACTOR=(1d/3600000d)        ;******
Read_Next_Var, pv.durations, pv.duration_type, $
               pv.duration_file, pv.duration_unit

;--------------------------
;(3/14/07) Now in met_vars
;--------------------------
;;Read_Next_Var, pv.T_air, pv.T_air_type, $
;;               pv.T_air_file, pv.T_air_unit

END;  Update_Precip_Vars
;*****************************************************************
pro Update_Met_Vars, mv

;---------------------------------------
;NOTES:  mv = met_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.
;---------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
;if (sv.method eq 0) OR (ev.method eq 0) OR $
;   (pv.method eq 0) then RETURN

Read_Next_Var, mv.Qn_SW,   mv.Qn_SW_type,   mv.Qn_SW_file,   mv.Qn_SW_unit
Read_Next_Var, mv.Qn_LW,   mv.Qn_LW_type,   mv.Qn_LW_file,   mv.Qn_LW_unit
Read_Next_Var, mv.T_air,   mv.T_air_type,   mv.T_air_file,   mv.T_air_unit
Read_Next_Var, mv.T_surf,  mv.T_surf_type,  mv.T_surf_file,  mv.T_surf_unit
Read_Next_Var, mv.RH,      mv.RH_type,      mv.RH_file,      mv.RH_unit
Read_Next_Var, mv.p0,      mv.p0_type,      mv.p0_file,      mv.p0_unit
Read_Next_Var, mv.uz,      mv.uz_type,      mv.uz_file,      mv.uz_unit
Read_Next_Var, mv.z,       mv.z_type,       mv.z_file,       mv.z_unit
Read_Next_Var, mv.z0_air,  mv.z0_air_type,  mv.z0_air_file,  mv.z0_air_unit

END;  Update_Met_Vars
;*****************************************************************
pro Update_Snow_Vars, sv

;---------------------------------------
;NOTES:  sv = snow_vars = structure
;        All grids are assumed to have
;        data type of 'FLOAT'.
;---------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (sv.method eq 0) then RETURN

Read_Next_Var, sv.c0,      sv.c0_type,      sv.c0_file,      sv.c0_unit
Read_Next_Var, sv.T0,      sv.T0_type,      sv.T0_file,      sv.T0_unit
Read_Next_Var, sv.h0_snow, sv.h0_snow_type, sv.h0_snow_file, sv.h0_snow_unit
Read_Next_Var, sv.h0_swe,  sv.h0_swe_type,  sv.h0_swe_file,  sv.h0_swe_unit
Read_Next_Var, sv.rho_snow, sv.rho_snow_type, sv.rho_snow_file, sv.rho_snow_unit

;------------------------------------
;(3/14/07) These are now in met_vars
;------------------------------------
;Read_Next_Var, sv.Qn_SW,   sv.Qn_SW_type,   sv.Qn_SW_file,   sv.Qn_SW_unit
;Read_Next_Var, sv.Qn_LW,   sv.Qn_LW_type,   sv.Qn_LW_file,   sv.Qn_LW_unit
;Read_Next_Var, sv.T_air,   sv.T_air_type,   sv.T_air_file,   sv.T_air_unit
;Read_Next_Var, sv.T_surf,  sv.T_surf_type,  sv.T_surf_file,  sv.T_surf_unit
;Read_Next_Var, sv.RH,      sv.RH_type,      sv.RH_file,      sv.RH_unit
;Read_Next_Var, sv.p0,      sv.p0_type,      sv.p0_file,      sv.p0_unit
;Read_Next_Var, sv.uz,      sv.uz_type,      sv.uz_file,      sv.uz_unit
;Read_Next_Var, sv.z,       sv.z_type,       sv.z_file,       sv.z_unit
;Read_Next_Var, sv.z0_air,  sv.z0_air_type,  sv.z0_air_file,  sv.z0_air_unit

END;  Update_Snow_Vars
;******************************************************************
pro Update_ET_Vars, ev, sv

;-------------------------------------------------------------
;NOTES:  ev = ET_vars   = structure
;        sv = snow_vars = structure
;        All grids are assumed to have data type of 'FLOAT'.

;        This routine should use snow_vars when possible,
;        similar to what Read_ET_Vars does.
;-------------------------------------------------------------

;-----------------------------
;Return if no method selected
;-----------------------------
if (ev.method eq 0) then RETURN

Read_Next_Var, ev.alpha,    ev.alpha_type,    ev.alpha_file, $
               ev.alpha_unit
Read_Next_Var, ev.Ks,       ev.Ks_type,       ev.Ks_file, $
               ev.Ks_unit
Read_Next_Var, ev.soil_x,   ev.soil_x_type,   ev.soil_x_file, $
               ev.soil_x_unit
Read_Next_Var, ev.T_soil_x, ev.T_soil_x_type, ev.T_soil_x_file, $
               ev.T_soil_x_unit

;------------------------------------
;(3/14/07) These are now in met_vars
;------------------------------------
;Read_Next_Var, ev.Qn_SW,   ev.Qn_SW_type,   ev.Qn_SW_file,   ev.Qn_SW_unit
;Read_Next_Var, ev.Qn_LW,   ev.Qn_LW_type,   ev.Qn_LW_file,   ev.Qn_LW_unit
;Read_Next_Var, ev.T_air,   ev.T_air_type,   ev.T_air_file,   ev.T_air_unit
;Read_Next_Var, ev.T_surf,  ev.T_surf_type,  ev.T_surf_file,  ev.T_surf_unit
;Read_Next_Var, ev.RH,      ev.RH_type,      ev.RH_file,      ev.RH_unit
;Read_Next_Var, ev.p0,      ev.p0_type,      ev.p0_file,      ev.p0_unit
;Read_Next_Var, ev.uz,      ev.uz_type,      ev.uz_file,      ev.uz_unit
;Read_Next_Var, ev.z,       ev.z_type,       ev.z_file,       ev.z_unit
;Read_Next_Var, ev.z0_air,  ev.z0_air_type,  ev.z0_air_file,  ev.z0_air_unit

;------------------------------------------
;(3/14/07) These are now only in snow_vars
;------------------------------------------
;Read_Next_Var, ev.h0_snow, ev.h0_snow_type, ev.h0_snow_file, ev.h0_snow_unit
;Read_Next_Var, ev.h0_swe,  ev.h0_swe_type,  ev.h0_swe_file,  ev.h0_swe_unit

END;  Update_ET_Vars
;******************************************************************
pro Update_Infil_Vars, iv, nx, ny

;-------------------------------------------------------------
;NOTES:  iv = infil_vars   = structure
;        All grids are assumed to have data type of 'FLOAT'.
;-------------------------------------------------------------
;        nx & ny are needed for multiple soil layers and are
;        passed from Route_Flow to Infiltration function to
;        here.
;-------------------------------------------------------------
;NB!  The concept of vars like Ks and qs changing over time
;     is a bit strange.  However, it *may* be possible to use
;     this capability to model gradual freezing or thawing of
;     soil.  Note that the dynamic vars themselves, like
;     iv.q, iv.p, iv.K and iv.v are not modified by this and
;     retain whatever state they had prior to changing these.
;     But their future evolution will be different.

;     We assume here that vars like nz and dz can't change.
;-------------------------------------------------------------
FORWARD_FUNCTION Theta_Min

;-----------------------------
;Return if no method selected
;-----------------------------
if (iv.method eq 0) then RETURN

;-------------------------------------
;Vars not used by Richards eqn method
;-------------------------------------
RICHARDS = (iv.method eq 4b)
if NOT(RICHARDS) then begin
    Read_Next_Var, iv.Ks,  iv.Ks_type[0],  iv.Ks_file[0],  iv.Ks_unit[0]
    Read_Next_Var, iv.Ki,  iv.Ki_type[0],  iv.Ki_file[0],  iv.Ki_unit[0]
    Read_Next_Var, iv.qs,  iv.qs_type[0],  iv.qs_file[0],  iv.qs_unit[0]
    Read_Next_Var, iv.qi,  iv.qi_type[0],  iv.qi_file[0],  iv.qi_unit[0]
    ;---------------------------------------------------------------------
    Read_Next_Var, iv.G,   iv.G_type,      iv.G_file,      iv.G_unit 
    Read_Next_Var, iv.gam, iv.gam_type,    iv.gam_file,    iv.gam_unit
    RETURN 
endif

;-------------------------------------------------
;Vars used by Richards method for all soil layers
;-------------------------------------------------
for j=0L,(iv.n_layers-1) do begin
    Read_Next_Var, iv.Ks_val[j],  iv.Ks_type[j],  iv.Ks_file[j],  iv.Ks_unit[j]
    Read_Next_Var, iv.Ki_val[j],  iv.Ki_type[j],  iv.Ki_file[j],  iv.Ki_unit[j]
    Read_Next_Var, iv.qs_val[j],  iv.qs_type[j],  iv.qs_file[j],  iv.qs_unit[j]
    Read_Next_Var, iv.qi_val[j],  iv.qi_type[j],  iv.qi_file[j],  iv.qi_unit[j]
    Read_Next_Var, iv.qr_val[j],  iv.qr_type[j],  iv.qr_file[j],  iv.qr_unit[j] 
    Read_Next_Var, iv.pB_val[j],  iv.pB_type[j],  iv.pB_file[j],  iv.pB_unit[j]
    Read_Next_Var, iv.pA_val[j],  iv.pA_type[j],  iv.pA_file[j],  iv.pA_unit[j] 
    Read_Next_Var, iv.lam_val[j], iv.lam_type[j], iv.lam_file[j], iv.lam_unit[j] 
    Read_Next_Var, iv.c_val[j],   iv.c_type[j],   iv.c_file[j],   iv.c_unit[j]

    ;--------------------------------------
    ;Update eta, given by [2 + (3*lambda)]
    ;--------------------------------------
    if (iv.lam_type[j] eq 1) OR (iv.lam_type[j] eq 3) then $
        *iv.eta_val[j] = (2d + (3d * (*iv.lam_val[j])))
    ;***************************************************
    ;Make sure pointer use is correct here.  Seems OK.
    ;***************************************************
    ;** if (iv.lam_type[j] eq 1) OR (iv.lam_type[j] eq 3) then $
    ;**     *(iv.eta_val[j]) = 2d + (3d * (*(iv.lam_val[j])))

    ;----------------------------------------
    ;Update qH, given by Theta_Min function
    ;----------------------------------------
    *iv.qH_val[j] = Theta_Min(*iv.qs_val[j], *iv.qr_val[j], *iv.pB_val[j], $
                              *iv.pA_val[j], *iv.c_val[j],  *iv.lam_val[j])
endfor 



;------------------------------------------
;Now build a 1D or 3D array for each input
;var as was done by Initialize_Infil_Vars
;------------------------------------------------------
;(3/12/08) Same code should work if (iv.n_layers eq 1)
;------------------------------------------------------
Build_Layered_Var, iv,nx,ny, iv.Ks,  iv.Ks_val
Build_Layered_Var, iv,nx,ny, iv.Ki,  iv.Ki_val
Build_Layered_Var, iv,nx,ny, iv.qs,  iv.qs_val
Build_Layered_Var, iv,nx,ny, iv.qi,  iv.qi_val
Build_Layered_Var, iv,nx,ny, iv.qr,  iv.qr_val
Build_Layered_Var, iv,nx,ny, iv.pB,  iv.pB_val
Build_Layered_Var, iv,nx,ny, iv.pA,  iv.pA_val
Build_Layered_Var, iv,nx,ny, iv.lam, iv.lam_val
Build_Layered_Var, iv,nx,ny, iv.c,   iv.c_val
;-----------------------------------------------
;Note:  These two are computed from the others
;-----------------------------------------------
Build_Layered_Var, iv,nx,ny, iv.eta, iv.eta_val
Build_Layered_Var, iv,nx,ny, iv.qH,  iv.qH_val 


END;  Update_Infil_Vars
;******************************************************************
pro Update_GW_Vars, gv

;-------------------------------------------------------------
;NOTES:  gw = GW_vars = structure
;        All grids are assumed to have data type of 'FLOAT'.

;        See the routine called Read_GW_Vars_in_Files that
;        reads initial values.
;-------------------------------------------------------------
RETURN

;-----------------------------
;Return if no method selected
;-----------------------------
if (gv.method eq 0) then RETURN

for j=0,(gv.nlayers-1) do begin
    Read_Next_Var, gv.Ks[j], gv.Ks_type[j], gv.Ks_file[j], gv.Ks_unit[j]
    Read_Next_Var, gv.qs[j], gv.qs_type[j], gv.qs_file[j], gv.qs_unit[j]
    Read_Next_Var, gv.th[j], gv.th_type[j], gv.th_file[j], gv.th_unit[j]
endfor

;---------------------------------------------------
;These are computed variables, not input variables,
;although we may read their initial value from file
;---------------------------------------------------
;Read_Next_Var, gv.h0_table, gv.h0_table_type, gv.h0_table_file
;Read_Next_Var, gv.d_freeze, gv.d_freeze_type, gv.d_freeze_file
;Read_Next_Var, gv.d_thaw,   gv.d_thaw_type,   gv.d_thaw_file

;----------------------------------------------------
;Bolton's variable K vars don't vary in time so they
;don't need to be updated here.  Instead, see the
;routine called Read_GW_Vars_in_Files.
;----------------------------------------------------

END;  Update_GW_Vars
;******************************************************************
function Var_Setting, type, ptr, file, FACTOR=FACTOR

case (type) of
    ;----------------
    ;Var is a scalar
    ;----------------
    0b : if (keyword_set(FACTOR)) then begin
             str = TF_String(*ptr * FACTOR)
         endif else begin
             str = TF_String(*ptr)
         endelse

    ;-----------------------
    ;Var is saved in a file
    ;-----------------------
    else : str = file
endcase

RETURN, str

end;  Var_Setting
;******************************************************************

