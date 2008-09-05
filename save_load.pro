
;NB!  The Read_Vars routine here has name very similar to the
;     Read_Var routine in getvars.pro.  Should rename soon.

;******************************************************************
;   save_vars.pro

;   Copyright (c) 2007-2008, Scott D. Peckham 
;   Created:  (moved from getvars.pro, extended) Mar 2007
;   Modified:  Mar 2008  (Any number of layers for Richards, etc.)

;******************************************************************

;   Save_Var            (7/24/06)
;   Save_Var2           (7/25/06)
;   Save_Var3           (7/25/06)
;   Save_All_TF_Vars    (7/25/06)
;---------------------------------
;   Read_Vars           (7/25/06)    ;(Name similar to Read_Var )
;   Load_Var
;   Type_Code 
;   Load_All_TF_Vars    (7/25/06)

;******************************************************************
pro Save_Var, LUN, name, var, type, units

fF = '(F12.6)'   ;(WILL '(F0)' PRINT ALL DIGITS ?? **************)
fD = '(D18.8)'

if (n_elements(type) eq 0) then type='STRING'

;--------------------------
;Convert "var" to a string
;--------------------------
case strupcase(type) of
    'BYTE'   : str2 = TF_String(fix(var))
    'FLOAT'  : str2 = TF_String(var, format=fF)
    'DOUBLE' : str2 = TF_String(var, format=fD)
    'STRING' : str2 = var
     ELSE    : str2 = TF_String(var)
endcase

;------------------------
;Left justify the phrase
;------------------------
n    = 24                 ;(width of name section)
len  = strlen(name)
m    = (n - len) > 1
str1 = name + string(replicate(32b, m))

;-------------------------
;Add optional unit string
;-------------------------
if (n_elements(units) ne 0) then str3=units else str3=''

;-------------------------------
;Print phrase and value to file
;-------------------------------
printf, LUN, (str1 + str2 + str3)

end;  Save_Var
;******************************************************************
pro Save_Var2, LUN, name, type, code, ptr, file, units, $
               NOT_PTR=NOT_PTR, FACTOR=FACTOR

;------------------------------------------------------------------
;Notes:  LUN   = logical unit number of output file
;        name  = variable name or description as string ('T_air:')
;        type  = data type ('BYTE', 'FLOAT', 'DOUBLE', etc.)
;        code  = input type code (0=Scalar, 1=Time series, etc.)
;        ptr   = pointer to the heap variable (for scalars)
;        file  = name of file that contains variable (for others)
;        units = units of the variable, as a string

;        The NOT_PTR keyword can be set to indicate that the
;        "ptr" argument is a scalar value (e.g. pv.dt).

;        (2/12/07) Added FACTOR keyword for rainrates.
;------------------------------------------------------------------
NOT_PTR = keyword_set(NOT_PTR)
if NOT(keyword_set(FACTOR)) then FACTOR=1d

fF = '(F12.6)'   ;(WILL '(F0)' PRINT ALL DIGITS ?? ************)
fD = '(D18.8)'

types = ['Scalar         ', $
         'Time_series    ', $
         'Grid           ', $
         'Grid_Sequence  ']

;-------------------------------
;Left justify the variable name
;-------------------------------
n    = 24                 ;(width of name section)
len  = strlen(name)
m    = (n - len) > 1
str1 = name + string(replicate(32b, m))

;--------------------------
;Get the input type string
;--------------------------
str2 = types[code]

;------------------------------
;Get scalar string or filename
;------------------------------
if (code eq 0b) then begin
    if (NOT_PTR) then var=ptr else var=*ptr

    ;-----------------------------
    ;Option to change units, etc.
    ;-----------------------------
    if (FACTOR ne 1d) then var = (var * FACTOR)

    ;------------------------------------------
    ;Make sure var is a scalar (for rainrates)
    ;------------------------------------------
    ;nv = n_elements(var)
    ;if (nv gt 1) then begin
    ;    var  = var[0]
        ;msg  = ['WARNING: ', ' ',$
        ;       'The list of rainrates and durations is not saved when ',$
        ;       'the "Uniform in space" method for precipitation is used.',$
        ;       'Only the first value in the list will be saved. ',$
        ;       ' ',$
        ;       'If you use the "General data types" method instead, ',$
        ;       'then File > Load Input Vars can restore all settings.',$
        ;       ' ']
        ;result = GUI_Message(msg, /INFO)
        ;str2 = 'Time_series    '
    ;endif

    ;---------------------------
    ;Convert scalar to a string
    ;---------------------------
    case strupcase(type) of
        'BYTE'   : str3 = TF_String(fix(var))
        'FLOAT'  : str3 = TF_String(var, format=fF)
        'DOUBLE' : str3 = TF_String(var, format=fD)
         ELSE    : str3 = TF_String(var)
    endcase
endif else begin
    str3 = file
endelse

;------------------
;Left justify str3
;------------------
n3   = 24               ;(width of str3 section)
len3 = strlen(str3)
m3   = (n3 - len3) > 1  ;********
str3 = str3 + string(replicate(32b, m3))

;-------------------------------
;Print name, type, etc. to file
;-------------------------------
printf, LUN, (str1 + str2 + str3 + units)

end;  Save_Var2
;******************************************************************
pro Save_Var3, LUN, name, flag, file, units

;------------------------------------------------------------------
;Notes:  LUN   = logical unit number of output file
;        name  = variable name or description as string ('T_air:')
;        flag  = 1 = save this var, 0 = don't save
;        file  = name of output file
;        units = units of the variable, as a string
;------------------------------------------------------------------
if (file eq '') then file='n/a'

;-----------------------------
;Left justify the option name
;-----------------------------
n    = 24                 ;(width of name section)
len  = strlen(name)
m    = (n - len)

str1 = name + string(replicate(32b, m))

;--------------------------
;Get the option flag string
;---------------------------
str2 = TF_String(fix(flag)) + string(replicate(32b,14))

;-------------------------------
;Left-justified filename string
;-------------------------------
n3   = 24               ;(width of str3 section)
len3 = strlen(file)
m3   = (n3 - len3) > 1
str3 = file + string(replicate(32b, m3))

;--------------------------------------
;Print name, flag and filename to file
;--------------------------------------
printf, LUN, (str1 + str2 + str3 + units)

end;  Save_Var3
;******************************************************************
pro Save_All_TF_Vars, mstate

;-----------------------------
;Get name of output text file
;-----------------------------
run_prefix = mstate.run_vars.run_prefix
filename   = ('00_' + run_prefix + '_INPUT.txt')
out_file = dialog_pickfile(FILE=filename, /WRITE)
                   ;***** /OVERWRITE_PROMPT)
if (out_file eq '') then RETURN
Check_Overwrite, out_file, OK
if NOT(OK) then RETURN

;------------------------
;Open text file to write
;------------------------
TF_Get_LUN, unit, out_file
openw, unit, out_file

;--------------------
;Local abbreviations
;--------------------
av = mstate.grid_vars
rv = mstate.run_vars
pv = mstate.precip_vars
cv = mstate.channel_vars
mv = mstate.met_vars        ;(new, 3/13/07)
sv = mstate.snow_vars
ev = mstate.ET_vars
gv = mstate.GW_vars
iv = mstate.infil_vars
dv = mstate.diversion_vars
fv = mstate.stop_vars

;---------------------
;Prepare for printing
;---------------------
line = string(replicate(45b, 50))
;*** shortline = string(replicate(45b, 22))

;-------------------
;Print out a header
;-------------------
printf, unit, 'TopoFlow 1.5 Input File'
printf, unit, ' '

;---------------------------------------
;Read run vars from the "run var" panel
;----------------------------------------------
;Run vars are not saved until the user goes to
;the next panel, and they may not have done so
;----------------------------------------------
widget_control, rv.prefix_ID,       get_value=prefix
widget_control, rv.run_prefix_ID,   get_value=run_prefix
widget_control, rv.directory_ID,    get_value=directory
widget_control, rv.log_file_ID,     get_value=log_file
widget_control, rv.comment_file_ID, get_value=comment_file
;------------------------------------
;Alternate approach (see note above)
;------------------------------------
;;prefix       = rv.prefix
;;run_prefix   = rv.run_prefix
;;directory    = rv.directory
;;log_file     = rv.log_file

;;comment_file = rv.comment_file

;-----------------------------
;Print out the model run vars
;-----------------------------
printf, unit, line
printf, unit, '  Model Run Variables'
printf, unit, line
Save_Var, unit, 'Prefix:',       prefix
Save_Var, unit, 'Run prefix:',   run_prefix
Save_Var, unit, 'Directory:',    directory
Save_Var, unit, 'Log file:',     log_file
Save_Var, unit, 'Comment file:', comment_file
printf, unit, ' '

;------------------------
;Print out the grid vars
;------------------------
printf, unit, line
printf, unit, '  Grid Variables'
printf, unit, line
Save_Var, unit, 'RTI file:',           av.RTI_file
Save_Var, unit, 'Number of columns:',  av.ncols, 'LONG'
Save_Var, unit, 'Number of rows:',     av.nrows, 'LONG'
Save_Var, unit, 'Pixel xsize:',        av.xres, 'FLOAT'
Save_Var, unit, 'Pixel ysize:',        av.yres, 'FLOAT'
Save_Var, unit, 'DEM data type:',      av.data_type
Save_Var, unit, 'Pixel geom code:',    av.pixel_geom, 'BYTE'
Save_Var, unit, 'Byte order:',         av.byte_order
Save_Var, unit, 'Minimum xsize:',      av.min_dx, 'FLOAT'
Save_Var, unit, 'Basin RTM file:',     av.RTM_file
Save_Var, unit, 'Compute precip vol:', av.get_pvolume, 'BYTE'
printf, unit, ' '

;------------------------------------
;Print out info for monitored pixels
;------------------------------------
printf, unit, line
printf, unit, '  Monitored Pixels'
printf, unit, line
w  = where(*av.outlet_IDs gt 0L, nw)
Save_Var, unit, 'Number monitored:', nw, 'LONG'
;-------------------------------------------------
for k=0,(nw-1) do begin
    kstr = 'Pixel ' + TF_String(k+1) + ' '
    wk   = w[k]
    xk   = (*av.outlet_IDs)[wk] mod av.ncols
    yk   = (*av.outlet_IDs)[wk]  /  av.ncols
    ak   = (*av.basin_areas)[wk]
    rk   = (*av.basin_reliefs)[wk]
    ;-------------------------------------------------
    Save_Var, unit, (kstr + 'column:'), xk, 'LONG'
    Save_Var, unit, (kstr + 'row:'),    yk, 'LONG'
    Save_Var, unit, (kstr + 'area:'),   ak, 'DOUBLE', '   [km^2]'
    Save_Var, unit, (kstr + 'relief:'), rk, 'DOUBLE', '  [m]'
endfor
printf, unit, ' '

;--------------------------
;Print out the precip vars
;--------------------------
printf, unit, line
printf, unit, '  Precipitation Process Variables'
printf, unit, line
;*** Save_Var, unit, 'Time step [sec]:', pv.dt, 'DOUBLE'
if (pv.method eq 1) then begin
    ;---------------------------------------------------------
    ;Method is "Uniform in space", which does not follow the
    ;standard pattern used for all other methods in TopoFlow.
    ;---------------------------------------------------------
    ;Save rates and durations to files, change the type to
    ;"Time series" and the method to "General data types"
    ;--------------------------------------------------------
    new_rate_type = 1b   ;(time series)
    new_dur_type  = 1b   ;(time series)
    new_rate_file = '00_' + prefix + '_rainrates.txt'
    new_dur_file  = '00_' + prefix + '_durations.txt'
    ;-------------------------------------------------
    Check_Overwrite, new_rate_file, OK
    if NOT(OK) then begin
        msg = [' ',$
               'Please use the next dialog to select a filename',$
               'for saving the time series of RAINRATES.', ' ']
        result = GUI_Message(msg, /INFO)
        new_rate_file = dialog_pickfile(file=new_rate_file, /WRITE, $
                                        /OVERWRITE_PROMPT)
    endif
    ;----------------------------------------------------------------
    Check_Overwrite, new_dur_file, OK
    if NOT(OK) then begin
        msg = [' ',$
               'Please use the next dialog to select a filename',$
               'for saving the time series of DURATIONS.', ' ']
        result = GUI_Message(msg, /INFO)
        new_dur_file = dialog_pickfile(file=new_dur_file, /WRITE, $
                                       /OVERWRITE_PROMPT)
    endif
    ;------------------------------------
    TF_Get_LUN, rate_unit, new_rate_file
    openw, rate_unit, new_rate_file
    ;------------------------------------
    TF_Get_LUN, dur_unit, new_dur_file
    openw, dur_unit, new_dur_file
    ;------------------------------------
    np = n_elements(*pv.rates)
    for k=0L,(np - 1L) do begin
        printf, rate_unit, ((*pv.rates)[k] * 3600000d)   ;[mm/hr]
        printf, dur_unit,  (*pv.durations)[k]
        ;-----------------------------------------------
        ;** printf, rate_unit, 100.0
        ;** printf, rate_unit, (*pv.rates)[k], (*pv.rates)[k] * 3600000d
        ;** printf, rate_unit, (*pv.rates)[k]   ;[mm/hr]
    endfor
    free_lun, rate_unit, dur_unit
    ;--------------------------------------------------------
    ;NB!  Use FACTOR to convert units from [m/s] to [mm/hr]
    ;--------------------------------------------------------
    Save_Var, unit, 'Method code:', 2b, 'BYTE'
    Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
               pv.dt, '', '[sec]', /NOT_PTR
    Save_Var2, unit, 'Rate:', 'DOUBLE', new_rate_type, $
               pv.rates, new_rate_file, '[mm/hr]', FACTOR=3600000d
    Save_Var2, unit, 'Duration:', 'DOUBLE', new_dur_type, $
               pv.durations, new_dur_file, '[min]'
    ;------------------------------------------------------
    ;Save_Var2, unit, 'T_air:', 'DOUBLE', pv.T_air_type, $  ;(now with met vars)
    ;           pv.T_air, pv.T_air_file, '[deg C]'
endif else begin
    ;--------------------------------------------------------
    ;NB!  Use FACTOR to convert units from [m/s] to [mm/hr]
    ;--------------------------------------------------------
    Save_Var, unit, 'Method code:', pv.method, 'BYTE'
    Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
               pv.dt, '', '[sec]', /NOT_PTR
    Save_Var2, unit, 'Rate:', 'DOUBLE', pv.rate_type, $
               pv.rates, pv.rate_file, '[mm/hr]', FACTOR=3600000d
    Save_Var2, unit, 'Duration:', 'DOUBLE', pv.duration_type, $
               pv.durations, pv.duration_file, '[min]'
    ;------------------------------------------------------
    ;Save_Var2, unit, 'T_air:', 'DOUBLE', pv.T_air_type, $  ;(now with met vars)
    ;           pv.T_air, pv.T_air_file, '[deg C]'
endelse
printf, unit, ' '

;---------------------------
;Print out the channel vars
;---------------------------
printf, unit, line
printf, unit, '  Channel Process Variables'
printf, unit, line
Save_Var, unit, 'Method code:', cv.method, 'BYTE'
;-----------------------------------------------------------------
Save_Var, unit, 'Kinematic wave flag:', cv.kinematic_wave, 'BYTE'
Save_Var, unit, 'Diffusive wave flag:', cv.diffusive_wave, 'BYTE'
Save_Var, unit, 'Dynamic wave flag:  ', cv.dynamic_wave, 'BYTE'
Save_Var, unit, 'Manning flag:       ', cv.manning, 'BYTE'
Save_Var, unit, 'Law of Wall flag:   ', cv.law_of_wall, 'BYTE'
;-----------------------------------------------------------------
Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
           cv.dt, '', '[sec]', /NOT_PTR
Save_Var2, unit, 'D8 flow code:', 'BYTE', cv.code_type, $
           cv.codes, cv.code_file, '[none]'
Save_Var2, unit, 'D8 slope:', 'DOUBLE', cv.slope_type, $
           cv.slopes, cv.slope_file, '[m/m]'
;-----------------------------------------------------------------
if (cv.manning eq 1b) then begin
    Save_Var2, unit, 'Manning N:', 'DOUBLE', cv.nval_type, $
               cv.nvals, cv.nval_file, '[s/m^(1/3)]'
endif
;-----------------------------------------------------------------
if (cv.law_of_wall eq 1b) then begin
    Save_Var2, unit, 'Roughness z0:', 'DOUBLE', cv.z0val_type, $
               cv.z0vals, cv.z0val_file, '[m]'
endif
;-----------------------------------------------------------------
Save_Var2, unit, 'Bed width:', 'DOUBLE', cv.width_type, $
           cv.widths, cv.width_file, '[m]'
Save_Var2, unit, 'Bank angle:', 'DOUBLE', cv.angle_type, $
           cv.angles, cv.angle_file, '[deg]'
Save_Var2, unit, 'Init. depth:', 'DOUBLE', cv.d0_type, $
           cv.d0, cv.d0_file, '[m]'
Save_Var2, unit, 'Sinuosity:', 'DOUBLE', cv.sinu_type, $
           cv.sinu, cv.sinu_file, '[m/m]'
;----------------------------------------------------------------------------
Save_Var2, unit, 'Save grid timestep:', 'DOUBLE', 0b, $
           cv.save_grid_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save Q grids:', cv.save_Q_grids, cv.Q_rts_file, '[m^3/s]
Save_Var3, unit, 'Save u grids:', cv.save_u_grids, cv.u_rts_file, '[m/s]
Save_Var3, unit, 'Save d grids:', cv.save_d_grids, cv.d_rts_file, '[m]
Save_Var3, unit, 'Save f grids:', cv.save_f_grids, cv.f_rts_file, '[none]
;----------------------------------------------------------------------------
Save_Var2, unit, 'Save pixels timestep:', 'DOUBLE', 0b, $
           cv.save_pixels_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save Q pixels:', cv.save_Q_pixels, cv.Q_out_file, '[m^3/s]
Save_Var3, unit, 'Save u pixels:', cv.save_u_pixels, cv.u_out_file, '[m/s]
Save_Var3, unit, 'Save d pixels:', cv.save_d_pixels, cv.d_out_file, '[m]
Save_Var3, unit, 'Save f pixels:', cv.save_f_pixels, cv.f_out_file, '[none]
;----------------------------------------------------------------------------
printf, unit, ' '

;-----------------------
;Print out the met vars
;-----------------------
printf, unit, line
printf, unit, '  Meteorological Variables'
printf, unit, line
;----------------------------------------------
Save_Var2, unit, 'rho_H2O:', 'DOUBLE', 0b, $
           mv.rho_H2O, '', '[kg/m^3]'
Save_Var2, unit, 'Cp_air:', 'DOUBLE', 0b, $
           mv.Cp_air, '', '[J/kg/K]'
Save_Var2, unit, 'rho_air:', 'DOUBLE', 0b, $
           mv.rho_air, '', '[kg/m^3]'
;--------------------------------------------------------
Save_Var2, unit, 'Qn_SW:', 'DOUBLE', mv.Qn_SW_type, $
           mv.Qn_SW, mv.Qn_SW_file, '[W/m^2]'
Save_Var2, unit, 'Qn_LW:', 'DOUBLE', mv.Qn_LW_type, $
           mv.Qn_LW, mv.Qn_LW_file, '[W/m^2]'
Save_Var2, unit, 'T_air:', 'DOUBLE', mv.T_air_type, $
           mv.T_air, mv.T_air_file, '[deg C]'
Save_Var2, unit, 'T_surf:', 'DOUBLE', mv.T_surf_type, $
           mv.T_surf, mv.T_surf_file, '[deg C]'
Save_Var2, unit, 'RH:', 'DOUBLE', mv.RH_type, $
           mv.RH, mv.RH_file, '[none]'
Save_Var2, unit, 'p0:', 'DOUBLE', mv.p0_type, $
           mv.p0, mv.p0_file, '[mbar]'
Save_Var2, unit, 'uz:', 'DOUBLE', mv.uz_type, $
           mv.uz, mv.uz_file, '[m/s]'
Save_Var2, unit, 'z:', 'DOUBLE', mv.z_type, $
           mv.z, mv.z_file, '[m]'
Save_Var2, unit, 'z0_air:', 'DOUBLE', mv.z0_air_type, $
           mv.z0_air, mv.z0_air_file, '[m]'
printf,    unit, ' '

;------------------------
;Print out the snow vars
;------------------------
printf, unit, line
printf, unit, '  Snowmelt Process Variables'
printf, unit, line
Save_Var, unit, 'Method code:', sv.method, 'BYTE' 
;** Save_Var, unit, 'Time step [sec]:', sv.dt, 'DOUBLE'
Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
           sv.dt, '', '[sec]', /NOT_PTR
;---------------------------------------------------------------
Save_Var2, unit, 'Cp_snow:', 'DOUBLE', 0b, $
           sv.Cp_snow, '', '[J/kg/K]'
Save_Var2, unit, 'rho_snow:', 'DOUBLE', sv.rho_snow_type, $
           sv.rho_snow, sv.rho_snow_file, '[kg/m^3]'
Save_Var2, unit, 'c0:', 'DOUBLE', sv.c0_type, $
           sv.c0, sv.c0_file, '[mm/day/deg C]'
Save_Var2, unit, 'T0:', 'DOUBLE', sv.T0_type, $
           sv.T0, sv.T0_file, '[deg C]'
Save_Var2, unit, 'h0_snow:', 'DOUBLE', sv.h0_snow_type, $
           sv.h0_snow, sv.h0_snow_file, '[m]'
Save_Var2, unit, 'h0_swe:', 'DOUBLE', sv.h0_swe_type, $
           sv.h0_swe, sv.h0_swe_file, '[m]'
;------------------------------------------------------------------------------
Save_Var2, unit, 'Save grid timestep:', 'DOUBLE', 0b, $
           sv.save_grid_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save mr grids:', sv.save_mr_grids, sv.mr_rts_file, '[m/s]'
Save_Var3, unit, 'Save hs grids:', sv.save_hs_grids, sv.hs_rts_file, '[m]'
Save_Var3, unit, 'Save sw grids:', sv.save_sw_grids, sv.sw_rts_file, '[m]'
Save_Var3, unit, 'Save cc grids:', sv.save_cc_grids, sv.cc_rts_file, '[J/m^2]'
Save_Var3, unit, 'Save ea grids:', sv.save_ea_grids, sv.ea_rts_file, '[mbar]'
Save_Var3, unit, 'Save es grids:', sv.save_es_grids, sv.es_rts_file, '[mbar]'
;------------------------------------------------------------------------------
Save_Var2, unit, 'Save pixels timestep:', 'DOUBLE', 0b, $
           sv.save_pixels_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save mr pixels:', sv.save_mr_pixels, sv.mr_out_file, '[m/s]'
Save_Var3, unit, 'Save hs pixels:', sv.save_hs_pixels, sv.hs_out_file, '[m]'
Save_Var3, unit, 'Save sw pixels:', sv.save_sw_pixels, sv.sw_out_file, '[m]'
Save_Var3, unit, 'Save cc pixels:', sv.save_cc_pixels, sv.cc_out_file, '[J/m^2]'
Save_Var3, unit, 'Save ea pixels:', sv.save_ea_pixels, sv.ea_out_file, '[mbar]'
Save_Var3, unit, 'Save es pixels:', sv.save_es_pixels, sv.es_out_file, '[mbar]'
printf,    unit, ' '

;----------------------
;Print out the ET vars
;----------------------
printf, unit, line
printf, unit, '  Evapotranspiration Process Variables'
printf, unit, line
Save_Var, unit, 'Method code:', ev.method, 'BYTE'
;** Save_Var, unit, 'Time step [sec]:', ev.dt, 'DOUBLE'
Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
           ev.dt, '', '[sec]', /NOT_PTR
;---------------------------------------------------------------
Save_Var2, unit, 'alpha:', 'DOUBLE', ev.alpha_type, $
           ev.alpha, ev.alpha_file, '[none]'
Save_Var2, unit, 'Ks:', 'DOUBLE', ev.Ks_type, $
           ev.Ks, ev.Ks_file, '[W/m/deg_C]'
Save_Var2, unit, 'soil_x:', 'DOUBLE', ev.soil_x_type, $
           ev.soil_x, ev.soil_x_file, '[m]'
Save_Var2, unit, 'T_soil_x:', 'DOUBLE', ev.T_soil_x_type, $
           ev.T_soil_x, ev.T_soil_x_file, '[deg C]'
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save grid timestep:', 'DOUBLE', 0b, $
           ev.save_grid_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save er grids:', ev.save_er_grids, ev.er_rts_file, '[m/s]'
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save pixels timestep:', 'DOUBLE', 0b, $
           ev.save_pixels_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save er pixels:', ev.save_er_pixels, ev.er_out_file, '[m/s]'
printf,    unit, ' '

;-------------------------
;Print out the infil vars
;-------------------------
printf, unit, line
printf, unit, '  Infiltration Process Variables'
printf, unit, line
Save_Var, unit, 'Method code:', iv.method, 'BYTE'
Save_Var, unit, 'Number of layers:', iv.n_layers, 'LONG'
;** Save_Var, unit, 'Time step [sec]:', iv.dt, 'DOUBLE'
;------------------------------------------------------------
Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
           iv.dt, '', '[sec]', /NOT_PTR
;------------------------------------------------------------
if (iv.method lt 4) then begin
    ;---------------------------------------
    ;Vars for Green-Ampt and Smith-Parlange
    ;---------------------------------------
    Save_Var2, unit, 'Ks:', 'DOUBLE', iv.Ks_type[0], $
               iv.Ks, iv.Ks_file[0], '[m/s]'
    Save_Var2, unit, 'Ki:', 'DOUBLE', iv.Ki_type[0], $
               iv.Ki, iv.Ki_file[0], '[m/s]'
    Save_Var2, unit, 'qs:', 'DOUBLE', iv.qs_type[0], $
               iv.qs, iv.qs_file[0], '[none]'
    Save_Var2, unit, 'qi:', 'DOUBLE', iv.qi_type[0], $
               iv.qi, iv.qi_file[0], '[none]'
    Save_Var2, unit, 'G:', 'DOUBLE', iv.G_type, $
               iv.G, iv.G_file, '[m]'
    Save_Var2, unit, 'gamma:', 'DOUBLE', iv.gam_type, $
               iv.gam, iv.gam_file, '[none]'
    Save_Var,  unit, 'Closest soil_type:', iv.soil_type[0], 'STRING' 
endif else begin
    ;------------------------------------
    ;Vars for Richards' equation method
    ;------------------------------------
    for k=0, (iv.n_layers-1) do begin
        str1 = 'Layer ' + TF_String(k+1) + ' '
        Save_Var2, unit, str1 + 'Ks:', 'DOUBLE', iv.Ks_type[k], $
                   iv.Ks_val[k], iv.Ks_file[k], '[m/s]'
        Save_Var2, unit, str1 + 'Ki:', 'DOUBLE', iv.Ki_type[k], $
                   iv.Ki_val[k], iv.Ki_file[k], '[m/s]'
        Save_Var2, unit, str1 + 'qs:', 'DOUBLE', iv.qs_type[k], $
                   iv.qs_val[k], iv.qs_file[k], '[unitless]'
        Save_Var2, unit, str1 + 'qi:', 'DOUBLE', iv.qi_type[k], $
                   iv.qi_val[k], iv.qi_file[k], '[unitless]'
        Save_Var2, unit, str1 + 'qr:', 'DOUBLE', iv.qr_type[k], $
                   iv.qr_val[k], iv.qr_file[k], '[unitless]'
        Save_Var2, unit, str1 + 'pB:', 'DOUBLE', iv.pB_type[k], $
                   iv.pB_val[k], iv.pB_file[k], '[m]'
        Save_Var2, unit, str1 + 'pA:', 'DOUBLE', iv.pA_type[k], $
                   iv.pA_val[k], iv.pA_file[k], '[m]'
        Save_Var2, unit, str1 + 'lam:', 'DOUBLE', iv.lam_type[k], $
                   iv.lam_val[k], iv.lam_file[k], '[unitless]'
        Save_Var2, unit, str1 + 'c:', 'DOUBLE', iv.c_type[k], $
                   iv.c_val[k], iv.c_file[k], '[unitless]'
        Save_Var2, unit, str1 + 'dz:', 'DOUBLE', 0b, $
                   iv.dz_val[k], '', '[m]', /NOT_PTR
        Save_Var, unit, str1 + 'nz:', iv.nz_val[k], 'LONG'
        Save_Var, unit, str1 + 'soil_type:', iv.soil_type[k], 'STRING'
    endfor
endelse
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save grid timestep:', 'DOUBLE', 0b, $
           iv.save_grid_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save v0 grids:', iv.save_v0_grids, iv.v0_rts_file, '[m/s]'
Save_Var3, unit, 'Save q0 grids:', iv.save_q0_grids, iv.q0_rts_file, '[none]'
Save_Var3, unit, 'Save I  grids:', iv.save_I_grids,  iv.I_rts_file,  '[m]'
Save_Var3, unit, 'Save Zw grids:', iv.save_Zw_grids, iv.Zw_rts_file, '[m]'
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save pixels timestep:', 'DOUBLE', 0b, $
           iv.save_pixels_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save v0 pixels:', iv.save_v0_pixels, iv.v0_out_file, '[m/s]'
Save_Var3, unit, 'Save q0 pixels:', iv.save_q0_pixels, iv.q0_out_file, '[none]'
Save_Var3, unit, 'Save I  pixels:', iv.save_I_pixels,  iv.I_out_file,  '[m]'
Save_Var3, unit, 'Save Zw pixels:', iv.save_Zw_pixels, iv.Zw_out_file, '[m]'
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save stack timestep:', 'DOUBLE', 0b, $
           iv.save_stack_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save q stacks:', iv.save_q_stacks, iv.q_stack_file, '[none]'
Save_Var3, unit, 'Save p stacks:', iv.save_p_stacks, iv.p_stack_file, '[m]'
Save_Var3, unit, 'Save K stacks:', iv.save_K_stacks, iv.K_stack_file, '[m/s]'
Save_Var3, unit, 'Save v stacks:', iv.save_v_stacks, iv.v_stack_file, '[m/s]'
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save profile timestep:', 'DOUBLE', 0b, $
           iv.save_profile_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save q profiles:', iv.save_q_profiles, $
           iv.q_profile_file, '[none]'
Save_Var3, unit, 'Save p profiles:', iv.save_p_profiles, $
           iv.p_profile_file, '[m]'
Save_Var3, unit, 'Save K profiles:', iv.save_K_profiles, $
           iv.K_profile_file, '[m/s]'
Save_Var3, unit, 'Save v profiles:', iv.save_v_profiles, $
           iv.v_profile_file, '[m/s]'
;-----------------------------------------------------------------------------
printf,    unit, ' '


;----------------------
;Print out the GW vars
;----------------------
printf, unit, line
printf, unit, '  Subsurface Flow Process Variables'
printf, unit, line
Save_Var, unit, 'Method code:', gv.method, 'BYTE'
Save_Var, unit, 'Number of layers:', gv.nlayers, 'LONG'
;** Save_Var, unit, 'Time step [sec]:', gv.dt, 'DOUBLE'
;---------------------------------------------------------------
Save_Var2, unit, 'Time step:', 'DOUBLE', 0b, $
           gv.dt, '', '[sec]', /NOT_PTR
Save_Var2, unit, 'z_surface:', 'DOUBLE', gv.elev_type, $
           gv.elev, gv.elev_file, '[m]'
Save_Var2, unit, 'z0_table:', 'DOUBLE', gv.h0_table_type, $
           gv.h0_table, gv.h0_table_file, '[m]'
Save_Var2, unit, 'd_freeze:', 'DOUBLE', gv.d_freeze_type, $
           gv.d_freeze, '', '[m]'
Save_Var2, unit, 'd_thaw:', 'DOUBLE', gv.d_thaw_type, $
           gv.d_thaw, '', '[m]'
;---------------------------------------------------------------
for k=0,(gv.nlayers-1) do begin
    str1 = 'Layer ' + TF_String(k+1) + ' '
    Save_Var2, unit, str1 + 'Ks:', 'DOUBLE', gv.Ks_type[k], $
               gv.Ks[k], gv.Ks_file[k], '[m/s]'
    Save_Var2, unit, str1 + 'qs:', 'DOUBLE', gv.qs_type[k], $
               gv.qs[k], gv.qs_file[k], '[none]'
    Save_Var2, unit, str1 + 'thickness:', 'DOUBLE', gv.th_type[k], $
               gv.th[k], gv.th_file[k], '[m]'
endfor
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save grid timestep:', 'DOUBLE', 0b, $
           gv.save_grid_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save ht grids:', gv.save_ht_grids, gv.ht_rts_file, '[m]'
Save_Var3, unit, 'Save df grids:', gv.save_df_grids, gv.df_rts_file, '[m]'
Save_Var3, unit, 'Save dt grids:', gv.save_dt_grids, gv.dt_rts_file, '[m]'
;-----------------------------------------------------------------------------
Save_Var2, unit, 'Save pixels timestep:', 'DOUBLE', 0b, $
           gv.save_pixels_dt, '', '[sec]', /NOT_PTR
Save_Var3, unit, 'Save ht pixels:', gv.save_ht_pixels, gv.ht_out_file, '[m]'
Save_Var3, unit, 'Save df pixels:', gv.save_df_pixels, gv.df_out_file, '[m]'
Save_Var3, unit, 'Save dt pixels:', gv.save_dt_pixels, gv.dt_out_file, '[m]'
printf,    unit, ' '

;-----------------------------
;Print out the diversion vars
;-----------------------------
printf, unit, line
printf, unit, '  Diversion Process Variables'
printf, unit, line
Save_Var, unit, 'Method code:', dv.method, 'BYTE'
;-----------------------------------------------------------
Save_Var3, unit, 'Use sources:', dv.use_sources, dv.source_file, '[N/A]'
Save_Var3, unit, 'Use sinks:',   dv.use_sinks,   dv.sink_file,   '[N/A]'
Save_Var3, unit, 'Use canals:',  dv.use_canals,  dv.canal_file,  '[N/A]'
printf,    unit, ' '

;------------------------
;Print out the stop vars
;------------------------
printf, unit, line
printf, unit, '  Stopping Criterion Variables'
printf, unit, line
Save_Var, unit, 'Method code:', fv.method, 'BYTE'
;--------------------------------------------------------------------
Save_Var, unit, 'Qp_fraction:',  fv.Qp_fraction,  'DOUBLE'
Save_Var, unit, 'T_stop_model:', fv.T_stop_model, 'DOUBLE', '   [min]'
Save_Var, unit, 'T_stop_real:',  fv.T_stop_real,  'DOUBLE', '   [min]'
Save_Var, unit, 'n_steps:',      fv.n_steps,      'LONG'
printf,   unit, ' '

;----------------------
;Close the output file
;----------------------
free_lun, unit

;---------------------------
;Display information dialog
;---------------------------
msg = [ $
'All input variables saved to the file:', $
out_file, ' ']
result = GUI_Message(msg, /INFO, TITLE='Save Settings Status')

end;  Save_All_TF_Vars
;******************************************************************
pro Read_Vars, LUN, var1, var2, var3, var4, TYPE=type, NAME=name

;------------------------------------------------------------ 
;Notes:  By default, the STRSPLIT routine uses both blank
;        spaces and tabs as delimiters.  Others can be
;        specified with the optional "pattern" argument.

;NB!     If (type eq 'STRING') (the default), then we want
;        to read everything after the ":", which may contain
;        space characters in the interior (e.g a directory),
;        but with leading/trailing spaces removed.
;------------------------------------------------------------
FORWARD_FUNCTION strsplit, strpos, strmid

if NOT(keyword_set(TYPE)) then type='STRING'
line = ''
readf, LUN, line

;--------------------------------
;Exract the variable name, which
;may contain blank spaces
;--------------------------------
p = strpos(line, ':')
if (p eq -1) then RETURN
name = strmid(line, 0, p)
line = strmid(line, p+1)

;-----------------------------
;Extract variables as strings
;-----------------------------
words = strsplit(line, /EXTRACT, COUNT=count)

;----------------------
;Convert var1 string ?
;----------------------
if (count ge 1) then begin
    case (strupcase(type)) of
        'BYTE'    : var1 = byte(fix(words[0]))
        'INTEGER' : var1 = fix(words[0])
        'LONG'    : var1 = long(words[0])
        'FLOAT'   : var1 = float(words[0])
        'DOUBLE'  : var1 = double(words[0])
        'STRING'  : var1 = words[0]
        'FILE'    : var1 = strtrim(line, 2)   ;(may contain blanks)
         ELSE     : var1 = words[0]
    endcase
endif

;-----------------------------------
;Extract additional vars as strings
;-----------------------------------
if (count ge 2) then var2 = words[1]
if (count ge 3) then var3 = words[2]
if (count ge 4) then var4 = words[3]

end;  Read_Vars
;******************************************************************
pro Load_Var, str, type, ptr, file, FACTOR=FACTOR

if NOT(keyword_set(FACTOR)) then FACTOR = 1d

if (strupcase(type) eq 'SCALAR') then begin
    if (FACTOR ne 1) then *ptr = (double(str) * FACTOR) $
                     else *ptr = double(str)
    file = ''
endif else begin
    file = str
endelse

end;  Load_Var
;******************************************************************
function Type_Code, type

case strupcase(type) of
    'SCALAR'        : code = 0b
    'TIME_SERIES'   : code = 1b
    'GRID'          : code = 2b
    'GRID_SEQUENCE' : code = 3b
    ;----------------------------
    'TIME SERIES'   : code = 1b
    'GRID SEQUENCE' : code = 3b
    'GRID_STACK'    : code = 3b
    'GRID STACK'    : code = 3b
    ;----------------------------
     else           : code = 0b
endcase

RETURN, code

end;  Type_Code
;******************************************************************
pro Load_All_TF_Vars, mstate

;-----------------------------
;Get name of input text file
;-----------------------------
run_prefix = mstate.run_vars.run_prefix
filename   = ('00_' + run_prefix + '_INPUT.txt')
in_file    = dialog_pickfile(FILE=filename, /READ)
if (in_file eq '') then RETURN

;-----------------------
;Open text file to read
;-----------------------
TF_Get_LUN, unit, in_file
openr, unit, in_file

;-----------------------
;Skip over header lines
;-----------------------
line = ''
for k=1,5 do readf, unit, line

;------------------------
;Read the model run vars
;------------------------
Read_Vars, unit, prefix,       type='FILE'
Read_Vars, unit, run_prefix,   type='FILE'
Read_Vars, unit, directory,    type='FILE'
Read_Vars, unit, log_file,     type='FILE'
Read_Vars, unit, comment_file, type='FILE'
;-------------------------------------------
mstate.run_vars.prefix       = prefix
mstate.run_vars.run_prefix   = run_prefix
mstate.run_vars.directory    = directory
mstate.run_vars.log_file     = log_file
mstate.run_vars.comment_file = comment_file

;-------------------------
;CD to the directory ?
;May contain blank spaces
;-------------------------
cd, directory

;-------------------
;Read the grid vars
;-------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, RTI_file,    type='FILE'
Read_Vars, unit, ncols,       type='LONG'
Read_Vars, unit, nrows,       type='LONG'
Read_Vars, unit, xres,        type='DOUBLE'
Read_Vars, unit, yres,        type='DOUBLE'
Read_Vars, unit, data_type,   type='STRING'
Read_Vars, unit, pixel_geom,  type='BYTE'
Read_Vars, unit, byte_order,  type='STRING'
Read_Vars, unit, min_dx,      type='DOUBLE'
Read_Vars, unit, RTM_file,    type='FILE'
Read_Vars, unit, get_pvolume, type='BYTE'
;-------------------------------------------
mstate.grid_vars.RTI_file    = RTI_file
mstate.grid_vars.ncols       = ncols
mstate.grid_vars.nrows       = nrows
mstate.grid_vars.xres        = xres
mstate.grid_vars.yres        = yres
mstate.grid_vars.data_type   = data_type
mstate.grid_vars.pixel_geom  = pixel_geom
mstate.grid_vars.byte_order  = byte_order
mstate.grid_vars.min_dx      = min_dx
mstate.grid_vars.RTM_file    = RTM_file
mstate.grid_vars.get_pvolume = get_pvolume

;------------------------------
;Read the monitored pixel info
;------------------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, n_pixels, type='LONG'
if (n_pixels gt 0) then begin
    cols    = lonarr(n_pixels)
    rows    = lonarr(n_pixels)
    areas   = fltarr(n_pixels)
    reliefs = fltarr(n_pixels)
    ;-------------------------------
    for k=0L,(n_pixels-1L) do begin
        Read_Vars, unit, col,    type='LONG'
        Read_Vars, unit, row,    type='LONG'
        Read_Vars, unit, area,   type='FLOAT'
        Read_Vars, unit, relief, type='FLOAT'
        ;-------------------------------------
        cols[k]    = col
        rows[k]    = row
        areas[k]   = area
        reliefs[k] = relief
    endfor
    *mstate.grid_vars.outlet_IDs    = (rows * ncols) + cols
    *mstate.grid_vars.basin_areas   = areas
    *mstate.grid_vars.basin_reliefs = reliefs
endif

;---------------------
;Read the precip vars
;---------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, precip_method, type='BYTE'
Read_Vars, unit, dt_type, dt
Read_Vars, unit, rate_type, rate
Read_Vars, unit, duration_type, duration
;Read_Vars, unit, T_air_type, T_air         ;(now with met vars)

;print,'rate_type = ', rate_type
;print,'rate      = ', rate
;print,'duration_type = ', duration_type
;print,'duration  = ', duration

;-----------------------------------------------------
mstate.precip_vars.method = precip_method
mstate.precip_vars.dt     = double(dt)
;--------------------------------------------------------
;NB!  Use FACTOR to convert units from [mm/hr] to [m/s]
;--------------------------------------------------------
Load_Var, rate, rate_type, mstate.precip_vars.rates, rate_file, $
          FACTOR=(1d/3600000d)
mstate.precip_vars.rate_type = Type_Code(rate_type) 
mstate.precip_vars.rate_file = rate_file
Load_Var, duration, duration_type, mstate.precip_vars.durations, $
          duration_file
mstate.precip_vars.duration_type = Type_Code(duration_type)
mstate.precip_vars.duration_file = duration_file
;------------------------------------------------------------------
;Now with met vars
;------------------
;Load_Var, T_air, T_air_type, mstate.precip_vars.T_air, T_air_file
;mstate.precip_vars.T_air_type = Type_Code(T_air_type)
;mstate.precip_vars.T_air_file = T_air_file

;----------------------
;Read the channel vars
;----------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, channel_method, type='BYTE'
Read_Vars, unit, kinematic,      type='BYTE'
Read_Vars, unit, diffusive,      type='BYTE'
Read_Vars, unit, dynamic,        type='BYTE'
Read_Vars, unit, manning,        type='BYTE'
Read_Vars, unit, law_of_wall,    type='BYTE'
Read_Vars, unit, dt_type, dt
;---------------------------------------------------
mstate.channel_vars.method         = channel_method
mstate.channel_vars.kinematic_wave = kinematic
mstate.channel_vars.diffusive_wave = diffusive
mstate.channel_vars.dynamic_wave   = dynamic
mstate.channel_vars.manning        = manning
mstate.channel_vars.law_of_wall    = law_of_wall
mstate.channel_vars.dt             = double(dt)
;---------------------------------------------------
Read_Vars, unit, code_type,  code
Read_Vars, unit, slope_type, slope
if (MANNING)     then Read_Vars, unit, nval_type,  nval
if (LAW_OF_WALL) then Read_Vars, unit, z0val_type, z0val
Read_Vars, unit, width_type, width
Read_Vars, unit, angle_type, angle
Read_Vars, unit, d0_type,    d0
Read_Vars, unit, sinu_type,  sinu
;-----------------------------------------------------------------------
Load_Var, code, code_type, mstate.channel_vars.codes, code_file
mstate.channel_vars.code_type = Type_Code(code_type)
mstate.channel_vars.code_file = code_file
Load_Var, slope, slope_type, mstate.channel_vars.slopes, slope_file
mstate.channel_vars.slope_type = Type_Code(slope_type)
mstate.channel_vars.slope_file = slope_file
if (MANNING) then begin
    Load_Var, nval, nval_type, mstate.channel_vars.nvals, nval_file
    mstate.channel_vars.nval_type = Type_Code(nval_type)
    mstate.channel_vars.nval_file = nval_file
endif
if (LAW_OF_WALL) then begin
    Load_Var, z0val, z0val_type, mstate.channel_vars.z0vals, z0val_file
    mstate.channel_vars.z0val_type = Type_Code(z0val_type)
    mstate.channel_vars.z0val_file = z0val_file
endif
Load_Var, width, width_type, mstate.channel_vars.widths, width_file
mstate.channel_vars.width_type = Type_Code(width_type)
mstate.channel_vars.width_file = width_file
Load_Var, angle, angle_type, mstate.channel_vars.angles, angle_file
mstate.channel_vars.angle_type = Type_Code(angle_type)
mstate.channel_vars.angle_file = angle_file
Load_Var, d0, d0_type, mstate.channel_vars.d0, d0_file
mstate.channel_vars.d0_type = Type_Code(d0_type)
mstate.channel_vars.d0_file = d0_file
Load_Var, sinu, sinu_type, mstate.channel_vars.sinu, sinu_file
mstate.channel_vars.sinu_type = Type_Code(sinu_type)
mstate.channel_vars.sinu_file = sinu_file
;-----------------------------------------------------------------------
Read_Vars, unit, dum_str, save_grid_dt
Read_Vars, unit, save_Q_grids, Q_rts_file, type='BYTE'
Read_Vars, unit, save_u_grids, u_rts_file, type='BYTE'
Read_Vars, unit, save_d_grids, d_rts_file, type='BYTE'
Read_Vars, unit, save_f_grids, f_rts_file, type='BYTE'
;-------------------------------------------------------
mstate.channel_vars.save_grid_dt = double(save_grid_dt)
mstate.channel_vars.save_Q_grids = save_Q_grids
mstate.channel_vars.save_u_grids = save_u_grids
mstate.channel_vars.save_d_grids = save_d_grids
mstate.channel_vars.save_f_grids = save_f_grids
mstate.channel_vars.Q_rts_file   = Q_rts_file
mstate.channel_vars.u_rts_file   = u_rts_file
mstate.channel_vars.d_rts_file   = d_rts_file
mstate.channel_vars.f_rts_file   = f_rts_file
;-------------------------------------------------------
Read_Vars, unit, dum_str, save_pixels_dt
Read_Vars, unit, save_Q_pixels, Q_out_file, type='BYTE'
Read_Vars, unit, save_u_pixels, u_out_file, type='BYTE'
Read_Vars, unit, save_d_pixels, d_out_file, type='BYTE'
Read_Vars, unit, save_f_pixels, f_out_file, type='BYTE'
;-----------------------------------------------------------
mstate.channel_vars.save_pixels_dt = double(save_pixels_dt)
mstate.channel_vars.save_Q_pixels  = save_Q_pixels
mstate.channel_vars.save_u_pixels  = save_u_pixels
mstate.channel_vars.save_d_pixels  = save_d_pixels
mstate.channel_vars.save_f_pixels  = save_f_pixels
mstate.channel_vars.Q_out_file     = Q_out_file
mstate.channel_vars.u_out_file     = u_out_file
mstate.channel_vars.d_out_file     = d_out_file
mstate.channel_vars.f_out_file     = f_out_file

;-----------------------------
;Read the meteorological vars
;-----------------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, rho_H2O_type, rho_H2O
Read_Vars, unit, rho_air_type, rho_air
Read_Vars, unit, Cp_air_type,  Cp_air
;-------------------------------------------
*mstate.met_vars.rho_H2O  = double(rho_H2O)
*mstate.met_vars.rho_air  = double(rho_air)
*mstate.met_vars.Cp_air   = double(Cp_air)
;-------------------------------------------
Read_Vars, unit, Qn_SW_type,     Qn_SW
Read_Vars, unit, Qn_LW_type,     Qn_LW
Read_Vars, unit, T_air_type,     T_air
Read_Vars, unit, T_surf_type,    T_surf
Read_Vars, unit, RH_type,        RH
Read_Vars, unit, p0_type,        p0
Read_Vars, unit, uz_type,        uz
Read_Vars, unit, z_type,         z
Read_Vars, unit, z0_air_type,    z0_air
;----------------------------------------------------------------------
Load_Var, Qn_SW, Qn_SW_type, mstate.met_vars.Qn_SW, Qn_SW_file
mstate.met_vars.Qn_SW_type = Type_Code(Qn_SW_type)
mstate.met_vars.Qn_SW_file = Qn_SW_file
Load_Var, Qn_LW, Qn_LW_type, mstate.met_vars.Qn_LW, Qn_LW_file
mstate.met_vars.Qn_LW_type = Type_Code(Qn_LW_type)
mstate.met_vars.Qn_LW_file = Qn_LW_file
Load_Var, T_air, T_air_type, mstate.met_vars.T_air, T_air_file
mstate.met_vars.T_air_type = Type_Code(T_air_type)
mstate.met_vars.T_air_file = T_air_file
Load_Var, T_surf, T_surf_type, mstate.met_vars.T_surf, T_surf_file
mstate.met_vars.T_surf_type = Type_Code(T_surf_type)
mstate.met_vars.T_surf_file = T_surf_file
Load_Var, RH, RH_type, mstate.met_vars.RH, RH_file
mstate.met_vars.RH_type = Type_Code(RH_type)
mstate.met_vars.RH_file = RH_file
Load_Var, p0, p0_type, mstate.met_vars.p0, p0_file
mstate.met_vars.p0_type = Type_Code(p0_type)
mstate.met_vars.p0_file = p0_file
Load_Var, uz, uz_type, mstate.met_vars.uz, uz_file
mstate.met_vars.uz_type = Type_Code(uz_type)
mstate.met_vars.uz_file = uz_file
Load_Var, z, z_type, mstate.met_vars.z, z_file
mstate.met_vars.z_type = Type_Code(z_type)
mstate.met_vars.z_file = z_file
Load_Var, z0_air, z0_air_type, mstate.met_vars.z0_air, z0_air_file
mstate.met_vars.z0_air_type = Type_Code(z0_air_type)
mstate.met_vars.z0_air_file = z0_air_file

;-----------------------
;Read the snowmelt vars
;-----------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, snow_method, type='BYTE'
Read_Vars, unit, dt_type, dt
;-----------------------------------------
mstate.snow_vars.method   = snow_method
mstate.snow_vars.dt       = double(dt)
;-----------------------------------------
Read_Vars, unit, Cp_snow_type,   Cp_snow
Read_Vars, unit, rho_snow_type,  rho_snow
Read_Vars, unit, c0_type,        c0
Read_Vars, unit, T0_type,        T0
Read_Vars, unit, h0_snow_type,   h0_snow
Read_Vars, unit, h0_swe_type,    h0_swe
;---------------------------------------------------------------------------
*mstate.snow_vars.Cp_snow = double(Cp_snow)  ;********
Load_Var, rho_snow, rho_snow_type, mstate.snow_vars.rho_snow, rho_snow_file
mstate.snow_vars.rho_snow_type = Type_Code(rho_snow_type)
mstate.snow_vars.rho_snow_file = rho_snow_file
Load_Var, c0, c0_type, mstate.snow_vars.c0, c0_file
mstate.snow_vars.c0_type = Type_Code(c0_type)
mstate.snow_vars.c0_file = c0_file
Load_Var, T0, T0_type, mstate.snow_vars.T0, T0_file
mstate.snow_vars.T0_type = Type_Code(T0_type)
mstate.snow_vars.T0_file = T0_file
Load_Var, h0_snow, h0_snow_type, mstate.snow_vars.h0_snow, h0_snow_file
mstate.snow_vars.h0_snow_type = Type_Code(h0_snow_type)
mstate.snow_vars.h0_snow_file = h0_snow_file
Load_Var, h0_swe, h0_swe_type, mstate.snow_vars.h0_swe, h0_swe_file
mstate.snow_vars.h0_swe_type = Type_Code(h0_swe_type)
mstate.snow_vars.h0_swe_file = h0_swe_file
;-----------------------------------------------------------------------
Read_Vars, unit, dum_str, save_grid_dt
Read_Vars, unit, save_mr_grids, mr_rts_file, type='BYTE'
Read_Vars, unit, save_hs_grids, hs_rts_file, type='BYTE'
Read_Vars, unit, save_sw_grids, sw_rts_file, type='BYTE'
Read_Vars, unit, save_cc_grids, cc_rts_file, type='BYTE'
Read_Vars, unit, save_ea_grids, ea_rts_file, type='BYTE'
Read_Vars, unit, save_es_grids, es_rts_file, type='BYTE'
;--------------------------------------------------------
mstate.snow_vars.save_grid_dt  = double(save_grid_dt)
mstate.snow_vars.save_mr_grids = save_mr_grids
mstate.snow_vars.save_hs_grids = save_hs_grids
mstate.snow_vars.save_sw_grids = save_sw_grids
mstate.snow_vars.save_cc_grids = save_cc_grids
mstate.snow_vars.save_ea_grids = save_ea_grids
mstate.snow_vars.save_es_grids = save_es_grids
;-----------------------------------------------
mstate.snow_vars.mr_rts_file   = mr_rts_file
mstate.snow_vars.hs_rts_file   = hs_rts_file
mstate.snow_vars.sw_rts_file   = sw_rts_file
mstate.snow_vars.cc_rts_file   = cc_rts_file
mstate.snow_vars.ea_rts_file   = ea_rts_file
mstate.snow_vars.es_rts_file   = es_rts_file
;-------------------------------------------------------
Read_Vars, unit, dum_str, save_pixels_dt
Read_Vars, unit, save_mr_pixels, mr_out_file, type='BYTE'
Read_Vars, unit, save_hs_pixels, hs_out_file, type='BYTE'
Read_Vars, unit, save_sw_pixels, sw_out_file, type='BYTE'
Read_Vars, unit, save_cc_pixels, cc_out_file, type='BYTE'
Read_Vars, unit, save_ea_pixels, ea_out_file, type='BYTE'
Read_Vars, unit, save_es_pixels, es_out_file, type='BYTE'
;----------------------------------------------------------
mstate.snow_vars.save_pixels_dt  = double(save_pixels_dt)
mstate.snow_vars.save_mr_pixels  = save_mr_pixels
mstate.snow_vars.save_hs_pixels  = save_hs_pixels
mstate.snow_vars.save_sw_pixels  = save_sw_pixels
mstate.snow_vars.save_cc_pixels  = save_cc_pixels
mstate.snow_vars.save_ea_pixels  = save_ea_pixels
mstate.snow_vars.save_es_pixels  = save_es_pixels
;--------------------------------------------------
mstate.snow_vars.mr_out_file     = mr_out_file
mstate.snow_vars.hs_out_file     = hs_out_file
mstate.snow_vars.sw_out_file     = sw_out_file
mstate.snow_vars.cc_out_file     = cc_out_file
mstate.snow_vars.ea_out_file     = ea_out_file
mstate.snow_vars.es_out_file     = es_out_file

;-----------------
;Read the ET vars
;-----------------
for k=1,4 do readf, unit, line
;---------------------------------------
Read_Vars, unit, ET_method, type='BYTE'
Read_Vars, unit, dt_type, dt
;---------------------------------------
mstate.ET_vars.method   = ET_method
mstate.ET_vars.dt       = double(dt)
;------------------------------------------
Read_Vars, unit, alpha_type,     alpha
Read_Vars, unit, Ks_type,        Ks
Read_Vars, unit, soil_x_type,    soil_x
Read_Vars, unit, T_soil_x_type,  T_soil_x
;---------------------------------------------------------------------------
Load_Var, alpha, alpha_type, mstate.ET_vars.alpha, alpha_file
mstate.ET_vars.alpha_type = Type_Code(alpha_type)
mstate.ET_vars.alpha_file = alpha_file
Load_Var, Ks, Ks_type, mstate.ET_vars.Ks, Ks_file
mstate.ET_vars.Ks_type = Type_Code(Ks_type)
mstate.ET_vars.Ks_file = Ks_file
Load_Var, soil_x, soil_x_type, mstate.ET_vars.soil_x, soil_x_file
mstate.ET_vars.soil_x_type = Type_Code(soil_x_type)
mstate.ET_vars.soil_x_file = soil_x_file
Load_Var, T_soil_x, T_soil_x_type, mstate.ET_vars.T_soil_x, T_soil_x_file
mstate.ET_vars.T_soil_x_type = Type_Code(T_soil_x_type)
mstate.ET_vars.T_soil_x_file = T_soil_x_file
;---------------------------------------------------------------------------
Read_Vars, unit, dum_str, save_grid_dt
Read_Vars, unit, save_er_grids, er_rts_file, type='BYTE'
;** Read_Vars, unit, save_hs_grids, hs_rts_file, type='BYTE'
;------------------------------------------------------
mstate.ET_vars.save_grid_dt  = double(save_grid_dt)
mstate.ET_vars.save_er_grids = save_er_grids
mstate.ET_vars.er_rts_file   = er_rts_file
;** mstate.ET_vars.save_hs_grids = save_hs_grids 
;** mstate.ET_vars.hs_rts_file   = hs_rts_file
;---------------------------------------------------------
Read_Vars, unit, dum_str, save_pixels_dt
Read_Vars, unit, save_er_pixels, er_out_file, type='BYTE'
;** Read_Vars, unit, save_hs_pixels, hs_out_file, type='BYTE'
;---------------------------------------------------------
mstate.ET_vars.save_pixels_dt = double(save_pixels_dt)
mstate.ET_vars.save_er_pixels = save_er_pixels
;** mstate.ET_vars.save_hs_pixels = save_hs_pixels
mstate.ET_vars.er_out_file    = er_out_file
;** mstate.ET_vars.hs_out_file    = hs_out_file

;--------------------
;Read the infil vars
;--------------------
for k=1,4 do readf, unit, line
;--------------------------------------------
Read_Vars, unit, infil_method,   type='BYTE'
Read_Vars, unit, n_layers, type='INTEGER'
Read_Vars, unit, dt_type,  dt
;-----------------------------------------
mstate.infil_vars.method   = infil_method
mstate.infil_vars.n_layers = n_layers
mstate.infil_vars.dt       = double(dt)
;-----------------------------------------
if (infil_method lt 4) then begin
    ;---------------------------------------
    ;Vars for Green-Ampt and Smith-Parlange
    ;---------------------------------------
    Read_Vars, unit, Ks_type, Ks
    Read_Vars, unit, Ki_type, Ki
    Read_Vars, unit, qs_type, qs
    Read_Vars, unit, qi_type, qi
    Read_Vars, unit, G_type,   G
    Read_Vars, unit, gam_type, gam
    Read_Vars, unit, soil_type, type='STRING'
    ;------------------------------------------------------------
    Load_Var, Ks, Ks_type, mstate.infil_vars.Ks_val[0], Ks_file
    mstate.infil_vars.Ks_type[0] = Type_Code(Ks_type)
    mstate.infil_vars.Ks_file[0] = Ks_file
    Load_Var, Ki, Ki_type, mstate.infil_vars.Ki_val[0], Ki_file
    mstate.infil_vars.Ki_type[0] = Type_Code(Ki_type)
    mstate.infil_vars.Ki_file[0] = Ki_file
    Load_Var, qs, qs_type, mstate.infil_vars.qs_val[0], qs_file
    mstate.infil_vars.qs_type[0] = Type_Code(qs_type)
    mstate.infil_vars.qs_file[0] = qs_file
    Load_Var, qi, qi_type, mstate.infil_vars.qi_val[0], qi_file
    mstate.infil_vars.qi_type[0] = Type_Code(qi_type)
    mstate.infil_vars.qi_file[0] = qi_file
    ;------------------------------------------------------------
    Load_Var, G, G_type, mstate.infil_vars.G, G_file
    mstate.infil_vars.G_type = Type_Code(G_type)
    mstate.infil_vars.G_file = G_file
    Load_Var, gam, gam_type, mstate.infil_vars.gam, gam_file
    mstate.infil_vars.gam_type = Type_Code(gam_type)
    mstate.infil_vars.gam_file = gam_file
    ;----------------------------------------
    mstate.infil_vars.soil_type[0] = soil_type
endif else begin
    ;-----------------------------------
    ;Vars for Richards' equation method
    ;-----------------------------------
    for k=0,(n_layers-1) do begin
        Read_Vars, unit, Ks_type,  Ks
        Read_Vars, unit, Ki_type,  Ki
        Read_Vars, unit, qs_type,  qs
        Read_Vars, unit, qi_type,  qi
        Read_Vars, unit, qr_type,  qr   ;**** added 3/14/08
        Read_Vars, unit, pB_type,  pB
        Read_Vars, unit, pA_type,  pA
        Read_Vars, unit, lam_type, lam
        Read_Vars, unit, c_type,   c
        Read_Vars, unit, dz_type,  dz
        Read_Vars, unit, nz, type='INTEGER'
        Read_Vars, unit, soil_type, type='STRING'
        ;----------------------------------------------------------------
        Load_Var, Ks, Ks_type, mstate.infil_vars.Ks_val[k], Ks_file
        mstate.infil_vars.Ks_type[k] = Type_Code(Ks_type)
        mstate.infil_vars.Ks_file[k] = Ks_file
        Load_Var, Ki, Ki_type, mstate.infil_vars.Ki_val[k], Ki_file
        mstate.infil_vars.Ki_type[k] = Type_Code(Ki_type)
        mstate.infil_vars.Ki_file[k] = Ki_file
        Load_Var, qs, qs_type, mstate.infil_vars.qs_val[k], qs_file
        mstate.infil_vars.qs_type[k] = Type_Code(qs_type)
        mstate.infil_vars.qs_file[k] = qs_file
        Load_Var, qi, qi_type, mstate.infil_vars.qi_val[k], qi_file
        mstate.infil_vars.qi_type[k] = Type_Code(qi_type)
        mstate.infil_vars.qi_file[k] = qi_file
        Load_Var, qr, qr_type, mstate.infil_vars.qr_val[k], qr_file   ;**** added 3/14/08
        mstate.infil_vars.qr_type[k] = Type_Code(qr_type)
        mstate.infil_vars.qr_file[k] = qr_file
        Load_Var, pB, pB_type, mstate.infil_vars.pB_val[k], pB_file
        mstate.infil_vars.pB_type[k] = Type_Code(pB_type)
        mstate.infil_vars.pB_file[k] = pB_file
        Load_Var, pA, pA_type, mstate.infil_vars.pA_val[k], pA_file
        mstate.infil_vars.pA_type[k] = Type_Code(pA_type)
        mstate.infil_vars.pA_file[k] = pA_file
        Load_Var, lam, lam_type, mstate.infil_vars.lam_val[k], lam_file
        mstate.infil_vars.lam_type[k] = Type_Code(lam_type)
        mstate.infil_vars.lam_file[k] = lam_file
        Load_Var, c, c_type, mstate.infil_vars.c_val[k], c_file
        mstate.infil_vars.c_type[k] = Type_Code(c_type)
        mstate.infil_vars.c_file[k] = c_file
        ;----------------------------------------------------------------
        mstate.infil_vars.dz_val[k]    = double(dz)
        mstate.infil_vars.nz_val[k]    = fix(nz)
        mstate.infil_vars.soil_type[k] = soil_type
    endfor
endelse
;------------------------------------------
Read_Vars, unit, dum_str, save_grid_dt
Read_Vars, unit, save_v0_grids, v0_rts_file, type='BYTE'
Read_Vars, unit, save_q0_grids, q0_rts_file, type='BYTE'
Read_Vars, unit, save_I_grids,  I_rts_file,  type='BYTE'
Read_Vars, unit, save_Zw_grids, Zw_rts_file, type='BYTE'
;--------------------------------------------------------
mstate.infil_vars.save_grid_dt  = double(save_grid_dt)
mstate.infil_vars.save_v0_grids = save_v0_grids
mstate.infil_vars.save_q0_grids = save_q0_grids
mstate.infil_vars.save_I_grids  = save_I_grids
mstate.infil_vars.save_Zw_grids = save_Zw_grids
mstate.infil_vars.v0_rts_file   = v0_rts_file
mstate.infil_vars.q0_rts_file   = q0_rts_file
mstate.infil_vars.I_rts_file    = I_rts_file
mstate.infil_vars.Zw_rts_file   = Zw_rts_file
;------------------------------------------------------
Read_Vars, unit, dum_str, save_pixels_dt
Read_Vars, unit, save_v0_pixels, v0_out_file, type='BYTE'
Read_Vars, unit, save_q0_pixels, q0_out_file, type='BYTE'
Read_Vars, unit, save_I_pixels,  I_out_file,  type='BYTE'
Read_Vars, unit, save_Zw_pixels, Zw_out_file, type='BYTE'
;---------------------------------------------------------
mstate.infil_vars.save_pixels_dt = double(save_pixels_dt)
mstate.infil_vars.save_v0_pixels = save_v0_pixels
mstate.infil_vars.save_q0_pixels = save_q0_pixels
mstate.infil_vars.save_I_pixels  = save_I_pixels
mstate.infil_vars.save_Zw_pixels = save_Zw_pixels
mstate.infil_vars.v0_out_file    = v0_out_file
mstate.infil_vars.q0_out_file    = q0_out_file
mstate.infil_vars.I_out_file     = I_out_file
mstate.infil_vars.Zw_out_file    = Zw_out_file
;---------------------------------------------------------
Read_Vars, unit, dum_str, save_stack_dt
Read_Vars, unit, save_q_stacks, q_stack_file, type='BYTE'
Read_Vars, unit, save_p_stacks, p_stack_file, type='BYTE'
Read_Vars, unit, save_K_stacks, K_stack_file, type='BYTE'
Read_Vars, unit, save_v_stacks, v_stack_file, type='BYTE'
;---------------------------------------------------------
mstate.infil_vars.save_stack_dt = double(save_stack_dt)
mstate.infil_vars.save_q_stacks = save_q_stacks
mstate.infil_vars.save_p_stacks = save_p_stacks
mstate.infil_vars.save_K_stacks = save_K_stacks
mstate.infil_vars.save_v_stacks = save_v_stacks
mstate.infil_vars.q_stack_file  = q_stack_file
mstate.infil_vars.p_stack_file  = p_stack_file
mstate.infil_vars.K_stack_file  = K_stack_file
mstate.infil_vars.v_stack_file  = v_stack_file
;-------------------------------------------------------------
Read_Vars, unit, dum_str, save_profile_dt
Read_Vars, unit, save_q_profiles, q_profile_file, type='BYTE'
Read_Vars, unit, save_p_profiles, p_profile_file, type='BYTE'
Read_Vars, unit, save_K_profiles, K_profile_file, type='BYTE'
Read_Vars, unit, save_v_profiles, v_profile_file, type='BYTE'
;-------------------------------------------------------------
mstate.infil_vars.save_profile_dt = double(save_profile_dt)
mstate.infil_vars.save_q_profiles = save_q_profiles
mstate.infil_vars.save_p_profiles = save_p_profiles
mstate.infil_vars.save_K_profiles = save_K_profiles
mstate.infil_vars.save_v_profiles = save_v_profiles
mstate.infil_vars.q_profile_file  = q_profile_file
mstate.infil_vars.p_profile_file  = p_profile_file
mstate.infil_vars.K_profile_file  = K_profile_file
mstate.infil_vars.v_profile_file  = v_profile_file


;-----------------
;Read the GW vars
;-----------------
for k=1,4 do readf, unit, line
;----------------------------------------
Read_Vars, unit, GW_method, type='BYTE'
Read_Vars, unit, nlayers, type='INTEGER'
Read_Vars, unit, dt_type, dt
;----------------------------------------
mstate.GW_vars.method  = GW_method
mstate.GW_vars.nlayers = nlayers
mstate.GW_vars.dt      = double(dt)
;----------------------------------------
Read_Vars, unit, elev_type,     elev
Read_Vars, unit, h0_table_type, h0_table
Read_Vars, unit, d_freeze_type, d_freeze
Read_Vars, unit, d_thaw_type,   d_thaw
;--------------------------------------------------------------------------
Load_Var, elev, elev_type, mstate.GW_vars.elev, elev_file
mstate.GW_vars.elev_type = Type_Code(elev_type)
mstate.GW_vars.elev_file = elev_file
Load_Var, h0_table, h0_table_type, mstate.GW_vars.h0_table, h0_table_file
mstate.GW_vars.h0_table_type = Type_Code(h0_table_type)
mstate.GW_vars.h0_table_file = h0_table_file
Load_Var, d_freeze, d_freeze_type, mstate.GW_vars.d_freeze, d_freeze_file
mstate.GW_vars.d_freeze_type = Type_Code(d_freeze_type)
mstate.GW_vars.d_freeze_file = d_freeze_file
Load_Var, d_thaw, d_thaw_type, mstate.GW_vars.d_thaw, d_thaw_file
mstate.GW_vars.d_thaw_type = Type_Code(d_thaw_type)
mstate.GW_vars.d_thaw_file = d_thaw_file
;--------------------------------------------------------------------------
for k=0,(nlayers-1) do begin
    Read_Vars, unit, Ks_type, Ks
    Read_Vars, unit, qs_type, qs
    Read_Vars, unit, th_type, th
    ;-----------------------------------------------------
    Load_Var, Ks, Ks_type, mstate.GW_vars.Ks[k], Ks_file
    mstate.GW_vars.Ks_type[k] = Type_Code(Ks_type)
    mstate.GW_vars.Ks_file[k] = Ks_file
    Load_Var, qs, qs_type, mstate.GW_vars.qs[k], qs_file
    mstate.GW_vars.qs_type[k] = Type_Code(qs_type)
    mstate.GW_vars.qs_file[k] = qs_file
    Load_Var, th, th_type, mstate.GW_vars.th[k], th_file
    mstate.GW_vars.th_type[k] = Type_Code(th_type)
    mstate.GW_vars.th_file[k] = th_file
endfor
;----------------------------------------
Read_Vars, unit, dum_str, save_grid_dt
Read_Vars, unit, save_ht_grids, ht_rts_file, type='BYTE'
Read_Vars, unit, save_df_grids, df_rts_file, type='BYTE'
Read_Vars, unit, save_dt_grids, dt_rts_file, type='BYTE'
;--------------------------------------------------------
mstate.GW_vars.save_grid_dt  = double(save_grid_dt)
mstate.GW_vars.save_ht_grids = save_ht_grids
mstate.GW_vars.save_df_grids = save_df_grids
mstate.GW_vars.save_dt_grids = save_dt_grids
mstate.GW_vars.ht_rts_file   = ht_rts_file
mstate.GW_vars.df_rts_file   = dt_rts_file
mstate.GW_vars.dt_rts_file   = dt_rts_file
;---------------------------------------------------------
Read_Vars, unit, dum_str, save_pixels_dt
Read_Vars, unit, save_ht_pixels, ht_out_file, type='BYTE'
Read_Vars, unit, save_df_pixels, df_out_file, type='BYTE'
Read_Vars, unit, save_dt_pixels, dt_out_file, type='BYTE'
;---------------------------------------------------------
mstate.GW_vars.save_pixels_dt = double(save_pixels_dt)
mstate.GW_vars.save_ht_pixels = save_ht_pixels
mstate.GW_vars.save_df_pixels = save_df_pixels
mstate.GW_vars.save_dt_pixels = save_dt_pixels
mstate.GW_vars.ht_out_file    = ht_out_file
mstate.GW_vars.df_out_file    = dt_out_file
mstate.GW_vars.dt_out_file    = dt_out_file

;------------------------
;Read the diversion vars
;------------------------
for k=1,4 do readf, unit, line
;------------------------------
Read_Vars, unit, diversion_method, type='BYTE'
Read_Vars, unit, use_sources, source_file, 'BYTE'
Read_Vars, unit, use_sinks,   sink_file,   'BYTE'
Read_Vars, unit, use_canals,  canal_file,  'BYTE'
;----------------------------------------------------
mstate.diversion_vars.method      = diversion_method
mstate.diversion_vars.use_sources = use_sources
mstate.diversion_vars.use_sinks   = use_sinks
mstate.diversion_vars.use_canals  = use_canals
;----------------------------------------------------
mstate.diversion_vars.source_file = source_file
mstate.diversion_vars.sink_file   = sink_file
mstate.diversion_vars.canal_file  = canal_file

;-----------------------
;Read the stopping vars
;-----------------------
for k=1,4 do readf, unit, line
;--------------------------------------------
Read_Vars, unit, stop_method,  type='BYTE'
Read_Vars, unit, Qp_fraction,  type='DOUBLE'
Read_Vars, unit, T_stop_model, type='DOUBLE'
Read_Vars, unit, T_stop_real,  type='DOUBLE'
Read_Vars, unit, n_steps,      type='LONG'
;--------------------------------------------
mstate.stop_vars.method       = stop_method
mstate.stop_vars.Qp_fraction  = Qp_fraction
mstate.stop_vars.T_stop_model = T_stop_model
mstate.stop_vars.T_stop_real  = T_stop_real
mstate.stop_vars.n_steps      = n_steps

;---------------------
;Close the input file
;---------------------
free_lun, unit

;-----------------------------------
;Copy run vars into "run var" panel
;-----------------------------------
widget_control, mstate.run_vars.prefix_ID,       set_value=prefix
widget_control, mstate.run_vars.run_prefix_ID,   set_value=run_prefix
widget_control, mstate.run_vars.directory_ID,    set_value=directory
widget_control, mstate.run_vars.log_file_ID,     set_value=log_file
widget_control, mstate.run_vars.comment_file_ID, set_value=comment_file

;-------------------------------------------
;Copy run comments into comment text box ??
;-------------------------------------------
;Read comments from comment file.
;Copy comments into text box.

;---------------------------------
;Set all of the methods droplists
;---------------------------------
widget_control, mstate.precip_vars.method_ID, $
                set_droplist_select=precip_method
widget_control, mstate.channel_vars.method_ID, $
                set_droplist_select=channel_method
widget_control, mstate.snow_vars.method_ID, $
                set_droplist_select=snow_method
widget_control, mstate.ET_vars.method_ID, $
                set_droplist_select=ET_method
widget_control, mstate.GW_vars.method_ID, $
                set_droplist_select=GW_method
widget_control, mstate.infil_vars.method_ID, $
                set_droplist_select=infil_method
widget_control, mstate.diversion_vars.method_ID, $
                set_droplist_select=diversion_method
widget_control, mstate.stop_vars.method_ID, $
                set_droplist_select=stop_method

;---------------------------------------------------
;Copy new information into "monitored pixels" panel
;---------------------------------------------------
if (n_pixels gt 0) then begin
    np    = n_pixels
    nmax  = (np > 100)
    list  = dblarr(4, nmax)
    list[0,0:np-1] = cols
    list[1,0:np-1] = rows
    list[2,0:np-1] = areas     ;[km^2]
    list[3,0:np-1] = reliefs   ;[m]
    widget_control, mstate.grid_vars.basin_table_ID, set_value=list
endif
widget_control, mstate.grid_vars.RTM_file_ID, set_value=RTM_file
widget_control, mstate.grid_vars.get_pvolume_ID, $
                set_button=get_pvolume
widget_control, mstate.grid_vars.RTM_file_ID, sensitive=get_pvolume

;---------------------------
;Display information dialog
;---------------------------
msg = [ $
'Finished reading input variables from the file:', $
' ', in_file, ' ']
result = GUI_Message(msg, /INFO, TITLE='Load Settings Status')

end;  Load_All_TF_Vars
;******************************************************************


