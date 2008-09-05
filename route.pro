
;NB!  The functions:  Number_of_Samples and T_Stop_Model_Estimate
;     still need a better approach for "Get Outfile Size" 

;Idea:  Rename "stack_file" and "stack_unit" to "rt3_file" and
;       "rt3_unit" via search and replace in getvars.pro and in
;       route.pro.  Something similar for "profile_file", etc. ?

;************************************************************************
;   route.pro  

;   Copyright (c) 2001-2008,  Scott D. Peckham
;   Created:   Oct 2001 - Jan 2002
;   Modified:  June 2002, Sep 2002, Feb-Mar 2004, July 2005
;   Modified:  Jan 2006, April-July 2006
;   Modified:  Feb 2006 (e.g. Qnet -> Qn_SW & Qn_LW)
;   Modified:  Mar 2007 (continuous saving of outlet values)
;   Modified:  Mar 2007 (wrote set of I/O procedures)
;   Modified:  Mar 2007 (Added diffusive wave option)
;   Modified:  Mar 2008 (Updates to Initialize_Infil_Vars)

;************************************************************************

;   Precipitation          (function)
;   Snowmelt               (function)
;   Evaporation            (function)
;   Infiltration           (function)
;   Seepage                (function)

;   Number_of_Samples      (function, 2/10/04)
;   Sample_Step            (function, 2/10/04)
;   RTG                    (function, 3/10/04)
;   Pixel_Var              (function, 3/10/04)
;   Stack                  (function, 7/14/06)
;   Profile_Var            (function, 7/15/06)

;   Remove_Bad_Slopes      (7/18/05)
;   Update_Flow_Volume 
;   Update_Flow_Depth      (7/21/05)
;   Update_Overland_Flow   (2/17/07)  (Not used yet)
;   Update_Water_Table
;   Update_Snow_Depth      (not written; needed?)

;   Update_Mass_Totals     (3/20/07)
;   Update_Volume_In       (7/21/05)
;   Update_Volume_Out      (7/21/05)
;   Update_Velocity_Dynamic     (2/12/07;  bug fix)

;   Initialize_Precip_Vars   (3/19/07, separate)
;   Initialize_Channel_Vars  (5/12/06)
;   Initialize_Snow_Vars
;   Initialize_ET_Vars
;   Build_Layered_Var        (5/13/06)
;   Initialize_Infil_Vars    (5/13/06)
;   Initialize_GW_Vars

;   Route_Flow               (Main program)

;   Check_Flow_Depth
;   Check_Flow_Velocity
;   Check_Infiltration        (3/20/07)
;   Check_Steady_State        (3/8/07)

;   Print_Final_Report
;   Print_Mins_And_Maxes
;   Print_Uniform_Precip_Data
;   Print_Dimless_Number_Data

;   Check_Output_Options      (3/8/07,   separate routine)
;   Open_New_RTS_Files        (7/21/05)
;   Write_Pixel_File_Header   (3/8/07)
;   Open_New_Pixel_Files      (3/8/07)
;   Save_Pixel_Values         (3/8/07,   separate routine)
;   Save_Grid_Values          (3/20/07,  separate routine)
;   Write_Profile             (7/15/06)
;   Close_All_Output_Files    (3/8/07)

;************************************************************************
function Precipitation, pv, mv, sv, time, duration_sums

;------------------------------------------------
;NOTES:  pv = precip_vars = structure
;        mv = met_vars = structure
;        sv = snow_vars = structure

;        P  = precip rate with units of [m/s],
;             converted from [mm/hr] by the
;             routines in GUI_precip.pro
;        Will this work if some intervals have
;        precip. rates of zero ??

;        In the case of (pv.method eq 2), must
;        pass the P_grids variable which is set
;        up in Route_Flow for associated I/O.

;        Removed use of P_Grids on 7/15/05.  Now
;        using pointers and Update_Precip_Vars.
;------------------------------------------------
;7/13/06  Return immediately if METHOD eq 0;
;         note that duration_sums is undefined.
;------------------------------------------------
if (pv.method eq 0) then RETURN, 0.0

;------------------------------------
;Turn off precip if time exceeds max
;of the precip. rate duration sums
;------------------------------------
wd = where(time lt duration_sums, nwd)
if (nwd eq 0) then RETURN, 0.0   ;(scalar)

;----------------------------------
;Get precip rate for this timestep
;----------------------------------
case (pv.method) of
    0b : P = 0.0                  ;(no precip)
    1b : P = (*pv.rates)[wd[0]]   ;(scalar)
    2b : P = *pv.rates            ;(scalar/series/RTG/RTS)
endcase

;---------------------------------------
;Save the maximum precip. rate in [m/s]
;---------------------------------------
pv.max_rate = pv.max_rate > max(P)

;----------------------------------------------------
;NB!  This turned out to be a bad idea, because
;     *pv.rates is used in many various other places,
;     such as to initialize infiltration.  Now rates
;     are read with units of [mm/hr] but converted
;     before storage and use to [m/s].
;----------------------------------------------------
;NB!  Units are read and saved with units of [mm/hr]
;     and then this function converts from [mm/hr]
;     to [m/s] prior to use.  (7/25/06)
;-----------------------------------------------------
;*** P = P / (1000d * 3600d)   ;[m/s]

;---------------------------------
;Method can't change during a run
;so don't need to update any vars
;---------------------------------
if (pv.method eq 0) then RETURN, P

;-----------------------------------------
;Read next precip vars from input files ?
;-----------------------------------------
Update_Precip_Vars, pv

;-----------------------------------
;Don't allow precip to fall as snow
;if there is no Snowmelt process
;-----------------------------------
;if (sv.method eq 0) then RETURN, P
;-------------------------------------
;Allow precip to fall as snow even if
;the Snowmelt process is turned off
;-------------------------------------

;--------------------------------------------
;Update snow water equivalent and snow depth
;--------------------------------------------
;If P or T is a grid, then we must have
;h_swe and h_snow be grids.  This is set
;up at start of Route_Flow.
;-------------------------------------------
;(3/14/07) New method that works regardless
;of whether P and T are scalars or grids.
;-------------------------------------------
dh_swe     = (P * (*mv.T_air le 0)) * pv.dt
*sv.h_swe  = (*sv.h_swe + dh_swe)
ratio      = (*mv.rho_H2O / *sv.rho_snow)
*sv.h_snow = *sv.h_swe * ratio

;-------------------------------------------
;Where precip falls as snow and gets saved
;to SWE, P must be set to zero so it can't
;go on to produce runoff.
;---------------------------------------------
;A special situation occurs if P is a scalar
;and T is a grid.  P must then be converted
;to a grid in order to be able to reflect the
;fact that water was stored as snow in some
;pixels and not at others.  This happens
;automatically as written here. (3/14/07)
;If P and T are both scalars, P stays scalar.
;---------------------------------------------
P = (P * (dh_swe eq 0))

;------------------------------------
;Message about rainfall or snowfall
;------------------------------------
;Note that rain and snow may fall
;simultaneously at different pixels
;------------------------------------
;Reporting this slows model too much
;but can be used for testing
;------------------------------------
;w1 = where(dh_swe gt 0, n1)
;if (n1 gt 0) then TF_Print,'   Snow is falling...'
;--------------------------------------------------
;dh_rain = (P * (*mv.T_air gt 0))
;w2 = where(dh_rain gt 0, n2)
;if (n2 gt 0) then TF_Print,'   Rain is falling...'

;--------------
;For debugging
;--------------
;nT = n_elements(*mv.T_air)
;nP = n_elements(P)
;if (nT eq 1) then begin
;    tstr = TF_String(*mv.T_air)
;    TF_Print,'   T_air = ' + tstr
;endif
;if (nP eq 1) then begin
;    pstr = TF_String(P)
;    TF_Print,'   P     = ' + pstr
;endif

RETURN, P
END;    Precipitation
;************************************************************************
function Snowmelt, s, m, time

;------------------------------------------------
;NOTE:  s  = snow_vars = structure
;       m  = met_vars = structure
;       SM = snowmelt rate with units of [m/s].
;       Each method should update snow depth as
;       snow melts.
;------------------------------------------------

;--------------
;For debugging
;--------------
;nT = n_elements(*m.T_air)
;if (nT eq 1) then begin
;    tstr = TF_String(*m.T_air)
;    TF_Print,'   T_air = ' + tstr
;endif

;------------------------------
;Compute the snowmelt rate, SM
;------------------------------
case (s.method) of
    0 : SM = 0.0
    1 : SM = Degree_Day_Meltrate(s.c0, s.T0, m.T_air)
    2 : SM = Energy_Balance_Meltrate(m.Qn_SW, m.Qn_LW, m.T_air, m.T_surf, $
                                     m.RH, m.p0, m.uz, m.z, m.z0_air, $
                                     m.rho_air, m.Cp_air, $
                                     ;-------------------------------------
                                     s.Ecc, s.h_snow, s.rho_snow, $
                                     s.Cp_snow, s.dt, $
                                     ;-------------------------------------
                                     e_air, e_surf)  ;(these 2 returned)
endcase

;---------------------------------
;Method can't change during a run
;so don't need to update any vars
;---------------------------------
if (s.method eq 0) then RETURN, SM

;---------------------------
;Save new vapor pressures ?
;---------------------------
if (s.method eq 2) then begin
    *m.e_air  = e_air    ;[mbars]  (see Latent_Heat_Flux function)
    *m.e_surf = e_surf   ;[mbars]
endif

;---------------------------------------
;Read next snow vars from input files ?
;---------------------------------------
Update_Snow_Vars, s

;---------------------------------
;Make sure that meltrate does not
;exceed max possible meltrate
;---------------------------------
SM_max = Max_Meltrate(s.h_snow, m.rho_H2O, s.rho_snow, s.dt)
SM = (SM < SM_max) > 0.0

;------------------------------------------
;Update/decrease the snow water equivalent
;------------------------------------------
*s.h_swe = (*s.h_swe - (SM * s.dt)) > 0.0

;-----------------------------------
;Decrease snow depth due to melting
;-----------------------------------
ratio     = (*m.rho_H2O / *s.rho_snow) 
dh        = SM * ratio * s.dt
*s.h_snow = (*s.h_snow - dh) > 0.0

;----------------------------------------
;Increase snow depth due to falling snow
;This is done by Precipitation function.
;----------------------------------------

RETURN, SM
END;    Snowmelt
;************************************************************************
function Evaporation, ev, mv, sv, gv, iv, depth, h, y  ;***, h_snow

;------------------------------------------------------
;NOTE:  ET = ET rate with units of [m/s].
;       ev = ET_vars = structure
;       mv = met_vars = structure
;       gv = gw_vars = structure
;       iv = infil_vars = structure
;    depth = depth of surface water [m]
;        h = water table height above datum
;        y = thicknesses [m] of all soil layers
;            when using Darcy subsurface flow
;--------------------------------------------------------------
;NB!  h_snow is needed by the Bulk_Exchange_Coeff function
;     (which is called by the Energy_Balance_ET_Rate function)
;     to adjust reference height, z.  Don't just use h0_snow.
;--------------------------------------------------------------
case (ev.method) of
    0 : ET = 0.0
    1 : ET = Priestley_Taylor_ET_Rate(ev.alpha, ev.Ks, $
                                      ev.T_soil_x, ev.soil_x, $
                                      ;--------------------------
                                      mv.Qn_SW, mv.Qn_LW, $
                                      mv.T_air, mv.T_surf)
    2 : ET = Energy_Balance_ET_Rate(ev.Ks, ev.T_soil_x, ev.soil_x, $
                                    ;------------------------------------
                                    mv.Qn_SW, mv.Qn_LW, mv.T_air, $
                                    mv.T_surf, mv.uz, mv.z, mv.z0_air, $
                                    mv.rho_air, mv.Cp_air, $
                                    ;------------------------------------
                                    sv.h_snow)
endcase

;---------------------------------
;Method can't change during a run
;so don't need to update any vars
;---------------------------------
if (ev.method eq 0) then RETURN, ET

;-------------------------------------
;Read next ET vars from input files ?
;-------------------------------------
Update_ET_Vars, ev

;----------------------------------
;Convert ET from scalar to grid ?
;----------------------------------
;Assume h is always a grid.
;h, dzw and ET must be compatible.
;----------------------------------
nh  = n_elements(h)
nET = n_elements(ET)
if (nh gt 1) AND (nET eq 1) then begin
    s  = size(h, /dimensions)
    nx = s[0]
    ny = s[1]
    ET = (ET + fltarr(nx,ny))
    nET = n_elements(ET)
endif

;-----------------------------------------
;If Richards' equation is being used for
;infiltration, then don't need to remove
;water from layers as done in remainder
;and y (wetted thicknesses) is not needed
;-----------------------------------------
;But still need to remove surface water
;first !!  This isn't done yet. ********
;-----------------------------------------
RICHARDS = (iv.method eq 4b)
if (RICHARDS) then RETURN, ET

;-----------------------------------
;Depth of water to be removed by ET
;-----------------------------------
dzw = (ev.dt * ET)


;--------------
;For debugging
;--------------
;if (n_elements(dzw) eq 1) then begin
;    msg = [' ','ERROR: dzw is not an array. ', ' ']
;    result = GUI_Message(msg, /INFO)
;    STOP
;endif

wL = where(dzw le depth, nwL, COMP=wG, NCOMP=nwG)


if (nwL ne 0) then begin
    ;-------------------------------
    ;Reduce the surface water depth
    ;-------------------------------
    depth[wL] = (depth[wL] - dzw[wL])
    dzw[wL]   = 0.0
endif

if (nwG ne 0) then begin
    ;---------------------------
    ;Save a copy of initial dzw
    ;---------------------------
    dzw0 = dzw

    ;-----------------------------------
    ;Consume all surface water first
    ;This doesn't account for channels.
    ;-----------------------------------
    dzw[wG]   = dzw[wG] - depth[wG]
    depth[wG] = 0.0

    ;-------------------------------------
    ;Try to take remainder from top layer
    ;Compute water content of top layer
    ;-------------------------------------
    ;Used before 7/13/06
    ;--------------------
    ;p  = gv.soil_P[0]  ;(top layer porosity)
    ;y1 = y[*,*,0]
    ;content_1 = (y1[wG] * p)
    ;-----------------------------------------
    p1 = *gv.qs[0]  ;(top layer porosity)
    y1 = y[*,*,0]
    SCALAR_POROSITY = (n_elements(p1) eq 1)
    if (SCALAR_POROSITY) then begin
        content_1 = (y1[wG] * p1)
    endif else begin
        content_1 = (y1[wG] * p1[wG])
    endelse

    wwL = where(dzw[wG] le content_1, nwwL, COMP=wwG, NCOMP=nwwG)

    ;-------------------------------------------
    ;Can get all remaining water from top layer
    ;Reduce the water table height
    ;-------------------------------------------
    if (nwwL ne 0) then begin
        ;*** dh      = (dzw[wG[wwL]] / p)
        ;--------------------------------------
        if (SCALAR_POROSITY) then begin
            dh = dzw[wG[wwL]] / p1
        endif else begin
            dh = dzw[wG[wwL]] / p1[wG[wwL]]
        endelse

        h[wG[wwL]]   = h[wG[wwL]]  - dh
        y1[wG[wwL]]  = y1[wG[wwL]] - dh
        dzw[wG[wwL]] = 0.0   ;(not really needed ?)
    endif

    ;---------------------------------------------
    ;Can't get all remaining water from top layer
    ;---------------------------------------------
    ;Get what is available, and then redefine ET
    ;for mass balance consistency
    ;---------------------------------------------
    if (nwwG ne 0) then begin
        dh           = y1[wG[wwG]]
        h[wG[wwG]]   = h[wG[wwG]] - dh
        y1[wG[wwG]]  = 0.0
        dzw[wG[wwG]] = dzw[wG[wwG]] - content_1[wwG]
        ;--------------------------------------------
        dzw_used     = dzw0[wG[wwG]] - dzw[wG[wwG]]
        ET[wG[wwG]]  = (dzw_used / ev.dt)
    endif

    ;-----------------------
    ;Replace top layer in y
    ;-----------------------
    y[*,*,0] = y1

endif

RETURN, ET

END;    Evaporation
;************************************************************************
function Infiltration, iv, P, SM, ET, h, z, Rg, r_last, n, nx, ny

;----------------------------------------------------------
;NOTE:  iv = infil_vars  = structure
;       pv = precip_vars = structure
;       P  = precipitation rate [m/s]
;       SM = snowmelt rate [m/s]
;       r  = (P + SM)  [m/s]
;       ET = evapotranspiration rate [m/s]
;       IN = infiltration rate with units of [m/s]
;       Rg = rate at which water reaches water table [m/s]
;            (Rg is returned)
;       h  = water table elevation [m]
;       z  = land surface elevation [m]
;       I  = total infiltrated depth [m]
;       n  = time step (for computing t_start & t_end)
;----------------------------------------------------------
r = (P + SM)           ;(P_total, not R=runoff)

;*************************************************
;NB! Other infil methods should also return Rg,
;    in addition to the Richards infil method.
;    Need this for computing "vertical exit mass"
;*************************************************
Rg = 0.0   ;(default)

case (iv.method) of
    0 : IN = 0.0
    1 : IN = r
    ;------------------------------------------------------
    ;These next two are not totally correct but are stable
    ;and give results similar to the correct method
    ;------------------------------------------------------
    2 : IN = Green_Ampt_Infil_Rate_v1(iv, r)
    3 : IN = Smith_Parlange_Infil_Rate_v1(iv, r)
    ;------------------------------------------------
    ;These next two use the correct approach but so
    ;far have convergence and "jump" issues
    ;------------------------------------------------
    ;** 2 : IN = Green_Ampt_Infil_Rate_v2(iv, r, r_last, n)
    ;** 3 : IN = Smith_Parlange_Infil_Rate_v2(iv, r, r_last, n)
    ;-----------------------------------------------
    4 : IN = Richards_Infil_Rate(iv, P, SM, ET, Rg)
    ;** 5 : IN = Beven_Exp_K_Infil_Rate_v1(iv, r)    ;(no GUI yet)
else : IN = 0.0
endcase


;---------------------------------
;Method can't change during a run
;so don't need to update any vars
;---------------------------------
if (iv.method eq 0) then RETURN, IN

;----------------------------------
;This is done in the infiltration
;functions but could be done here.

;----------------------------------
;Is r less than Ks anywhere ?
;If so, set IN = r there.
;----------------------------------
;** CHECK_LOW_RAIN = (iv.method ne 4)
;** if (CHECK_LOW_RAIN) then begin
;**     Check_Low_Rainrate, iv, IN, r
;** endif

;---------------------------------
;Convert IN from scalar to grid ?
;---------------------------------
;Assume z and h are always grids.
;z, h and IN must be compatible.
;---------------------------------
nh = n_elements(h)
nI = n_elements(IN)

if (nh gt 1) AND (nI eq 1) then begin
    ;s  = size(h, /dimensions)
    ;nx = s[0]
    ;ny = s[1]

    ;--------------------------------
    ;NB! nx and ny are passed in now
    ;and needed by Update_Infil_Vars
    ;--------------------------------
    IN = (IN + fltarr(nx,ny))
    nI = n_elements(IN)
endif


;--------------------------------------
;Can't infiltrate where water table
;is above ground, i.e. where (h ge z)
;If z & h are given, IN is a grid too.
;--------------------------------------
;NOTE:  h and z aren't defined
;now unless (GW_METHOD eq 1).
;--------------------------------
nz = n_elements(z)
if (nh ne 0) AND (nz ne 0) then begin
    w = where(h eq z, nw)              ;(SHOULD THIS BE (h GE z) ?)
    if (nw ne 0) then begin
        IN[w] = 0.0
    endif
endif

;--------------------------------------
;Set groundwater recharge rate to IN ?
;Save last value of r for next time.
;--------------------------------------
if (iv.method ne 4) then begin
    Rg = IN
    r_last = r
endif

;-------------------------
;Print min and max values
;-------------------------
;nI = n_elements(IN)
;if (nI eq 1) then begin
;    print,'IN = ', IN
;endif else begin
;    imin = min(IN, max=imax)
;    print,'(imin, imax) = ', imin, imax
;endelse

;------------------------
;For debugging & testing
;------------------------
;print,'max(IN) = ', max(IN)
;print,'min(IN), max(IN) = ', min(IN), max(IN)
;print,'iv.dt = ', iv.dt

;-------------------------------------
;Update the total infiltrated depth
;Do this for all methods ?
;This becomes a grid if IN is a grid.
;-------------------------------------
*iv.I = (*iv.I + (IN * iv.dt))    ;[m]
;;print,'*iv.I = ', *iv.I

;------------
;For testing
;------------
;print,'IN (min, max) = ', min(IN), max(IN)
;print,'Infiltration timestep = ', iv.dt
;print,'n_elements(IN) = ', n_elements(IN)
;print,'n_elements(z)  = ', n_elements(z)
;print,'n_elements(h)  = ', n_elements(h)

;------------
;For testing
;------------
;if (n_elements(*iv.I) eq 1) then begin
;    TF_Print,'    Tot. infil. depth = ' + TF_String(*iv.I)
;endif

;----------------------------------------
;Reset the total infiltrated depth after
;each "event";  not sure how to do this.
;----------------------------------------
;if (????) then *iv.I = (*iv.I < 0)

;----------------------------------------
;Read next INFIL vars from input files ?
;----------------------------------------
Update_Infil_Vars, iv, nx, ny

RETURN, IN
END;    Infiltration
;************************************************************************
function Seepage, gv, z, h, h_last, $
                  y, Rg, dw, ds, da, pIDs, $
                  p1,p2,p3,p4,p5,p6,p7,p8, $
                  w1,w2,w3,w4,w5,w6,w7,w8


;--------------------------------------------------
;Notes: gv          = GW_vars = structure
;       gv.h_table  = elevation of water table [m]
;       gv.h0_table = init. elev. of water table
;       gv.dt       = GW timestep [sec]
;--------------------------------------------------
;       h  = elevation of water table [m]
;       z  = elevation of bed [m]
;       y  = wetted thicknesses [m] of all soil
;            layers
;       Rg = rate at which water from the surface
;            arrives at the water table [m/s]
;            Now set to infiltration rate.           ;*********
;       Sh = slope of the water table [m/m]
;       dw = flow width grid [m]
;       ds = flow length grid [m]
;       da = pixel area [m^2]  (grid or scalar)

;       GW = seepage rate (to/from surface) [m/s]
;--------------------------------------------------
;NB!    Gv.h_table must be periodically updated
;       in order for this to work.
;--------------------------------------------------
;       METHOD 0:  (Maybe the only method)

;       Where (h lt z) there is no contribution,
;       but where (h gt z), an amount (h - h_last)
;       must be contributed over a time dt.
;--------------------------------------------------
;NB!  This function is a bit different than the
;others in that GW is computed in the same way
;each time, but Q_gw may be computed by different
;methods.  Also, it makes sense to update the
;water table height here since it is also needed
;to compute the seepage, GW.
;--------------------------------------------------

;---------------------------------
;Compute water table slope from h
;This is independent of method.
;**** Pass Sh vs. pIDs and ds.
;---------------------------------
;if (gv.method eq 0) then RETURN, 0.0

;Sh = abs(h - h[pIDs]) / ds
;Sh = Free_Surface_Slope(0.0, h, ds, pIDs)

case (gv.method) of
    0 : RETURN, 0.0
    1 : Q_gw = Total_Darcy_Layer_Flow(gv, h, y, dw, ds, pIDs)
    ;2 : Q_gw = Total_Darcy_Layer_Flow_VK(gv, h, y, dw, ds, pIDs)
endcase


;---------------------------------
;Method can't change during a run
;so don't need to update any vars
;---------------------------------
;*** if (gv.method eq 0) then RETURN, 0.0


;---------------------------------
;Update the water table, h, and
;the wetted thicknesses, y
;---------------------------------
;Is this independent of method ?
;If not, don't do it here.
;---------------------------------
Update_Water_Table, h, y, Q_gw, Rg, da, gv, $
                    p1,p2,p3,p4,p5,p6,p7,p8, $
                    w1,w2,w3,w4,w5,w6,w7,w8

;-----------------------------------------
;Update h_table via pointer ?

;It is updated by Update_Water_Table now.
;Move it to here ??
;-----------------------------------------
;*gv.h_table = h

;---------------------------------------------
;Compute the "seep rate" as the rate at which
;the water table has risen over a time step.
;Allow sign to be positive or negative?
;---------------------------------------------
;Is this independent of method ?
;If not, don't do it here.
;---------------------------------
dh_dt = (h - h_last) / gv.dt        ;[m/s]
GW = (h gt z) * dh_dt
;*** GW = (GW > 0.0)  ;(force to be positive)

;------------------------------
;Redefine h_last for next time
;------------------------------
h_last = h

;-------------------------------------
;Read next GW vars from input files ?
;----------------------------------------------
;7/19/05.  We don't need to update any GW
;vars unless we have an input variable that
;varies over time (time series or grid stack).

;In the more typical case, this routine just
;returns without doing anything.  
;----------------------------------------------
Update_GW_Vars, gv


RETURN, GW
END;    Seepage
;************************************************************************
function Number_of_Samples, stop_vars, sample_dt, dt

STOP_METHOD = stop_vars.method

;-----------------------------------------------
;T_Stop_Model_Estimate uses max basin area to
;get a dynamic upper bound for T_stop_model.
;T_stop_model = (t_Pstop + (L_max / v_avg))
;-----------------------------------------------
;durations    = *pv.durations
;Qp_fraction  = stop_vars.Qp_fraction
;basin_areas  = (*grid_vars.basin_areas)[0]
;t_stop_model = T_Stop_Model_Estimate(basin_areas, durations, $
;                                     Qp_fraction)

;----------------------------
;Get number of sampled times
;----------------------------
case (STOP_METHOD) of

    0b : begin
         ;---------------------------------
         ;Run until Q drops to P% of Qpeak
         ;-------------------------------------------------
         t_stop_model = stop_vars.t_stop_model
         n_samps = long(t_stop_model * 60d  / sample_dt) + 1L
     
         ;***********************************
         ;4/19/06.  EXPERIMENTAL
         ;***********************************
         ;** n_samps = (n_samps * 4L)

         ;*** print,'t_stop_model = ', t_stop_model
         ;*** print,'n_samps      = ', n_samps
         ;*** n_samps = 10000L   ;*******

         ;secs_per_year = 31449600L   ;(60 * 60 * 24 * 365)
         ;n_samps = long(secs_per_year / sample_dt) + 1L
         end
    1b : begin
         ;---------------------------------
         ;Run for a specified time (model)
         ;---------------------------------
         t_stop_model = stop_vars.t_stop_model
         n_samps = long(t_stop_model * 60d  / sample_dt) + 1L
         end
    2b : begin
         ;------------------------------------
         ;Run for a specified number of steps
         ;------------------------------------
         n_steps = stop_vars.n_steps
         t_stop_model = n_steps * dt
         n_samps = long(t_stop_model / sample_dt) + 1L
         ;------------------------------------
         ;Could also update t_stop_model when
         ;n_steps is first set by user
         ;------------------------------------
         ;** t_stop_model = stop_vars.t_stop_model
         ;** n_samps = long(t_stop_model * 60d / sample_dt) + 1L
         end
endcase

RETURN, n_samps

end;  Number_of_Samples
;*******************************************************************
function Sample_Step, sample_dt, chan_dt

step = ceil((sample_dt > chan_dt) / chan_dt)
 
RETURN, step

end;  Sample_Step
;*******************************************************************
function RTG, var, nx, ny

n = n_elements(var)

if (n gt 1) then RETURN, float(var)

;-------------------------------
;Var is scalar; convert to grid
;-------------------------------
RETURN, (fltarr(nx,ny) + float(var))

end;  RTG
;*******************************************************************
function Pixel_Var, var, outlet_IDs

n = n_elements(var)

;-------------------------------
;Is variable a grid or scalar ?
;-------------------------------
if (n gt 1) then begin
    RETURN, float(var[outlet_IDs])
endif else begin
    ;-----------------------------------------------------
    ;(3/16/07) Bug fix.  This gets used in case of q0,
    ;which is a scalar when INFIL_ALL_SCALARS is true.
    ;Without this, don't get a value for every outlet_ID.
    ;-----------------------------------------------------
    RETURN, float(var) + fltarr(n_elements(outlet_IDs))
endelse

end;  Pixel_Var
;*******************************************************************
function Stack, ptr, nx, ny

;-------------------
;Get the dimensions
;-------------------

dim = size(*ptr, /dimensions)


if (n_elements(dim) eq 3) then begin
    ;-----------------------
    ;Variable is a 3D array
    ;-----------------------
    RETURN, float(*ptr) 
endif else begin
    ;---------------------------------------
    ;Var is 1D profile; convert to 3D array
    ;---------------------------------------
    nz  = dim[0]
    var = fltarr(nx,ny,nz)
    for k=0,(nz-1) do begin
        var[*,*,k] = float((*ptr)[k])
    endfor
    RETURN, var
endelse


end;  Stack
;*******************************************************************
function Profile_Var, ptr, outlet_IDs

;-------------------
;Get the dimensions
;-------------------
dim = size(*ptr, /dimensions)

if (n_elements(dim) eq 1) then begin
    ;-------------------------------
    ;Variable is a 1D profile, and
    ;is the same for all outlet_IDs
    ;-------------------------------
    RETURN, float(*ptr)
endif else begin
    ;-------------------------------
    ;Variable is a 3D array; return
    ;a profile for each outlet_ID
    ;-------------------------------
    nz    = dim[2]
    n_IDs = n_elements(outlet_IDs)
    var   = fltarr(n_IDs, nz)
    for k=0,(nz-1) do begin
        layer    = (*ptr)[k]
        vals     = layer[outlet_IDs]
        var[*,k] = float(vals)
    endfor
    RETURN, var
endelse

end;  Profile_Var
;*******************************************************************
pro Remove_Bad_Slopes, S_bed, FLOAT=FLOAT

;----------------------------------------------------------
;Notes:  The main purpose of this routine is to find
;        pixels that have nonpositive slopes and replace
;        then with the smallest value that occurs anywhere
;        in the input slope grid.  For example, pixels on
;        the edges of the DEM will have a slope of zero.

;        With the Kinematic Wave option, flow cannot leave
;        a pixel that has a slope of zero and the depth
;        increases in an unrealistic manner to create a
;        spike in the depth grid.

;        It would be better, of course, if there were
;        no zero-slope pixels in the DEM.  We could use
;        an "Imposed gradient DEM" to get slopes or some
;        method of "profile smoothing".

;        It is possible for the flow code to be nonzero
;        at a pixel that has NaN for its slope. For these
;        pixels, we also set the slope to our min value.

;        7/18/05. Broke this out into separate procedure.
;---------------------------------------------------------

;---------------------------------
;Are there any "bad" pixels ?
;If not, return with no messages.
;---------------------------------
wb = where((S_bed le 0.0) OR (finite(S_bed) ne 1), $
            nbad, COMP=wg, NCOMP=ngood)
if (nbad eq 0) OR (ngood eq 0) then RETURN

;-------------------------------------------
;Find smallest positive value in slope grid
;and replace the "bad" values with smin.
;-------------------------------------------
TF_Print,'Replacing nonpositive slopes with smin...'
smin = min(S_bed[wg], MAX=smax)
TF_Print,'    smin = ' + TF_String(smin)
TF_Print,'    smax = ' + TF_String(smax)
;-------------------------------------------
S_bed[wb] = smin

;------------------------------
;Convert data type to double ?
;------------------------------
if (keyword_set(FLOAT)) then begin
    S_bed = float(S_bed)
endif else begin
    S_bed = double(S_bed)
endelse

end;  Remove_Bad_Slopes
;*******************************************************************
pro Update_Flow_Volume, Q, R, da, dt, vol, $
                        p1, p2, p3, p4, p5, p6, p7, p8, $
                        w1, w2, w3, w4, w5, w6, w7, w8

;--------------------------------------------------------
;Notes:  Q   = surface discharge  [m^3/s]
;        R   = excess precip. rate  [m/s]
;        da  = pixel area  [m^2]
;        dt  = channel flow timestep  [s]
;        vol = total volume of water in pixel [m^3]
;        v2  = temp version of vol
;        w1  = IDs of pixels that...
;        p1  = IDs of parent pixels that...
;--------------------------------------------------------

;---------------------------------
;Initialize v2 with outflow term.

;Doesn't involve neighbor pixels.
;---------------------------------
v2 = vol + dt*((R*da) - Q)

;-----------------------------------
;Some flow directions may not occur
;-----------------------------------
p1_OK = (p1[0L] ne -1L)
p2_OK = (p2[0L] ne -1L)
p3_OK = (p3[0L] ne -1L)
p4_OK = (p4[0L] ne -1L)
p5_OK = (p5[0L] ne -1L)
p6_OK = (p6[0L] ne -1L)
p7_OK = (p7[0L] ne -1L)
p8_OK = (p8[0L] ne -1L)


;---------------------------------------
;Add contributions from neighbor pixels
;---------------------------------------
if (p1_OK) then v2[p1] = v2[p1] + (dt * Q[w1])
if (p2_OK) then v2[p2] = v2[p2] + (dt * Q[w2]) 
if (p3_OK) then v2[p3] = v2[p3] + (dt * Q[w3])
if (p4_OK) then v2[p4] = v2[p4] + (dt * Q[w4]) 
if (p5_OK) then v2[p5] = v2[p5] + (dt * Q[w5]) 
if (p6_OK) then v2[p6] = v2[p6] + (dt * Q[w6]) 
if (p7_OK) then v2[p7] = v2[p7] + (dt * Q[w7])
if (p8_OK) then v2[p8] = v2[p8] + (dt * Q[w8]) 

;---------------
;Copy v2 to vol
;---------------
vol = v2

;---------------------
;Update flow depth ??
;Uniform over pixel ?
;---------------------
;d = (vol / da)
;d = (vol / da_chan)  ;(Assume: d_channel >> d_overland)

END;  Update_Flow_Volume
;*******************************************************************
pro Update_Flow_Depth, d, d_last, vol, angles, widths, ds_chan

;-----------------------------------------------------------
;Notes:  7/18/05.  Modified to use the equation for volume
;        of a trapezoidal channel:  vol = Ac * ds, where
;        Ac=d*[w + d*tan(t)], and to solve the resulting
;        quadratic (discarding neg. root) for new depth, d.

;        8/29/05.  Now original ds is used for subsurface
;        flow and there is a ds_chan which can include a
;        sinuosity greater than 1.  This may be especially
;        important for larger pixel sizes.


;        Removed (ds > 1) here which was only meant to
;        avoid a "divide by zero" error at pixels where
;        (ds eq 0).  This isn't necessary since the
;        Flow_Lengths function in utils_TF.pro never
;        returns a value of zero.
;----------------------------------------------------------
d_last = d

;------------------------------------------
;Are bank angles given as scalar or grid ?
;------------------------------------------
na = n_elements(angles)
if (na eq 1) then begin
    ;----------------------------------
    ;Bank angles are given as a scalar
    ;----------------------------------
    if (angles eq 0.0) then begin
        d = vol / (widths * ds_chan)
    endif else begin
        ;----------------------------------------------
        ;See Notes for TF_Tan function in utils_TF.pro
        ;----------------------------------------------
        term1 = 2.0 * TF_Tan(angles)
        arg   = 2.0 * term1 * vol / ds_chan
        arg   = arg + widths^2.0
        d     = (sqrt(arg) - widths) / term1
    endelse
    RETURN
endif

;--------------------------------
;Bank angles are given as a grid
;--------------------------------
wz = where(angles eq 0, nwz, COMP=wzc, NCOMP=nwzc)

if (nwz ne 0) then begin
    d[wz]  = vol[wz] / (widths[wz] * ds_chan[wz])
endif

if (nwzc ne 0) then begin
    ;----------------------------------------------
    ;See Notes for TF_Tan function in utils_TF.pro
    ;----------------------------------------------
    term1  = 2.0 * TF_Tan(angles[wzc])
    arg    = 2.0 * term1 * vol[wzc] / ds_chan[wzc]
    arg    = arg + widths[wzc]^2.0
    d[wzc] = (sqrt(arg) - widths[wzc]) / term1
endif

;----------------------------
;Old method (before 7/18/05)
;----------------------------
;da2    = (widths * ds)
;d_last = d   ;(save d from last time step)
;d      = (vol / da2)

end;  Update_Flow_Depth
;*******************************************************************
pro Update_Overland_Flow, h, qx, qy, S_ox, S_oy, n_over, $
                          R, dt, dx, dy, nx, ny

;----------------------------------------------------------------
;Notes:  This uses a 2D treatment of mass and momentum equations
;        and a diffusive wave approximation for momentum.

;        Do we need to compute y-derivatives differently ??
;        e.g. look at the following case:
;        IDL> a = indgen(5,5)
;        IDL> print, a
;        IDL> print, shift(a,0,-1) - a
;----------------------------------------------------------------

;-------------------------------------------
;Update the unit-width discharge components
;-------------------------------------------
dh_dx = (shift(h,-1,0) - h) / dx
dh_dy = (shift(h,0,-1) - h) / dy
dh_dx[nx-1,*] = 0d
dh_dy[*,ny-1] = 0d
;--------------------------------- 
S_fx  = S_ox - dh_dx
S_fy  = S_oy - dh_dy
p     = (5d / 3d)
qx    = (h^p) * sqrt(abs(S_fx)) / n_over
qy    = (h^p) * sqrt(abs(S_fy)) / n_over
;----------------------------------------
w1 = where(S_fx lt 0, n1)
if (n1 ne 0) then qx[w1] = -1d * qx[w1]
w2 = where(S_fy lt 0, n2)
if (nw ne 0) then qy[w2] = -1d * qy[w2]

;-------------------------------
;Update the overland flow depth
;-------------------------------
dq_dx  = (shift(qx,-1,0) - qx) / dx
dq_dy  = (shift(qy,0,-1) - qy) / dy
dq_dx[nx-1,*] = 0d
dq_dy[*,ny-1] = 0d                            ;***********
h = h + (R - dq_dx - dq_dy) * dt

end;  Update_Overland_Flow
;*******************************************************************
pro Update_Water_Table, h, y, Q, Rg, da, gv, $
                        p1,p2,p3,p4,p5,p6,p7,p8, $
                        w1,w2,w3,w4,w5,w6,w7,w8

;----------------------------------------------------
;Notes:  gv = gw_vars = structure of GW variables
;        h  = elevation of water table [m]
;        h2 = temp version of h
;        Q  = Q_gw = total subsurface flux [m^3/s]
;        Rg = rate at which water from the surface
;             arrives at the water table [m/s]
;        da = pixel area [m^2]
;        dt = GW timestep [sec]
;        w1 = IDs of pixels that flow in direction 1
;        p1 = IDs of parent pixels for "w1 pixels"

;Note:   h and wetted-depths, y, are updated
;-------------------------------------------------------------
;NOTES:  There seems to be an implicit assumption here
;        that Ks is nondecreasing towards the surface.
;        Once an element is saturated there is no internal
;        storage and the amount of water flowing in through
;        its faces must equal the amount that is flowing out.
;        So if the amount flowing in from the sides exceeds
;        the amount flowing out, (which will occur when the
;        flow is convergent) then the excess must flow
;        through the upper or lower faces.  With saturated
;        soil all the way down to an impermeable bedrock
;        boundary, this means that it must flow through the
;        upper face. But all that enters in a given time step
;        can only flow through the upper face if Ks in the
;        element above is high enough to accommodate it.
;-------------------------------------------------------------

dt = gv.dt
nlayers = gv.nlayers

;--------------------
;Used before 7/13/06
;--------------------
;** t = gv.soil_thick      ;(thicknesses)
;** p = gv.soil_P          ;(porosities)

;----------------------------------------
;Compute wetted-depth, y, for each layer
;----------------------------------------
;Now passed as a variable in & out 
;----------------------------------------
;dims = size(z, /DIM)
;ncols = dims[0]
;nrows = dims[1]
;y     = fltarr(ncols, nrows)
;diff  = -(z - h)
;for k=0, (nlayers - 1) do begin
;    diff = (diff + t[k])
;    y[*,*,k] = (diff > 0.0) < t[k]
;endfor

;------------------------------------------
;Compute dzw = total amount of water to be
;added to or removed from the soil column
;during the subsurface flow timestep
;------------------------------------------
;Initialize dzw with outflow term.
;Doesn't involve neighbor pixels.
;----------------------------------
dzw = dt * (Rg - Q/da)

;--------------
;For debugging
;--------------
if (n_elements(dzw) eq 1) then begin
    msg = [' ','ERROR: dzw is not an array. ', ' ']
    result = GUI_Message(msg, /INFO)
    STOP
endif

;print,'dt = ',dt
;print,'Rg = ',Rg
;-----------------------------
;Q_min = min(Q, max=Q_max)
;print,'Q_min  = ',Q_min
;print,'Q_max  = ',Q_max
;-----------------------------
dz_min = min(dzw, max=dz_max)  ;***********************
TF_Print,'   dz_min = ' + TF_String(dz_min)
TF_Print,'   dz_max = ' + TF_String(dz_max)
;*** TF_Print,' '

;-----------------------------------
;Some flow directions may not occur
;-----------------------------------
p1_OK = (p1[0L] ne -1L)
p2_OK = (p2[0L] ne -1L)
p3_OK = (p3[0L] ne -1L)
p4_OK = (p4[0L] ne -1L)
p5_OK = (p5[0L] ne -1L)
p6_OK = (p6[0L] ne -1L)
p7_OK = (p7[0L] ne -1L)
p8_OK = (p8[0L] ne -1L)

;---------------------------------------
;Add contributions from neighbor pixels
;---------------------------------------
if (n_elements(da) eq 1) then begin
    factor = (dt / da)
    if (p1_OK) then dzw[p1] = dzw[p1] + (Q[w1] * factor)
    if (p2_OK) then dzw[p2] = dzw[p2] + (Q[w2] * factor)
    if (p3_OK) then dzw[p3] = dzw[p3] + (Q[w3] * factor)
    if (p4_OK) then dzw[p4] = dzw[p4] + (Q[w4] * factor)
    if (p5_OK) then dzw[p5] = dzw[p5] + (Q[w5] * factor)
    if (p6_OK) then dzw[p6] = dzw[p6] + (Q[w6] * factor)
    if (p7_OK) then dzw[p7] = dzw[p7] + (Q[w7] * factor)
    if (p8_OK) then dzw[p8] = dzw[p8] + (Q[w8] * factor)
endif else begin
    if (p1_OK) then dzw[p1] = dzw[p1] + (dt * Q[w1] / da[p1])
    if (p2_OK) then dzw[p2] = dzw[p2] + (dt * Q[w2] / da[p2])
    if (p3_OK) then dzw[p3] = dzw[p3] + (dt * Q[w3] / da[p3])
    if (p4_OK) then dzw[p4] = dzw[p4] + (dt * Q[w4] / da[p4])
    if (p5_OK) then dzw[p5] = dzw[p5] + (dt * Q[w5] / da[p5])
    if (p6_OK) then dzw[p6] = dzw[p6] + (dt * Q[w6] / da[p6])
    if (p7_OK) then dzw[p7] = dzw[p7] + (dt * Q[w7] / da[p7])
    if (p8_OK) then dzw[p8] = dzw[p8] + (dt * Q[w8] / da[p8])
endelse

;------------------------------------------------
;Find pixels where water table will rise or fall
;Note: R = Rising, F = Falling
;------------------------------------------------
wR = where(dzw gt 0, n_rising, COMP=wF, NCOMP=n_falling)

;----------------------------------------
;For debugging: save initial value of h  ;***********************
;----------------------------------------
start_h = h

;---------------------------------------
;Process pixels where water table rises
;---------------------------------------
TF_Print,'   n_rising = ' + TF_String(n_rising)   ;*****************

if (n_rising gt 0) then begin
    ;-------------------------------
    ;Must work from bottom layer up
    ;-------------------------------
    for k=(nlayers - 1),0,-1 do begin

        ;-----------------------------------
        ;Compute unused capacity of layer k
        ;Used before 7/13/06.
        ;-----------------------------------
        ;** yk = y[*,*,k]
        ;** capacity_k = (t[k] - yk[wR]) * p[k]

        ;-----------------------------------
        ;Compute unused capacity of layer k
        ;-----------------------------------
        yk  = y[*,*,k]
        tk  = *gv.th[k]    ;(thickness of layer)
        pk  = *gv.qs[k]    ;("porosity" of layer)
        SCALAR_THICKNESS = (n_elements(tk) eq 1)
        SCALAR_POROSITY  = (n_elements(pk) eq 1)

        if (SCALAR_THICKNESS) then begin
            if (SCALAR_POROSITY) then begin
                capacity_k = (tk - yk[wR]) * pk
            endif else begin
                capacity_k = (tk - yk[wR]) * pk[wR]
            endelse
        endif else begin
            if (SCALAR_POROSITY) then begin
                capacity_k = (tk[wR] - yk[wR]) * pk
            endif else begin
                capacity_k = (tk[wR] - yk[wR]) * pk[wR]
            endelse
        endelse

        w = where(capacity_k gt 0, n_not_full)
        ;print,'n_not_full = ',n_not_full ;************************
        if (n_not_full ne 0) then begin
            ;-----------------------------------------
            ;Raise water table, update y, consume dzw
            ;Note: Both nwG and nwL may be nonzero.
            ;-----------------------------------------
            wG = where(dzw[wR[w]] gt capacity_k[w], nwG, $
                       COMP=wL, NCOMP=nwL)
            ;print,'nwG = ',nwG  ;***************************
            ;print,'nwL = ',nwL  ;***************************

            if (nwG ne 0) then begin
                IDs = wR[w[wG]]

                ;-------------------
                ;Used after 7/13/06
                ;-------------------
                if (SCALAR_THICKNESS) then begin
                    h[IDs]  = h[IDs] + (tk - yk[IDs])
                    yk[IDs] = tk
                endif else begin
                    h[IDs]  = h[IDs] + (tk[IDs] - yk[IDs])
                    yk[IDs] = tk[IDs]
                endelse

                ;--------------------
                ;Used before 7/13/06
                ;--------------------
                ;*** h[IDs]  = h[IDs] + (t[k] - yk[IDs])
                ;*** yk[IDs] = t[k]

                dzw[IDs] = dzw[IDs] - capacity_k[w[wG]]
            endif

            if (nwL ne 0) then begin   ;(3/8/04: BUG FIX)
                IDs = wR[w[wL]]
                ;------------------------------
                ;Note: p[k]=0 => capacity_k=0,
                ;so okay to divide by p[k]
                ;Used before 7/13/06.
                ;------------------------------
                ;*** dh  = dzw[IDs] / p[k]

                ;-------------------
                ;Used after 7/13/06
                ;-------------------
                if (SCALAR_POROSITY) then begin
                    dh = dzw[IDs] / pk
                endif else begin
                    dh = dzw[IDs] / pk[IDs]
                endelse

                h[IDs]   = h[IDs]  + dh
                yk[IDs]  = yk[IDs] + dh

                dzw[IDs] = 0.0
            endif
            y[*,*,k] = yk   ;(replace a layer in y)
        endif else begin
            ;kstr = TF_String(k)
            ;TF_Print,'   Layer ' + kstr + ' is full.'
        endelse

        ;hmin = min(h, max=hmax)   ;***************
        ;TF_Print,'hmin = ' + TF_String(hmin)
        ;TF_Print,'hmax = ' + TF_String(hmax) 
    endfor

    ;-----------------------------------------------
    ;Where dzw is still gt 0, we must add it to h
    ;since we have exhausted the capacity of the
    ;soil layers.  This will bring h above the DEM
    ;surface, z.  The increase in h will result in 
    ;surface runoff via a positive seepage rate.
    ;----------------------------------------------
    w = where(dzw[wR] gt 0.0, nIDs)
    if (nIDs gt 0) then begin
        IDs = wR[w]
        h[IDs]   = h[IDs] + dzw[IDs]
        dzw[IDs] = 0.0  ;(need this)
    endif
endif

;---------------------------------------
;Process pixels where water table falls
;---------------------------------------
TF_Print,'   n_falling = ' + TF_String(n_falling)  ;*********

if (n_falling gt 0) then begin

    ;------------------------------
    ;Must work from top layer down
    ;------------------------------
    for k=0,(nlayers - 1) do begin
        ;---------------------------------
        ;Compute water content of layer k
        ;---------------------------------
        yk  = y[*,*,k]
        tk  = *gv.th[k]    ;(thickness of layer)
        pk  = *gv.qs[k]    ;("porosity" of layer)
        SCALAR_THICKNESS = (n_elements(tk) eq 1)
        SCALAR_POROSITY  = (n_elements(pk) eq 1)

        ;-------------------
        ;Used after 7/13/06
        ;-------------------
        if (SCALAR_POROSITY) then begin
            content_k = (yk[wF] * pk)
        endif else begin
            content_k = (yk[wF] * pk[wF])
        endelse

        ;--------------------
        ;Used before 7/13/06
        ;--------------------
        ;*** content_k = (yk[wF] * p[k])

        w = where(content_k gt 0, n_not_empty)
        ;print,'n_not_empty = ',n_not_empty ;************************
        if (n_not_empty ne 0) then begin
            ;-----------------------------------------
            ;Lower water table, update y, consume dzw
            ;Now dzw is negative or zero.
            ;Note: Both nwG and nwL may be nonzero.
            ;-----------------------------------------
            wG = where(abs(dzw[wF[w]]) gt content_k[w], nwG, $
                       COMP=wL, NCOMP=nwL)

            ;print,'nwG = ',nwG  ;***************************
            ;print,'nwL = ',nwL  ;***************************

            if (nwG ne 0) then begin
                IDs      = wF[w[wG]]
                h[IDs]   = h[IDs] - yk[IDs]
                yk[IDs]  = 0.0
                dzw[IDs] = dzw[IDs] + content_k[w[wG]]  ;(neg + pos)
            endif
    
            if (nwL ne 0) then begin  ;(3/8/04: BUG FIX)
                IDs = wF[w[wL]]
                ;-----------------------------
                ;Note: p[k]=0 => content_k=0,
                ;so okay to divide by p[k]
                ;Used before 7/13/06.
                ;-----------------------------
                ;*** dh = dzw[IDs] / p[k]

                ;-------------------
                ;Used after 7/13/06
                ;-------------------
                if (SCALAR_POROSITY) then begin
                    dh = dzw[IDs] / pk
                endif else begin
                    dh = dzw[IDs] / pk[IDs]
                endelse

                h[IDs]   = h[IDs]  + dh
                yk[IDs]  = yk[IDs] + dh
                dzw[IDs] = 0.0

                ;dz_min = min(dzw, max=dz_max)  ;*******************
                ;print,'dz_min = ',dz_min
                ;print,'dz_max = ',dz_max
                ;dh_min = min(dh, max=dh_max)   ;*******************
                ;print,'pk[0]   = ',pk[0]
                ;print,'dh_min = ',dh_min
                ;print,'dh_max = ',dh_max 
            endif

            y[*,*,k] = yk   ;(replace a layer in y)
        endif else begin

            ;kstr = TF_String(k)
            ;TF_Print,'   Layer ' + kstr + ' is full.'
        endelse

        ;hmin = min(h, max=hmax)   ;*************************
        ;print,'hmin = ',hmin
        ;print,'hmax = ',hmax 
    endfor

    ;----------------------------------------------
    ;Where dzw is still lt 0, we must subtract it
    ;from h;  all soil layers are now empty.  This
    ;will bring h below the depth of the lowest
    ;soil layer.  Should we assume that porosity
    ;is the same as for the lowest layer or should
    ;bottom of bottom layer be impermeable?
    ;----------------------------------------------
    ;This is where we should use depth to bedrock.
    ;----------------------------------------------
    w = where(dzw[wF] lt 0.0, nIDs)

    if (nIDs gt 0) then begin
        IDs = wF[w]
       
        ;--------------------
        ;Used before 7/13/06
        ;--------------------
        ;*** dh = dzw[IDs] / p[nlayers - 1]

        ;-------------------------
        ;Used after 7/13/06
        ;pk = *gv.qs[nlayers - 1]
        ;-------------------------
        if (SCALAR_POROSITY) then begin
            dh = dzw[IDs] / pk
        endif else begin
            dh = dzw[IDs] / pk[IDs]
        endelse

        h[IDs]   = h[IDs] + dh
        dzw[IDs] = 0.0  ;(need this ?)
    endif
endif


;------------------------------------------
;For debugging: How much has h changed ?     ;****************
;NB!  h may have NaNs if derived from DEM.
;Exclude these from min & max via /NAN.
;------------------------------------------
;dh2 = (h - start_h)
;dh_min = min(dh2, max=dh_max, /NAN)
;print,'(dh_min, dh_max) = ', dh_min, dh_max

;-------------------------------
;Option to print mins and maxes
;-------------------------------
;hmin = min(h, max=hmax)
;print,' '
;print,'hmin = ',hmin
;print,'hmax = ',hmax
;-----------------------
;ymin = min(y, max=ymax)
;print,'ymin = ',ymin
;print,'ymax = ',ymax

;-----------------------------
;Update h_table in gw_vars ?
;-----------------------------
*gv.h_table = h

;--------------
;For debugging
;--------------
;if (n_elements(h) eq 1) then begin
;    msg = [' ', 'ERROR: h is not a 2D array.', ' ']
;    result = GUI_Message(msg, /INFO)
;    STOP
;endif

END;  Update_Water_Table
;***************************************************************
;pro Update_Snow_Depth


;end;  Update_Snow_Depth
;***************************************************************
pro Update_Mass_Totals, P,SM,IN,ET,GW,R,Rg, da,dt, n_pixels, $
                        vol_P, vol_SM, vol_IN, vol_ET, $
                        vol_GW, vol_R, vol_Rg

if (n_elements(P)  eq 1) then N_P=n_pixels else N_P=1
if (n_elements(SM) eq 1) then N_S=n_pixels else N_S=1
if (n_elements(IN) eq 1) then N_I=n_pixels else N_I=1
if (n_elements(ET) eq 1) then N_E=n_pixels else N_E=1
if (n_elements(GW) eq 1) then N_G=n_pixels else N_G=1
if (n_elements(R)  eq 1) then N_R=n_pixels else N_R=1
if (n_elements(Rg) eq 1) then N_Rg=n_pixels else N_Rg=1
;-------------------------------------------------------
mfac = dt * da

if (n_elements(da) eq 1) then begin
    vol_P  = vol_P  + total(P  * mfac, /DOUBLE) * N_P
    vol_SM = vol_SM + total(SM * mfac, /DOUBLE) * N_S
    vol_IN = vol_IN + total(IN * mfac, /DOUBLE) * N_I
    vol_ET = vol_ET + total(ET * mfac, /DOUBLE) * N_E
    vol_GW = vol_GW + total(GW * mfac, /DOUBLE) * N_G
    vol_R  = vol_R  + total(R  * mfac, /DOUBLE) * N_R
    vol_Rg = vol_Rg + total(Rg * mfac, /DOUBLE) * N_Rg 
endif else begin
    ;--------------------------------------
    ;Note:  da is a grid so mfac is a grid
    ;--------------------------------------
    vol_P  = vol_P  + total(P  * mfac, /DOUBLE)
    vol_SM = vol_SM + total(SM * mfac, /DOUBLE)
    vol_IN = vol_IN + total(IN * mfac, /DOUBLE)
    vol_ET = vol_ET + total(ET * mfac, /DOUBLE)
    vol_GW = vol_GW + total(GW * mfac, /DOUBLE)
    vol_R  = vol_R  + total(R  * mfac, /DOUBLE)
    vol_Rg = vol_Rg + total(Rg * mfac, /DOUBLE)
endelse

end;  Update_Mass_Totals
;***************************************************************
pro Update_Volume_In, volume_in, time, duration_sums, pv, $
                      basin_area, basin_IDs, dt, da

;--------------------------------------------------------
;Notes:  This procedure integrates precip. over the main
;        basin using the model vs. sampling timestep.

;        Recall that da is a grid [km^2].
;--------------------------------------------------------
if (pv.method eq 0) then RETURN

if (pv.method eq 1) then begin
    ;----------------------------------------------
    ;In this case, *pv.rates and *pv.durations are
    ;1D vectors (vs. scalar or grid), which does
    ;not conform to the general approach now used
    ;throughout TopoFlow and by PRECIP_METHOD 2.
    ;----------------------------------------------
    wd = where(time lt duration_sums, nwd)
    if (nwd ne 0) then begin
        rate      = (*pv.rates)[wd[0]]
        dvol      = dt * rate * basin_area * 1000000d
        volume_in = volume_in + dvol
    endif
endif else begin
    ;--------------------------------------------------
    ;If *pv.durations is a scalar, then duration_sums
    ;is equal to the same scalar, but this still works
    ;as written (3/20/07)
    ;--------------------------------------------------
    n_rates = n_elements(*pv.rates)
    if (n_rates eq 1) then P_rates = *pv.rates $
                      else P_rates = (*pv.rates)[basin_IDs]
    ;-------------------------------------------------------
    n_durs = n_elements(duration_sums)
    if (time le duration_sums[n_durs-1]) then begin
        if (n_elements(da) eq 1) then begin
            nb   = n_elements(basin_IDs)
            dvol = dt * total(P_rates * da * nb, /double)
        endif else begin
            dvol = dt * total(P_rates * da[basin_IDs], /double)
        endelse
        volume_in = volume_in + dvol
    endif
endelse

end;  Update_Volume_In
;***************************************************************
pro Update_Volume_Out, volume_out, Q, outlet_ID, dt

volume_out = (volume_out + (Q[outlet_ID] * dt))

end;  Update_Volume_Out
;***************************************************************
pro Update_Velocity_Dynamic, u, Q, d, S_free, R, da, dt, $
                             widths, angles, ds_chan, cv, $
                             p1,p2,p3,p4,p5,p6,p7,p8, $
                             w1,w2,w3,w4,w5,w6,w7,w8

;--------------------------------------------------------
;NOTES:  This update to u uses values of u, d, Q, and
;        S_free from the last time step, then u is

;        overwritten with new values.

;        S_free = Free_Surface_Slope(S_bed, d, ds, pIDs)
;        To compute S_free locally, must also pass:
;        pIDs, ds, and S_bed.
;--------------------------------------------------------
;on_error, 2  ;(return to caller)

;-------------------------------------
;Always need to know where (d le 0),
;so do it here before everything else

;-------------------------------------
wp = where(d gt 0, np, COMP=wn, NCOMP=nn)
;---------------------------------------
;This makes f=0 and du=0 where (d le 0)
;---------------------------------------
dinv = d
if (np ne 0) then dinv[wp] = (1d / d[wp])
if (nn ne 0) then dinv[wn] = 0d

;------------------------------
;Compute an effective depth so
;that f doesn't blow up ??
;------------------------------
;** deff = (d > 1e-9)
;** deff = (d > 0.001)   ;(1 mm)
;--------------------------------
;deff = d

;---------------------------
;Compute f for Manning case
;---------------------------
if (cv.MANNING) then begin
    nvals = *cv.nvals
    f     = 9.81d * (nvals * nvals * dinv^(0.33333333d))

    ;-------------------
    ;Previous approach
    ;-------------------
    ;nvals = *cv.nvals
    ;f = 9.81d * (nvals * nvals * deff^(-0.3333333d))

    ;-----------------
    ;Bug fix: 2/13/07
    ;-----------------
    ;** f = 9.81d * (nvals * nvals / deff^(-0.3333333d))

endif

;-------------------------------
;Compute f for Law of Wall case
;-------------------------------
if (cv.LAW_OF_WALL) then begin
    z0vals     = *cv.z0vals
    smoothness = (0.476d / z0vals) * d 
    ;**** smoothness = (0.476d / z0vals) * deff
    smoothness = (smoothness > 1.1d)   ;****************
    f = (0.408d / alog(smoothness))^2d
endif

;---------------
;Before 2/13/07
;--------------------------------------------
;Increment flow velocities: inputs - outputs
;--------------------------------------------
;*** grav = 9.81d * (S_free * d)    ;(USE  deff vs. d ??)
;grav = 9.81d * (S_free * deff)     ;(USE  deff vs. d ??)
;fric = u*(R + (f * u))
;acc  = (grav - fric)           ;(positive or negative)
;u2   = u + (dt * acc / deff)   ;(before next part)

;----------------------------------------
;Start with the interior (nonflux) terms
;----------------------------------------
;When (n eq 0), should have (d gt 0) and
;(u eq 0), so (grav gt 0) and (acc gt 0) 
;but fric = Atrm = Rtrm = 0.
;----------------------------------------
;Multiply all scalars first, then grids
;Note:  u, d and Q are always grids
;Note:  (Pw / wtop) = (A2 / Atop)
;----------------------------------------
grav = 9.81d * (S_free * d)
fric = f * u^2d
;---------------------------------------
wtop = widths + (2 * d * TF_Tan(angles))   ;(top width)
Atop = ds_chan * wtop                      ;(top area)
Rtrm = (u * R) * (da / Atop)               ;(da = pixel area)
;---------------------------------------
Pw   = widths + (2d * d / cos(angles))     ;(wetted perimeter)
A2   = ds_chan * Pw                        ;(wetted surf. area)
Atrm = (u * Q) * ((1d/Atop) - (1d/A2))
;---------------------------------------
acc  = (grav + Atrm - fric - Rtrm)         ;(positive or negative)
u2   = u + (dt * dinv * acc)               ;(before next part)
;---------------------------------------
fac  = (dt / A2) * dinv                    ;(always grid)
uu   = u * (Pw / wtop)                     ;(always grid)

;-----------------------------------
;Some flow directions may not occur
;-----------------------------------
p1_OK = (p1[0L] ne -1L)
p2_OK = (p2[0L] ne -1L)
p3_OK = (p3[0L] ne -1L)
p4_OK = (p4[0L] ne -1L)
p5_OK = (p5[0L] ne -1L)
p6_OK = (p6[0L] ne -1L)
p7_OK = (p7[0L] ne -1L)
p8_OK = (p8[0L] ne -1L)

;-----------------------------------------
;Add momentum fluxes from D8 child pixels
;-----------------------------------------
if (p1_OK) then $
    u2[p1] = u2[p1] + (u[w1] - uu[p1]) * Q[w1] * fac[p1] 
if (p2_OK) then $
    u2[p2] = u2[p2] + (u[w2] - uu[p2]) * Q[w2] * fac[p2]
if (p3_OK) then $
    u2[p3] = u2[p3] + (u[w3] - uu[p3]) * Q[w3] * fac[p3]
if (p4_OK) then $
    u2[p4] = u2[p4] + (u[w4] - uu[p4]) * Q[w4] * fac[p4]
if (p5_OK) then $
    u2[p5] = u2[p5] + (u[w5] - uu[p5]) * Q[w5] * fac[p5]
if (p6_OK) then $
    u2[p6] = u2[p6] + (u[w6] - uu[p6]) * Q[w6] * fac[p6]
if (p7_OK) then $
    u2[p7] = u2[p7] + (u[w7] - uu[p7]) * Q[w7] * fac[p7]
if (p8_OK) then $
    u2[p8] = u2[p8] + (u[w8] - uu[p8]) * Q[w8] * fac[p8]

;------------------------------
;Don't allow u2 to be negative
;---------------------------------------------
;If uphill flow is allowed, to which pixel ?
;This worked when d0 grid was used.
;---------------------------------------------
u2 = (u2 > 0d)

;-------------
;Copy u2 to u
;-------------
u = u2

END;  Update_Velocity_Dynamic
;*******************************************************************
pro Initialize_Precip_Vars, pv, duration_sums

;--------------------------------
;Return if no method is selected
;--------------------------------
if (pv.method eq 0b) then RETURN

;----------------------------------------
;Compute partial sums of durations array
;----------------------------------------
;Note that there is no leading zero and
;there should not be.
;----------------------------------------
duration_sums = total(*pv.durations, /cumulative, /double)

;----------------------------------
;Are all input variables scalars ?
;----------------------------------
types = [pv.rate_type, pv.duration_type]
ALL_SCALARS = (max(types) lt 2b)
pv.all_scalars = ALL_SCALARS

end;  Initialize_Precip_Vars
;*******************************************************************
pro Initialize_Channel_Vars, cv, nx, ny, ds, $
               MANNING, LAW_OF_WALL, $
               KINEMATIC_WAVE, DIFFUSIVE_WAVE, DYNAMIC_WAVE, $
               S_bed, widths, angles, d0, ds_chan, u, f, d, vol

;--------------------------------------------------------------
;Notes:  If the GUI is used, then the channel timestep will
;get set either by File > Load Vars or by Read_Run_Info_Panel.
;If the GUI is not used, then it will be read from the input
;file and set.  So it will be nonzero (default) before here.
;--------------------------------------------------------------

;--------------------------------
;Return if no method is selected
;--------------------------------
if (cv.method eq 0b) then begin
    cv.SAVE_Q_PIXELS = 0b          ;(It is ON by default.)
    RETURN
endif

;-------------------------------------
;Set vars for channel flow method
;7/15/05.  Replaces previous approach
;-------------------------------------
MANNING        = cv.manning
LAW_OF_WALL    = cv.law_of_wall
KINEMATIC_WAVE = cv.kinematic_wave
DIFFUSIVE_WAVE = cv.diffusive_wave
DYNAMIC_WAVE   = cv.dynamic_wave
S_bed          = *cv.slopes          ;[none]
widths         = *cv.widths          ;[meters]
angles         = *cv.angles * !DTOR  ;[radians]
d0             = *cv.d0

;----------------------------------------------
;8/29/05.  Multiply ds by (unitless) sinuosity
;Orig. ds is used by subsurface flow
;----------------------------------------------
;NB!  We should also divide slopes in S_bed by
;the sinuosity, as now done here. 

;----------------------------------------------
ds_chan = (*cv.sinu * ds)
S_bed   = (S_bed / *cv.sinu)     ;*************

;-------------------------
;Initialize spatial grids
;---------------------------------------------
;NB!  It is not a good idea to initialize the
;water depth grid to a nonzero scalar value.
;---------------------------------------------
TF_Print,'Initializing output grids...'
u = dblarr(nx,ny)             ;(Init to zero)
f = dblarr(nx,ny)
d = dblarr(nx,ny) + d0

;-----------------------------------------
;Make sure all slopes are valid & nonzero
;since otherwise flow will accumulate
;-----------------------------------------
if (KINEMATIC_WAVE) then begin
    Remove_Bad_Slopes, S_bed      ;(3/8/07. Only Kin Wave case)
endif

;--------------------------------------
;Initial volume of water in each pixel
;----------------------------------------------
;See Notes for TF_Tan function in utils_TF.pro
;----------------------------------------------
vol = d * (widths + (d * TF_Tan(angles))) * ds_chan   ;[m^3]

;*** vol = (d * da)   ;[m^3]

;------------------------------------------------
;Make sure cv.Q_out_file is not NULL (12/22/05)

;This is only output file that is set by default
;and is still NULL if user hasn't opened the
;output var dialog for the channel process.
;------------------------------------------------
if (cv.SAVE_Q_PIXELS AND (cv.Q_out_file eq '')) then $
    cv.Q_out_file = (run_prefix + '_0D-Q.txt')

end;  Initialize_Channel_Vars
;*******************************************************************
pro Initialize_Snow_Vars, sv, mv, pv, ev, nx, ny

;--------------------------------------------------------
;Notes:  Need to make sure than h_swe matches h_snow ?
;        User may have entered incompatible values.
;--------------------------------------------------------
;(3/14/07) If the Energy Balance method is used for ET,
;then we must initialize and track snow depth even if
;there is no snowmelt method because the snow depth
;affects the ET rate.  Otherwise, return to caller.
;--------------------------------------------------------
if (sv.method eq 0b) AND (ev.method ne 2) then RETURN

;---------------------------------------
;Set h_snow and h_swe to initial values
;---------------------------------------
*sv.h_snow = *sv.h0_snow
*sv.h_swe  = *sv.h0_swe

;----------------------------------------
;If T_air or precip are grids, then make
;sure that h_snow and h_swe are grids
;----------------------------------------
nT = n_elements(*mv.T_air)
nP = n_elements(*pv.rates)
;------------------------------------
if (nT gt 1) OR (nP gt 1) then begin
    n1 = n_elements(*sv.h0_snow)
    if (n1 eq 1) then *sv.h_snow = *sv.h_snow + fltarr(nx,ny)
    ;---------------------------------------------------------
    n2 = n_elements(*sv.h0_swe)
    if (n2 eq 1) then *sv.h_swe = *sv.h_swe  + fltarr(nx,ny)
endif

;--------------------------------------------------
;Initialize the cold content of snowpack (2/21/07)
;--------------------------------------------------
;Should we rename T_surf to T_snow for clarity ??
;--------------------------------------------------
*sv.Ecc = Initial_Cold_Content(sv.h0_snow, mv.T_surf, $
                               sv.rho_snow, sv.Cp_snow)

;----------------------------------
;Are all input variables scalars ?
;----------------------------------
case (sv.method) of
    1 : types = [sv.c0_type, sv.T0_type, mv.T_air_type]
    2 : types = [mv.Qn_SW_type, mv.Qn_LW_type, mv.T_air_type, $
                 mv.T_surf_type, mv.RH_type, mv.p0_type, $
                 mv.uz_type, mv.z_type, mv.z0_air_type]
                 ;-----------------------------
                 ;These are always scalars now
                 ;-----------------------------
                 ;*** sv.Cp_snow_type]
                 ;*** mv.rho_H2O_type, mv.rho_air_type, mv.Cp_air_type]
    else : types = [pv.rate_type]  ;******
endcase
types = [types, sv.rho_snow_type, sv.h0_snow_type, sv.h0_swe_type]
types = [types, pv.rate_type, pv.duration_type]
ALL_SCALARS = (max(types) lt 2b)
sv.all_scalars = ALL_SCALARS

end;  Initialize_Snow_Vars
;*******************************************************************
pro Initialize_ET_Vars, ev, mv, sv, iv, gv, d, y, h, h_last

;--------------------------------
;Return if no method is selected
;--------------------------------
if (ev.method eq 0b) then RETURN

;--------------------------------------------------------------
;7/5/06.  If there is no GW method and if the infiltration
;method is not Richards equation, then we must initialize
;y with zeros here.  ET can still take water from d, P and SM.
;--------------------------------------------------------------
if (gv.method eq 0b) AND (iv.method ne 4b) then begin
    dims = size(d, /dim)
    nx   = dims[0]
    ny   = dims[1]
    y    = fltarr(nx, ny, gv.nlayers)

    ;------------------------------
    ;We must initialize these also
    ;------------------------------
    h = *gv.h_table
    h_last = h
endif

;----------------------------------
;Are all input variables scalars ?
;----------------------------------
case (ev.method) of
    1 : types = [mv.Qn_SW_type, mv.Qn_LW_type, mv.T_air_type, $
                 mv.T_surf_type, ev.T_soil_x_type, ev.soil_x_type, $
                 ev.Ks_type, ev.alpha_type]
    2 : types = [mv.Qn_SW_type, mv.Qn_LW_type, mv.T_air_type, $
                 mv.T_surf_type, ev.T_soil_x_type, ev.soil_x_type, $
                 ev.Ks_type, mv.uz_type, mv.z_type, mv.z0_air_type, $ 
                 sv.h0_snow_type]  ;(for roughness calc) 
                 ;-----------------------------
                 ;These are always scalars now
                 ;-----------------------------
                 ;*** mv.rho_H2O_type, mv.rho_air_type, mv.Cp_air_type]
    else : types = [mv.T_air_type]  ;******
endcase
if (ev.method eq 2) AND (n_elements(*sv.h_snow) gt 1) then begin
    ;-------------------------------------------------------
    ;Note: h_snow (and h0_snow) are used for rel. roughness
    ;-------------------------------------------------------
    types = [types, 2b]
endif
;** types = [types, pv.rate_type, pv.duration_type]
ALL_SCALARS = (max(types) lt 2b)
ev.all_scalars = ALL_SCALARS

end;  Initialize_ET_Vars
;*******************************************************************
pro Build_Layered_Var, iv, nx, ny, var, v_by_layer

;---------------------------------------------------------
;Notes:  This routine examines user-selected parameters
;        for each soil layer.  If all layers have a
;        scalar value, then a 1D array (a z-profile) is
;        constructed for this variable.  Due to IDL's
;        dynamic data typing, it will get used correctly
;        by the "Update_Richards" routines.  If any layer
;        has a 2D value, then a 3D array is constructed
;        for this variable.

;        Note that iv.nz was previously set to the sum:
;           long(total(iv.nz_val))
;---------------------------------------------------------

;---------------------------
;Create an array of indices
;---------------------------
;i[0] = 0
;i[1] = iv.nz_val[0]
;i[2] = iv.nz_val[0] + iv.nz_val[1]
;etc.
;----------------------------------------------
i = [0L, long(total(iv.nz_val, /cumulative))]

;----------------------------------------------
;Do all layers have a scalar parameter value ?
;----------------------------------------------
nmax = 1
for j=0,(iv.n_layers-1) do begin
    nj = n_elements(*(v_by_layer[j]))
    nmax = (nmax > nj)
endfor
ALL_SCALARS = (nmax eq 1)

;-----------------------------------------
;Build a "data cube" from layer variables
;-----------------------------------------
if (ALL_SCALARS) then begin
    ;--------------------------------------------
    ;All layers have a scalar value for this var
    ;--------------------------------------------
    *var = dblarr(iv.nz)
    for j=0,(iv.n_layers-1) do begin
        (*var)[ i[j]: i[j+1]-1 ] = *(v_by_layer[j])
    endfor
endif else begin
    ;-------------------------------------------------------
    ;Note that all nz "levels" in a given layer can be
    ;initialized to a grid, but not yet to different grids
    ;-------------------------------------------------------
    *var = dblarr(nx, ny, iv.nz)
    for j=0,(iv.n_layers-1) do begin
        for k=i[j], (i[j+1]-1) do (*var)[*,*,k] = *(v_by_layer[j])

        ;------------------------------------------------------
        ;Next line doesn't work if *(v_by_layer[j]) is a grid
        ;------------------------------------------------------
        ;;  (*var)[*, *, i[j]:i[j+1]-1 ] = *(v_by_layer[j])
    endfor
endelse


end;  Build_Layered_Var
;*******************************************************************
pro Initialize_Infil_Vars, iv, pv, sv, ev, nx, ny, r_last

;------------------------------------------------------------
;Notes:  This routine initializes the infiltration variables
;        that will be computed by Route_Flow.
;        Total infiltrated depth is reset to 1e-6 at
;        the beginning of each model run.

;        3/19/07.  Now iv.all_scalars is computed for use
;        by routines that need to know whether infiltration
;        can vary spatially (not just with depth).
;------------------------------------------------------------
if (iv.method eq 0b) then RETURN

;----------------------------------
;Are all input variables scalars ?
;-----------------------------------------------------------
;For Richards, (iv.method eq 4) and if any input var is 2D
;then q, p, K and v must be 3D.  As of 3/11/08, Ks_type,
;etc. is an array but this still works.
;-----------------------------------------------------------
case (iv.method) of
     1b : itypes = [0b]    ;(scalar?  100% until h equals z ?)
     2b : itypes = [iv.Ks_type, iv.Ki_type, iv.qs_type, iv.qi_type, $
                    iv.G_type ]
     3b : itypes = [iv.Ks_type, iv.Ki_type, iv.qs_type, iv.qi_type, $
                    iv.G_type, iv.gam_type ]
     4b : itypes = [iv.Ks_type, iv.Ki_type, iv.qs_type, iv.qr_type, $
                    iv.qi_type, iv.pB_type, iv.pA_type, $
                    iv.lam_type, iv.c_type ]
endcase
;------------------------------------------------
;NB! Infil also depends on Precip, snowmelt & ET
;------------------------------------------------
if NOT(pv.all_scalars) then itypes = [itypes, 2b]    ;(precip)
if NOT(sv.all_scalars) then itypes = [itypes, 2b]    ;(snowmelt)
if NOT(ev.all_scalars) then itypes = [itypes, 2b]    ;(ET)
ALL_SCALARS = (max(itypes) lt 2b)
iv.all_scalars = ALL_SCALARS

;--------------------------
;For debugging and testing
;--------------------------
;if NOT(ALL_SCALARS) then begin
;    print,'*********************************'
;    print,'  INFILTRATION VARIES SPATIALLY'
;    print,'*********************************'
;    print,' '
;endif else begin
;    print,'****************************************'
;    print,'  INFILTRATION DOES NOT VARY SPATIALLY'
;    print,'****************************************'
;endelse

;print, 'ALL_SCALARS = ', ALL_SCALARS

RICHARDS = (iv.method eq 4b)
if NOT(RICHARDS) then begin
    ;-----------------------------
    ;Green-Ampt or Smith-Parlange 
    ;---------------------------------------------------
    ;NB! These other methods use Ks, Ki, etc. directly
    ;and don't allow possibility of multiple layers
    ;---------------------------------------------------
    ;NB! If we do a non-Richards run after a Richards
    ;run as of 5/15/06, then vars like Ks will still
    ;be z-profiles instead of scalars and will need to
    ;be reset.
    ;---------------------------------------------------
    ;;iv.Ks  = iv.Ks1  ;(This isn't a good solution.)
    ;;iv.Ki  = iv.Ki1
    ;;iv.qs  = iv.qs1
    ;;iv.qi  = iv.qi1

    TF_Print,'Total infiltrated depth reset to 1e-6 meters.'

    if (ALL_SCALARS) then begin
        *iv.I  = 1e-6
        *iv.tp = -1d
        *iv.fp = 0d
        r_last = 0d      ;(P+SM at previous step)
    endif else begin
        *iv.I  = dblarr(nx, ny)
        *iv.tp = dblarr(nx, ny) - 1d
        *iv.fp = dblarr(nx, ny)
        r_last = dblarr(nx, ny)
    endelse
    RETURN
   ;***********
endif

;----------------------------------------
;Define input vars for Richards equation
;----------------------------------------
;Open windows, for plotting profiles
;------------------------------------
XS=400  &  YS=250
window, 0, xsize=XS, ysize=YS, title='Soil moisture vs. depth'
window, 1, xsize=XS, ysize=YS, title='Pressure head vs. depth', ypos=280

;--------------------------------------------
;Compute the z-vector, for plotting profiles
;--------------------------------------------
nz = long(total(iv.nz_val))
dz = replicate(iv.dz_val[0], iv.nz_val[0])
for j=1L, (iv.n_layers-1) do begin
    layer_dz = iv.dz_val[j]
    layer_nz = iv.nz_val[j]
    dz = [dz, replicate(layer_dz, layer_nz) ]
endfor
*iv.z = total(dz, /cumulative, /double)

;--------------------------------------------
;Compute the z-vector, for plotting profiles
;--------------------------------------------
;Used before 3/11/08, seems to be wrong.
;--------------------------------------------
;z1 = dindgen(iv.nz1) * iv.dz1
;z2 = (iv.nz1 + dindgen(iv.nz2)) * iv.dz2
;z3 = (iv.nz1 + iv.nz2 + dindgen(iv.nz3)) * iv.dz3
;*iv.z = [z1, z2, z3]
;----------------------------------------
;** zmin = min(*iv.z, max=zmax)
;** print,'(zmin, zmax) = ', zmin, zmax


;---------------------------------------
;Compute min soil moisture from min psi
;---------------------------------------------------
;Don't need to set qH in Get_Infil_Vars, it will
;just be overwritten here.
;---------------------------------------------------
for j=0L, (iv.n_layers-1) do begin
    *iv.qH_val[j] = Theta_Min(*iv.qs_val[j], *iv.qr_val[j], *iv.pB_val[j], $
                              *iv.pA_val[j], *iv.c_val[j],  *iv.lam_val[j] )
    ;----------------------------------------------------
    ;(3/17/08)  Turns out that this is what the previous
    ;version (which worked) was actually doing.
    ;----------------------------------------------------
    ;*iv.qH_val[j] = *iv.qi_val[j]
    ;--------------------------------
    ;(3/17/08)   Try this instead ?
    ;--------------------------------
    ;*iv.qH_val[j] = *iv.qr_val[j]
endfor

;------------------------------------------
;Build a 1D or 3D array for each input var
;------------------------------------------------------
;(3/12/08) Same code should work if (iv.n_layers eq 1)
;------------------------------------------------------
;We did this above:   iv.nz = long(total(iv.nz_val))
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
Build_Layered_Var, iv,nx,ny, iv.qH,  iv.qH_val    ;(see notes above)

;-------------------
;For debugging only
;----------------------------------------------------
;All of these values except qH agree with values in
;previous version where Richards' method worked.
;----------------------------------------------------
;The qH values here seem to be computed correctly
;----------------------------------------------------
;Even if qH is made to agree, Richards' method
;still doesn't work in the current version.  Hmmm.
;----------------------------------------------------
DEBUG = 0b
if (DEBUG) then begin
Ks_min = min(*iv.Ks, max=Ks_max)
print,'(Ks_min, Ks_max) = ', Ks_min, Ks_max
;-----------------------------------------------
Ki_min = min(*iv.Ki, max=Ki_max)
print,'(Ki_min, Ki_max) = ', Ki_min, Ki_max
;-----------------------------------------------
qs_min = min(*iv.qs, max=qs_max)
print,'(qs_min, qs_max) = ', qs_min, qs_max
;-----------------------------------------------
qi_min = min(*iv.qi, max=qi_max)
print,'(qi_min, qi_max) = ', qi_min, qi_max
;-----------------------------------------------
qr_min = min(*iv.qr, max=qr_max)
print,'(qr_min, qr_max) = ', qr_min, qr_max
;-----------------------------------------------
qH_min = min(*iv.qH, max=qH_max)
print,'(qH_min, qH_max) = ', qH_min, qH_max
;-----------------------------------------------
pB_min = min(*iv.pB, max=pB_max)
print,'(pB_min, pB_max) = ', pB_min, pB_max
;-----------------------------------------------
pA_min = min(*iv.pA, max=pA_max)
print,'(pA_min, pA_max) = ', pA_min, pA_max
;-----------------------------------------------
lam_min = min(*iv.lam, max=lam_max)
print,'(lam_min, lam_max) = ', lam_min, lam_max
;-----------------------------------------------
c_min = min(*iv.c, max=c_max)
print,'(c_min, c_max) = ', c_min, c_max
;-----------------------------------------------
eta_min = min(*iv.eta, max=eta_max)
print,'(eta_min, eta_max) = ', eta_min, eta_max
;-----------------------------------------------
STOP
endif

;--------------------------------------------------
;Compute dz as 1D array from scalars in iv.dz_val
;--------------------------------------------------
;NB! Values in iv.dz_val are scalars vs. pointers
;so we can't use the Build_Layered_Var routine.
;------------------------------------------------
dz_min = min(iv.dz_val, max=dz_max)

if (dz_min eq dz_max) then begin
    ;--------------------
    ;dz is just a scalar
    ;--------------------
    *iv.dz = iv.dz_val[0]
endif else begin
    ;-----------------
    ;dz is a 1D array
    ;-----------------
    *iv.dz = dblarr(iv.nz)

    ;-----------------------------------------------
    ;Create array of indices. See Build_Layered_Var
    ;-----------------------------------------------
    i = [0L, long(total(iv.nz_val, /cumulative))]
    for j=0,(iv.n_layers-1) do begin
        (*iv.dz)[ i[j]: i[j+1]-1 ] = iv.dz_val[j]
    endfor
endelse

;------------------------------
;Initialize computed variables
;------------------------------
if (ALL_SCALARS) then begin
    ;--------------------------------
    ;Infiltration varies with z only
    ;--------------------------------
    *iv.q = dblarr(iv.nz) + *iv.qi
    *iv.p = dblarr(iv.nz)
    *iv.K = dblarr(iv.nz) + *iv.Ki
    *iv.v = dblarr(iv.nz)
    ;------------------------------
    *iv.Zw = 1e-6                     ;(wetting front depth)
    *iv.I  = 1e-6                     ;(total infil. depth)

    ;---------------------------
    ;NB!  This may be redundant
    ;---------------------------
    (*iv.v)[0] = (*pv.rates)[0]       ;(boundary condition)

endif else begin
    ;----------------------------------
    ;Infiltration varies with x, y & z
    ;----------------------------------
    *iv.q = dblarr(nx, ny, iv.nz)
    *iv.p = dblarr(nx, ny, iv.nz)
    *iv.K = dblarr(nx, ny, iv.nz)
    *iv.v = dblarr(nx, ny, iv.nz)

    ;****************************************************************
    ;  (3/13/08)   This section should be checked over carefully.
    ;****************************************************************

    ;------------------------------------
    ;Initialize q to qi (qi is 1D or 3D)
    ;------------------------------------
    if (n_elements(*iv.qi) eq iv.nz) then begin
        for j=0,(iv.nz-1) do (*iv.q)[*,*,j]=(*iv.qi)[j]  ;*****
        ;----------------------------------------
        ;How can we do it with array operators ?
        ;----------------------------------------
        ;**  ones = dblarr(nx,ny,nz) + 1d  ;(doesn't work)
        ;**  *iv.q = ones # (*iv.qi)
    endif else begin
        *iv.q = *iv.q + *iv.qi   ;(OK; see how *iv.q is initialized)
    endelse

    ;------------------------------------
    ;Initialize K to Ki (Ki is 1D or 3D)
    ;------------------------------------
    if (n_elements(*iv.qi) eq iv.nz) then begin
        for j=0,(iv.nz-1) do (*iv.K)[*,*,j]=(*iv.Ki)[j]  ;*****
        ;----------------------------------------
        ;How can we do it with array operators ?
        ;----------------------------------------
    endif else begin
        *iv.K = *iv.K + *iv.Ki   ;(OK; see how *iv.K is initialized)
    endelse

    *iv.Zw = dblarr(nx, ny) + 1e-6     ;(wetting front depth)
    *iv.I  = dblarr(nx, ny) + 1e-6     ;(total infil. depth)
    ;--------------------------
    ;NB! This may be redundant
    ;--------------------------
    (*iv.v)[*,*,0] = (*pv.rates)[0]    ;(boundary condition)

endelse


;print,'size(*iv.q) = ', size(*iv.q)
;print,'size(*iv.p) = ', size(*iv.p)
;print,'size(*iv.K) = ', size(*iv.K)
;print,'size(*iv.v) = ', size(*iv.v)
;print,' '

;** TF_Print,'INFIL_METHOD = ' + TF_String(fix(iv.method))

end;  Initialize_Infil_Vars
;*******************************************************************
pro Initialize_GW_Vars, gv, d, z, y, h, h_last 

;--------------------------------------------------------------
;Notes:  If the GUI is used, then the GW timestep will
;get set either by File > Load Vars or by Read_Run_Info_Panel.
;If the GUI is not used, then it will be read from the input
;file and set.  So it will be nonzero (default) before here.
;--------------------------------------------------------------

;--------------------------------
;Return if no method is selected
;--------------------------------
if (gv.method eq 0b) then RETURN

;---------------------------------------
;OLD WAY. Read elevations from DEM_file
;---------------------------------------
;** DEM_file = gv.elev_file
;** TF_Print,'Reading elevation grid...'
;** Read_Grid, z, DEM_file, /report   ;(gets type from RTI file)

;----------------------------------------------
;4/27/06.  DEM is now read via prior call to
;Read_GW_Vars_in_Files and stored as *gv.elev.
;Data type of DEM need not be 'FLOAT'.
;----------------------------------------------
;Note that Infiltration function returns Rg,
;the rate at which water reaches water table.
;----------------------------------------------
z = *gv.elev

;--------------------------------------------
;Convert h from scalar to grid, if necessary
;--------------------------------------------
dims = size(d, /dimensions)
nx = dims[0]
ny = dims[1]
nh = n_elements(*gv.h0_table)

if (nh eq 1) then begin
    h = *gv.h0_table + dblarr(nx,ny)
endif else begin
    h = *gv.h0_table
endelse

y = Wetted_Thicknesses(gv, z, h)

h_last = h   ;******

;-------------------------------
;If water table > land surface,
;increment the flow depth grid.
;-------------------------------
nodata = -9999.0  ;************************
diff  = (h - z)
;** w = where(diff gt 0, nw)
w = where((diff gt 0) AND (z gt nodata) AND (finite(z) eq 1), nw)
if (nw ne 0) then d[w] = d[w] + diff[w]
    
;------------------------
;For debugging & testing
;------------------------
dmin = min(d, max=dmax)
dstr = TF_String(dmin) + ', ' + TF_String(dmax) + ')'
TF_Print,'Initial depths due to water table: (dmin, dmax) = (' + dstr

end;  Initialize_GW_Vars
;*******************************************************************
pro Route_Flow, grid_vars, stop_vars, $
    rv, pv, cv, mv, sv, ev, gv, iv, ov, dv, time, $
    ;------------------------------------------------------
    Q_outlet, u_outlet, d_outlet, f_outlet, t_outlet, $
    ;------------------------------------------------------
    DIRECTORY=DIRECTORY, NAME=NAME, COMMENT=COMMENT, $
    LOG_FILE=log_file, VERBOSE=VERBOSE, STOP_ID=STOP_ID, $
    DRAW_ID=DRAW_ID, PLOT=PLOT

;----------------------------------------------------------
;NOTES:  This routine computes downstream velocity, u,
;        and flow depth, d, as functions of time, from
;        the following inputs (among others):

;        (1) a RiverTools flow grid, grid
;        (2) an input sequence of precip. rate arrays, R
;        (3) an array of roughness parameters, z0 or n
;        (4) a real-valued time step, dt, and
;        (5) pixel dimensions, (dx x dy)

;        Variables needed to model different processes are
;        passed around in structures that contain pointers,

;        filenames, variable types, etc.
;            rv = run_vars
;            pv = precip vars
;            cv = channel vars
;            mv = meteorological vars
;            sv = snow vars
;            ev = evapotranspiration vars
;            iv = infiltration vars
;            ov = overland flow vars
;            dv = diversion vars

;        Vertically-integrated mass and momentum equations
;        are used to determine the change in depth and
;        velocity at each pixel, as a function of previous
;        values and the inputs.  Initially, the u and d
;        arrays contain only zeros.  A future version will
;        allow other initial conditions.

;        The sequences of (optional) output arrays, u(t),
;        d(t), and Q(t) are processed as IDL "associated"
;        arrays.  This minimizes RAM usage.

;        For uniform and "step" rain, it saves a lot of
;        filespace to produce the scene internally,
;        rather than save it to a file first with the
;        Make_Rain_File routine.

;        In this version, flow widths are computed to take
;        both diagonals, etc. and latitude-dependence into
;        account via the Flow_Widths function.

;        Units for u and R are meters/second.
;        Units for d is meters.
;        Units for da are square meters.

;        In a typical DEM, many pixels will have a
;        slope of 0, and edge pixels are assigned
;        slopes of 0.0 when the slopefile is created.
;        In this version, all pixels with slopes less
;        than 0 are assigned the min positive slope.
;        Should check what happens in equations if
;        local slope is zero.  Note also that negative
;        slopes can lead to negative velocities.

;IDEAS:  Perhaps dt can be increased after the system has
;        had time to get "ramped up."  But hydrograph will
;        need to be prepared differently.

;        Rainrate seems coupled to dt; a lower rainrate
;        requires a smaller dt to avoid crashing.  This seems
;        to be a sort of Courant condition for stability.
;        Must make sure flow can't travel a whole pixel in
;        one time step ??  **************

;        Can default time step be increased in latest
;        version?
;--------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

start = systime(1)
TF_Print,' '
run_prefix = rv.run_prefix

;---------------------
;Set keyword defaults
;---------------------
if NOT(keyword_set(NAME)) then NAME=run_prefix
if NOT(keyword_set(DIRECTORY)) then begin
   directory = Current_Directory()
   TF_Print,'directory = ' + directory
endif
;----------------------------------------
VERBOSE         = keyword_set(VERBOSE)
PLOT            = keyword_set(PLOT)
WRITE_LOG       = keyword_set(log_file)
;----------------------------------------
STOP_METHOD     = stop_vars.method
PRECIP_METHOD   = pv.method
SNOW_METHOD     = sv.method
ET_METHOD       = ev.method
INFIL_METHOD    = iv.method
GW_METHOD       = gv.method
CHANNEL_METHOD  = cv.method
OVERLAND_METHOD = ov.method

;-------------------------------
;Get name of RTI file, which is
;used by several subroutines
;-------------------------------
RTI_file = grid_vars.RTI_file 
TF_Print, 'RTI_file = ' + RTI_file
TF_Print, ' '

;--------------------
;Get grid dimensions
;--------------------
nx = grid_vars.ncols    ;(All grids must have these dimensions.)
ny = grid_vars.nrows
nz = iv.nz

n_pixels = (long(nx) * long(ny))   ;(used in some places)
;--------------------------------------------
;These are returned by Get_Pixel_Sizes later
;--------------------------------------------
;** dx = grid_vars.xres   ;(scalar)
;** dy = grid_vars.yres   ;(scalar)

;----------------------------------
;Check if output options are valid
;----------------------------------
Check_Output_Options, grid_vars, rv,cv,sv,ev,iv,gv, nx,ny, $
                      outlet_IDs, n_outlets, outlet_ID, OK
if NOT(OK) then RETURN

;---------------------------------------
;Open the RTS output files to write
;(put into a separate routine: 7/21/05)
;---------------------------------------
Open_New_RTS_Files, grid_vars, rv,cv,mv,sv,ev,iv,gv

;----------------------------------------------
;Open the "pixel" output files to write
;(3/8/07) Now written continuously, vs. at end
;----------------------------------------------
Open_New_Pixel_Files, rv,cv,mv,sv,ev,iv,gv, outlet_IDs
pformat = '(' + TF_String(n_outlets + 1) + 'F15.7)'

;----------------------------------
;Set the model run timestep [secs]
;----------------------------------
dt = cv.dt

;------------------------------
;Get the "change steps" [secs]
;------------------------------
snow_change_step = ceil(sv.dt / dt)
ET_change_step   = ceil(ev.dt / dt)
IN_change_step   = ceil(iv.dt / dt)
gw_change_step   = ceil(gv.dt / dt)
over_change_step = ceil(ov.dt / dt)

;-----------------------------------
;Get the "grid sample steps" (>= 1)
;-----------------------------------
chan_grid_samp_step = Sample_Step(cv.save_grid_dt, dt)
snow_grid_samp_step = Sample_Step(sv.save_grid_dt, dt)
et_grid_samp_step   = Sample_Step(ev.save_grid_dt, dt)
in_grid_samp_step   = Sample_Step(iv.save_grid_dt, dt)
gw_grid_samp_step   = Sample_Step(gv.save_grid_dt, dt)

;------------------------------------
;Get the "pixel sample steps" (>= 1)
;------------------------------------
chan_pixel_samp_step = Sample_Step(cv.save_pixels_dt, dt)
snow_pixel_samp_step = Sample_Step(sv.save_pixels_dt, dt)
et_pixel_samp_step   = Sample_Step(ev.save_pixels_dt, dt)
in_pixel_samp_step   = Sample_Step(iv.save_pixels_dt, dt)
gw_pixel_samp_step   = Sample_Step(gv.save_pixels_dt, dt)

;------------------------------------
;Get the "stack sample steps" (>= 1)
;------------------------------------
in_stack_samp_step = Sample_Step(iv.save_stack_dt, dt)

;-------------------------------------
;Get the "profile sample steps" (>= 1)
;-------------------------------------
in_profile_samp_step = Sample_Step(iv.save_profile_dt, dt)

;------------------------------------
;Read source vars from source_file ?
;------------------------------------
SOURCES = ((dv.method eq 1b) AND dv.use_sources)
if (SOURCES) then begin
    Read_Source_Data, dv.source_file, source_IDs, $

                      dur_sum_sources, Q_sources, OK
    if NOT(OK) then SOURCES = 0b
endif

;--------------------------------
;Read sink vars from sink_file ?
;--------------------------------
SINKS = ((dv.method eq 1b) AND dv.use_sinks)
if (SINKS) then begin
    Read_Sink_Data, dv.sink_file, sink_IDs, $
                    dur_sum_sinks, Q_sinks, OK
    if NOT(OK) then SINKS = 0b
endif

;----------------------------------
;Read canal vars from canal_file ?
;----------------------------------
CANALS = ((dv.method eq 1b) AND dv.use_canals)
if (CANALS) then begin
    Read_Canal_Data, dv.canal_file, canal_in_IDs, $
                     canal_out_IDs, dur_sum_canals_in, $
                     Q_canals_in, t_canals, OK
    if NOT(OK) then CANALS = 0b
endif

;------------------------
;Set the stopping method
;------------------------
TF_Print, 'Setting the stop method...'
case (STOP_METHOD) of
    0b : begin
         Qpeak_fraction = stop_vars.Qp_fraction
         TSTR = '  [min]'
         end
    1b : begin
         T_stop = stop_vars.T_stop_model
         TSTR = ' of ' + TF_String(T_stop, FORMAT='(F10.2)') + '  [min]'
         end
    2b : begin
         n_steps = stop_vars.n_steps
         T_stop = (n_steps * cv.dt / 60d)   ;[sec] -> [min]
         TSTR = ' of ' + TF_String(T_stop, FORMAT='(F10.2)') + '  [min]'
         end
  else : RETURN
endcase

;-----------------------
;Initialize peak values
;-----------------------
Q_last   = 0d
Q_peak   = 0d
u_peak   = 0d
d_peak   = 0d
T_peak   = 0d
Tu_peak  = 0d
Td_peak  = 0d

;----------------------------------
;Pixel dimensions; convert km to m
;These are planform dimensions.
;-------------------------------------
;Flow widths are for entire pixel and
;are appropriate for overland flow.
;-------------------------------------
Get_Pixel_Sizes, dx,dy,dd, da_vec, RTI_file, /METERS

;-----------------------------------------
;7/13/06.  Allow da to be scalar or grid.
;For speed;  was always grid before.
;-----------------------------------------
if (grid_vars.pixel_geom eq 1) then begin
    TF_Print,'dx = ' + TF_String(dx[0]) + '  [m]'
    TF_Print,'dy = ' + TF_String(dy[0]) + '  [m]'
    da = da_vec[0]
    TF_Print,'da = ' + TF_String(da) + '  [m^2]'
endif else begin
    ;-------------------------------
    ;Convert da from 1D to 2D array
    ;Then subscript with the wk's.
    ;-------------------------------
    TF_Print,'Computing pixel area grid...'
    da = (replicate(1,nx) # da_vec)
    da_min = min(da, max=da_max)
    TF_Print,'    min(da) = ' + TF_String(da_min) + '  [m^2]'
    TF_Print,'    max(da) = ' + TF_String(da_max) + '  [m^2]'
    dx_min = min(dx, max=dx_max)
    TF_Print,'    min(dx) = ' + TF_String(dx_min) + '  [m]'
    TF_Print,'    max(dx) = ' + TF_String(dx_max) + '  [m]'
    dy_min = min(dx, max=dy_max)
    TF_Print,'    min(dy) = ' + TF_String(dy_min) + '  [m]'
    TF_Print,'    max(dy) = ' + TF_String(dy_max) + '  [m]'
endelse
TF_Print,' '

;******************************************************
;Initialize flow grid and vars that depend on it
;******************************************************
TF_Print,'Reading D8 flow grid...'
Read_Grid, grid, cv.code_file, TYPE='BYTE'
Get_Flux_Indices,  grid, w1,w2,w3,w4,w5,w6,w7,w8, $
                         n1,n2,n3,n4,n5,n6,n7,n8
;------------------------------------------------
;1/19/07.  Need to set d and u to zero at any ID
;where flow terminates.  This includes pixels on
;the edges, those with unresolved flow direction
;and those where elevation is nodata or NaN.
;A RiverTools flow grid will have a flow code of
;zero at all of these places.
;------------------------------------------------
;Rename "grid" to "flow grid" or "D8_codes" ?
;------------------------------------------------
noflow_IDs = where(grid le 0, nf_IDs)
if (nf_IDs eq 0) then begin
    ;-----------------------
    ;Get IDs of edge pixels
    ;-----------------------
    TF_Print,'Finding edge pixel IDs...'
    TF_Print,' '
    T_IDs = lindgen(nx)
    B_IDs = T_IDs + (ny-1L)*nx
    L_IDs = (1L + lindgen(ny-2L))*nx
    R_IDs = L_IDs + (nx-1L)
    edge_IDs  = [T_IDs, B_IDs, L_IDs, R_IDs]
    noflow_IDs = edge_IDs
    ;---------------------------
    ;This double-counts corners
    ;---------------------------
    ;;L_IDs = lindgen(ny)*nx
endif

;----------------------------
;Get IDs of all "downstream"
;or "parent" pixels
;----------------------------
TF_Print,'Finding parent pixel IDs...'
pIDs = Parent_IDs(grid)
TF_Print,'n1 = ' + TF_String(n1)
;--------------------------------------
if (n1 ne 0) then p1=pIDs[w1] else p1=[-1L]   ;northeast
if (n2 ne 0) then p2=pIDs[w2] else p2=[-1L]   ;east
if (n3 ne 0) then p3=pIDs[w3] else p3=[-1L]   ;southeast
if (n4 ne 0) then p4=pIDs[w4] else p4=[-1L]   ;south
if (n5 ne 0) then p5=pIDs[w5] else p5=[-1L]   ;southwest
if (n6 ne 0) then p6=pIDs[w6] else p6=[-1L]   ;west
if (n7 ne 0) then p7=pIDs[w7] else p7=[-1L]   ;northwest
if (n8 ne 0) then p8=pIDs[w8] else p8=[-1L]   ;north

;--------------------
;Compute flow widths
;-------------------------------------------
;This is only used by Seepage function now.
;-------------------------------------------
TF_Print,'Computing flow width grid...'
dw = Flow_Widths(grid, RTI_file, /METERS, /DOUBLE)   ;(2D array)
dw_min = min(dw, max=dw_max)
TF_Print,'    min(dw) = ' + TF_String(dw_min) + '  [m]'
TF_Print,'    max(dw) = ' + TF_String(dw_max) + '  [m]'

;---------------------
;Compute flow lengths
;---------------------
TF_Print,'Computing flow length grid...'
ds = Flow_Lengths(grid, RTI_file, /METERS, /DOUBLE)  ;(2D array)
ds_min = min(ds, max=ds_max)
TF_Print,'    min(ds) = ' + TF_String(ds_min) + '  [m]'
TF_Print,'    max(ds) = ' + TF_String(ds_max) + '  [m]'
;******************************************************

;--------------------------------
;Read input variables from files
;----------------------------------------------
;This must come before the Initialize routines
;----------------------------------------------
Read_Precip_Vars_in_Files, pv, nx, ny        ;(7/13/05)
Read_Channel_Vars_in_Files, cv, nx, ny       ;(7/13/05)
Read_Met_Vars_in_Files, mv, nx, ny           ;(new, 3/13/07)
Read_Snow_Vars_in_Files, sv, nx, ny
Read_ET_Vars_in_Files, ev, sv, nx, ny
Read_Infil_Vars_in_Files, iv, nx, ny
Read_GW_Vars_in_Files, gv, nx, ny

;------------------------------------
;Initialize variables to be computed
;-----------------------------------------------
;Note: If ET is turned on and Richards equation
;      is not used for infiltration, then we
;      need to make sure that y is defined by
;      Initialize_ET_Vars.
;-----------------------------------------------
Initialize_Precip_Vars, pv, duration_sums    ;(3/19/07, separate)
Initialize_Channel_Vars, cv, nx, ny, ds, $
                   MANNING, LAW_OF_WALL, $
                   KINEMATIC_WAVE, DIFFUSIVE_WAVE, DYNAMIC_WAVE, $
                   S_bed, widths, angles, d0, ds_chan, u, f, d, vol
Initialize_Snow_Vars, sv, mv, pv, ev, nx, ny
Initialize_ET_Vars, ev, mv, sv, iv, gv, d, y, h, h_last
Initialize_Infil_Vars, iv, pv, sv, ev, nx, ny, r_last
Initialize_GW_Vars, gv, d, z, y, h, h_last

;--------------------------------------
;Option to plot hydrograph dynamically
;--------------------------------------
PLOT = 1b
if (PLOT) then begin
    ;------------------------------------------------------
    ;NB!  Get the window number for the draw widget.
    ;     This assumes that a small draw window has been
    ;     added in the output log panel.  See the function
    ;     called GUI_Stopping in GUI_main.pro.
    ;------------------------------------------------------
    ;NB!  Rainbow + white color table was loaded earlier
    ;     by GUI_Stopping, so black=0 and white=255.
    ;------------------------------------------------------
    Initialize_Hydrograph_Window, DRAW_ID, win_num
    np = 0L
    np_init = 1000
    np_max  = np_init
    tvals = fltarr(np_max)
    Qvals = fltarr(np_max)
endif

;-------------------
;Open the logfile ?      ******* (Append or overwrite ??)
;-------------------
if (WRITE_LOG) then begin
    TF_Get_LUN, LU, log_file
    openw, LU, log_file 
endif

;-----------------------------------------
;Prepare to track total water in and out
;of the main basin (using basin RTM file)
;-----------------------------------------
TRACK_VOLUME = (grid_vars.get_pvolume AND $
                grid_vars.RTM_file ne '')
if (TRACK_VOLUME) then begin
    volume_in  = 0d
    volume_out = 0d
    ;------------------------------------------
    ;Read basin pixels from RTM_file (7/15/05)
    ;------------------------------------------
    TF_Get_LUN, RTM_unit, grid_vars.RTM_file
    openr, RTM_unit, grid_vars.RTM_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(grid_vars.byte_order)
    temp = fstat(RTM_unit)
    basin_IDs = lonarr(temp.size / 4L)

    readu, RTM_unit, basin_IDs
    free_lun, RTM_unit

    wb = where(basin_IDs ge 0, nwb)

    if (nwb ne 0) then begin
        basin_IDs = basin_IDs[wb]
    endif else begin
        basin_IDs = (*grid_vars.outlet_IDs)[0L]
    endelse
endif
basin_area   = (*grid_vars.basin_areas)[0]
;basin_relief = (*grid_vars.basin_reliefs)[0]  ;(not used yet)

;-----------------------------------------------------
;Prepare to track total mass of each process for the
;entire DEM.  This will actually be a volume, in m^3.
;-----------------------------------------------------
vol_P  = 0d
vol_SM = 0d
vol_IN = 0d
vol_ET = 0d
vol_GW = 0d
vol_R  = 0d
vol_Rg = 0d

;--------------
;For debugging
;--------------
;TF_Print,'grid_vars.byte_order = ' + grid_vars.byte_order
;TF_Print,'# pixels in basin = ' + TF_String(nwb)
;TF_Print,' '

;--------------------
;Print start message
;--------------------
TF_Print,' '
TF_Print,'Starting new model run...'
TF_Print,'-------------------------------------------------'

;--------------
;The main loop
;-------------------------------------------
;Note that u and d from previous time step
;must be used on RHS of the equations here.
;--------------------------------------------
;If T_STOP keyword is set, then it is used
;to determine when DONE.  Otherwise, we're
;DONE when Q reaches Qp/20 on falling limb.
;--------------------------------------------
;Have already set dt = cv.dt
;----------------------------
n=0L  &  n_same=0L  &  DONE=0b
last_check_time  = systime(1)
last_check_time2 = systime(1)
last_plot_time   = 0.0

while NOT(DONE) do begin
    ;------------------------
    ;Compute time from start
    ;------------------------
    time_sec = (double(n) * dt)
    time = (time_sec / 60d)      ;[minutes]

    ;----------------------
    ;Check for interrupt ?
    ;------------------------------------------------------------
    ;(3/21/07) This method checks for a keyboard interrupt
    ;after a fixed amount of real time has elapsed.  This
    ;works much better than using something like (n mod n_check)
    ;since it avoids checking fast runs too often (which slows
    ;down performance) or slow runs too seldom (which means the
    ;user can't interrupt the run).  It only checks the elapsed
    ;time once each time through the loop, however, so the time
    ;required for one pass imposes a lower bound.
    ;------------------------------------------------------------
    elapsed_time = (systime(1) - last_check_time)
    if (elapsed_time gt 2) then begin
        ;print,'****************************************************'
        ;print,'Checking interrupt: n = ' + TF_String(n) 
        ;print,'****************************************************'
        Check_Interrupt, STOP_ID, OK 
        if NOT(OK) then begin
            Close_All_Output_Files, cv,sv,ev,iv,gv
            RETURN
        endif
        last_check_time = systime(1)
    endif

    ;-----------------------------------
    ;Print a progress message (NEW WAY)
    ;-----------------------------------
    elapsed_time2 = (systime(1) - last_check_time2)
    if (elapsed_time2 gt 2) then begin
        ;----------------------------------
        ;Report an "instantaneous" Q value
        ;----------------------------------
        Q_main_out = Pixel_Var(Q, outlet_ID)
        QSTR = ',   Q_out = ' + TF_String(Q_main_out)
        QSTR = QSTR + ' [m^3/s]'
        ASTR = (TSTR + QSTR)
        time_str = TF_String(time, FORMAT='(F10.2)')
        TF_Print,'Time = ' + time_str + ASTR
        ;-------------------------------------------
        ;Plotting hydrograph here is too infrequent
        ;-------------------------------------------
        last_check_time2 = systime(1)
    endif

    ;---------------------------------------
    ;Plot hydrograph up until now (3/22/07)
    ;---------------------------------------
    ;plus sign (psym=1), point (psym=3)
    ;---------------------------------------
    elapsed_plot_time = (time - last_plot_time)
    if (PLOT) AND (elapsed_plot_time gt 1.0) then begin
        ;----------------------------------
        ;Report an "instantaneous" Q value
        ;----------------------------------
        Q_main_out = Pixel_Var(Q, outlet_ID)
        tvals[np] = time
        Qvals[np] = Q_main_out
        wset, win_num
        plot, tvals[0:np], Qvals[0:np], $
              XMARGIN=[0,0.1], YMARGIN=[0,0.1], $
              YRANGE=[0,1.03 * max(Qvals)], $
              color=0, background=255, psym=-3  ;**** -1
        wait,0.005  ;[seconds]
        np = (np + 1L)
        if (np eq np_max) then begin
            tvals  = [tvals, fltarr(np_init)]
            Qvals  = [Qvals, fltarr(np_init)]
            np_max = n_elements(tvals)
        endif
        last_plot_time = time
    endif

    ;------------------------------------
    ;Print a progress message  (OLD WAY)
    ;------------------------------------
    ;if (VERBOSE) OR $
    ;   (NOT(VERBOSE) AND ((n mod nreport) eq 0) $
    ;    AND (n ne 0)) then begin
    ;    case (STOP_METHOD) of
    ;        0b : begin
                 ;----------------------------------------
                 ;This method reports "instantaneous" Q
                 ;values; need for "dynamic wave" option
                 ;----------------------------------------
    ;             Q_main_out = Pixel_Var(Q, outlet_ID)
    ;             QSTR = ',   Q_out = ' + TF_String(Q_main_out)
    ;             QSTR = QSTR + ' [m^3/s]'
    ;             ASTR = (TSTR + QSTR)
    ;             end
    ;      else : ASTR = TSTR
    ;    endcase
    ;    time_str = TF_String(time, FORMAT='(F8.2)')
    ;    TF_Print,'Time = ' + time_str + ASTR
    ;endif

    ;--------------
    ;For debugging
    ;--------------
    ;if (n gt 0) then begin
    ;    Q_main_out = Pixel_Var(Q, outlet_ID)
    ;    if (Q_main_out ne 0) then begin
    ;        print,'Q_main_out = ' + TF_String(Q_main_out)
    ;        print,'n = ' + TF_String(n)
    ;        STOP
    ;    endif
    ;endif

    ;---------------------------
    ;Compute the discharge grid
    ;-----------------------------------------------
    ;7/15/05.  Cross-sectional area of a trapezoid
    ;is given by:  Ac = d * (w + (d * tan(theta))),
    ;where w is the bottom width.  If we were to
    ;use: Ac = w * d, then Ac=0 when w=0.
    ;We also need angle units to be radians.
    ;----------------------------------------------
    ;See Notes for TF_Tan function in utils_TF.pro
    ;----------------------------------------------
    Ac = d * (widths + (d * TF_Tan(angles)) ) 
    Q  = float(u * Ac)

    ;--------------------------------------------------
    ;Wherever depth is less than z0, assume that water
    ;is not flowing and set u and Q to zero.
    ;However, we also need (d gt 0) to avoid a divide
    ;by zero problem, even when numerators are zero.
    ;--------------------------------------------------
    ;FLOWING = (d gt (z0/aval))
    ;*** FLOWING[noflow_IDs] = 0b    ;******
    ;u = (u * FLOWING)
    ;Q = (Q * FLOWING)
    ;d = (d > 0d)         ;(allow depths lt z0, if gt 0.)

    ;-------------------------
    ;Change the precip rate ?
    ;---------------------------------------------------------
    ;NB!  Precip is currently handled differently from the
    ;other processes, because variable durations are allowed.
    ;This means the Precipitation function must be called for
    ;every time step to see if it has changed.
    ;---------------------------------------------------------
    P = Precipitation(pv, mv, sv, time, duration_sums)
 
    ;---------------------------
    ;Change the snowmelt rate ?
    ;---------------------------
    if ((n mod snow_change_step) eq 0) then begin
        SM = Snowmelt(sv, mv)
        ;** print,'min(SM), max(SM) = ', min(SM), max(SM)
    endif

    ;--------------------------
    ;Change the ET loss rate ?
    ;--------------------------
    if ((n mod ET_change_step) eq 0) then begin
         ET = Evaporation(ev, mv, sv, gv, iv, d, h, y)  ;***, h_snow)
    endif

    ;----------------------
    ;Update the met vars ?      ;**********************************
    ;----------------------
    ;Update_Met_Vars

    ;------------------------
    ;Update the snow depth ?      ;**********************************
    ;------------------------
    ;Update_Snow_Depth, sv, mv  ;***, h_snow, h_swe

    ;-----------------------------
    ;Change the infil loss rate ?
    ;-----------------------------
    ;Both IN and Rg are returned.
    ;Rg gets passed to Seepage.
    ;------------------------------
    ;ET is applied as surface B.C.
    ;for Richards' eqn. method
    ;------------------------------
    ;Does infil_dt need to be the
    ;same as precip_dt ??
    ;------------------------------
    if ((n mod IN_change_step) eq 0) then begin
         IN = Infiltration(iv, P, SM, ET, h, z, Rg, r_last, n, nx,ny)
    endif

    ;---------------------------------------
    ;Change the GW seep gain or loss rate ?
    ;(seep can have either sign?)
    ;Does gw_dt need to match infil_dt ?
    ;---------------------------------------
    ;Note that dw grid is used here.
    ;---------------------------------------
    if ((n mod GW_change_step) eq 0) then begin
        GW = Seepage(gv, z, h, h_last, $
                     y, Rg, dw, ds, da, pIDs, $
                     p1,p2,p3,p4,p5,p6,p7,p8, $
                     w1,w2,w3,w4,w5,w6,w7,w8)
    endif

    ;------------
    ;For testing
    ;------------
    ;print,'min(P),  max(P)  = ', min(P), max(P)
    ;print,'min(IN), max(IN) = ', min(IN), max(IN)
    ;print,' '

    ;--------------------------------------
    ;Compute runoff or "excess rainrate"
    ;Each term must have same units: [m/s]
    ;Sum = net gain/loss rate over pixel.
    ;--------------------------------------
    R = (P + SM + GW) - (ET + IN)

    ;------------------------------------
    ;Increment rain and infiltrated mass
    ;for a mass balance check at the end
    ;------------------------------------
    Update_Mass_Totals, P,SM,IN,ET,GW,R,Rg, da,dt, n_pixels, $
                        vol_P, vol_SM, vol_IN, vol_ET, $
                        vol_GW, vol_R, vol_Rg

    ;--------------------------------------------------
    ;R can be positive or negative.  If negative, then
    ;water is removed from the surface at rate R until
    ;surface water is consumed.  (7/13/06)
    ;--------------------------------------------------
    ;NB!  As written, the Evaporation function also
    ;consumes water.  *******************************
    ;--------------------------------------------------
    ;** R = (R > 0.0)

    ;-------------------------------------------------
    ;Increment the volume of water in each pixel's
    ;channel based on flow in/out and excess rainrate
    ;------------------------------------------------------
    ;An implicit assumption here is that water contributed
    ;to pixels by excess rainrate gets into the channel in
    ;a single timestep.  May want to handle differently.
    ;------------------------------------------------------
    Update_Flow_Volume, Q, R, da, dt, vol, $
                        p1,p2,p3,p4,p5,p6,p7,p8, $
                        w1,w2,w3,w4,w5,w6,w7,w8

    ;-----------------------------------------------------
    ;Process sources, sinks and canals and update volumes
    ;These routines don't allow vol to become < 0.0.
    ;-----------------------------------------------------
    if (SOURCES) then Update_Sources, vol, time, dt, $
                      source_IDs, dur_sum_sources, Q_sources
    if (SINKS) then Update_Sinks, vol, time, dt, sink_IDs, $
                           dur_sum_sinks, Q_sinks
    if (CANALS) then Update_Canals, vol, time, dt, canal_in_IDs, $
                            canal_out_IDs, dur_sum_canals_in, $
                            Q_canals_in, t_canals

    ;------------------------------------------------------
    ;While R can be positive or negative, the surface flow
    ;volume must always be nonnegative. This also ensures
    ;that the flow depth is nonnegative.  (7/13/06)
    ;------------------------------------------------------
    vol = (vol > 0d)
    ;print,'min(vol), max(vol) = ', min(vol), max(vol)

    ;---------------------------------------------------
    ;Compute flow depth from volume and area only
    ;after all contributions have been added to volume
    ;---------------------------------------------------
    Update_Flow_Depth, d, d_last, vol, angles, widths, ds_chan

    ;----------------------------------------
    ;Set depth values on edges to zero since
    ;they become spikes (no outflow) 7/15/06
    ;----------------------------------------
    d[noflow_IDs] = 0.0

    ;----------------------------------------------
    ;4/19/06.  Force flow depth to be positive ?
    ;----------------------------------------------
    ;This seems to be needed with the non-Richards
    ;infiltration routines when starting with zero
    ;depth everywhere, since all water infiltrates
    ;for some period of time.  It also seems to be
    ;needed more for short rainfall records to
    ;avoid a negative flow depth error.
    ;----------------------------------------------
    ;7/13/06.  Still needed for Richards method
    ;----------------------------------------------
    d = (d > 0.0)

    ;print,'KINEMATIC WAVE = ', KINEMATIC_WAVE
    ;print,'DIFFUSIVE WAVE = ', DIFFUSIVE_WAVE
    ;print,'DYNAMIC WAVE   = ', DYNAMIC_WAVE
    ;STOP

    ;-------------------------------------------
    ;Update velocity via kinematic wave model ?
    ;Use the just-updated value of d.
    ;-------------------------------------------
    if (KINEMATIC_WAVE) then begin
        u = Kinematic_Velocity(d, S_bed, cv)    ;(7/13/05)
    endif

    ;-------------------------------------------
    ;Update velocity via diffusive wave model ?
    ;-------------------------------------------
    if (DIFFUSIVE_WAVE) then begin
        ;---------------------------
        ;Get the free-surface slope
        ;Use updated d vs. last one
        ;---------------------------
        ;** S_free = Free_Surface_Slope(S_bed, d_last, ds, pIDs)
        S_free = Free_Surface_Slope(S_bed, d, ds_chan, pIDs)

        ;-----------------------------------------
        ;Compute velocity using S_free vs. S_bed
        ;-----------------------------------------
        ;NB! This involves computing sqrt(S_free)
        ;so Disallow "backflow" this way ?
        ;-----------------------------------------
        S_free = (S_free > 0d)
        u = Kinematic_Velocity(d, S_free, cv)

        ;--------------------------------------
        ;Allow negative velocity (backflow) ??
        ;But to which child pixel ??
        ;--------------------------------------
        ;u = Kinematic_Velocity(d, abs(S_free), cv)
        ;wn = where(S_free lt 0d, nwn)
        ;if (nwn ne 0) then u[wn] = -1d * u[wn]
    endif

    ;-----------------------------------------
    ;Update velocity via dynamic wave model ?
    ;-----------------------------------------------
    ;Use just-updated d vs. last one? (in 2 places)
    ;If so, then we can eliminate d_last.
    ;-----------------------------------------------
    if (DYNAMIC_WAVE) then begin
        ;---------------------------
        ;Get the free-surface slope
        ;Use updated d vs. last one
        ;---------------------------
        ;** S_free = Free_Surface_Slope(S_bed, d_last, ds, pIDs)
        S_free = Free_Surface_Slope(S_bed, d, ds_chan, pIDs)

        ;--------------------------------------
        ;Compute the wetted bed area (2/12/07)
        ;--------------------------------------
        ;pb = d / cos(angles)
        ;;ww = where(abs(angles) ge (!dpi/2d), nww)  ;(should disallow)
        ;;if (nww ne 0) then pb[ww]=(d * 10d)
        ;pw = widths + (2d * pb)
        ;bA = pw * ds_chan        ;(a 2D array)
        ;*** bA = da             ;(for testing only)

        ;----------------------------------------
        ;Update the velocity (overwrite old one)
        ;----------------------------------------
        ;** Update_Velocity_Dynamic, u, Q, d_last, S_free, $
        Update_Velocity_Dynamic, u, Q, d, S_free, R, da, dt, $
                      widths, angles, ds_chan, cv, $
                      p1,p2,p3,p4,p5,p6,p7,p8, $
                      w1,w2,w3,w4,w5,w6,w7,w8
    endif

    ;--------------------------------------
    ;Force edge pixels to have u=0.
    ;--------------------------------------
    ;Large slope around 1 flows into small
    ;slope & leads to a negative velocity.
    ;--------------------------------------
    u[noflow_IDs] = 0d 

    ;-------------------------------------------
    ;Track volume of water in and out of basin?
    ;-------------------------------------------
    if (TRACK_VOLUME) then begin
        ;-----------------------------------
        ;Integrate discharge at main outlet
        ;using model vs. sampling timestep
        ;-----------------------------------
        Update_Volume_Out, volume_out, Q, outlet_ID, dt

        ;----------------------------------------------
        ;Integrate precip. over basin that drains to
        ;main outlet using model vs. sampling timestep
        ;----------------------------------------------
        Update_Volume_In, volume_in, time, duration_sums, pv, $
                          basin_area, basin_IDs, dt, da
    endif

    ;---------------------------
    ;Record peak values & times
    ;---------------------------
    ;Update_Peak_Values  ******
    ;----------------------------
    if (Q[outlet_ID] gt Q_peak) then begin
        Q_peak = Q[outlet_ID]
        T_peak = time
    endif
    if (u[outlet_ID] gt u_peak) then begin
        u_peak  = u[outlet_ID]
        Tu_peak = time
    endif
    if (d[outlet_ID] gt d_peak) then begin
        d_peak  = d[outlet_ID]
        Td_peak = time
    endif

    ;--------------------------------------
    ;Richards' equation for infiltration ?
    ;--------------------------------------
    if (INFIL_METHOD eq 4b) then begin
        ;-----------------------------------
        ;Get soil moisture at surface as q0
        ;-----------------------------------
        if (iv.ALL_SCALARS) then q0=(*iv.q)[0] else q0=(*iv.q)[*,*,0]
    endif

    ;------------------------------------------
    ;Save a subsequence of outlet pixel values
    ;for selected output variables
    ;------------------------------------------
    Save_Pixel_Values, n, time, outlet_IDs, pformat, $
                       Q, u, d, f, SM, ET, IN, q0, $
                       ;-----------------------------------------
                       cv, mv, sv, ev, iv, gv, iv.ALL_SCALARS, $
                       ;---------------------------------------------
                       chan_pixel_samp_step, snow_pixel_samp_step, $
                       et_pixel_samp_step, in_pixel_samp_step, $
                       gw_pixel_samp_step

    ;-----------------------------------------------
    ;Save a subsequence of computed grids, "cubes"
    ;and "z-profiles" (at outlet_IDs) for a set of
    ;selected output variables
    ;-----------------------------------------------
    Save_Grid_Values, n, time, outlet_IDs, nx, ny, $
                      cv, mv, sv, ev, iv, gv, $
                      Q, u, d, f, SM, ET, IN, q0, $
                      ;-------------------------------------------
                      chan_grid_samp_step, snow_grid_samp_step, $
                      et_grid_samp_step, in_grid_samp_step, $
                      gw_grid_samp_step, $
                      ;-------------------------------------------
                      in_stack_samp_step, in_profile_samp_step 

    ;-----------------------
    ;Optional status report
    ;-----------------------
    if (VERBOSE) then begin
        wflow   = where(FLOWING ne 0b, n_flow)
        percent = 100.0 * (float(n_flow) / n_pixels)
        fstr    = strtrim(string(percent, FORMAT='(F5.1)'), 2) + '%'
        TF_Print,' Percentage of pixels with flow = ' + fstr
        TF_Print,' '
        ;------------------------------------------------
        Print_Mins_And_Maxes, Q,u,d, nx,ny,0b, QMAX=Qmax
        ;------------------------------------------------
        wmax = where(Q eq Qmax, nwmax)
        TF_Print,' Max(Q) occurs at: ' + TF_String(wmax[0])
        ;print,' Max attained at ', nwmax, ' pixels.'
        TF_Print,' '
        TF_Print,'-------------------------------------------------'
    endif

    ;-------------------------------
    ;Methods to stop the simulation
    ;-------------------------------
    case (STOP_METHOD) of
        0b : begin
             FALLING_LIMB = (Q_last gt Q[outlet_ID])

             ;------------------------------------------------------
             ;With DYNAMIC_WAVE, it is possible for some reason for
             ;Q_outlet to drop back to zero early in the simulation
             ;so that model run ends right away.  (2/13/07)
             ;Uncomment the debugging section below.
             ;------------------------------------------------------
             if (FALLING_LIMB) then begin
                 Q_stop = (Q_peak * Qpeak_fraction)
                 DONE = (Q[outlet_ID] le Q_stop) AND $
                        ;**********************************************
                        (Q[outlet_ID] gt 0)  ;(Is this OK in general?)
                        ;**********************************************
                        ;** (n gt 4)  ;(doesn't work)

                 ;--------------
                 ;For debugging
                 ;--------------
                 ;if (DONE) then begin
                 ;    print,'Q_last = ',Q_last
                 ;    print,'Q_peak = ',Q_peak
                 ;    print,'Qpeak_fraction = ', Qpeak_fraction
                 ;    print,'Q[outlet_ID] = ',Q[outlet_ID]
                 ;    print,'Q_stop = ',Q_stop
                 ;    print,' '
                 ;endif
             endif
             ;*** Q_last = Q[outlet_ID]   ;(Now done always, below)
             end
        1b : DONE = (time ge T_stop)  ;[minutes]
        2b : DONE = (n ge n_steps)
    endcase

    ;--------------------------
    ;Check for negative depth
    ;----------------------------------------------------
    ;NB! DONE may already have been set to 1b.  If so,
    ;then don't need to check things and must be careful
    ;not to set DONE back to 0b in the check routines.
    ;Put after case statement.
    ;----------------------------------------------------
    if NOT(DONE) then Check_Flow_Depth, d, dt, nx, DONE    ;*****
    ;** d[noflow_IDs] = 0d    ;*****

    ;-----------------------------
    ;Check for negative velocity
    ;due to numerical instability
    ;Put after case statement.
    ;-----------------------------
    if NOT(DONE) then Check_Flow_Velocity, u, d, dt, nx, DONE  ;***, grav, fric

    ;-------------------------------
    ;Check for NaNs in infiltration
    ;-------------------------------
    if (iv.method ne 0) AND NOT(DONE) then begin
        Check_Infiltration, IN, DONE
    endif

    ;-----------------------------------
    ;Stop if steady state was reached ?
    ;-----------------------------------
    ;Count number of steps for which
    ;Q at outlet has not changed
    ;-----------------------------------
    Q_outlet = Q[outlet_ID]
    if NOT(DONE) then Check_Steady_State, Q_outlet, Q_last, Q_peak, $
                                          n_same, STOP_METHOD, DONE
    Q_last = Q[outlet_ID]    ;(Need this always)

    ;-----------------------
    ;Increment the timestep
    ;-----------------------
    n = (n + 1L)

endwhile;  (time step loop)

;---------------------------
;Get final values at outlet
;---------------------------
Q_final = Q[outlet_ID]

;----------------
;Close the files
;----------------
Close_All_Output_Files, cv,sv,ev,iv,gv

;------------------------------------
;NB! This is now done inside of loop
;------------------------------------
;Check for valid, nonzero hydrograph
;------------------------------------
;if (Q_peak eq 0.0) then begin
;    msg = [' ', $
;    'ERROR:  Q_outlet is zero for all times. ', ' ',$
;    'Is there a runoff-producing process ? ', ' ']
;    TF_Print,'*************************************************'
;    TF_Print, msg[1]
;    TF_Print, msg[3]
;    TF_Print,'*************************************************'
;    TF_Print,' '
;    result = GUI_Message(msg, /INFO)
;endif

;-----------------------
;Print a summary report
;-----------------------
;*** if (STOP_METHOD ne 1) then T_STOP = 0.0
if (n_elements(COMMENT) eq 0) then COMMENT=''
;---------------------------------------------
Print_Final_Report, NAME, COMMENT, time, start, outlet_ID, nx, $
                    TRACK_VOLUME, volume_in, volume_out, $
                    basin_area, pv.max_rate, $
                    cv.dt, sv.dt, ev.dt, iv.dt, gv.dt, ov.dt, $
                    cv.save_pixels_dt, $   ;********
                    n, STOP_METHOD, T_STOP, $
                    MANNING, *cv.nvals, LAW_OF_WALL, *cv.z0vals, $
                    Q_final, Q_peak, T_peak, u_peak, Tu_peak, $
                    d_peak, Td_peak, WRITE_LOG, LU

;---------------------------
;Print final mins and maxes
;---------------------------
Print_Mins_And_Maxes, Q, u, d, nx, ny, $
                      WRITE_LOG, LU, /FINAL

;-----------------------------------------
;Print uniform precip. rate information ?
;-----------------------------------------
UNIFORM_PRECIP = (pv.rate_type le 1b)  ;(scalar or time series)
if (UNIFORM_PRECIP) then begin
    Print_Uniform_Precip_Data, pv, WRITE_LOG, LU
    ;** Print_Dimless_Number_Data, pv, T_peak, $
    ;**                            T_final, basin_length, $
    ;**                            WRITE_LOG, V
endif

;--------------------------
;Print mass balance report
;--------------------------
TF_Print,'Volume rain        = ' + TF_String(vol_P)  + ' [m^3]'
TF_Print,'Volume snowmelt    = ' + TF_String(vol_SM) + ' [m^3]'
TF_Print,'Volume infiltrated = ' + TF_String(vol_IN) + ' [m^3]'
TF_Print,'Volume evaporated  = ' + TF_String(vol_ET) + ' [m^3]'
TF_Print,'Volume seepage     = ' + TF_String(vol_GW) + ' [m^3]'
TF_Print,'Volume runoff      = ' + TF_String(vol_R)  + ' [m^3]'
TF_Print,'Volume bottom loss = ' + TF_String(vol_Rg) + ' [m^3]'
;-----------------------------------------------------------------

if (iv.method eq 4b) then begin
    ;------------------------------------
    ;Richards' equation for infiltration
    ;Ground water losses not counted yet
    ;------------------------------------
    vol_stored = 0d
    SCALAR_dz  = (n_elements(*iv.dz) eq 1)
    SCALAR_da  = (n_elements(da) eq 1)     ;(assume da is scalar or grid)
    ;---------------------------------
    dim_q  = (size(*iv.q))[0]
    dim_qi = (size(*iv.qi))[0]
    ;--------------------------
    for k=0,(iv.nz-1) do begin
        ;------------------------------------
        ;In this loop, dz is always a scalar
        ;------------------------------------
        if (SCALAR_dz)   then dz=*iv.dz           else dz=(*iv.dz)[k]
        if (dim_q  eq 3) then qq=(*iv.q)[*,*,k]   else qq=(*iv.q)[k]
        if (dim_qi eq 3) then qqi=(*iv.qi)[*,*,k] else qqi=(*iv.qi)[k]
        ;--------------------------------------
        ;Get increase in q over initial values
        ;(but q may have decreased due to ET)
        ;--------------------------------------
        dq = (qq - qqi)   ;(grid or scalar)
        SCALAR_dq = (n_elements(dq) eq 1)
        if (SCALAR_da AND SCALAR_dq) then begin
            dm = dq * (dz * da * n_pixels)      ;(da is a SCALAR)
        endif else begin
            dm = total(dq * (da * dz), /DOUBLE)  ;(da is a GRID)
        endelse
        vol_stored = vol_stored + dm
    endfor
    mass_error = 100d * (vol_IN - vol_stored) / vol_IN
    err_str    = string(mass_error, format='(D6.2)') + ' % '
    TF_Print,'Volume stored      = ' + TF_String(vol_stored) + $
             ' [m^3]'
    TF_Print,'Volume error       = ' + err_str
endif
TF_Print,' '

;------------------
;Close the logfile
;------------------
if (WRITE_LOG) then free_lun, LU

;---------------------------
;Close any files still open
;---------------------------
close, /ALL
TF_Print,'Finished.' + '  (' + run_prefix + ')'

END;  Route_Flow
;***************************************************************
pro Check_Flow_Depth, d, dt, nx, DONE

on_error, 2

;-------------------------------
;All all flow depths positive ?
;-------------------------------
wbad = where((d lt 0.0) OR (finite(d) ne 1), nbad)

if (nbad eq 0) then RETURN

dmin = min(d[wbad])
badi = wbad[0]
bcol = (badi mod nx)
brow = (badi  /  nx)
crstr = TF_String(bcol) + ', ' + TF_String(brow)
msg  = [' ', $
'*******************************************',$
'ERROR: Route_Flow aborted.',$
' ',$
'Negative depth found: ' + TF_String(dmin), $
'Time step may be too large.',$
' ',$
'(Column, Row):  ' + crstr,$
'Time step:      ' + TF_String(dt) + ' [s]',$
'*******************************************',$
' ']
for k=0,(n_elements(msg) - 1) do TF_Print,msg[k]

GUI_Error_Message,msg
DONE = 1b  ;(abort loop)

END;  Check_Flow_Depth
;************************************************************************
pro Check_Flow_Velocity, u, d, dt, nx, DONE  ;*** grav, fric 

on_error, 2

;------------------------------
;Are all velocities positive ?
;------------------------------
wbad = where((u lt 0.0) OR (finite(u) ne 1), nbad)
if (nbad eq 0) then RETURN

umin = min(u[wbad])
badi = wbad[0]
bcol = (badi mod nx)

brow = (badi  /  nx)
crstr = TF_String(bcol) + ', ' + TF_String(brow)
msg  = [' ', $
'*******************************************',$
'ERROR: Route_Flow aborted.',$
' ',$
'Negative velocity found: ' + TF_String(umin),$
'Time step may be too large.',$
' ',$
'(Column, Row):  ' + crstr,$
'Flow depth:     ' + TF_String(d[badi]),$
;*** 'Gravity term:   ' + TF_String(grav[badi]),$   ;(DYNAMIC_WAVE only)
;*** 'Friction term:  ' + TF_String(fric[badi]),$
'Time step:      ' + TF_String(dt) + ' [s]',$
'*******************************************',$
' ']
for k=0,(n_elements(msg) - 1) do TF_Print,msg[k]

GUI_Error_Message,msg
DONE = 1b  ;(* Abort the loop *)

END;  Check_Flow_Velocity
;************************************************************************
pro Check_Infiltration, IN, DONE

;------------------------------------
;Check for NaNs in infiltration rate
;------------------------------------
;NB!  Don't set DONE = 0b, it may
;already have been set to 1
;------------------------------------
w = where(finite(IN) ne 1, nbad)
if (nbad eq 0) then RETURN

;----------------------------------------
;Issue warning message and abort the run
;----------------------------------------
DONE = 1b
msg  = ['ERROR:  Aborting model run', ' ',$
        'Invalid values found in infiltration rates.', $
        'Number of NaN values = ' + TF_String(nbad), ' ']
GUI_Error_Message, msg

END;  Check_Infiltration
;************************************************************************
pro Check_Steady_State, Q_outlet, Q_last, Q_peak, $
                        n_same, STOP_METHOD, DONE

;----------------
;Local constants
;--------------------------------------------------------------
;Best value of tolerance also depends on the time step.
;For "plane" case, result changed with timestep = 2 or 4 secs.
;--------------------------------------------------------------
tol  = 1e-5       ;("optimal" value found by trial and error)
nn   = 500L
tol2 = 1e-5
;tol2 = 1e-6      ;(worked better for "plane" case with step=2s)

;----------------------------------------
;Count number of steps with same Q-value
;----------------------------------------
if (abs(Q_outlet - Q_last) le tol) then begin
    n_same = (n_same + 1L)
endif else begin
    n_same = 0L
endelse

;-----------------------------------
;Check for steady-state, with Q > 0
;-----------------------------------
if ((STOP_METHOD eq 0b) AND (Q_outlet gt tol2) AND (n_same gt nn)) then begin
    msg = ['WARNING: ', ' ', $
           'It appears that discharge, Q, has reached',$
           'a steady-state condition.', ' ', $
           'Do you want to continue anyway ?', ' ']
    answer = GUI_Message(msg, /QUESTION)
    DONE   = (strlowcase(answer) eq 'no')
    if NOT(DONE) then n_same = 0L

    ;TF_Print,'****************************************************'
    ;TF_Print,'Aborting model run: '
    ;TF_Print,'Discharge, Q, has reached steady-state.'
    ;TF_Print,'****************************************************'
    ;msg = [ $
    ;'WARNING:  Route_Flow aborted.', ' ',$
    ;'Discharge, Q, has reached steady-state. ', ' ']
    ;GUI_Error_Message, msg
    ;DONE = 1b
endif

;---------------------------------------------
;(3/20/07) Commented out, since user can now
;press any key to stop the model run.  Note
;that Q-value can remain zero for a long time
;if everything is infiltrating or snow depth
;is building, etc.
;---------------------------------------------
;Check for unchanging Q-value of zero ?
;---------------------------------------
;if (STOP_METHOD eq 0b) AND (Q_peak eq 0.0) AND (n_same gt nn) then begin
;    msg = [' ', $
;    'ERROR: ', ' ', $
;    'Discharge at outlet is zero for all times. ', ' ',$
;    'Is there a runoff-producing process ? ', ' ']
;    TF_Print,'*************************************************'
;    TF_Print, msg[1]
;    TF_Print, msg[3]
;    TF_Print,'*************************************************'
;    TF_Print,' '
;    GUI_Error_Message, msg
;    DONE = 1b
;endif

end;  Check_Steady_State
;************************************************************************
pro Print_Final_Report, NAME, COMMENT, time, start, outlet_ID, nx, $
                TRACK_VOLUME, volume_in, volume_out, $
                basin_area, max_precip_rate, $
                ;--------------------------------------------
                cv_dt, sv_dt, ev_dt, iv_dt, gv_dt, ov_dt, $
                sample_dt, n, STOP_METHOD, T_STOP, $
                ;--------------------------------------------
                MANNING, nvals, LAW_OF_WALL, z0vals, $
                Q_final, Q_peak, T_peak, u_peak, Tu_peak, $
                d_peak, Td_peak, WRITE_LOG, LU

on_error, 2    ;(return to caller)

;---------------------
;Compute derived vars 
;---------------------
T_final  = time
run_time = (systime(1) - start)/60d
out_col  = (outlet_ID mod nx)
out_row  = (outlet_ID  /  nx)

if (TRACK_VOLUME) then begin
    if (volume_in ne 0) then begin
        percent_out = 100.0 * volume_out / volume_in
    endif else percent_out = 0.0
endif

;-----------------
;Print the report
;-----------------
TF_Print,'-----------------------------------------------------------'
TF_Print, TopoFlow_Version()
TF_Print,' '
TF_Print,'Simulated Hydrograph for ' + NAME
if (keyword_set(COMMENT)) then TF_Print,COMMENT

TF_Print,' '
TF_Print,'Simulated time:    ' + TF_String(T_final) + ' [min]'
TF_Print,'Program run time:  ' + TF_String(run_time) + ' [min]'
TF_Print,' '
TF_Print,'Channel timestep:  ' + TF_String(cv_dt) + ' [s]'

TF_Print,'Snowmelt timestep: ' + TF_String(sv_dt) + ' [s]'
TF_Print,'ET timestep:       ' + TF_String(ev_dt) + ' [s]'

TF_Print,'Infiltr. timestep: ' + TF_String(iv_dt) + ' [s]'
TF_Print,'Subsurf timestep:  ' + TF_String(gv_dt) + ' [s]'
;** TF_Print,'Overland timestep: ' + TF_String(ov_dt) + ' [s]'
;** TF_Print,'Sampled timestep:  ' + TF_String(sample_dt) + ' [s]'
TF_Print,'N_timesteps:       ' + TF_String(n)
if (STOP_METHOD eq 1b) then $  ;********


TF_Print,'T_stop:            ' + TF_String(T_STOP) + ' [min]'
TF_Print,' '
TF_Print,'Main outlet ID:    ' + TF_String(outlet_ID)
TF_Print,'Outlet (col,row):  ' + TF_String(out_col) + ', ' + $
                                 TF_String(out_row)
TF_Print,'Basin_area:        ' + TF_String(basin_area) + ' [km^2] '
;*** TF_Print,'Basin_length:      ' + TF_String(basin_length) + ' [m]'
TF_Print,' '
if (MANNING) then begin

    nmin = min(nvals, max=nmax)
    TF_Print,"Min Manning's n:   " + TF_String(nmin)
    TF_Print,"Max Manning's n:   " + TF_String(nmax)
endif

if (LAW_OF_WALL) then begin
    z0min = min(z0vals, max=z0max)
    TF_Print,"Min z0 value:      " + TF_String(z0min) + ' [m]'
    TF_Print,"Max z0 value:      " + TF_String(z0max) + ' [m]'
endif
TF_Print,' '
TF_Print,'Q_final:           ' + TF_String(Q_final) + ' [m^3/s]'
TF_Print,'Q_peak:            ' + TF_String(Q_peak)  + ' [m^3/s]'
TF_Print,'T_peak:            ' + TF_String(T_peak)  + ' [min]'
TF_Print,'u_peak:            ' + TF_String(u_peak)  + ' [m/s]'
TF_Print,'Tu_peak:           ' + TF_String(Tu_peak) + ' [min]'
TF_Print,'d_peak:            ' + TF_String(d_peak)  + ' [m]'
TF_Print,'Td_peak:           ' + TF_String(Td_peak) + ' [min]'
TF_Print,' '

if (TRACK_VOLUME) then begin
    TF_Print,'Total volume out:  ' + TF_String(volume_out) + ' [m^3]'
    if (basin_area ne 0) then begin
        TF_Print,'Rain volume in:    ' + TF_String(volume_in)  + ' [m^3]'
        TF_Print,'Percentage out:    ' + TF_String(percent_out) + ' [%]'
    endif
endif

;------------------------------
;Print the maximum precip rate
;------------------------------
MPR = (max_precip_rate * 3600000d)   ;(Convert [m/s] -> [mm/hr])
TF_Print,' '
TF_Print,'Max(precip rate):  ' + TF_String(MPR) + ' [mm/hr]' 
TF_Print,' '

;-------------------
;Write to logfile ?
;-------------------
if NOT(WRITE_LOG) then RETURN
printf,LU,' '
printf,LU,TopoFlow_Version() 

printf,LU,' '
printf,LU,'Simulated Hydrograph for ' + NAME
if (keyword_set(COMMENT)) then printf,LU,COMMENT
printf,LU,' '
printf,LU,'Simulated time:      ' + TF_String(T_final) + ' [min]'
printf,LU,'Program run time:    ' + TF_String(run_time) + ' [min]'
printf,LU,' '

printf,LU,'Channel timestep:   ' + TF_String(cv_dt) + ' [s]'
printf,LU,'Snowmelt timestep:  ' + TF_String(sv_dt) + ' [s]'
printf,LU,'ET timestep:        ' + TF_String(ev_dt) + ' [s]'
printf,LU,'Infiltr. timestep:  ' + TF_String(iv_dt) + ' [s]'
printf,LU,'Subsurf timestep:   ' + TF_String(gv_dt) + ' [s]'
;** printf,LU,'Overland timestep:  ' + TF_String(ov_dt) + ' [s]'

;** printf,LU,'Sampled timestep:   ' + TF_String(sample_dt) + ' [s]'
printf,LU,'N_timesteps:        ' + TF_String(n)
if (STOP_METHOD eq 1b) then $  ;********
printf,LU,'T_stop:            ' + TF_String(T_STOP) + ' [min]'
printf,LU,' '
printf,LU,'Main Outlet ID:    ' + TF_String(outlet_ID)
printf,LU,'Outlet (col,row):  ' + TF_String(out_col) + ', ' + $
                                  TF_String(out_row)
printf,LU,'Basin_area:        ' + TF_String(basin_area) + ' [km^2]'
;*** printf,LU,'Basin_length:      ' + TF_String(basin_length) + ' [m]'
 
printf,LU,' '
if (MANNING) then begin
    ;*** nmin = min(nvals, max=nmax)
    printf,LU,"Min Manning's n:   " + TF_String(nmin)
    printf,LU,"Max Manning's n:   " + TF_String(nmax)
endif

if (LAW_OF_WALL) then begin
    ;*** z0min = min(z0vals, max=z0max)
    printf,LU,"Min z0 value:      " + TF_String(z0min) + ' [m]'
    printf,LU,"Max z0 value:      " + TF_String(z0max) + ' [m]'
endif
printf,LU,' '

printf,LU,'Q_final:           ' + TF_String(Q_final) + ' [m^3/s]'
printf,LU,'Q_peak:            ' + TF_String(Q_peak)  + ' [m^3/s]'
printf,LU,'T_peak:            ' + TF_String(T_peak)  + ' [min]'
printf,LU,'u_peak:            ' + TF_String(u_peak)  + ' [m/s]'
printf,LU,'Tu_peak:           ' + TF_String(Tu_peak) + ' [min]'
printf,LU,'d_peak:            ' + TF_String(d_peak)  + ' [m]'
printf,LU,'Td_peak:           ' + TF_String(Td_peak) + ' [min]'
printf,LU,' '

if (TRACK_VOLUME) then begin
    printf,LU,'Total volume out:  ' + TF_String(volume_out) + ' [m^3]'
    if (basin_area ne 0) then begin
        printf,LU,'Rain volume in:    ' + TF_String(volume_in)  + ' [m^3]'

        printf,LU,'Percentage out:    ' + TF_String(percent_out) + ' [%]'
        printf,LU,' '
    endif
endif

printf,LU,' '
printf,LU,'Max(precip rate):  ' + TF_String(max_precip_rate) + ' [m/s]' 
printf,LU,' '

END;  Print_Final_Report
;************************************************************************
pro Print_Mins_And_Maxes, Q, u, d, nx, ny, WRITE_LOG, LU, $
                          QMIN=Qmin, QMAX=Qmax, $
                          FINAL=FINAL

on_error, 2    ;(return to caller)

FINAL = keyword_set(FINAL)

;---------------------------
;Compute the mins and maxes
;---------------------------
Qmin = min(Q[1:nx-2,1:ny-2], max=Qmax)
umin = min(u[1:nx-2,1:ny-2], max=umax)
dmin = min(d[1:nx-2,1:ny-2], max=dmax)

f1 = '(F14.4)'   ;(2/12/07)

Qstr = TF_String(Qmin, FORMAT=f1)
Qstr = Qstr + ', ' + TF_String(Qmax, FORMAT=f1)

ustr = TF_String(umin, FORMAT=f1)
ustr = ustr + ', ' + TF_String(umax, FORMAT=f1)
dstr = TF_String(dmin, FORMAT=f1)
dstr = dstr + ', ' + TF_String(dmax, FORMAT=f1)

if (FINAL) then TF_Print,'Final grid mins and maxes:' $
else TF_Print,'------------------------------------------'
TF_Print,'Min(Q), Max(Q):   ' + Qstr + ' [m^3/s]'
TF_Print,'Min(u), Max(u):   ' + ustr + ' [m/s]'
TF_Print,'Min(d), Max(d):   ' + dstr + ' [m]
TF_Print,' '

;--------------------
;Write to log file ?
;--------------------
if (WRITE_LOG) then begin
    if (FINAL) then printf, LU, 'Final grid mins and maxes:' $
    else printf,LU,'------------------------------------------'
    printf, LU, 'Min(Q), Max(Q):   ' + Qstr + ' [m^3/s]'
    printf, LU, 'Min(u), Max(u):   ' + ustr + ' [m/s]'
    printf, LU, 'Min(d), Max(d):   ' + dstr + ' [m]
    printf, LU, ' '
endif

END;  Print_Mins_And_Maxes
;************************************************************************
pro Print_Uniform_Precip_Data, pv, WRITE_LOG, LU

on_error, 2    ;(return to caller)

rates     = (*pv.rates * 3600000d)    ;[m/s] -> [mm/hr]
durations = *pv.durations

rstr  = TF_String(rates[0])
dstr  = TF_String(durations[0])
nr    = n_elements(rates)

for m=1,(nr-1L) do begin
    rstr  = (rstr  + '  ' + TF_String(rates[m]))
    dstr  = (dstr  + '  ' + TF_String(durations[m]))
endfor

;-----------------------------------
;This is too verbose in most cases?
;-----------------------------------

;TF_Print,'Uniform precip. rate information: '
;TF_Print,'Precip. rate:     ' + rstr + ' [mm/hr]'
;TF_Print,'Duration:         ' + dstr + ' [min]'
;TF_Print,' '

;--------------------
;Write to log file ?
;--------------------
if (WRITE_LOG) then begin
    printf,LU,'Uniform precip. rate information: '
    printf,LU,'Precip. rate:     ' + rstr + ' [mm/hr]'
    printf,LU,'Duration:         ' + dstr + ' [min]'
    printf,LU,' '
endif

END;  Print_Uniform_Precip_Data
;************************************************************************
pro Print_Dimless_Number_Data, pv, T_peak, T_final, $
                               basin_length, WRITE_LOG, LU

on_error, 2    ;(return to caller)

rates     = *pv.rates
durations = *pv.durations

;-----------------------------------
;Compute some dimensionless numbers
;that characterize the hydrograph

;-----------------------------------
TAU_P = (T_peak  / durations)
TAU_F = (T_final / durations)
PSI   = (basin_length / (rates[0] * durations[0] * 60d))

nd    = n_elements(durations)
tpstr = TF_String(TAU_P[0])
tfstr = TF_String(TAU_F[0])
for m=1,(nd - 1L) do begin
    tpstr  = (tpstr  + '  ' + TF_String(TAU_P[m]))
    tfstr  = (tfstr  + '  ' + TF_String(TAU_F[m]))
endfor

;---------------------------------------
;Make predictions for Q_peak and T_peak
;---------------------------------------
Q_peak_pred = (0.2d * rates[0] * (basin_length)^2d / 3d)
T_peak_pred = (3d * durations[0])

TF_Print,'Dimensionless number information:'

TF_Print,'T_peak /Duration: ' + tpstr
TF_Print,'T_final/Duration: ' + tfstr

TF_Print,'Psi=L/(R*TD):     ' + TF_String(PSI) + ' [unitless]'
TF_Print,' '
TF_Print,'Q_peak predicted: ' + TF_String(Q_peak_pred) + ' [m^3/s]'
TF_Print,'T_peak predicted: ' + TF_String(T_peak_pred) + ' (min]'

;--------------------
;Write to log file ?
;--------------------
if (WRITE_LOG) then begin
    printf,LU,'Dimensionless number information:'

    printf,LU,'T_peak /Duration: ' + tpstr

    printf,LU,'T_final/Duration: ' + tfstr

    ;printf,LU,'Psi=L/(R*TD):     ' + TF_String(PSI) + ' [unitless]'
    ;printf,LU,' '
    ;printf,LU,'Q_peak predicted: ' + TF_String(Q_peak_pred) + ' [m^3/s]'

    ;printf,LU,'T_peak predicted: ' + TF_String(T_peak_pred) + ' [min]'
    ;printf,LU,' '


endif

END;  Print_Dimless_Number_Data
;************************************************************************
pro Check_Output_Options, grid_vars, rv,cv,sv,ev,iv,gv, nx,ny, $
                          outlet_IDs, n_outlets, outlet_ID, OK

OK = 1b
RTI_file = grid_vars.RTI_file

GRID_OUTPUT  =  cv.SAVE_Q_GRIDS  + cv.SAVE_U_GRIDS  + $
                cv.SAVE_D_GRIDS  + cv.SAVE_F_GRIDS  + $
                ;-----------------------------------------
                sv.SAVE_MR_GRIDS + sv.SAVE_HS_GRIDS + $
                sv.SAVE_SW_GRIDS + sv.SAVE_CC_GRIDS + $
                sv.SAVE_EA_GRIDS + sv.SAVE_ES_GRIDS + $
                ;-----------------------------------------
                ev.SAVE_ER_GRIDS + $
                ;-----------------------------------------
                iv.SAVE_V0_GRIDS + iv.SAVE_Q0_GRIDS + $
                iv.SAVE_I_GRIDS  + iv.SAVE_ZW_GRIDS + $
                ;-----------------------------------------
                gv.SAVE_HT_GRIDS + gv.SAVE_DF_GRIDS + $
                gv.SAVE_DT_GRIDS

PIXEL_OUTPUT = cv.SAVE_Q_PIXELS  + cv.SAVE_U_PIXELS  + $
               cv.SAVE_D_PIXELS  + cv.SAVE_F_PIXELS  + $
               ;------------------------------------------
               sv.SAVE_MR_PIXELS + sv.SAVE_HS_PIXELS + $
               sv.SAVE_SW_PIXELS + sv.SAVE_CC_PIXELS + $
               sv.SAVE_EA_PIXELS + sv.SAVE_ES_PIXELS + $
               ;------------------------------------------
               ev.SAVE_ER_PIXELS + $
               ;------------------------------------------
               iv.SAVE_V0_PIXELS + iv.SAVE_Q0_PIXELS + $
               iv.SAVE_I_PIXELS  + iv.SAVE_ZW_PIXELS + $
               ;------------------------------------------
               gv.SAVE_HT_PIXELS + gv.SAVE_DF_PIXELS  + $
               gv.SAVE_DT_PIXELS

STACK_OUTPUT = iv.SAVE_Q_STACKS + iv.SAVE_P_STACKS + $
               iv.SAVE_K_STACKS + iv.SAVE_V_STACKS

PROFILE_OUTPUT = iv.SAVE_Q_PROFILES + iv.SAVE_P_PROFILES + $
                 iv.SAVE_K_PROFILES + iv.SAVE_V_PROFILES

ANY_OUTPUT = GRID_OUTPUT + PIXEL_OUTPUT + STACK_OUTPUT + $
             PROFILE_OUTPUT

if (ANY_OUTPUT eq 0) then begin
    msg = [ $
    'ERROR:  No output files specified. ', ' ', $
    'You must specify one or more output files ', $
    'by clicking on an Output Vars button and ', $
    'checking boxes. ', ' ']
    result = GUI_Message(msg, /INFO, TITLE='No Output')
    OK = 0b
    RETURN
endif

;----------------------------------------
;Create a copy of current RTI file that
;has the run prefix of the new RTS files
;----------------------------------------
if (GRID_OUTPUT ne 0) then begin
    TF_Get_LUN, tunit, RTI_file
    openr, tunit, RTI_file 
    temp      = fstat(tunit)
    filesize  = temp.size
    all_chars = bytarr(filesize)
    readu, tunit, all_chars
    ;---------------------------
    TF_Get_LUN, tunit2, (rv.run_prefix + '.rti')
    openw, tunit2, (rv.run_prefix + '.rti') 
    writeu, tunit2, all_chars
    free_lun, tunit, tunit2
endif

;--------------------------
;Are outlet IDs in range ?
;-------------------------- 
if (PIXEL_OUTPUT ne 0) then begin
    ;---------------------------
    ;Get the outlet pixel IDs ?
    ;---------------------------
    outlet_IDs = *grid_vars.outlet_IDs
    n_outlets  = n_elements(outlet_IDs)
    outlet_ID  = outlet_IDs[0L]

    ;** print,'OUTLET_IDS = ', outlet_IDs  ;***************
    ;** print,'N_OUTLETS  = ', n_outlets
    ;** print,'OUTLET_ID  = ', outlet_ID

    n_pixels = (long(nx) * long(ny))
    out = where((outlet_IDs lt 0L) OR $
                (outlet_IDs gt (n_pixels-1L)), n_out)

    if (n_out ne 0) then begin
        msg = [ $
        'SORRY, ', ' ', $
        'One or more of the monitored pixel IDs ', $
        'are not in the range of valid values. ',$
        ' ',$
        'In RiverTools, you can use File > View Basin Info ',$
        'to get the outlet ID for the main basin. ',$
        ' ']
        result = GUI_Message(msg, /INFO, TITLE='Missing Input')
        OK = 0b
        RETURN
    endif 
endif

end;  Check_Output_Options
;************************************************************************
pro Open_New_RTS_Files, grid_vars, rv,cv,mv,sv,ev,iv,gv

;-------------------------------------------------------------------
;Notes:  (7/14/06)  Stopped using ASSOCIATED I/O for file creation
;        (just use WRITEU) and removed all "*_grids" args.  Added
;        the "rt3" file units for saving grid stacks.

;        (3/20/07) Updated to save unit numbers in structures.
;-------------------------------------------------------------------
RTI_file = grid_vars.RTI_file

;------------------------
;Prepare to save Q grids
;------------------------
if (cv.SAVE_Q_GRIDS) then begin
    Open_RTS_File, cv.Q_rts_file, Q_rts_unit, RTI_FILE=RTI_file, /WRITE
    cv.Q_rts_unit = Q_rts_unit
endif

;------------------------
;Prepare to save U grids
;------------------------
if (cv.SAVE_U_GRIDS) then begin
    Open_RTS_File, cv.u_rts_file, u_rts_unit, RTI_FILE=RTI_file, /WRITE
    cv.u_rts_unit = u_rts_unit
endif

;------------------------
;Prepare to save D grids
;------------------------
if (cv.SAVE_D_GRIDS) then begin
    Open_RTS_File, cv.d_rts_file, d_rts_unit, RTI_FILE=RTI_file, /WRITE
    cv.d_rts_unit = d_rts_unit
endif

;------------------------
;Prepare to save F grids
;------------------------
if (cv.SAVE_F_GRIDS) then begin
    Open_RTS_File, cv.f_rts_file, f_rts_unit, RTI_FILE=RTI_file, /WRITE
    cv.f_rts_unit = f_rts_unit
endif

;------------------------------------
;Prepare to save snow meltrate grids
;------------------------------------
if (sv.SAVE_MR_GRIDS) then begin
    Open_RTS_File, sv.mr_rts_file, mr_rts_unit, RTI_FILE=RTI_file, /WRITE
    sv.mr_rts_unit = mr_rts_unit
endif

;---------------------------------
;Prepare to save snow depth grids
;---------------------------------
if (sv.SAVE_HS_GRIDS) then begin
    Open_RTS_File, sv.hs_rts_file, hs_rts_unit, RTI_FILE=RTI_file, /WRITE
    sv.hs_rts_unit = hs_rts_unit
endif

;--------------------------------------------------
;Prepare to save snow water equivalent (swe) grids
;--------------------------------------------------
if (sv.SAVE_SW_GRIDS) then begin
    Open_RTS_File, sv.sw_rts_file, sw_rts_unit, RTI_FILE=RTI_file, /WRITE
    sv.sw_rts_unit = sw_rts_unit
endif

;-----------------------------------------
;Prepare to save cold content grids (Ecc)
;-----------------------------------------
if (sv.SAVE_CC_GRIDS) then begin
    Open_RTS_File, sv.cc_rts_file, cc_rts_unit, RTI_FILE=RTI_file, /WRITE
    sv.cc_rts_unit = cc_rts_unit
endif

;-------------------------------------------
;Prepare to save vapor pressure (air) grids
;-------------------------------------------
if (sv.SAVE_EA_GRIDS) then begin
    Open_RTS_File, sv.ea_rts_file, ea_rts_unit, RTI_FILE=RTI_file, /WRITE
    sv.ea_rts_unit = ea_rts_unit
endif

;--------------------------------------------
;Prepare to save vapor pressure (surf) grids
;--------------------------------------------
if (sv.SAVE_ES_GRIDS) then begin
    Open_RTS_File, sv.es_rts_file, es_rts_unit, RTI_FILE=RTI_file, /WRITE
    sv.es_rts_unit = es_rts_unit
endif

;------------------------------
;Prepare to save ET rate grids
;------------------------------
if (ev.SAVE_ER_GRIDS) then begin
    Open_RTS_File, ev.er_rts_file, er_rts_unit, RTI_FILE=RTI_file, /WRITE
    ev.er_rts_unit = er_rts_unit
endif

;------------------------------
;Prepare to save IN rate grids
;------------------------------
if (iv.SAVE_V0_GRIDS) then begin
    Open_RTS_File, iv.v0_rts_file, v0_rts_unit, RTI_FILE=RTI_file, /WRITE
    iv.v0_rts_unit = v0_rts_unit
endif

;-------------------------------------
;Prepare to save surf. moisture grids
;-------------------------------------
if (iv.SAVE_Q0_GRIDS) then begin
    Open_RTS_File, iv.q0_rts_file, q0_rts_unit, RTI_FILE=RTI_file, /WRITE
    iv.q0_rts_unit = q0_rts_unit
endif

;----------------------------------------
;Prepare to save tot. infil. depth grids
;----------------------------------------
if (iv.SAVE_I_GRIDS) then begin
    Open_RTS_File, iv.I_rts_file, I_rts_unit, RTI_FILE=RTI_file, /WRITE
    iv.I_rts_unit = I_rts_unit 
endif

;---------------------------------------
;Prepare to save wet. front depth grids
;---------------------------------------
if (iv.SAVE_ZW_GRIDS) then begin
    Open_RTS_File, iv.Zw_rts_file, Zw_rts_unit, RTI_FILE=RTI_file, /WRITE
    iv.Zw_rts_unit = Zw_rts_unit 
endif

;----------------------------------
;Prepare to save water table grids
;----------------------------------
if (gv.SAVE_HT_GRIDS) then begin
    Open_RTS_File, gv.ht_rts_file, ht_rts_unit, RTI_FILE=RTI_file, /WRITE
    gv.ht_rts_unit = ht_rts_unit
endif

;--------------------------------------
;Prepare to save depth of freeze grids
;--------------------------------------
if (gv.SAVE_DF_GRIDS) then begin
    Open_RTS_File, gv.df_rts_file, df_rts_unit, RTI_FILE=RTI_file, /WRITE
    gv.df_rts_unit = df_rts_unit 
endif

;------------------------------------
;Prepare to save depth of thaw grids
;------------------------------------
if (gv.SAVE_DT_GRIDS) then begin
    Open_RTS_File, gv.dt_rts_file, dt_rts_unit, RTI_FILE=RTI_file, /WRITE
    gv.dt_rts_unit = dt_rts_unit 
endif

;-------------------------------------
;Prepare to save soil moisture stacks
;-------------------------------------
if (iv.SAVE_Q_STACKS) then begin
    Open_RT3_File, iv.q_stack_file, q_stack_unit, iv.nz, $
                   RTI_FILE=RTI_file, /WRITE
    iv.q_stack_unit = q_stack_unit
endif

;-------------------------------------
;Prepare to save pressure head stacks
;-------------------------------------
if (iv.SAVE_P_STACKS) then begin
    Open_RT3_File, iv.p_stack_file, p_stack_unit, iv.nz, $
                   RTI_FILE=RTI_file, /WRITE
    iv.p_stack_unit = p_stack_unit
endif

;---------------------------------------
;Prepare to save hydraulic cond. stacks
;---------------------------------------
if (iv.SAVE_K_STACKS) then begin
    Open_RT3_File, iv.K_stack_file, K_stack_unit, iv.nz, $
                   RTI_FILE=RTI_file, /WRITE
    iv.K_stack_unit = K_stack_unit
endif

;------------------------------------------
;Prepare to save vertical flow rate stacks
;------------------------------------------
if (iv.SAVE_V_STACKS) then begin
    Open_RT3_File, iv.v_stack_file, v_stack_unit, iv.nz, $
                   RTI_FILE=RTI_file, /WRITE
    iv.v_stack_unit = v_stack_unit
endif

;---------------------------------------
;Prepare to save soil moisture profiles
;---------------------------------------
if (iv.SAVE_Q_PROFILES) then begin
    TF_Get_LUN, q_profile_unit, iv.q_profile_file
    iv.q_profile_unit = q_profile_unit
    openw, q_profile_unit, iv.q_profile_file
endif

;---------------------------------------
;Prepare to save pressure head profiles
;---------------------------------------
if (iv.SAVE_P_PROFILES) then begin
    TF_Get_LUN, p_profile_unit, iv.p_profile_file
    iv.p_profile_unit = p_profile_unit
    openw, p_profile_unit, iv.p_profile_file
endif

;-----------------------------------------
;Prepare to save hydraulic cond. profiles
;-----------------------------------------
if (iv.SAVE_K_PROFILES) then begin
    TF_Get_LUN, K_profile_unit, iv.K_profile_file
    iv.K_profile_unit = K_profile_unit
    openw, K_profile_unit, iv.K_profile_file
endif

;--------------------------------------------
;Prepare to save vertical flow rate profiles
;--------------------------------------------
if (iv.SAVE_V_PROFILES) then begin
    TF_Get_LUN, v_profile_unit, iv.v_profile_file
    iv.v_profile_unit = v_profile_unit
    openw, v_profile_unit, iv.v_profile_file
endif

end;  Open_New_RTS_Files
;************************************************************************
pro Write_Pixel_File_Header, unit, outlet_IDs, VARNAME=VARNAME

;----------------------------------------------------
;Notes:  This is currently hardwired to have columns
;        that are each 15 characters wide.
;----------------------------------------------------
on_error, 2    ;(return to caller)

;-------------------
;Create the heading
;-------------------
n_outlets = n_elements(outlet_IDs)
if NOT(keyword_set(VARNAME)) then varname = 'F'
ostr = varname + '[' + TF_String(outlet_IDs) + ']'
tstr = 'Time [min]'
f0   = '(' + TF_String(n_outlets + 1) + 'A15)'

printf, unit, tstr, ostr, format=f0

;----------------------------------
;Draw a horizontal line of hyphens
;----------------------------------
width = (n_outlets + 1) * 15
hline = replicate(string(45b), width)
f1 = '(' + TF_String(width) + 'A)'
printf, unit, hline, format=f1

end;  Write_Pixel_File_Header
;************************************************************************
pro Open_New_Pixel_Files, rv,cv,mv,sv,ev,iv,gv, outlet_IDs

;--------------------------------------------
;Notes:  (3/20/07) Updated to save the file
;        unit numbers in structures.
;--------------------------------------------

;------------------------
;Prepare to save Q values
;------------------------
if (cv.SAVE_Q_PIXELS) then begin
    TF_Get_LUN, Q_out_unit, cv.Q_out_file
    cv.Q_out_unit = Q_out_unit
    openw, Q_out_unit, cv.Q_out_file
    Write_Pixel_File_Header, Q_out_unit, outlet_IDs, VARNAME='Q'
endif

;------------------------
;Prepare to save U values
;------------------------
if (cv.SAVE_U_PIXELS) then begin
    TF_Get_LUN, u_out_unit, cv.u_out_file
    cv.u_out_unit = u_out_unit
    openw, u_out_unit, cv.u_out_file
    Write_Pixel_File_Header, u_out_unit, outlet_IDs, VARNAME='u'
endif

;------------------------
;Prepare to save D values
;------------------------
if (cv.SAVE_D_PIXELS) then begin
    TF_Get_LUN, d_out_unit, cv.d_out_file
    cv.d_out_unit = d_out_unit
    openw, d_out_unit, cv.d_out_file
    Write_Pixel_File_Header, d_out_unit, outlet_IDs, VARNAME='d'
endif

;------------------------
;Prepare to save F values
;------------------------
if (cv.SAVE_F_PIXELS) then begin
    TF_Get_LUN, f_out_unit, cv.f_out_file
    cv.f_out_unit = f_out_unit
    openw, f_out_unit, cv.f_out_file
    Write_Pixel_File_Header, f_out_unit, outlet_IDs, VARNAME='f'
endif

;------------------------------------
;Prepare to save snow meltrate values
;------------------------------------
if (sv.SAVE_MR_PIXELS) then begin
    TF_Get_LUN, mr_out_unit, sv.mr_out_file
    sv.mr_out_unit = mr_out_unit
    openw, mr_out_unit, sv.mr_out_file
    Write_Pixel_File_Header, mr_out_unit, outlet_IDs, VARNAME='M'
endif

;---------------------------------
;Prepare to save snow depth values
;---------------------------------
if (sv.SAVE_HS_PIXELS) then begin
    TF_Get_LUN, hs_out_unit, sv.hs_out_file
    sv.hs_out_unit = hs_out_unit
    openw, hs_out_unit, sv.hs_out_file
    Write_Pixel_File_Header, hs_out_unit, outlet_IDs, VARNAME='h'
endif

;---------------------------------------------------
;Prepare to save snow water equivalent (swe) values
;---------------------------------------------------
if (sv.SAVE_SW_PIXELS) then begin
    TF_Get_LUN, sw_out_unit, sv.sw_out_file
    sv.sw_out_unit = sw_out_unit
    openw, sw_out_unit, sv.sw_out_file
    Write_Pixel_File_Header, sw_out_unit, outlet_IDs, VARNAME='swe'
endif

;------------------------------------------
;Prepare to save cold content values (Ecc)
;------------------------------------------
if (sv.SAVE_CC_PIXELS) then begin
    TF_Get_LUN, cc_out_unit, sv.cc_out_file
    sv.cc_out_unit = cc_out_unit
    openw, cc_out_unit, sv.cc_out_file
    Write_Pixel_File_Header, cc_out_unit, outlet_IDs, VARNAME='Ecc'
endif

;--------------------------------------------
;Prepare to save vapor pressure (air) values
;--------------------------------------------
if (sv.SAVE_EA_PIXELS) then begin
    TF_Get_LUN, ea_out_unit, sv.ea_out_file
    sv.ea_out_unit = ea_out_unit
    openw, ea_out_unit, sv.ea_out_file
    Write_Pixel_File_Header, ea_out_unit, outlet_IDs, VARNAME='ea'
endif

;---------------------------------------------
;Prepare to save vapor pressure (surf) values
;---------------------------------------------
if (sv.SAVE_ES_PIXELS) then begin
    TF_Get_LUN, es_out_unit, sv.es_out_file
    sv.es_out_unit = es_out_unit
    openw, es_out_unit, sv.es_out_file
    Write_Pixel_File_Header, es_out_unit, outlet_IDs, VARNAME='es'
endif

;------------------------------
;Prepare to save ET rate values
;------------------------------
if (ev.SAVE_ER_PIXELS) then begin
    TF_Get_LUN, er_out_unit, ev.er_out_file
    ev.er_out_unit = er_out_unit
    openw, er_out_unit, ev.er_out_file
    Write_Pixel_File_Header, er_out_unit, outlet_IDs, VARNAME='ET'
endif

;------------------------------
;Prepare to save IN rate values
;------------------------------
if (iv.SAVE_V0_PIXELS) then begin
    TF_Get_LUN, v0_out_unit, iv.v0_out_file
    iv.v0_out_unit = v0_out_unit
    openw, v0_out_unit, iv.v0_out_file
    Write_Pixel_File_Header, v0_out_unit, outlet_IDs, VARNAME='IN'
endif

;-------------------------------------
;Prepare to save surf. moisture values
;-------------------------------------
if (iv.SAVE_Q0_PIXELS) then begin
    TF_Get_LUN, q0_out_unit, iv.q0_out_file
    iv.q0_out_unit = q0_out_unit
    openw, q0_out_unit, iv.q0_out_file
    Write_Pixel_File_Header, q0_out_unit, outlet_IDs, VARNAME='q0'
endif

;----------------------------------------
;Prepare to save tot. infil. depth values
;----------------------------------------
if (iv.SAVE_I_PIXELS) then begin
    TF_Get_LUN, I_out_unit, iv.I_out_file
    iv.I_out_unit = I_out_unit
    openw, I_out_unit, iv.I_out_file
    Write_Pixel_File_Header, I_out_unit, outlet_IDs, VARNAME='I'
endif

;---------------------------------------
;Prepare to save wet. front depth values
;---------------------------------------
if (iv.SAVE_ZW_PIXELS) then begin
    TF_Get_LUN, Zw_out_unit, iv.Zw_out_file
    iv.Zw_out_unit = Zw_out_unit
    openw, Zw_out_unit, iv.Zw_out_file
    Write_Pixel_File_Header, Zw_out_unit, outlet_IDs, VARNAME='Zw' 
endif

;----------------------------------
;Prepare to save water table values
;----------------------------------
if (gv.SAVE_HT_PIXELS) then begin
    TF_Get_LUN, ht_out_unit, gv.ht_out_file
    gv.ht_out_unit = ht_out_unit
    openw, ht_out_unit, gv.ht_out_file
    Write_Pixel_File_Header, ht_out_unit, outlet_IDs, VARNAME='ht'
endif

;--------------------------------------
;Prepare to save depth of freeze values
;--------------------------------------
if (gv.SAVE_DF_PIXELS) then begin
    TF_Get_LUN, df_out_unit, gv.df_out_file
    gv.df_out_unit = df_out_unit
    openw, df_out_unit, gv.df_out_file
    Write_Pixel_File_Header, df_out_unit, outlet_IDs, VARNAME='df'
endif

;------------------------------------
;Prepare to save depth of thaw values
;------------------------------------
if (gv.SAVE_DT_PIXELS) then begin
    TF_Get_LUN, dt_out_unit, gv.dt_out_file
    gv.dt_out_unit = dt_out_unit
    openw, dt_out_unit, gv.dt_out_file
    Write_Pixel_File_Header, dt_out_unit, outlet_IDs, VARNAME='dt'
endif

end;  Open_New_Pixel_Files
;************************************************************************
pro Save_Pixel_Values, n, time, outlet_IDs, pformat, $
                       Q, u, d, f, SM, ET, IN, q0, $
                       ;---------------------------------------------
                       cv, mv, sv, ev, iv, gv, INFIL_ALL_SCALARS, $
                       ;---------------------------------------------
                       chan_pixel_samp_step, snow_pixel_samp_step, $
                       et_pixel_samp_step, in_pixel_samp_step, $
                       gw_pixel_samp_step

FORWARD_FUNCTION Pixel_Var
pf = pformat

;-----------------------------------------------
;Save a subsequence of channel var pixel values
;------------------------------------------------
if ((n mod chan_pixel_samp_step) eq 0) then begin
    if (cv.SAVE_Q_PIXELS) then $
        printf, cv.Q_out_unit, time, Pixel_Var(Q,outlet_IDs), format=pf
    if (cv.SAVE_U_PIXELS) then $
        printf, cv.u_out_unit, time, Pixel_Var(u,outlet_IDs), format=pf
    if (cv.SAVE_D_PIXELS) then $
        printf, cv.d_out_unit, time, Pixel_Var(d,outlet_IDs), format=pf
    if (cv.SAVE_F_PIXELS) then $
        printf, cv.f_out_unit, time, Pixel_Var(f,outlet_IDs), format=pf
endif

;--------------------------------------------
;Save a subsequence of snow var pixel values
;--------------------------------------------
if ((n mod snow_pixel_samp_step) eq 0) then begin
    if (sv.SAVE_MR_PIXELS) then $
        printf, sv.mr_out_unit, time, Pixel_Var(SM, outlet_IDs), format=pf
    if (sv.SAVE_HS_PIXELS) then $
        printf, sv.hs_out_unit, time, Pixel_Var(*sv.h_snow, outlet_IDs), $
                format=pf
    if (sv.SAVE_SW_PIXELS) then $
        printf, sv.sw_out_unit, time, Pixel_Var(*sv.h_swe, outlet_IDs), format=pf
    if (sv.SAVE_CC_PIXELS) then $
        printf, sv.cc_out_unit, time, Pixel_Var(*sv.Ecc, outlet_IDs), format=pf
    if (sv.SAVE_EA_PIXELS) then $
        printf, sv.ea_out_unit, time, Pixel_Var(*mv.e_air, outlet_IDs), format=pf
    if (sv.SAVE_ES_PIXELS) then $
        printf, sv.es_out_unit, time, Pixel_Var(*mv.e_surf, outlet_IDs), format=pf
endif

;------------------------------------------
;Save a subsequence of ET var pixel values
;Should ET be renamed to ER here ????
;------------------------------------------
if ((n mod et_pixel_samp_step) eq 0) then begin
    if (ev.SAVE_ER_PIXELS) then $
        printf, ev.er_out_unit, time, Pixel_Var(ET, outlet_IDs), format=pf
endif

;------------------------------------------
;Save a subsequence of IN var pixel values
;------------------------------------------
if ((n mod in_pixel_samp_step) eq 0) then begin
    if (iv.SAVE_V0_PIXELS) then $
        printf, iv.v0_out_unit, time, Pixel_Var(IN, outlet_IDs), format=pf
    if (iv.SAVE_I_PIXELS) then $
        printf, iv.I_out_unit, time, Pixel_Var(*iv.I, outlet_IDs), format=pf
    ;--------------------------------------
    ;Richards' equation for infiltration ?
    ;--------------------------------------
    if (iv.method eq 4b) then begin
        if (INFIL_ALL_SCALARS) then begin
            if (iv.SAVE_Q0_PIXELS) then $
                printf, iv.q0_out_unit, time, q0, format=pf
            if (iv.SAVE_ZW_PIXELS) then $
                printf, iv.Zw_out_unit, time, *iv.Zw, format=pf
        endif else begin
            if (iv.SAVE_Q0_PIXELS) then $
                printf, iv.q0_out_unit, time, Pixel_Var(q0, outlet_IDs), $
                        format=pf
            if (iv.SAVE_ZW_PIXELS) then $
                printf, iv.Zw_out_unit, time, Pixel_Var(*iv.Zw, outlet_IDs), $
                        format=pf
        endelse
    endif
endif

;------------------------------------------
;Save a subsequence of GW var pixel values
;------------------------------------------
if ((n mod gw_pixel_samp_step) eq 0) then begin
    if (gv.SAVE_HT_PIXELS) then $
        printf, gv.ht_out_unit, time, Pixel_Var(*gv.h_table, outlet_IDs), $
                format=pf
    if (gv.SAVE_DF_PIXELS) then $
        printf, gv.df_out_unit, time, Pixel_Var(*gv.d_freeze, outlet_IDs), $
                format=pf
    if (gv.SAVE_DT_PIXELS) then $
        printf, gv.dt_out_unit, time, Pixel_Var(*gv.d_thaw, outlet_IDs), $
                format=pf
endif

end;  Save_Pixel_Values
;************************************************************************
pro Save_Grid_Values, n, time, outlet_IDs, nx, ny, $
                      cv, mv, sv, ev, iv, gv, $
                      Q, u, d, f, SM, ET, IN, q0, $
                      ;-------------------------------------------
                      chan_grid_samp_step, snow_grid_samp_step, $
                      et_grid_samp_step, in_grid_samp_step, $
                      gw_grid_samp_step, $
                      ;-------------------------------------------
                      in_stack_samp_step, in_profile_samp_step 

;----------------------------------------------------------
;Notes:  The RTG function will work whether argument is a
;        scalar or already a 2D grid.

;        The Stack function will work whether argument is
;        a 1D profile or already a 3D array.
;        The Profile_Var function will work whether its
;        argument is a 1D profile or a 3D array.  (It is
;        called by Write_Profile.)
;----------------------------------------------------------
FORWARD_FUNCTION RTG, Stack

;----------------------------------------
;Save a subsequence of channel var grids
;----------------------------------------
if ((n mod chan_grid_samp_step) eq 0) then begin
    if (cv.SAVE_Q_GRIDS) then writeu, cv.Q_rts_unit, RTG(Q)
    if (cv.SAVE_U_GRIDS) then writeu, cv.u_rts_unit, RTG(u)
    if (cv.SAVE_D_GRIDS) then writeu, cv.d_rts_unit, RTG(d)
    if (cv.SAVE_F_GRIDS) then writeu, cv.f_rts_unit, RTG(f)
endif

;-------------------------------------
;Save a subsequence of snow var grids
;-------------------------------------
if ((n mod snow_grid_samp_step) eq 0) then begin
    if (sv.SAVE_MR_GRIDS) then writeu, sv.mr_rts_unit, RTG(SM, nx,ny)
    if (sv.SAVE_HS_GRIDS) then writeu, sv.hs_rts_unit, RTG(*sv.h_snow, nx,ny)
    if (sv.SAVE_SW_GRIDS) then writeu, sv.sw_rts_unit, RTG(*sv.h_swe, nx,ny)
    if (sv.SAVE_CC_GRIDS) then writeu, sv.cc_rts_unit, RTG(*sv.Ecc, nx,ny)
    if (sv.SAVE_EA_GRIDS) then writeu, sv.ea_rts_unit, RTG(*mv.e_air, nx,ny)
    if (sv.SAVE_ES_GRIDS) then writeu, sv.es_rts_unit, RTG(*mv.e_surf, nx,ny)
endif

;-----------------------------------
;Save a subsequence of ET var grids
;-----------------------------------
if ((n mod et_grid_samp_step) eq 0) then begin
    if (ev.SAVE_ER_GRIDS) then writeu, ev.er_rts_unit, RTG(ET, nx,ny)
endif

;-----------------------------------
;Save a subsequence of IN var grids
;-----------------------------------
if ((n mod in_grid_samp_step) eq 0) then begin
    if (iv.SAVE_V0_GRIDS) then writeu, iv.v0_rts_unit, RTG(IN, nx,ny)
    if (iv.SAVE_Q0_GRIDS) then writeu, iv.q0_rts_unit, RTG(q0, nx,ny)
    if (iv.SAVE_I_GRIDS)  then writeu, iv.I_rts_unit,  RTG(*iv.I, nx,ny)
    if (iv.SAVE_ZW_GRIDS) then writeu, iv.Zw_rts_unit, RTG(*iv.Zw, nx,ny)
endif

;-----------------------------------
;Save a subsequence of GW var grids
;-----------------------------------
if ((n mod gw_grid_samp_step) eq 0) then begin
    if (gv.SAVE_HT_GRIDS) then writeu, gv.ht_rts_unit, RTG(*gv.h_table, nx,ny)
    if (gv.SAVE_DF_GRIDS) then writeu, gv.df_rts_unit, RTG(*gv.d_freeze, nx,ny)
    if (gv.SAVE_DT_GRIDS) then writeu, gv.dt_rts_unit, RTG(*gv.d_thaw, nx,ny)
endif

;-------------------------------------
;Save a subsequence of IN var stacks
;-------------------------------------
if ((n mod in_stack_samp_step) eq 0) then begin
    if (iv.SAVE_Q_STACKS) then writeu, iv.q_stack_unit, Stack(iv.q,nx,ny)
    if (iv.SAVE_P_STACKS) then writeu, iv.p_stack_unit, Stack(iv.p,nx,ny)
    if (iv.SAVE_K_STACKS) then writeu, iv.K_stack_unit, Stack(iv.K,nx,ny)
    if (iv.SAVE_V_STACKS) then writeu, iv.v_stack_unit, Stack(iv.v,nx,ny)
endif

;--------------------------------------
;Save a subsequence of IN var profiles
;--------------------------------------
if ((n mod in_profile_samp_step) eq 0) then begin
    tmstr = '***********  Time = '
    tmstr = tmstr + TF_String(time, FORMAT='(F8.1)')
    tmstr = tmstr + '  [minutes]'
    nz = iv.nz
    ;------------------------------
    if (iv.SAVE_Q_PROFILES) then $
        Write_Profile, iv.q_profile_unit, iv.q, outlet_IDs, nz, tmstr
     if (iv.SAVE_P_PROFILES) then $
        Write_Profile, iv.p_profile_unit, iv.p, outlet_IDs, nz, tmstr
    if (iv.SAVE_K_PROFILES) then $
        Write_Profile, iv.K_profile_unit, iv.K, outlet_IDs, nz, tmstr
    if (iv.SAVE_V_PROFILES) then $
        Write_Profile, iv.v_profile_unit, iv.v, outlet_IDs, nz, tmstr
endif

end;  Save_Grid_Values
;************************************************************************
pro Write_Profile, unit, ptr, outlet_IDs, nz, tmstr

;------------------------------------------------------
;If (*ptr) is a 1D profile, and therefore the same
;for all outlet_IDs, then the Profile_Var function
;now returns a 1D array.  Otherwise, it returns a
;2D array, with a separate profile for each outlet_ID.
;------------------------------------------------------
FORWARD_FUNCTION Profile_Var

var = Profile_Var(ptr, outlet_IDs)
dim = size(var, /dimensions)
nd  = n_elements(dim)
n   = n_elements(outlet_IDs)  ;(needed below)

;--------------
;For debugging
;--------------
;print,'nz = ', nz
;print,'n  = ', n
;print,'size(var) = ', size(var)

printf, unit, tmstr

if (nd eq 1) then begin
    f = '(F18.10)'
    for j=0L,(nz-1) do printf, unit, var[j], format=f
endif else begin
    f = '(' + TF_String(n) + 'F18.10)'
    for j=0L,(nz-1) do printf, unit, var[*,j], format=f
endelse

end;  Write_Profile
;************************************************************************
pro Close_All_Output_Files, cv,sv,ev,iv,gv

;----------------------
;Print an info message
;----------------------
TF_Print,'Closing all output files...'
TF_Print,' '
;----------------------------------------------------
if (cv.SAVE_Q_PIXELS)  then free_lun, cv.Q_out_unit
if (cv.SAVE_U_PIXELS)  then free_lun, cv.u_out_unit
if (cv.SAVE_D_PIXELS)  then free_lun, cv.d_out_unit
if (cv.SAVE_F_PIXELS)  then free_lun, cv.f_out_unit
;----------------------------------------------------
if (sv.SAVE_MR_PIXELS) then free_lun, sv.mr_out_unit
if (sv.SAVE_HS_PIXELS) then free_lun, sv.hs_out_unit
if (sv.SAVE_SW_PIXELS) then free_lun, sv.sw_out_unit
if (sv.SAVE_CC_PIXELS) then free_lun, sv.cc_out_unit
if (sv.SAVE_EA_PIXELS) then free_lun, sv.ea_out_unit
if (sv.SAVE_ES_PIXELS) then free_lun, sv.es_out_unit
;----------------------------------------------------
if (ev.SAVE_ER_PIXELS) then free_lun, ev.er_out_unit
;----------------------------------------------------
if (iv.SAVE_V0_PIXELS) then free_lun, iv.v0_out_unit
if (iv.SAVE_Q0_PIXELS) then free_lun, iv.q0_out_unit
if (iv.SAVE_I_PIXELS)  then free_lun, iv.I_out_unit
if (iv.SAVE_Zw_PIXELS) then free_lun, iv.Zw_out_unit 
;----------------------------------------------------
if (gv.SAVE_HT_PIXELS) then free_lun, gv.ht_out_unit
if (gv.SAVE_DF_PIXELS) then free_lun, gv.df_out_unit
if (gv.SAVE_DT_PIXELS) then free_lun, gv.dt_out_unit
;----------------------------------------------------
;----------------------------------------------------
if (cv.SAVE_Q_GRIDS)  then free_lun, cv.Q_rts_unit
if (cv.SAVE_U_GRIDS)  then free_lun, cv.U_rts_unit
if (cv.SAVE_D_GRIDS)  then free_lun, cv.D_rts_unit
if (cv.SAVE_F_GRIDS)  then free_lun, cv.F_rts_unit
;---------------------------------------------------
if (sv.SAVE_MR_GRIDS) then free_lun, sv.mr_rts_unit
if (sv.SAVE_HS_GRIDS) then free_lun, sv.hs_rts_unit
if (sv.SAVE_SW_GRIDS) then free_lun, sv.sw_rts_unit
if (sv.SAVE_CC_GRIDS) then free_lun, sv.cc_rts_unit
if (sv.SAVE_EA_GRIDS) then free_lun, sv.ea_rts_unit
if (sv.SAVE_ES_GRIDS) then free_lun, sv.es_rts_unit
;---------------------------------------------------
if (ev.SAVE_ER_GRIDS) then free_lun, ev.er_rts_unit
;---------------------------------------------------
if (iv.SAVE_V0_GRIDS) then free_lun, iv.v0_rts_unit
if (iv.SAVE_Q0_GRIDS) then free_lun, iv.q0_rts_unit
if (iv.SAVE_I_GRIDS)  then free_lun, iv.I_rts_unit
if (iv.SAVE_Zw_GRIDS) then free_lun, iv.Zw_rts_unit 
;------------------------------------------------
if (gv.SAVE_HT_GRIDS) then free_lun, gv.ht_rts_unit
if (gv.SAVE_DF_GRIDS) then free_lun, gv.df_rts_unit
if (gv.SAVE_DT_GRIDS) then free_lun, gv.dt_rts_unit
;----------------------------------------------------
if (iv.SAVE_Q_STACKS) then free_lun, iv.q_stack_unit
if (iv.SAVE_P_STACKS) then free_lun, iv.p_stack_unit
if (iv.SAVE_K_STACKS) then free_lun, iv.K_stack_unit
if (iv.SAVE_V_STACKS) then free_lun, iv.v_stack_unit
;--------------------------------------------------------
if (iv.SAVE_Q_PROFILES) then free_lun, iv.q_profile_unit
if (iv.SAVE_P_PROFILES) then free_lun, iv.p_profile_unit
if (iv.SAVE_K_PROFILES) then free_lun, iv.K_profile_unit
if (iv.SAVE_V_PROFILES) then free_lun, iv.v_profile_unit

end;  Close_All_Output_Files
;************************************************************************




