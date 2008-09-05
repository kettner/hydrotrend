
;***************************************************************
;   formulas.pro

;   Copyright (c) 2001-2008,  Scott D. Peckham
;   Created:   Oct 2001 - Jan 2002  
;   Modified:  Feb 2004, July 2005
;   Modified:  Jan 2006, April 2006  (Richards eqn routines)
;   Modified:  May 2006  (All infiltration routines)
;   Modified:  Mar 2007  (vapor pressure treatment, e_air)
;   Modified:  Mar 2008  (added Z_Derivative_1D)

;***************************************************************

;   Richardson_Number          (function)
;   Bulk_Exchange_Coeff        (function)  Neutral, Stable, Unstable
;   Sensible_Heat_Flux         (function)
;   Saturation_Vapor_Pressure  (function)  (3/13/07, from Qnet_file.pro)
;   Vapor_Pressure             (function)  (3/13/07, from Qnet_file.pro)
;   Latent_Heat_Flux           (function)
;   Conduction_Heat_Flux       (function)
;   Advection_Heat_Flux        (function)
;   Initial_Cold_Content       (function)

;   Max_Meltrate               (function)   (for snow)
;   Degree_Day_Meltrate        (function)
;   Energy_Balance_Meltrate    (function)

;   Priestley_Taylor_ET_Rate   (function)
;   Energy_Balance_ET_Rate     (function)

;   Infil_Is_1D                   (function)   (may now be OBSOLETE, 3/19/07)
;   Check_Low_Rainrate            (procedure)
;---------------------------------------------
;   Green_Ampt_Infil_Rate_v1      (function)
;   Green_Ampt_Infil_Rate_1D      (function)
;   Green_Ampt_Infil_Rate_3D      (function)
;   Green_Ampt_Infil_Rate_v2      (function)
;---------------------------------------------
;   Smith_Parlange_Infil_Rate_v1  (function)
;   Smith_Parlange_Infil_Rate_1D
;   Smith_Parlange_Infil_Rate_3D
;   Smith_Parlange_Infil_Rate_v2  (function)
;---------------------------------------------
;   Beven_Exp_K_Infil_Rate_v1     (function)    (May 2006)
;   Beven_Exp_K_Infil_Rate_1D     (function)    (Not written)
;   Beven_Exp_K_Infil_Rate_3D     (function)    (Not written)    
;   Beven_Exp_K_Infil_Rate_v2     (function)    (Not written)
;---------------------------------------------
;   Theta_Field                (function)       (Mar 2008)
;   Theta_Wilting              (function)       (Mar 2008)
;   Theta_Min                  (function)
;   Theta_Residual             (function)       (Mar 2008)
;   Theta_Max                  (function)

;   Z_Derivative_1D            (function)       (Mar 2008)
;   Z_Derivative_3D            (function)       (Mar 2007)
;   Update_Richards_Theta      (procedure)
;   Update_Richards_Psi        (procedure)
;   Update_Richards_K          (procedure)
;   Update_Richards_V          (procedure)
;   Update_Richards_Zw         (procedure)
;   Richards_Infil_Rate        (function)

;   Wetted_Thicknesses         (function)
;   Darcy_Flow                 (function)  (no pointers yet?)
;   Total_Darcy_Layer_Flow     (function)
;   Total_Darcy_Layer_Flow_VK  (function)  (Bob's variable K version)
;   Total_Subsurface_Flow      (function)
;   Darcy_Layer_Seep_Rate      (function)  (IS THIS OBSOLETE ?)

;   Trapezoid_R                (function)
;   Manning_Formula            (function)
;   Law_of_the_Wall            (function)
;   Free_Surface_Slope         (function)  (no pointers yet?)
;   Kinematic_Velocity         (function)  (updated: 7/13/05)

;   Overland_Flow              (function)  (not used anywhere)

;   Hydrograph                 (function)
;   Rain_Volume                (function, 10/16/01)
;   Hydrograph_Integral        (function)

;*******************************************************************
function Richardson_Number, z, uz, T_air, T_surf

;---------------------------------------------------------------
;Notes:  Other definitions are possible, such as the one given
;        by Dingman (2002, p. 599).  However, this one is the
;        one given by Zhang et al. (2000) and is meant for use
;        with the stability criterion also given there.
;---------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

top = (9.81d * (*z) * (*T_air - *T_surf))    ;(g = 9.81 [m/s^2])
bot = (*uz)^2d * (*T_air + 273.15d)
Ri  = (top / bot)

RETURN, Ri
END;    Richardson_Number
;*******************************************************************
function Bulk_Exchange_Coeff, uz, z, h_snow, z0_air, T_air, T_surf

;----------------------------------------------------------------
;Notes:  Dn       = bulk exchange coeff for the conditions of
;                   neutral atmospheric stability [m/s]
;        Dh       = bulk exchange coeff for heat  [m/s]
;        De       = bulk exchange coeff for vapor [m/s]
;        h_snow   = snow depth [m]
;        z0_air   = surface roughness length scale [m]
;                   (includes vegetation not covered by snow)
;        z        = height that has wind speed uz [m]
;        uz       = wind speed at height z [m/s]
;        kappa    = 0.41 = von Karman's constant [unitless]
;        RI       = Richardson's number (see function)
;----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;--------------------------------------------------
;Compute bulk exchange coeffs (neutral stability)
;--------------------------------------------------
Dn = (*uz) * (0.41d / alog((*z - *h_snow) / (*z0_air)) )^2d

;---------------------------------------------
;NB! Dn could be a scalar or a grid, so this
;    must be written to handle both cases.
;    Note that WHERE can be used on a scalar:

;    IDL> a = 1
;    IDL> print, size(a)
;    IDL> w = where(a ge 1, nw)
;    IDL> print, nw
;    IDL> a[w] = 2
;    IDL> print, a
;    IDL> print, size(a)
;---------------------------------------------
w = where(*T_air ne *T_surf, nw)

if (nw eq 0) then RETURN, Dn

;-----------------------------------
;One or more pixels are not neutral
;so make a correction using RI
;-------------------------------------------
;NB!  RI could be a grid when Dn is a
;scalar, and this will change Dn to a grid.
;-------------------------------------------
RI = Richardson_Number(z, uz, T_air, T_surf)

;-------------------------------------------
;Before 12/21/07.  Has bug if RI is a grid
;-------------------------------------------
;w_stable = where(*T_air gt *T_surf, n_stable)
;if (n_stable ne 0) then begin
;    Dn[w_stable] = Dn[w_stable]/(1d + (10d * RI))
;endif
;w_unstable = where(*T_air lt *T_surf, n_unstable)
;if (n_unstable ne 0) then begin
   ;----------------------------------------------
   ;Multiplication and substraction vs. opposites
   ;for the stable case.  Zhang et al. (2000)
   ;Hopefully not just a typo.
   ;----------------------------------------------
;   Dn[w_unstable] = Dn[w_unstable]*(1d - (10d * RI))
;endif

;----------------
;After 12/21/07
;-----------------------------------------------------------
;If T_air, T_surf or uz is a grid, then RI will be a grid.
;This version makes only one call to WHERE, so its faster.
;-----------------------------------------------------------
;Multiplication and substraction vs. opposites for the
;stable case (Zhang et al., 2000); hopefully not a typo.
;It plots as a smooth curve through RI=0.
;-----------------------------------------------------------
nD = n_elements(Dn)
nR = n_elements(RI)
if (nR gt 1) then begin
    ;-------------------------
    ;Case where RI is a grid
    ;-------------------------
    ws = where(RI gt 0, ns, COMP=wu, NCOMP=nu)
    if (nD eq 1) then begin
       ;****************************************
       ;Convert Dn to a grid here or somewhere
       ;Should stop with an error message
       ;****************************************
    endif
    if (ns ne 0) then Dn[ws] = Dn[ws] / (1d + (10d * RI[ws]))
    if (nu ne 0) then Dn[wu] = Dn[wu] * (1d - (10d * RI[wu]))
endif else begin
    ;--------------------------
    ;Case where RI is a scalar
    ;------------------------------
    ;Works if Dn is grid or scalar
    ;------------------------------
    if (RI gt 0) then Dn = Dn / (1d + (10d * RI)) $
                 else Dn = Dn * (1d - (10d * RI))
endelse

RETURN, Dn

END;    Bulk_Exchange_Coeff
;*******************************************************************
function Sensible_Heat_Flux, rho_air, Cp_air, Dh, T_air, T_surf

;------------------------------------------------------
;Notes: All the Q's have units of W/m^2 = J/(m^2 s).
;       Dh is returned by Bulk_Exchange_Coeff function
;       and is not a pointer.
;------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;-------------------
;Physical constants
;-------------------
;rho_air = 1.225d   ;[kg/m^3, at sea-level]
;Cp_air  = 1005.7   ;[J/kg/deg_C]

;---------------------------
;Compute sensible heat flux
;Note: Dh is not a pointer
;---------------------------
Qh = (*rho_air * (*Cp_air)) * Dh * (*T_air - *T_surf)

RETURN, Qh
END;    Sensible_Heat_Flux
;***************************************************************
function Saturation_Vapor_Pressure, T, MBAR=MBAR, $
                                    SATTERLUND=SATTERLUND

;----------------------------------------------------------------
;Notes:  Saturation vapor pressure is a function of temperature.
;        T is temperature in Celsius.  By default, the method
;        of Brutsaert (1975) is used.  However, the SATTERLUND
;        keyword is set then the method of Satterlund (1979) is
;        used.  When plotted, they look almost identical.  See
;        the Compare_em_air_Method routine in Qnet_file.pro.
;        Dingman (2002) uses the Brutsaert method.
;        Liston (1995, EnBal) uses the Satterlund method.

;        By default, the result is returned with units of kPa.
;        Set the MBAR keyword for units of millibars.
;        100 kPa = 1 bar = 1000 mbars
;                => 1 kPa = 10 mbars
;----------------------------------------------------------------
;NB!     Here, 237.3 is correct, and not a misprint of 273.2.
;        See footnote on p. 586 in Dingman (Appendix D).
;----------------------------------------------------------------
SATTERLUND = keyword_set(SATTERLUND)

if NOT(SATTERLUND) then begin
    ;------------------------
    ;Brutsaert (1975) method
    ;------------------------
    term1 = (17.3d * T) / (T + 237.3)
    e_sat = 0.611d * exp(term1)        ;[kPa]
endif else begin
    ;-------------------------
    ;Satterlund (1979) method
    ;-------------------------
    e_sat = 10d^(11.4d - (2353d/(T_air + 273.15)))   ;[Pa]
    e_sat = (e_sat / 1000d)   ;[kPa]
endelse

;---------------------------------
;Convert units from kPa to mbars?
;---------------------------------
if (keyword_set(MBAR)) then begin
    e_sat = (e_sat * 10d)   ;[mbar]
endif

RETURN, e_sat

end;  Saturation_Vapor_Pressure
;***************************************************************
function Vapor_Pressure, T, RH, MBAR=MBAR, $
                         SATTERLUND=SATTERLUND

;--------------------------------------------------
;Notes: T is temperature in Celsius
;       RH = relative humidity, in [0,1]
;            by definition, it equals (e / e_sat)
;       e has units of kPa.
;-------------------------------------------------
e = RH * Saturation_Vapor_Pressure(T)

;---------------------------------
;Convert units from kPa to mbars?
;---------------------------------
if (keyword_set(MBAR)) then begin
    e = (e * 10d)   ;[mbar]
endif

RETURN, e

end;  Vapor_Pressure
;*******************************************************************
function Latent_Heat_Flux, rho_air, De, T_air, T_surf, RH, p0, $
                           e_air, e_surf  ;(these 2 returned)

;------------------------------------------------------
;Notes: All the Q's have units of W/m^2 = J/(m^2 s).
;       2/17/04. Bug fix: Lv units should be J/kg,
;       and sign should be POSITIVE so that if
;       (e_air > e_surf) then water vapor should
;       condense on the surface, releasing heat,
;       and making Qe POSITIVE.  Note that De, the
;       bulk exchange coefficient, is also POSITIVE.

;       De is returned by Bulk_Exchange_Coeff function
;       and is not a pointer.
;------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;----------------------------
;Latent heat of vaporization
;Should minus sign be used? 
;----------------------------
;Lv = 2500        ;[Joules / gram]
Lv =  2500000.0   ;[Joules / kg]

;------------------------
;Compute vapor pressures
;------------------------
e_air  = Vapor_Pressure(*T_air, *RH, /MBAR)          ;[mbars]
e_surf = Saturation_Vapor_Pressure(*T_surf, /MBAR)   ;[mbars]

;--------------------------
;Compute latent heat flux
;--------------------------------------------------------
;Notes: De is not a pointer.  Pressure units cancel out.
;According to Dingman (2002, p. 273), constant should be
;0.622 instead of 0.662 (Zhang et al., 2000).
;--------------------------------------------------------
Qe = *rho_air * Lv * De * (0.662/(*p0)) * (e_air - e_surf)

RETURN, Qe

END;  Latent_Heat_Flux
;*******************************************************************
function Conduction_Heat_Flux

;-----------------------------------------------------------------

;Notes:  The conduction heat flux from snow to soil for computing
;        snowmelt energy, Qm, is close to zero.

;        However, the conduction heat flux from surface and sub-
;        surface for computing Qet is given by Fourier's Law,
;        namely Qc = Ks(Tx - Ts)/x.

;        All the Q's have units of W/m^2 = J/(m^2 s).
;-----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

RETURN, 0d

END;    Conduction_Heat_Flux
;*******************************************************************
function Advection_Heat_Flux

;----------------------------------------------------
;Notes: All the Q's have units of W/m^2 = J/(m^2 s).
;----------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

RETURN, 0d
END;    Advection_Heat_Flux
;*******************************************************************
function Initial_Cold_Content, h_snow, T_snow, rho_snow, Cp_snow

;----------------------------------------------------------------
;NOTES:  This function is used to initialize the cold content
;        of a snow pack (in Initialize_Snow_Vars in route.pro).
;        The cold content has units of [J/m^2] (_NOT_ [W/m^2]).
;        It is an energy (per unit area) threshold (or deficit)
;        that must be overcome before melting of snow can occur.
;        Cold content changes over time as the snowpack warms or
;        cools, but must always be non-negative.  See the Notes
;        for the Energy_Balance_Meltrate function.

;        Caller sets T_snow argument to T_surf.

;        K_snow is between 0.063 and 0.71  [W/m/deg_C]
;        All of the Q's have units of W/m^2 = J/(m^2 s).

;        12/22/05.  Removed T0 from argument list and
;        set to zero here.
;---------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;-----------------------------------------
;Compute initial cold content of snowpack
;-----------------------------------------
T0   = 0.0
Ecc0 = (*rho_snow * (*Cp_snow)) * (*h_snow) * (T0 - *T_snow)

RETURN, Ecc0
END;    Initial_Cold_Content
;*******************************************************************
function Max_Meltrate, h_snow, rho_H2O, rho_snow, snow_dt

;--------------------------------------------------------
;NOTES:  This function returns the max possible meltrate
;        for snow, assuming that all snow (given by snow
;        depth) is melted in the time interval, snow_dt.
;        Snow meltrates should never exceed this value.

;        h_snow, rho_H2O, and rho_snow are pointers.
;        snow_dt is real-valued.
;--------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

M_max = *h_snow * (*rho_H2O / *rho_snow) / snow_dt    ;[m/s]

RETURN, M_max
END;    Max_Meltrate
;*******************************************************************
function Degree_Day_Meltrate, c0, T0, T_air

;------------------------------------------------------------
;Notes:  Arguments are assumed to be pointers to a scalar
;        or grid so that:
;        c0 = degree-day melt factor [mm/day/deg_C]
;        T0 = threshold temperature [deg_C]
;        M  = water equivalent of snowmelt [m/s]
;        M_max  = max possible meltrate if all snow melts
;        T_air  = air temperature [deg_C]

;        Model must start when snow is isothermal.
;        Cooling of the snowpack is not considered.

;        This is a simple snowmelt model that can be used
;        when there is insufficient data to use the energy
;        balance method.  If c0 and T0 are established for a
;        range of conditions, Kane et al. (1997) showed that
;        this method gives comparable results to the energy
;        balance method.

;        86400000d = 1000 [mm/m] * 60 [sec/min] *
;                    60 [min/sec] * 24 [hrs/day]

;        rho_snow is not needed in formula here, but is
;        needed to convert snowmelt to water equivalent?
;-------------------------------------------------------------


;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;----------------------------
;Compute degree-day meltrate
;----------------------------
M = (*c0 / 8.64e7) * (*T_air - *T0)   ;[m/s]

RETURN, (M > 0d)
END;    Degree_Day_Meltrate
;*******************************************************************
function Energy_Balance_Meltrate, Qn_SW, Qn_LW, T_air, T_surf, $
                                  RH, p0, uz, z, z0_air, $
                                  rho_air, Cp_air, $
                                  ;---------------------------
                                  Ecc, h_snow, rho_snow, $
                                  Cp_snow, dt, $
                                  ;---------------------------
                                  e_air, e_surf  ;(returned)

;-----------------------------------------------------------------
;Notes:  3/13/07.  This function used to have vapor pressure
;        arguments e_air and e_surf.  However, e_air is always
;        computed as a function of T_air and RH and e_surf is
;        computed as a function of T_surf (saturated vap. press.)
;        So it makes sense to remove these two arguments and add
;        RH (relative humidity).  This change only affects the
;        Latent_Heat_Flux function call, which calls a new
;        function called Vapor_Pressure.
;-----------------------------------------------------------------
;        Qm    = energy used to melt snowpack (if > 0)
;        Qn_SW = net shortwave radiation flux (solar)
;        Qn_LW = net longwave radiation flux (air, surface)
;        Qh    = sensible heat flux from turbulent convection
;                between snow surface and air
;        Qe    = latent heat flux from evaporation, sublimation,
;                and condensation
;        Qa    = energy advected by moving water (i.e. rainfall)
;                (ARHYTHM assumes this to be negligible; Qa=0.)
;        Qc    = energy flux via conduction from snow to soil
;                (ARHYTHM assumes this to be negligible; Qc=0.)
;        Ecc   = cold content of snowpack = amount of energy
;                needed before snow can begin to melt [J/m^2]

;        All Q's here have units of [W/m^2].
;        Are they all treated as positive quantities ?

;        rho_air  = density of air [kg/m^3]
;        rho_snow = density of snow [kg/m^3]
;        Cp_air   = specific heat of air [J/kg/deg_C]
;        Cp_snow  = heat capacity of snow [J/kg/deg_C]
;                 = ???????? = specific heat of snow
;        Kh       = eddy diffusivity for heat [m^2/s]
;        Ke       = eddy diffusivity for water vapor [m^2/s]
;        Lv       = latent heat of vaporization [J/kg]
;        Lf       = latent heat of fusion [J/kg]
;        ------------------------------------------------------
;        Dn       = bulk exchange coeff for the conditions of
;                   neutral atmospheric stability [m/s]
;        Dh       = bulk exchange coeff for heat
;        De       = bulk exchange coeff for vapor
;        ------------------------------------------------------
;        T_air    = air temperature [deg_C]
;        T_surf   = surface temperature [deg_C]
;        T_snow   = average snow temperature [deg_C]
;        RH       = relative humidity [none] (in [0,1])
;        e_air    = air vapor pressure at height z [mbar]
;        e_surf   = surface vapor pressure [mbar]
;        ------------------------------------------------------
;        h_snow   = snow depth [m]
;        z        = height where wind speed is uz [m]
;        uz       = wind speed at height z [m/s]
;        P0       = atmospheric pressure [mbar]
;        T0       = snow temperature when isothermal [deg_C]
;                   (This is usually 0.)
;        z0_air   = surface roughness length scale [m]
;                   (includes vegetation not covered by snow)
;                   (Values from page 1033: 0.0013, 0.02 [m])
;        kappa    = von Karman's constant [unitless] = 0.41
;        dt       = snowmelt timestep [seconds]
;----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

FORWARD_FUNCTION Richardson_Number

;---------------------------------
;Some required physical constants
;are defined in the functions:
;e.g. Lv, Lf 
;---------------------------------

;------------------------------
;Compute the Richardson number
;------------------------------
Ri = Richardson_Number(z, uz, T_air, T_surf)

;-------------------------------------------------
;Compute bulk exchange coeffs (neutral stability)
;-------------------------------------------------
Dn = Bulk_Exchange_Coeff(uz, z, h_snow, z0_air, T_air, T_surf)
Dh = Dn
De = Dn

;---------------------------
;Compute sensible heat flux
;---------------------------
Qh = Sensible_Heat_Flux(rho_air, Cp_air, Dh, T_air, T_surf)
;Formula:  Qh = rho_air * Cp_air * Dh * (T_air - T_surf)
;print,'Dh = ', Dh
;print,'Qh = ', Qh

;-------------------------
;Compute latent heat flux
;-------------------------
Qe = Latent_Heat_Flux(rho_air, De, T_air, T_surf, RH, p0, $
                      e_air, e_surf)  ;(these 2 returned)
;Formula:  Qe = rho_air * Lv * De * (0.662/p0) * (e_air - e_surf)

;print,'Qe = ', Qe

;-----------------------------
;Compute conduction heat flux
;-----------------------------
Qc = Conduction_Heat_Flux()
;Formula:  Qc = 0d

;-----------------------------
;Compute advective heat flux
;-----------------------------
Qa = Advection_Heat_Flux()
;Formula:  Qa = 0d

;---------------------------------
;Qn_SW, Qn_SW & Ecc are pointers,
;others are local variables
;----------------------------------------------------
;Ecc is initialized with the Initial_Cold_Content
;function by Initialize_Snow_Vars function (2/21/07)
;----------------------------------------------------
;The following pseudocode only works for scalars but
;is otherwise equivalent to that given below and
;clarifies the logic:
;----------------------------------------------------
;  if (Qsum gt 0) then begin
;      if ((Qsum * dt) gt Ecc) then begin
;          ;-----------------------------------------
;          ;Snow is melting.  Use some of Qsum to
;          ;overcome Ecc, and remainder to melt snow
;          ;-----------------------------------------
;          Qm  = Qsum - (Ecc/dt)
;          Ecc = 0
;          M   = (Qm / (rho_w * Lf))
;      endif else begin
;          ;----------------------------
;          ;Snow is warming; reduce Ecc
;          ;----------------------------
;          Ecc = (Ecc - (Qsum * dt))
;          M   = 0d
;      endelse
;  endif else begin
;      ;------------------------------
;      ;Snow is cooling; increase Ecc
;      ;------------------------------
;      Ecc = Ecc - (Qsum * dt)
;      M   = 0d
;  endelse     
;-------------------------------------------------------
Qsum = *Qn_SW + *Qn_LW + Qh + Qe + Qa + Qc    ;[W/m^2]
Qcc  = (*Ecc / dt)                            ;[W/m^2]
Qm   = (Qsum - Qcc) > 0d                      ;[W/m^2]
*Ecc = (*Ecc - (Qsum * dt)) > 0d              ;[J/m^2]
;print,'Qm = ', Qm
;print,' '

;-----------------------------------
;Convert melt energy to a melt rate
;----------------------------------------
;Lf = latent heat of fusion [J/kg]
;Lv = latent heat of vaporization [J/kg]
;M  = (Qm/ (rho_w * Lf))
;----------------------------------------
;rho_w = 1000d       ;[kg/m^3]
;Lf    = 334000d     ;[J/kg = W*s/kg]
;So (rho_w * Lf) = 3.34e+8  [J/m^3]
;-------------------------------------
M  = (Qm / 3.34e+8)   ;[m/s]

RETURN, (M > 0.0)
END;    Energy_Balance_Meltrate
;**********************************************************************
function Priestley_Taylor_ET_Rate, alpha, Ks, T_soil_x, soil_x, $
                                   Qn_SW, Qn_LW, T_air, T_surf

;--------------------------------------------------------------
;Notes:  Qet   = energy used for ET of water from surface
;        Qn_SW = net shortwave radiation flux (solar)
;        Qn_LW = net longwave radiation flux (air, surface)
;        Qh    = sensible heat flux from turbulent convection
;                between snow surface and air
;        Qc    = energy transferred from surface to subsurface

;        All of the Q's have units of [W/m^2].

;        T_air    = air temperature [deg_C]
;        T_surf   = soil temp at the surface [deg_C]
;        T_soil_x = soil temp at depth of x meters [deg_C]

;        Ks   = thermal conductivity of soil [W/m/deg_C]
;        Ks = 0.45   ;[W/m/deg_C] (thawed soil; moisture content
;                     near field capacity)
;        Ks = 1.0    ;[W/m/deg_C] (frozen soil)

;        alpha = evaporation parameter
;        alpha = 0.95   ;(average found by Rouse)
;        alpha = 1.26   ;(Jackson et al. (1996), at saturation)

;        Modification of alpha:  alpha = (a1 * R) + a2
;        R  = 1.0d   ;(equals 1 for saturation; R in [0,1]) 
;        a1 = 1.0d   ;(accounts for moisture content of soil)
;        a2 = 0.2d   ;(accounts for vegetation effect)
;--------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;-------------------------------------------
;Compute the conductive energy between the
;surface and subsurface using Fourier's law
;-------------------------------------------
;soil_x is converted from [cm] to [m] when
;it is read from the GUI and then stored
;-------------------------------------------
;In Qet formula, the constant 0.011 has
;units of 1/[deg_C] to cancel T_air units.
;-------------------------------------------

Qc   = *Ks * (*T_soil_x - *T_surf) / (*soil_x)
Qnet = *Qn_SW + *Qn_LW
Qet  = *alpha * (0.406d + (0.011 * (*T_air))) * (Qnet - Qc)

;---------------------------------
;Convert ET energy to a loss rate
;----------------------------------------
;Lf = latent heat of fusion [J/kg]
;Lv = latent heat of vaporization [J/kg]
;ET = (Qet / (rho_w * Lv))
;----------------------------------------
;rho_w = 1000d       ;[kg/m^3]
;Lv    = -2500000d   ;[J/kg]
;So (rho_w * Lv) = -2.5e+9  [J/m^3]
;-----------------------------------
ET = (Qet / 2.5e+9)  ;[m/s]  (A loss, but returned as positive.)

RETURN, (ET > 0d)
END;    Priestley_Taylor_ET_Rate
;***************************************************************
function Energy_Balance_ET_Rate, Ks, T_soil_x, soil_x, $
                                 Qn_SW, Qn_LW, T_air, T_surf, $
                                 uz, z, z0_air, rho_air, $
                                 Cp_air, h_snow



;--------------------------------------------------------------
;Notes:  Qet   = energy used for ET of water from surface
;        Qn_SW = net shortwave radiation flux (solar)
;        Qn_LW = net longwave radiation flux (air, surface)
;        Qh    = sensible heat flux from turbulent convection
;                between snow surface and air
;        Qc    = energy transferred from surface to subsurface

;        All of the Q's have units of [W/m^2].

;        T_air    = air temperature [deg_C]
;        T_surf   = soil temp at the surface [deg_C]
;        T_soil_x = soil temp at depth of x meters [deg_C]

;        Ks = thermal conductivity of soil [W/m/deg_C]
;        Ks = 0.45   ;[W/m/deg_C] (thawed soil; moisture
;                     content near field capacity)
;        Ks = 1.0    ;[W/m/deg_C] (frozen soil)

;        z0_air = roughness length scale [m]
;        h_snow = snow depth [m]
;--------------------------------------------------------------
;NB!  h_snow is needed by the Bulk_Exchange_Coeff function
;     to adjust reference height, z.  It is not a pointer.

;--------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

;------------------------------
;Get bulk exchange coefficient
;------------------------------
Dh = Bulk_Exchange_Coeff(uz, z, h_snow, z0_air, T_air, T_surf)

;---------------------------
;Compute sensible heat flux
;---------------------------
;** T_surf = T0  ;?????????
Qh = Sensible_Heat_Flux(rho_air, Cp_air, Dh, T_air, T_surf) 
;Formula:  Qh = (rho_air * Cp_air) * Dh * (T_air - T_surf)

;-------------------------------------------
;Compute the conductive energy between the
;surface and subsurface using Fourier's law
;-------------------------------------------
Qc = (*Ks) * (*T_soil_x - *T_surf) / (*soil_x)

;------------------------------
;Qn_SW and Qn_LW are pointers,
;others are local variables
;------------------------------
Qnet = (*Qn_SW + *Qn_LW)
Qet  = (Qnet + Qh + Qc)

;----------------------------------------
;Lf = latent heat of fusion [J/kg]
;Lv = latent heat of vaporization [J/kg]
;ET = (Qet / (rho_w * Lv))
;-----------------------------------------
;rho_w = 1000d       ;[kg/m^3]
;Lv    = -2500000d   ;[J/kg]
;So (rho_w * Lv) = -2.5e+9  [J/m^3]
;-----------------------------------
ET = (Qet / 2.5e+9)  ;[m/s]  (A loss, but returned as positive.)

RETURN, (ET > 0d)

END;    Energy_Balance_ET_Rate
;*****************************************************************
function Infil_Is_1D, var, NX=nx, NY=ny, NZ=nz, $
                      RICHARDS=RICHARDS, NOT_PTR=NOT_PTR

;-------------------------------------------------------
;Notes:  The soil moisture array, theta, can be either
;        1D or 3D.  Several of the other functions need
;        to know whether theta is 1D or 3D.

;        If RICHARDS keyword is set, then vars like
;        theta, v, psi and K will be either 1D or 3D
;        arrays.  Otherwise, vars like q, I, etc. will
;        be scalars (1D case) or 2D arrays.

;        Argument, var, is assumed to be a pointer
;        unless the NOT_PTR keyword is set.
;-------------------------------------------------------
RICHARDS = keyword_set(RICHARDS)
POINTER  = NOT(keyword_set(NOT_PTR))

;----------------------------------
;Get dimensions of var, e.g. theta
;----------------------------------
;si[0] = number of dimensions
;It equals 0 for scalar, 1 for 1D,
;2 for 2D, etc.
;----------------------------------
if (POINTER) then si=size(*var) else si=size(var)

if (RICHARDS) then begin
    INFIL_1D = (si[0] eq 1)
endif else begin
    INFIL_1D = (si[0] eq 0)
endelse

if (INFIL_1D) then begin
    nx=0L     &  ny=0L     &  nz=si[1]
endif else begin
    nx=si[1]  &  ny=si[2]  &  nz=si[3]
endelse

RETURN, INFIL_1D

end;  Infil_Is_1D
;************************************************************************
pro Check_Low_Rainrate, iv, IN, P_total

;---------------------------------------------------------
;Notes:  Pulled out as common to all of the infiltration
;        methods based on total infiltrated depth, I.
;        This isn't needed for Richards' equation method.
;        P_total = (P + SM) is computed by caller.
;---------------------------------------------------------

;-----------------------------------
;Is P_total less than Ks anywhere ?
;If so, set IN = P_total there.
;-----------------------------------
np = n_elements(P_total)
nK = n_elements(*iv.Ks)
if (np eq 1) AND (nK eq 1) then begin
    ;--------------------------------
    ;P_total and Ks are both scalars
    ;--------------------------------
    if (P_total lt *iv.Ks) then IN = P_total
endif else begin
    ;-------------------------------
    ;Either P_total or Ks is a grid
    ;so IN will have become a grid
    ;-------------------------------
    w = where(P_total lt *iv.Ks, nw)

    if (nw ne 0) then begin
        if (np gt 1) then begin
            IN[w] = P_total[w]
        endif else begin
            IN[w] = P_total
        endelse
    endif
endelse

end;  Check_Low_Rainrate
;************************************************************************
function Green_Ampt_Infil_Rate_v1, iv, r

;------------------------------------------------------------ 
;Notes:  This was rewritten on 7/21/05 using information
;        from Smith, R.E. (2002) Infiltration Theory for
;        Hydrologic Applications, Water Resources Monograph
;        15, AGU.

;        IN  = infiltration rate [m/s]
;        Ks  = saturated hydraulic conductivity [m/s]
;        Ki  = initial hydraulic conductivity [m/s]
;        qs  = soil moisture content [dimless]
;        qi  = soil initial moisture content [dimless]
;         G  = capillary length scale [m]
;         I  = cum. infiltration depth (since reset) [m]
;         P  = precipitation rate [m/s]
;        SM  = snowmelt rate [m/s]
;         r  = "effective rainrate" = P + SM

;        Note that the infiltration rate has a max possible
;        value of (P + SM) and asymptotes to Ks as the
;        total infiltrated depth increases.  Need to reset
;        this depth between "events", but haven't decided
;        how to do this yet.

;        Total infiltrated depth is incremented in the
;        calling function, called Infiltration.
;------------------------------------------------------------
dK = (*iv.Ks - *iv.Ki)
dq = (*iv.qs - *iv.qi)
IN = (*iv.G * dK * dq / *iv.I) + *iv.Ks

;-----------------------------------
;This is what R.E. Smith gives as
;Equation 5.34, which is equivalent
;mathematically, but numerically ??
;-----------------------------------
;dK = (*iv.Ks - *iv.Ki)
;dq = (*iv.qs - *iv.qi)
;t2 = (*iv.G * dq) + *iv.I
;IN = (dK * t2 / *iv.I) + *iv.Ki

;print,'STEP 1: max(IN) = ', max(IN)

;-------------------------------------------
;Initially, IN = r, and all of the incoming
;water infiltrates.  IN cannot exceed r.
;Ponding time, Tp, is time until (IN lt r).
;-------------------------------------------
IN = (IN < r)

;print,'STEP 2: max(IN) = ', max(IN)

;-----------------------------------
;Is P_total less than Ks anywhere ?
;If so, set IN = P_total there.
;-----------------------------------
Check_Low_Rainrate, iv, IN, r

RETURN, IN

end;  Green_Ampt_Infil_Rate_v1
;************************************************************************
function Green_Ampt_Infil_Rate_1D, iv, r, r_last, n

;------------------------------------------------------------------
;Notes:  This version was written on 5/8-11/06 using the method
;        for dealing with variable rainfall that is described
;        on p. 114-116 in Smith, R.E. (2002) Infiltration Theory

;        for Hydrologic Applications, Water Resources Monograph
;        15, AGU.  The method used previously gives very similar
;        results, at least for sufficiently small time steps, but
;        is technically incorrect.  Due to its simplicity, it may
;        still be useful as a faster approximate method if we can
;        estimate error for various parameter settings.

;        The main difference with this version is that ponding
;        times may occur between timesteps and two different
;        cases are considered - ponding in the middle of a time
;        step and ponding during a rise in rainrate at the very
;        end of a timestep. (MID_PONDING vs. END_PONDING) Also
;        ponding time was not specifically computed before.

;        In order to test the second case (END_PONDING) uniform
;        precip was used with rates of 5e-5 m/s and 1e-4 m/s with
;        durations of 8 minutes and 5 minutes.  The soil type of
;        "silty clay" was selected to set infiltration parameters,
;        the infiltration rate was set to 0.1 minutes (6 seconds,
;        same as default for channel process).  The model was then
;        run for 200 time steps and the infiltration rate was
;        plotted (TEST_0D-v0.txt).  These settings triggered an
;        END_PONDING event and results appeared to be correct.
;        However, with these settings a virtually identical result
;        was obtained by the much simpler incorrect method used by
;        Green_Ampt_Infil_Rate_v1.  However, when infiltration
;        time step was increased to 0.3 minutes (18 seconds),
;        there was a fairly significant difference and had to
;        change initial f from (r/2) to (0.4 * r) to avoid a jump
;        in the infiltration curve.
;------------------------------------------------------------------

;---------------------------
;If r <= Ks, ponding cannot
;occur, so return f = r
;---------------------------
if (r le *iv.Ks) then RETURN, r

;----------------------------------
;Time at start and end of timestep
;----------------------------------
t_start = n * iv.dt
t_end   = (n+1) * iv.dt

;-----------------------
;Other common variables 
;-----------------------
dq  = (*iv.qs - *iv.qi)
dK  = (*iv.Ks - *iv.Ki)
top = (*iv.G * dq * dK)

PONDED  = (*iv.tp ne -1d)
R_GT_KS = (r gt *iv.Ks)
if NOT(PONDED) AND (R_GT_KS) then begin
    ;--------------------------------------------
    ;Compute test values of Ip  (Smith eqn 5.32)
    ;--------------------------------------------
    ;If (r_last lt Ks) then we'll have Ip1 < 0,
    ;so ponding at end of interval can't occur.
    ;But it seems it should be able to if r is
    ;big enough or time step is long enough.
    ;So set Ip1 large in this case ??
    ;--------------------------------------------
    ;*** if (r_last le *iv.Ks) AND (n eq 1) then begin
    if (r_last le *iv.Ks) then begin
        Ip1 = 999999d
    endif else begin
        Ip1 = top / (r_last - *iv.Ks)
    endelse
    Ip2 = top / (r - *iv.Ks)
    dIp = (Ip2 - *iv.I)

    ;------------------------------------------
    ;Does ponding occur within this timestep ?
    ;------------------------------------------
    ;   Ia(n) = sum_{k=0}^n (r_k * dt)
    ;   Ia(n-1) < Ip(r_n) < Ia(n)
    ;   0       < [Ip(r_n) - Ia(n-1)] < r_n*dt
    ;   I = Ia(n-1), not updated yet by caller
    ;------------------------------------------
    MID_PONDING = (dIp gt 0) AND (dIp lt (r * iv.dt))
    if (MID_PONDING) then begin
        print,'MID_PONDING occurred.'
        ;------------------------
        ;For this case: Ip = Ip2
        ;------------------------
        *iv.tp = t_start + (dIp / r)    ;(r gt Ks, above)
        *iv.fp = r
    endif

    ;----------------------------------------
    ;Does ponding occur at end of timestep ?
    ;----------------------------------------
    ;   Ip(r_n) < Ia(n-1) < Ip(r_{n-1})
    ;   [Ip(r_n) - Ia(n-1)] < 0  AND
    ;    Ia(n-1) < Ip(r_{n-1})
    ;----------------------------------------
    END_PONDING = (dIp lt 0) AND (*iv.I lt Ip1) AND $
                  NOT(MID_PONDING)
    if (END_PONDING) then begin
        print,'END_PONDING occurred.'
        ;--------------------------
        ;For this case: Ip = *iv.I
        ;--------------------------
        *iv.tp = t_end
        *iv.fp = (top / *iv.I) + *iv.Ks
    endif

    ;------------------------
    ;For debugging & testing
    ;------------------------
    if (MID_PONDING OR END_PONDING) then begin
        TF_Print,'tp = ' + TF_String(*iv.tp)
        TF_Print,' '
    endif

endif

;--------------------------------------------------
;If ponding still hasn't occurred, all "rain" will
;infiltrate; infil rate = r.  RETURN to caller and
;increment the cumulative depth, iv.I by (r * dt)
;--------------------------------------------------
PONDED = (*iv.tp ne -1d)
if NOT(PONDED) then RETURN, r

;---------------------------------------------------
;If ponding from the start or in previous 2 tests
;then forge ahead and compute infiltration rate
;via Newton-Raphson iteration using fp and tp.
;---------------------------------------------------
;First, set initial guess for iteration, f.
;*iv.fp was either set above or in a previous call.
;---------------------------------------------------
;   f = r / 2d    ;(converged usually, 
;                   but jumped to f < 0 sometimes)
;   f = r         ;(converges, but jumps to f < 0)
;   f  = 0.9 * r  ;(didn't converge)
;   f = r / 100d  ;(didn't converge)
;   f = 2d * r    ;(didn't converge)
;   f = 1000d     ;(didn't converge)
;---------------------------------------------------
fp = *iv.fp
f  = 0.41d * r
;** f  = 0.5d * r
;------------------
CONVERGED = 0b
tol       = 1e-8
n_tries   = 0
n_max     = 20
while (n_tries lt n_max) AND NOT(CONVERGED) do begin
    ;----------------------------------------
    ;We're using eqn. (6.19) in Smith's book
    ;for t(f) and solving it for f(t_end).
    ;----------------------------------------
    term1 = (t_end - *iv.tp) / (*iv.G * dq)
    term2 = 1d / (f  - *iv.Ks)
    term3 = 1d / (fp - *iv.Ks)
    term4 = (f  - *iv.Ki) / (f  - *iv.Ks)
    term5 = (fp - *iv.Ks) / (fp - *iv.Ki) 
    term6 = alog(term4 * term5) / dK
    h     = (term1 - term2 + term3 + term6)   ;(eq 0)
    ;---------------------------------------------
    dh_df = dK / ((f - *iv.Ki) * (f - *iv.Ks)^2d)
    del_f = -(h / dh_df)
    f     = f + del_f
    CONVERGED = (abs(del_f) le tol)
    n_tries = n_tries + 1
endwhile
 
;--------------------------------------
;Did Newton-Raphson fail to converge ?
;--------------------------------------
if NOT(CONVERGED) then begin
    TF_Print,'********************************************'
    TF_Print,'  ERROR:  Failure to converge.
    TF_Print,'  Max number of iterations exceeded'
    TF_Print,'  while computing infiltration rate.'
    TF_Print,'********************************************'
    TF_Print,' '
    RETURN, 0d
endif

;----------------------------------------
;Return infiltration rate at time, t_end
;----------------------------------------
RETURN, (f < r)

end;  Green_Ampt_Infil_Rate_1D
;************************************************************************
function Green_Ampt_Infil_Rate_3D, iv, r, r_last, n

;----------------------------------
;Time at start and end of timestep
;----------------------------------
t_start = n * iv.dt
t_end   = (n+1) * iv.dt

;-----------------------
;Other common variables 
;-----------------------
dq  = (*iv.qs - *iv.qi)

dK  = (*iv.Ks - *iv.Ki)

;-----------------------------------------------------
;NB!  wf is where ponding *could* occur during this
;time step.  Ponding can't occur where (r le *iv.Ks)       *** CHECK ***
;and Ip2 is undefined in this case.
;-----------------------------------------------------
wf = where((*iv.tp eq -1d) AND (r gt *iv.Ks), n_flux)
if (n_flux gt 0) then begin
    ;--------------------------------------------
    ;Compute test values of Ip  (Smith eqn 5.32)
    ;--------------------------------------------
    Ip2 = (*iv.G * dq * dK) / (r - *iv.Ks)       ;(must be 2D arrays  *****)
    Ip1 = (*iv.G * dq * dK) / (r_last - *iv.Ks)
    dIp = (Ip2 - *iv.I)

    ;--------------------------------------
    ;Does ponding occur for any grid cells
    ;within this timestep ?
    ;--------------------------------------
    dIp = (Ip2[wf] - (*iv.I)[wf])
    wm  = where((dIp gt 0) AND (dIp lt (r[wf] * iv.dt)), n_mid)  ;******
    if (n_mid ne 0) then begin
        ;--------------------
        ;Note that: Ip = Ip2
        ;--------------------
        (*iv.tp)[wf[wm]] = t_start + (dIp[wm] / r[wf[wm]])
        (*iv.fp)[wf[wm]] = r[wf[wm]]
    endif

    ;--------------------------------------
    ;Does ponding occur for any grid cells
    ;at the end of this timestep ?
    ;--------------------------------------
    we = where((dIp lt 0) AND (*iv.I lt Ip1), n_end)
    if (n_end ne 0) then begin
        ;----------------------
        ;Note that: Ip = *iv.I
        ;----------------------
        (*iv.tp)[wf[we]] = t_end
        fp_array = (*iv.G * dK * dq / *iv.I) + *iv.Ks  ;*****
        (*iv.fp)[wf[we]] = fp_array[wf[we]]
    endif
endif

;------------------------------------------
;Initialize the grid of infiltration rates
;------------------------------------------
ss = size(*iv.tp, /dimensions)
nx = ss[0]
ny = ss[1]
f  = dblarr(nx, ny)

;---------------------------------------------------
;For grid cells where ponding has not yet occurred,
;all "rain" should infiltrate and we set f=r.
;This will include cases where r < Ks.
;-------------------------------------------------------
;Grid cells that *have* ponded but that now have r < Ks        **********
;are special.  They should also be assigned f = r,
;since we always have fc > Ks, but are not part of wf.
;This case is handled at the end when we return (f < R)
;(the lesser of f and R), since r < Ks < fc.
;(iv.I will be incremented by (f * dt) in caller.)
;-------------------------------------------------------
;wp is where ponding has occurred, just now or earlier
;wf is where ponding has not occurred.
;-------------------------------------------------------
wp = where(*iv.tp ne -1d, n_ponded, COMPLEMENT=wf, NCOMP=n_flux)
if (n_flux ne 0) then f[wf] = r[wf]
if (n_ponded eq 0) then RETURN, f

;---------------------------------------------------
;For grid cells where ponding has occurred, we must
;compute the infiltration rate via grid-based
;Newton-Raphson iteration using fp and tp arrays.
;---------------------------------------------------
;First, we must initialize fp for those grid cells.
;---------------------------------------------------
f[wp] = 0.41d * r[wp]
;** f[wp] = 0.5d * r[wp]
;-----------------------
CONVERGED = 0b             ;(all grid cells)
tol     = 1e-8             ;(tolerance)
n_tries = 0
n_max   = 20
while (n_tries lt n_max) AND NOT(CONVERGED) do begin
    ;-----------------------------------------
    ;We're using eqn. (6.19) in Smith's book
    ;for t(f) and solving it for f(t_end).
    ;-----------------------------------------
    ;Some vars here may be scalars and others
    ;may be 2D arrays, but h must be 2D ??
    ;We compute h as a 2D array here because        ;**********
    ;of this issue, which is not optimal.
    ;-----------------------------------------
    term1 = (t_end - tp) / (iv.G * dq)
    term2 = 1d / (f  - *iv.Ks)
    term3 = 1d / (*iv.fp - *iv.Ks)
    term4 = (f  - *iv.Ki) / (f  - *iv.Ks)
    term5 = (*iv.fp - *iv.Ks) / (*iv.fp - *iv.Ki) 
    term6 = alog(term4 * term5) / dK
    h     = (term1 - term2 + term3 + term6)   ;(eq 0)
    ;-------------------------------------------------
    dh_df = dK / ((f - *iv.Ki) * (f - *iv.Ks)^2d)
    del_f = -(h / dh_df)
    ;-------------------------------------------------
    f[wp] = f[wp] + del_f[wp]
    CONVERGED = (abs(del_f[wp]) le tol)
    n_tries = n_tries + 1
endwhile

;--------------------------------------
;Did Newton-Raphson fail to converge ?
;--------------------------------------
if NOT(CONVERGED) then begin
    TF_Print,'********************************************'
    TF_Print,'  ERROR:  Failure to converge.
    TF_Print,'  Max number of iterations exceeded'
    TF_Print,'  while computing infiltration rate.'
    TF_Print,'********************************************'
    TF_Print,' '
    RETURN, 0d
endif

;-----------------------------
;Is r less than Ks anywhere ?
;If so, set f = r there.
;-----------------------------
;***  Check_Low_Rainrate, iv, f, r   ;(NO LONGER NEEDED, SEE ABOVE)

;-----------------------------------------
;Return infiltration rates at time, t_end
;-----------------------------------------
RETURN, (f < r)

end;  Green_Ampt_Infil_Rate_3D
;************************************************************************
function Green_Ampt_Infil_Rate_v2, iv, r, r_last, n

;------------------------------------------------------------ 
;Notes:  This was rewritten on 5/8/06 using information
;        from Smith, R.E. (2002) Infiltration Theory for
;        Hydrologic Applications, Water Resources Monograph
;        15, AGU, pages 114-116.

;        IN  = infiltration rate [m/s]
;        Ks  = saturated hydraulic conductivity [m/s]
;        Ki  = initial hydraulic conductivity [m/s]
;        qs  = soil moisture content [dimless]
;        qi  = soil initial moisture content [dimless]
;         G  = capillary length scale [m]
;         I  = cum. infiltration depth (since reset) [m]
;         P  = precipitation rate [m/s]
;        SM  = snowmelt rate [m/s]
;         n  = time step (for computing t_start & t_end)

;        Note that the infiltration rate has a max possible
;        value of (P + SM) and asymptotes to Ks as the
;        total infiltrated depth increases.  Need to reset
;        this depth between "events", but haven't decided
;        how to do this yet.

;        Total infiltrated depth is incremented in the
;        calling function, called Infiltration.
;------------------------------------------------------------
SINGLE_PROFILE = iv.all_scalars  ;(3/19/07)

if (SINGLE_PROFILE) then begin
    IN = Green_Ampt_Infil_Rate_1D(iv, r, r_last, n)
endif else begin
    IN = Green_Ampt_Infil_Rate_3D(iv, r, r_last, n)
endelse

RETURN, IN

end;  Green_Ampt_Infil_Rate_v2
;************************************************************************
function Smith_Parlange_Infil_Rate_v1, iv, r

;------------------------------------------------------------ 
;Notes:  IN  = infiltration rate [m/s]
;        Ks  = saturated hydraulic conductivity [m/s]
;        Ki  = initial hydraulic conductivity [m/s]
;        qs  = soil moisture content [dimless]
;        qi  = soil initial moisture content [dimless]
;         G  = capillary length scale [m]
;         I  = cum. infiltration depth (since reset) [m]
;         P  = precipitation rate [m/s]

;        SM  = snowmelt rate [m/s]
;         r  = (P + SM)  [m/s]

;        Note that the infiltration rate has a max possible
;        value of (P + SM) and asymptotes to Ks as the
;        total infiltrated depth increases.  Need to reset
;        this depth between "events", but haven't decided
;        how to do this yet.

;        Total infiltrated depth is incremented in the
;        calling function, called Infiltration.
;------------------------------------------------------------
dK = (*iv.Ks - *iv.Ki)
dq = (*iv.qs - *iv.qi)
t2 = exp((*iv.gam * *iv.I)/(*iv.G * dq)) - 1d
IN = (*iv.gam * dK / t2) + *iv.Ks

;--------------------------------------------
;Initially, IN = r, and all of the incoming
;water infiltrates.  IN cannot exceed r.
;Ponding time, Tp, is time until (IN lt r).
;--------------------------------------------
IN = (IN < r)

;-----------------------------------
;Is P_total less than Ks anywhere ?
;If so, set IN = P_total there.
;-----------------------------------
Check_Low_Rainrate, iv, IN, r

RETURN, IN

end;  Smith_Parlange_Infil_Rate_v1
;*****************************************************************
function Smith_Parlange_Infil_Rate_1D, iv, r, r_last, n

;------------------------------------------------------------------
;Notes:  This version was written on 5/8-11/06 using the method
;        for dealing with variable rainfall that is described
;        on p. 114-116 in Smith, R.E. (2002) Infiltration Theory
;        for Hydrologic Applications, Water Resources Monograph
;        15, AGU.  The method used previously gives very similar
;        results, at least for sufficiently small time steps, but
;        is technically incorrect.  Due to its simplicity, it may
;        still be useful as a faster approximate method if we can
;        estimate error for various parameter settings.

;        The main difference with this version is that ponding
;        times may occur between timesteps and two different
;        cases are considered - ponding in the middle of a time
;        step and ponding during a rise in rainrate at the very
;        end of a timestep (MID_PONDING vs. END_PONDING). Also
;        ponding time was not specifically computed before.

;        In order to test the second case (END_PONDING) uniform

;        precip was used with rates of 5e-5 m/s and 1e-4 m/s with
;        durations of 8 minutes and 5 minutes.  The soil type of
;        "silty clay" was selected to set infiltration parameters,
;        the infiltration rate was set to 0.1 minutes (6 seconds,
;        same as default for channel process).  The model was then
;        run for 200 time steps and the infiltration rate was
;        plotted (TEST_0D-v0.txt).  These settings triggered an
;        END_PONDING event and results appeared to be correct.
;        However, with these settings a virtually identical result
;        was obtained by the much simpler incorrect method used by
;        Green_Ampt_Infil_Rate_v1.  However, when infiltration
;        time step was increased to 0.3 minutes (18 seconds),
;        there was a fairly significant difference and had to
;        change initial f from (r/2) to (0.4 * r) to avoid a jump
;        in the infiltration curve.
;------------------------------------------------------------------

;---------------------------
;If r <= Ks, ponding cannot
;occur, so return f = r
;---------------------------
if (r le *iv.Ks) then RETURN, r

;----------------------------------
;Time at start and end of timestep
;----------------------------------
t_start = n * iv.dt
t_end   = (n+1) * iv.dt

;-----------------------
;Other common variables 
;-----------------------
dq  = (*iv.qs - *iv.qi)
dK  = (*iv.Ks - *iv.Ki)
fac = (*iv.G * dq / *iv.gam)

PONDED  = (*iv.tp ne -1d)
R_GT_KS = (r gt *iv.Ks)
if NOT(PONDED) AND (R_GT_KS) then begin
    ;--------------------------------------------
    ;Compute test values of Ip  (Smith eqn 5.47)
    ;--------------------------------------------
    ;If (r_last lt Ks) then we'll have Ip1 < 0,
    ;so ponding at end of interval can't occur.
    ;But it seems it should be able to if r is
    ;big enough or time step is long enough.
    ;So set Ip1 large in this case ??
    ;--------------------------------------------
    ;*** if (r_last le *iv.Ks) AND (n eq 1) then begin
    if (r_last le *iv.Ks) then begin
        Ip1 = 999999d
    endif else begin

        Ip1 = fac * alog(1d + (*iv.gam * dK / (r_last - *iv.Ks)))
    endelse
    Ip2 = fac * alog(1d + (*iv.gam * dK / (r - *iv.Ks)))
    dIp = (Ip2 - *iv.I)

    ;------------------------------------------
    ;Does ponding occur within this timestep ?
    ;------------------------------------------
    ;   Ia(n) = sum_{k=0}^n (r_k * dt)
    ;   Ia(n-1) < Ip(r_n) < Ia(n)
    ;   0       < [Ip(r_n) - Ia(n-1)] < r_n*dt
    ;   I = Ia(n-1), not updated yet by caller
    ;------------------------------------------
    MID_PONDING = (dIp gt 0) AND (dIp lt (r * iv.dt))
    if (MID_PONDING) then begin
        print,'MID_PONDING occurred.'
        ;------------------------
        ;For this case: Ip = Ip2
        ;------------------------
        *iv.tp = t_start + (dIp / r)    ;(r gt Ks, above)
        *iv.fp = r
    endif

    ;----------------------------------------
    ;Does ponding occur at end of timestep ?
    ;----------------------------------------
    ;   Ip(r_n) < Ia(n-1) < Ip(r_{n-1})
    ;   [Ip(r_n) - Ia(n-1)] < 0  AND
    ;    Ia(n-1) < Ip(r_{n-1})
    ;----------------------------------------
    END_PONDING = (dIp lt 0) AND (*iv.I lt Ip1) AND $
                  NOT(MID_PONDING)
    if (END_PONDING) then begin
        print,'END_PONDING occurred.'
        ;--------------------------
        ;For this case: Ip = *iv.I
        ;--------------------------
        *iv.tp = t_end
        *iv.fp = ((*iv.gam * dK) / (exp(*iv.I / fac)-1d)) + *iv.Ks
    endif

    ;------------------------
    ;For debugging & testing
    ;------------------------
    if (MID_PONDING OR END_PONDING) then begin
        TF_Print,'tp = ' + TF_String(*iv.tp)
        TF_Print,' '
    endif

endif

;--------------------------------------------------
;If ponding still hasn't occurred, all "rain" will
;infiltrate; infil rate = r.  RETURN to caller and
;increment the cumulative depth, iv.I by (r * dt)
;--------------------------------------------------
PONDED = (*iv.tp ne -1d)
if NOT(PONDED) then RETURN, r

;---------------------------------------------------
;If ponding from the start or in previous 2 tests
;then forge ahead and compute infiltration rate
;via Newton-Raphson iteration using fp and tp.
;---------------------------------------------------
;First, set initial guess for iteration, f.
;*iv.fp was either set above or in a previous call.
;---------------------------------------------------
;   f = r / 2d    ;(converged usually, 
;                   but jumped to f < 0 sometimes)
;   f = r         ;(converges, but jumps to f < 0)
;   f  = 0.9 * r  ;(didn't converge)
;   f = r / 100d  ;(didn't converge)
;   f = 2d * r    ;(didn't converge)
;   f = 1000d     ;(didn't converge)
;---------------------------------------------------
fp = *iv.fp
f  = 0.41d * r
;** f  = 0.5d * r    ;(converged for complex rain, but 0.4 and 0.6 didn't)
;**                  ;(for simple rain, jumped to f < 0)
;-----------------
CONVERGED = 0b
tol       = 1e-8
n_tries   = 0
n_max     = 20
while (n_tries lt n_max) AND NOT(CONVERGED) do begin
    ;----------------------------------------
    ;We're using eqn. (6.27) in Smith's book
    ;for t(f) and solving it for f(t_end).
    ;----------------------------------------
    term1 = (t_end - *iv.tp) * dK * (1d - *iv.gam) / (*iv.G * dq)
    term2 = alog(1d + (*iv.gam * dK) / (f - *iv.Ks)) / *iv.gam
    term3 = alog((f - *iv.Ki) / (f - *iv.Ks))
    term4 = alog(1d + (*iv.gam * dK) / (fp - *iv.Ks)) / *iv.gam
    term5 = alog((fp - *iv.Ki) / (fp - *iv.Ks))
    h     = term1 - term2 + term3 + term4 - term5   ;(eq 0)
    ;---------------------------------------------
    termA = dK / (f - *iv.Ks)
    termB = 1d / ((f - *iv.Ks) + (*iv.gam * dK))
    termC = 1d / (f - *iv.Ki)
    dh_df = termA * (termB - termC)
    del_f = -(h / dh_df)
    f     = f + del_f
    CONVERGED = (abs(del_f) le tol)
    n_tries = n_tries + 1
endwhile
 
;--------------------------------------
;Did Newton-Raphson fail to converge ?
;--------------------------------------
if NOT(CONVERGED) then begin
    TF_Print,'********************************************'
    TF_Print,'  ERROR:  Failure to converge.
    TF_Print,'  Max number of iterations exceeded'
    TF_Print,'  while computing infiltration rate.'
    TF_Print,'********************************************'
    TF_Print,' '
    RETURN, 0d
endif

;----------------------------------------
;Return infiltration rate at time, t_end
;----------------------------------------
RETURN, (f < r)

end;  Smith_Parlange_Infil_Rate_1D
;************************************************************************
function Smith_Parlange_Infil_Rate_3D, iv, r, r_last, n

;----------------------------------
;Time at start and end of timestep
;----------------------------------
t_start = n * iv.dt
t_end   = (n+1) * iv.dt

;-----------------------
;Other common variables 
;-----------------------
dq  = (*iv.qs - *iv.qi)
dK  = (*iv.Ks - *iv.Ki)
fac = (*iv.G * dq / *iv.gam)

;-----------------------------------------------------
;NB!  wf is where ponding *could* occur during this
;time step.  Ponding can't occur where (r le *iv.Ks)     *** CHECK ***
;and Ip2 is undefined in this case.
;-----------------------------------------------------
wf = where((*iv.tp eq -1d) AND (r gt *iv.Ks), n_flux)
if (n_flux gt 0) then begin
    ;--------------------------------------------
    ;Compute test values of Ip  (Smith eqn 5.47)
    ;--------------------------------------------
    ;If (r_last lt Ks) then we'll have Ip1 < 0,
    ;so ponding at end of interval can't occur.
    ;But it seems it should be able to if r is

    ;big enough or time step is long enough.
    ;So set Ip1 large in this case ??
    ;--------------------------------------------
    if (r_last le *iv.Ks) then begin
        Ip1 = 999999d
    endif else begin
        Ip1 = fac * alog(1d + (*iv.gam * dK / (r_last - *iv.Ks)))
    endelse
    Ip2 = fac * alog(1d + (*iv.gam * dK / (r - *iv.Ks)))
    dIp = (Ip2 - *iv.I)

    ;--------------------------------------
    ;Does ponding occur for any grid cells
    ;within this timestep ?
    ;--------------------------------------
    dIp = (Ip2[wf] - (*iv.I)[wf])
    wm  = where((dIp gt 0) AND (dIp lt (r[wf] * iv.dt)), n_mid)  ;******
    if (n_mid ne 0) then begin
        ;--------------------
        ;Note that: Ip = Ip2
        ;--------------------
        (*iv.tp)[wf[wm]] = t_start + (dIp[wm] / r[wf[wm]])
        (*iv.fp)[wf[wm]] = r[wf[wm]]
    endif

    ;--------------------------------------
    ;Does ponding occur for any grid cells
    ;at the end of this timestep ?
    ;--------------------------------------
    we = where((dIp lt 0) AND (*iv.I lt Ip1), n_end)
    if (n_end ne 0) then begin
        ;----------------------
        ;Note that: Ip = *iv.I
        ;----------------------
        (*iv.tp)[wf[we]] = t_end
        fp_array = ((*iv.gam * dK) / (exp(*iv.I / fac)-1d)) + *iv.Ks  ;***
        (*iv.fp)[wf[we]] = fp_array[wf[we]]
    endif

endif

;------------------------------------------
;Initialize the grid of infiltration rates
;------------------------------------------
ss = size(*iv.tp, /dimensions)
nx = ss[0]
ny = ss[1]
f  = dblarr(nx, ny)

;---------------------------------------------------
;For grid cells where ponding has not yet occurred,
;all "rain" should infiltrate and we set f=r.
;This will include cases where r < Ks.
;-------------------------------------------------------
;Grid cells that *have* ponded but that now have r < Ks        **********
;are special.  They should also be assigned f = r,
;since we always have fc > Ks, but are not part of wf.
;This case is handled at the end when we return (f < R)
;(the lesser of f and R), since r < Ks < fc.
;(iv.I will be incremented by (f * dt) in caller.)
;-------------------------------------------------------
;wp is where ponding has occurred, just now or earlier
;wf is where ponding has not occurred.
;-------------------------------------------------------
wp = where(*iv.tp ne -1d, n_ponded, COMPLEMENT=wf, NCOMP=n_flux)
if (n_flux ne 0) then f[wf] = r[wf]
if (n_ponded eq 0) then RETURN, f

;---------------------------------------------------
;For grid cells where ponding has occurred, we must
;compute the infiltration rate via grid-based
;Newton-Raphson iteration using fp and tp arrays.
;---------------------------------------------------
;First, we must initialize fp for those grid cells.
;---------------------------------------------------
f[wp] = 0.41d * r[wp]
;** f[wp] = 0.5d * r[wp]
;-----------------------
CONVERGED = 0b             ;(all grid cells)
tol     = 1e-8            ;(tolerance)
n_tries = 0
n_max   = 20
while (n_tries lt n_max) AND NOT(CONVERGED) do begin
    ;----------------------------------------
    ;We're using eqn. (6.27) in Smith's book
    ;for t(f) and solving it for f(t_end).
    ;----------------------------------------
    ;Some vars here may be scalars and others
    ;may be 2D arrays, but h must be 2D ??
    ;We compute h as a 2D array here because        ;**********
    ;of this issue, which is not optimal.
    ;-----------------------------------------
    term1 = (t_end - *iv.tp) * dK * (1d - *iv.gam) / (*iv.G * dq)
    term2 = alog(1d + (*iv.gam * dK) / (f - *iv.Ks)) / *iv.gam
    term3 = alog((f - *iv.Ki) / (f - *iv.Ks))
    term4 = alog(1d + (*iv.gam * dK) / (fp - *iv.Ks)) / *iv.gam
    term5 = alog((fp - *iv.Ki) / (fp - *iv.Ks))
    h     = term1 - term2 + term3 + term4 - term5   ;(eq 0)
    ;---------------------------------------------
    termA = dK / (f - *iv.Ks)
    termB = 1d / ((f - *iv.Ks) + (*iv.gam * dK))
    termC = 1d / (f - *iv.Ki)
    dh_df = termA * (termB - termC)
    ;---------------------------------------------
    del_f = -(h / dh_df)
    f[wp] = f[wp] + del_f[wp]
    CONVERGED = (abs(del_f[wp]) le tol)
    n_tries = n_tries + 1
endwhile

;--------------------------------------
;Did Newton-Raphson fail to converge ?
;--------------------------------------
if NOT(CONVERGED) then begin
    TF_Print,'********************************************'
    TF_Print,'  ERROR:  Failure to converge.
    TF_Print,'  Max number of iterations exceeded'
    TF_Print,'  while computing infiltration rate.'
    TF_Print,'********************************************'
    TF_Print,' '
    RETURN, 0d
endif

;-----------------------------
;Is r less than Ks anywhere ?
;If so, set f = r there.
;-----------------------------
;***  Check_Low_Rainrate, iv, f, r   ;(NO LONGER NEEDED, SEE ABOVE)

;-----------------------------------------
;Return infiltration rates at time, t_end
;-----------------------------------------
RETURN, (f < r)

end;  Smith_Parlange_Infil_Rate_3D
;************************************************************************
function Smith_Parlange_Infil_Rate_v2, iv, r, r_last, n

;------------------------------------------------------------ 
;Notes:  This was written on 5/11/06 using information
;        from Smith, R.E. (2002) Infiltration Theory for
;        Hydrologic Applications, Water Resources Monograph
;        15, AGU, pages 114-116.

;        IN  = infiltration rate [m/s]
;        Ks  = saturated hydraulic conductivity [m/s]
;        Ki  = initial hydraulic conductivity [m/s]
;        qs  = soil moisture content [dimless]
;        qi  = soil initial moisture content [dimless]
;         G  = capillary length scale [m]
;         I  = cum. infiltration depth (since reset) [m]
;         P  = precipitation rate [m/s]
;        SM  = snowmelt rate [m/s]
;         n  = time step (for computing t_start & t_end)

;        Note that the infiltration rate has a max possible
;        value of (P + SM) and asymptotes to Ks as the
;        total infiltrated depth increases.  Need to reset
;        this depth between "events", but haven't decided
;        how to do this yet.

;        Total infiltrated depth is incremented in the
;        calling function, called Infiltration.
;------------------------------------------------------------
SINGLE_PROFILE = iv.all_scalars  ;(3/19/07)

if (SINGLE_PROFILE) then begin
    IN = Smith_Parlange_Infil_Rate_1D(iv, r, r_last, n)
endif else begin
    IN = Smith_Parlange_Infil_Rate_3D(iv, r, r_last, n)
endelse

RETURN, IN

end;  Smith_Parlange_Infil_Rate_v2
;************************************************************************
function Beven_Exp_K_Infil_Rate_v1, iv, r

;------------------------------------------------------------ 
;Notes:  IN  = infiltration rate [m/s]
;        Ks  = saturated hydraulic conductivity [m/s]
;        Ki  = initial hydraulic conductivity [m/s]
;        qs  = soil moisture content [dimless]
;        qi  = soil initial moisture content [dimless]
;        C   = "storage suction factor" [m]
;            = (delta_psi * delta_theta) > 0
;        f   = parameter in K*(z) = K0 * exp(f*z) [1/m]
;              K* < Ks, is "eff. K behind wetting front"
;              (f < 0, roughly between -1 and -13)
;              Is f roughly equal to (-gamma/G) in the
;              Smith-Parlange method?  (Compare denoms.)
;              f is sometimes written as (del_theta/m).
;        K0  = K*(0) = coeff. in previous equation [m/s]
;         I  = cum. infiltration depth (since reset) [m]
;         P  = precipitation rate [m/s]
;        SM  = snowmelt rate [m/s]
;         r  = (P + SM)  [m/s]

;        This comes from: Beven, K. (1984) "Infiltration
;        into a class of vertically non-uniform soils",
;        Hydrol. Sciences J., 29(4), 425-434.

;        Note that the infiltration rate has a max possible
;        value of (P + SM).  It seems that this method does
;        not asymptote to Ks as I increases.  Can we simply
;        add Ks?  We should reset I between "events", but
;        haven't decided how to do this yet.

;        Total infiltrated depth, I, is incremented in the
;        calling function, called Infiltration.
;------------------------------------------------------------

;--------------------------------------------
;Note that Ks is used to store K* and c is
;different than c used for Richards' method.
;Need to add f to the set of infil vars.
;Also need to add GUI to collect all vars.
;Note that t1 and t3 are both < 0 & c > 0.
;--------------------------------------------
dq = (*iv.qs - *iv.qi)
t1 = (*iv.Ks * (*iv.f)) / dq
t2 = (*iv.c  * (*iv.I))
t3 = (1d - exp(-1d * (*iv.f) * (*iv.I) / dq)) 
IN = t1 * t2 / t3

;-------------------------------------------------
;Initially, IN = r, and all of the incoming
;water infiltrates.  IN cannot exceed r.
;Ponding time, Tp, is time until (IN lt r).
;-------------------------------------------------
IN = (IN < r)

;-----------------------------------
;Is P_total less than Ks anywhere ?
;If so, set IN = P_total there.
;-----------------------------------
Check_Low_Rainrate, iv, IN, r 

RETURN, IN

end;  Beven_Exp_K_Infil_Rate_v1
;*****************************************************************
function Theta_Field, theta_s, theta_r, psi_B, psi_A, c, $
                      lambda, REPORT=REPORT

REPORT = keyword_set(REPORT)

;-------------------------------------
;Compute "field capacity" theta value
;-------------------------------------
psi_f = -340d           ;[cm]
psi_f = (psi_f / 100d)  ; [cm -> meters]

ratio = (psi_f + psi_A) / psi_B    ;(should be > 0)

theta_f = (1d + ratio^c)^(-lambda/c)
theta_f = theta_f * (theta_s - theta_r) + theta_r

;----------------
;Optional report
;----------------
if (REPORT) then begin
    print,'theta_s = ', theta_s
    print,'theta_f = ', theta_f
    print,'theta_r = ', theta_r
    print,' '
endif

RETURN, theta_f

end;  Theta_Field
;*****************************************************************
function Theta_Wilting, theta_s, theta_r, psi_B, psi_A, c, $
                        lambda, REPORT=REPORT

REPORT = keyword_set(REPORT)

;----------------------------------------------
;Compute "permanent wilting point" theta value
;----------------------------------------------
psi_w = -15000d          ;[cm]
psi_w = (psi_w / 100d)   ;[cm -> meters]

ratio = (psi_w + psi_A) / psi_B    ;(should be > 0)

theta_w = (1d + ratio^c)^(-lambda/c)
theta_w = theta_w * (theta_s - theta_r) + theta_r

;----------------
;Optional report
;----------------
if (REPORT) then begin
    print,'theta_s = ', theta_s
    print,'theta_w = ', theta_w
    print,'theta_r = ', theta_r
    print,' '
endif

RETURN, theta_w

end;  Theta_Wilting
;*****************************************************************
function Theta_Min, theta_s, theta_r, psi_B, psi_A, c, lambda, $
                    REPORT=REPORT

;---------------------------------------------------------------
;Notes:  Note that for both B-C and TB-C, psi goes to
;        -Infinity as theta goes to theta_r (S_eff goes
;        to zero).  However, natural soils do not have heads
;        (tensions) less than -31,000 cm.  In this range they
;        absorb water from the air (H = hygroscopic).  While
;        initial theta values will always be set to a number
;        greater than theta_r, evaporation at the surface can
;        cause theta to drop to values near theta_r.  Here we
;        use the T-BC equation for theta(psi) to compute a
;        value theta_H corresponding to psi_H=-31,000 cm.
;---------------------------------------------------------------
REPORT = keyword_set(REPORT)

;---------------------------
;Compute min value of theta
;---------------------------
psi_H = -31000d         ;[cm]
psi_H = (psi_H / 100d)  ;[cm -> meters]

ratio = (psi_H + psi_A) / psi_B    ;(should be > 0)

theta_H = (1d + ratio^c)^(-lambda/c)
theta_H = theta_H * (theta_s - theta_r) + theta_r

;----------------
;Optional report
;----------------
if (REPORT) then begin
    print,'theta_s = ', theta_s
    print,'theta_H = ', theta_H
    print,'theta_r = ', theta_r
    print,' '
endif

RETURN, theta_H

end;  Theta_Min
;*****************************************************************
function Theta_Residual, theta_s, psi_B, lambda, REPORT=REPORT

;------------------------------------------------------------
;Note:  (3/18/08)  This is based on Equation 6-19 (p. 235),
;       and Figure 6-13 (p. 237) in Dingman (2002), but
;       uses a pressure head value of -1000000.  See also
;       Dingman's Table 6-1 (p. 235).

;       Recall that lambda = (1 / b).

;       This is called by Get_Soil_Params in GUI_infil.pro.
;------------------------------------------------------------
REPORT = keyword_set(REPORT)

;-------------------------------
;Compute "residual" theta value
;-------------------------------
psi_r = -1000000d        ;[cm]
psi_r = (psi_r / 100d)   ;[cm -> meters]

;-------------------------------------
;Note:  Both psi's < 0, so ratio > 0
;-------------------------------------
theta_r = theta_s * (psi_B / psi_r)^lambda

;----------------
;Optional report
;----------------
if (REPORT) then begin
    print,'psi_B    = ', psi_B
    print,'theta_s  = ', theta_s
    print,'theta_r  = ', theta_r
    print,' '
endif

RETURN, theta_r

end;  Theta_Residual
;*****************************************************************
function Theta_Max, r0, K_s, eta, lambda, theta_s, $
                    theta_r, theta_i, REPORT=REPORT

;------------------------------------------------------- 
;Note:  The limiting value of theta turns out to be the
;       same for the Brooks-Corey and the transitional
;       Brooks-Corey relations.  It is computed by
;       setting K = r0 and solving for theta.
;------------------------------------------------------- 
REPORT = keyword_set(REPORT)

;---------------------------
;Compute max value of theta
;---------------------------
if (r0 lt K_s) then begin
    eps     = eta / lambda
    arg     = (r0/K_s)^(1d/eps)
    theta_u = theta_r + ((theta_s - theta_r) * arg) 
endif else begin
    theta_u = theta_s
endelse

;----------------
;Optional report
;----------------
if (REPORT) then begin
    print,'theta_s = ', theta_s
    print,'theta_u = ', theta_u
    print,'theta_i = ', theta_i
    print,' '
endif

RETURN, theta_u

end;  Theta_Max
;*****************************************************************
function Z_Derivative_1D, v, dz    ;(pointer arguments)

;------------------------------------------------------ 
;Notes:  Both arguments are pointers.  The first is a
;        pointer to a 1D array and the second points
;        to a scalar or 1D array.  The result is a 1D
;        array, same size as v.

;        This routine does not worry about the wrap
;        around affect of SHIFT at bottom.  This must
;        be handled by the caller.
;------------------------------------------------------ 
dv_dz = (shift(*v,-1) - *v) / (*dz)

RETURN, dv_dz

end;  Z_Derivative_1D
;*****************************************************************
function Z_Derivative_3D, v, dz    ;(pointer arguments)

;----------------------------------------------------- 
;Notes:  Both arguments are pointers.  The first is a
;        pointer to a 3D array (or data cube) and the
;        second points to a scalar or 1D array.  The
;        result is a 3D array, same size as v.

;        This routine does not worry about the wrap
;        around affect of SHIFT at bottom.  This must
;        be handled by the caller.
;----------------------------------------------------- 
n_dz = n_elements(*dz)

if (n_dz eq 1) then begin
    ;---------------
    ;dz is a scalar
    ;---------------
    dv_dz = (shift(*v,0,0,-1) - *v) / (*dz)
endif else begin
    dv_dz = (shift(*v,0,0,-1) - *v)
    for j=0L,(n_dz - 1L) do begin
        dv_dz[*,*,j] = dv_dz[*,*,j] / (*dz)[j]
    endfor
endelse

RETURN, dv_dz

end;  Z_Derivative_3D
;*****************************************************************
pro Update_Richards_Theta, iv, REPORT=REPORT

;--------------------------------------------------------
;Notes:  This procedure updates the soil moisture, theta
;        as a function of the vertical flow rate, v.

;        q, v, qs and qr are pointers.
;        dz, nz and dt are scalars.

;        Theta is called "q" here and in getvars.pro.
;        Called by Richards_Infil_Rate function.
;--------------------------------------------------------
;3/6/08  Bug fix. Was OK for 1D but sign error for 3D.
;        Introduced Z_Derivative_1D function to clarify
;        changed sign in dtheta expression and left
;        Z_Derivative_3D function unchanged.
;--------------------------------------------------------
REPORT = keyword_set(REPORT)

;--------------------------------------
;Use the same profile for all pixels ?
;--------------------------------------
SINGLE_PROFILE = iv.all_scalars   ;(3/19/07)

if (SINGLE_PROFILE) then begin
    ;------------------------------
    ;Theta is a 1D array & so is v
    ;------------------------------
    ;dz may be scalar or 1D array
    ;------------------------------
    dv_dz = Z_Derivative_1D(iv.v, iv.dz)  ;(pointer args)

    ;-----------------------------------------------
    ;What should we do at lower boundary ?
    ;dv_dz = 0 matches v=const in Update_Richards_V
    ;-----------------------------------------------
    dv_dz[iv.nz-1L] = 0d
    ;*** dv_dz[iv.nz-1L] = dv_dz[iv.nz-2L]
endif else begin
    ;------------------------------
    ;Theta is a 3D array & so is v
    ;------------------------------
    dv_dz = Z_Derivative_3D(iv.v, iv.dz)

    ;-----------------------------------------------
    ;What should we do at lower boundary ?
    ;dv_dz = 0 matches v=const in Update_Richards_V
    ;-----------------------------------------------
    dv_dz[*,*,iv.nz-1L] = 0d
    ;*** dv_dz[*,*,iv.nz-1L] = dv_dz[*,*,iv.nz-2L]
endelse

;---------------------------------------------
;Update soil moisture, theta  (mass balance)
;---------------------------------------------
*iv.q = *iv.q - (dv_dz * iv.dt)

;print,'min(q), max(q) = ', min(*iv.q), max(*iv.q)   ;**********

;---------------------------
;Make sure theta <= theta_s
;and that  theta >= theta_r
;-------------------------------------------------
;NB! We don't need this when we check for layers
;that are filling or losing in Update_Richards_V.
;-------------------------------------------------
;  *iv.q = (*iv.q < *iv.qs)
;  *iv.q = (*iv.q < *iv.qH)
;; *iv.q = (*iv.q > *iv.qi)
;; *iv.q = (*iv.q > *iv.qr)

;----------------
;Optional report 
;----------------
;if (REPORT) then begin
;    print,'dv_dz = ', dv_dz[0:3]
;    print,'theta = ', *iv.q[0:3]
;    ;print,' '
;endif

end;  Update_Richards_Theta
;*****************************************************************
pro Update_Richards_Psi, iv, REPORT=REPORT

;----------------------------------------------------------------
;Notes:  This procedure updates the pressure head, psi, as
;        a function of the soil moisture, theta, via the
;        Brooks-Corey (B-C) or "transitional Brooks-Corey"
;        (TB-C) relation.  The TB-C relation has a continuous
;        derivative at saturation, unlike the B-C relation.

;        Psi is < 0 in the unsaturated zone, is 0 at saturation
;        (e.g. water table) and is > 0 below the water table.

;        Called by Richards_Infil_Rate function.
;        All arguments are pointers.

;        Note that for both B-C and TB-C, psi goes to
;        -Infinity as theta goes to theta_r (S_eff goes
;        to zero).  So initial theta values should always
;        be set to a number greater than theta_r.

;        For B-C, psi goes to psi_B as theta goes to theta_s,
;        and psi <= psi_B < 0, or abs(psi) >= abs(psi_B).
;            pow     = -1d / *iv.lam
;            arg     = S_eff^pow
;            *iv.psi = *iv.psiB * arg

;        For TB-C, psi goes to -psi_a as theta goes to theta_s
;        and psi <= -psi_a.  If we take psi_a=0, then psi=0 at
;        saturation (as is commonly assumed).  The hysteresis
;        effect can be addressed by taking psi_a ne 0 when the
;        soil is drying (theta decreasing) and perhaps also by
;        changing the other parameters.
 
;        There is a typo in R.E. Smith's AGU monograph in
;        equation (2.14), where lambda should be -lambda.

;        See "Infiltration Theory for Hydrologic Applica-
;        tions" by R.E. Smith (2002), p. 21-22.

;----------------------------------------------------------------
;NB!     Due to multiple layers, each input var was set to
;        a 1D or 3D array by the Initialize_Infil_Var routine.
;----------------------------------------------------------------
REPORT = keyword_set(REPORT)

;------------------------
;For testing & debugging
;------------------------
;print,'SINGLE_PROFILE = ', SINGLE_PROFILE
;print,'size(*iv.q)    = ', size(*iv.q)
;print,'size(*iv.p)    = ', size(*iv.p)
;print,'size(*iv.qs)   = ', size(*iv.qs)
;print,'size(*iv.qr)   = ', size(*iv.qr)
;print,'size(*iv.pB)   = ', size(*iv.pB)
;print,'size(*iv.pA)   = ', size(*iv.pA)
;print,'size(*iv.lam)  = ', size(*iv.lam)
;print,'size(*iv.c)    = ', size(*iv.c)
;print,' '

;--------------------------------------
;Use the same profile for all pixels ?
;--------------------------------------
SINGLE_PROFILE = iv.all_scalars   ;(3/19/07)

;-------------------------------------
;Compute the "effective saturation"
;Relative saturation = theta/porosity
;-------------------------------------
if (SINGLE_PROFILE) then begin
    ;------------------------------
    ;All of the vars are 1D arrays
    ;------------------------------
    S_eff = (*iv.q - *iv.qr) / (*iv.qs - *iv.qr)
    pow = -1d * (*iv.c / *iv.lam)
    arg   = (S_eff^pow - 1d)^(1d/(*iv.c))
    *iv.p = (*iv.pB * arg) - *iv.pA
endif else begin
    ;------------------------------------
    ;Each var is either a 1D or 3D array
    ;------------------------------------
    dim_qs  = (size(*iv.qs))[0]
    dim_qr  = (size(*iv.qr))[0]
    dim_pB  = (size(*iv.pB))[0]
    dim_pA  = (size(*iv.pA))[0]
    dim_lam = (size(*iv.lam))[0]
    dim_c   = (size(*iv.c))[0]
    ;-----------------------------
    for j=0L,(iv.nz - 1L) do begin
        ;------------------------------------------------
        ;At a given z, every input var is scalar or grid
        ;------------------------------------------------
        if (dim_qs  eq 3) then qs=(*iv.qs)[*,*,j]   else qs=(*iv.qs)[j]
        if (dim_qr  eq 3) then qr=(*iv.qr)[*,*,j]   else qr=(*iv.qr)[j]
        if (dim_pB  eq 3) then pB=(*iv.pB)[*,*,j]   else pB=(*iv.pB)[j]
        if (dim_pA  eq 3) then pA=(*iv.pA)[*,*,j]   else pA=(*iv.pA)[j]
        if (dim_lam eq 3) then lam=(*iv.lam)[*,*,j] else lam=(*iv.lam)[j]
        if (dim_c   eq 3) then c=(*iv.c)[*,*,j]     else c=(*iv.c)[j]
        ;----------------------------------------------
        ;NB!  It is OK to raise a grid to a grid power
        ;----------------------------------------------
        S_eff = ((*iv.q)[*,*,j] - qr) / (qs - qr)     ;(grid)
        pow   = -1d * (c / lam)                       ;(grid or scalar)
        arg   = (S_eff^pow - 1d)^(1d / c)             ;(grid)
        (*iv.p)[*,*,j] = (pB * arg) - pA              ;(grid)
    endfor
endelse

;print,'min(p), max(p) = ', min(*iv.p), max(*iv.p)   ;**********

;----------------
;Optional report 
;----------------
if (REPORT) then begin
    print,'S_eff = ', S_eff[0:3]
    print,'psi   = ', *iv.p[0:3]
    ;print,' '
endif

end;  Update_Richards_Psi
;*****************************************************************
pro Update_Richards_K, iv, REPORT=REPORT

;-----------------------------------------------------------
;Notes:  This procedure updates the hydraulic conductivity,
;        K, as a function of the pressure head, psi, via
;        the "Brooks-Corey" (B-C) or "transitional Brooks-
;        Corey" (TB-C) relation.


;        Called by Richards_Infil_Rate function.

;        lambda = pore size distribution parameter
;        eta    = "pore-disconnectedness" parameter
;        eta    = 2d + (3d * lambda)

;        There is a typo in R.E. Smith's AGU monograph in
;        equation (2.14), where eta should be -eta.

;        See "Infiltration Theory for Hydrologic Applica-
;        tions" by R.E. Smith (2002), p. 21-22.

;        For standard Brooks-Corey we would have:
;            pow = -1d * (*eta)
;            K_r = (*p / *pB)^pow

;-----------------------------------------------------------
REPORT = keyword_set(REPORT)

;--------------------------------------
;Use the same profile for all pixels ?
;--------------------------------------
SINGLE_PROFILE = iv.all_scalars   ;(3/19/07)

;-----------------------------------------------
;Compute K from the "relative conductivity", Kr
;-----------------------------------------------
;Use Transitional Brooks-Corey (TB-C)
;w/ continuous derivative at saturation
;Note:  q=qs => psi=0, + psiA=0 => K=Ks 
;---------------------------------------
if (SINGLE_PROFILE) then begin
    ;------------------------------
    ;All of the vars are 1D arrays
    ;------------------------------
    pow = -1d * (*iv.eta) / *iv.c
    Kr  = (1d + ((*iv.p + *iv.pA) / *iv.pB)^(*iv.c))^pow
    Kr  = (Kr < 1d) > 0d
    *iv.K = (*iv.Ks) * Kr
endif else begin
    ;------------------------------------
    ;Each var is either a 1D or 3D array
    ;------------------------------------
    dim_Ks  = (size(*iv.Ks))[0]
    dim_pB  = (size(*iv.pB))[0]
    dim_pA  = (size(*iv.pA))[0]
    dim_eta = (size(*iv.eta))[0]
    dim_c   = (size(*iv.c))[0]
    for j=0L,(iv.nz - 1L) do begin
        ;------------------------------------------------
        ;At a given z, every input var is scalar or grid
        ;------------------------------------------------
        if (dim_Ks  eq 3) then Ks=(*iv.Ks)[*,*,j]   else Ks=(*iv.Ks)[j]
        if (dim_pB  eq 3) then pB=(*iv.pB)[*,*,j]   else pB=(*iv.pB)[j]
        if (dim_pA  eq 3) then pA=(*iv.pA)[*,*,j]   else pA=(*iv.pA)[j]
        if (dim_eta eq 3) then eta=(*iv.eta)[*,*,j] else eta=(*iv.eta)[j]
        if (dim_c   eq 3) then c=(*iv.c)[*,*,j]     else c=(*iv.c)[j]
        ;----------------------------------------------
        ;NB!  It is OK to raise a grid to a grid power
        ;----------------------------------------------
        arg = ((*iv.p)[*,*,j] + pA) / pB      ;(grid)
        pow = -1d * (eta / c)                 ;(grid or scalar)
        Kr  = (1d + arg^c)^pow                ;(grid)
        Kr  = (Kr < 1d) > 0d                  ;(grid)
        (*iv.K)[*,*,j] = (Ks * Kr)            ;(grid)
    endfor
endelse

;print,'min(K), max(K) = ', min(*iv.K), max(*iv.K)   ;**********

;----------------
;Optional report 
;----------------
if (REPORT) then begin
    print,'K = ', *iv.K[0:3]
    ;print,' '
endif

end;  Update_Richards_K
;*****************************************************************
pro Update_Richards_V, iv, R, Rg, REPORT=REPORT

;-----------------------------------------------------------
;Notes:  This procedure updates the vertical flow rate
;        at each level, v, as a function of psi, K & theta.
;        Called by Richards_Infil_Rate function.
;        q, v, psi, K, qs and qr are pointers.
;        dz, nz, and dt are scalars.

;        If R is a grid or grid sequence, then the infil
;        vars are initialized as 3D vs. 1D by the routine
;        Initialize_Infil_Vars, and no action here is
;        required.

;        R   = (P + SM - ET) for current timestep
;              (could be either grid or scalar)
;              and is computed & passed by caller.
;        vB  = flow rate [m/s] at bottom of a layer
;        vT  = flow rate [m/s] at top of a layer
;        dz  = z-distance between nodes
;        nn  = number of nodes on z-axis

;        K_bar is a "mean value" of K.
;        Using K_bar = K doesn't work for the case of
;        redistribution due to evaporation, but seems
;        to work OK for many other cases.

;        K, psi and theta are assumed to be uniform
;        within any given soil layer, while the flow
;        rates are for the boundaries between layers.

;        If one is a scalar or grid, they all are.

;        The first derivative of psi is computed using
;        psi values on either side of a boundary and
;        the z-distance between the layer centers.
;-----------------------------------------------------------
REPORT = keyword_set(REPORT)

;--------------------------------------
;Use the same profile for all pixels ?
;--------------------------------------
SINGLE_PROFILE = iv.all_scalars   ;(3/19/07)

;-----------------------------------------------
;NB!  There are better ways to compute K_bar
;     than the mean value used here.  Another
;     method is discussed by R.E. Smith, at
;     the top of page 192 in his AGU monograph.
;----------------------------------------------- 
if (SINGLE_PROFILE) then begin
    ;------------------------------
    ;All of the vars are 1D arrays
    ;------------------------------
    dp_dz = Z_Derivative_1D(iv.p, iv.dz)   ;(pointer args)
    K_bar = (shift(*iv.K,-1) + *iv.K) / 2d
    vB    = K_bar * (1d - dp_dz)
    vT    = shift(vB,1)

    ;-------------------------------------
    ;If R is not a scalar, then the next
    ;line will generate an error
    ;----------------------------------------------
    ;Initialize_Infil_Vars now sets iv.all_scalars
    ;(used to define SINGLE_PROFILE) based on
    ;infil, precip, snowmelt and ET. (3/19/07)
    ;----------------------------------------------
    vT[0] = R

    ;-----------------------------------------------
    ;"Gravity drainage" bottom BC (dp_dz=0 => v=Ki)   ;************
    ;Assumes wetting front doesn't reach bottom ?
    ;-----------------------------------------------
    ;** vB[iv.nz-1L] = *Ki

    ;-----------------------------
    ;Simple bottom BC (dv_dz = 0)                     ;************
    ;-----------------------------
    vB[iv.nz-1L] = vB[iv.nz-2L]
endif else begin
    ;-----------------------------------
    ;Compute pressure gradient, dpsi_dz 
    ;-----------------------------------
    dp_dz = Z_Derivative_3D(iv.p, iv.dz)     ;(pointer args)

    ;--------------------------------------
    ;Theta, K, psi and v are all 3D arrays
    ;--------------------------------------
    K_bar     = (shift(*iv.K,0,0,-1) + *iv.K) / 2d
    vB        = K_bar * (1d - dp_dz)
    vT        = shift(vB, 0,0,1)
    vT[*,*,0] = R                    ;(works if R is scalar or grid)

    ;-----------------------------------------------
    ;"Gravity drainage" bottom BC (dp_dz=0 => v=Ki)   ;************
    ;Assumes wetting front doesn't reach bottom ?
    ;-----------------------------------------------
    ;** vB[*,*,iv.nz-1L] = *iv.Ki

    ;-----------------------------
    ;Simple bottom BC (dv/dz = 0)                     ;************
    ;-----------------------------
    vB[*,*,iv.nz-1L] = vB[*,*,iv.nz-2L]
endelse

;--------------------------------------------------
;Flow rate into any layer must be less than the
;"amount of space available", while flow rate
;out must be less than "amount of water available"
;-----------------------------------------------------
;With this, it seems to be unnecessary to force theta
;to stay within limits in Update_Richards_Theta
;-----------------------------------------------------
filling = where(((vT - vB) ge 0d), n_filling, $
                 COMP=losing, NCOMP=n_losing)
if (n_filling ne 0) then begin
    space_avail = (*iv.dz / iv.dt) * (*iv.qs - *iv.q)
    vT[filling] = (vT[filling] < (space_avail[filling] + vB[filling]))
endif

if (n_losing ne 0) then begin
    ;---------------------------------------------------------
    ;Note that for both B-C and TB-C, psi goes to -Infinity
    ;as theta goes to theta_r (S_eff goes to zero).  However,
    ;natural soils do not have heads (tensions) less than
    ;-31,000 cm.  In this range they absorb water from the air
    ;(hygroscopic).  The function Theta_Min finds the smallest
    ;theta-value, theta_H, corresponding to this limiting
    ;psi-value, and uses it to determine water available for
    ;exfiltration. It is incorrect to use theta_i or theta_r.
    ;During evaporation, the surface flow rate equals the
    ;evaporation rate (which is < 0) until the soil moisture
    ;at the surface drops to theta_H.  It then gradually
    ;approaches a value of 0 (from values < 0) because though
    ;water is drawn from below it cannot keep up with the
    ;demand at the surface.  This can be seen in the plot of
    ;infiltration rate when the EVAP keyword is set, in
    ;richards0.pro.
    ;---------------------------------------------------------
    water_avail = (*iv.dz / iv.dt) * (*iv.q - *iv.qH)
    vT[losing] = (vT[losing] > (vB[losing] - water_avail[losing]))
endif

;----------------------
;Update heap var for v
;----------------------
*iv.v = vT

;print,'min(v), max(v) = ', min(*iv.v), max(*iv.v)   ;**********
;print,' '

;---------------------------------
;Return flow rate in bottom layer
;---------------------------------
if (SINGLE_PROFILE) then begin
    Rg = (*iv.v)[iv.nz - 1L]
endif else begin
    Rg = (*iv.v)[*,*,iv.nz - 1L]
endelse

;----------------
;Optional report 
;----------------
;if (REPORT) then begin
;    print,'dpsi_dz = ', dp_dz[0:3]
;    print,'vT      = ', vT[0:3]
;    print,'vB      = ', vB[0:3]
;endif

end;  Update_Richards_V
;************************************************************************
pro Update_Richards_Zw, iv, REPORT=REPORT

;------------------------------------------------------------
;Note:  This procedure attempts to identify the depth of the
;       wetting front from examination of the theta values.

;       Notice that it is not assumed that the soil moisture
;       profile is a decreasing function from surface down.
;       If soil moisture profile starts to increase as we
;       approach the water table, this should still work.

;       Notice also that limiting theta value may not be
;       equal to theta_s.  For example, it approaches a
;       smaller value for a sustained (R lt K_s).
;------------------------------------------------------------

;--------------------------------------
;Use the same profile for all pixels ?
;--------------------------------------
SINGLE_PROFILE = iv.all_scalars   ;(3/19/07)

if (SINGLE_PROFILE) then begin
    q_below = shift(*iv.q, -1)
    diff = (*iv.q - q_below)
    diff[iv.nz-1L] = 0.0
    indices = where(diff GT 0, nd)    ;(must be GT not GE)

    if (nd eq 0) then begin
        ;--------------------------------
        ;Wetting front is at the surface
        ;--------------------------------------------
        ;This can happen for an equilibrium profile
        ;that is monotonically increasing with depth
        ;or for case where theta = theta_i for all z
        ;--------------------------------------------
        *iv.Zw = 0.0
    endif else begin
        imax = indices[nd-1]

        ;--------------------------------
        ;This is one way to define Z_wet
        ;--------------------------------
        ;;*iv.Zw = imax * (*iv.dz)

        ;--------------------------------------------
        ;Get min and max theta of decreasing section
        ;--------------------------------------------
        frac = 0.2
        tmax = max((*iv.q)[indices], min=tmin, /NAN)
        tmid = tmin + frac*(tmax - tmin)
        w    = where((*iv.q)[0:imax] gt tmid, nw2)
        if (nw2 gt 0) then begin
            imax2 = w[nw2-1]
            *iv.Zw = imax2 * (*iv.dz)
        endif else begin
            *iv.Zw = 0.0         ;*******  IS THIS RIGHT ? *******
        endelse
    endelse
endif else begin

    q_below = shift(*iv.q,0,0,-1)
    diff = (*iv.q - q_below)
    diff[*,*,iv.nz-1L] = 0.0
    n_dz = n_elements(*iv.dz)     ;(either 1 or iv.nz)

    ;-----------------------------
    ;Zero out the 2D array *iv.Zw
    ;---------------------------------------
    ;Note: *iv.Zw should have already been
    ;set to be a 2D array of correct size
    ;---------------------------------------
    *iv.Zw = (*iv.Zw > 0d) < 0d
  
    ;-------------------------------------
    ;Loop over the z-levels to find Z_wet
    ;-------------------------------------
    for j=0L,(iv.nz-2L) do begin           ;(nz-2) vs. (nz-1)
        diff_j      = diff[*,*,j]
        next_diff_j = diff[*,*,j+1]     ;*******
        if (n_dz eq 1) then dz=(*iv.dz) else dz=(*iv.dz)[j]

        ;------------------------------------------------------
        ;NB!  We are looking for a local min in theta profile.
        ;------------------------------------------------------
        ;NB!  If theta is same at all levels, then we will
        ;never get (diff_j GT 0) so Z_wet will remain at 0,
        ;even if (theta eq theta_s) at all levels !!!!    ****************
        ;How can we get Z_wet = Z_bot = (nz-1) * dz ???  ****************
        ;------------------------------------------------------
        ;NB!  If theta increases at all levels, which can
        ;happen for an equilibrium profile with water table,
        ;then we will never get (diff_j GT 0.0) and Z_wet will
        ;remain at 0.
        ;------------------------------------------------------
        ;NB!  For (j eq (nz-2)), we have (next_diff_j EQ 0.0).
        ;------------------------------------------------------
        IDs = where((diff_j GT 0.0) AND $
                    (next_diff_j LE 0.0), n_IDs)

        if (n_IDs ne 0) then (*iv.Zw)[IDs] = (j * dz)
    endfor
endelse

end;  Update_Richards_Zw
;************************************************************************
function Richards_Infil_Rate, iv, P, SM, ET, Rg

;------------------------------------------------------------
;Notes:  iv   = infiltration vars structure
;        IN   = infiltration rate [m/s]
;        Rg   = groundwater recharge rate [m/s]
;               (Returned to caller)
;        Ks   = saturated hydraulic conductivity [m/s]
;        Ki   = initial hydraulic conductivity [m/s]
;        qs   = soil moisture content (sat.)  [dimless]
;        qi   = soil moisture content (init.) [dimless]
;        qr   = soil residual moisture content [dimless]
;        pB   = bubbling pressure head [m]
;        pA   = optional pressure head offset [m]
;        cid  = cum. infiltration depth (since reset) [m]
;         P   = precipitation rate [m/s]
;        SM   = snowmelt rate [m/s]

;        Note that the infiltration rate has a max possible
;        value of (P + SM) and asymptotes to Ks as the
;        total infiltrated depth increases.

;        Total infiltrated depth is incremented in the
;        calling function, called Infiltration.
;------------------------------------------------------------
R = (P + SM) - ET

;--------------------------------------
;Use the same profile for all pixels ?
;--------------------------------------
SINGLE_PROFILE = iv.all_scalars   ;(3/19/07)

;----------------------------------
;Update the Richards eqn variables
;----------------------------------
;All layers processed at once
;----------------------------------
Update_Richards_Theta, iv
Update_Richards_Psi,   iv
Update_Richards_K,     iv
Update_Richards_v,     iv, R, Rg
Update_Richards_Zw,    iv           ;(not tested yet)

;-------------------------------------------
;Infiltration rate is flow rate at surface
;-------------------------------------------
;NB!  Must use (*iv.v)[0L] vs. *iv.v[0L]
;-------------------------------------------
;Infiltration rate for node just below the
;surface will & should be different than at
;the surface and won't compare as well with
;Green-Ampt, etc.
;-------------------------------------------
if (SINGLE_PROFILE) then begin
    IN = (*iv.v)[0L]
endif else begin
    IN = (*iv.v)[*,*,0]
endelse

;-----------------------------------------------------------
;Richards' equation is only used in the so-called "upper
;layers".  There can be between 1 and 3 of these layers.
;To avoid high computational cost in the less dynamic
;"lower zone" below, a simplified method is used to route
;flow through the lower zone to the water table and to
;estimate a "groundwater recharge rate", Rg.  This is done
;using the vertical flow rate at the bottom of the set of
;upper layers and perhaps other information.  If the water
;table intersects the upper layers, then Rg is computed as
;the vertical flow rate in the layer just above the water
;table.
;-----------------------------------------------------------
;The simplest method of estimating Rg is to simply set it
;to the vertical flow rate at the bottom of the upper zone.
;However, travel time through the lower zone is not taken
;into account.  If Rg < 0, then water can even be drawn
;upward from the water table. 
;-----------------------------------------------------------
;Another simple idea for estimating Rg is to assume that
;psi varies linearly in the lower zone:   psi = a*z + b.
;Since psi=0 at the bottom and psi=psi_zb at the top, we
;can compute a and b as:
;    a = -psi_zb/(z_wt - z_zb)
;    b = -a * z_wt
;The flow rate in the lower zone is then computed as:
;    v = K(a*z + b) * (1 - a)
;It follows that the flow rate at the bottom of the lower
;zone can be written as:
;    Rg = v(z_wt) = v(z_zb) * (Ks / K(psi_zb)).
;Since K(psi_zb) <= Ks, we have v(z_zb) < Rg < Ks.
;If psi_zb < (z_zb - z_wt), then we get Rg < 0 and water
;can be drawn upward from the water table.
;-----------------------------------------------------------

;--------------------------------------
;For testing:  Plot the theta profiles
;--------------------------------------
PLOT = 1b
if (PLOT) then begin
    wset, 0
    ;** wait, 0.005

    ymin = min(*iv.qi)
    ymax = max(*iv.qs + 0.05d)
    if (SINGLE_PROFILE) then begin
        plot, *iv.z, *iv.q, psym=-1, /ynozero, $
               xtitle='Depth [meters]', ytitle='Soil moisture', $
               ystyle=1, yrange=[ymin, ymax]
    endif else begin
        plot, *iv.z, (*iv.q)[2,2,*], psym=-1, /ynozero, $
               xtitle='Depth [meters]', ytitle='Soil moisture', $
               ystyle=1, yrange=[ymin, ymax]      
    endelse
endif

;------------------------------------
;For testing:  Plot the psi profiles
;------------------------------------
if (PLOT) then begin
    wset, 1
    ;** wait, 0.005
    yrange = [-3.0, 0.5]    ;(Log case)
    ytitle = '-Log(-Pressure head) [m]'
    ;--------------------------------------
    ;ytitle = 'Pressure head [m]' 
    ;yrange = [-20.0, 0.0]  ;(Linear case)
    ;--------------------------------------
    if (SINGLE_PROFILE) then begin
        y = -1d * alog(abs(*iv.p) + 1)
        plot, *iv.z, y, psym=-1, /ynozero, $
               xtitle='Depth [meters]', ytitle=ytitle, $
               ystyle=1, yrange=yrange
    endif else begin
        y = -1d * alog(abs((*iv.p)[2,2,*]) + 1d)
        plot, *iv.z, y, psym=-1, /ynozero, $
               xtitle='Depth [meters]', ytitle=ytitle, $
               ystyle=1, yrange=yrange
    endelse
endif

;-----------------------------------------------
;Note:  The Route_Flow routine keeps track of
;mass balance and reports values at end of run
;-----------------------------------------------
RETURN, IN

end;  Richards_Infil_Rate
;************************************************************************
function Wetted_Thicknesses, gv, z, h 

;-----------------------------------------------------------
;NOTES:  gv   = gw_vars = structure
;        z    = elevation of land surface [m]
;        h    = elevation of water table [m]
;       (z-h) = depth to water table [m]
;        diff = (partial sum of soil thicknesses -
;               depth to water table)

;        Let ti be the thickness of the ith soil layer,
;        starting from the surface, and yi the wetted
;        thickness of this layer, which must be between
;        0 and ti.  If (z gt h), then we have:
;        sum_{i=1}^n yi = [sum_{i=1}^n ti] – (z-h)

;        e.g.
;        y1 = (t1 – (z-h)) > 0 < t1
;        y2 = ((t1+t2) – y1 - (z-h)) > 0 < t2
;        y3 = ((t1+t2+t3) – (y1+y2) – (z-h)) > 0 < t3

;        This function was tested by defining some vars
;        as follows, and both methods seemed to work.

;        IDL> gv = {nlayers:5, soil_thick:(findgen(5)+1)*0.1} 
;        IDL> z  = fltarr(3,3) + 2.0
;        IDL> h  = fltarr(3.3) + 1.85
;        IDL> y  = Wetted_Thicknesses(gv, z, h)
;        IDL> print, y[*,*,0]
;        IDL> print, y[*,*,1]  ;(etc.)

;        If (h gt z), then testing shows that (yk eq tk)
;        for all layers, as it should.
;-----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

dims  = size(z, /dim)
ncols = dims[0]
nrows = dims[1]

ysum  = fltarr(ncols, nrows)
y = fltarr(ncols, nrows, gv.nlayers)

;------------------------------
;Original way: Prior to 3/1/04
;Seems to agree with new way?
;------------------------------
;Compute wetted thickness, y,
;for each soil layer
;-----------------------------
tsum = (h - z)
for k=0, (gv.nlayers - 1) do begin
    ;-----------------------------
    ;Before 7/7/06. (tk = scalar)
    ;-----------------------------
    ;; tk   = gv.soil_thick[k]
    ;; tsum = (tsum + tk)
    ;; y[*,*,k] = (tsum > 0.0) < tk)

    ;------------------------------
    ;7/7/06.  Allow scalar or grid
    ;------------------------------
    tsum = tsum + (*gv.th[k])
    y[*,*,k] = (tsum > 0.0) < (*gv.th[k])
endfor

;-----------------------------
;Compute wetted thickness, y,
;for each soil layer
;-----------------------------
;Slightly more costly method.
;-----------------------------
;tsum = (h - z)                       ;(negative, if z > h)
;for k=0, (gv.nlayers - 1) do begin
;    tk = gv.soil_thick[k]            ;(is now a scalar)
;    tsum = (tsum + tk)
;    y[*,*,k] = ((tsum - ysum) > 0.0) < tk
;    ysum = ysum + y[*,*,k]
;endfor


RETURN, y
END;    Wetted_Thicknesses
;*******************************************************************
function Darcy_Flow, K, S, dw, y

;-----------------------------------------------------
;Notes:  K  = hydraulic conductivity [m/s]
;        S  = water table slope (perhaps topo slope)
;        dw = width of element [m]
;        y  = wetted flow depth in a layer [m]
;        Q  = discharge through a layer [m^3 / s]
;-----------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

Q = K * S * dw * y


RETURN, Q
END;    Darcy_Flow
;*******************************************************************
function Total_Darcy_Layer_Flow, gv, h, y, dw, ds, pIDs

;---------------------------------------------------------
;NOTES:  gv = gw_vars = structure
;        z  = elevation of land surface [m]
;        h  = elevation of water table [m]
;        (z-h) = depth to water table [m]
;        Sh = water table slope [unitless]
;        Ks = soil_K = sat. hydraulic conductivity [m/s]
;        dw = element width [m]
;        ds = hor. Dist. between pixel and parent [m]
;        y  = wetted flow depth in each layer [m]
;             (could be a recycled local variable)
;        Q  = total Darcy-flow discharge [m^3/s]
;             (summed over all layers)
;        diff = (partial sum of soil thicknesses -
;                depth to water table)

;        See Notes for Update_Water_Table routine.
;---------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

FORWARD_FUNCTION Free_Surface_Slope

;---------------------------------
;Compute water table slope from h
;---------------------------------
;NB!  h is assumed to be a grid.
;---------------------------------
;NB!  Q is zero where Sh is zero.
;-----------------------------------------
;NB!  Flow direction is still assumed to
;     be given by the DEM's D8 flow grid.
;-----------------------------------------
Sh = Free_Surface_Slope(0.0, h, ds, pIDs)

;----------------------------------------
;Compute wetted-depth, y, for each layer
;Now passed by caller.
;----------------------------------------
;** diff = -(z - h)

;-------------------------------
;NB!  h is assumed to be a grid.
;It is converted, if necessary,
;by Initialize_GW_Vars.
;-------------------------------
dims = size(h, /dimensions)
nx   = dims[0]
ny   = dims[1]
Q    = fltarr(nx, ny)

for k=0, (gv.nlayers - 1) do begin
    ;------------------------------------
    ;Add Q for this layer, via Darcy law
    ;------------------------------------
    Q = Q + (*(gv.Ks[k]) * Sh * dw * y[*,*,k])

    ;-------------------
    ;Used before 7/7/06
    ;-------------------
    ;** Q = Q + (gv.soil_K[k] * Sh * dw * y[*,*,k])
endfor

RETURN, Q
END;    Total_Darcy_Layer_Flow
;*******************************************************************
function Total_Darcy_Layer_Flow_VK, gv, h, y, dw, ds, pIDs

;-----------------------------------------------------
;NOTES:  gv = gw_vars = structure
;        z  = elevation of land surface [m]
;        h  = elevation of water table [m]
;        (z-h) = depth to water table [m]
;        Sh = water table slope [unitless]
;        K   = hydraulic conductivity [m/s];
;              each layer can have its own K grid,
;              represented as VK1, VK2, etc.
;        dw = element width [m]
;        ds = hor. Dist. between pixel and parent [m]
;        y  = wetted flow depth in each layer [m]
;             (could be a recycled local variable)
;        Q  = total Darcy-flow discharge [m^3/s]

;             (summed over all layers)
;        diff = (partial sum of soil thicknesses -
;                depth to water table)
;-----------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

FORWARD_FUNCTION Free_Surface_Slope

;---------------------------------
;Compute water table slope from h
;---------------------------------
;NB!  h is assumed to be a grid.
;---------------------------------
;NB!  Q is zero where Sh is zero.
;-----------------------------------------
;NB!  Flow direction is still assumed to
;     be given by the DEM's D8 flow grid.
;-----------------------------------------
Sh = Free_Surface_Slope(0.0, h, ds, pIDs)

;----------------------------------------
;Compute wetted-depth, y, for each layer
;Now passed by caller.
;----------------------------------------
;** diff = -(z - h)

;---------------------------------
;NB!  h is assumed to be a grid.
;---------------------------------
dims  = size(h, /dim)
ncols = dims[0]
nrows = dims[1]
Q     = fltarr(ncols, nrows)

;------------------------------------
;Add Q for each layer, via Darcy law
;------------------------------------
Q = Q + (*gv.VK1  * Sh * dw * y[*,*,0])
Q = Q + (*gv.VK2  * Sh * dw * y[*,*,1])
Q = Q + (*gv.VK3  * Sh * dw * y[*,*,2])
Q = Q + (*gv.VK4  * Sh * dw * y[*,*,3])
Q = Q + (*gv.VK5  * Sh * dw * y[*,*,4])
Q = Q + (*gv.VK6  * Sh * dw * y[*,*,5])
Q = Q + (*gv.VK7  * Sh * dw * y[*,*,6])
Q = Q + (*gv.VK8  * Sh * dw * y[*,*,7])
Q = Q + (*gv.VK9  * Sh * dw * y[*,*,8])
Q = Q + (*gv.VK10 * Sh * dw * y[*,*,9])

RETURN, Q
END;    Total_Darcy_Layer_Flow_VK
;*******************************************************************
function Total_Subsurface_Flow, gv, h, y, dw, ds, pIDs

;-------------------------------------------------------
;NOTES:  gv = gw_vars = structure
;        h  = elevation of water table [m]

;        Updates to y are also returned.
;-------------------------------------------------------
case (gv.method) of
    0 : Q_gw = 0.0
    1 : Q_gw = Total_Darcy_Layer_Flow(gv, h, y, dw, ds, pIDs)
    2 : Q_gw = Total_Darcy_Layer_Flow_VK(gv, h, y, dw, ds, pIDs)

endcase

RETURN, Q_gw
END;    Total_Subsurface_Flow
;*******************************************************************
function Darcy_Layer_Seep_Rate, gv, h, z, y, Rg, $
                                dw, ds, da, pIDs, $
                                p1,p2,p3,p4,p5,p6,p7,p8, $
                                w1,w2,w3,w4,w5,w6,w7,w8

;-------------------------------------------------
;Notes:  gv = gw_vars = structure
;        Bug fix on 7/19/05, gw_vars vs. gv used.
;        
;7/19/05:  This function may no longer be in use
;          anywhere.  The call in the Seepage
;          function is commented out in favor of
;          a call to Total_Darcy_Layer_Flow.
;-------------------------------------------------

;------------------------------
;Get the vertical contribution
;See call to Precipitation.
;------------------------------
;Rg = 0.0

;-----------------------------
;Sum discharges of all layers 
;-----------------------------
case (gw_vars.method) of
    0: Q_gw = 0.0
    1: Q_gw = Total_Darcy_Layer_Flow(gv, h, y, dw, ds, pIDs)

    2: Q_gw = Total_Darcy_Layer_Flow_VK(gv, h, y, dw, ds, pIDs)
endcase

;--------------------------
;Print min and max of Q_gw
;--------------------------
Q_min = min(Q_gw, max=Q_max)
TF_Print,'   Q_min = ' + TF_String(Q_min)
TF_Print,'   Q_max = ' + TF_String(Q_max)

;--------------------------------
;Overwrite h & y with new values
;Need to pass gw_vars.
;--------------------------------
Update_Water_Table, h, y, Q_gw, Rg, da, gv, $
                    p1,p2,p3,p4,p5,p6,p7,p8, $
                    w1,w2,w3,w4,w5,w6,w7,w8

;--------------------------------------------
;y is now updated in previous routine, but
;is initialized with Wetted_Thicknesses fcn.
;--------------------------------------------
;*** y = Wetted_Thicknesses(gv, z, h)

;--------------
;For debugging
;--------------
;h_min = min(h, max=h_max)

;print,'h_min = ',h_min
;print,'h_max = ',h_max
;------------------------------
;z_min = min(z, max=z_max)
;print,'z_min = ',z_min
;print,'z_max = ',z_max

;------------------------
;Compute the "seep rate"
;Can be pos. or neg. ?
;------------------------
dh_dt = (h - h_last) / gv.dt
GW = (h gt z) * dh_dt
;*** GW = (h gt z) * (dh_dt > 0.0)

;------------------------
;Print min and max of GW
;------------------------
gw_min = min(gw, max=gw_max)
TF_Print,'   GW_min = ' + TF_String(gw_min)
TF_Print,'   GW_max = ' + TF_String(gw_max)

RETURN, GW
END;    Darcy_Layer_Seep_Rate
;*******************************************************************
function Trapezoid_R, d, wb, theta, AW=AW, PW=PW

;---------------------------------------------------------------
;Notes:  Compute the hydraulic radius of a trapezoid that:
;          (1) has a bed width of wb >= 0 (0 for triangular)
;          (2) has a bank angle of theta (0 for rectangular)
;          (3) is filled with water to a depth of d.
;        The units of wb and d are meters.  The units of theta
;        are assumed to be degrees and are converted.
;---------------------------------------------------------------
 
;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

theta_rad = (theta * !DTOR)

;----------------------------------------------
;See Notes for TF_Tan function in utils_TF.pro
;----------------------------------------------
AW = d * (wb + (d * TF_Tan(theta_rad)))
PW = wb + (2.0 * d / cos(theta_rad))   ;(what if (PW eq 0) ?)

R  = (AW / PW)

RETURN, R
END;    Trapezoid_R
;*******************************************************************
function Manning_Formula, R, S, N

;---------------------------------------------------------
;Notes:  R = (A/P) = hydraulic radius [m]
;        N = Manning's roughness coefficient
;            (usually in the range 0.012 to 0.035)
;        S = bed slope (assumed equal to friction slope)

;        R,S, and N may be 2D arrays.

;        If length units are all *feet*, then an extra
;        factor of 1.49 must be applied.  If units are
;        meters, no such factor is needed.

;        Note that Q = Ac * u, where Ac is cross-section
;        area.  For a trapezoid, Ac does not equal w*d.
;---------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

if (n_elements(N) eq 0) then N=0.03

;** p = (2d / 3d)

u = (R^0.6666666666d) * sqrt(S) / N 

;*****************************
;Add a hydraulic jump option
;for when u gets too big ??
;*****************************

;------------------------------------
;Option to return friction factor, f
;(Correct this for R vs. d.)
;------------------------------------
;*** f = (g * d * S) / (u^2d)

RETURN, u
END;    Manning_Formula
;*******************************************************************
function Law_of_the_Wall, d, S, z0, f=f

;---------------------------------------------------------

;Notes:  u  = flow velocity  [m/s]
;        d  = flow depth [m]
;        z0 = roughness height
;        S  = bed slope (assumed equal to friction slope)

;        g     = 9.81 = gravitation constant [m/s^2]
;        kappa = 0.41 = von Karman's constant
;        aval  = 0.48 = integration constant

;        sqrt(g)/kappa = 7.6393d
;        smoothness = (aval / z0) * d
;        f = (kappa / alog(smoothness))^2d
;        tau_bed = rho_w * f * u^2 = rho_w * g * d * S

;        d, S, and z0 can be arrays.

;        To make default z0 correspond to default
;        Manning's n, can use this approximation:
;        z0 = a * (2.34 * sqrt(9.81) * n / kappa)^6d
;        For n=0.03, this gives: z0 = 0.011417
;        However, for n=0.3, it gives: z0 = 11417.413
;        which is 11.4 km!  So the approximation only
;        holds within some range of values.
;---------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

if (n_elements(z0) eq 0) then z0 = 0.011417d   ;(about 1 cm)


smoothness = (0.476d / z0) * d


;-----------------------------
;Make sure (smoothness gt 1d)
;-----------------------------
smoothness = (smoothness > 1.1d)

u = 7.676d * sqrt(d * S) * alog(smoothness)
;*** u = 7.6393d * sqrt(d * S) * alog(smoothness)

;*****************************
;Add a hydraulic jump option
;for when u gets too big ??
;*****************************

;------------------------------------
;Option to return friction factor, f
;------------------------------------
;***  f = (0.41d / alog(smoothness))^2d

RETURN, u
END;    Law_of_the_Wall
;*******************************************************************
function Free_Surface_Slope, S_bed, d, ds, pIDs

;------------------------------------------------------------
;Notes:  It is assumed that the flow directions don't change
;        even though the free surface is changing.
;------------------------------------------------------------
S_free = S_bed + ((d - d[pIDs]) / ds)

;------------------------------------------
;Don't do this; negative slopes are needed
;to decelerate flow in dynamic wave case
;and for backwater effects.
;------------------------------------------
;Set negative slopes to zero
;----------------------------
;*** S_free = (S_free > 0d)

RETURN, S_free
END;    Free_Surface_Slope
;***************************************************************
function Kinematic_Velocity, d, S_bed, cv

;---------------------------------------------------------
;Notes:  Compute u from d and S_bed.  (7/13/05 version)

;        cv         = channel vars structure
;        cv.nvals   = ptr to Manning's n values (grid)
;        cv.z0_vals = ptr to z0 roughness values (grid)
;        cv.widths  = ptr to channel bottom widths (grid)
;        cv.angles  = ptr to channel bank angles (grid)

;        Could use slopes in cv also, but S_bed has
;        been modified from those values to impose a

;        minimum slope that is nonzero.

;        Rh = hydraulic radius (trapezoid here)     
;        Must use S_bed vs. S_free.
;---------------------------------------------------------
on_error, 2  ;(return to caller)

FORWARD_FUNCTION Trapezoid_R, Manning_Formula, Law_of_the_Wall

;----------------------
;Use Manning's formula
;----------------------
if (cv.MANNING) then begin
    Rh = Trapezoid_R(d, *cv.widths, *cv.angles)
    u  = Manning_Formula(Rh, S_bed, *cv.nvals)
    RETURN, u
endif

;------------------------------------
;Use the Logarithmic Law of the Wall
;------------------------------------
if (cv.LAW_OF_WALL) then begin
    Rh = Trapezoid_R(d, *cv.widths, *cv.angles)
    u  = Law_of_the_Wall(Rh, S_bed, *cv.z0vals)
    RETURN, u
endif

END;  Kinematic_Velocity
;*******************************************************************
function Overland_Flow, y, S, B, N

;---------------------------------------------------------

;Notes:  N = Manning's roughness coefficient
;        B = projected length perpendicular to flow [m]
;            (Get this from the Flow_Width function.)
;        S = bed slope (assumed equal to friction slope)
;        y = flow depth [m]

;        y, S, B and N may all be 2D arrays.

;        If length units are all *feet*, then an extra
;        factor of 1.49 must be applied.  If units are
;        meters, no such factor is needed.
;---------------------------------------------------------


;-----------
;Error trap
;-----------
No_Catch, status

Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L


if NOT(keyword_set(N)) then N=0.3  ;(see page 1036)

q_over = (B / N) * sqrt(S) * y^(5d / 3d)

RETURN, q_over
END;    Overland_Flow
;***********************************************************************
function Hydrograph, Q_file, ID

;-------------------------------------------------------
;NOTES:  This function computes the hydrograph for any
;        pixel in the DEM, given its ID.  The ID is
;        given by: ID=(nx * yDEM) + xDEM, where nx is
;        the number of samples or columns in the DEM,
;        and (xDEM,yDEM) are the array subscripts of
;        the pixel in question.

;        The RiverTools Read_Grid routine is useful in
;        connection with this routine. The command:
;        Read_Grid,areas,filename, can be used to return
;        the upstream areas, which can then be explored
;        with IDL commands like MIN, MAX, and WHERE.
;-------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

Open_RTS_File, Q_file, Q_unit, Q_grids, N_GRIDS=n_grids, $
               /READ, /ASSOCIATED

h = fltarr(n_grids)
for n=0L,(n_grids - 1L) do h[n]=Q_grids[ID,n]

;**h = Q_grids[ID, 0L:n_grids-1L]

free_lun, Q_unit

RETURN, h
END;    Hydrograph

;***************************************************************
function Rain_Volume, basin_area, pv, RTM_file

;--------------------------------------------------------
;NOTES:  pv = precip_vars = structure

;        Rate is a 1D vector with units of [m/s].
;        Duration is a 1D vector with units of minutes.
;        Basin area is assumed to have units of [km^2].
;        Rain volume then has units of [m^3].

;        To compute rain volume for a basin when
;        rainrate is nonuniform, would need to use
;        a basin mask.
;--------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

volume = 0d

if (basin_area le 0) then RETURN, volume

if (pv.method eq 1b) then begin
    ;--------------------------------
    ;pv.rates and pv.durations point
    ;to 1D arrays of values
    ;-------------------------------- 
    nr = n_elements(*pv.rates)
    for k=0L,(nr - 1L) do begin
        R      = (*pv.rates)[k]
        T      = (*pv.durations)[k]
        volume = (volume + (R * T * 60d))
    endfor
    volume = (volume * basin_area * 1000000d)
endif

RETURN, volume
END;    Rain_Volume
;***************************************************************
function Hydrograph_Integral, Q, dt

;----------------------------------------------------------
;NOTES:  Q is a 1D array (hydrograph) with units (m^3/s).
;        dt is the time interval, in seconds, between
;        successive values of Q.  This dt may be much
;        larger than the overall dt in Route_Flow.
;----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

value = 0d
nq = n_elements(Q)



for k=0L,(nq-1L) do begin
    value = (value + (Q[k]*dt))
endfor

RETURN, value
END;    Hydrograph_Integral
;***************************************************************

