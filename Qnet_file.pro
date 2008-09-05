
;******************************************************************
;   Qnet_file.pro

;   Copyright (c) 2005-2007, Scott D. Peckham
;   Created:    Jul-Aug 2005 (shortwave radiation only)
;   Modified:   Feb-Mar 2007 (longwave radiation tools)

;   References: Dingman, S.L. (2002) Physical Hydrology,
;                    Appendix E.
;               Kasten and Young (1989) Optical air mass eqn.
;               Whitman, A.M. (2003) A Simple Expression for
;                    the Equation of Time (online document)
;               http://www.sunspot.noao.edu/sunspot/pr/
;                     answerbook/expl-5.html
;               Various websites (links were saved)
;               Glen Liston's papers
;               see also D. Tarboton's papers (w/ C. Luce)
;               see also Marks and Dozier (1992) WRR paper
;               see also Ch.8 in Geomorphometry book (Boehner)

;   Notes:      NB!  Not yet ready to create grids of lats &
;               lons for DEM with fixed-length pixels (e.g. UTM).

;   Notes:      Functions that should be double-checked include:
;                  Vernal_Equinox, Earth_Perihelion and
;                  ET_Radiation_Flux_Slope

;******************************************************************

;----------------
;   Functions
;----------------
;   Solar_Constant
;   Day_Angle
;   Eccentricity_Correction
;   Declination
;   Earth_Angular_Velocity  ;*** (See Earth_Rotation_Rate)
;   Zenith_Angle
;   Sunrise_Offset
;   Sunset_Offset
;   ET_Radiation_Flux
;------------------------------
;   Saturation_Vapor_Pressure     ;(moved to formulas.pro)
;   Vapor_Pressure                ;(moved to formulas.pro)
;   Dew_Point
;   Precipitable_Water_Content
;   Optical_Air_Mass  ;***
;   Dust_Attenuation
;   Atmospheric_Transmissivity
;   Direct_Radiation_Flux
;------------------------------
;   Scattering_Attenuation
;   Diffuse_Radiation_Flux
;------------------------------
;   Global_Radiation_Flux
;   BS_Radiation_Flux
;------------------------------
;   Longitude_Offset
;   Equivalent_Latitude
;   Noon_Offset_Slope
;   Sunrise_Offset_Slope
;   Sunset_Offset_Slope
;   Day_Length_Slope
;   ET_Radiation_Flux_Slope
;   Clear_Sky_Radiation
;----------------------------
;   Julian_Day                ;***
;   Days_Per_Year             ;***
;   Earth_Rotation_Rate       ;***
;   Earth_Tilt_Angle          ;***
;   Earth_Orbit_Eccentricity  ;***
;   Vernal_Equinox            ;***
;   Earth_Perihelion          ;***
;   Equation_of_Time          ;***
;   True_Solar_Noon           ;***
;----------------------------
;   Latitude_Grid
;   Longitude_Grid
;   Make_RTS_File_for_Qnet_SW      (procedure)
;   Get_Time_Zone_List
;   GUI_Make_Qnet_SW_File_event
;   GUI_Make_Qnet_SW_File

;---------------------------------------------
;   Routines for longwave radiation, 2/21/07
;---------------------------------------------
;   Compare_em_air_Methods         (3/11/07)   **** CHECK MORE ****
;   Make_RTS_File_for_Qnet_LW
;   Check_Number_of_Frames         (3/9/07)
;   GUI_Make_Qnet_LW_File_event
;   GUI_Make_Qnet_LW_File

;***************************************************************
function Solar_Constant

RETURN, 1367d   ;[Watts / m^2]

end;  Solar_Constant
;***************************************************************
function Day_Angle, Julian_day, DEGREES=DEGREES

;-------------------------------------------------------
;Notes:  The Julian day does not need to be an integer;
;        decimal values can be used for more precision.
;-------------------------------------------------------

angle = (2d * !dpi) * (Julian_day-1d) / 365d

if (keyword_set(DEGREES)) then angle = angle * (180d / !dpi)

RETURN, angle

end;  Day_Angle
;***************************************************************
function Eccentricity_Correction, day_angle

E0 =  1.000110d + $
     (0.034221d * cos(day_angle)) + $
     (0.001280d * sin(day_angle)) + $
     (0.000719d * cos(2d * day_angle)) + $
     (0.000077d * sin(2d * day_angle))
   
RETURN, E0   ;[unitless, ratio of two lengths]

end;  Eccentricity_Correction
;***************************************************************
function Declination, day_angle, DEGREES=DEGREES, DMS=DMS

;---------------------------------------------------------
;Note:  The declination reaches its lowest value of -23.5
;       degrees on the Winter Solstice (Dec. 21/22) and
;       reaches its highest value of 23.5 degrees on the
;       Summer Solstice (June 21/22).  It is zero for
;       both the Vernal Equinox (Mar. 20/21) and the
;       Autumnal Equinox (Sept. 22/23).  The value of
;       23.4397 degrees is the fixed tilt angle of the
;       Earth's axis from from the plane of the ecliptic.
;---------------------------------------------------------
DMS     = keyword_set(DMS)
DEGREES = keyword_set(DEGREES)

delta = 0.006918d - $
        (0.399912d * cos(day_angle)) + $
        (0.070257d * sin(day_angle)) - $
        (0.006758d * cos(2d * day_angle)) + $
        (0.000907d * sin(2d * day_angle)) - $
        (0.002697d * cos(3d * day_angle)) + $
        (0.001480d * sin(3d * day_angle))

;----------------------------------
;Convert from radians to degrees ?
;----------------------------------
if (DEGREES) then begin
    delta = delta * (180d / !dpi)
endif

;--------------------------------------
;Convert from radians to "decimal DMS"
;--------------------------------------
if (DMS) then begin
    delta  = delta * (180d / !dpi)  ;[decimal degrees]
    deg    = fix(delta)
    min    = fix((delta - deg)*60d)
    sec    = fix(((delta - deg)*60d - min)*60d)
    delta  = deg + (min/100d) + (sec/10000d)   ;[decimal DMS, DD.MMSS]
endif

RETURN, delta

end;  Declination
;***************************************************************
function Earth_Angular_Velocity

;-------------------------------------------------
;Notes:  Compare to Earth_Rotation_Rate function.
;-------------------------------------------------
deg_per_hour = 360d / 24d   ;(equals 15)
rad_per_hour = deg_per_hour * (!dpi / 180d)

RETURN, rad_per_hour

end;  Earth_Angular_Velocity
;***************************************************************
function Zenith_Angle, lat_deg, declination, th

;-------------------------------------------------------
;Notes: lat_deg has units of DEGREES and declination
;       must have units of RADIANS.

;       th is number of hours before (-) or or after (+)
;       the true solar noon.

;       Sunrise and sunset occur when zenith angle is
;       equal to pi/2, so cos(Z)=0 and we can then
;       solve for time offsets as th.

;-------------------------------------------------------
omega   = Earth_Angular_Velocity()   ;[radians / hour]
lat_rad = lat_deg * (!dpi / 180d)
term1   = sin(lat_rad) * sin(declination)
term2   = cos(lat_rad) * cos(declination) * cos(omega * th)

RETURN, acos(term1 + term2)   ;[radians]

end;  Zenith_Angle
;***************************************************************
function Sunrise_Offset, lat_deg, declination

;--------------------------------------------------------
;Notes: lat and declination must have units of RADIANS.
;       time has units of hours before true solar noon.

;       If (abs(lat_deg) gt 66.5) we are above Arctic
;       circle or below Antarctic circle.  This can also
;       happen if lat_deg is an "equivalent latitude"
;       for a slope.  In this case, the absolute value
;       of the argument to ACOS can exceed one and there
;       is either no sunrise or no sunset at the given
;       location.

;--------------------------------------------------------
omega   = Earth_Angular_Velocity()
lat_rad = lat_deg * (!dpi / 180d)

;----------------------------------------------
;See Notes for TF_Tan function in utils_TF.pro
;----------------------------------------------
arg     = -1d * TF_Tan(lat_rad) * TF_Tan(declination)
arg     = (-1d > arg) < 1d
time    = (-1d * acos(arg) / omega)

RETURN, time

end;  Sunrise_Offset
;***************************************************************
function Sunset_Offset, lat_deg, declination

;-------------------------------------------------------
;Notes: lat and declination must have units of RADIANS.
;       time has units of hours before true solar noon.

;       If (abs(lat_deg) gt 66.5) we are above Arctic
;       circle or below Antarctic circle.  This can also
;       happen if lat_deg is an "equivalent latitude"
;       for a slope.  In this case, the absolute value
;       of the argument to ACOS can exceed one and there
;       is either no sunrise or no sunset at the given
;       location.
;-------------------------------------------------------
omega   = Earth_Angular_Velocity()
lat_rad = lat_deg * (!dpi / 180d)

;----------------------------------------------
;See Notes for TF_Tan function in utils_TF.pro
;----------------------------------------------
arg     = -1d * TF_Tan(lat_rad) * TF_Tan(declination)
arg     = (-1d > arg) < 1d
time    = (acos(arg) / omega)

RETURN, time

end;  Sunset_Offset
;***************************************************************
function ET_Radiation_Flux, lat_deg, Julian_day, th

;----------------------------------------------------------
;Notes:  This is the instantaneous extraterrestrial
;        radiation flux on a horizontal plane at a time
;        th hours before (-) or after (+) true solar noon.
;----------------------------------------------------------

I_sc    = Solar_Constant()            ;[W / m^2]
omega   = Earth_Angular_Velocity()    ;[radians / hour]
;---------------------------------
Gamma   = Day_Angle(Julian_day)              ;[radians]
delta   = Declination(Gamma)                 ;[radians]
E0      = Eccentricity_Correction(Gamma)     ;[unitless]
lat_rad = lat_deg * (!dpi / 180d)
;------------------------------------------------------
term1   = cos(delta) * cos(lat_rad) * cos(omega * th)
term2   = sin(delta) * sin(lat_rad)
K_ET    = I_sc * E0 * (term1 + term2)

;-------------------------
;This shouldn't be needed
;-------------------------
K_ET = (K_ET > 0d)

RETURN, K_ET    ;[Watts / m^2]

end;  ET_Radiation_Flux
;***************************************************************
function Dew_Point, T, rel_humidity

;-------------------------------------------------------
;Notes:  Temps are in degrees C, and vapor pressure
;        units are kPa.  Relative humidity is unitless.
;-------------------------------------------------------
FORWARD_FUNCTION Vapor_Pressure

vp  = Vapor_Pressure(T, rel_humidity)
top = alog(vp) + 0.4926d
bot = 0.0708d - 0.00421d * alog(vp)
Td  = (top / bot)

RETURN, Td

end;  Dew_Point
;***************************************************************
function Precipitable_Water_Content, T, rel_humidity

Td = Dew_Point(T, rel_humidity)   ;[degrees C]
Wp = 1.12d * exp(0.0614d * Td)    ;[centimeters]

RETURN, Wp  ;[centimeters]

end;  Precipitable_Water_Content
;***************************************************************
function Optical_Air_Mass, lat_deg, declination, th

;--------------------------------------------------------
;Notes: This is a dimensionless number that gives the
;       relative path length (greater than 1) that
;       radiation must travel through the atmosphere as
;       the result of not entering at a right angle.

;       t is number of hours before (-) or or after (+)
;       the true solar noon.

;       Dingman gives only a table (Figure E-4, p. 605)
;       of daily average values as a function of lat
;       and declination.

;       The approximation formula used here is widely
;       used and is from Kasten and Young (1989).
;--------------------------------------------------------
Z     = Zenith_Angle(lat_deg, declination, th)  ;[radians]
term1 = (96.07995d - Z)^(-1.6364d)
denom = cos(Z) + (0.50572d * term1)
M_opt = (1d / denom)

RETURN, M_opt   ;[unitless]

end;  Optical_Air_Mass
;***************************************************************
function Dust_Attenuation

;--------------------------------------------------------
;Notes:  Typical clear-sky values are between 0 and 0.2.
;        Bolsenga (1964) cites values of:
;            0.00 to 0.05,   remote stations
;            0.03 to 0.10,   moderate-sized cities
;            0.10 to 0.13,   larger metro. areas 
;        See Dingman, p. 604-605.
;--------------------------------------------------------
RETURN, 0.08

end;  Dust_Attenuation
;***************************************************************
function Atmospheric_Transmissivity, lat_deg, Julian_day, $
                                     T, rel_humidity, th, $
                                     gamma_dust

;---------------------------------------------
;Notes:  T is air temp in degrees C.
;        lat is latitude in decimal degrees.
;        Relative humidity is unitless.
;        Atmos. Trans = tau = unitless and
;          must be between zero and one.
;---------------------------------------------
if (n_elements(gamma_dust) eq 0) then $
    gamma_dust = Dust_Attenuation()
;------------------------------------
Gamma   = Day_Angle(Julian_day)             ;[radians]
delta   = Declination(Gamma)                ;[radians]
;------------------------------------
W_p     = Precipitable_Water_Content(T, rel_humidity)  ;[cm]
a_sa    = -0.1240d - (0.0207d * W_p)
b_sa    = -0.0682d - (0.0248d * W_p)
M_opt   = Optical_Air_Mass(lat_deg, delta, th) ;[unitless]
tau_sa  = exp(a_sa + (b_sa * M_opt))
tau     = (tau_sa - gamma_dust)          ;[unitless]

RETURN, (tau > 0d) < 1d

end;  Atmospheric_Transmissivity
;***************************************************************
function Direct_Radiation_Flux, lat_deg, Julian_day, T, $
                                rel_humidity, th, $
                                gamma_dust

if (n_elements(gamma_dust) eq 0) then $
    gamma_dust = Dust_Attenuation()
tau   = Atmospheric_Transmissivity(lat_deg, Julian_day, $
                                   T, rel_humidity, th, $
                                   gamma_dust)
K_ET  = ET_Radiation_Flux(lat_deg, Julian_day, th)
K_dir = (tau * K_ET)

RETURN, K_dir

end;  Direct_Radiation_Flux
;***************************************************************
function Scattering_Attenuation, lat_deg, Julian_day, $
                                 T, rel_humidity, th, $
                                 gamma_dust

if (n_elements(gamma_dust) eq 0) then $
    gamma_dust = Dust_Attenuation()
;-----------------------------------
Gamma   = Day_Angle(Julian_day)           ;[radians]
delta   = Declination(Gamma)              ;[radians]
;-----------------------------------
W_p     = Precipitable_Water_Content(T, rel_humidity)
a_s     = -0.0363d - (0.0084d * W_p)
b_s     = -0.0572d - (0.0173d * W_p)
M_opt   = Optical_Air_Mass(lat_deg, delta, th)  ;[unitless]
tau_s   = exp(a_s + (b_s * M_opt))
gam_s   = (1d - tau_s) + gamma_dust 

RETURN, gam_s

end;  Scattering_Attenuation
;***************************************************************
function Diffuse_Radiation_Flux, lat_deg, Julian_day, T, $
                                 rel_humidity, th, gamma_dust

;** if (n_elements(gamma_dust) eq 0) then $
;**     gamma_dust = Dust_Attenuation()

gam_s = Scattering_Attenuation(lat_deg, Julian_day, T, $
                               rel_humidity, th, gamma_dust)  ;[unitless]
K_ET  = ET_Radiation_Flux(lat_deg, Julian_day, th)
K_dif = 0.5d * gam_s * K_ET

RETURN, K_dif   ;[Watts / meter^2]

end;  Diffuse_Radiation_Flux
;***************************************************************
function Global_Radiation_Flux, lat_deg, Julian_day, $
                                T, rel_humidity, th, $
                                gamma_dust

if (n_elements(gamma_dust) eq 0) then $
    gamma_dust = Dust_Attenuation()
K_dir = Direct_Radiation_Flux(lat_deg, Julian_day, T, $
                              rel_humidity, th, gamma_dust)

K_dif = Diffuse_Radiation_Flux(lat_deg, Julian_day, T, $
                               rel_humidity, th, gamma_dust)

K_global = (K_dir + K_dif)

RETURN, K_global   ;[Watts / m^2]

end;  Global_Radiation_Flux
;***************************************************************
function BS_Radiation_Flux, lat_deg, Julian_day, T, $
                            rel_humidity, albedo, th, $
                            gamma_dust

;--------------------------------------------------------
;Notes:  Compute the backscattered radiation flux.
;        A table of typical albedos is given by Dingman,
;        Table D-2 on page 584.
;--------------------------------------------------------
if (n_elements(gamma_dust) eq 0) then $
    gamma_dust = Dust_Attenuation()
gam_s = Scattering_Attenuation(lat_deg, Julian_day, T, $
                               rel_humidity, th, gamma_dust)

K_global = Global_Radiation_Flux(lat_deg, Julian_day, T, $
                                 rel_humidity, th, gamma_dust)

;--------------
;For debugging
;--------------
;print,'min(gam_s),  max(gam_s)  = ', min(gam_s), max(gam_s)
;print,'min(albedo), max(albedo) = ', min(albedo), max(albedo)
;print,'min(K_global), max(K_global) = ', min(K_global), max(K_global)

K_bs = 0.5d * gam_s * albedo * K_global

RETURN, K_bs

end;  BS_Radiation_Flux
;***************************************************************
function Longitude_Offset, lat_deg, alpha, beta

;-----------------------------------------------------------
;Notes:  beta  = "slope angle" satisfies slope = tan(beta).
;        alpha = "aspect_angle" or azimuth is measured
;                clockwise from north.
;        Both angles have units of radians.
;        Returned value, dlon, has units of radians.

;        If (alpha eq 0) or (beta eq 0) then (term1 eq 0)
;        and this offset will be 0.

;-----------------------------------------------------------
lat_rad = lat_deg * (!dpi / 180d)
term1   = sin(beta) * sin(alpha)
term2   = cos(beta) * cos(lat_rad)
term3   = sin(beta) * sin(lat_rad) * cos(alpha)
dlon    = atan(term1 / (term2 - term3))

RETURN, dlon  ;[radians]

end;  Longitude_Offset
;***************************************************************
function Equivalent_Latitude, lat_deg, alpha, beta, $
                              DEGREES=DEGREES


;-----------------------------------------------------------
;Notes:  beta  = "slope angle" satisfies slope = tan(beta).
;        alpha = "aspect_angle" or azimuth is measured
;                clockwise from north.
;        Both angles have units of radians.

;        Note that if (beta eq 0), then lat_eq (in deg) is
;        always equal to lat_deg.  Also, beta will always
;        be in the range [0, pi/2].
;-----------------------------------------------------------
lat_rad = lat_deg * (!dpi / 180d)
term1   = sin(beta) * cos(alpha) * cos(lat_rad)
term2   = cos(beta) * sin(lat_rad)

eq_lat  = asin(term1 + term2)

;-------------------------------------
;Convert to degrees?  Sunrise_Offset
;function requires lat in degrees.
;-------------------------------------
if (keyword_set(DEGREES)) then begin
    eq_lat = eq_lat * (180d/!dpi)
    RETURN, eq_lat    ;[degrees]
endif else begin
    RETURN, eq_lat    ;[radians]
endelse


end;  Equivalent_Latitude
;***************************************************************
function Noon_Offset_Slope, lat_deg, alpha, beta

dlon   = Longitude_Offset(lat_deg, alpha, beta)  ;[radians]
omega  = Earth_Angular_Velocity()
t_noon = -1d * dlon / omega

RETURN, t_noon

end;  Noon_Offset_Slope
;***************************************************************
function Sunrise_Offset_Slope, lat_deg, Julian_day, alpha, beta

;-----------------------------------------------------------
;Notes:  beta  = "slope angle" satisfies slope = tan(beta).
;        alpha = "aspect_angle" or azimuth is measured
;                clockwise from north.
;        Both angles have units of radians.
;-----------------------------------------------------------
Gamma      = Day_Angle(Julian_day)         ;[radians]
delta      = Declination(Gamma)            ;[radians]
eq_lat_deg = Equivalent_Latitude(lat_deg, alpha, beta, /DEGREES)
;----------------------------------------------------
t_noon     = Noon_Offset_Slope(lat_deg, alpha, beta)
t_sr       = Sunrise_Offset(eq_lat_deg, delta)
t_sr       = t_sr + t_noon   ;(plus sign is correct)

;---------------------------------------------
;This is what Dingman does in his spreadsheet
;---------------------------------------------
t_sr = t_sr > Sunrise_Offset(lat_deg, delta)

RETURN, t_sr   ;[hours]

end;  Sunrise_Offset_Slope
;***************************************************************
function Sunset_Offset_Slope, lat_deg, Julian_day, alpha, beta

;-----------------------------------------------------------
;Notes:  beta  = "slope angle" satisfies slope = tan(beta).
;        alpha = "aspect_angle" or azimuth is measured
;                clockwise from north.
;        Both angles have units of radians.
;-----------------------------------------------------------
Gamma      = Day_Angle(Julian_day)         ;[radians]
delta      = Declination(Gamma)            ;[radians]
eq_lat_deg = Equivalent_Latitude(lat_deg, alpha, beta, /DEGREES)
;----------------------------------------------------
t_noon     = Noon_Offset_Slope(lat_deg, alpha, beta)
t_ss       = Sunset_Offset(eq_lat_deg, delta)
t_ss       = t_ss + t_noon

;---------------------------------------------
;This is what Dingman does in his spreadsheet
;---------------------------------------------
t_ss = t_ss < Sunset_Offset(lat_deg, delta)

RETURN, t_ss   ;[hours]

end;  Sunset_Offset_Slope
;***************************************************************
function Day_Length_Slope, lat_deg, Julian_day, alpha, beta

t_sr = Sunrise_Offset_Slope(lat_deg, Julian_day, alpha, beta)
t_ss = Sunset_Offset_Slope(lat_deg, Julian_day, alpha, beta)

RETURN, (t_ss - t_sr)  ;[hours]

end;  Day_Length_Slope
;***************************************************************
function ET_Radiation_Flux_Slope, lat_deg, Julian_day, $
                                  th, alpha, beta

;-----------------------------------------------------------
;Notes:  This is the instantaneous extraterrestrial
;        radiation flux on a sloping plane.
;-----------------------------------------------------------
;Notes:  beta  = "slope angle" satisfies slope = tan(beta).
;        alpha = "aspect_angle" or azimuth is measured
;                clockwise from north.
;        Both angles have units of radians.
;-----------------------------------------------------------
I_sc    = Solar_Constant()            ;[W / m^2]
omega   = Earth_Angular_Velocity()    ;[radians / hour]
;---------------------------------
Gamma   = Day_Angle(Julian_day)                ;[radians]
delta   = Declination(Gamma)                   ;[radians]
E0      = Eccentricity_Correction(Gamma)       ;[unitless]
;--------------------------------------------------------------
lat_eq  = Equivalent_Latitude(lat_deg, alpha, beta) ;[radians]
dlon    = Longitude_Offset(lat_deg, alpha, beta)    ;[radians]
;--------------------------------------------------------------
term1   = cos(delta) * cos(lat_eq)
term2   = cos((omega * th) + dlon)
term3   = sin(lat_eq) * sin(delta)
K_ET    = I_sc * E0 * ((term1 * term2) + term3)

;--------------
;For debugging
;-----------------------------------------
;NaNs should only occur because alpha and
;beta grids have them along the edges.
;-----------------------------------------
;;print, 'min(I_sc),  max(I_sc)  = ', min(I_sc),  max(I_sc)
;;print, 'min(E0),    max(E0)    = ', min(E0),    max(E0)
;print, 'min(term1), max(term1) = ', min(term1), max(term1)
;print, 'min(term2), max(term2) = ', min(term2), max(term2)
;print, 'min(term3), max(term3) = ', min(term3), max(term3)
;print,' '

;--------------------------
;This shouldn't be needed.
;--------------------------
K_ET = (K_ET > 0d)

RETURN, K_ET    ;[Watts / m^2]

end;  ET_Radiation_Flux_Slope
;***************************************************************
function Clear_Sky_Radiation, lat_deg, Julian_day, T_air, $
                              rel_humidity, t_offset, $
                              alpha, beta, albedo, $
                              gamma_dust

;------------------------------------------------------
;Notes:  I think K_cs is the same as the Qnet required
;        for the energy-balance routines in TopoFlow.
;        Both have units of Watts/m^2.
;------------------------------------------------------
;** if (n_elements(gamma_dust) eq 0) then $
;**     gamma_dust = Dust_Attenuation()
;------------------------------------------------------
tau   = Atmospheric_Transmissivity(lat_deg, Julian_day, T_air, $
                                   rel_humidity, t_offset, $
                                   gamma_dust)
K_ET  = ET_Radiation_Flux_Slope(lat_deg, Julian_day, $
                                t_offset, alpha, beta)
K_dif = Diffuse_Radiation_Flux(lat_deg, Julian_day, T_air, $ 
                               rel_humidity, t_offset, $
                               gamma_dust) 
K_bs  = BS_Radiation_Flux(lat_deg, Julian_day, T_air, $
                          rel_humidity, albedo, t_offset, $
                          gamma_dust)

K_cs = (tau * K_ET) + K_dif + K_bs

;--------------
;For debugging
;--------------
;print,'min(alpha), max(alpha) = ', min(alpha), max(alpha)
;print,'min(beta),  max(beta)  = ', min(beta), max(beta)
;--------------------------------------------------------------------
;print,'min(tau),   max(tau)   = ', min(tau),   max(tau)
;print,'min(K_ET),  max(K_ET)  = ', min(K_ET),  max(K_ET)
;print,'min(K_dif), max(K_dif) = ', min(K_dif), max(K_dif)
;print,'min(K_bs),  max(K_bs)  = ', min(K_bs),  max(K_bs)
;print,'min(K_cs),  max(K_cs)  = ', min(K_cs),  max(K_cs)
;print,' '

;---------------------------------------------
;Set K_cs to zero between (local) dusk & dawn
;NB!  These next two variables are GRIDS.
;---------------------------------------------
T_sr = Sunrise_Offset_Slope(lat_deg, Julian_day, alpha, beta)
T_ss = Sunset_Offset_Slope(lat_deg, Julian_day, alpha, beta)

;---------------------------------------------- 
;Use of LE & GE also takes care of case where
;Tsr = T_ss = 0, when abs(eq_lat_deg) gt 66.5.
;---------------------------------------------- 

dark = where((t_offset LE T_sr) OR (t_offset GE T_ss), n_dark)
if (n_dark ne 0) then K_cs[dark]=0d

RETURN, K_cs   ;[Watts / m^2]

end;  Clear_Sky_Radiation
;***************************************************************
function Julian_Day, month_num, day_num, hour_num
                     ;*** YEAR=YEAR  ;******

;----------------------------------------------------------
;NB!  month_num is an integer between 1 and 12 inclusive.
;     This function does not account for leap years.
;----------------------------------------------------------
month_days = [0,31,28,31,30,31,30,31,31,30,31,30,31]
JD = total(month_days[0:month_num-1]) + day_num

if (n_elements(hour_num) ne 0) then begin
    JD = JD + (hour_num / 24d)
endif

RETURN, JD

end;  Julian_Day
;***************************************************************
function Days_Per_Year, SIDEREAL=SIDEREAL

;---------------------------------------------------------
;Notes:  A day is typically defined as a mean solar day
;        and contains exactly 24 hours.  A year is then
;        the number of these days required for the Earth
;        to trace out one complete orbit, and is given by
;        365.24219 days (365.2422).  A year is also known
;        as a tropical year.

;        A sidereal day is the length of time that it
;        takes for the Earth to spin through 360 degrees
;        on its axis.

;        1 mean solar day = 86400 seconds = 24 hours

;        1 sidereal day   = 86164.09 secs = 23.934470 hrs.

;        This function returns the number of solar days
;        in one year (complete orbit) by default, but can
;        also return the number of sidereal days in one
;        year by setting the SIDEREAL keyword.
;   
;        An approximate value of 365.2425 for the number
;        solar days per year is used in accordance with
;        the Gregorian calendar.    
;--------------------------------------------------------
if (keyword_set(SIDEREAL)) then begin
    ;-----------------------------------
    ;Return the number of sidereal days
    ;-----------------------------------
    n_days = 366.2425d
endif else begin
    ;--------------------------------
    ;Return the number of solar days
    ;(Gregorian calendar convention)
    ;--------------------------------
    n_days = 365.2425d
endelse

RETURN, n_days   ;[days]

end;  Days_Per_Year
;***************************************************************
function Earth_Rotation_Rate, PER_HOUR=PER_HOUR

;-------------------------------------------------------
;Notes:  Compare to Earth_Angular_Velocity function.
;        The default is to return the rotation rate
;        as the total number of radians rotated in one
;        Earth orbit (tropical year).  About 2294.8863.
;----------------------------------------------------------
;        1 mean solar day = 86400 seconds = 24 hours
;        1 sidereal day   = 86164.09 secs = 23.934470 hrs.
;----------------------------------------------------------
if (keyword_set(PER_HOUR)) then begin
    Omega = 2d * !dpi / 24d   ;[radians / hour]
endif else begin
    DPY   = Days_Per_Year(/SIDEREAL)
    Omega = 2d * !dpi * DPY   ;[radians / year]
endelse

RETURN, Omega

end;  Earth_Rotation_Rate
;***************************************************************
function Earth_Tilt_Angle, DEGREES=DEGREES

;----------------------------------------------------
;Note:  The Earth's tilt angle is slowly decreasing.
;       It is also known as the "obliquity".
;----------------------------------------------------
angle = 23.4397d  ;[degrees]

DEGREES = keyword_set(DEGREES)
if NOT(DEGREES) then begin
    angle = angle * (!dpi / 180d)  ;[radians]
endif

RETURN, angle  ;[degrees or radians]

end;  Earth_Tilt_Angle
;***************************************************************
function Earth_Orbit_Eccentricity

;--------------------------------------------------
;Notes:  Return the eccentricity of Earth's orbit,
;        which measures the difference between its
;        elliptical orbit and a circular orbit.
;        It is computed as:  e = (b-a)/a, where
;        a and b are the semi-major and semi-minor
;        axes of the elliptical orbit.
;--------------------------------------------------
RETURN, 0.016713d

end;  Earth_Orbit_Eccentricity
;***************************************************************
function Vernal_Equinox, year

;---------------------------------------------------------------
;Notes:  This function assumes that vernal equinoxes from one
;        year to the next are separated by exactly 365.2425
;        days, or one tropical year, in accordance with the
;        Gregorian calendar.  The difference between using this
;        value and a "true" tropical year of 365.2422 days will
;        be about 2.88 hours in 400 years.

;        The time of vernal equinox for the year 2000 A.D. is
;        March 20, 7:36 GMT [NASA Reference Publication 1349,
;        Oct. 1994].

;        We assume here that the vernal equinox for year 2000
;        will be on March 20, 7:30, or 79.3125 days from 2000
;        January 1, hour 0.  Vernal equinoxes for other years
;        are returned as the total number of days since
;        2000 January 1, hour 0.

;        Note that:   79.3125 = 31 + 29 + 19 + 7.5/24.
;---------------------------------------------------------------
DPY     = Days_Per_Year()
VE_2000 = 79.3125d
VE      = VE_2000 + DPY*(year - 2000d)

RETURN, VE

end;  Vernal_Equinox
;***************************************************************
function Earth_Perihelion, year

;-----------------------------------------------------------
;NOTES:  Perihelion refers to the point along the orbit of
;        a planet around the sun where it is closest to the
;        sun.  For Earth, this typically occurs between the
;        dates of January 2 and January 5.  This function
;        returns the time when this event occurs as a
;        Julian date in the given year.
;-----------------------------------------------------------
if (n_elements(year) eq 0) then year = 2005
if ((year lt 1992) OR (year gt 2020)) then year = 2005

;----------------------------------
;Use published values from a table
;for the years 1992 to 2020.
;----------------------------------
Tp_years = indgen(29) + 1992
Tp_days  = [3,  4, 2,  4, 4, 2,  4,  3, 3, 4,  2, 4,  4, 2,  4,  3]
Tp_days  = [Tp_days,  3, 4, 3,  3, 5, 2,  4, 4,  2,  4, 3, 3, 5]
Tp_hours = [15, 3, 6, 11, 7, 0, 21, 13, 5, 9, 14, 5, 18, 1, 15, 20]
Tp_hours = [Tp_hours, 0, 15, 0, 19, 0, 5, 12, 7, 23, 14, 6, 5, 8]

;--------------------------------------
;Get day and hour from table for given
;year and convert to a Julian day.
;--------------------------------------
w        = where(Tp_years eq year)
Tp_vals  = Julian_Day(1, Tp_days, Tp_hours)
Tp_JD    = Tp_vals[w[0]]

RETURN, Tp_JD

end;  Earth_Perihelion
;***************************************************************
function Equation_of_Time, Julian_day, year, DEGREES=DEGREES, $
                           DMS=DMS

;-----------------------------------------------------
;Notes:  The so-called "equation of time" gives the
;        time difference between true solar noon and
;        local clock noon, without accounting for any
;        arbitrary time zone adjustments.  The latter
;        are determined by humans and would introduce
;        a whole-number offset from local clock noon
;        in hours.

;        The equation of time is closely related to
;        the figure-8-shaped "analemma".

;        Note that TE equals zero at 4 different
;        times during the year.

;        To test this against tables, try this:
;            IDL>  JD = dindgen(365) + 1d
;            IDL>  TE = Equation_of_Time(JD)
;            IDL>  plot, JD, TE
;            IDL>  Gamma = Day_Angle(JD)
;            IDL>  delta = Declination(Gamma)
;-----------------------------------------------------
DEGREES = keyword_set(DEGREES)
DMS     = keyword_set(DMS)

if (n_elements(year) eq 0) then year = 2005
if ((year lt 1992) OR (year gt 2020)) then year = 2005  ;*********

;------------------------------
;Eccentricity of Earth's orbit
;Computed as: e = (b-a)/a
;------------------------------
e = Earth_Orbit_Eccentricity()

;--------------------------------
;Earth's tilt angle or obliquity
;(which is slowly decreasing)
;--------------------------------
eps = Earth_Tilt_Angle()

;-------------------------------------
;Number of mean solar days that occur
;in one complete Earth orbit
;-------------------------------------
days_per_year = Days_Per_Year()

;----------------------------------
;Get Julian date of the perihelion
;----------------------------------
Tp_JD = Earth_Perihelion(year)

;--------------------------------------------------
;Compute the mean anomaly, or the angular distance
;from perihelion that would be travelled by a
;uniformly moving (mean) sun.  It is zero at the
;perihelion.
;--------------------------------------------------
twopi = 2d * !dpi
M     = (twopi / days_per_year) * (Julian_day - Tp_JD)    ;[radians]
M     = (M + twopi) mod twopi

;------------------------------------
;Get "longitude of the perihelion"
;--------------------------------------------------
;This is the angle between the semi-major axis
;(the line of apsides) and a line between the Sun
;and the Earth at the time of the Vernal Equinox.
;Note that celestial longitudes are measured from
;the Vernal Equinox (analogous to prime meridian).
;--------------------------------------------------
;omega is roughly equal to 4.9358 radians,
;or -77.20 degrees or 282.8 degrees.
;------------------------------------------
year0 = 2000
VE_JD = Vernal_Equinox(year0)
PT    = (365d + Tp_JD) - VE_JD   ;[days, about 287]
omega = twopi * (PT / days_per_year)   ;[radians]

;------------------------------------
;Compute "mean longitude of the sun"
;------------------------------------
L = (M + omega)   ;[radians]

;---------------------------
;Compute "equation of time"
;---------------------------
TE = (-2d * e * sin(M)) + (sin(2d*L)*(eps/2d)^2d)

;----------------------------------
;Convert from radians to degrees ?
;----------------------------------
if (DEGREES) then begin
    TE = TE * (180d / !dpi)
    RETURN, TE    ;[degrees]
endif

;--------------------------------------
;Convert from radians to "decimal DMS"
;--------------------------------------
if (DMS) then begin
    TE  = TE * (180d / !dpi)  ;[decimal degrees]
    deg = fix(TE)
    min = fix((TE - deg)*60d)
    sec = fix(((TE - deg)*60d - min)*60d)
    TE  = deg + (min/100d) + (sec/10000d)
    RETURN, TE   ;[decimal DMS, DD.MMSS]
endif

;--------------------------------------
;Earth's rotation rate (angular speed)
;--------------------------------------
spin_rate = Earth_Rotation_Rate(/PER_HOUR)    ;[radians / hour]

;------------------------------
;Convert from radians to hours
;------------------------------
TE = (TE / spin_rate)   ;[hours]
RETURN, TE   ;[hours]

end;  Equation_of_Time
;***************************************************************
function True_Solar_Noon, Julian_day, longitude, time_zone, $
                          DST_offset

;------------------------------------------------------------
;Notes:  We need to know the local clock time when True
;        Solar Noon occurs, since some of our equations
;        depend on the time offset in hours from True Solar
;        Noon. Note that TE may be negative or positive.

;        The time zone used by the location of interest
;        should be entered as an integer between 0 and 12,
;        with negative values for locations west of the
;        prime meridian.  LC is longitude correction and
;        should be negative for longitudes east of the
;        time zone's central meridian and positive other-
;        wise, since solar noon will occur earlier for
;        locations further to the east.  Note that some
;        countries, like Iceland, may lie entirely outside
;        of the time zone strip (i.e. the 15-degree wide
;        strip of longitudes) that they set their clocks by.
;-------------------------------------------------------------
;NB!     Should we add or subtract TE below ??  ***************************
;------------------------------------------------------------
;NB!     The effect of Daylight Savings Time can be obtained
;        by choosing an adjacent time zone number, or by
;        using the optional DST_offset argument.  Be aware
;        that different countries use different conventions.
;------------------------------------------------------------
time_zone_center_lon = time_zone * 15d         ;[degrees]
lon_diff = (time_zone_center_lon - longitude)  ;[degrees]
LC       = (lon_diff / 15d)   ;[hours]
TE       = Equation_of_Time(Julian_day)  ;[hours]
T_noon   = 12d + LC + TE      ;[hours; 24-hour military time]

;---------------------------
;Add or subtract 1 hour for
;Daylight Savings Time ?
;---------------------------
if (n_elements(DST_offset) ne 0) then $
    T_noon = T_noon + DST_offset

RETURN, T_noon

end;  True_Solar_Noon
;***************************************************************
function Latitude_Grid, info

;---------------------------
;Create a grid of latitudes
;---------------------------
if (info.pixel_geom eq 0b) then begin
    ;--------------------------------------
    ;Geographic coords, fixed-angle pixels
    ;Compute lats for pixel centers.
    ;--------------------------------------
    dy   = (info.yres / 3600d)  ;[arcsecs -> degrees]
    lats = (dindgen(info.nrows) * dy) + info.y_south_edge + (dy/2d)
    lats = rotate(float(lats), 2)
    ones = fltarr(info.ncols) + 1.0
    lat_deg = (ones # lats)

    ;** print,'min(lat_deg), max(lat_deg) = ', min(lat_deg), max(lat_deg)

endif else begin
    ;---------------------------------
    ;UTM coords, fixed-length pixels
    ;------------------------------------------------
    ;Must convert UTM coords to lats, which is a           ;***********
    ;complicated procedure. Do this with RiverTools?
    ;------------------------------------------------
    lat_deg = 0.0
    msg = ['SORRY: ', ' ', $
           'The DEM for this data set uses UTM coordinates ',$
           'and TopoFlow cannot yet convert UTM coordinates ',$
           'to Geographic coordinates (lon and lat). ', $
           ' ', $
           'A latitude value of 0.0 will be returned.', $
           ' ']
    GUI_Error_Message, msg 
endelse

RETURN, lat_deg

end;  Latitude_Grid
;***************************************************************
function Longitude_Grid, info

;----------------------------
;Create a grid of longitudes
;----------------------------
if (info.pixel_geom eq 0b) then begin
    ;--------------------------------------
    ;Geographic coords, fixed-angle pixels
    ;Compute lons for pixel centers.
    ;--------------------------------------
    dx   = (info.xres / 3600d)  ;[arcsecs -> degrees]
    lons = (dindgen(info.ncols) * dx) + info.x_west_edge + (dx/2d)
    lons = float(lons)
    ones = fltarr(info.nrows) + 1.0
    lon_deg = (lons # ones)

    ;** print,'min(lon_deg), max(lon_deg) = ', min(lon_deg), max(lon_deg)

endif else begin
    ;---------------------------------
    ;UTM coords, fixed-length pixels
    ;------------------------------------------------
    ;Must convert UTM coords to lons, which is a           ;***********
    ;complicated procedure. Do this with RiverTools?
    ;------------------------------------------------
    lon_deg = 0.0
    msg = ['SORRY: ', ' ', $
           'The DEM for this data set uses UTM coordinates ',$
           'and TopoFlow cannot yet convert UTM coordinates ',$
           'to Geographic coordinates (lon and lat). ', $
           ' ', $
           'A longitude value of 0.0 will be returned.', $
           ' ']
    GUI_Error_Message, msg
endelse

RETURN, lon_deg

end;  Longitude_Grid
;***************************************************************
pro Make_RTS_File_for_Qnet_SW, Qn_SW_RTS_file, RTI_file, $
             ;----------------------------------------
             start_month, start_day, start_hour, $
             stop_month,  stop_day,  stop_hour, $
             timestep, time_zone, $
             ;------------------------------------------
             T_air,    T_air_file,    T_air_type,    $
             RH,       RH_file,       RH_type,       $
             albedo,   albedo_file,   albedo_type,   $
             dust_att, dust_att_file, dust_att_type, $
             factor,   factor_file,   factor_type,   $
             slope,    slope_file,    slope_type,    $
             aspect,   aspect_file,   aspect_type,   $
             lon_deg,  lon_file,      lon_type,      $
             lat_deg,  lat_file,      lat_type,      $
             ;------------------------------------------
             MSG_BOX_ID=MSG_BOX_ID

;-------------------------------------------------------
;Notes:  If time is before local sunrise or after local
;        sunset then Qnet should be zero.
;-------------------------------------------------------

;----------------------------
;Read info from the RTI file
;----------------------------
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows

;------------------------------
;Create grids of lats and lons
;------------------------------
if (n_elements(lon_deg) eq 0) then begin
    lon_deg = Longitude_Grid(info)
    lat_deg = Latitude_Grid(info)
endif else begin
    if (lon_type eq 2) then $
        Read_Grid, lon_deg, lon_file, TYPE='FLOAT', /SILENT
    if (lat_type eq 2) then $
        Read_Grid, lat_deg, lat_file, TYPE='FLOAT', /SILENT
endelse

;------------------------------------
;If data type is grid, read the grid
;------------------------------------
if (T_air_type eq 2) then $
    Read_Grid, T_air, T_air_file, TYPE='FLOAT', /SILENT
if (RH_type eq 2) then $
    Read_Grid, RH, RH_file, TYPE='FLOAT', /SILENT
if (albedo_type eq 2) then $
    Read_Grid, albedo, albedo_file, TYPE='FLOAT', /SILENT
if (dust_att_type eq 2) then $
    Read_Grid, dust_att, dust_att_file, TYPE='FLOAT', /SILENT
if (factor_type eq 2) then $
    Read_Grid, factor, factor_file, TYPE='FLOAT', /SILENT

;----------------------------------------
;Read slope grid, convert to slope angle
;NB!  RT slope grids have NaNs on edges.
;----------------------------------------
Read_Grid, slopes, slope_file, TYPE='FLOAT', /SILENT
beta  = atan(slopes)
twopi = 2d * !dpi
beta  = (twopi + beta) mod twopi
;---------------------------------------
w_nan = where(finite(beta) eq 0, n_nan)
if (n_nan ne 0) then beta[w_nan]=0d
w_bad = where((beta lt 0d) OR (beta gt !dpi/2d), n_bad)
if (n_bad ne 0) then begin
    msg = ['ERROR:  Some slope angles are out of range.', ' ']
    result = GUI_Message(msg, /INFO, TITLE='ERROR MESSAGE')
    RETURN
endif

;----------------------------------------------
;Read aspect grid; Alpha must be CW from north  ;****************
;NB!  RT aspect grids have NaNs on edges.
;----------------------------------------------
Read_Grid, aspects, aspect_file, TYPE='FLOAT', /SILENT
alpha = (!dpi/2d) - aspects
twopi = 2d * !dpi
alpha = (twopi + alpha) mod twopi
;----------------------------------------
w_nan = where(finite(alpha) eq 0, n_nan)
if (n_nan ne 0) then alpha[w_nan]=0d

;---------------------------------
;Open files for input variables ?
;---------------------------------
if (T_air_type eq 1) OR (T_air_type eq 3) then begin
    TF_Get_LUN, T_air_unit, T_air_file
    openr, T_air_unit, T_air_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (T_air_type eq 3) then T_air = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (RH_type eq 1) OR (RH_type eq 3) then begin
    TF_Get_LUN, RH_unit, RH_file
    openr, RH_unit, RH_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (RH_type eq 3) then RH = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (albedo_type eq 1) OR (albedo_type eq 3) then begin
    TF_Get_LUN, albedo_unit, albedo_file
    openr, albedo_unit, albedo_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (albedo_type eq 3) then albedo = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (dust_att_type eq 1) OR (dust_att_type eq 3) then begin
    TF_Get_LUN, dust_att_unit, dust_att_file
    openr, dust_att_unit, dust_att_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (dust_att_type eq 3) then dust_att = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (factor_type eq 1) OR (factor_type eq 3) then begin
    TF_Get_LUN, factor_unit, factor_file
    openr, factor_unit, factor_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (factor_type eq 3) then factor = fltarr(nx, ny)
endif

;-----------------------
;Open RTS_file to write
;-----------------------
TF_Get_LUN, Qn_SW_RTS_unit, Qn_SW_RTS_file
openw, Qn_SW_RTS_unit, Qn_SW_RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)

;----------------------------------------------
;Get start & stop times as decimal Julian days
;----------------------------------------------
start_Julian_day = Julian_Day(start_month, start_day)
stop_Julian_day  = Julian_Day(stop_month, stop_day)
start_time = start_Julian_day + (start_hour / 24d)
stop_time  = stop_Julian_day  + (stop_hour  / 24d)

;------------------------------------
;Convert timestep from hours to days
;------------------------------------
timestep_JD = (timestep / 24d)

;------------------------------------
;Create the RTS file, frame by frame
;------------------------------------
for Julian_day=start_time, stop_time, timestep_JD do begin
    ;----------------------------------------
    ;Compute the offset from True Solar Noon
    ;clock_hour is in 24-hour military time
    ;but it can have a decimal part.
    ;----------------------------------------
    clock_hour = (Julian_day - fix(Julian_day)) * 24d
    solar_noon = True_Solar_Noon(Julian_day, lon_deg, time_zone)
    t_offset   = (clock_hour - solar_noon)    ;[hours]

    ;-------------------------
    ;Write a progress message
    ;-------------------------
    if (keyword_set(MSG_BOX_ID)) then begin
        jstr = 'Day = ' + TF_String(fix(Julian_day))
        hstr = 'Hour = ' + TF_String(clock_hour, FORMAT='(F5.2)')
        mstr = jstr + ', ' + hstr
        widget_control, MSG_BOX_ID, set_value=mstr
    endif

    ;------------------------------
    ;Read next values from files ?
    ;------------------------------
    if (T_air_type eq 1) then readf, T_air_unit, T_air
    if (T_air_type eq 3) then readu, T_air_unit, T_air
    ;------------------------------------------------------
    if (RH_type eq 1) then readf, RH_unit, RH
    if (RH_type eq 3) then readu, RH_unit, RH
    ;------------------------------------------------------
    if (albedo_type eq 1) then readf, albedo_unit, albedo
    if (albedo_type eq 3) then readu, albedo_unit, albedo
    ;-----------------------------------------------------------
    if (dust_att_type eq 1) then readf, dust_att_unit, dust_att
    if (dust_att_type eq 3) then readu, dust_att_unit, dust_att
    ;-----------------------------------------------------------
    if (factor_type eq 1) then readf, factor_unit, factor
    if (factor_type eq 3) then readu, factor_unit, factor

    ;-----------------------------------------
    ;Compute min/max Sunrise and Sunset times
    ;-----------------------------------------
    ;sunrise_offset = Sunrise_Offset_Slope(lat_deg, Julian_day, alpha, beta)
    ;sunrise_time   = solar_noon + sunrise_offset
    ;---------------------------------------------
    ;sunset_offset  = Sunset_Offset_Slope(lat_deg, Julian_day, alpha, beta)
    ;sunset_time    = solar_noon + sunset_offset
    ;---------------------------------------------
    ;print,'solar_noon     = ', min(solar_noon),   max(solar_noon)
    ;print,'sunrise_offset = ', min(sunrise_offset), max(sunrise_offset)
    ;print,'sunset_offset  = ', min(sunset_offset), max(sunset_offset)
    ;print,'sunrise_time   = ', min(sunrise_time), max(sunrise_time)
    ;print,'sunset_time    = ', min(sunset_time),  max(sunset_time)
    ;print,'-----------------------------------------------------'

    ;--------------------------------
    ;Compute Qnet grid for this time
    ;--------------------------------
    Qnet_SW = Clear_Sky_Radiation(lat_deg, Julian_day, T_air, $
                                  RH, t_offset, $
                                  alpha, beta, albedo, dust_att)

    ;-------------------------------------------
    ;Multiply by an optional factor that can be
    ;used to account for cloud or canopy cover
    ;-------------------------------------------
    Qnet_SW = (Qnet_SW * factor)

    ;---------------------------
    ;Make sure result is a grid
    ;---------------------------
    if (n_elements(Qnet_SW) eq 1) then begin
        Qnet_SW = Qnet_SW + fltarr(nx, ny)
    endif

    ;------------------------
    ;Write frame to RTS file
    ;------------------------
    writeu, Qn_SW_RTS_unit, float(Qnet_SW)
endfor


;-------------------------
;Write a finished message
;-------------------------
if (keyword_set(MSG_BOX_ID)) then begin
    widget_control, MSG_BOX_ID, set_value='Finished.'
endif

;----------------
;Close the files
;----------------
free_lun, Qn_SW_RTS_unit

end;  Make_RTS_File_for_Qnet_SW
;****************************************************************
pro Get_Time_Zone_List, time_zones, time_zones2

time_zones = ['GMT – 12', 'GMT – 11', 'GMT – 10', 'GMT – 9', 'GMT – 8', $
              'GMT – 7', 'GMT – 6', 'GMT – 5', 'GMT – 4', 'GMT – 3', $
              'GMT – 2', 'GMT – 1', 'GMT    ', 'GMT + 1', 'GMT + 2', $
              'GMT + 3', 'GMT + 4', 'GMT + 5', 'GMT + 6', 'GMT + 7', $
              'GMT + 8', 'GMT + 9', 'GMT + 10', 'GMT + 11', 'GMT + 12']
time_zones = time_zones + ' hours'
time_zones[11] = 'GMT - 1 hour'
time_zones[12] = 'GMT    '
time_zones[13] = 'GMT + 1 hour'


time_zones2 = [ $
'GMT-12: International Data Line West', $
'GMT-11: Midway Island, Samoa', $
'GMT-10: Hawaii', $
'GMT-9:  Alaska', $
'GMT-8:  Pacific Time (US & Canada), Tijuana ',$
'GMT-7:  Mountain Time (US & Canada)', $
'GMT-6:  Central Time (US & Canada), Central America', $
'GMT-5:  Eastern Time (US & Canada), Bogota, Lima',$
'GMT-4:  Atlantic Time (Canada), Santiago', $
'GMT-3:  Brasilia, Buenos Aires, Greenland ',$
'GMT-2:  Mid-Atlantic ', $
'GMT-1:  Azores, Cape Verde Island ',$
'GMT:    Greenwich Mean Time, London, Casablanca ',$
'GMT+1:  Amsterdam, Berlin, Madrid, Paris, Rome ',$
'GMT+2:  Athens, Beirut, Cairo, Istanbul, Minsk ',$ 
'GMT+3:  Baghdad, Kuwait, Moscow, Nairobi, Riyadh ',$
'GMT+4:  Abu Dhabi, Baku, Muscat, Tbilisi, Yerevan ',$
'GMT+5:  Ekaterinburg, Islamabad, Tashkent ',$
'GMT+6:  Almaty, Astana, Dhaka, Novosibirsk ',$ 
'GMT+7:  Bangkok, Hanoi, Jakarta, Krasnoyarsk ', $
'GMT+8:  Beijing, Hong Kong, Taipei ',$
'GMT+9:  Osaka, Tokyo, Seoul, Yakutsk ',$
'GMT+10: Brisbane, Canberra, Guam, Vladivostok ',$
'GMT+11: Magadan, New Caledonia, Solomon Is., ',$
'GMT+12: Auckland, Fiji, Kamchatka, Wellington ' ]

end;  Get_Time_Zone_List
;***************************************************************
pro GUI_Make_Qnet_SW_File_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;******************
'START_MONTH' : $
;******************
state.start_month = (event.index + 1)

;*****************
'STOP_MONTH' : $
;*****************
state.stop_month = (event.index + 1)

;****************
'TIME_ZONE' : $
;****************
state.time_zone = (event.index - 12)

;***********************************************************
'T_AIR_TYPE'    : state.T_air_type    = event.index
'RH_TYPE'       : state.RH_type       = event.index
'ALBEDO_TYPE'   : state.albedo_type   = event.index
'DUST_ATT_TYPE' : state.dust_att_type = event.index
'FACTOR_TYPE'   : state.factor_type   = event.index
'SLOPE_TYPE'    : state.slope_type    = event.index
'ASPECT_TYPE'   : state.aspect_type   = event.index
'LON_TYPE'      : state.lon_type      = (event.index * 2b)
'LAT_TYPE'      : state.lat_type      = (event.index * 2b)
;***********************************************************

;************
'START' : $
;************
begin
;---------------------------------
;Get months selected via droplist
;---------------------------------
start_month = state.start_month
stop_month  = state.stop_month
time_zone   = state.time_zone
;------------------------
;Read start day and hour
;------------------------
Read_Text_Box, state.start_day_ID, start_day, OK, /INTEGER
if NOT(OK) then RETURN
Read_Text_Box, state.start_hour_ID, start_hour, OK, /FLOAT
if NOT(OK) then RETURN
;-----------------------
;Read stop day and hour
;-----------------------
Read_Text_Box, state.stop_day_ID, stop_day, OK, /INTEGER
if NOT(OK) then RETURN
Read_Text_Box, state.stop_hour_ID, stop_hour, OK, /FLOAT
if NOT(OK) then RETURN
;---------------------------
;Read the timestep in hours
;---------------------------
Read_Text_Box, state.timestep_ID, timestep, OK, /FLOAT
if NOT(OK) then RETURN

;--------------------------------------
;Read name of new RTS file for Qnet_SW
;--------------------------------------
Read_Text_Box, state.Qn_SW_RTS_file_ID, Qn_SW_RTS_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, Qn_SW_RTS_file, OK
if NOT(OK) then RETURN
;----------------------
;Read name of RTI file
;----------------------
Read_Text_Box, state.RTI_file_ID, RTI_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(RTI_file)
if NOT(OK) then RETURN

;--------------------------
;Read additional variables
;--------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.RH_type, state.RH_ID, RH, $
                 OK, filename=RH_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.albedo_type, state.albedo_ID, albedo, $
                 OK, filename=albedo_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.dust_att_type, state.dust_att_ID, dust_att, $
                 OK, filename=dust_att_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.factor_type, state.factor_ID, factor, $
                 OK, filename=factor_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.slope_type, state.slope_ID, slope, $
                 OK, filename=slope_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.aspect_type, state.aspect_ID, aspect, $
                 OK, filename=aspect_file
if NOT(OK) then RETURN

;-------------------
;Read lon and lat ?
;-------------------
if (state.lon_ID ne 0L) then begin
    Read_Input_Type, state.lon_type, state.lon_ID, lon, $
                     OK, filename=lon_file
    if NOT(OK) then RETURN
    ;-------------------------------------------------------
    Read_Input_Type, state.lat_type, state.lat_ID, lat, $
                     OK, filename=lat_file
    if NOT(OK) then RETURN
endif

;--------------------------------------------------------
;Get number of frames in new RTS file from time settings
;--------------------------------------------------------
;Get start & stop times as decimal Julian days
;----------------------------------------------
Julian_start_day = Julian_Day(start_month, start_day, start_hour)
Julian_stop_day  = Julian_Day(stop_month,  stop_day,  stop_hour)
timestep_JD      = (timestep / 24d)   ;([hours] -> [days])
nf_match         = long((Julian_stop_day - Julian_start_day) / timestep_JD)
;-----------------------------------------------
;Check if number of frames in RTS files matches
;the number obtained from the time settings
;-----------------------------------------------
if (state.T_air_type eq 3) then $
    Check_Number_of_Frames, T_air_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;-----------------------------------------------------------------
if (state.RH_type eq 3) then $
    Check_Number_of_Frames, RH_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;-----------------------------------------------------------------
if (state.albedo_type eq 3) then $
    Check_Number_of_Frames, albedo_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;-----------------------------------------------------------------
if (state.dust_att_type eq 3) then $
    Check_Number_of_Frames, dust_att_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;-----------------------------------------------------------------
if (state.factor_type eq 3) then $
    Check_Number_of_Frames, factor_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN

;---------------------------------------
;Call routines that create the RTS file
;---------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Make_RTS_File_for_Qnet_SW, Qn_SW_RTS_file, RTI_file, $
                  ;----------------------------------------
                  start_month, start_day, start_hour, $
                  stop_month,  stop_day,  stop_hour, $
                  timestep, time_zone, $
                  ;------------------------------------------------
                  T_air,    T_air_file,    state.T_air_type,    $
                  RH,       RH_file,       state.RH_type,       $
                  albedo,   albedo_file,   state.albedo_type,   $
                  dust_att, dust_att_file, state.dust_att_type, $
                  factor,   factor_file,   state.factor_type,   $
                  slope,    slope_file,    state.slope_type,    $
                  aspect,   aspect_file,   state.aspect_type,   $
                  lon,      lon_file,      state.lon_type,      $
                  lat,      lat_file,      state.lat_type,      $
                  ;------------------------------------------------
                  MSG_BOX_ID=state.msg_box_ID
widget_control, event.ID, sensitive=1    ;(enable button)

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating Qnet-SW file.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")
;*** Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'shortwave_calc.htm'

;************
'CLOSE' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CLOSE') AND $
   (uvalue ne 'START') then $
    widget_control, event.top, set_uvalue=state 

end;  GUI_Make_Qnet_SW_File_event
;****************************************************************
pro GUI_Make_Qnet_SW_File, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then leader=0L 

;-----------------------------------
;Get current values from main state
;-----------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;---------------------------------------
prefix         = mstate.run_vars.prefix
Qn_SW_RTS_file = prefix + '_Qn-SW.rts'
RTI_file       = prefix + '.rti'
Read_RTI_File, RTI_file, info, OK    ;(to get pixel_geom)
if NOT(OK) then RETURN

;--------------------------
;Get default string values
;--------------------------
start_day_str  = ' 1 '
start_hour_str = ' 0.0 '
stop_day_str   = ' 2 '
stop_hour_str  = ' 0.0 '
timestep_str   = ' 1.0 '

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, msg_box_ID:0L, $
Qn_SW_RTS_file_ID:0L, RTI_file_ID:0L, $
;----------------------------------------------------
start_month:1, start_day_ID:0L, start_hour_ID:0L, $
stop_month: 1, stop_day_ID: 0L, stop_hour_ID: 0L, $
timestep_ID:0L, time_zone:0, $
;----------------------------------------------------
T_air_ID:0L,    T_air_type:3b,    $
RH_ID:0L,       RH_type:0b,       $
albedo_ID:0L,   albedo_type:0b,   $
dust_att_ID:0L, dust_att_type:0b, $
factor_ID:0L,   factor_type:0b,   $
slope_ID:0L,    slope_type:2b,    $
aspect_ID:0L,   aspect_type:2b,   $
lon_ID:0L,      lon_type:0b,      $
lat_ID:0L,      lat_type:0b       }

ngap = 6
XS   = 24
months = [' January ', ' February ', ' March ', ' April ', ' May ',$
          ' June ', ' July ', ' August ', ' September ', ' October ',$
          ' November ', ' December ']
Get_Time_Zone_List, time_zones

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Make RTS File for Qnet_SW', $
            /COLUMN, LEADER=leader
B1  = widget_base(MB, /COLUMN, SPACE=1, /FRAME)
B2  = widget_base(MB, /COLUMN, SPACE=1, /FRAME)
B3  = widget_base(MB, /COLUMN, SPACE=1, /FRAME)
BOT = widget_base(MB, /ROW, SPACE=1)

;---------------------------
;Get the start day and hour
;---------------------------
ST = widget_base(B1, /ROW, SPACE=1)
  SM = widget_base(ST, /ROW, SPACE=ngap)
    SM1 = widget_label(SM, VALUE='Start Month: ')
    SM2 = widget_droplist(SM, VALUE=months, UVALUE='START_MONTH')
    widget_control, SM2, set_droplist_select=0
    ;SM2 = widget_text(SM, VALUE=start_month_str, UVALUE='NONE', $
    ;                  /EDITABLE, XSIZE=5)
  ;--------------------------------------------------------------
  SD = widget_base(ST, /ROW, SPACE=ngap)
    SD1 = widget_label(SD, VALUE='Day:')
    SD2 = widget_text(SD, VALUE=start_day_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.start_day_ID = SD2
  ;--------------------------------------------------------------
  SH = widget_base(ST, /ROW, SPACE=ngap)
    SH1 = widget_label(SH, VALUE='Hour:')
    SH2 = widget_text(SH, VALUE=start_hour_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.start_hour_ID = SH2

;--------------------------
;Get the stop day and hour
;--------------------------
ET = widget_base(B1, /ROW, SPACE=1)
  EM = widget_base(ET, /ROW, SPACE=ngap)
    EM1 = widget_label(EM, VALUE='Stop Month: ')
    EM2 = widget_droplist(EM, VALUE=months, UVALUE='STOP_MONTH')
    widget_control, EM2, set_droplist_select=0
    ;EM2 = widget_text(EM, VALUE=stop_month_str, UVALUE='NONE', $
    ;                  /EDITABLE, XSIZE=5)
  ;--------------------------------------------------------------
  ED = widget_base(ET, /ROW, SPACE=ngap)
    ED1 = widget_label(ED, VALUE='Day:')
    ED2 = widget_text(ED, VALUE=stop_day_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.stop_day_ID = ED2
  ;--------------------------------------------------------------
  EH = widget_base(ET, /ROW, SPACE=ngap)
    EH1 = widget_label(EH, VALUE='Hour:')
    EH2 = widget_text(EH, VALUE=stop_hour_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.stop_hour_ID = EH2

;-----------------
;Get the timestep
;-----------------
TS0 = widget_base(B1, /ROW, SPACE=1)
TS = widget_base(TS0, /ROW, SPACE=ngap)
  TS1 = widget_label(TS, VALUE='Timestep: ')
  TS2 = widget_text(TS, VALUE=timestep_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=6)
  TS3 = widget_label(TS, VALUE='[hours] ')
  state.timestep_ID = TS2

;------------------
;Get the time zone 
;------------------
TZ = widget_base(TS0, /ROW, SPACE=ngap)
  TZ1 = widget_label(TZ, VALUE='  Time zone: ')
  TZ2 = widget_droplist(TZ, VALUE=time_zones, UVALUE='TIME_ZONE')
  widget_control, TZ2, set_droplist_select=12

;------------------
;Get the time zone 
;------------------
;TZ0 = widget_base(B1, /ROW, SPACE=1)
;TZ = widget_base(TZ0, /ROW, SPACE=ngap)
;  TZ1 = widget_label(TZ, VALUE='Time zone: ')
;  TZ2 = widget_droplist(TZ, VALUE=time_zones2, UVALUE='TIME_ZONE')
;  widget_control, TZ2, set_droplist_select=12

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [SM1, EM1, TS1]
Align_Text_Boxes, [SM2, EM2] 

;---------------------
;Add some blank space
;----------------------
;** P0 = widget_label(B1, VALUE=' ')


;------------------------
;Get the input variables
;------------------------
ngap  = 6
XS    = 22
types = Model_Input_Types()
gtype = [' Grid ']
sg_type = [' Scalar ', ' Grid ']
;------------------------------------
T_air_str    = prefix + '_Tair.rts'
RH_str       = '0.30'
albedo_str   = '0.8'
dust_att_str = '0.08'
factor_str   = '1.0'
slope_str    = prefix + '_slope.rtg'
aspect_str   = prefix + '_aspect.rtg'
lon_str      = '0.0'
lat_str      = '0.0'

;-------------------
;Get the parameters
;-------------------
A1 = widget_base(B2, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ', UVALUE='NONE')
  A12 = widget_label(A1, VALUE='Type: ', UVALUE='NONE')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ', UVALUE='NONE')
  A14 = widget_label(A1, VALUE='Units: ', UVALUE='NONE')
;--------------------------------------------------------------------------
TA = widget_base(B2, /ROW, SPACE=ngap)
  TA1 = widget_label(TA, VALUE='T_air: ', UVALUE='NONE')
  TA2 = widget_droplist(TA, VALUE=types, UVALUE='T_AIR_TYPE')
  TA3 = widget_text(TA, VALUE=T_air_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  TA4 = widget_label(TA, VALUE='[deg C]', UVALUE='NONE')
  state.T_air_ID = TA3
  widget_control, TA2, set_droplist_select=3
;--------------------------------------------------------------------------
RH = widget_base(B2, /ROW, SPACE=ngap)
  RH1 = widget_label(RH, VALUE='RH: ', UVALUE='NONE')
  RH2 = widget_droplist(RH, VALUE=types, UVALUE='RH_TYPE')
  RH3 = widget_text(RH, VALUE=RH_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  RH4 = widget_label(RH, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.RH_ID = RH3
;--------------------------------------------------------------------------
;CC = widget_base(B2, /ROW, SPACE=ngap)
;  CC1 = widget_label(CC, VALUE='cloud frac.: ', UVALUE='NONE')
;  CC2 = widget_droplist(CC, VALUE=types, UVALUE='C_TYPE')
;  CC3 = widget_text(CC, VALUE=C_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
;  CC4 = widget_label(CC, VALUE='[none]  in [0,1]', UVALUE='NONE')
;  state.C_ID = CC3
;--------------------------------------------------------------------------
;FF = widget_base(B2, /ROW, SPACE=ngap)
;  FF1 = widget_label(FF, VALUE='canopy frac.: ', UVALUE='NONE')
;  FF2 = widget_droplist(FF, VALUE=types, UVALUE='F_TYPE')
;  FF3 = widget_text(FF, VALUE=F_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
;  FF4 = widget_label(FF, VALUE='[none]  in [0,1]', UVALUE='NONE')
;  state.F_ID = FF3
;--------------------------------------------------------------------------
AL = widget_base(B2, /ROW, SPACE=ngap)
  AL1 = widget_label(AL, VALUE='albedo: ', UVALUE='NONE')
  AL2 = widget_droplist(AL, VALUE=types, UVALUE='ALBEDO_TYPE')
  AL3 = widget_text(AL, VALUE=albedo_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  AL4 = widget_label(AL, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.albedo_ID = AL3
  ;*** widget_control, AL2, set_droplist_select=3
;--------------------------------------------------------------------------
DA = widget_base(B2, /ROW, SPACE=ngap)
  DA1 = widget_label(DA, VALUE='dust att.: ', UVALUE='NONE')
  DA2 = widget_droplist(DA, VALUE=types, UVALUE='DUST_ATT_TYPE')
  DA3 = widget_text(DA, VALUE=dust_att_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  DA4 = widget_label(DA, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.dust_att_ID = DA3
;--------------------------------------------------------------------------
FA = widget_base(B2, /ROW, SPACE=ngap)
  FA1 = widget_label(FA, VALUE='factor: ', UVALUE='NONE')
  FA2 = widget_droplist(FA, VALUE=types, UVALUE='FACTOR_TYPE')
  FA3 = widget_text(FA, VALUE=factor_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  FA4 = widget_label(FA, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.factor_ID = FA3
;--------------------------------------------------------------------------
SL = widget_base(B2, /ROW, SPACE=ngap)
  SL1 = widget_label(SL, VALUE='slope: ', UVALUE='NONE')
  SL2 = widget_droplist(SL, VALUE=gtype, UVALUE='SLOPE_TYPE')
  SL3 = widget_text(SL, VALUE=slope_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  SL4 = widget_label(SL, VALUE='[m/m]', UVALUE='NONE')
  state.slope_ID = SL3
;--------------------------------------------------------------------------
AS = widget_base(B2, /ROW, SPACE=ngap)
  AS1 = widget_label(AS, VALUE='aspect: ', UVALUE='NONE')
  AS2 = widget_droplist(AS, VALUE=gtype, UVALUE='ASPECT_TYPE')
  AS3 = widget_text(AS, VALUE=aspect_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  AS4 = widget_label(AS, VALUE='[radians]', UVALUE='NONE')
  state.aspect_ID = AS3

;-------------------------------------------------
;Read longitude and latitude (if not in RTI file)
;-------------------------------------------------
if (info.pixel_geom ne 0b) then begin
    LO = widget_base(B2, /ROW, SPACE=ngap)
      LO1 = widget_label(LO, VALUE='longitude: ', UVALUE='NONE')
      LO2 = widget_droplist(LO, VALUE=sg_type, UVALUE='LON_TYPE')
      LO3 = widget_text(LO, VALUE=lon_str, /EDITABLE, XSIZE=XS)
      LO4 = widget_label(LO, VALUE='[deg]', UVALUE='NONE')
      state.lon_ID = LO3
    ;--------------------------------------------------------------------------
    LA = widget_base(B2, /ROW, SPACE=ngap)
      LA1 = widget_label(LA, VALUE='latitude: ', UVALUE='NONE')
      LA2 = widget_droplist(LA, VALUE=sg_type, UVALUE='LAT_TYPE')
      LA3 = widget_text(LA, VALUE=lat_str, /EDITABLE, XSIZE=XS)
      LA4 = widget_label(LA, VALUE='[deg]', UVALUE='NONE')
      state.lat_ID = LA3
endif

;---------------------
;Add some blank space
;---------------------
P1 = widget_label(B2, VALUE=' ')

;------------------
;Align the widgets
;------------------
if (info.pixel_geom ne 0b) then begin
    Align_Text_Boxes, [A11, TA1, RH1, AL1, DA1, FA1, SL1, AS1, LO1, LA1]
    Align_Text_Boxes, [A12, TA2, RH2, AL2, DA2, FA2, SL2, AS2, LO2, LA2]
    Align_Text_Boxes, [A13, TA3, RH3, AL3, DA3, FA3, SL3, AS3, LO3, LA3]
endif else begin
    Align_Text_Boxes, [A11, TA1, RH1, AL1, DA1, FA1, SL1, AS1]
    Align_Text_Boxes, [A12, TA2, RH2, AL2, DA2, FA2, SL2, AS2]
    Align_Text_Boxes, [A13, TA3, RH3, AL3, DA3, FA3, SL3, AS3]
endelse

;-------------------------------------
;Get name of new RTS file for Qnet_SW
;-------------------------------------
NF = widget_base(B3, /ROW, SPACE=ngap)
  NF1 = widget_label(NF, VALUE='New RTS file for Qnet_SW: ')
  NF2 = widget_text(NF, VALUE=Qn_SW_RTS_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  NF3 = widget_label(NF, VALUE=' [W / m^2] ')
  state.Qn_SW_RTS_file_ID = NF2

;-----------------------------------
;Get name of new RTS file for e_air
;-----------------------------------
;EA = widget_base(B3, /ROW, SPACE=ngap)
;  EA1 = widget_label(EA, VALUE='New RTS file for e_air: ')
;  EA2 = widget_text(EA, VALUE=e_air_RTS_file, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=XS)
;  EA3 = widget_label(EA, VALUE=' [mbar] ')
;  state.e_air_RTS_file_ID = EA2

;-------------------------------------
;Get name of new RTS file for em_air
;-------------------------------------
;EM = widget_base(B3, /ROW, SPACE=ngap)
;  EM1 = widget_label(EM, VALUE='New RTS file for em_air: ')
;  EM2 = widget_text(EM, VALUE=em_air_RTS_file, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=XS)
;  EM3 = widget_label(EM, VALUE=' [none] ')
;  state.em_air_RTS_file_ID = EM2

;-----------------------
;Get name of RTI file
;-----------------------
RF = widget_base(B3, /ROW, SPACE=ngap)
  RF1 = widget_label(RF, VALUE='Existing RTI file: ')
  RF2 = widget_text(RF, VALUE=RTI_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTI_file_ID = RF2

;---------------------
;Add some blank space
;---------------------
P1 = widget_label(B3, VALUE=' ')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [NF1, RF1]
Align_Text_Boxes, [NF2, RF2]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, BOT, /START, /HELP, /CLOSE 

;---------------------
;A status message box
;---------------------
MS = widget_base(BOT, /ROW, SPACE=ngap)
  MS1 = widget_label(MS, VALUE='Status: ')
  MS2 = widget_text(MS, VALUE='Ready.', XSIZE=20)
  state.msg_box_ID = MS2

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Make_Qnet_SW_File', XOFF=XOFF

end;  GUI_Make_Qnet_SW_File
;***************************************************************
pro Compare_em_air_Methods

;------------------------------------------------------------
;Notes:  There seems to be two different methods that are
;        commonly used to compute the vapor pressure of air,
;        e_air, and then the emissivity of air, em_air, for
;        use in longwave radiation calculations.
;        This routine compares them graphically.
;------------------------------------------------------------
T_air = findgen(80) - 40.0   ;[Celsius]  (-40 to 40)
RH = 1.0
C2K = 273.15

;------------------------
;Brutsaert (1975) method
;------------------------
e_air1  = RH * 0.611 * exp((17.3 * T_air)/(T_air + 237.3))  ;[kPa]
em_air1 = 1.72 * (e_air1 / (T_air + C2K))^(1.0/7)

;--------------------------
;Satterlund (1979) method
;--------------------------
;NB! e_air has units of Pa
;--------------------------
e_air2  = RH * 10d^(11.40 - (2353d/(T_air + C2K)))   ;[Pa]
eterm   = exp(-1d * (e_air2/100d)^((T_air + C2K) / 2016d))
em_air2 = 1.08d * (1d - eterm)

;--------------------------
;Plot the two e_air curves
;------------------------------
;These two agree quite closely
;------------------------------
window, /free
plot,  T_air, e_air1
oplot, T_air, (e_air2 / 1000.0), psym=-3   ;[Pa -> kPa]

;---------------------------
;Plot the two em_air curves
;------------------------------------------------
;These two don't agree very well for some reason
;------------------------------------------------
window, /free
plot,  T_air, em_air1
oplot, T_air, em_air2, psym=-3

end;  Compare_em_air_Methods
;***************************************************************
pro Make_RTS_File_for_Qnet_LW, Qn_LW_RTS_file, $
                  e_air_RTS_file, em_air_RTS_file, RTI_file, $
                  ;----------------------------------------------
                  start_month, start_day, start_hour, $
                  stop_month,  stop_day,  stop_hour, timestep, $
                  ;----------------------------------------------
                  T_air,   T_air_file,   T_air_type,   $
                  RH,      RH_file,      RH_type,      $
                  C,       C_file,       C_type,       $
                  F,       F_file,       F_type,       $
                  T_surf,  T_surf_file,  T_surf_type,  $
                  em_surf, em_surf_file, em_surf_type, $
                  ;---------------------------------------
                  MSG_BOX_ID=MSG_BOX_ID 

;----------------------------------------------------------------
;Notes:  Net longwave radiation is computed using the
;        Stefan-Boltzman law.  All four data types
;        should be allowed (scalar, time series, grid or
;        grid stack).

;        Qnet_LW = (LW_in - LW_out)
;        LW_in   = em_air  * sigma * (T_air  + 273.15)^4
;        LW_out  = em_surf * sigma * (T_surf + 273.15)^4
;        
;        Temperatures in [deg_C] must be converted to
;        [deg_K].  Recall that absolute zero occurs at
;        0 [deg_K] or -273.15 [deg_C].

;----------------------------------------------------------------
; First, e_air is computed as:
;   e_air = RH * 0.611 * exp[(17.3 * T_air) / (T_air + 237.3)]
; Then, em_air is computed as:
;   em_air = (1 - F) * 1.72 * [e_air / (T_air + 273.15)]^(1/7) *
;             (1 + 0.22 * C^2) + F
;----------------------------------------------------------------
FORWARD_FUNCTION Vapor_Pressure

;** sigma = 5.6697e-8   ;[W/(m^2 K^4)]  (Stefan-Boltzman constant)
sigma = 5.67e-8   ;[W/(m^2 K^4)]  (Stefan-Boltzman constant)
C2K   = 273.15    ;(add to convert [deg_C] to [deg_K])

;----------------------------
;Read info from the RTI file
;----------------------------
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows

;------------------------------------
;If data type is grid, read the grid
;------------------------------------
if (T_air_type eq 2) then $
    Read_Grid, T_air, T_air_file, TYPE='FLOAT', /SILENT
if (RH_type eq 2) then $
    Read_Grid, RH, RH_file, TYPE='FLOAT', /SILENT
if (C_type eq 2) then $
    Read_Grid, C, C_file, TYPE='FLOAT', /SILENT
if (F_type eq 2) then $
    Read_Grid, F, F_file, TYPE='FLOAT', /SILENT
;-----------------------------------------------------------
if (T_surf_type eq 2) then $
    Read_Grid, T_surf, T_surf_file, TYPE='FLOAT', /SILENT
if (em_surf_type eq 2) then $ 
    Read_Grid, em_surf, em_surf_file, TYPE='FLOAT', /SILENT

;---------------------------------
;Open files for input variables ?
;---------------------------------
if (T_air_type eq 1) OR (T_air_type eq 3) then begin
    TF_Get_LUN, T_air_unit, T_air_file
    openr, T_air_unit, T_air_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (T_air_type eq 3) then T_air = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (RH_type eq 1) OR (RH_type eq 3) then begin
    TF_Get_LUN, RH_unit, RH_file
    openr, RH_unit, RH_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (RH_type eq 3) then RH = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (C_type eq 1) OR (C_type eq 3) then begin
    TF_Get_LUN, C_unit, C_file
    openr, C_unit, C_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (C_type eq 3) then C = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (F_type eq 1) OR (F_type eq 3) then begin
    TF_Get_LUN, F_unit, F_file
    openr, F_unit, F_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (F_type eq 3) then F = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (T_surf_type eq 1) OR (T_surf_type eq 3) then begin
    TF_Get_LUN, T_surf_unit, T_surf_file
    openr, T_surf_unit, T_surf_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (T_surf_type eq 3) then T_surf = fltarr(nx, ny)
endif
;-----------------------------------------------------------
if (em_surf_type eq 1) OR (em_surf_type eq 3) then begin
    TF_Get_LUN, em_surf_unit, em_surf_file
    openr, em_surf_unit, em_surf_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    if (em_surf_type eq 3) then em_surf = fltarr(nx, ny)
endif

;------------------------
;Open RTS_files to write
;------------------------
TF_Get_LUN, Qn_LW_RTS_unit, Qn_LW_RTS_file
openw, Qn_LW_RTS_unit, Qn_LW_RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
;--------------------------------------------------------
TF_Get_LUN, e_air_RTS_unit, e_air_RTS_file
openw, e_air_RTS_unit, e_air_RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
;--------------------------------------------------------
TF_Get_LUN, em_air_RTS_unit, em_air_RTS_file
openw, em_air_RTS_unit, em_air_RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)

;----------------------------------------------
;Get start & stop times as decimal Julian days
;----------------------------------------------
start_Julian_day = Julian_Day(start_month, start_day)
stop_Julian_day  = Julian_Day(stop_month, stop_day)
start_time = start_Julian_day + (start_hour / 24d)
stop_time  = stop_Julian_day  + (stop_hour  / 24d)

;------------------------------------
;Convert timestep from hours to days
;------------------------------------
timestep_JD = (timestep / 24d)

;------------------------------------
;Create the RTS file, frame by frame
;------------------------------------
for Julian_day=start_time, stop_time, timestep_JD do begin
    ;----------------------------------------
    ;Compute the offset from True Solar Noon
    ;clock_hour is in 24-hour military time
    ;but it can have a decimal part.
    ;----------------------------------------
    clock_hour = (Julian_day - fix(Julian_day)) * 24d

    ;-------------------------
    ;Write a progress message
    ;-------------------------
    if (keyword_set(MSG_BOX_ID)) then begin
        jstr = 'Day = ' + TF_String(fix(Julian_day))
        hstr = 'Hour = ' + TF_String(clock_hour, FORMAT='(F5.2)')
        mstr = jstr + ', ' + hstr
        widget_control, MSG_BOX_ID, set_value=mstr
    endif

    ;------------------------------
    ;Read next values from files ?
    ;------------------------------
    if (T_air_type eq 1) then readf, T_air_unit, T_air
    if (T_air_type eq 3) then readu, T_air_unit, T_air
    ;--------------------------------------------------------
    if (RH_type eq 1) then readf, RH_unit, RH
    if (RH_type eq 3) then readu, RH_unit, RH
    ;--------------------------------------------------------
    if (C_type eq 1) then readf, C_unit, C
    if (C_type eq 3) then readu, C_unit, C
    ;--------------------------------------------------------
    if (F_type eq 1) then readf, F_unit, F
    if (F_type eq 3) then readu, F_unit, F
    ;--------------------------------------------------------
    if (T_surf_type eq 1) then readf, T_surf_unit, T_surf
    if (T_surf_type eq 3) then readu, T_surf_unit, T_surf
    ;--------------------------------------------------------
    if (em_surf_type eq 1) then readf, em_surf_unit, em_surf
    if (em_surf_type eq 3) then readu, em_surf_unit, em_surf

    ;-------------------------------------------------------
    ;Brutsaert (1975) method for computing emissivity
    ;of the air, em-air.  (From Dingman (2002, p. 196))
    ;See notes for Vapor_Pressure function in formulas.pro.
    ;RH = relative humidity [unitless]
    ;-------------------------------------------------------
    ;NB!  Temperatures are assumed to be given with units
    ;     of degrees Celsius and are converted to Kelvin
    ;     wherever necessary by adding C2K = 273.15.
    ;-------------------------------------------------------
    ;NB!  I'm not sure about how F is added at end because
    ;     of how the equation is printed in Dingman (2002)
    ;-------------------------------------------------------
    e_air  = Vapor_Pressure(T_air, RH)   ;[kPa]
    term1  = (1.0 - F) * 1.72 * (e_air / (T_air + C2K))^(1.0/7)
    term2  = (1.0 + (0.22 * C^2.0))
    em_air = (term1 * term2) + F   ;***  DOUBLE CHECK  ***

    ;-------------------------------------------------------
    ;Convert e_air from kPa to mbar before saving, since
    ;those are the units used in the snowmelt energy balance
    ;equations obtained from Zhang et al. (2000).
    ;-------------------------------------------------------
    ;NB!  100 kPa = 1 bar = 1000 mbars
    ;      => 1 kPa = 10 mbars
    ;----------------------------------
    e_air = (e_air * 10.0)   ;[mbars]

    ;------------------------------------------------------
    ;NB!  This formula for vapor pressure as a function of
    ;     temperature compares well with the Brutsaert
    ;     formula above, see Compare_em_air_Methods.
    ;     However, must pay close attention to whether
    ;     equations require units of kPa, Pa, or mbar.
    ;     Note also that we must add C2K to T_air below.
    ;------------------------------------------------------
    ;Satterlund (1979) method for computing the emissivity
    ;of the air, em_air, that is intended to "correct
    ;apparent deficiencies in this formulation at air
    ;temperatures below 0 degrees C" (see G. Liston)
    ;Liston cites Aase and Idso(1978), Satterlund (1979)
    ;------------------------------------------------------
    ;e_air  = Vapor_Pressure(T_air, RH, /SATT, /MBAR)   ;[mbar]
    ;eterm  = exp(-1d * (e_air_mb)^((T_air + C2K) / 2016d))
    ;em_air = 1.08d * (1d - eterm)

    ;-----------------------------------
    ;Compute Qnet_LW grid for this time
    ;-----------------------------------
    LW_in   = em_air  * sigma * (T_air  + C2K)^4d
    LW_out  = em_surf * sigma * (T_surf + C2K)^4d
    LW_out  = LW_out + ((1.0 - em_surf) * LW_in)
    Qnet_LW = (LW_in - LW_out)

    ;---------------------------
    ;Make sure result is a grid
    ;---------------------------
    if (n_elements(Qnet_LW) eq 1) then begin
        Qnet_LW = Qnet_LW + fltarr(nx, ny)
    endif
    if (n_elements(e_air) eq 1) then begin
        e_air = e_air + fltarr(nx, ny)
    endif
    if (n_elements(em_air) eq 1) then begin
        em_air = em_air + fltarr(nx, ny)
    endif

    ;--------------------------
    ;Write frames to RTS files
    ;--------------------------
    writeu, Qn_LW_RTS_unit,  float(Qnet_LW)
    writeu, e_air_RTS_unit,  float(e_air)
    writeu, em_air_RTS_unit, float(em_air)
endfor

;-------------------------
;Write a finished message
;-------------------------
if (keyword_set(MSG_BOX_ID)) then begin
    widget_control, MSG_BOX_ID, set_value='Finished.'
endif

;--------------------
;Close input files ?
;--------------------
if (T_air_type eq 1)   OR (T_air_type eq 3)   then free_lun, T_air_unit
if (RH_type eq 1)      OR (RH_type eq 3)      then free_lun, RH_unit
if (C_type eq 1)       OR (C_type eq 3)       then free_lun, C_unit
if (F_type eq 1)       OR (F_type eq 3)       then free_lun, F_unit
if (T_surf_type eq 1)  OR (T_surf_type eq 3)  then free_lun, T_surf_unit
if (em_surf_type eq 1) OR (em_surf_type eq 3) then free_lun, em_surf_unit

;-----------------------
;Close the output files
;-----------------------
free_lun, Qn_LW_RTS_unit, e_air_RTS_unit, em_air_RTS_unit

end;  Make_RTS_File_for_Qnet_LW
;***************************************************************
pro Check_Number_of_Frames, RTS_file, RTI_file, nf_match, OK

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

FORWARD_FUNCTION Number_of_Frames
OK = 1b

;-----------------------------------
;Count number of frames in RTS file
;-----------------------------------
nf = Number_of_Frames(RTS_file, RTI_file)

;--------------------------
;Issue a warning message ?
;--------------------------
if (nf ne nf_match) then begin
    msg = ['ERROR: ', ' ', $
           'Number of frames in the RTS file: ',$
           '   ' + RTS_file, $
           ' ',$
           'does not match the number of frames in the RTS',$
           'file to be created, based on the time settings. ',$
           ' ', $
           'Number of frames in RTS file = ' + TF_String(nf), $
           'Number of frames in new file = ' + TF_String(nf_match), $
           ' ']
    GUI_Error_Message, msg
    OK = 0b
endif

end;  Check_Number_of_Frames
;***************************************************************
pro GUI_Make_Qnet_LW_File_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;******************
'START_MONTH' : $
;******************
state.start_month = (event.index + 1)

;*****************
'STOP_MONTH' : $
;*****************
state.stop_month = (event.index + 1)

;**************************************************
'T_AIR_TYPE'   : state.T_air_type   = event.index
'RH_TYPE'      : state.RH_type      = event.index
'C_TYPE'       : state.C_type       = event.index
'F_TYPE'       : state.F_type       = event.index
'T_SURF_TYPE'  : state.T_surf_type  = event.index
'EM_SURF_TYPE' : state.em_surf_type = event.index
;**************************************************

;************
'START' : $
;************
begin
;---------------------------------
;Get months selected via droplist
;---------------------------------
start_month = state.start_month
stop_month  = state.stop_month
;------------------------
;Read start day and hour
;------------------------
Read_Text_Box, state.start_day_ID, start_day, OK, /INTEGER
if NOT(OK) then RETURN
Read_Text_Box, state.start_hour_ID, start_hour, OK, /FLOAT
if NOT(OK) then RETURN
;-----------------------
;Read stop day and hour
;-----------------------
Read_Text_Box, state.stop_day_ID, stop_day, OK, /INTEGER
if NOT(OK) then RETURN
Read_Text_Box, state.stop_hour_ID, stop_hour, OK, /FLOAT
if NOT(OK) then RETURN
;---------------------------
;Read the timestep in hours
;---------------------------
Read_Text_Box, state.timestep_ID, timestep, OK, /FLOAT
if NOT(OK) then RETURN

;--------------------------------------
;Read name of new RTS file for Qnet_LW
;--------------------------------------
Read_Text_Box, state.Qn_LW_RTS_file_ID, Qn_LW_RTS_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, Qn_LW_RTS_file, OK
if NOT(OK) then RETURN
;------------------------------------
;Read name of new RTS file for e_air
;------------------------------------
Read_Text_Box, state.e_air_RTS_file_ID, e_air_RTS_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, e_air_RTS_file, OK
if NOT(OK) then RETURN
;-------------------------------------
;Read name of new RTS file for em_air
;-------------------------------------
Read_Text_Box, state.em_air_RTS_file_ID, em_air_RTS_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, em_air_RTS_file, OK
if NOT(OK) then RETURN
;----------------------
;Read name of RTI file
;----------------------
Read_Text_Box, state.RTI_file_ID, RTI_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(RTI_file)
if NOT(OK) then RETURN

;--------------------------
;Read additional variables
;--------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.RH_type, state.RH_ID, RH, $
                 OK, filename=RH_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.C_type, state.C_ID, C, $
                 OK, filename=C_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.F_type, state.F_ID, F, $
                 OK, filename=F_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.T_surf_type, state.T_surf_ID, T_surf, $
                 OK, filename=T_surf_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
Read_Input_Type, state.em_surf_type, state.em_surf_ID, em_surf, $
                 OK, filename=em_surf_file
if NOT(OK) then RETURN

;--------------------------------------------------------
;Get number of frames in new RTS file from time settings
;--------------------------------------------------------
;Get start & stop times as decimal Julian days
;----------------------------------------------
Julian_start_day = Julian_Day(start_month, start_day, start_hour)
Julian_stop_day  = Julian_Day(stop_month,  stop_day,  stop_hour)
timestep_JD      = (timestep / 24d)   ;([hours] -> [days])
nf_match         = long((Julian_stop_day - Julian_start_day) / timestep_JD)
;-----------------------------------------------
;Check if number of frames in RTS files matches
;the number obtained from the time settings
;-----------------------------------------------
if (state.T_air_type eq 3) then $
    Check_Number_of_Frames, T_air_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;----------------------------------------------------------------
if (state.RH_type eq 3) then $
    Check_Number_of_Frames, RH_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;----------------------------------------------------------------
if (state.C_type eq 3) then $
    Check_Number_of_Frames, C_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;----------------------------------------------------------------
if (state.F_type eq 3) then $
    Check_Number_of_Frames, F_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;----------------------------------------------------------------
if (state.T_surf_type eq 3) then $
    Check_Number_of_Frames, T_surf_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN
;----------------------------------------------------------------
if (state.em_surf_type eq 3) then $
    Check_Number_of_Frames, em_surf_file, RTI_file, nf_match, OK
if NOT(OK) then RETURN

;---------------------------------------
;Call routines that create the RTS file
;---------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Make_RTS_File_for_Qnet_LW, Qn_LW_RTS_file, e_air_RTS_file, $
                  em_air_RTS_file, RTI_file, $
                  ;----------------------------------------------
                  start_month, start_day, start_hour, $
                  stop_month,  stop_day,  stop_hour, timestep, $
                  ;----------------------------------------------
                  T_air,   T_air_file,   state.T_air_type,   $
                  RH,      RH_file,      state.RH_type,      $
                  C,       C_file,       state.C_type,       $
                  F,       F_file,       state.F_type,       $
                  T_surf,  T_surf_file,  state.T_surf_type,  $
                  em_surf, em_surf_file, state.em_surf_type, $
                  ;--------------------------------------------
                  MSG_BOX_ID=state.msg_box_ID
widget_control, event.ID, sensitive=1    ;(enable button)

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating RTS files for', $
       'Qnet_LW, e_air and em_air.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")
;*** Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'longwave_calc.htm'

;************
'CLOSE' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CLOSE') AND $
   (uvalue ne 'START') then $
    widget_control, event.top, set_uvalue=state 

end;  GUI_Make_Qnet_LW_File_event
;****************************************************************
pro GUI_Make_Qnet_LW_File, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then leader=0L 

;-----------------------------------
;Get current values from main state
;-----------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;----------------------------------------
prefix          = mstate.run_vars.prefix
Qn_LW_RTS_file  = prefix + '_Qn-LW.rts'
e_air_RTS_file  = prefix + '_e-air.rts'
em_air_RTS_file = prefix + '_em-air.rts'
RTI_file        = prefix + '.rti'
;-----------------------------------------
start_day_str  = ' 1 '
start_hour_str = ' 0.0 '
stop_day_str   = ' 2 '
stop_hour_str  = ' 0.0 '
timestep_str   = ' 1.0 '

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, msg_box_ID:0L, RTI_file_ID:0L, $
Qn_LW_RTS_file_ID:0L, e_air_RTS_file_ID:0L, $
em_air_RTS_file_ID:0L, $
;----------------------------------------------------
start_month:1, start_day_ID:0L, start_hour_ID:0L, $
stop_month: 1, stop_day_ID: 0L, stop_hour_ID: 0L, $
timestep_ID:0L, $    ;*** time_zone:0, $
;----------------------------------------------------
T_air_ID:0L,   T_air_type:3b,  $
RH_ID:0L,      RH_type:0b,     $
C_ID:0L,       C_type:0b,      $
F_ID:0L,       F_type:0b,      $
T_surf_ID:0L,  T_surf_type:3b, $
em_surf_ID:0L, em_surf_type:0b }

ngap = 6
XS   = 24
months = [' January ', ' February ', ' March ', ' April ', ' May ',$
          ' June ', ' July ', ' August ', ' September ', ' October ',$
          ' November ', ' December ']
;** Get_Time_Zone_List, time_zones

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Make RTS File for Qnet_LW', $
            /COLUMN, LEADER=leader
B1  = widget_base(MB, /COLUMN, SPACE=1, /FRAME)
B2  = widget_base(MB, /COLUMN, SPACE=1, /FRAME)
B3  = widget_base(MB, /COLUMN, SPACE=1, /FRAME)
BOT = widget_base(MB, /ROW, SPACE=1)

;---------------------------
;Get the start day and hour
;---------------------------
ST = widget_base(B1, /ROW, SPACE=1)
  SM = widget_base(ST, /ROW, SPACE=ngap)
    SM1 = widget_label(SM, VALUE='Start Month: ')
    SM2 = widget_droplist(SM, VALUE=months, UVALUE='START_MONTH')
    widget_control, SM2, set_droplist_select=0
    ;SM2 = widget_text(SM, VALUE=start_month_str, UVALUE='NONE', $
    ;                  /EDITABLE, XSIZE=5)
  ;----------------------------------------------------------------
  SD = widget_base(ST, /ROW, SPACE=ngap)
    SD1 = widget_label(SD, VALUE='Day:')
    SD2 = widget_text(SD, VALUE=start_day_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.start_day_ID = SD2
  ;----------------------------------------------------------------
  SH = widget_base(ST, /ROW, SPACE=ngap)
    SH1 = widget_label(SH, VALUE='Hour:')
    SH2 = widget_text(SH, VALUE=start_hour_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.start_hour_ID = SH2

;--------------------------
;Get the stop day and hour
;--------------------------
ET = widget_base(B1, /ROW, SPACE=1)
  EM = widget_base(ET, /ROW, SPACE=ngap)
    EM1 = widget_label(EM, VALUE='Stop Month: ')
    EM2 = widget_droplist(EM, VALUE=months, UVALUE='STOP_MONTH')
    widget_control, EM2, set_droplist_select=0
    ;EM2 = widget_text(EM, VALUE=stop_month_str, UVALUE='NONE', $
    ;                  /EDITABLE, XSIZE=5)
  ;---------------------------------------------------------------
  ED = widget_base(ET, /ROW, SPACE=ngap)
    ED1 = widget_label(ED, VALUE='Day:')
    ED2 = widget_text(ED, VALUE=stop_day_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.stop_day_ID = ED2
  ;---------------------------------------------------------------
  EH = widget_base(ET, /ROW, SPACE=ngap)
    EH1 = widget_label(EH, VALUE='Hour:')
    EH2 = widget_text(EH, VALUE=stop_hour_str, UVALUE='NONE', $
                      /EDITABLE, XSIZE=5)
    state.stop_hour_ID = EH2

;-----------------
;Get the timestep
;-----------------
TS0 = widget_base(B1, /ROW, SPACE=1)
TS = widget_base(TS0, /ROW, SPACE=ngap)
  TS1 = widget_label(TS, VALUE='Timestep: ')
  TS2 = widget_text(TS, VALUE=timestep_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=6)
  TS3 = widget_label(TS, VALUE='[hours] ')
  state.timestep_ID = TS2

;------------------
;Get the time zone 
;------------------
;TZ = widget_base(TS0, /ROW, SPACE=ngap)
;  TZ1 = widget_label(TZ, VALUE='  Time zone: ')
;  TZ2 = widget_droplist(TZ, VALUE=time_zones, UVALUE='TIME_ZONE')
;  widget_control, TZ2, set_droplist_select=12

;---------------------
;Add some blank space
;----------------------
;** P0 = widget_label(B1, VALUE=' ')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [SM1, EM1, TS1]
Align_Text_Boxes, [SM2, EM2] 


;------------------------
;Get the input variables
;------------------------
ngap  = 6
XS    = 22
types = Model_Input_Types()
;---------------------------
RH_str      = '0.30'
C_str       = '0.0'
F_str       = '0.0'
;em_air_str = '0.70'
em_surf_str = '0.98'
;------------------------------------
T_air_str   = prefix + '_Tair.rts'
T_surf_str  = prefix + '_Tsurf.rts'

;-------------------
;Get the parameters
;-------------------
A1 = widget_base(B2, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ', UVALUE='NONE')
  A12 = widget_label(A1, VALUE='Type: ', UVALUE='NONE')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ', UVALUE='NONE')
  A14 = widget_label(A1, VALUE='Units: ', UVALUE='NONE')
;--------------------------------------------------------------------------
TA = widget_base(B2, /ROW, SPACE=ngap)
  TA1 = widget_label(TA, VALUE='T_air: ', UVALUE='NONE')
  TA2 = widget_droplist(TA, VALUE=types, UVALUE='T_AIR_TYPE')
  TA3 = widget_text(TA, VALUE=T_air_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  TA4 = widget_label(TA, VALUE='[deg C]', UVALUE='NONE')
  state.T_air_ID = TA3
  widget_control, TA2, set_droplist_select=3
;--------------------------------------------------------------------------
;EA = widget_base(B2, /ROW, SPACE=ngap)
;  EA1 = widget_label(EA, VALUE='em_air: ', UVALUE='NONE')
;  EA2 = widget_droplist(EA, VALUE=types, UVALUE='EM_AIR_TYPE')
;  EA3 = widget_text(EA, VALUE=em_air_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
;  EA4 = widget_label(EA, VALUE='[none]', UVALUE='NONE')
;  state.em_air_ID = EA3
;--------------------------------------------------------------------------
RH = widget_base(B2, /ROW, SPACE=ngap)
  RH1 = widget_label(RH, VALUE='RH: ', UVALUE='NONE')
  RH2 = widget_droplist(RH, VALUE=types, UVALUE='RH_TYPE')
  RH3 = widget_text(RH, VALUE=RH_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  RH4 = widget_label(RH, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.RH_ID = RH3
;--------------------------------------------------------------------------
CC = widget_base(B2, /ROW, SPACE=ngap)
  CC1 = widget_label(CC, VALUE='cloud frac.: ', UVALUE='NONE')
  CC2 = widget_droplist(CC, VALUE=types, UVALUE='C_TYPE')
  CC3 = widget_text(CC, VALUE=C_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  CC4 = widget_label(CC, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.C_ID = CC3
;--------------------------------------------------------------------------
FF = widget_base(B2, /ROW, SPACE=ngap)
  FF1 = widget_label(FF, VALUE='canopy frac.: ', UVALUE='NONE')
  FF2 = widget_droplist(FF, VALUE=types, UVALUE='F_TYPE')
  FF3 = widget_text(FF, VALUE=F_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  FF4 = widget_label(FF, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.F_ID = FF3
;--------------------------------------------------------------------------
TS = widget_base(B2, /ROW, SPACE=ngap)
  TS1 = widget_label(TS, VALUE='T_surf: ', UVALUE='NONE')
  TS2 = widget_droplist(TS, VALUE=types, UVALUE='T_SURF_TYPE')
  TS3 = widget_text(TS, VALUE=T_surf_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  TS4 = widget_label(TS, VALUE='[deg C]', UVALUE='NONE')
  state.T_surf_ID = TS3
  widget_control, TS2, set_droplist_select=3
;--------------------------------------------------------------------------
ES = widget_base(B2, /ROW, SPACE=ngap)
  ES1 = widget_label(ES, VALUE='em_surf: ', UVALUE='NONE')
  ES2 = widget_droplist(ES, VALUE=types, UVALUE='EM_SURF_TYPE')
  ES3 = widget_text(ES, VALUE=em_surf_str, UVALUE='NONE', /EDITABLE, XSIZE=XS)
  ES4 = widget_label(ES, VALUE='[none]', UVALUE='NONE')
  state.em_surf_ID = ES3
;---------------------
;Add some blank space
;---------------------
P1 = widget_label(B2, VALUE=' ')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, TA1, RH1, CC1, FF1, TS1, ES1]
Align_Text_Boxes, [A12, TA2, RH2, CC2, FF2, TS2, ES2]
Align_Text_Boxes, [A13, TA3, RH3, CC3, FF3, TS3, ES3]


;-------------------------------------
;Get name of new RTS file for Qnet_LW
;-------------------------------------
NF = widget_base(B3, /ROW, SPACE=ngap)
  NF1 = widget_label(NF, VALUE='New RTS file for Qnet_LW: ')
  NF2 = widget_text(NF, VALUE=Qn_LW_RTS_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  NF3 = widget_label(NF, VALUE=' [W / m^2] ')
  state.Qn_LW_RTS_file_ID = NF2

;-----------------------------------
;Get name of new RTS file for e_air
;-----------------------------------
EA = widget_base(B3, /ROW, SPACE=ngap)
  EA1 = widget_label(EA, VALUE='New RTS file for e_air: ')
  EA2 = widget_text(EA, VALUE=e_air_RTS_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  EA3 = widget_label(EA, VALUE=' [mbar] ')
  state.e_air_RTS_file_ID = EA2

;-------------------------------------
;Get name of new RTS file for em_air
;-------------------------------------
EM = widget_base(B3, /ROW, SPACE=ngap)
  EM1 = widget_label(EM, VALUE='New RTS file for em_air: ')
  EM2 = widget_text(EM, VALUE=em_air_RTS_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  EM3 = widget_label(EM, VALUE=' [none] ')
  state.em_air_RTS_file_ID = EM2

;-----------------------
;Get name of RTI file
;-----------------------
RF = widget_base(B3, /ROW, SPACE=ngap)
  RF1 = widget_label(RF, VALUE='Existing RTI file: ')
  RF2 = widget_text(RF, VALUE=RTI_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTI_file_ID = RF2

;---------------------
;Add some blank space
;---------------------
P1 = widget_label(B3, VALUE=' ')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [NF1, EA1, EM1, RF1]
Align_Text_Boxes, [NF2, EA2, EM2, RF2]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, BOT, /START, /HELP, /CLOSE 

;---------------------
;A status message box
;---------------------
MS = widget_base(BOT, /ROW, SPACE=ngap)
  MS1 = widget_label(MS, VALUE='Status: ')
  MS2 = widget_text(MS, VALUE='Ready.', XSIZE=20)
  state.msg_box_ID = MS2

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Make_Qnet_LW_File', XOFF=XOFF

end;  GUI_Make_Qnet_LW_File 
;***************************************************************



