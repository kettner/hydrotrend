
;*****************************************************************
;   richards.pro   ;(A simple solver for 1D Richard's Equation.)

;   Copyright (c) 2006, Scott D. Peckham
;   Created:   January 2006
;   Modified:  April, May 2006

;*****************************************************************

;   Psi_of_Theta    (function)
;   K_of_Psi        (function)
;   Flow_Rate       (function)
;   Theta_Min       (function)
;   Theta_Max       (function)

;   Get_Soil_Params (procedure)
;   Get_Soil_Vars   (procedure)
;   Update_Theta    (procedure)

;   Infiltrate      (main routine)

;---------------------------------------
;   Broadbridge-White Analytic Solution
;---------------------------------------
;   Erc             (function)
;   BW_Solution
;   BW_Solution_vs_Time

;   Fairbanks_Test
 
;*****************************************************************
function Psi_of_Theta, theta, theta_r, theta_s, c, lambda, $
                       psi_B, psi_a, REPORT=REPORT

;----------------------------------------------------------------
;Notes:  This function returns the pressure head, psi, as
;        a function of the soil moisture, theta, via the
;        Brooks-Corey (B-C) or "transitional Brooks-Corey"
;        (TB-C) relation.  Psi is < 0 in the unsaturated zone,
;        is 0 at saturation (e.g. water table) and is > 0
;        below the water table.  Psi_B is the height of the
;        capillary rise, and is the pressure head at the top
;        of the tension-saturated zone (capillary fringe).
;        (See p. 239 in Dingman's book.)

;        Note that for both B-C and TB-C, psi goes to
;        -Infinity as theta goes to theta_r (S_eff goes
;        to zero).  However, natural soils do not have heads
;        (tensions) less than -31,000 cm.  In this range they
;        absorb water from the air (hygroscopic).  A function
;        called Theta_Min uses this limiting value of psi,
;        psi_H, to compute a minimum value of theta, theta_H,
;        and the Flow_Rate function forbids flow rates that
;        would reduce theta below theta_H.  With this set up,
;        evaporation at the surface cannot cause theta to drop
;        below theta_H and therefore psi stays > psi_H.
;        Note that we cannot define theta_r to be theta_H,
;        because psi goes to -Infinity (vs. psi_H) as theta
;        goes to theta_r.  So we need theta_H > theta_r.
;        The difference between theta_H and theta_r seems to
;        be related to the well-known hysteresis effect that
;        causes wetting and drying soils to follow different
;        paths in theta-psi space.  See p. 11 in Smith.

;        Theta_r is the water content not removable by capillary
;        forces (Smith, p. 201).  The field capacity, theta_fc,
;        (not discussed by Smith) seems to be the maximum content
;        that can be held against the force of gravity.  It can
;        be estimated using equation (6-19) on p. 235 in Dingman's
;        book.   

;        For B-C, psi goes to psi_B as theta goes to theta_s,
;        and psi <= psi_B < 0, or abs(psi) >= abs(psi_B).

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
;---------------------------------------------------------------
REPORT = keyword_set(REPORT)

;-------------------------------------
;Compute the "effective saturation"
;Relative saturation = theta/porosity
;-------------------------------------
S_eff  = (theta - theta_r) / (theta_s - theta_r)

BROOKS_COREY = 0b
if (BROOKS_COREY) then begin
    arg = S_eff^(-1d/lambda)
    psi = psi_B * arg
endif else begin
    ;---------------------------------------
    ;Transitional Brooks-Corey (TB-C)
    ;w/ continuous derivative at saturation
    ;---------------------------------------
    arg = (S_eff^(-c/lambda) - 1d)^(1d/c)
    psi = (psi_B * arg) - psi_a
endelse

;----------------
;Optional report 
;----------------
if (REPORT) then begin
    print,'S_eff = ', S_eff[0:3]
    print,'psi   = ', psi[0:3]
    ;print,'arg   = ', arg
    ;print,' '
endif

RETURN, psi

end;  Psi_of_Theta
;*****************************************************************
function K_of_Psi, psi, K_s, psi_B, psi_a, c, eta, $
                   REPORT=REPORT

;----------------------------------------------------------
;Notes:  This function returns the hydraulic conductivity,
;        K, as a function of the pressure head, psi, via
;        the "Brooks-Corey" (B-C) or "transitional Brooks-
;        Corey" (TB-C) relation.

;        lambda = pore size distribution parameter
;        eta    = "pore-disconnectedness" parameter
;        eta    = 2d + (3d * lambda)

;        There is a typo in R.E. Smith's AGU monograph in
;        equation (2.14), where eta should be -eta.

;        See "Infiltration Theory for Hydrologic Applica-
;        tions" by R.E. Smith (2002), p. 21-22.
;----------------------------------------------------------
REPORT = keyword_set(REPORT)
BROOKS_COREY = 0b

;------------------------------------
;Compute the "relative conductivity"
;------------------------------------
if (BROOKS_COREY) then begin
    K_r = (psi / psi_B)^(-eta)
endif else begin
    ;---------------------------------------
    ;Transitional Brooks-Corey (TB-C)
    ;w/ continuous derivative at saturation
    ;---------------------------------------
    K_r = (1d + ((psi + psi_a)/psi_B)^c)^(-eta/c)
endelse

;---------------------------
;Compute K from K_s and K_r
;---------------------------
K_r = (K_r < 1d) > 0d
K   = K_s * K_r

;----------------
;Optional report 
;----------------
if (REPORT) then begin
    print,'K = ', K[0:3]
    ;print,' '
endif

RETURN, K

end;  K_of_Psi
;*****************************************************************
function Flow_Rate, K, psi, theta, L, dt, theta_s, theta_r, $
                    theta_i, theta_H, r, REPORT=REPORT

;------------------------------------------------------
;Notes:  r  = rainrate for current timestep
;        vB = flow rate [m/s] at bottom of a layer
;        vT = flow rate [m/s] at top of a layer
;        K_bar is a "mean value" of K

;        K, psi and theta are assumed to be uniform
;        within any given soil layer, while the flow
;        rates are for the boundaries between layers.

;        The first derivative of psi is computed using
;        psi values on either side of a boundary and
;        the z-distance between the layer centers.
;------------------------------------------------------
REPORT  = keyword_set(REPORT)

dpsi_dz = (shift(psi,-1) - psi) / L

;----------------------------
;Compute a "mean value" of K
;------------------------------------------
;Using K_bar = K doesn't work for the case
;of redistribution due to evaporation, but
;seems to work for many other cases.
;--------------------------------------------
;Ideally, should compute K_bar as integral
;of K(h) from h=psi_1 to psi_2 and then
;divide by (psi_2 - psi_1).  But for T-BC,
;this integral can't be done in closed form.
;Next 2 lines is an intermediate approach.
;--------------------------------------------
;psi_bar = (shift(psi,-1) + psi)/2d
;K_bar   = K_of_Psi(psi_bar, K_s, psi_B, psi_a, c, eta) 
;-------------------------------------------------------

K_bar = (shift(K,-1) + K)/2d
vB    = K_bar * (1d - dpsi_dz)

;---------------------------------
;Simple bottom boundary condition
;---------------------------------
n = n_elements(K)
vB[n-1L] = vB[n-2L]  ;(matches dv_dz=0 in Update_Theta ? **********)

;-----------------------------------
;Note that vT[0] = r[j]; j=timestep
;-----------------------------------
vT = [r, vB[0L:n-2L]]

;-------------------------------------------------
;Flow rate into any layer must be less than the
;"amount of space available", while flow rate
;out must be less than "amount of water available
;-------------------------------------------------
filling = where(((vT - vB) ge 0d), n_filling, $
                 COMP=losing, NCOMP=n_losing)
if (n_filling ne 0) then begin
    space_avail = (L/dt)*(theta_s - theta)
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
    ;infiltration rate when the EVAP keyword is set.
    ;---------------------------------------------------------
    water_avail = (L/dt)*(theta - theta_H) > 0
    vT[losing] = (vT[losing] > (vB[losing] - water_avail[losing]))
endif

;----------------
;Optional report 
;----------------
if (REPORT) then begin
    print,'dpsi_dz = ', dpsi_dz[0:3]
    print,'vT      = ', vT[0:3]
    ;print,'vB      = ', vB[0:3]
endif

RETURN, vT

end;  Flow_Rate
;*****************************************************************
function Theta_Min, theta_s, theta_r, psi_B, psi_A, c, lambda, $
                    REPORT=REPORT

;---------------------------------------------------------------
;Notes:  Note that for both B-C and TB-C, psi goes to
;        -Infinity as theta goes to theta_r (S_eff goes
;        to zero).  However, natural soils do not have heads
;        (tensions) less than -31,000 cm.  In this range they
;        absorb water from the air (hygroscopic).  While
;        initial theta values will always be set to a number
;        greater than theta_r, evaporation at the surface can
;        cause theta to drop to values near theta_r.  Here we
;        use the T-BC equation for theta(psi) to compute a
;        value theta_H corresponding to psi_H=-31,000 cm.
;---------------------------------------------------------------

psi_H = -31000d  ;[cm]
psi_H = (psi_H / 100d)  ;[cm -> meters]

theta_H = (1d + ((psi_H + psi_A)/psi_B)^c)^(-lambda/c)
theta_H = theta_H * (theta_s - theta_r) + theta_r

RETURN, theta_H

end;  Theta_Min
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
BROOKS_COREY = 0b

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
pro Get_Soil_Params, soil_type, phi, K_s, psi_B, b, $
                     lambda, eta, theta_s, $
                     REPORT=REPORT

;--------------------------------------------------------
;Notes:  The values here are from Table 6.1 (p. 235) in
;        Dingman (2002), 2nd. edition.  Data originally
;        from Clapp and Hornberger (1978).
;--------------------------------------------------------
REPORT = keyword_set(REPORT)

table = $
[ [0.395,  1.76e-2,  12.1,  4.05], $    ;(sand)
  [0.410,  1.56e-2,   9.0,  4.38], $    ;(loamy sand)
  [0.435,  3.47e-3,  21.8,  4.90], $    ;(sandy loam)
  [0.485,  7.20e-4,  78.6,  5.30], $    ;(silt loam)
  [0.451,  6.95e-4,  47.8,  5.39], $    ;(loam)
  [0.420,  6.30e-4,  29.9,  7.12], $    ;(sandy clay loam)
  [0.477,  1.70e-4,  35.6,  7.75], $    ;(silty clay loam)
  [0.476,  2.45e-4,  63.0,  8.52], $    ;(clay loam)
  [0.426,  2.17e-4,  15.3,  10.4], $    ;(sandy clay)
  [0.492,  1.03e-4,  49.0,  10.4], $    ;(silty clay)
  [0.482,  1.28e-4,  40.5,  11.4] ]     ;(clay)

types = ['sand', 'loamy_sand', 'sandy_loam', 'silt_loam', $
         'loam', 'sandy_clay_loam', 'silty_clay_loam', $
         'clay_loam', 'sandy_clay', 'silty_clay', 'clay']

w = where(types eq strlowcase(soil_type), nw)

if (nw eq 0) then begin
    print,'*******************************'
    print,' Sorry, soil type not found. '
    print,'*******************************'
    print,' '  &  RETURN
endif

;--------------------------
;Get parameters from table
;--------------------------
row   = table[*, w[0]]
por   = row[0]   ;[unitless]
K_s   = row[1]   ;[cm/s]
psi_B = row[2]   ;[cm]
b     = row[3]   ;[unitless]

;--------------
;Convert units
;--------------
K_s   = K_s   / 100d    ;([cm/s] -> [m/s])
psi_B = psi_B / 100d    ;([cm]   -> [m])

;----------------------
;Computable parameters
;----------------------

lambda  = (1d/b)
eta     = 2d + (3d * lambda)
theta_s = por                    ;(not true in general)

;----------------
;Optional report
;----------------
if (REPORT) then begin
    print,'porosity = ', por
    print,'K_s      = ', K_s,   ' [m/s]'
    print,'psi_B    = ', psi_B, ' [m]'
    print,'b        = ', b
    print,'lambda   = ', lambda
    print,'eta      = ', eta
    print,' '
endif

end;  Get_Soil_Params
;*****************************************************************
pro Get_Soil_Vars, K_s, K_i, theta_s, theta_i, theta_r, $
                   psi_B, psi_a, c, lambda, eta, TYPE=type

;------------------------------------------------------
;Notes:  The values here are from Table 7.1, p. 131,
;        in R.E. Smith's AGU monograph.
;        See Notes for the Psi_of_Theta function.

;        K_s     = saturated hydraulic conductivity
;        theta_s = saturated soil moisture (porosity)
;        theta_i = initial soil moisture
;        theta_r = residual soil moisture


;        lambda  = pore size distribution parameter
;                  typical range is 0.15 to 0.65.
;        c       = "curvature parameter" in [1,10] ?
;        psi_B   = bubbling pressure (at saturation)
;        psi_a   = optional shift parameter
;------------------------------------------------------
if NOT(keyword_set(TYPE)) then type='SOIL_1'

case strupcase(type) of

;*************
'SOIL_1' : $
;*************
begin
K_s     = 0.4       ;[mm/h]
theta_s = 0.3325d   ;[unitless]
theta_i = 0.1659d   ;[unitless]
theta_r = 0.1225d   ;[unitless]
psi_B   = -0.8d     ;[meters]
psi_a   = 0.1d      ;[meters]
lambda  = 0.2d
end

;*************
'SOIL_2' : $
;*************
begin
K_s     = 4.0       ;[mm/h]
theta_s = 0.4d      ;[unitless]
theta_i = 0.0516d   ;[unitless]      
theta_r = 0.04d     ;[unitless]
psi_B   = -0.4d     ;[meters]
psi_a   =  0.05d    ;[meters]
lambda  = 0.4d
end

;*************
'SOIL_A' : $
;*************
begin
K_s     = 3.6       ;[mm/h]       ;(only change vs. SOIL_1)
theta_s = 0.3325d   ;[unitless]
theta_i = 0.1659d   ;[unitless]
theta_r = 0.1225d   ;[unitless]
psi_B   = -0.8d     ;[meters]
psi_a   =  0.1d     ;[meters]
lambda  = 0.2d
end

endcase

;--------------------------
;Computable or common vars
;----------------------------------------------------
;NB!  For "trans. Brooks-Corey" relations, numerical
;instabilities seem to appear sooner for larger c.
;----------------------------------------------------
eta = 2d + (3d * lambda)
c   = 1d
K_i = 0d

psi_a = 0d    ;*************************

;----------------------------------
;Convert K-values from mm/h to m/s
;----------------------------------
K_s = K_s / (3600d * 1000d)
K_i = K_i / (3600d * 1000d)

end;  Get_Soil_Vars
;*****************************************************************
pro Update_Theta, theta, theta_last, v, dt, L, theta_s, $
                  theta_r, theta_i, REPORT=REPORT

REPORT = keyword_set(REPORT)

dv_dz = (v - shift(v,-1)) / L
n     = n_elements(dv_dz)

;--------------------------------------
;What should we do at lower boundary ?
;--------------------------------------
;** dv_dz[n-1L] = dv_dz[n-2L]      ;**********
dv_dz[n-1L] = 0d   ;(5/1/06)

theta = theta_last + (dv_dz * dt)

;---------------------------
;Make sure theta <= theta_s
;and that  theta >= theta_r
;------------------------------------------------------
;NB! Seems we don't need this when we check for layers
;that are filling or losing in the Flow_Rate function.
;------------------------------------------------------
;theta = (theta < theta_s)
;theta = (theta > theta_r)
;*** theta = (theta > theta_i)   ;*********************

;--------------------------------
;Update theta_last for next step
;--------------------------------
theta_last = theta

;----------------
;Optional report 
;----------------
if (REPORT) then begin
    print,'dv_dz = ', dv_dz[0:3]
    print,'theta = ', theta[0:3]
    ;print,' '
endif 

end;  Update_Theta
;*****************************************************************
pro Infiltrate, n_steps, PLOT=PLOT, REPORT=REPORT, TYPE=TYPE, $
                DT=dt, NZ=nz, DZ=dz, R0=R0, $
                SUB_KS=SUB_KS, EVAP=EVAP, R_EVAP=R_EVAP, $
                REDIST=REDIST, HIATUS=HIATUS, WAIT_DT=WAIT_DT

;------------------------------------------------------------
;Notes:  SOIL_1 & defaults is stable for dt=20 (not dt=30).
;        SOIL_2 & defaults is stable for dt=10 (not dt=20).

;Examp.  infiltrate, 2000, /silent, dt=300, /sub_ks
;        infiltrate, 1500, /silent, /evap, /plot
;        infiltrate, 3500, /silent, /redist, /plot
;------------------------------------------------------------
if (n_elements(n_steps) eq 0) then n_steps=30
if NOT(keyword_set(WAIT_DT))  then wait_dt=0.007  ;[seconds]

if NOT(keyword_set(TYPE))     then type='SOIL_1'
if NOT(keyword_set(NZ))       then nz=30
if NOT(keyword_set(DT))       then dt=20d    ;[seconds]
if NOT(keyword_set(DZ))       then dz=0.01d  ;[meters]
SUB_KS = keyword_set(SUB_KS)
EVAP   = keyword_set(EVAP)
R_EVAP = keyword_set(R_EVAP)
REDIST = keyword_set(REDIST)
HIATUS = keyword_set(HIATUS)
PLOT   = keyword_set(PLOT)
REPORT = keyword_set(REPORT)

;-----------------------
;Get the soil variables
;-----------------------
Get_Soil_Vars, K_s, K_i, theta_s, theta_i, theta_r, $
               psi_B, psi_A, c, lambda, eta, TYPE=type

;---------------------------
;Initialize the rain vector
;---------------------------
if NOT(keyword_set(R0)) then begin
   if (SUB_KS) then begin
       r0 = K_s / 5d   ;(theta will not reach theta_s) 
       ;r0 = K_s / 15d
       ;r0 = K_s / 50d
   endif else begin
       ;** r0 = K_s * 5d   ;(theta reaches theta_s)
       r0 = K_s * 30d  ;(theta reaches theta_s)
   endelse
endif
if (EVAP) then begin
    ;---------------------------
    ;Rain followed by lesser ET
    ;---------------------------
    r = dblarr(n_steps) - (r0/15d)
    r[0L:n_steps/2] = r0
endif

if (R_EVAP) then begin
    ;---------------------------
    ;Rain followed by lesser ET
    ;---------------------------
    ;** r = dblarr(n_steps) - (r0/15d)
    r = dblarr(n_steps) - (2d * r0)
    ;** r = dblarr(n_steps) - (5d * r0)
    r[0L:n_steps/20] = r0
endif
if (REDIST) then begin
    ;--------------------------
    ;Rain for 1/20 of the time
    ;--------------------------
    r = dblarr(n_steps)
    r[0L:n_steps/20] = r0
endif
if (HIATUS) then begin
    ;-------------------------------
    ;Rain for 1st and last segments
    ;-------------------------------
    r = dblarr(n_steps) + r0
    m  = 60
    p  = 30
    n1 = (n_steps / m)
    r[p*n1: (m-1)*n1] = 0d
endif
if (n_elements(r) eq 0) then begin
    r = dblarr(n_steps) + r0
endif

;----------------------------
;Compute maximum theta-value
;----------------------------
theta_u = Theta_Max(r0, K_s, eta, lambda, theta_s, $
                    theta_r, theta_i, /REPORT)

;----------------------------
;Compute minimum theta-value
;----------------------------
theta_H = Theta_Min(theta_s, theta_r, psi_B, psi_A, c, lambda)
print,'theta_min = ', theta_H
print,' '

;-------------------------
;Initialize other vectors
;-------------------------
theta_last = dblarr(nz) + theta_i

v = dblarr(nz)         ;(zeros) ;*******************
;*** v = dblarr(nz) + K_i
v[0] = r[0]

L = dblarr(nz) + dz        ;(z-dist. between nodes)
z = total(L, /CUMULATIVE, /DOUBLE)
z = [0L, z[0L:nz-2L]]

f = dblarr(n_steps)

;----------------
;Prepare to plot
;----------------
device, decomposed=0
loadct, 39, /silent     ;(rainbow + white)
black = 0
white = 255
line  = string(replicate(45b, 76))
if (PLOT) then window,0

;----------------------------------
;Compute evolution of the profiles
;----------------------------------
for j=0L,(n_steps-1L) do begin

    if (REPORT) then print,'j = ', j+1L

    Update_Theta, theta, theta_last, v, dt, L, theta_s, $
                  theta_r, theta_i, REPORT=REPORT


    psi = Psi_of_Theta(theta, theta_r, theta_s, c, $
                       lambda, psi_B, psi_a, REPORT=REPORT)

    K   = K_of_Psi(psi, K_s, psi_B, psi_a, c, eta, $
                   REPORT=REPORT)

    v   = Flow_Rate(K, psi, theta, L, dt, theta_s, theta_r, $
                    theta_i, theta_H, r[j], REPORT=REPORT)



    if (REPORT) then print, line

    ;----------------------------------
    ;Save infiltration rate at surface
    ;(Could be exfiltration rate.)
    ;----------------------------------
    f[j] = v[0]

    ;--------------------------------------
    ;Option to plot evolving theta profile
    ;--------------------------------------
    if (PLOT) then begin
        yrange = [theta_r, theta_u + 0.01]
        plot, z, theta, psym=-1, color=black, back=white, $
              xtitle='Depth [meters]', /ynozero, ystyle=1, $
              ytitle='Soil moisture', yrange=yrange

        oplot, z, theta_u + dblarr(nz), psym=-1, color=black
        wait, wait_dt
    endif

endfor

;---------------------
;Check surface values
;---------------------
;print,'K_s = ', K_s
;print,'theta_s = ', theta_s
;print,'psi[0:3] = ', psi[0:3]
;print,'theta[0:3] = ', theta[0:3]
;print,'K[0:3]     = ', K[0:3]
;print,'v[0:3]     = ', v[0:3]
;print,' '



;------------------------------
;Check for mass balance errors
;------------------------------
mass_rain = total(r * dt, /DOUBLE)
mass_in   = total(f * dt, /DOUBLE)
mass      = total((theta-theta_i) * L, /DOUBLE)
error     = 100d * (mass_in - mass)/mass_in
print,'Mass rain = ', mass_rain
print,'Mass in   = ', mass_in
print,'Mass      = ', mass
print,'Error     = ', error, ' % ', format='(A12, D6.2, A3)
print,' '



;------------------------------
;Plot only final theta profile
;------------------------------
if NOT(PLOT) then begin
    window, 0
    yrange = [theta_r, theta_u + 0.01]
    plot, z, theta, psym=-1, color=black, back=white, $
          xtitle='Depth [meters]', /ynozero, ystyle=1, $
          ytitle='Soil moisture', yrange=yrange
    oplot, z, theta_u + dblarr(nz), psym=-1, color=black
endif

;-----------------------------------
;Plot surf. infiltration rate curve
;-----------------------------------
window, 1
time = dt * findgen(n_steps) / 60d   ;[minutes]
f    = f * 3600d * 1000d             ;[mm/h]
fmax = max(f, min=fmin)
plot, time, f, psym=-1, color=black, back=white, $
      xtitle='Time [min]', /ynozero, ystyle=1, $
      ytitle='Infiltration Rate [mm/h]', yrange=[fmin,fmax*1.01]

tmin = min(time, max=tmax)
oplot, [tmin,tmax], [0,0], color=black   ;(x-axis)

end;  Infiltrate
;*****************************************************************
function Erc, x, PLOT=PLOT

;-------------------------------------------------------------
;Notes:  To test against figure in R.E. Smith's book (p. 196)
;           IDL>  y = Erc(/PLOT)
;-------------------------------------------------------------
PLOT = keyword_set(PLOT)
if (PLOT) and (n_elements(x) eq 0) then begin
    x = (dindgen(101) * 6d / 100) - 2d
endif

;--------------------------
;Coeffs in a Taylor series
;--------------------------
p  =  0.3275911d
a1 =  0.254829592d
a2 = -0.284496736d
a3 =  1.421413741d
a4 = -1.453152027d
a5 =  1.061405429d

z  = abs(x)
t  = 1d/(1d + (p*z))
y  = (a1*t) + (a2*t^2d) + (a3*t^3d) + (a4*t^4d) + (a5*t^5d)
 
wL = where(x lt 0, nL)
if (nL ne 0) then begin
    y[wL] = (2d * exp(z[wL]^2d)) - y[wL]
endif

;--------------------------
;Optional plot for testing

;--------------------------
if (PLOT) then plot, x, y, yrange=[0,20], xtitle='x', $
                     ytitle='Erc(x)'
    
RETURN, y

end;  Erc
;*****************************************************************
pro BW_Solution, q, z_star, z, ZMAX=zmax, $
                 QS=qs, QI=qi, QR=qr, R=R, T=T, CN=Cn, $
                 G=G, NZ=nz, PLOT_Z1=PLOT_Z1, PLOT_Z2=PLOT_Z2, $
                 PARAM_SET1=PARAM_SET1, $
                 PARAM_SET2=PARAM_SET2 

;-----------------------------------------------------------
;Notes:  This is one of the only analytic solutions to
;        Richards' equation and therefore provides a method
;        to check the accuracy of the numerical methods.
;        It was taken from R.E. Smith's book, but several
;        typos had to be corrected.

;        q is used in place of theta throughout.
;        G is only used to scale z* to z [mm].
;-----------------------------------------------------------
if NOT(keyword_set(NZ)) then nz=300
PLOT_Z1 = keyword_set(PLOT_Z1)
PLOT_Z2 = keyword_set(PLOT_Z2)
PLOT_Z2 = 1b                              ;****************
PARAM_SET1 = keyword_set(PARAM_SET1)
PARAM_SET2 = keyword_set(PARAM_SET2)
if NOT(keyword_set(ZMAX)) then zmax=2.0

;-----------------------------------
;Values used in Matlab code, p. 197
;-----------------------------------
if (PARAM_SET1) then begin
    qs = 0.495
    qi = 0.3d
    qr = 0.2376d
    R  = 1d          ;(equals R* = R/Ks, or (R-Kr)/(Ks-Kr) )
    T  = 2d          ;(equals t/ts, ts = G*(qs-qr)/(Ks-Kr), p. 52)
    G  = 20d         ;(equals lambda_s;  see p. 52,194,201)
    Cn = 1.17d
endif

;----------------------------------
;Values used for Figure A3, p. 194
;----------------------------------
if (PARAM_SET2) then begin
    qs = 0.4
    qi = 0.1d
    qr = 0.05d      ;(this one was not given)
    R  = 2.5d       ;(equals R* = R/Ks, or (R-Kr)/(Ks-Kr) )
    T  = 0.155d     ;(want t=1 hr;  this one found by guessing)
    G  = 20d        ;(equals lambda_s; [mm]; (see p. 194,201))
    Cn = 1.01d      ;(nonlinearity parameter)
    ;Cn = 1.1d
    ;Cn = 2d
endif

if (n_elements(qs) eq 0) then begin
    print,'************************************'
    print,' ERROR: Missing input parameters.'
    print,'************************************'
    print,' '
    RETURN
endif

;-------------------------------------
;Compute initial effective saturation
;-------------------------------------
Si_eff  = (qi - qr) / (qs - qr)

;------------------------------------
;Additional constants (see p. 56-57)
;------------------------------------
rho   = R / (4d * Cn * (Cn-1d))
tau   = 4d * Cn * (Cn-1d) * T
kappa = (2d * rho) - (Si_eff / (Cn - Si_eff))

;--------------------
;Compute zeta vector
;--------------------
z1       = (Cn-1d) + (1.5d * (Cn-Si_eff))
zeta_max = z1 * R * T / (1d - Si_eff)
;-----------------------
;zeta_max seems too big
;-----------------------
;*** zeta_max = zeta_max * 0.6d    ;***************************
zeta     = dindgen(nz+1L) * (zeta_max / double(nz))
;*** zeta = LINSPACE(0, zeta_max, 101)

;------------------------------------------------------------
;NB!  Equation (4.43) on page 57 was corrected for 3 typos
;with the aid of Mathematica on 5/1/06, including:
;  (1) a factor of (1/4) in next line (OK in code on p. 197)
;  (2) a factor of (1/2) that multiplies e1 (OK in code)
;      in equation (4.43) but not in (4.44)
;  (3) a sign error on term u5 (OK in Matlab code, p. 197)
;------------------------------------------------------------
;NB!  Equation (4.44) has no typos.
;------------------------------------------------------------
;NB!  Equation (4.45) on p. 58 uses (1/u) to define B1, but
;     the code (p. 197) uses (du_dzeta/u).  Which is right?
;------------------------------------------------------------
;NB!  Another difference between code (p. 197) and text
;     (p. 57) is whether (rho + 1) or (rho - 1) is used.
;     We define rho_term here to simplify toggling.
;------------------------------------------------------------
rho_term  = (rho + 1d)      ;(from text, p. 57)
rho_term2 = (rho + 1d)
;-------------------------
;From Matlab code, p. 197
;Doesn't seem to work.
;-------------------------
;rho_term  = (rho - 1d)
;rho_term2 = (rho - 1d)
;;rho_term2 = (rho + 1d)
;-------------------------

;print,'rho = ', rho
;print,'rho + 1 = ', rho+1d

;--------------
;Now compute u
;--------------
u1 = exp((kappa * zeta) + (kappa^2d * tau / 4d))
a1 = zeta / sqrt(tau)
a2 = sqrt(rho * rho_term * tau)
a3 = kappa * sqrt(tau) / 2d
;----------------------------
e1 = exp(-1d * a1^2d)
u2 = Erc(a1 + a2)
u3 = Erc(a1 - a2)
u4 = Erc(a1 + a3)
u5 = Erc(a1 - a3)
u  = u1 + (e1/2d)*(u2 + u3 - u4 - u5)    ;(eqn (4.43) in text)

;-----------------
;Compute du_dzeta
;---------------------------------------
;NB!  Kappa, tau and rho are constants.
;---------------------------------------
;NB!  There is an error in Matlab code
;     on p. 197 in defining uze, or else
;     a typo elsewhere.
;---------------------------------------
v1 = kappa * u1

c1 = sqrt(rho * rho_term)
;*** c1 = sqrt(rho + 1d)         ;(In Matlab code, p. 197)
v2 = c1 * u2
v3 = c1 * u3
v4 = (kappa / 2d) * u4
v5 = (kappa / 2d) * u5
du_dzeta = v1 + e1*(v2 - v3 - v4 + v5)    ;(eqn (4.44) in text)

;-----------------------------
;Compute S_eff and then theta
;-----------------------------
;** B0   = 1d + (2d*rho) - (1d/u)      ;(from text, p. 58; typo ?)
B0    = 1d + (2d*rho) - (du_dzeta/u)   ;(from Matlab code, p. 197)
B1    = (1d / B0)
S_eff = Cn * (1d - B1)
q     = (S_eff * (qs - qr)) + qr

;------------------------------------
;Compute dimensionless z* and then z
;Note that rho_term is again used.
;------------------------------------
;lambda_s = G (see. p. 52, 194, 201)
;------------------------------------
part1  = rho * rho_term2 * tau
part2  = ((2d * rho) + 1d) * zeta
z_star = (part1 + part2 - alog(u)) / Cn   ;(from text, p. 58)
;z_star = (part1 + part2 - alog(u))       ;(from Matlab code, p. 197)
z      = z_star * G                       ;(units of [mm], same as G)

;--------------
;Optional plot
;--------------
if (PLOT_Z1 OR PLOT_Z2) then begin
    ;------------------------
    ;Need to open a window ?
    ;------------------------
    if (!d.window eq -1) then begin
        device, decomposed=0
        loadct, 39, /silent   ;(rainbow + white)
        window, 0
    endif
    black = 0
    white = 255

    ;-----------------------------------------------------
    ;Compare to Figure 4.4 (p. 59) and Figure A3 (p. 194)
    ;-----------------------------------------------------
    if (PLOT_Z1) then begin
        tol = 0.1d
        w = where((q - qi) lt tol, nw)
        zmax = 1.3 * z[w[0]]
        plot, z, q, psym=-1, color=black, back=white, $
              xtitle = 'Depth, z [mm]', xrange=[0,zmax], xstyle=1, $
              ytitle = 'Soil moisture, theta', /ynozero, $
              yrange=[qr, 1.01 * qs], ystyle=1
    endif else begin
        ;---------------------------------
        ;Use z_star (dimensionless) vs. z
        ;---------------------------------

        plot, z_star, q, psym=-1, color=black, back=white, $
              xtitle = 'Depth, z*', $
              ;*** xrange=[0,0.6], xstyle=1, $
              xrange=[0.0, zmax], xstyle=1, $
              ytitle = 'Soil moisture, theta', /ynozero, $
              yrange=[qr, 1.01 * qs], ystyle=1
    endelse 
endif

end;  BW_Solution
;*****************************************************************
pro BW_Solution_vs_Time, TM=Tm, ZMAX=zmax, N=n, QS=qs, QI=qi, $
                         QR=qr,R=R, T=T, CN=Cn, G=G,  $
                         PARAM_SET1=PARAM_SET1, $
                         PARAM_SET2=PARAM_SET2

;-----------------
;Keyword defaults

;-----------------
PARAM_SET1 = keyword_set(PARAM_SET1)
PARAM_SET2 = keyword_set(PARAM_SET2)
if ((PARAM_SET1 + PARAM_SET2) eq 0) then begin
    print,'************************************'
    print,' ERROR: Missing input parameters.'
    print,'************************************'
    print,' '
    RETURN
endif
if NOT(keyword_set(N)) then n=100

;-------------------------------------
;Set the nondimensional stopping time
;Same Tm allows about same Zmax.
;-------------------------------------
Tm = 1d
if NOT(keyword_set(ZMAX)) then zmax=3.5

;-----------------------------------
;Values used in Matlab code, p. 197
;-----------------------------------
if (PARAM_SET1) then begin
    qs = 0.495
    qi = 0.3d
    qr = 0.2376d
    R  = 1d          ;(equals R* = R/Ks, or (R-Kr)/(Ks-Kr) )
    G  = 20d         ;(equals lambda_s;  see p. 52,194,201)
    Cn = 1.17d
    ;** T  = 2d      ;(equals t/ts, ts = G*(qs-qr)/(Ks-Kr), p. 52)
endif

;----------------------------------
;Values used for Figure A3, p. 194
;----------------------------------
if (PARAM_SET2) then begin
    qs = 0.4
    qi = 0.1d
    qr = 0.05d      ;(this one was not given)
    R  = 2.5d       ;(equals R* = R/Ks, or (R-Kr)/(Ks-Kr) )
    ;** R  = 1.5d
    G  = 20d        ;(equals lambda_s; [mm]; (see p. 194,201))
    Cn = 1.01d      ;(nonlinearity parameter)
    ;** T  = 0.155d     ;(want t=1 hr;  this one found by guessing)
endif

;----------------
;Prepare to plot
;----------------
device, decomposed=0
loadct, 39, /silent   ;(rainbow + white)
window, 0

;----------------------------
;Evolve the solution in time
;----------------------------
for k=0L,n do begin
     t = (double(k)/double(n)) * Tm   ;(nondimensional time)
     BW_Solution, QS=qs, QI=qi, QR=qr, R=R, $
                  CN=Cn, G=G, T=t, ZMAX=zmax, /PLOT_Z2
     wait, 0.07d
endfor

end;  BW_Solution_vs_Time
;*****************************************************************
pro Fairbanks_Test

;---------------------------------------------------------------
;Notes:  Bob sent a plot of theta vs. depth to water table
;        for a silt loam from Fairbanks, AK, using data from
;        Kane et al., 1978.  In that plot, the data was fit
;        by an exponential curve and plotted with a logarithmic
;        x-axis.  Here, we are using the "transitional Brooks-
;        -Corey" (abbrev. T-BC) equations using parameters
;        typical of silt loam (from tables) and the assumption
;        that the pressure head, psi, is simply linear.  We
;        expect that psi will be approximately linear near the
;        water table, but here we are assuming it is linear
;        from the surface down to the water table.  The value
;        of psi at the water table is 0, of course, and the
;        value at the surface is computed from the theta value
;        that is indicated by the data near the surface, again
;        using the T-BC equations.

;        The resulting plot looks quite similar to the one
;        Bob sent for depths to water table from 0 to about 10
;        meters.  This suggests that we may be able to solve
;        Richards' equation for surface layers and then simply
;        approximate conditions from the bottom of these down
;        to the water table by assuming psi is linear and is
;        zero at the water table.  If so, we can save the
;        computational cost of solving Richards' equation at
;        depths that are far from the surface.
;---------------------------------------------------------------

;--------------------------
;Params for silt loam soil
;--------------------------
Ks = 7.2e-4
;** qs   = 0.485d
qs   = 0.52d
qr   = 0.01d
psiB = -78.6d    ;[cm]
b    = 5.3d
;** lam  = (1d/b)
;** lam  = 0.25d     ;**********************
;** lam  = 0.35d
lam  = 0.45d
c    = 1d

q_top   = 0.12d
Se_top  = (q_top - qr)/(qs - qr)
psi_top = psiB *(Se_top^(-c/lam) - 1d)^(1d/c)

zwt = 100d   ;[meters]
a   = (psi_top / zwt)
;** a   = 0.02d * a    ;********************
;** a   = 0.09d * a   ;(for lam = 0.25)
print,'a = ', a
nz  = 100000L
z   = dindgen(nz)*(zwt / nz)
psi = a*(zwt - z)

theta = ((1d + (psi/psiB)^c)^(-lam/c))*(qs-qr) + qr

plot, (zwt-z), theta, /xlog, /ynozero, $
       yrange=[0.1d, 0.53], ystyle=1, $
       xtitle='Depth to water table (m)', $
       ytitle='Soil Moisture Content (% Volume)'

;--------------------------------------
;Overplot Kane et al. best fit to data
;--------------------------------------
z2     = (zwt - z)
theta2 = 11.765d + (39.35d * exp(-z2/9.462d))
theta2 = (theta2 / 100d)  ;(percent to decimal)
oplot, z2, theta2, linestyle=2

;*** plot, z2, theta2, /xlog, /ynozero

end;  Fairbanks_Test
;*****************************************************************

