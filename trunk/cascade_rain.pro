
;***********************************************************************
;    cascade_rain.pro

;--------------------------------------------
;    Copyright (c) 1995-2005, Thomas M. Over
;--------------------------------------------
;    Cascade       (function)
;    Evolve_BD     (function)
;    Evolve_OU     (function)
;    Cascade_Rain  (function)  Creates RTS file of simulated rain.

;-----------------------------------------
;    Copyright (c) 2005, Scott D. Peckham
;-----------------------------------------
;    GUI_Make_Cascade_Rain_RTS_event
;    GUI_Make_Cascade_Rain_RTS

;***********************************************************************
;  (1) How to run it:

;  For example,

;  ntimes  = 100
;  kbparms = [10, 0.5, 2]
;  betarr  = replicate(0.1, 100)
;  kyparms = [10, 0.5, 2]
;  a       = 0.1
;  b       = 4
;  nlevs   = [10, 2]
;  seed    = 253720L
;  out     = Smtmscl2(ntimes, kbparms, betarr, kyparms, $
;                     a, b, nlevs, seed)
;      
;  simulates a 100 timestep 10-level cascade with branching number
;  b = 4 (so splitting 2 by 2 at each level), averaged back up to 8
;  levels at the end, with beta fixed at 0.1 and a (the parameter of
;  the lognormal model) also equal to 0.1, and initial random number
;  generator seed of 253720. The beta and lognormal processes evolve
;  with time constants kmin of 10 time steps at the smallest scale
;  and increase with scale up until level 2 as:
;      k[level] = kmin*b^(H*(nlevels-level)),
;  so in this case with b = 4 and H = 0.5, the time constant increases
;  by a factor of 2 at each "higher" (in the sense of larger scale)
;  level, until level 2 when it stops changing. The output would then
;  be fltarr(256,256,100)

;  (2) How to use it:

;  The idea would be that you just create a time-evolving cascade
;  that covers your basin and let it run.  It will take some time;
;  this run took about 10 minutes on my old Win98 PC at home.
;  Letting stoplevb and stoplevy be greater than 1 will stop the
;  scaling at the longest time scales, giving more of a Poisson-like
;  large-scale temporal structure rather than scaling.

;  As to realistic parameter values, that is a whole nother story.
;  For now, let it suffice to say a and beta should be "small"
;  (in the neighborhood of 0.1) and H about 0.5.

;  Kmin should be at least 5; the smaller it is, the less well the
;  stochastic processes specified are simulated, but the larger it
;  is, the more time your run takes (for a given number of evolutions).

;  I would further note that the spatial scaling assumption for this
;  structure and these parameters has only been checked for radar
;  scales, i.e., 2 to 256 km.

;  But notice this is a Lagrangian model: it evolves in time but does
;  not advect.  This is not, in actuality, very realistic; in fact,
;  the motion across a basin dominates the temporal variability at
;  storm time scales. I have done a set of runs with advection, but
;  I don't think the code is such good shape. I will look for it and
;  perhaps send you another set of code. But hopefully this will get
;  you started.

;***********************************************************************
function Cascade, Warr, b, nlevels

;--------------------------------------------------------
;Notes:  This routine converts a 1-d array of W's into
;        a 2d-cascade of nlevels with branching number b
;        Written by Thomas M. Over (9-12-93).
;--------------------------------------------------------

;----------------------------------------------
;Check that size of Warr matches b and nlevels
;----------------------------------------------
nWs = b^(findgen(nlevels) + 1)
totnWs = total(nWs)
dum    = size(Warr)
if (dum[1] ne totnWs) then begin
    print, 'Size of Warr is incompatible with b and nlevels.'
    RETURN, -1
endif

;----------------------------------------
;Convert 1D array of W's into 2D cascade
;----------------------------------------
hicnt  = -1
newarr = fltarr(1,1)
newarr[0,0] = 1.0

for level=1,nlevels do begin
    dim    = sqrt(b)^level
    oldarr = rebin(newarr, dim, dim, /sample)
    locnt  = hicnt + 1
    hicnt  = hicnt + dim^2
    newarr = reform(Warr[locnt:hicnt], dim, dim) * oldarr
endfor

RETURN, newarr

end;  Cascade
;***********************************************************************
function Evolve_BD, Warr, oldp, newp, posval, time, seed

p       = (oldp + newp)/2.
nWs     = n_elements(Warr)
newWarr = fltarr(nWs)
rnum    = randomu(seed, nWs)

;-------------------------------------
;Make transitions from posval to zero
;-------------------------------------
p10 = p*(1-exp(-time))
subarr = where((Warr gt 0.0) and (rnum le p10), count)
if (count gt 0) then newWarr[subarr] = 0.0
subarr = where(Warr gt 0.0 and rnum gt p10, count)
if (count gt 0) then newWarr[subarr] = posval

;-------------------------------------
;Make transitions from zero to posval
;-------------------------------------
p01 = 1-p-(1-p)*exp(-time)
subarr = where((Warr eq 0.0) and (rnum le p01), count)
if (count gt 0) then newWarr[subarr] = posval
subarr = where((Warr eq 0.0) and (rnum gt p01), count)
if (count gt 0) then newWarr[subarr] = 0.0

RETURN, newWarr

end;  Evolve_BD
;***********************************************************************
function Evolve_OU, Xarr, time, seed

;----------------------------------------------------------------- 
;NOTES:  This routine evolves a 1-d array of stationary
;        Ornstein-Uhlenbeck processes with unit variance rho=1

;        (Reference: Leo Breiman, Probability, SIAM, 1992,
;        pp. 347-350)

;        The Ornstein-Uhlenbeck process X(t) has transition
;        probabilities which are Gaussian with mean =
;        x*exp(-alpha*t) and variance = rho*(1-exp(-2*alpha*t))
;        where x is value at t=0 (Breiman, p. 349; Bhattacharya
;        & Waymire, p. 370), so for each process we simulate a
;        Gaussian RV with this distribution with a small time
;        step and add it to the present value.


;        Adapted from evolve_bd01.pro. (Thomas M. Over, 3-15-95)
;-----------------------------------------------------------------
;INPUT VARIABLES
;  Xarr - O-U process at time t=0 (present)
;  time - timestep (as a fraction of characteristic time, 1/alpha)
;  seed - seed for random number generator

;OUTPUT
;  newXarr - O-U process at time t = time/alpha
;-----------------------------------------------------------------

;-----------------------------------------
;Compute standard deviation of transition
;probability distribution (rho=1)
;-----------------------------------------
std = sqrt(1-exp(-2*time))

;----------------------------------------------
;Apply the transition probability distribution
;----------------------------------------------
nXs     = n_elements(Xarr)
newXarr = randomn(seed,nXs)*std + temporary(Xarr)*exp(-time)

RETURN, newXarr

end;  Evolve_OU
;***********************************************************************
function Cascade_Rain, ncols, nrows, nframes, max_rate, $
                 b, nlev_up, seed, $
                 beta, H_B, kmin_B, stop_lev_B, $
                 a,    H_Y, kmin_Y, stop_lev_Y, $
                 RTS_FILE=RTS_file, RTI_FILE=RTI_file, $
                 NLEVELS=nlevels, MSG_BOX_ID=MSG_BOX_ID

;--------------------------------------------------------------------
;NOTES:  This routine simulates time evolution of a 2d random
;        cascade with generator processes that are the "compos-
;        ition" of two independent generator processes, i.e.,

;        W(t) = B(t)Y(t), where B(t) is a "space-time beta model"
;        B(t) = I(t)/P(I(t)>0), where I(t) is a birth-death process
;               on {0,1} and
;        Y(t) is a stationary process on positive support,
;        independent of B(t).

;        In particular, Y(t) = b^(-a^2*alog(b)/2 + a*X(t) where
;        X(t) is an Ornstein-Uhlenbeck process with unit variance.
;        In this version, the characteristic time of evolution of
;        the processes varies with scale according to:

;	   kb(level) = kbmax*sqrt(b)^(Hbeta*(nlevels-level)) and
;	   ky(level) = kymax*sqrt(b)^(Hpos*(nlevels-level))

;        so that larger scale processes evolve more slowly and
;        scaling in time is obtained.

;        Adapted from simtim2f.pro  (Thomas M. Over, 7-98)

;        Modified to allow scaling of fluctuation times to be
;        stopped at some level through use of the stop_lev parameter
;        (Thomas M. Over, 1-99)

;        Modified further to accept the arguments: ncols, nrows,
;        RTS_file & RTI_file and keywords BETA_MODEL & LOGNORMAL.
;        NLEVELS is retained as an optional keyword, in which
;        case the ncols & nrows arguments are ignored.  The RTS_FILE
;        keyword allows grid sequence to be saved as an RTS file.
;        (Scott D. Peckham, Aug. 2005)

;-----------------
; Input parameters
;-----------------
;  BETA_MODEL – used for the Beta Model component
;  LOGNORMAL  – used for the Lognormal Model component
;  nframes    - number of timesteps to simulate
;  kmin       - characteristic time of evolution of B(t) (BETA_MODEL)
;               or Y(t) (LOGNORMAL) in timesteps at nlevels
;  H          - scaling exponent of characteristic time of B(t) or Y(t)
;  stop_lev   - level at which to stop increasing the characteristic
;               time of B(t) or Y(t). (assuming you started increasing
;               it at the "bottom", i.e., the smallest scale simulated,
;               level nlevels) (minimum value: 1 since no process is
;               simulated at the zeroth level)
;  b          - two-dimensional branching number (i.e., sqrt(b) in each
;               direction, so at least 4 (and a perfect square ?))
;  nlevels    - number of levels in cascade
;  nlevup     - number of levels to average up (ge zero and le nlevels)
;  seed       - seed for the random number generation
;  beta       – beta model parameter in the range [0,1]
;  a          - the parameter of Y(t);
;               set to zero for Y(t)=1 for all time 
;-----------------------------------------------------------------------
BETA_MODEL = (beta gt 0.0)
LOGNORMAL  = (a gt 0.0)
max_rate   = float(max_rate)

;-------------------------------------------
;Set nlevels using ncols, nrows & nlev_up ?
;-------------------------------------------
if NOT(keyword_set(NLEVELS)) then begin
    b1       = sqrt(b)
    levs     = lindgen(12) + 1L
    sizes    = 2L^levs
    w        = where((sizes GE ncols) AND (sizes GE nrows), nw)
    if (nw eq 0) then begin
        msg = [' ',$
        'Number of columns and rows must be <= 4096.', $
        'Larger DEMs are not yet supported.', $
        ' ']
        result = GUI_Message(msg, /INFO, TITLE='ERROR')
        RETURN, -1L
    endif
    netlevs  = levs[w[0]]
    nlevels  = netlevs + nlev_up
endif else begin
    netlevs  = nlevels - nlev_up
endelse

;---------------------
;Initialize variables
;---------------------
nWs    = b^(findgen(nlevels)+1)
totnWs = total(nWs)

;------------------------------
;Set vars for the BETA process
;------------------------------
if (BETA_MODEL) then begin
    ;---------------------------------
    ;All values in array now the same
    ;---------------------------------
    beta_arr = fltarr(nframes) + beta
    ;-----------------------------
    ;Is beta_arr in range [0,1] ?
    ;-----------------------------
    if (min(beta_arr) lt 0.) OR (max(beta_arr) gt 1.) then begin
        msg = [' ',$
        'ERROR:  Beta value must be between 0 and 1.',$
        ' ']
        result = GUI_Message(msg, /INFO, TITLE='ERROR')
        RETURN, -1L
    endif

    r = 1-b^(-beta_arr)

    ;------------------------
    ;Set kB for BETA process
    ;------------------------
    ramp_B = (nlevels - stop_lev_B)
    ramp_B = ramp_B - (indgen(nlevels - stop_lev_B + 1))
    kB = kmin_B * b^(H_B * ramp_B)
    if (stop_lev_B gt 1) then kB = [replicate(max(kB),stop_lev_B-1),kB]

    ;---------------------------------------
    ;Construct initial array of B processes
    ;---------------------------------------
    initbeta   = beta_arr[0]
    initBzfrac = 1-b^(-initbeta)
    Barr  = fltarr(totnWs)
    rnums = randomu(seed,totnWs)
    subs  = where(rnums le initBzfrac,count)
    if (count gt 0) then Barr[subs] = 0.0
    subs = where(rnums gt initBzfrac, count)
    if (count gt 0) then Barr[subs] = 1./(1-initBzfrac)
    rnums = 0.0
endif else Barr = replicate(1.,totnWs)


;-----------------------------------
;Set vars for the LOGNORMAL process
;-----------------------------------
if (LOGNORMAL) then begin
    ;-----------------------------
    ;Set kY for LOGNORMAL process
    ;-----------------------------
    ramp_Y = (nlevels - stop_lev_Y)
    ramp_Y = ramp_Y - (indgen(nlevels - stop_lev_Y + 1))
    kY = kmin_Y * b^(H_Y * ramp_Y)
    if (stop_lev_Y gt 1) then kY = [replicate(max(kY),stop_lev_Y-1),kY]

    ;---------------------------------------
    ;Construct initial array of Y processes
    ;---------------------------------------
    seed = seed[0] * (-98347L)
    Xarr = randomn(seed, totnWs)
    Yarr = b^(-a^2*alog(b)/2 + a*Xarr)
endif else Yarr = replicate(1., totnWs)


;----------------------
;Construct the W array
;----------------------
Warr = Barr * Yarr

;------------------------------------
;Convert array of W's into a cascade
;------------------------------------
muarr = Cascade(Warr, b, nlevels)
muarr = rebin(temporary(muarr), b1^netlevs, b1^netlevs)

;--------------------------------------------
;Write first frame to RTS file or 3D array ?
;--------------------------------------------
if (keyword_set(RTS_FILE)) then begin
    Read_RTI_File, RTI_File, info
    TF_Get_LUN, RTS_unit, RTS_file
    openw, RTS_unit, RTS_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
    frame = float(muarr[0:ncols-1,0:nrows-1])
    fmax  = max(frame, /NAN)
    frame = frame * (max_rate / fmax)  ;*******************
    writeu, RTS_unit, frame
    store_mu = -1
endif else begin
    store_mu = fltarr(b1^netlevs, b1^netlevs, nframes, /nozero)
    store_mu[*,*,0] = muarr
    muarr = 0.0
endelse

;---------------------------
;Evolve the cascade in time
;---------------------------
chgcntarr = lonarr(nlevels)
for t=1,(nframes-1) do begin
    cnt = 0L
    for level=1,nlevels do begin
        ;--------------------------------------
        ;Print a status message in message box
        ;--------------------------------------
        if (keyword_set(MSG_BOX_ID)) then begin
            tstr     = 'Frame = ' + TF_String(t) + ',  '
            levstr   = 'Level = ' + TF_String(level)
            stat_msg = tstr + levstr
            widget_control, MSG_BOX_ID, set_value=stat_msg
            wait, 0.1  ;***********
        endif

        ;------------------------------
        ;Change the random number seed
        ;---------------------------------------------------
        ;8/28/05.  If RANDOMU or RANDOMN is called with a
        ;scalar seed, then seed is returned as an array.
        ;A subsequent call to RANDOMU or RANDOMN will then
        ;be passed this array instead of a new scalar, and
        ;an error message is produced, such as:
        ;"RANDOMN: Seed argument is corrupt"
        ;Here we create a new scalar seed by multiplying
        ;seed[0] by an arbitrary but fixed number.
        ;---------------------------------------------------
        ;*** print,'n_elements(seed) = ', n_elements(seed)
        ;*** print,'seed[0] = ', seed[0]
        seed = seed[0] * (-924341L)
        ;*** print,'seed = ', seed

        ;---------------------------------
        ;Use Evolve_BD when p(t) is known
        ;-------------------------------------------------------
        ;In this version, beta_arr contains only a single value
        ;that is used for all times.  For dynamic forcing, we
        ;would have (beta = f(t)) and need to choose some
        ;average beta over the timestep.  Evolve_BD averages
        ;the two values of beta given.
        ;-----------------------------------------------------
        dim = long(b)^level
        if (BETA_MODEL) then begin
            Bsubarr = Barr[cnt:cnt+dim-1]
            Bsubarr = Evolve_BD(Bsubarr, r[t-1], r[t], 1/(1-r[t]), $
                                1./kB[level-1], seed)
            subs = where(bsubarr ne barr[cnt:cnt+dim-1], chgcnt)
            chgcntarr[level-1] = chgcntarr[level-1] + chgcnt
            Barr[cnt:cnt+dim-1] = Bsubarr
        endif

        if (LOGNORMAL) then begin
            seed = seed[0] * (-46347582L)
            Xsubarr = Xarr[cnt:cnt+dim-1]
            Xsubarr = Evolve_OU(Xsubarr, 1./kY[level-1], seed)
            Xarr[cnt:cnt+dim-1] = Xsubarr
        endif

        cnt = (cnt + dim)
    endfor

    if NOT(BETA_MODEL) then Barr = replicate(1.,totnWs)

    if (LOGNORMAL)  then begin
        Yarr = b^(-a^2*alog(b)/2 + a*Xarr)
    endif else begin
        Yarr = replicate(1.,totnWs)
    endelse

    Warr  = Barr * Yarr
    muarr = Cascade(Warr, b, nlevels)
    muarr = rebin(temporary(muarr), b1^netlevs, b1^netlevs)

    ;-----------------------------------------
    ;Write frame to RTS file or to 3D array ?
    ;-----------------------------------------
    if (keyword_set(RTS_FILE)) then begin
        frame = float(muarr[0:ncols-1,0:nrows-1])
        fmax  = max(frame, /NAN)
        frame = frame * (max_rate / fmax)  ;*******************
        writeu, RTS_unit, frame
    endif else begin
        store_mu[*,*,t] = muarr
    endelse

endfor

;---------------------
;Close the RTS file ?
;---------------------
if (keyword_set(RTS_FILE)) then free_lun,RTS_unit

;--------------------------------------
;Write finished message to message box
;--------------------------------------
if (keyword_set(MSG_BOX_ID)) then begin
    stat_msg = 'Finished.'
    widget_control, MSG_BOX_ID, set_value=stat_msg
endif

;----------------------------
;Return the result, store_mu
;----------------------------
RETURN, store_mu

end;  Cascade_Rain 
;***********************************************************************
pro GUI_Make_Cascade_Rain_RTS_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*****************
'MODEL_TYPE' : $
;*****************
state.model_type = event.index

;************
'START' : $
;************
begin
;------------------
;Read the settings
;------------------
Read_Text_Box, state.RTS_file_ID, RTS_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, RTS_file, OK
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.RTI_file_ID, RTI_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.ncols_ID, ncols, OK, /LONG
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.nrows_ID, nrows, OK, /LONG
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.nframes_ID, nframes, OK, /LONG
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.max_rate_ID, max_rate, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.b_ID, b, OK, /INTEGER
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.H_ID, H, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.beta_ID, beta, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.a_ID, a, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.kmin_ID, kmin, OK, /INTEGER   ;(OR FLOAT ??)
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.stop_lev_ID, stop_lev, OK, /INTEGER
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.nlev_up_ID, nlev_up, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.seed_ID, seed, OK, /LONG
seed = long(seed)  ;*****
if NOT(OK) then RETURN

kmin_B = kmin
kmin_Y = kmin
H_B    = H
H_Y    = H
stop_lev_B = stop_lev
stop_lev_Y = stop_lev

;---------------------------------------
;Call routine that creates the RTS file 
;---------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
mu = Cascade_Rain(ncols, nrows, nframes, max_rate, $
             b, nlev_up, seed, $
             beta, H_B, kmin_B, stop_lev_B, $
             a,    H_Y, kmin_Y, stop_lev_Y, $
             RTS_FILE=RTS_file, RTI_FILE=RTI_file, $
             MSG_BOX_ID=state.msg_box_ID)
widget_control, event.ID, sensitive=1    ;(enable button)

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating new RTS file.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'cascade_rain.htm'

;************
'CLOSE' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CLOSE') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Make_Cascade_Rain_RTS_event
;*****************************************************************
pro GUI_Make_Cascade_Rain_RTS, leader

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
prefix       = mstate.run_vars.prefix
RTS_file     = prefix + '_casc-rain.rts'
RTI_file     = prefix + '.rti'
ncols        = mstate.grid_vars.ncols
nrows        = mstate.grid_vars.nrows

;----------------------------------
;Compute required number of levels
;to achieve ncols & nrows
;----------------------------------
b       = 4
b1      = sqrt(b)
nlev_up = 2
;-----------------
levs  = lindgen(12) + 1L
sizes = 2L^levs
w     = where((sizes GE ncols) AND (sizes GE nrows), nw)
if (nw eq 0) then begin
    msg = [' ', $
   'Number of columns and rows must be <= 4096.', $
   'Larger DEMs are not yet supported.', $
    ' ']
    result = GUI_Message(msg, /INFO, TITLE='ERROR')
    RETURN
endif
netlevs  = levs[w[0]]
nlevels  = netlevs + nlev_up

;--------------------------
;Construct default strings
;--------------------------
ncols_str    = TF_String(ncols)
nrows_str    = TF_String(nrows)
nlevels_str  = TF_String(nlevels)
nframes_str  = '20'
maxrate_str  = '100'  ;[mm/hr]
b_str        = '4'
H_str        = '0.5'
kmin_str     = '10'
stop_lev_str = '2'
nlev_up_str  = '2'
beta_str     = '0.1'
a_str        = '0.1'
seed_str     = '173838'
;*** seed_str     = '253720'

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, msg_box_ID:0L, $
ncols_ID:0L, nrows_ID:0L, nlevels_ID:0L, nframes_ID:0L, $
max_rate_ID:0L, b_ID:0L, kmin_ID:0L, H_ID:0L, $
beta_ID:0L, a_ID:0L, stop_lev_ID:0L, nlev_up_ID:0L, $
seed_ID:0L, RTS_file_ID:0L, RTI_file_ID:0L }

;----------------
;Initialize vars
;----------------
ngap = 6
XS   = 20
XS2  = 8

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Make Cascade Rain RTS Dialog ', $
            /COLUMN, LEADER=leader
MID = widget_base(MB, /ROW)
  B1 = widget_base(MID, /COLUMN, /FRAME)
  B2 = widget_base(MID, /COLUMN, /FRAME)
BOT = widget_base(MB, /ROW)

;------------------------------------
;Get name of new RTS file & RTI file
;------------------------------------
F1 = widget_base(B1, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Name of new RTS file: ')
  F12 = widget_text(F1, VALUE=RTS_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTS_file_ID = F12
;---------------------------------------------------------
F2 = widget_base(B1, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Name of RTI file: ')
  F22 = widget_text(F2, VALUE=RTI_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTI_file_ID = F22

;---------------------------
;Get ncols, nrows & nframes
;---------------------------
NC = widget_base(B1, /ROW, SPACE=ngap)
  NC1 = widget_label(NC, VALUE='Number of columns: ')
  NC2 = widget_text(NC, VALUE=ncols_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.ncols_ID = NC2
;---------------------------------------------------------
NR = widget_base(B1, /ROW, SPACE=ngap)
  NR1 = widget_label(NR, VALUE='Number of rows: ')
  NR2 = widget_text(NR, VALUE=nrows_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.nrows_ID = NR2
;---------------------------------------------------------
NL = widget_base(B1, /ROW, SPACE=ngap)
  NL1 = widget_label(NL, VALUE='Number of levels: ')
  NL2 = widget_text(NL, VALUE=nlevels_str, XSIZE=XS2)
  state.nlevels_ID = NL2
;---------------------------------------------------------
NF = widget_base(B1, /ROW, SPACE=ngap)
  NF1 = widget_label(NF, VALUE='Number of frames: ')
  NF2 = widget_text(NF, VALUE=nframes_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.nframes_ID = NF2

;------------------------------
;Get the max rainrate in mm/hr
;------------------------------
MR = widget_base(B1, /ROW, SPACE=ngap)
  MR1 = widget_label(MR, VALUE='Maximum rainrate: ', UVALUE='NONE')
  MR2 = widget_text(MR, VALUE=maxrate_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  MR3 = widget_label(MR, VALUE=' [mm / hr]')
  state.max_rate_ID = MR2

;-----------------------------
;Get the remaining parameters
;-----------------------------
GB = widget_base(B2, /ROW, SPACE=ngap)
  GB1 = widget_label(GB, VALUE='Branching number, b: ')
  GB2 = widget_text(GB, VALUE=b_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.B_ID = GB2
  GB3 = widget_label(GB, VALUE=' = n^2')
;---------------------------------------------------------
GH = widget_base(B2, /ROW, SPACE=ngap)
  GH1 = widget_label(GH, VALUE='Scaling exponent, H: ')
  GH2 = widget_text(GH, VALUE=H_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.H_ID = GH2
  GH3 = widget_label(GH, VALUE=' in [0,1]', UVALUE='NONE')
;---------------------------------------------------------------
BP = widget_base(B2, /ROW, SPACE=ngap)
  BP1 = widget_label(BP, VALUE='Beta parameter: ')
  BP2 = widget_text(BP, VALUE=beta_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.beta_ID = BP2
  BP3 = widget_label(BP, VALUE=' in [0,1]')
;---------------------------------------------------------------
AP = widget_base(B2, /ROW, SPACE=ngap)
  AP1 = widget_label(AP, VALUE='Lognormal param, a: ')
  AP2 = widget_text(AP, VALUE=a_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.a_ID = AP2
  AP3 = widget_label(AP, VALUE=' in [0,1]')
;----------------------------------------------------------
GK = widget_base(B2, /ROW, SPACE=ngap)
  GK1 = widget_label(GK, VALUE='Characteristic time: ')
  GK2 = widget_text(GK, VALUE=kmin_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.kmin_ID = GK2
  GK3 = widget_label(GK, VALUE=' >= 5')
;---------------------------------------------------------------
SL = widget_base(B2, /ROW, SPACE=ngap) 
SL1 = widget_label(SL, VALUE='Stop level for char. time: ')
  SL2 = widget_text(SL, VALUE=stop_lev_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.stop_lev_ID = SL2
  SL3 = widget_label(SL, VALUE=' in {1,nlevs}')
;---------------------------------------------------------------
UL = widget_base(B2, /ROW, SPACE=ngap)
  UL1 = widget_label(UL, VALUE='Number of levels up: ')
  UL2 = widget_text(UL, VALUE=nlev_up_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.nlev_up_ID = UL2
  UL3 = widget_label(UL, VALUE=' in {0,nlevs}')
;---------------------------------------------------------------
RS = widget_base(B2, /ROW, SPACE=ngap)
  RS1 = widget_label(RS, VALUE='Random number seed: ')
  RS2 = widget_text(RS, VALUE=seed_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.seed_ID = RS2

Align_Text_Boxes, [F11, F21, NC1, NR1, NL1, NF1, MR1]
Align_Text_Boxes, [GB1, GH1, BP1, AP1, GK1, SL1, UL1, RS1]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, BOT, /START, /HELP, /CLOSE

;---------------------
;A status message box
;---------------------
MS = widget_base(BOT, /ROW, SPACE=ngap)
  MS1 = widget_label(MS, VALUE='  Status: ')
  MS2 = widget_text(MS, VALUE='Ready.', XSIZE=40)
  state.msg_box_ID = MS2

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Make_Cascade_Rain_RTS', XOFF=480

END;  GUI_Make_Cascade_Rain_RTS 
;*****************************************************************

