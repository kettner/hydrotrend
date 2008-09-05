
;*****************************************************************
;   init_depth.pro

;   Copyright (c) 2005-2006, Scott D. Peckham
;   Created:  Jul-Aug 2005

;*****************************************************************

;   Make_Initial_Depth_Grid
;   GUI_Make_Init_Depth_Grid_event
;   GUI_Make_Init_Depth_Grid

;*****************************************************************
pro Make_Initial_Depth_Grid, d0_file, v0_file, RTI_file, $
                 width_file, area_file, slope_file, manning_file, $
                 bank_angle_file, recharge_rate, $
                 MSG_BOX_ID=MSG_BOX_ID 

;----------------------------------------------------------
;Note:  This routine uses a grid-based Newton-Raphson
;       iterative scheme to solve an equation for water
;       depth as a function of:
;           w = bed width [meters]
;           A = upstream area [km^2]
;           S = downstream slope [none]
;           n = Manning roughness parameter  [s/m^(1/3)]
;           theta = bank angle [degrees ??]

;       Newton's method solves an equation F(d)=0, by
;       iterating the equation:
;             d(j+1) = d(j) - [F(d)/F'(d)]
;       until the difference between d(j+1) and d(j)
;       drops below the specified tolerance for all
;       pixels in the grid.  This usually takes about
;       10 iterations.  Max # of iterations is max_tries.

;       Note that NaNs, as can occur on the edges of the
;       slope grid, are preserved in the depth grid.

;----------------------------------------------------------
;NB!    Are multiple roots possible?  If so, does an
;       alternate root correspond to case of a hydraulic
;       jump?  Once we have solved for d, we can easily
;       compute velocity, u, and then use it to select
;       the appropriate root.
;----------------------------------------------------------

;------------------------------
;Set tolerance for convergence
;------------------------------
if NOT(keyword_set(TOL)) then tol = 0.001d
Rb = recharge_rate
;** print,'Rb = ', Rb  ;****************

;-----------------------------
;Read grid info from RTI file
;-----------------------------
Read_RTI_File, RTI_file, info
byte_order = info.byte_order

;---------------------
;Read the input grids
;---------------------
Read_Grid, w, width_file, TYPE='FLOAT', /SILENT
Read_Grid, A, area_file,  TYPE='FLOAT', /SILENT
Read_Grid, S, slope_file, TYPE='FLOAT', /SILENT
Read_Grid, n, manning_file, TYPE='FLOAT', /SILENT
Read_Grid, theta, bank_angle_file, TYPE='FLOAT', /SILENT

;-------------------------------
;Convert A from [km^2] to [m^2]
;-------------------------------
A = A * 1000000d  ;[m^2]

;----------------------------------
;Convert theta from [deg] to [rad]
;----------------------------------
theta = theta * (!dpi / 180d)  ;[radians]

;------------------------------------
;Remove zero slopes from slope grid
;or else we'll get NaNs for them.
;------------------------------------
;Better to use profile-smoothed DEM;
;then zero slopes should be gone.
;------------------------------------
Remove_Bad_Slopes, S, /FLOAT

;---------------------------------------
;Construct the quantity, K, where:
;   F(d) = Ac^(5/3) – [K * P^(2/3)] = 0
;---------------------------------------
K = (Rb * A * n) / sqrt(S)

;-----------------------------------------------
;Initialize depth grid, d, for Newton iteration
;by assuming the case where d << w and R = d,
;where R = Ac/P = hydraulic radius.
;----------------------------------------------
;d = K / w^(3d/5d)
;----------------------------------------------
;Initialize to very large depth so that we can
;get largest root (and smallest velocity) if
;there are multiple roots.
;----------------------------------------------
d0 = 500d    ;[meters]
d  = dblarr(info.ncols, info.nrows) + d0

;----------------
;Initialize vars
;----------------
pow1 = (5d/3d)
pow2 = (2d/3d)
pow3 = (-1d/3d)
;---------------
n_tries   = 0L
max_tries = 40L

;-----------------------------
;Iterate entire grid to get d
;-----------------------------
DONE = 0b
while NOT(DONE) do begin
    ;-------------------------------
    ;Use grid-based Newton's method
    ;----------------------------------------------
    ;See Notes for TF_Tan function in utils_TF.pro
    ;----------------------------------------------
    last_d = d
    Ac     = d * (w + (d * TF_Tan(theta)))
    P      = w + (2 * d / cos(theta))
    numer  = Ac^pow1 - (K * P^pow2)
    term1  = pow1 * Ac^pow2 * (w + (2*d*TF_Tan(theta)))
    term2  = pow2 * K * P^pow3 * (2d / cos(theta))
    denom  = term1 - term2
    d      = d - (numer/denom)

    ;--------------
    ;For debugging
    ;--------------
    ;*** print,'min(denom) = ', min(denom)

    ;-----------------------------
    ;Compute difference from goal
    ;Note: gap = abs(numer/denom)
    ;-----------------------------
    gap = abs(d - last_d)
    ;*** gmin = min(gap, max=gmax, /NAN)
    ;*** print,'     gmin, gmax = ',gmin, gmax

    ;------------------
    ;Are we done yet ?
    ;------------------
    n_tries = (n_tries + 1L)
    wg = where(gap gt tol, nwg)
    DONE = (nwg eq 0) OR (n_tries gt max_tries)

    ;----------------------------
    ;Write status to message box
    ;----------------------------
    if (keyword_set(MSG_BOX_ID)) then begin
        mstr = 'Pixels left = ' + TF_String(nwg)
        widget_control, MSG_BOX_ID, set_value=mstr
    endif

    ;--------------------------------
    ;Make sure depth is reasonable ?
    ;--------------------------------
    ;dmax = max(d, min=dmin, /NAN)
    ;if (dmax gt 100.0) OR (dmin lt 0) then DONE=1b
    ;print,'dmin, dmax = ',dmin,dmax

endwhile

;------------------------------
;Failure-to-converge message ?
;------------------------------
if (n_tries gt max_tries) then begin
    ;----------------------------
    ;Write status to message box
    ;----------------------------
    if (keyword_set(MSG_BOX_ID)) then begin
        nstr = TF_String(max_tries)
        mstr = 'No convergence after ' + nstr + ' tries'
        widget_control, MSG_BOX_ID, set_value=mstr
    endif
endif

;--------------------------
;Save result to depth_file
;--------------------------
TF_Get_LUN, unit, d0_file
openw, unit, d0_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
writeu, unit, float(d)
free_lun, unit

;----------------------------------
;Compute the initial velocity grid
;using the Manning formula
;----------------------------------
v = (Ac / P)^(2d/3d) * sqrt(S) / n


;-------------------------------
;Save the initial velocity grid
;-------------------------------
TF_Get_LUN, unit2, v0_file
openw, unit2, v0_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
writeu, unit2, float(v)
free_lun, unit2
 
end;  Make_Initial_Depth_Grid
;*****************************************************************
pro GUI_Make_Init_Depth_Grid_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;************
'START' : $
;************
begin
;------------------
;Read the settings
;------------------
Read_Text_Box, state.d0_file_ID, d0_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, d0_file, OK
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.v0_file_ID, v0_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, v0_file, OK
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.RTI_file_ID, RTI_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.width_file_ID, width_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.area_file_ID, area_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.slope_file_ID, slope_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.manning_file_ID, manning_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.bangle_file_ID, bank_angle_file, OK, /TEXT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.recharge_ID, recharge_rate, OK, /FLOAT
if NOT(OK) then RETURN
;--------------------------------------------
;Convert rate from [mm/year] to [m/s] using:
;   1 [mm/yr] = 1/(1000 * 365 * 24 * 3600)
;   1 [mm/yr] = 3.1709792e-011 [m/s]
;--------------------------------------------
factor = 1d / (1000d * 365d * 24d * 3600d)
recharge_rate = recharge_rate * factor  ;[m/s]

;---------------------------------------
;Call routine that creates the RTG file 
;---------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Make_Initial_Depth_Grid, d0_file, v0_file, RTI_file, $
             width_file, area_file, slope_file, manning_file, $
             bank_angle_file, recharge_rate, $
             MSG_BOX_ID=state.msg_box_ID
widget_control, event.ID, sensitive=1    ;(enable button)

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating new depth grid.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'init_depth.htm'

;************
'CLOSE' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CLOSE') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

end;  GUI_Make_Init_Depth_Grid_event
;*****************************************************************
pro GUI_Make_Init_Depth_Grid, leader

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
;--------------------------------------
prefix       = mstate.run_vars.prefix
d0_file      = prefix + '_d0.rtg'
v0_file      = prefix + '_v0.rtg'
RTI_file     = prefix + '.rti'
width_file   = prefix + '_chan-w.rtg'
area_file    = prefix + '_area.rtg'
slope_file   = prefix + '_slope.rtg'
manning_file = prefix + '_chan-n.rtg'
bangle_file  = prefix + '_chan-a.rtg'

;-----------------------------------
;Set default baseflow recharge rate
;Typical rates are 1 to 200 mm/year. 
;1 [mm/yr] = 3.1709792e-011 [m/s]
;-----------------------------------
recharge_str = TF_String(50.0)  ;[mm/year]

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, msg_box_ID:0L, $
d0_file_ID:0L, v0_file_ID:0L, RTI_file_ID:0L, width_file_ID:0L, $
area_file_ID:0L, slope_file_ID:0L, manning_file_ID:0L, $
bangle_file_ID:0L, recharge_ID:0L }

;----------------
;Initialize vars
;----------------
ngap = 6
XS   = 20
XS2  = 10

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Make Initial Depth Grid Dialog ', $
            /COLUMN, LEADER=leader
B1 = widget_base(MB, /COLUMN, /FRAME)
B2 = widget_base(MB, /COLUMN, /FRAME)
B3 = widget_base(MB, /ROW)

;----------------------------
;Get names of new grid files
;----------------------------

DF = widget_base(B1, /ROW, SPACE=ngap)
  DF1 = widget_label(DF, VALUE='New depth grid file: ')
  DF2 = widget_text(DF, VALUE=d0_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.d0_file_ID = DF2
  DF3 = widget_label(DF, VALUE=' [meters] ')
;-----------------------------------------------------------
VF = widget_base(B1, /ROW, SPACE=ngap)
  VF1 = widget_label(VF, VALUE='New velocity grid file: ')
  VF2 = widget_text(VF, VALUE=v0_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.v0_file_ID = VF2
  VF3 = widget_label(VF, VALUE=' [m/s] ')

;------------------------------
;Get names of other grid files
;------------------------------
RF = widget_base(B2, /ROW, SPACE=ngap)
  RF1 = widget_label(RF, VALUE='Name of RTI file: ')
  RF2 = widget_text(RF, VALUE=RTI_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTI_file_ID = RF2
;-----------------------------------------------------------
WF = widget_base(B2, /ROW, SPACE=ngap)
  WF1 = widget_label(WF, VALUE='Bed width grid file: ')
  WF2 = widget_text(WF, VALUE=width_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.width_file_ID = WF2
  WF3 = widget_label(WF, VALUE=' [meters] ')
;-----------------------------------------------------------
AF = widget_base(B2, /ROW, SPACE=ngap)
  AF1 = widget_label(AF, VALUE='Area grid file: ')
  AF2 = widget_text(AF, VALUE=area_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.area_file_ID = AF2
  AF3 = widget_label(AF, VALUE=' [km^2] ')
;-----------------------------------------------------------
SF = widget_base(B2, /ROW, SPACE=ngap)
  SF1 = widget_label(SF, VALUE='Slope grid file: ')
  SF2 = widget_text(SF, VALUE=slope_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.slope_file_ID = SF2
  SF3 = widget_label(SF, VALUE=' [none] ')
;-----------------------------------------------------------
MF = widget_base(B2, /ROW, SPACE=ngap)
  MF1 = widget_label(MF, VALUE='Manning n grid file: ')
  MF2 = widget_text(MF, VALUE=manning_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.manning_file_ID = MF2
  MF3 = widget_label(MF, VALUE=' [s/m^(1/3)] ')
;-----------------------------------------------------------
BA = widget_base(B2, /ROW, SPACE=ngap)
  BA1 = widget_label(BA, VALUE='Bank angle grid file: ')
  BA2 = widget_text(BA, VALUE=bangle_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.bangle_file_ID = BA2
  BA3 = widget_label(BA, VALUE=' [degrees] ')

;-------------------------------
;Get the baseflow recharge rate
;-------------------------------
BF = widget_base(B2, /ROW, SPACE=ngap)
  BF1 = widget_label(BF, VALUE='Baseflow recharge rate: ')
  BF2 = widget_text(BF, VALUE=recharge_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.recharge_ID = BF2
  BF3 = widget_label(BF, VALUE=' [mm/year] ')

;-----------------
;Align text boxes
;-----------------
Align_Text_Boxes, [DF1, RF1, WF1, AF1, SF1, MF1, BA1, VF1, BF1]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, B3, /START, /HELP, /CLOSE

;---------------------
;A status message box
;---------------------
MS = widget_base(B3, /ROW, SPACE=ngap)
  MS1 = widget_label(MS, VALUE='Status: ')
  MS2 = widget_text(MS, VALUE='Ready.', XSIZE=16)
  state.msg_box_ID = MS2

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Make_Init_Depth_Grid', XOFF=480

end;  GUI_Make_Init_Depth_Grid
;*****************************************************************

