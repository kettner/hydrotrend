
;*****************************************************************
;   GUI_chan.pro

;   Copyright (c) 2001-2007, Scott D. Peckham 
;   Created:   Dec 2001 - Jan 2002
;   Modified:  May 2002, July 2005

;*****************************************************************

;   GUI_Kinematic_Wave_Formulas_event,
;   GUI_Kinematic_Wave_Formulas,

;---------------------------------
;   Still need to write these two
;---------------------------------
;   GUI_Dynamic_Wave_Formulas_event,
;   GUI_Dynamic_Wave_Formulas,

;-------------------------------------------------
;   These are used for both Kinematic and Dynamic
;   wave options, but with use of a TITLE keyword
;-------------------------------------------------
;   GUI_Kinematic_Wave_Manning_event   (NEW VERSION: 7/13/05)
;   GUI_Kinematic_Wave_Manning

;   GUI_Kinematic_Wave_z0_event        (NEW: 7/15/05)
;   GUI_Kinematic_Wave_z0

;   GUI_Save_Channel_Vars_event,
;   GUI_Save_Channel_Vars
 
;*****************************************************************
pro GUI_Kinematic_Wave_Formulas_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*********
'OK' : $
;*********
Close_Dialog, event.top 

;***********
'HELP' : $
;***********
Show_HTML_Help, 'channel_kin_wave_eqns.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Kinematic_Wave_Formulas_event
;*****************************************************************
pro GUI_Kinematic_Wave_Formulas, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then leader=0L 

;------------------------------------
;Structure to store selected options
;------------------------------------
state = {leader_ID:leader}
ngap = 6
fsize = 20
types = Model_Input_Types()
XS = 44

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Formulas for Kinematic Wave Method', $
            /COLUMN, LEADER=leader 
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Show the formulas
;-------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' Q = v * A'
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F13 = widget_label(F1, VALUE='[m^3 / s]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = ' v = R^(2/3) * S^(1/2) / n '
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F23 = widget_label(F2, VALUE='[m / s]', UVALUE='NONE')
;--------------------------------------------------------------------------
F3 = widget_base(A, /ROW, SPACE=ngap)
  F31 = widget_label(F3, VALUE='Formula 3:', UVALUE='NONE')
  v3  = ' R = Ac / P'
  F32 = widget_text(F3, VALUE=v3, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F33 = widget_label(F3, VALUE='[m]', UVALUE='NONE')
;--------------------------------------------------------------------------
F4 = widget_base(A, /ROW, SPACE=ngap)
  F41 = widget_label(F4, VALUE='Formula 4:', UVALUE='NONE')
  v4  = ' Ac = d * (w + (d * TAN( theta )))'
  F42 = widget_text(F4, VALUE=v4, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F43 = widget_label(F4, VALUE='[m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F5 = widget_base(A, /ROW, SPACE=ngap)
  F51 = widget_label(F5, VALUE='Formula 5:', UVALUE='NONE')
  v5  = ' P = w + (2 * d / COS( theta[i] )'
  F52 = widget_text(F5, VALUE=v5, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F53 = widget_label(F5, VALUE='[m]', UVALUE='NONE')
;--------------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21, F31, F41, F51]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Kinematic_Wave_Formulas', $
             XOFF=XOFF, TW=[F12, F22, F32, F42, F52]

END;  GUI_Kinematic_Wave_Formulas
;*****************************************************************
pro GUI_Kinematic_Wave_Manning_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

;---------------------------------------
;Only scalar and grid types are allowed
;---------------------------------------
type_codes = [0b, 2b]

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*********
'OK' : $
;*********
begin
;------------------------------------------------------------
Read_Input_Type, state.code_type, state.code_ID, codes, $
                 OK, filename=code_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.slope_type, state.slope_ID, slopes, $
                 OK, filename=slope_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.nval_type, state.nval_ID, nvals, $
                 OK, filename=nval_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.width_type, state.width_ID, widths, $
                 OK, filename=width_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.angle_type, state.angle_ID, angles, $
                 OK, filename=angle_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.sinu_type, state.sinu_ID, sinu, $
                 OK, filename=sinu_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.d0_type, state.d0_ID, d0, $
                 OK, filename=d0_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Text_Box, state.channel_dt_ID, channel_dt, OK, /DOUBLE
if NOT(OK) then RETURN

;-----------------------------------
;Prepare to upload values to leader
;-----------------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then begin
    print,'ERROR:  Group leader has died.'
    print,'state.leader_ID = ',TF_String(state.leader_ID)
    RETURN
endif

;-------------------------------------------
;Check the timestep for numerical stability
;-------------------------------------------
vmax = 5d
dx   = mstate.grid_vars.min_dx
OK   = Courant_Condition_OK(dx, channel_dt, vmax)
if NOT(OK) then begin
    msg = ['WARNING: ', ' ',$
           'The current timestep does not satisfy the Courant',$
           'stability condition based on current grid spacing',$
           '(with vmax = 5) and may make the model numerically ',$
           'unstable.', ' ',$
           'Do you want to use the current timestep anyway? ',$
           ' ',$
           'If not, please enter a smaller timestep. ', ' ']
    answer = GUI_Message(msg, /QUESTION, TITLE='Unstable Timestep ?')
    if (strupcase(answer) eq 'NO') then RETURN
endif

;------------------------
;Upload values to leader
;------------------------
*mstate.channel_vars.codes     = codes
*mstate.channel_vars.slopes    = slopes
*mstate.channel_vars.nvals     = nvals
*mstate.channel_vars.widths    = widths
*mstate.channel_vars.angles    = angles
*mstate.channel_vars.sinu      = sinu
*mstate.channel_vars.d0        = d0
;--------------------------------------------
mstate.channel_vars.code_file  = code_file
mstate.channel_vars.slope_file = slope_file
mstate.channel_vars.nval_file  = nval_file
mstate.channel_vars.width_file = width_file
mstate.channel_vars.angle_file = angle_file
mstate.channel_vars.sinu_file  = sinu_file
mstate.channel_vars.d0_file    = d0_file
;-------------------------------------------------
mstate.channel_vars.code_type  = state.code_type
mstate.channel_vars.slope_type = state.slope_type
mstate.channel_vars.nval_type  = state.nval_type
mstate.channel_vars.width_type = state.width_type
mstate.channel_vars.angle_type = state.angle_type
mstate.channel_vars.sinu_type  = state.sinu_type
mstate.channel_vars.d0_type    = state.d0_type
;-------------------------------------------------
mstate.channel_vars.dt         = channel_dt
;-------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;************************
;Set the variable types
;************************
'CODE_TYPE'  : state.code_type  = type_codes[event.index]
'SLOPE_TYPE' : state.slope_type = type_codes[event.index]
'NVAL_TYPE'  : state.nval_type  = type_codes[event.index]
'WIDTH_TYPE' : state.width_type = type_codes[event.index]
'ANGLE_TYPE' : state.angle_type = type_codes[event.index]
'SINU_TYPE'  : state.sinu_type  = type_codes[event.index]
'D0_TYPE'    : state.d0_type    = type_codes[event.index]

;***********
'HELP' : $
;***********
Show_HTML_Help, 'channel_kin_wave.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Kinematic_Wave_Manning_event
;*****************************************************************
pro GUI_Kinematic_Wave_Manning, leader, TITLE=title

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

FORWARD_FUNCTION Var_Setting

if (n_elements(leader) eq 0) then leader=0L
 
;--------------------------------
;Get leader's current state vars
;--------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;----------------------------------------------------------
code_str  = Var_Setting(mstate.channel_vars.code_type, $
                        mstate.channel_vars.codes, $
                        mstate.channel_vars.code_file)
slope_str = Var_Setting(mstate.channel_vars.slope_type, $
                        mstate.channel_vars.slopes, $
                        mstate.channel_vars.slope_file)
nval_str  = Var_Setting(mstate.channel_vars.nval_type,  $
                        mstate.channel_vars.nvals, $
                        mstate.channel_vars.nval_file)
width_str = Var_Setting(mstate.channel_vars.width_type, $
                        mstate.channel_vars.widths, $
                        mstate.channel_vars.width_file)
angle_str = Var_Setting(mstate.channel_vars.angle_type, $
                        mstate.channel_vars.angles, $
                        mstate.channel_vars.angle_file)
sinu_str  = Var_Setting(mstate.channel_vars.sinu_type, $
                        mstate.channel_vars.sinu, $
                        mstate.channel_vars.sinu_file)
d0_str    = Var_Setting(mstate.channel_vars.d0_type, $
                        mstate.channel_vars.d0, $
                        mstate.channel_vars.d0_file)
tstr = TF_String(mstate.channel_vars.dt)

;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
code_ID:0L,  code_type:  mstate.channel_vars.code_type,  $
slope_ID:0L, slope_type: mstate.channel_vars.slope_type, $
nval_ID: 0L, nval_type:  mstate.channel_vars.nval_type,  $
width_ID:0L, width_type: mstate.channel_vars.width_type, $
angle_ID:0L, angle_type: mstate.channel_vars.angle_type, $
sinu_ID:0L,  sinu_type:  mstate.channel_vars.sinu_type,  $
d0_ID:0L,    d0_type:    mstate.channel_vars.d0_type, $
channel_dt_ID:0L}

ngap  = 6
fsize = 20
XS    = 44
;*** types = Model_Input_Types()
types1 = ['    Grid       ']
types2 = ['    Scalar     ', '    Grid       ']
drops2 = [0b,0b,1b]

if NOT(keyword_set(TITLE)) then $
    title = 'Variables for Kinematic Wave Method'
 
;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE=title, /COLUMN, LEADER=leader
B = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters
;-------------------
A1 = widget_base(B, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ', UVALUE='NONE')
  A12 = widget_label(A1, VALUE='Type: ', UVALUE='NONE')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ', UVALUE='NONE')
  A14 = widget_label(A1, VALUE='Units: ', UVALUE='NONE')
;--------------------------------------------------------------------------
FC = widget_base(B, /ROW, SPACE=ngap)
  FC1 = widget_label(FC, VALUE='flow_codes: ', UVALUE='NONE')
  FC2 = widget_droplist(FC, VALUE=types1, UVALUE='CODE_TYPE')
  FC3 = widget_text(FC, VALUE=code_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  FC4 = widget_label(FC, VALUE='[none]', UVALUE='NONE')
  state.code_ID = FC3
  ;widget_control, FC2, set_droplist_select=mstate.channel_vars.code_type
;--------------------------------------------------------------------------
SL = widget_base(B, /ROW, SPACE=ngap)
  SL1 = widget_label(SL, VALUE='bed_slope: ', UVALUE='NONE')
  SL2 = widget_droplist(SL, VALUE=types1, UVALUE='SLOPE_TYPE')
  SL3 = widget_text(SL, VALUE=slope_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  SL4 = widget_label(SL, VALUE='[m/m]', UVALUE='NONE')
  state.slope_ID = SL3
  ;widget_control, SL2, set_droplist_select=mstate.channel_vars.slope_type
;--------------------------------------------------------------------------
MN = widget_base(B, /ROW, SPACE=ngap)
  MN1 = widget_label(MN, VALUE='Manning_n: ', UVALUE='NONE')
  MN2 = widget_droplist(MN, VALUE=types2, UVALUE='NVAL_TYPE')
  MN3 = widget_text(MN, VALUE=nval_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  MN4 = widget_label(MN, VALUE='[s/m^(1/3)]', UVALUE='NONE')
  state.nval_ID = MN3
  widget_control, MN2, $
         set_droplist_select=drops2(mstate.channel_vars.nval_type)
;--------------------------------------------------------------------------
BW = widget_base(B, /ROW, SPACE=ngap)
  BW1 = widget_label(BW, VALUE='bed_width: ', UVALUE='NONE')
  BW2 = widget_droplist(BW, VALUE=types2, UVALUE='WIDTH_TYPE')
  BW3 = widget_text(BW, VALUE=width_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  BW4 = widget_label(BW, VALUE='[meters]', UVALUE='NONE')
  state.width_ID   = BW3
  widget_control, BW2, $
         set_droplist_select=drops2(mstate.channel_vars.width_type) 
;--------------------------------------------------------------------------
BA = widget_base(B, /ROW, SPACE=ngap)
  BA1 = widget_label(BA, VALUE='bank_angle: ', UVALUE='NONE')
  BA2 = widget_droplist(BA, VALUE=types2, UVALUE='ANGLE_TYPE')
  BA3 = widget_text(BA, VALUE=angle_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  BA4 = widget_label(BA, VALUE='[deg]', UVALUE='NONE')
  state.angle_ID   = BA3

  widget_control, BA2, $
         set_droplist_select=drops2(mstate.channel_vars.angle_type) 
;--------------------------------------------------------------------------
SI = widget_base(B, /ROW, SPACE=ngap)
  SI1 = widget_label(SI, VALUE='sinuosity: ')
  SI2 = widget_droplist(SI, VALUE=types2, UVALUE='SINU_TYPE')
  SI3 = widget_text(SI, VALUE=sinu_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=8)
  SI4 = widget_label(SI, VALUE='[none]')
  state.sinu_ID = SI3
  widget_control, SI2, $
         set_droplist_select=drops2(mstate.channel_vars.sinu_type)
;--------------------------------------------------------------------------
DI = widget_base(B, /ROW, SPACE=ngap)
  DI1 = widget_label(DI, VALUE='init_depth: ', UVALUE='NONE')
  DI2 = widget_droplist(DI, VALUE=types2, UVALUE='D0_TYPE')
  DI3 = widget_text(DI, VALUE=d0_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  DI4 = widget_label(DI, VALUE='[meters]', UVALUE='NONE')
  state.d0_ID = DI3
  widget_control, DI2, $
         set_droplist_select=drops2[mstate.channel_vars.d0_type]
;--------------------------------------------------------------------------
BZ = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='Channel process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[seconds / timestep]', UVALUE='NONE')
  state.channel_dt_ID = TI2

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, FC1, SL1, MN1, BW1, BA1, SI1, DI1]
Align_Text_Boxes, [A12, FC2, SL2, MN2, BW2, BA2, SI2, DI2]
Align_Text_Boxes, [A13, FC3, SL3, MN3, BW3, BA3, SI3, DI3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Kinematic_Wave_Manning', XOFF=480, TW=TW

END;  GUI_Kinematic_Wave_Manning
;*****************************************************************
pro GUI_Kinematic_Wave_z0_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

type_codes = [0b, 2b]

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*********
'OK' : $
;*********
begin
;------------------------------------------------------------
Read_Input_Type, state.code_type, state.code_ID, codes, $
                 OK, filename=code_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.slope_type, state.slope_ID, slopes, $
                 OK, filename=slope_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.z0val_type, state.z0val_ID, z0vals, $
                 OK, filename=z0val_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.width_type, state.width_ID, widths, $
                 OK, filename=width_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.angle_type, state.angle_ID, angles, $
                 OK, filename=angle_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.sinu_type, state.sinu_ID, sinu, $
                 OK, filename=sinu_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.d0_type, state.d0_ID, d0, $
                 OK, filename=d0_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Text_Box, state.channel_dt_ID, channel_dt, OK, /DOUBLE
if NOT(OK) then RETURN

;-----------------------------------
;Prepare to upload values to leader
;-----------------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then begin
    print,'ERROR:  Group leader has died.'
    print,'state.leader_ID = ',TF_String(state.leader_ID)
    RETURN
endif

;-------------------------------------------
;Check the timestep for numerical stability
;-------------------------------------------
vmax = 5d
dx   = mstate.grid_vars.min_dx
OK   = Courant_Condition_OK(dx, channel_dt, vmax)
if NOT(OK) then begin
    msg = ['Sorry, but the current timestep does not ',$
           'satisfy the Courant stability condition ',$
           'based on current grid spacing (with vmax=5) ',$
           'and may make the model numerically unstable. ',$
           ' ','Please enter a smaller timestep. ', ' ']
    result = GUI_Message(msg, /INFO, TITLE='Unstable Timestep')
    RETURN
endif

;------------------------
;Upload values to leader
;------------------------
*mstate.channel_vars.codes     = codes
*mstate.channel_vars.slopes    = slopes
*mstate.channel_vars.z0vals    = z0vals
*mstate.channel_vars.widths    = widths
*mstate.channel_vars.angles    = angles
*mstate.channel_vars.sinu      = sinu
*mstate.channel_vars.d0        = d0
;--------------------------------------------
mstate.channel_vars.code_file = code_file
mstate.channel_vars.slope_file = slope_file
mstate.channel_vars.z0val_file = z0val_file
mstate.channel_vars.width_file = width_file
mstate.channel_vars.angle_file = angle_file
mstate.channel_vars.sinu_file  = sinu_file
mstate.channel_vars.d0_file    = d0_file
;-------------------------------------------------
mstate.channel_vars.code_type  = state.code_type 
mstate.channel_vars.slope_type = state.slope_type
mstate.channel_vars.z0val_type = state.z0val_type
mstate.channel_vars.width_type = state.width_type
mstate.channel_vars.angle_type = state.angle_type
mstate.channel_vars.sinu_type  = state.sinu_type
mstate.channel_vars.d0_type    = state.d0_type
;-------------------------------------------------
mstate.channel_vars.dt         = channel_dt
;-------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;****************
'CODE_TYPE' : $
;****************
state.code_type = type_codes(event.index)

;*****************
'SLOPE_TYPE' : $
;*****************
state.slope_type = type_codes(event.index)

;*****************
'Z0VAL_TYPE' : $
;*****************
state.z0val_type = type_codes(event.index)

;*****************
'WIDTH_TYPE' : $
;*****************
state.width_type = type_codes(event.index)

;*****************
'ANGLE_TYPE' : $
;*****************
state.angle_type = type_codes(event.index)

;****************
'SINU_TYPE' : $
;****************
state.sinu_type = type_codes(event.index)

;**************
'D0_TYPE' : $
;**************
state.d0_type = type_codes(event.index)

;***********
'HELP' : $
;***********
begin
msg = [' ',$
'These are the input variables used for the Kinematic Wave ',$
'method of routing flow in channels. Input variables should',$
'usually be specified as grids, except in special cases.',$
' ',$
'    S  = bed slope [m/m]',$
'    z0 = roughness parameter for law of the wall [meters]', $
'    w  = bed width for trapezoid [m] ', $
'    d0 = initial water depth [m] ', $
'    theta = bank angle for trapezoid [deg] (from vertical)', $
' ',$
'Flow directions are determined by a grid of D8 flow codes.',$
'Flow grids and slope grids can be created by RiverTools or',$
'a similar program and the other grids can be created using ',$
'tools in the Preprocessing section of TopoFlow. ',$
' ',$
'Channel cross-sections are modeled as trapezoids with a bottom',$
'or bed width of w [meters] and a bank angle of theta [degrees].',$
'The cross-section is triangular when w = 0, and is rectangular',$
'when theta = 0. ',$
' ',$
'Each pixel is classified as either a hillslope pixel (overland ',$
'flow) or a channel pixel (channelized flow) and appropriate ',$
'parameters must be used for each.  For overland flow, w >> d,',$
'R --> d, and bank angle drops out.  Overland flow can then be',$
'modeled with a large value of z0.  For channelized flow in ',$
'gravel-bed rivers, z0 is close to D50/30, where D50 is the ',$
'median grain size in meters.',$
' ',$
'Initial water depth in every channel is assumed to be zero ',$
'if an initial depth grid does not exist. ',$
' ', $
'Grids are assumed to be stored as RTG (RiverTools Grid) files', $
'and flow codes are assumed to follow the Jenson (1984) scheme ',$
'that is also used for RiverTools D8 flow grids.',$
' ']
title  = 'Kinematic Wave Variables Help'
GUI_Help, msg, state.leader_ID, TITLE=title
end

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Kinematic_Wave_z0_event
;*****************************************************************
pro GUI_Kinematic_Wave_z0, leader, TITLE=title

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

FORWARD_FUNCTION Var_Setting

if (n_elements(leader) eq 0) then leader=0L 

;-----------------------------------
;Get current values from main state
;-----------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;----------------------------------------------------------
code_str  = Var_Setting(mstate.channel_vars.code_type,  $
                        mstate.channel_vars.codes, $
                        mstate.channel_vars.code_file)
slope_str = Var_Setting(mstate.channel_vars.slope_type,  $
                        mstate.channel_vars.slopes, $
                        mstate.channel_vars.slope_file)
z0val_str = Var_Setting(mstate.channel_vars.z0val_type,  $
                        mstate.channel_vars.z0vals, $
                        mstate.channel_vars.z0val_file)
width_str = Var_Setting(mstate.channel_vars.width_type, $
                        mstate.channel_vars.widths, $
                        mstate.channel_vars.width_file)
angle_str = Var_Setting(mstate.channel_vars.angle_type, $
                        mstate.channel_vars.angles, $
                        mstate.channel_vars.angle_file)
sinu_str  = Var_Setting(mstate.channel_vars.sinu_type, $
                        mstate.channel_vars.sinu, $
                        mstate.channel_vars.sinu_file)
d0_str    = Var_Setting(mstate.channel_vars.d0_type, $
                        mstate.channel_vars.d0, $
                        mstate.channel_vars.d0_file)
;----------------------------------------------------------
tstr = TF_String(mstate.channel_vars.dt)

;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
code_ID: 0L, code_type:  mstate.channel_vars.code_type,  $
slope_ID:0L, slope_type: mstate.channel_vars.slope_type, $
z0val_ID:0L, z0val_type: mstate.channel_vars.z0val_type, $
width_ID:0L, width_type: mstate.channel_vars.width_type, $
angle_ID:0L, angle_type: mstate.channel_vars.angle_type, $
sinu_ID:0L,  sinu_type:  mstate.channel_vars.sinu_type,  $
d0_ID:0L,    d0_type:    mstate.channel_vars.d0_type,    $
channel_dt_ID:0L}

ngap  = 6
fsize = 20
XS    = 44
;*** types = Model_Input_Types()

types1 = ['    Grid       ']
types2 = ['    Scalar     ', '    Grid       ']
drops2 = [0b,0b,1b]

if NOT(keyword_set(TITLE)) then $
    title = 'Variables for Kinematic Wave Method'

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE=title, /COLUMN, LEADER=leader
B = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters
;-------------------
A1 = widget_base(B, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ', UVALUE='NONE')
  A12 = widget_label(A1, VALUE='Type: ', UVALUE='NONE')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ', UVALUE='NONE')
  A14 = widget_label(A1, VALUE='Units: ', UVALUE='NONE')
;--------------------------------------------------------------------------
FC = widget_base(B, /ROW, SPACE=ngap)
  FC1 = widget_label(FC, VALUE='flow_codes: ', UVALUE='NONE')
  FC2 = widget_droplist(FC, VALUE=types1, UVALUE='CODE_TYPE')
  FC3 = widget_text(FC, VALUE=code_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  FC4 = widget_label(FC, VALUE='[none]', UVALUE='NONE')
  state.code_ID = FC3
  ;widget_control, FC2, set_droplist_select=mstate.channel_vars.code_type
;--------------------------------------------------------------------------
SL = widget_base(B, /ROW, SPACE=ngap)
  SL1 = widget_label(SL, VALUE='bed_slope: ', UVALUE='NONE')
  SL2 = widget_droplist(SL, VALUE=types1, UVALUE='SLOPE_TYPE')
  SL3 = widget_text(SL, VALUE=slope_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  SL4 = widget_label(SL, VALUE='[none]', UVALUE='NONE')
  state.slope_ID = SL3
  ;widget_control, SL2, set_droplist_select=mstate.channel_vars.slope_type
;--------------------------------------------------------------------------
MN = widget_base(B, /ROW, SPACE=ngap)
  MN1 = widget_label(MN, VALUE='z0_roughness: ', UVALUE='NONE')
  MN2 = widget_droplist(MN, VALUE=types2, UVALUE='Z0VAL_TYPE')
  MN3 = widget_text(MN, VALUE=z0val_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  MN4 = widget_label(MN, VALUE='[meters]', UVALUE='NONE')
  state.z0val_ID = MN3
  widget_control, MN2, $
         set_droplist_select=drops2(mstate.channel_vars.z0val_type)
;--------------------------------------------------------------------------
BW = widget_base(B, /ROW, SPACE=ngap)
  BW1 = widget_label(BW, VALUE='bed_width: ', UVALUE='NONE')
  BW2 = widget_droplist(BW, VALUE=types2, UVALUE='WIDTH_TYPE')
  BW3 = widget_text(BW, VALUE=width_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  BW4 = widget_label(BW, VALUE='[meters]', UVALUE='NONE')
  state.width_ID   = BW3
  widget_control, BW2, $
         set_droplist_select=drops2(mstate.channel_vars.width_type) 
;--------------------------------------------------------------------------
BA = widget_base(B, /ROW, SPACE=ngap)
  BA1 = widget_label(BA, VALUE='bank_angle: ', UVALUE='NONE')
  BA2 = widget_droplist(BA, VALUE=types2, UVALUE='ANGLE_TYPE')
  BA3 = widget_text(BA, VALUE=angle_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  BA4 = widget_label(BA, VALUE='[deg]', UVALUE='NONE')
  state.angle_ID   = BA3
  widget_control, BA2, $
         set_droplist_select=drops2(mstate.channel_vars.angle_type)
;--------------------------------------------------------------------------
SI = widget_base(B, /ROW, SPACE=ngap)
  SI1 = widget_label(SI, VALUE='sinuosity: ')
  SI2 = widget_droplist(SI, VALUE=types2, UVALUE='SINU_TYPE')
  SI3 = widget_text(SI, VALUE=sinu_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=8)
  SI4 = widget_label(SI, VALUE='[none]')
  state.sinu_ID = SI3
  widget_control, SI2, $
         set_droplist_select=drops2(mstate.channel_vars.sinu_type)
;--------------------------------------------------------------------------
DI = widget_base(B, /ROW, SPACE=ngap)
  DI1 = widget_label(DI, VALUE='init_depth: ', UVALUE='NONE')
  DI2 = widget_droplist(DI, VALUE=types2, UVALUE='D0_TYPE')
  DI3 = widget_text(DI, VALUE=d0_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  DI4 = widget_label(DI, VALUE='[meters]', UVALUE='NONE')
  state.d0_ID = DI3
  widget_control, DI2, $
         set_droplist_select=drops2[mstate.channel_vars.d0_type]
;--------------------------------------------------------------------------
BZ = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='Channel process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=10)

  TI3 = widget_label(TI, VALUE='[seconds / timestep]', UVALUE='NONE')
  state.channel_dt_ID = TI2
;--------------------------------------------------------------------------

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, FC1, SL1, MN1, BW1, BA1, SI1, DI1]
Align_Text_Boxes, [A12, FC2, SL2, MN2, BW2, BA2, SI2, DI2]
Align_Text_Boxes, [A13, FC3, SL3, MN3, BW3, BA3, SI3, DI3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Kinematic_Wave_z0', XOFF=480, TW=TW

END;  GUI_Kinematic_Wave_z0
;*****************************************************************
pro GUI_Save_Channel_Vars_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*********
'OK' : $
;*********
begin
Read_Text_Box, state.save_grid_dt_ID, save_grid_dt, OK, /DOUBLE
if NOT(OK) then RETURN
save_grid_dt = (save_grid_dt * 60d)  ;[min -> sec]
;------------------------------------------------------
Read_Text_Box, state.save_pixels_dt_ID, save_pixels_dt, OK, /DOUBLE
if NOT(OK) then RETURN
save_pixels_dt = (save_pixels_dt * 60d)  ;[min -> sec]
;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
mstate.channel_vars.save_grid_dt   = save_grid_dt
mstate.channel_vars.save_pixels_dt = save_pixels_dt
;-----------------------------------------------------------
mstate.channel_vars.save_Q_grids  = state.save_Q_grids
mstate.channel_vars.save_u_grids  = state.save_u_grids
mstate.channel_vars.save_d_grids  = state.save_d_grids
mstate.channel_vars.save_f_grids  = state.save_f_grids 
;-----------------------------------------------------------
mstate.channel_vars.save_Q_pixels = state.save_Q_pixels
mstate.channel_vars.save_u_pixels = state.save_u_pixels
mstate.channel_vars.save_d_pixels = state.save_d_pixels
mstate.channel_vars.save_f_pixels = state.save_f_pixels 
;-----------------------------
;Collect the output filenames
;-----------------------------
if (state.save_Q_grids) then begin
    Read_Text_Box, state.Q_rts_file_ID, Q_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, Q_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.Q_rts_file = Q_rts_file
endif
;-----------------------------------------------------------------
if (state.save_u_grids) then begin
    Read_Text_Box, state.u_rts_file_ID, u_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, u_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.u_rts_file = u_rts_file

endif
;-----------------------------------------------------------------
if (state.save_d_grids) then begin
    Read_Text_Box, state.d_rts_file_ID, d_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, d_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.d_rts_file = d_rts_file
endif
;-----------------------------------------------------------------
if (state.save_f_grids) then begin
    Read_Text_Box, state.f_rts_file_ID, f_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, f_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.f_rts_file = f_rts_file
endif
;-----------------------------------------------------------------
if (state.save_Q_pixels) then begin
    Read_Text_Box, state.Q_out_file_ID, Q_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, Q_out_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.Q_out_file = Q_out_file
endif
;-----------------------------------------------------------------
if (state.save_u_pixels) then begin
    Read_Text_Box, state.u_out_file_ID, u_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, u_out_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.u_out_file = u_out_file
endif
;-----------------------------------------------------------------
if (state.save_d_pixels) then begin
    Read_Text_Box, state.d_out_file_ID, d_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, d_out_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.d_out_file = d_out_file

endif
;-----------------------------------------------------------------
if (state.save_f_pixels) then begin
    Read_Text_Box, state.f_out_file_ID, f_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, f_out_File, OK
    if NOT(OK) then RETURN
    mstate.channel_vars.f_out_file = f_out_file
endif
;-----------------------------------------------------------------
;2/16/04.  This is now done with a separate wizard panel.
;-----------------------------------------------------------------
;if (state.save_Q_pixels OR state.save_u_pixels OR $
;    state.save_d_pixels OR state.save_f_pixels) then begin
;    Read_Text_Box, state.pixel_file_ID, pixel_file, OK, /FILE
;    if NOT(OK) then RETURN
;    if NOT(File_Found(pixel_file)) then RETURN
;    mstate.channel_vars.pixel_file = pixel_file
;endif
;-----------------------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;*******************
'SAVE_Q_GRIDS' : $
;*******************
begin
state.save_Q_grids = (1b - state.save_Q_grids)
widget_control, state.Q_rts_file_ID, sensitive=state.save_Q_grids
end

;*******************
'SAVE_U_GRIDS' : $
;*******************
begin
state.save_u_grids = (1b - state.save_u_grids)
widget_control, state.u_rts_file_ID, sensitive=state.save_u_grids 
end

;*******************
'SAVE_D_GRIDS' : $
;*******************
begin
state.save_d_grids = (1b - state.save_d_grids)
widget_control, state.d_rts_file_ID, sensitive=state.save_d_grids
end

;*******************
'SAVE_F_GRIDS' : $
;*******************
begin
state.save_f_grids = (1b - state.save_f_grids)
widget_control, state.f_rts_file_ID, sensitive=state.save_f_grids 
end

;********************
'SAVE_Q_PIXELS' : $
;********************
begin
state.save_Q_pixels = (1b - state.save_Q_pixels)
widget_control, state.Q_out_file_ID, sensitive=state.save_Q_pixels 
end

;********************
'SAVE_U_PIXELS' : $
;********************
begin
state.save_u_pixels = (1b - state.save_u_pixels)
widget_control, state.u_out_file_ID, sensitive=state.save_u_pixels 
end

;********************
'SAVE_D_PIXELS' : $
;********************
begin
state.save_d_pixels = (1b - state.save_d_pixels)
widget_control, state.d_out_file_ID, sensitive=state.save_d_pixels 
end

;********************
'SAVE_F_PIXELS' : $
;********************
begin
state.save_f_pixels = (1b - state.save_f_pixels)
widget_control, state.f_out_file_ID, sensitive=state.save_f_pixels 
end 

;***********
'HELP' : $
;***********
begin
msg = [' ',$
'This dialog allows you to specify which computed channel ',$
'variables that you want to save as a grid sequence or time ',$
'series and the sampling timestep. ', ' ']
GUI_Help, msg, state.leader_ID, TITLE='Save Channel Variables Help'
end

;************
'CANCEL' : $
;************
Close_Dialog, event.top
ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Save_Channel_Vars_event
;*****************************************************************
pro GUI_Save_Channel_Vars, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then RETURN

;--------------------------------
;Get leader's current state vars
;--------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;----------------------------------------------------------
gstr = TF_String(mstate.channel_vars.save_grid_dt   / 60d)
pstr = TF_String(mstate.channel_vars.save_pixels_dt / 60d)
;----------------------------------------------------------

save_Q_grids  = mstate.channel_vars.save_Q_grids
save_u_grids  = mstate.channel_vars.save_u_grids
save_d_grids  = mstate.channel_vars.save_d_grids
save_f_grids  = mstate.channel_vars.save_f_grids 
;--------------------------------------------------
save_Q_pixels = mstate.channel_vars.save_Q_pixels
save_u_pixels = mstate.channel_vars.save_u_pixels
save_d_pixels = mstate.channel_vars.save_d_pixels
save_f_pixels = mstate.channel_vars.save_f_pixels 
;--------------------------------------------------
Q_rts_file = mstate.channel_vars.Q_rts_file
u_rts_file = mstate.channel_vars.u_rts_file
d_rts_file = mstate.channel_vars.d_rts_file
f_rts_file = mstate.channel_vars.f_rts_file
;--------------------------------------------
Q_out_file = mstate.channel_vars.Q_out_file
u_out_file = mstate.channel_vars.u_out_file
d_out_file = mstate.channel_vars.d_out_file
f_out_file = mstate.channel_vars.f_out_file

;--------------------
;Used before 7/27/06
;--------------------
;;prefix = mstate.run_vars.run_prefix
;;if (Q_rts_file eq '') then Q_rts_file=(prefix + '_2D-Q.rts')
;;if (u_rts_file eq '') then u_rts_file=(prefix + '_2D-u.rts')
;;if (d_rts_file eq '') then d_rts_file=(prefix + '_2D-d.rts')
;;if (f_rts_file eq '') then f_rts_file=(prefix + '_2D-f.rts')
;;if (Q_out_file eq '') then Q_out_file=(prefix + '_0D-Q.txt')
;;if (u_out_file eq '') then u_out_file=(prefix + '_0D-u.txt')
;;if (d_out_file eq '') then d_out_file=(prefix + '_0D-d.txt')
;;if (f_out_file eq '') then f_out_file=(prefix + '_0D-f.txt')

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, $
save_Q_grids:save_Q_grids, Q_rts_file_ID:0L, $
save_u_grids:save_u_grids, u_rts_file_ID:0L, $
save_d_grids:save_d_grids, d_rts_file_ID:0L, $
save_f_grids:save_f_grids, f_rts_file_ID:0L, $
;----------------------------------------------------
save_Q_pixels:save_Q_pixels, Q_out_file_ID:0L, $
save_u_pixels:save_u_pixels, u_out_file_ID:0L, $
save_d_pixels:save_d_pixels, d_out_file_ID:0L, $
save_f_pixels:save_f_pixels, f_out_file_ID:0L, $
;----------------------------------------------------
save_grid_dt_ID:0L, $
save_pixels_dt_ID:0L }

ngap = 6
XS1 = 20

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Save Channel Variables', $
               /COLUMN, LEADER=leader
A  = widget_base(MB, /COLUMN)

;---------------------
;Option to save grids 
;---------------------
SG = widget_base(A, /COL, /FRAME)
  SG1 = widget_base(SG, /ROW, SPACE=ngap)
    ;---------------------------------------------------------------------
    SG11 = widget_base(SG1, /COL, SPACE=ngap)
      SG111 = widget_label(SG11, VALUE='Grids to save: ', $
                          UVALUE='NONE', /ALIGN_LEFT)
      SG112 = widget_base(SG11, /COL, /NONEXCLUSIVE)
        SG1121 = widget_button(SG112, VALUE='Discharge [m^3/s]',  $
                               UVALUE='SAVE_Q_GRIDS')
        SG1122 = widget_button(SG112, VALUE='Velocity [m/s]',  $
                               UVALUE='SAVE_U_GRIDS')

        SG1123 = widget_button(SG112, VALUE='Flow depth [m]',  $
                               UVALUE='SAVE_D_GRIDS')
        SG1124 = widget_button(SG112, VALUE='Drag coeff. [none]',  $
                               UVALUE='SAVE_F_GRIDS')
        if (save_Q_grids) then widget_control,SG1121,/set_button
        if (save_u_grids) then widget_control,SG1122,/set_button
        if (save_d_grids) then widget_control,SG1123,/set_button
        if (save_f_grids) then widget_control,SG1124,/set_button
    ;---------------------------------------------------------------------
    SG12 = widget_base(SG1, /COL, SPACE=ngap)
      SG121 = widget_label(SG12, VALUE='Output filename  (*.rts): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SG122 = widget_base(SG12, /COL)
        SG1221 = widget_text(SG122, VALUE=Q_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SG1222 = widget_text(SG122, VALUE=u_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SG1223 = widget_text(SG122, VALUE=d_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SG1224 = widget_text(SG122, VALUE=f_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
      widget_control, SG1221, sensitive=save_Q_grids
      widget_control, SG1222, sensitive=save_u_grids
      widget_control, SG1223, sensitive=save_d_grids
      widget_control, SG1224, sensitive=save_f_grids
      state.Q_rts_file_ID = SG1221
      state.u_rts_file_ID = SG1222
      state.d_rts_file_ID = SG1223
      state.f_rts_file_ID = SG1224
  ;---------------------------------------------------------------------
  SG2 = widget_base(SG, /ROW, SPACE=ngap)
    SG21 = widget_label(SG2, VALUE='At this timestep: ', UVALUE='NONE')
    SG22 = widget_text(SG2, VALUE=gstr, UVALUE='NONE', $
                       /EDITABLE, XSIZE=XS1)
    SG23 = widget_label(SG2, VALUE='[min]', UVALUE='NONE')
    state.save_grid_dt_ID = SG22

;----------------------------
;Option to save pixel values
;----------------------------
SV = widget_base(A, /COL, /FRAME)
  SV1 = widget_base(SV, /ROW, SPACE=ngap)
    ;---------------------------------------------------------------------
    SV11 = widget_base(SV1, /COL, SPACE=ngap)
      SV111 = widget_label(SV11, VALUE='Values to save: ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SV112 = widget_base(SV11, /COL, /NONEXCLUSIVE)
        SV1121 = widget_button(SV112, VALUE='Discharge [m^3/s]',  $
                               UVALUE='SAVE_Q_PIXELS')
        SV1122 = widget_button(SV112, VALUE='Velocity [m/s]',  $
                               UVALUE='SAVE_U_PIXELS')
        SV1123 = widget_button(SV112, VALUE='Flow depth [m]',  $
                               UVALUE='SAVE_D_PIXELS')
        SV1124 = widget_button(SV112, VALUE='Drag coeff. [none]',  $
                               UVALUE='SAVE_F_PIXELS')
        if (save_Q_pixels) then widget_control,SV1121,/set_button
        if (save_u_pixels) then widget_control,SV1122,/set_button
        if (save_d_pixels) then widget_control,SV1123,/set_button
        if (save_f_pixels) then widget_control,SV1124,/set_button
    ;---------------------------------------------------------------------
    SV12 = widget_base(SV1, /COL, SPACE=ngap)
      SV121 = widget_label(SV12, VALUE='Output filename  (*.txt): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SV122 = widget_base(SV12, /COL)
        SV1221 = widget_text(SV122, VALUE=Q_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SV1222 = widget_text(SV122, VALUE=u_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SV1223 = widget_text(SV122, VALUE=d_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SV1224 = widget_text(SV122, VALUE=f_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
      widget_control, SV1221, sensitive=save_Q_pixels
      widget_control, SV1222, sensitive=save_u_pixels
      widget_control, SV1223, sensitive=save_d_pixels
      widget_control, SV1224, sensitive=save_f_pixels
      state.Q_out_file_ID = SV1221
      state.u_out_file_ID = SV1222

      state.d_out_file_ID = SV1223
      state.f_out_file_ID = SV1224
  ;---------------------------------------------------------------------
  SV2 = widget_base(SV, /ROW, SPACE=ngap)
    SV21 = widget_label(SV2, VALUE='At this timestep: ', UVALUE='NONE')
    SV22 = widget_text(SV2, VALUE=pstr, UVALUE='NONE', $
                       /EDITABLE, XSIZE=XS1)
    SV23 = widget_label(SV2, VALUE='[min]', UVALUE='NONE')
    state.save_pixels_dt_ID = SV22

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [SV21]
;** Align_Text_Boxes, [SV21, SV31]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Save_Channel_Vars', $
             XOFF=XOFF  ;***, TW=[F12, F22]

END;  GUI_Save_Channel_Vars 
;*****************************************************************


