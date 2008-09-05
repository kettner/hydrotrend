
;  Add Penman-Monteith method using Dingman (2002) as ref.
;  Add option to use un-modified Priestley-Taylor formula ?

;*****************************************************************
;   GUI_evap.pro

;   Copyright (c) 2001-2007, Scott D. Peckham 
;   Created:   Dec 2001 - Jan 2002
;   Modified:  Feb 2004, July 2006
;   Modified:  Feb 2007 (e.g. Qnet -> Qn_SW & Qn_LW)
;   Modified:  Mar 2007 (met_vars, separated)

;*****************************************************************

;   GUI_Qet_Priestley_Taylor_Formulas_event,
;   GUI_Qet_Priestley_Taylor_Formulas,
;   GUI_Qet_Priestley_Taylor_event,
;   GUI_Qet_Priestley_Taylor

;   GUI_Qet_Energy_Balance_Formulas_event,
;   GUI_Qet_Energy_Balance_Formulas,
;   GUI_Qet_Energy_Balance_event,
;   GUI_Qet_Energy_Balance

;   GUI_Save_ET_Vars_event,
;   GUI_Save_ET_Vars
 
;*****************************************************************
pro GUI_Qet_Priestley_Taylor_Formulas_event, event

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
begin
msg = [' ',$
'These are the formulas used for the Priestley-Taylor method', $
'of estimating losses due to evaporation. The variables are', $
'defined in the Help for the input variables.', ' ']
GUI_Help, msg, state.leader_ID, TITLE='ET: Priestley-Taylor Formulas Help'
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

END;  GUI_Qet_Priestley_Taylor_Formulas_event
;*****************************************************************
pro GUI_Qet_Priestley_Taylor_Formulas, leader

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
XS = 54

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='ET Formulas: Priestley-Taylor Method', $
            /COLUMN, LEADER=leader   ;***, /MODAL
A = widget_base(MB, /COLUMN, /FRAME)

;------------------
;Show the formulas 
;------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' ET_rate = (1000 * Qet) / (rho_w * Lv) '
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F13 = widget_label(F1, VALUE='[mm/sec]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2 = ' Qet = alpha * (0.406 + (0.011 * T_air)) * (Qn_SW + Qn_LW - Qc) '
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F23 = widget_label(F2, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F3 = widget_base(A, /ROW, SPACE=ngap)
  F31 = widget_label(F3, VALUE='Formula 3:', UVALUE='NONE')
  v3  = ' Qc = Ks * (T_soil_x - T_surf) / (x / 100)'
  F32 = widget_text(F3, VALUE=v3, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F33 = widget_label(F3, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21, F31]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP  ;***, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Qet_Priestley_Taylor_Formulas', $
             XOFF=480, TW=[F12, F22, F32]

END;  GUI_Qet_Priestley_Taylor_Formulas 
;*****************************************************************
pro GUI_Qet_Priestley_Taylor_event, event

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
;-------------------------------------------------------------
Read_Input_Type, state.Qn_SW_type, state.Qn_SW_ID, Qn_SW, $
                 OK, filename=Qn_SW_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.Qn_LW_type, state.Qn_LW_ID, Qn_LW, $
                 OK, filename=Qn_LW_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.T_surf_type, state.T_surf_ID, T_surf, $
                 OK, filename=T_surf_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.T_soil_x_type, state.T_soil_x_ID, T_soil_x, $
                 OK, filename=T_soil_x_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.soil_x_type, state.soil_x_ID, soil_x, $
                 OK, filename=soil_x_file
if NOT(OK) then RETURN
soil_x = (soil_x / 100.0)   ;(cm -> m *********************)
;-------------------------------------------------------------
Read_Input_Type, state.Ks_type, state.Ks_ID, Ks, $
                 OK, filename=Ks_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.alpha_type, state.alpha_ID, alpha, $
                 OK, filename=alpha_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.ET_dt_ID, ET_dt, OK, /DOUBLE
if NOT(OK) then RETURN

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;---------------------------------------------
*mstate.met_vars.Qn_SW       = Qn_SW
*mstate.met_vars.Qn_LW       = Qn_LW
*mstate.met_vars.T_air       = T_air
*mstate.met_vars.T_surf      = T_surf
;-------------------------------------------
mstate.met_vars.Qn_SW_file   = Qn_SW_file
mstate.met_vars.Qn_LW_file   = Qn_LW_file
mstate.met_vars.T_air_file   = T_air_file
mstate.met_vars.T_surf_file  = T_surf_file
;-------------------------------------------------
mstate.met_vars.Qn_SW_type   = state.Qn_SW_type
mstate.met_vars.Qn_LW_type   = state.Qn_LW_type
mstate.met_vars.T_air_type   = state.T_air_type
mstate.met_vars.T_surf_type  = state.T_surf_type
;-------------------------------------------------
;-------------------------------------------------
*mstate.ET_vars.T_soil_x     = T_soil_x
*mstate.ET_vars.soil_x       = soil_x
*mstate.ET_vars.Ks           = Ks
*mstate.ET_vars.alpha        = alpha
;---------------------------------------------
mstate.ET_vars.T_soil_x_file = T_soil_x_file
mstate.ET_vars.soil_x_file   = soil_x_file
mstate.ET_vars.Ks_file       = Ks_file
mstate.ET_vars.alpha_file    = alpha_file
;--------------------------------------------------
mstate.ET_vars.T_soil_x_type = state.T_soil_x_type
mstate.ET_vars.soil_x_type   = state.soil_x_type
mstate.ET_vars.Ks_type       = state.Ks_type
mstate.ET_vars.alpha_type    = state.alpha_type
;--------------------------------------------------
mstate.ET_vars.dt            = ET_dt
;---------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;*****************************
;Data types for ET variables
;*****************************
'QN_SW_TYPE'    : state.Qn_SW_type    = event.index
'QN_LW_TYPE'    : state.Qn_LW_type    = event.index
'T_AIR_TYPE'    : state.T_air_type    = event.index
'T_SURF_TYPE'   : state.T_surf_type   = event.index
'T_SOIL_X_TYPE' : state.T_soil_x_type = event.index
'SOIL_X_TYPE'   : state.soil_x_type   = event.index
'KS_TYPE'       : state.Ks_type       = event.index
'ALPHA_TYPE'    : state.alpha_type    = event.index

;***********
'HELP' : $
;***********
Show_HTML_Help, 'ET_Priestley-Taylor.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Qet_Priestley_Taylor_event
;*****************************************************************
pro GUI_Qet_Priestley_Taylor, leader

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
Qn_SW_str    = Var_Setting(mstate.met_vars.Qn_SW_type, $
                           mstate.met_vars.Qn_SW, $
                           mstate.met_vars.Qn_SW_file)
Qn_LW_str    = Var_Setting(mstate.met_vars.Qn_LW_type, $
                           mstate.met_vars.Qn_LW, $
                           mstate.met_vars.Qn_LW_file)
T_air_str    = Var_Setting(mstate.met_vars.T_air_type, $
                           mstate.met_vars.T_air, $
                           mstate.met_vars.T_air_file)
T_surf_str   = Var_Setting(mstate.met_vars.T_surf_type, $
                           mstate.met_vars.T_surf, $
                           mstate.met_vars.T_surf_file)
;--------------------------------------------------------------
T_soil_x_str = Var_Setting(mstate.ET_vars.T_soil_x_type, $
                           mstate.ET_vars.T_soil_x,$
                           mstate.ET_vars.T_soil_x_file)
soil_x_str   = Var_Setting(mstate.ET_vars.soil_x_type, $
                           mstate.ET_vars.soil_x,$
                           mstate.ET_vars.soil_x_file, FACTOR=100.0)  ;****
Ks_str       = Var_Setting(mstate.ET_vars.Ks_type, $
                           mstate.ET_vars.Ks, $
                           mstate.ET_vars.Ks_file)
alpha_str    = Var_Setting(mstate.ET_vars.alpha_type, $
                           mstate.ET_vars.alpha, $
                           mstate.ET_vars.alpha_file)
tstr = TF_String(mstate.ET_vars.dt)  ;[seconds]


;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
Qn_SW_ID:0L,    Qn_SW_type: mstate.met_vars.Qn_SW_type, $
Qn_LW_ID:0L,    Qn_LW_type: mstate.met_vars.Qn_LW_type, $
T_air_ID:0L,    T_air_type: mstate.met_vars.T_air_type, $
T_surf_ID:0L,   T_surf_type: mstate.met_vars.T_surf_type, $
;---------------------------------------------------------------
T_soil_x_ID:0L, T_soil_x_type: mstate.ET_vars.T_soil_x_type, $
soil_x_ID:0L,   soil_x_type: mstate.ET_vars.soil_x_type, $
Ks_ID:0L,       Ks_type: mstate.ET_vars.Ks_type, $
alpha_ID:0L,    alpha_type: mstate.ET_vars.alpha_type, $
ET_dt_ID:0L}

ngap  = 6
fsize = 20
XS    = 44
types = Model_Input_Types()

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='ET Variables: Priestley-Taylor Method', $
            /COLUMN, LEADER=leader   ;***, /MODAL
B  = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters
;-------------------
A1 = widget_base(B, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ', UVALUE='NONE')
  A12 = widget_label(A1, VALUE='Type: ', UVALUE='NONE')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ', UVALUE='NONE')
  A14 = widget_label(A1, VALUE='Units: ', UVALUE='NONE')
;--------------------------------------------------------------------------
QS = widget_base(B, /ROW, SPACE=ngap)
  QS1 = widget_label(QS, VALUE='Qn_SW: ', UVALUE='NONE')
  QS2 = widget_droplist(QS, VALUE=types, UVALUE='QN_SW_TYPE')
  QS3 = widget_text(QS, VALUE=Qn_SW_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  QS4 = widget_label(QS, VALUE='[W/m^2]', UVALUE='NONE')
  state.Qn_SW_ID = QS3
  widget_control, QS2, set_droplist_select=mstate.met_vars.Qn_SW_type
;--------------------------------------------------------------------------
QL = widget_base(B, /ROW, SPACE=ngap)
  QL1 = widget_label(QL, VALUE='Qn_LW: ', UVALUE='NONE')
  QL2 = widget_droplist(QL, VALUE=types, UVALUE='QN_LW_TYPE')
  QL3 = widget_text(QL, VALUE=Qn_LW_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  QL4 = widget_label(QL, VALUE='[W/m^2]', UVALUE='NONE')
  state.Qn_LW_ID = QL3
  widget_control, QL2, set_droplist_select=mstate.met_vars.Qn_LW_type
;--------------------------------------------------------------------------
TA = widget_base(B, /ROW, SPACE=ngap)
  TA1 = widget_label(TA, VALUE='T_air: ', UVALUE='NONE')
  TA2 = widget_droplist(TA, VALUE=types, UVALUE='T_AIR_TYPE')
  TA3 = widget_text(TA, VALUE=T_air_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  TA4 = widget_label(TA, VALUE='[deg_C]', UVALUE='NONE')
  state.T_air_ID   = TA3
  widget_control, TA2, set_droplist_select=mstate.met_vars.T_air_type
;--------------------------------------------------------------------------
TS = widget_base(B, /ROW, SPACE=ngap)
  TS1 = widget_label(TS, VALUE='T_surf: ', UVALUE='NONE')
  TS2 = widget_droplist(TS, VALUE=types, UVALUE='T_SURF_TYPE')
  TS3 = widget_text(TS, VALUE=T_surf_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TS4 = widget_label(TS, VALUE='[deg_C]', UVALUE='NONE')
  state.T_surf_ID   = TS3
  widget_control, TS2, set_droplist_select=mstate.met_vars.T_surf_type
;--------------------------------------------------------------------------
TX = widget_base(B, /ROW, SPACE=ngap)
  TX1 = widget_label(TX, VALUE='T_soil_x: ', UVALUE='NONE')
  TX2 = widget_droplist(TX, VALUE=types, UVALUE='T_SOIL_X_TYPE')
  TX3 = widget_text(TX, VALUE=T_soil_x_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TX4 = widget_label(TX, VALUE='[deg_C]', UVALUE='NONE')
  state.T_soil_x_ID   = TX3
  widget_control, TX2, set_droplist_select=mstate.ET_vars.T_soil_x_type
;--------------------------------------------------------------------------
XX = widget_base(B, /ROW, SPACE=ngap)
  XX1 = widget_label(XX, VALUE='x: ', UVALUE='NONE')
  XX2 = widget_droplist(XX, VALUE=types, UVALUE='SOIL_X_TYPE')
  XX3 = widget_text(XX, VALUE=soil_x_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  XX4 = widget_label(XX, VALUE='[cm]', UVALUE='NONE')
  state.soil_x_ID = XX3
  ;** widget_control, XX2, set_droplist_select=mstate.ET_vars.soil_x_type
;--------------------------------------------------------------------------
KS = widget_base(B, /ROW, SPACE=ngap)
  KS1 = widget_label(KS, VALUE='K_soil: ', UVALUE='NONE')
  KS2 = widget_droplist(KS, VALUE=types, UVALUE='KS_TYPE')
  KS3 = widget_text(KS, VALUE=Ks_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  KS4 = widget_label(KS, VALUE='[W/m/deg_C]', UVALUE='NONE')
  state.Ks_ID = KS3
  ;** widget_control, KS2, set_droplist_select=mstate.ET_vars.Ks_type
;--------------------------------------------------------------------------
AL = widget_base(B, /ROW, SPACE=ngap)
  AL1 = widget_label(AL, VALUE='alpha: ', UVALUE='NONE')
  AL2 = widget_droplist(AL, VALUE=types, UVALUE='ALPHA_TYPE')
  AL3 = widget_text(AL, VALUE=alpha_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  AL4 = widget_label(AL, VALUE='[unitless]', UVALUE='NONE')
  state.alpha_ID = AL3
;--------------------------------------------------------------------------
BZ = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='ET process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[seconds / timestep]', UVALUE='NONE')
  state.ET_dt_ID = TI2

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, QS1, QL1, TA1, TS1, TX1, XX1, KS1, AL1]
Align_Text_Boxes, [A12, QS2, QL2, TA2, TS2, TX2, XX2, KS2, AL2]
Align_Text_Boxes, [A13, QS3, QL3, TA3, TS3, TX3, XX3, KS3, AL3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Qet_Priestley_Taylor', $
             XOFF=480, TW=TW

END;  GUI_Qet_Priestley_Taylor 
;*****************************************************************
pro GUI_Qet_Energy_Balance_Formulas_event, event

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
begin
msg = [' ',$
'These are the formulas used for the Energy Balance method ',$
'of estimating losses due to evaporation.  The variables are', $
'defined in the Help for the input variables.', ' ']
GUI_Help, msg, state.leader_ID, TITLE='ET: Energy Balance Formulas Help'
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

END;  GUI_Qet_Energy_Balance_Formulas_event
;*****************************************************************
pro GUI_Qet_Energy_Balance_Formulas, leader


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
Create_TLB, MB, TITLE='ET Formulas: Energy Balance Method', $
            /COLUMN, LEADER=leader
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Show the formulas
;-------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' ET_rate = (1000 * Qet) / (rho_w * Lv)'
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS) ;**, /EDITABLE)
  F13 = widget_label(F1, VALUE='[mm/sec]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = ' Qet = Qn_SW + Qn_LW + Qc + Qh'
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F23 = widget_label(F2, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F3 = widget_base(A, /ROW, SPACE=ngap)
  F31 = widget_label(F3, VALUE='Formula 3:', UVALUE='NONE')
  v3  = ' Qc = Ks * (T_soil_x - T_surf) / (x / 100)'
  F32 = widget_text(F3, VALUE=v3, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F33 = widget_label(F3, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F4 = widget_base(A, /ROW, SPACE=ngap)
  F41 = widget_label(F4, VALUE='Formula 4:', UVALUE='NONE')
  v4  = ' Qh = rho_air * Cp_air * Dh * (T_air - T_surf)'
  F42 = widget_text(F4, VALUE=v4, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F43 = widget_label(F4, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F5 = widget_base(A, /ROW, SPACE=ngap)
  F51 = widget_label(F5, VALUE='Formula 5:', UVALUE='NONE')
  v5  = ' Dh = kappa^2 * uz / LN[(z - h_snow) / z0]^2'
  F52 = widget_text(F5, VALUE=v5, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F53 = widget_label(F5, VALUE='[m/s]', UVALUE='NONE')
;--------------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21, F31, F41, F51]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP   ;***, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Qet_Energy_Balance_Formulas', $
             XOFF=XOFF, TW=[F12, F22, F32, F42, F52]

END;  GUI_Qet_Energy_Balance_Formulas 
;*****************************************************************
pro GUI_Qet_Energy_Balance_event, event

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
Read_Input_Type, state.Qn_SW_type, state.Qn_SW_ID, Qn_SW, $
                 OK, filename=Qn_SW_file
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Input_Type, state.Qn_LW_type, state.Qn_LW_ID, Qn_LW, $
                 OK, filename=Qn_LW_file
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Input_Type, state.Ks_type, state.Ks_ID, Ks, $
                 OK, filename=Ks_file
if NOT(OK) then RETURN
;--------------------------------------------------------------
;*** Read_Text_Box, state.Ks_ID, Ks, OK, /FLOAT
;*** if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Input_Type, state.T_surf_type, state.T_surf_ID, T_surf, $
                 OK, filename=T_surf_file
if NOT(OK) then RETURN 
;--------------------------------------------------------------
Read_Input_Type, state.T_soil_x_type, state.T_soil_x_ID, T_soil_x, $
                 OK, filename=T_soil_x_file
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Input_Type, state.soil_x_type, state.soil_x_ID, soil_x, $
                 OK, filename=soil_x_file
if NOT(OK) then RETURN
soil_x = (soil_x / 100.0)   ;(cm -> m ************)
;--------------------------------------------------------------
Read_Input_Type, state.uz_type, state.uz_ID, uz, $
                 OK, filename=uz_file
if NOT(OK) then RETURN 
;--------------------------------------------------------------
Read_Input_Type, state.z_type, state.z_ID, z, $
                 OK, filename=z_file
if NOT(OK) then RETURN 
;-------------------------------------------------------------
Read_Input_Type, state.z0_air_type, state.z0_air_ID, z0_air, $
                 OK, filename=z0_air_file
if NOT(OK) then RETURN 
;-----------------------------------------------------------------
Read_Input_Type, state.h0_snow_type, state.h0_snow_ID, h0_snow, $
                 OK, filename=h0_snow_file
if NOT(OK) then RETURN
;-----------------------------------------------------------------
;Read_Input_Type, state.h0_swe_type, state.h0_swe_ID, h0_swe, $
;                 OK, filename=h0_swe_file
;if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Text_Box, state.rho_air_ID, rho_air, OK, /FLOAT
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Text_Box, state.Cp_air_ID, Cp_air, OK, /FLOAT
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Text_Box, state.ET_dt_ID, ET_dt, OK, /DOUBLE
if NOT(OK) then RETURN

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-----------------------------------------------
;Update met vars
;-----------------
*mstate.met_vars.Qn_SW      = Qn_SW
*mstate.met_vars.Qn_LW      = Qn_LW
*mstate.met_vars.T_air      = T_air
*mstate.met_vars.T_surf     = T_surf
*mstate.met_vars.z0_air     = z0_air
*mstate.met_vars.z          = z
*mstate.met_vars.uz         = uz
*mstate.met_vars.rho_air    = rho_air
*mstate.met_vars.Cp_air     = Cp_air
;-------------------------------------------
mstate.met_vars.Qn_SW_file  = Qn_SW_file
mstate.met_vars.Qn_LW_file  = Qn_LW_file
mstate.met_vars.T_air_file  = T_air_file
mstate.met_vars.T_surf_file = T_surf_file
mstate.met_vars.z0_air_file = z0_air_file
mstate.met_vars.z_file      = z_file         ;(always a scalar?)
mstate.met_vars.uz_file     = uz_file
;-------------------------------------------------
mstate.met_vars.Qn_SW_type  = state.Qn_SW_type
mstate.met_vars.Qn_LW_type  = state.Qn_LW_type
mstate.met_vars.T_air_type  = state.T_air_type
mstate.met_vars.T_surf_type = state.T_surf_type
mstate.met_vars.z0_air_type = state.z0_air_type
mstate.met_vars.z_type      = state.z_type
mstate.met_vars.uz_type     = state.uz_type
;--------------------------------------------------
;Update snow vars
;------------------
*mstate.snow_vars.h0_snow      = h0_snow
;*mstate.snow_vars.h0_swe      = h0_swe
;--------------------------------------------------
mstate.snow_vars.h0_snow_file  = h0_snow_file
;mstate.snow_vars.h0_swe_file  = h0_swe_file
;--------------------------------------------------
mstate.snow_vars.h0_snow_type  = state.h0_snow_type
;mstate.snow_vars.h0_swe_type  = state.h0_swe_type
;-------------------------------------------------
;Update ET vars
;-----------------
*mstate.ET_vars.Ks           = Ks
*mstate.ET_vars.soil_x       = soil_x
*mstate.ET_vars.T_soil_x     = T_soil_x
;-----------------------------------------------
mstate.ET_vars.Ks_file       = Ks_file
mstate.ET_vars.soil_x_file   = soil_x_file
mstate.ET_vars.T_soil_x_file = T_soil_x_file
;--------------------------------------------------
mstate.ET_vars.Ks_type       = state.Ks_type
mstate.ET_vars.soil_x_type   = state.soil_x_type
mstate.ET_vars.T_soil_x_type = state.T_soil_x_type
;--------------------------------------------------
mstate.ET_vars.dt            = ET_dt
;--------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;*********************************
;Set data types for ET variables
;*********************************
'QN_SW_TYPE'    : state.Qn_SW_type    = event.index
'QN_LW_TYPE'    : state.Qn_LW_type    = event.index
'T_AIR_TYPE'    : state.T_air_type    = event.index
'T_SURF_TYPE'   : state.T_surf_type   = event.index
'T_SOIL_X_TYPE' : state.T_soil_x_type = event.index
'SOIL_X_TYPE'   : state.soil_x_type   = event.index
'KS_TYPE'       : state.Ks_type       = event.index
'Z0_AIR_TYPE'   : state.z0_air_type   = event.index
'Z_TYPE'        : state.z_type        = event.index
'UZ_TYPE'       : state.uz_type       = event.index
'H0_SNOW_TYPE'  : state.h0_snow_type  = event.index
'H0_SWE_TYPE'   : state.h0_swe_type   = event.index

;***********
'HELP' : $
;***********
Show_HTML_Help, 'ET_energy_balance.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top


ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Qet_Energy_Balance_event
;*****************************************************************
pro GUI_Qet_Energy_Balance, leader

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
;-----------------------------------------------------------
Qn_SW_str    = Var_Setting(mstate.met_vars.Qn_SW_type, $
                           mstate.met_vars.Qn_SW, $
                           mstate.met_vars.Qn_SW_file)
Qn_LW_str    = Var_Setting(mstate.met_vars.Qn_LW_type, $
                           mstate.met_vars.Qn_LW, $
                           mstate.met_vars.Qn_LW_file)
T_air_str    = Var_Setting(mstate.met_vars.T_air_type, $
                           mstate.met_vars.T_air, $
                           mstate.met_vars.T_air_file)
T_surf_str   = Var_Setting(mstate.met_vars.T_surf_type, $
                           mstate.met_vars.T_surf, $
                           mstate.met_vars.T_surf_file)
z_str        = Var_Setting(mstate.met_vars.z_type, $
                           mstate.met_vars.z, $
                           mstate.met_vars.z_file)
uz_str       = Var_Setting(mstate.met_vars.uz_type, $
                           mstate.met_vars.uz, $
                           mstate.met_vars.uz_file)
z0_air_str   = Var_Setting(mstate.met_vars.z0_air_type, $
                           mstate.met_vars.z0_air, $
                           mstate.met_vars.z0_air_file)
rho_air_str  =  TF_String(*mstate.met_vars.rho_air)
Cp_air_str   =  TF_String(*mstate.met_vars.Cp_air)
;---------------------------------------------------------------------
Ks_str       = Var_Setting(mstate.ET_vars.Ks_type, $
                           mstate.ET_vars.Ks, $
                           mstate.ET_vars.Ks_file)
soil_x_str   = Var_Setting(mstate.ET_vars.soil_x_type, $
                           mstate.ET_vars.soil_x, $
                           mstate.ET_vars.soil_x_file, FACTOR=100.0)
T_soil_x_str = Var_Setting(mstate.ET_vars.T_soil_x_type, $
                           mstate.ET_vars.T_soil_x, $
                           mstate.ET_vars.T_soil_x_file)
;---------------------------------------------------------------------
h0_snow_str  = Var_Setting(mstate.snow_vars.h0_snow_type, $
                           mstate.snow_vars.h0_snow, $
                           mstate.snow_vars.h0_snow_file)
h0_swe_str   = Var_Setting(mstate.snow_vars.h0_swe_type, $
                           mstate.snow_vars.h0_swe, $
                           mstate.snow_vars.h0_swe_file)
tstr = TF_String(mstate.ET_vars.dt)


;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
Qn_SW_ID:0L,    Qn_SW_type:  mstate.met_vars.Qn_SW_type, $
Qn_LW_ID:0L,    Qn_LW_type:  mstate.met_vars.Qn_LW_type, $
T_air_ID:0L,    T_air_type:  mstate.met_vars.T_air_type, $
T_surf_ID:0L,   T_surf_type: mstate.met_vars.T_surf_type, $
uz_ID:0L,       uz_type:     mstate.met_vars.uz_type, $
z0_air_ID:0L,   z0_air_type: mstate.met_vars.z0_air_type, $
z_ID:0L,        z_type:      mstate.met_vars.z_type, $
;--------------------------------------------------------------
Ks_ID:0L,       Ks_type:       mstate.ET_vars.Ks_type, $
T_soil_x_ID:0L, T_soil_x_type: mstate.ET_vars.T_soil_x_type, $
soil_x_ID:0L,   soil_x_type:   mstate.ET_vars.soil_x_type, $
;--------------------------------------------------------------
h0_snow_ID:0L,  h0_snow_type: mstate.snow_vars.h0_snow_type, $
h0_swe_ID:0L,   h0_swe_type:  mstate.snow_vars.h0_swe_type, $
;--------------------------------------------------------------
rho_air_ID:0L, Cp_air_ID:0L, ET_dt_ID:0L }

ngap  = 6
fsize = 20
XS    = 44
types = Model_Input_Types()

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='ET Variables: Energy Balance Method', $
            /COLUMN, LEADER=leader
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
QS = widget_base(B, /ROW, SPACE=ngap)
  QS1 = widget_label(QS, VALUE='Qn_SW: ', UVALUE='NONE')
  QS2 = widget_droplist(QS, VALUE=types, UVALUE='QN_SW_TYPE')
  QS3 = widget_text(QS, VALUE=Qn_SW_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=8)
  QS4 = widget_label(QS, VALUE='[W/m^2]', UVALUE='NONE')
  state.Qn_SW_ID = QS3
  widget_control, QS2, set_droplist_select=mstate.met_vars.Qn_SW_type
;--------------------------------------------------------------------------
QL = widget_base(B, /ROW, SPACE=ngap)
  QL1 = widget_label(QL, VALUE='Qn_LW: ', UVALUE='NONE')
  QL2 = widget_droplist(QL, VALUE=types, UVALUE='QN_LW_TYPE')
  QL3 = widget_text(QL, VALUE=Qn_LW_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=8)
  QL4 = widget_label(QL, VALUE='[W/m^2]', UVALUE='NONE')
  state.Qn_LW_ID = QL3
  widget_control, QL2, set_droplist_select=mstate.met_vars.Qn_LW_type
;--------------------------------------------------------------------------
TA = widget_base(B, /ROW, SPACE=ngap)
  TA1 = widget_label(TA, VALUE='T_air: ', UVALUE='NONE')
  TA2 = widget_droplist(TA, VALUE=types, UVALUE='T_AIR_TYPE')
  TA3 = widget_text(TA, VALUE=T_air_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TA4 = widget_label(TA, VALUE='[deg_C]', UVALUE='NONE')
  state.T_air_ID   = TA3
  widget_control, TA2, set_droplist_select=mstate.met_vars.T_air_type
;--------------------------------------------------------------------------
TS = widget_base(B, /ROW, SPACE=ngap)
  TS1 = widget_label(TS, VALUE='T_surf: ', UVALUE='NONE')
  TS2 = widget_droplist(TS, VALUE=types, UVALUE='T_SURF_TYPE')
  TS3 = widget_text(TS, VALUE=T_surf_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TS4 = widget_label(TS, VALUE='[deg_C]', UVALUE='NONE')
  state.T_surf_ID   = TS3
  widget_control, TS2, set_droplist_select=mstate.met_vars.T_surf_type
;--------------------------------------------------------------------------
TX = widget_base(B, /ROW, SPACE=ngap)
  TX1 = widget_label(TX, VALUE='T_soil_x: ', UVALUE='NONE')
  TX2 = widget_droplist(TX, VALUE=types, UVALUE='T_SOIL_X_TYPE')
  TX3 = widget_text(TX, VALUE=T_soil_x_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TX4 = widget_label(TX, VALUE='[deg_C]', UVALUE='NONE')
  state.T_soil_x_ID   = TX3
  widget_control, TX2, set_droplist_select=mstate.ET_vars.T_soil_x_type
;--------------------------------------------------------------------------
XX = widget_base(B, /ROW, SPACE=ngap)
  XX1 = widget_label(XX, VALUE='x: ', UVALUE='NONE')
  XX2 = widget_droplist(XX, VALUE=types, UVALUE='SOIL_X_TYPE')
  XX3 = widget_text(XX, VALUE=soil_x_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  XX4 = widget_label(XX, VALUE='[cm]', UVALUE='NONE')
  state.soil_x_ID = XX3
  widget_control, XX2, set_droplist_select=mstate.ET_vars.soil_x_type
;--------------------------------------------------------------------------
KS = widget_base(B, /ROW, SPACE=ngap)
  KS1 = widget_label(KS, VALUE='K_soil: ', UVALUE='NONE')
  KS2 = widget_droplist(KS, VALUE=types, UVALUE='KS_TYPE')
  KS3 = widget_text(KS, VALUE=Ks_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  KS4 = widget_label(KS, VALUE='[W/m/deg_C]', UVALUE='NONE')
  state.Ks_ID = KS3
  widget_control, KS2, set_droplist_select=mstate.ET_vars.Ks_type
;--------------------------------------------------------------------------
UZ = widget_base(B, /ROW, SPACE=ngap)
  UZ1 = widget_label(UZ, VALUE='uz: ', UVALUE='NONE')
  UZ2 = widget_droplist(UZ, VALUE=types, UVALUE='UZ_TYPE')
  UZ3 = widget_text(UZ, VALUE=uz_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  UZ4 = widget_label(UZ, VALUE='[m/s]', UVALUE='NONE')
  state.uz_ID = UZ3
  widget_control, UZ2, set_droplist_select=mstate.met_vars.uz_type
;--------------------------------------------------------------------------
ZZ = widget_base(B, /ROW, SPACE=ngap)
  ZZ1 = widget_label(ZZ, VALUE='z: ', UVALUE='NONE')
  ZZ2 = widget_droplist(ZZ, VALUE=types, UVALUE='Z_TYPE')
  ZZ3 = widget_text(ZZ, VALUE=z_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  ZZ4 = widget_label(ZZ, VALUE='[m]', UVALUE='NONE')
  state.z_ID = ZZ3
  widget_control, ZZ2, set_droplist_select=mstate.met_vars.z_type
;--------------------------------------------------------------------------
Z0 = widget_base(B, /ROW, SPACE=ngap)
  Z01 = widget_label(Z0, VALUE='z0_air: ', UVALUE='NONE')
  Z02 = widget_droplist(Z0, VALUE=types, UVALUE='Z0_AIR_TYPE')
  Z03 = widget_text(Z0, VALUE=z0_air_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  Z04 = widget_label(Z0, VALUE='[m]', UVALUE='NONE')
  state.z0_air_ID = Z03
  widget_control, Z02, set_droplist_select=mstate.met_vars.z0_air_type
;--------------------------------------------------------------------------
HS = widget_base(B, /ROW, SPACE=ngap)
  HS1 = widget_label(HS, VALUE='h0_snow: ', UVALUE='NONE')
  HS2 = widget_droplist(HS, VALUE=types, UVALUE='H0_SNOW_TYPE')
  HS3 = widget_text(HS, VALUE=h0_snow_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  HS4 = widget_label(HS, VALUE='[m]', UVALUE='NONE')
  state.h0_snow_ID = HS3
  widget_control, HS2, set_droplist_select=mstate.snow_vars.h0_snow_type
;--------------------------------------------------------------------------
;HW = widget_base(B, /ROW, SPACE=ngap)
;  HW1 = widget_label(HW, VALUE='h0_swe: ', UVALUE='NONE')
;  HW2 = widget_droplist(HW, VALUE=types, UVALUE='H0_SWE_TYPE')
;  HW3 = widget_text(HW, VALUE=h0_swe_str, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=fsize)
;  HW4 = widget_label(HW, VALUE='[m]', UVALUE='NONE')
;  state.h0_swe_ID = HW3
;  widget_control, HW2, set_droplist_select=mstate.snow_vars.h0_swe_type
;--------------------------------------------------------------------------
RA = widget_base(B, /ROW, SPACE=ngap)
  RA1 = widget_label(RA, VALUE='rho_air: ', UVALUE='NONE')
  RA2 = widget_droplist(RA, VALUE=[' Scalar '], UVALUE='NONE')
  RA3 = widget_text(RA, VALUE=rho_air_str, UVALUE='NONE', /EDITABLE, $
                    XSIZE=fsize)
  RA4 = widget_label(RA, VALUE='[kg/m^3]', UVALUE='NONE')
  state.rho_air_ID = RA3
;--------------------------------------------------------------------------
CA = widget_base(B, /ROW, SPACE=ngap)
  CA1 = widget_label(CA, VALUE='Cp_air: ', UVALUE='NONE')
  CA2 = widget_droplist(CA, VALUE=[' Scalar '], UVALUE='NONE')
  CA3 = widget_text(CA, VALUE=Cp_air_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  CA4 = widget_label(CA, VALUE='[J/kg/deg_C]', UVALUE='NONE')
  state.Cp_air_ID = CA3
;--------------------------------------------------------------------------
BA = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='ET process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[seconds / timestep]', UVALUE='NONE')
  state.ET_dt_ID = TI2

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11,QS1,QL1, KS1,TA1,TS1,TX1,XX1, $
                      UZ1,ZZ1,Z01,HS1,RA1,CA1]
Align_Text_Boxes, [A12,QS2,QL2, KS2,TA2,TS2,TX2,XX2, $
                      UZ2,ZZ2,Z02,HS2,RA2,CA2]
Align_Text_Boxes, [A13,QS3,QL3, KS3,TA3,TS3,TX3,XX3, UZ3,ZZ3,Z03,HS3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Qet_Energy_Balance', $
             XOFF=XOFF, TW=TW 

END;  GUI_Qet_Energy_Balance 
;*****************************************************************
pro GUI_Save_ET_Vars_event, event

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
;---------------------------------------------------------------------
Read_Text_Box, state.save_pixels_dt_ID, save_pixels_dt, OK, /DOUBLE
if NOT(OK) then RETURN
save_pixels_dt = (save_pixels_dt * 60d)  ;[min -> sec]
;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-----------------------------------------------------
mstate.ET_vars.save_grid_dt   = save_grid_dt
mstate.ET_vars.save_pixels_dt = save_pixels_dt
;-----------------------------------------------------
;*** mstate.ET_vars.save_hs_grids  = state.save_hs_grids
mstate.ET_vars.save_er_grids  = state.save_er_grids
;-----------------------------------------------------
;** mstate.ET_vars.save_hs_pixels = state.save_hs_pixels
mstate.ET_vars.save_er_pixels = state.save_er_pixels
;-----------------------------
;Collect the output filenames
;-----------------------------
;if (state.save_hs_grids) then begin
;    Read_Text_Box, state.hs_rts_file_ID, hs_rts_file, OK, /FILE
;    if NOT(OK) then RETURN
;    Check_Overwrite, hs_RTS_File, OK
;    if NOT(OK) then RETURN
;    mstate.ET_vars.hs_rts_file = hs_rts_file
;endif
;-------------------------------------------------------------------
if (state.save_er_grids) then begin
    Read_Text_Box, state.er_rts_file_ID, er_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, er_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.ET_vars.er_rts_file = er_rts_file
endif
;-------------------------------------------------------------------
;if (state.save_hs_pixels) then begin
;    Read_Text_Box, state.hs_out_file_ID, hs_out_file, OK, /FILE
;    if NOT(OK) then RETURN
;    Check_Overwrite, hs_out_File, OK
;    if NOT(OK) then RETURN
;    mstate.ET_vars.hs_out_file = hs_out_file
;endif
;-------------------------------------------------------------------
if (state.save_er_pixels) then begin
    Read_Text_Box, state.er_out_file_ID, er_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, er_out_File, OK
    if NOT(OK) then RETURN
    mstate.ET_vars.er_out_file = er_out_file
endif
;-------------------------------------------------------------------
;if (state.save_hs_pixels OR state.save_er_pixels) then begin
;    Read_Text_Box, state.pixel_file_ID, pixel_file, OK, /FILE
;    if NOT(OK) then RETURN
;    mstate.ET_vars.pixel_file = pixel_file
;endif
;-----------------------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;******************
;'SAVE_HS_GRIDS' : $
;******************
;begin
;state.save_hs_grids = (1b - state.save_hs_grids)
;widget_control, state.hs_rts_file_ID, sensitive=state.save_hs_grids
;end

;******************
'SAVE_ER_GRIDS' : $
;******************
begin
state.save_er_grids = (1b - state.save_er_grids)
widget_control, state.er_rts_file_ID, sensitive=state.save_er_grids 
end

;********************
;'SAVE_HS_PIXELS' : $
;********************
;begin
;state.save_hs_pixels = (1b - state.save_hs_pixels)
;widget_control, state.hs_out_file_ID, sensitive=state.save_hs_pixels 
;end

;********************
'SAVE_ER_PIXELS' : $
;********************
begin
state.save_er_pixels = (1b - state.save_er_pixels)
widget_control, state.er_out_file_ID, sensitive=state.save_er_pixels 
end

;***********
'HELP' : $
;***********
begin
msg = [' ',$
'This dialog allows you to specify which computed evapo-transpiration ',$
'variables that you want to save as a grid sequence or time series ',$
'and the sampling timestep. ', ' ']
GUI_Help, msg, state.leader_ID, TITLE='Save ET Variables Help'
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

END;  GUI_Save_ET_Vars_event
;*****************************************************************
pro GUI_Save_ET_Vars, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then RETURN

;------------------------
;Get the dialog defaults
;------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;------------------------------------------------------
gstr = TF_String(mstate.ET_vars.save_grid_dt   / 60d)
pstr = TF_String(mstate.ET_vars.save_pixels_dt / 60d)
;------------------------------------------------------
save_er_grids  = mstate.ET_vars.save_er_grids
save_er_pixels = mstate.ET_vars.save_er_pixels
;save_hs_grids  = mstate.ET_vars.save_hs_grids
;save_hs_pixels = mstate.ET_vars.save_hs_pixels
;-----------------------------------------------
er_rts_file = mstate.ET_vars.er_rts_file
er_out_file = mstate.ET_vars.er_out_file
;hs_rts_file = mstate.ET_vars.hs_rts_file
;hs_out_file = mstate.ET_vars.hs_out_file

;--------------------
;Used before 7/27/06
;--------------------
;;prefix = mstate.run_vars.run_prefix
;;if (hs_rts_file eq '') then hs_rts_file = (prefix + '_2D-hsnow.rts')
;;if (er_rts_file eq '') then er_rts_file = (prefix + '_2D-ETrate.rts')
;;if (hs_out_file eq '') then hs_out_file = (prefix + '_0D-hsnow.txt')
;;if (er_out_file eq '') then er_out_file = (prefix + '_0D-ETrate.txt')

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, $
;save_hs_grids:save_hs_grids, hs_rts_file_ID:0L, $
save_er_grids:save_er_grids, er_rts_file_ID:0L, $
;----------------------------------------------------
;save_hs_pixels:save_hs_pixels, hs_out_file_ID:0L, $
save_er_pixels:save_er_pixels, er_out_file_ID:0L, $
;----------------------------------------------------
save_grid_dt_ID:0L, save_pixels_dt_ID:0L }

ngap = 6
XS1 = 20

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Save ET Variables', $
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
        SG1121 = widget_button(SG112, VALUE='ET rate [mm/day]',  $
                               UVALUE='SAVE_ER_GRIDS')
        if (save_er_grids) then widget_control, SG1121, /set_button
        ;-------------------------------------------------------------
        ;SG1122 = widget_button(SG112, VALUE='Snow Depth [m]',  $
        ;                       UVALUE='SAVE_HS_GRIDS')
        ;if (save_hs_grids) then widget_control, SG1122, /set_button
    ;---------------------------------------------------------------------
    SG12 = widget_base(SG1, /COL, SPACE=ngap)
      SG121 = widget_label(SG12, VALUE='Output filename  (*.rts): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SG122 = widget_base(SG12, /COL)
        SG1221 = widget_text(SG122, VALUE=er_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        widget_control, SG1221, sensitive=save_er_grids
        state.er_rts_file_ID = SG1221
        ;-------------------------------------------------------------
        ;SG1222 = widget_text(SG122, VALUE=hs_rts_file, $
        ;                     UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        ;widget_control, SG1222, sensitive=save_hs_grids
        ;state.hs_rts_file_ID = SG1222
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
        SV1121 = widget_button(SV112, VALUE='ET rate [mm/day]',  $
                               UVALUE='SAVE_ER_PIXELS')
        if (save_er_pixels) then widget_control, SV1121, /set_button
        ;-------------------------------------------------------------
        ;SV1122 = widget_button(SV112, VALUE='Snow Depth [m]',  $
        ;                       UVALUE='SAVE_HS_PIXELS')
        ;if (save_hs_pixels) then widget_control, SV1122, /set_button
    ;---------------------------------------------------------------------
    SV12 = widget_base(SV1, /COL, SPACE=ngap)
      SV121 = widget_label(SV12, VALUE='Output filename  (*.txt): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SV122 = widget_base(SV12, /COL)
        SV1221 = widget_text(SV122, VALUE=er_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        widget_control, SV1221, sensitive=save_er_pixels
        state.er_out_file_ID = SV1221 
        ;----------------------------------------------------------
        ;SV1222 = widget_text(SV122, VALUE=hs_out_file, $
        ;                     UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        ;widget_control, SV1222, sensitive=save_hs_pixels
        ;state.hs_out_file_ID = SV1222
  ;---------------------------------------------------------------------
  SV2 = widget_base(SV, /ROW, SPACE=ngap)
    SV21 = widget_label(SV2, VALUE='At this timestep: ', UVALUE='NONE')
    SV22 = widget_text(SV2, VALUE=pstr, UVALUE='NONE', $
                       /EDITABLE, XSIZE=XS1)
    SV23 = widget_label(SV2, VALUE='[min]', UVALUE='NONE')
    state.save_pixels_dt_ID = SV22
  ;---------------------------------------------------------------------
  ;SV3 = widget_base(SV, /ROW, SPACE=ngap)
  ;  SV31 = widget_label(SV3, VALUE='At these pixel IDs: ', UVALUE='NONE')
  ;  SV32 = widget_text(SV3, VALUE=pixel_file, UVALUE='NONE', $
  ;                     /EDITABLE, XSIZE=XS1)
  ;  state.pixel_file_ID = SV32

;------------------
;Align the widgets
;------------------
;Align_Text_Boxes, [SV21, SV31]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Save_ET_Vars', $
             XOFF=XOFF  ;***, TW=[F12, F22]

END;  GUI_Save_ET_Vars 
;*****************************************************************




