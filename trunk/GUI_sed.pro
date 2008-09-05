
;*****************************************************************
;   GUI_sed.pro

;   Copyright (c) 2002-2005, Scott D. Peckham 
;   Created:  Sept. 2002

;*****************************************************************

;   GUI_Qs_Slope_Area_Formulas_event
;   GUI_Qs_Slope_Area_Formulas
;   GUI_Qs_Slope_Area_event
;   GUI_Qs_Slope_Area

;   -------------------------------------------
;   Idea:  Add a Syvitski formula method, such
;          that Qs = F(H,A,T).
;   -------------------------------------------

;*****************************************************************
pro GUI_Qs_Slope_Area_Formulas_event, event

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
msg = [ $
'Sorry, no help available yet.', ' ']
result = GUI_Message(msg, /INFO, $
             TITLE='Sediment: Slope-Area Formulas Help')
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

END;  GUI_Qs_Slope_Area_Formulas_event
;*****************************************************************
pro GUI_Qs_Slope_Area_Formulas, leader

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
Create_TLB, MB, TITLE='Formulas for Sediment: Slope-Area Method', $
               /COLUMN, LEADER=leader   ;***, /MODAL
A = widget_base(MB, /COLUMN, /FRAME)

;------------------
;Show the formulas
;------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' Qs = E * Slope^p1 * Area^p2 '
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F13 = widget_label(F1, VALUE='[kg/m^3]', UVALUE='NONE')
;---------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = ' '
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F23 = widget_label(F2, VALUE='[unitless]', UVALUE='NONE')
;---------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21]
Align_Text_Boxes, [F12, F22]
Align_Text_Boxes, [F13, F23]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP  ;*** , /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Qs_Slope_Area_Formulas', $
                XOFF=480, TW=[F12, F22]

END;  GUI_Qs_Slope_Area_Formulas
;*****************************************************************
pro GUI_Qs_Slope_Area_event, event

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
Read_Input_Type, state.E_type, state.E_ID, E, $
                 OK, filename=E_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.S_type, state.S_ID, slope, $
                 OK, filename=slope_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.p1_ID, p1, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.A_type, state.A_ID, area, $
                 OK, filename=area_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.p2_ID, p2, OK, /FLOAT
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.sed_dt_ID, sed_dt, OK, /FLOAT
if NOT(OK) then RETURN
RETURN

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-----------------------------------------
*mstate.sed_vars.E         = E 
*mstate.sed_vars.slope     = slope
*mstate.sed_vars.p1        = p1
*mstate.sed_vars.area      = area
*mstate.sed_vars.p2        = p2
;-----------------------------------------
mstate.sed_vars.E_file     = E_file
mstate.sed_vars.slope_file = slope_file
mstate.sed_vars.area_file  = area_file
mstate.sed_vars.dt         = sed_dt
;-----------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;*************
'E_TYPE' : $
;*************
state.E_type = event.index

;*************
'S_TYPE' : $
;*************
state.S_type = event.index


;*************
'A_TYPE' : $
;*************
state.A_type = event.index

;***********
'HELP' : $
;***********
begin
msg = [ $
'Sorry, no help available yet.', ' ']
result = GUI_Message(msg, /INFO, $
             TITLE='Sediment: Slope-Area Variables Help')
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

END;  GUI_Qs_Slope_Area_event
;*****************************************************************
pro GUI_Qs_Slope_Area, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then leader=0L 
;------------------------
;Get the button defaults
;------------------------
if (leader ne 0L) then begin
    Get_TLB_State, leader, mstate
    slope_file = mstate.grid_vars.slope_file
    area_file  = mstate.grid_vars.area_file
endif else RETURN

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, $
E_type:0b, E_ID:0L, $
S_type:1b, S_ID:0L, p1_ID:0L, $
A_type:1b, A_ID:0L, p2_ID:0L, $
sed_dt_ID:0L}

;-----------------------------------
;Get current values from main state
;-----------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-----------------------------------------------
if (mstate.sed_vars.E_file eq '') then begin
    E_str  = TF_String(*mstate.sed_vars.E)
    E_drop = 0b
endif else begin
    E_str  = mstate.sed_vars.E_file
    E_drop = 1b
endelse
;-----------------------------------------------
S_str  = slope_file
S_drop = 1b
;-----------------------------------------------
;if (mstate.sed_vars.slope_file eq '') then begin
;    S_str  = TF_String(*mstate.sed_vars.slope)
;    S_drop = 0b
;endif else begin
;    S_str  = mstate.sed_vars.slope_file
;    S_drop = 1b
;endelse
;--------------------------------------------------
p1_str = TF_String(*mstate.sed_vars.p1)
;--------------------------------------------------
A_str  = area_file
A_drop = 1b
;-----------------------------------------------
;if (mstate.sed_vars.area_file eq '') then begin
;    A_str  = TF_String(*mstate.sed_vars.area)
;    A_drop = 0b
;endif else begin
;    A_str  = mstate.sed_vars.area_file
;    A_drop = 1b
;endelse
;----------------------------------------------------
p2_str = TF_String(*mstate.sed_vars.p2)

ngap  = 6
fsize = 20
XS    = 44
types = Model_Input_Types()

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Variables for Sediment: Slope-Area Method', $
               /COLUMN, LEADER=leader   ;***, /MODAL)
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
E = widget_base(B, /ROW, SPACE=ngap)
  E1 = widget_label(E, VALUE='E: ', UVALUE='NONE')
  E2 = widget_droplist(E, VALUE=types, UVALUE='E_TYPE')
  E3 = widget_text(E, VALUE=E_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  E4 = widget_label(E, VALUE='[???]', UVALUE='NONE')
  state.E_ID = E3
  widget_control, E2, set_droplist_select=E_drop
;--------------------------------------------------------------------------
S = widget_base(B, /ROW, SPACE=ngap)
  S1 = widget_label(S, VALUE='Slope: ', UVALUE='NONE')
  S2 = widget_droplist(S, VALUE=types, UVALUE='S_TYPE')
  S3 = widget_text(S, VALUE=S_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  S4 = widget_label(S, VALUE='[unitless]', UVALUE='NONE')
  state.S_type = 0b
  state.S_ID   = S3
  widget_control, S2, set_droplist_select=S_drop
;-------------------------------------------------------------------------
P1 = widget_base(B, /ROW, SPACE=ngap)
  P11 = widget_label(P1, VALUE='p1: ', UVALUE='NONE')
  P12 = widget_droplist(P1, VALUE=[' Scalar '], UVALUE='NONE')
  P13 = widget_text(P1, VALUE=p1_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  P14 = widget_label(P1, VALUE='[unitless]', UVALUE='NONE')
  state.p1_ID = P13
;--------------------------------------------------------------------------
AG = widget_base(B, /ROW, SPACE=ngap)
  AG1 = widget_label(AG, VALUE='Area: ', UVALUE='NONE')
  AG2 = widget_droplist(AG, VALUE=types, UVALUE='A_TYPE')
  AG3 = widget_text(AG, VALUE=A_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  AG4 = widget_label(AG, VALUE='[km^2]', UVALUE='NONE')
  state.A_type = 0b
  state.A_ID   = AG3
  widget_control, AG2, set_droplist_select=A_drop 
;-------------------------------------------------------------------------
P2 = widget_base(B, /ROW, SPACE=ngap)
  P21 = widget_label(P2, VALUE='p2: ', UVALUE='NONE')
  P22 = widget_droplist(P2, VALUE=[' Scalar '], UVALUE='NONE')
  P23 = widget_text(P2, VALUE=p2_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  P24 = widget_label(P2, VALUE='[unitless]', UVALUE='NONE')
  state.p2_ID = P23
;--------------------------------------------------------------------------
BZ = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='Sediment process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE='1.0', UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[hours/timestep]', UVALUE='NONE')
  state.sed_dt_ID = TI2
;--------------------------------------------------------------------------

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, E1, S1, P11, AG1, P21]
Align_Text_Boxes, [A12, E2, S2, P12, AG2, P22]
Align_Text_Boxes, [A13, E3, S3, P13, AG3, P23]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Qs_Slope_Area', XOFF=480, TW=TW

END;  GUI_Qs_Slope_Area
;*****************************************************************

