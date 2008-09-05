
;*****************************************************************
;   GUI_stop.pro

;   Copyright (c) 2001-2007, Scott D. Peckham 
;   Created:   Dec 2001 - Jan 2002
;   Modified:  July 2005

;*****************************************************************

;   GUI_Stop_By_Qpeak_Fraction_event,
;   GUI_Stop_By_Qpeak_Fraction,

;   GUI_Stop_By_Model_Time_event,
;   GUI_Stop_By_Model_Time,

;   GUI_Stop_By_Real_Time_event,
;   GUI_Stop_By_Real_Time,

;   GUI_Stop_By_Steps_event,
;   GUI_Stop_By_Steps

;*****************************************************************
pro GUI_Stop_By_Qpeak_Fraction_event, event

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
Read_Text_Box, state.Qp_fraction_ID, Qp_fraction, OK, /FLOAT
if NOT(OK) then RETURN
Qp_fraction = (Qp_fraction / 100d)  ;(percent to decimal)
;---------------------------------
;Prepare to update leader's state
;---------------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;----------------------------------
;Get new estimate for T_stop_model
;----------------------------------
basin_areas  = *mstate.grid_vars.basin_areas
durations    = *mstate.precip_vars.durations
T_stop_model = T_Stop_Model_Estimate(basin_areas, durations, $
                                     Qp_fraction)
;------------------------
;Upload values to leader
;------------------------
mstate.stop_vars.Qp_fraction  = Qp_fraction
mstate.stop_vars.T_stop_model = T_stop_model
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
begin
msg = ['Sorry, no help available yet.']
result = GUI_Message(msg, /INFO, TITLE='Stop by Value Help')
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

END;  GUI_Stop_By_Qpeak_Fraction_event
;*****************************************************************
pro GUI_Stop_By_Qpeak_Fraction, leader

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
    Qpstr = TF_String(mstate.stop_vars.Qp_fraction * 100.0)
endif else RETURN

;------------------------------------
;Structure to store selected options
;------------------------------------
state = {leader_ID:leader, Qp_fraction_ID:0L}
ngap = 6
XS1 = 6

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Stop by Q-Value Method', $
            /COLUMN, LEADER=leader
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters 
;-------------------
QP = widget_base(A, /ROW)
  QP1 = widget_label(QP, VALUE='Stop when Q drops to: ', UVALUE='NONE')
  QP2 = widget_text(QP, VALUE=qpstr, UVALUE='NONE', /EDITABLE, XSIZE=XS1)
  QP3 = widget_label(QP, VALUE=' % of Qpeak', UVALUE='NONE')
  state.Qp_fraction_ID = QP2
PP1 = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
;Align_Text_Boxes, [F11, F21, F31, F41]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Stop_By_Qpeak_Fraction', $
             XOFF=XOFF  ;***, TW=[F12, F22, F32, F42]

END;  GUI_Stop_By_Qpeak_Fraction 
;*****************************************************************
pro GUI_Stop_By_Model_Time_event, event

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
Read_Text_Box, state.stop_time_ID, stop_time, OK, /DOUBLE
if NOT(OK) then RETURN
;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
mstate.stop_vars.t_stop_model = stop_time   ;[minutes]
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;***********
'HELP' : $
;***********
begin
msg = ['Sorry, no help available yet.']
result = GUI_Message(msg, /INFO, TITLE='Stop by Model Time Help')
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

END;  GUI_Stop_By_Model_Time_event
;*****************************************************************
pro GUI_Stop_By_Model_Time, leader

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
    tstr = TF_String(mstate.stop_vars.T_stop_model)  ;[minutes]
endif else RETURN

;------------------------------------
;Structure to store selected options
;------------------------------------
state = {leader_ID:leader, stop_time_ID:0L}
ngap = 6
XS1  = 8

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Stop by Model Time Method', $
            /COLUMN, LEADER=leader
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters 
;-------------------
MT = widget_base(A, /ROW)
  MT1 = widget_label(MT, VALUE='Stop when model time reaches: ', UVALUE='NONE')
  MT2 = widget_text(MT, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=XS1)
  MT3 = widget_label(MT, VALUE=' [minutes]', UVALUE='NONE')
  state.stop_time_ID = MT2
PP1 = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
;Align_Text_Boxes, [F11, F21, F31, F41]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Stop_By_Model_Time', $
             XOFF=XOFF  ;***, TW=[F12, F22, F32, F42]

END;  GUI_Stop_By_Model_Time 
;*****************************************************************
pro GUI_Stop_By_Real_Time_event, event

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
Read_Text_Box, state.stop_time_ID, stop_time, OK, /DOUBLE
if NOT(OK) then RETURN
;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
mstate.stop_vars.stop_real_time = stop_time   ;[minutes]
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;***********
'HELP' : $
;***********
begin
msg = ['Sorry, no help available yet.']
result = GUI_Message(msg, /INFO, TITLE='Stop by Real Time Help')
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

END;  GUI_Stop_By_Real_Time_event
;*****************************************************************
pro GUI_Stop_By_Real_Time, leader

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
    tstr = TF_String(mstate.stop_vars.T_stop_real)   ;[minutes]
endif else RETURN

;------------------------------------
;Structure to store selected options
;------------------------------------
state = {leader_ID:leader, stop_time_ID:0L}
ngap = 6
XS1 = 6

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Stop by Real Time Method', $
            /COLUMN, LEADER=leader
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters 
;-------------------
RT = widget_base(A, /ROW)
  RT1 = widget_label(RT, VALUE='Stop when real time reaches: ', UVALUE='NONE')
  RT2 = widget_text(RT, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=XS1)
  RT3 = widget_label(RT, VALUE='[hours]', UVALUE='NONE')
  state.stop_time_ID = RT2
PP1 = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
;Align_Text_Boxes, [F11, F21]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Stop_By_Real_Time', $
             XOFF=XOFF  ;***, TW=[F12, F22]

END;  GUI_Stop_By_Real_Time 
;*****************************************************************
pro GUI_Stop_By_Steps_event, event

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
Read_Text_Box, state.n_steps_ID, n_steps, OK, /LONG
if NOT(OK) then RETURN
;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
mstate.stop_vars.n_steps = n_steps
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;***********
'HELP' : $
;***********
begin
msg = ['Sorry, no help available yet.']
result = GUI_Message(msg, /INFO, TITLE='Stop by Steps Help')
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

END;  GUI_Stop_By_Steps_event
;*****************************************************************
pro GUI_Stop_By_Steps, leader

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
    nstr = TF_String(mstate.stop_vars.n_steps)
endif else RETURN

;------------------------------------
;Structure to store selected options
;------------------------------------
state = {leader_ID:leader, n_steps_ID:0L}
ngap = 6
XS1 = 10

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Stop by Steps Method', $
            /COLUMN, LEADER=leader
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Get the parameters 
;-------------------
RT = widget_base(A, /ROW)
  RT1 = widget_label(RT, VALUE='Stop after: ', UVALUE='NONE')
  RT2 = widget_text(RT, VALUE=nstr, UVALUE='NONE', /EDITABLE, XSIZE=XS1)
  RT3 = widget_label(RT, VALUE=' [timesteps]', UVALUE='NONE')
  state.n_steps_ID = RT2
PP1 = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
; Align_Text_Boxes, [F11, F21]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Stop_By_Steps', XOFF=XOFF

END;  GUI_Stop_By_Steps 
;*****************************************************************

