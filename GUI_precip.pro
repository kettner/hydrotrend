
;*****************************************************************
;   GUI_precip.pro

;   Copyright (c) 2001-2008, Scott D. Peckham
;   Created:  Dec 2001 - Jan 2002
;   Modified: July 2005, July 2006
;   Modified: March 2008  (Moved "formulas" to bottom)

;*****************************************************************

;---------------------------------------------
;   7/15/05.  The new GUI_Precip GUI contains
;   all the others as special cases.
;---------------------------------------------
;   GUI_Precip_event     ;(New: 7/15/05)
;   GUI_Precip

;-------------------------------------------
;   7/15/05.  These could be obsolete now
;   but are kept for backward compatibility
;   and due to their familiar interface.
;-------------------------------------------
;   GUI_Uniform_Precip_event
;   GUI_Uniform_Precip 

;-------------------------------------------
;   3/14/08.  These should be obsolete now.
;-------------------------------------------
;   GUI_Precip_Formulas_event
;   GUI_Precip_Formulas
;   GUI_Uniform_Precip_Formulas_event
;   GUI_Uniform_Precip_Formulas

;*****************************************************************
pro GUI_Precip_event, event

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
;----------------------------------------------------------
;NB!  Units for input (text box or files) are [mm/hr], but
;     they are converted to [m/s] as soon as they are read
;     and these units are used internally.
;----------------------------------------------------------
Read_Input_Type, state.rate_type, state.rate_ID, rates, $
                 OK, filename=rate_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.duration_type, state.duration_ID, $
                 durations, OK, filename=duration_file
if NOT(OK) then RETURN
;------------------------------------------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
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

;-----------------------------------------------------
;NB!  Rainrates are read with units of [mm/hr], but
;     must be converted here to [m/s] since *pv.rates
;     is used by infiltration routines, etc.
;-----------------------------------------------------

;------------------------
;Upload values to leader
;------------------------
*mstate.precip_vars.rates        = rates / (1000d * 3600d)  ;[m/s]
*mstate.precip_vars.durations    = durations
;------------------------------------------------
mstate.precip_vars.rate_file     = rate_file
mstate.precip_vars.duration_file = duration_file
;------------------------------------------------------
mstate.precip_vars.rate_type     = state.rate_type
mstate.precip_vars.duration_type = state.duration_type
;------------------------------------------------------
*mstate.met_vars.T_air     = T_air
mstate.met_vars.T_air_file = T_air_file           ;(in met_vars)
mstate.met_vars.T_air_type = state.T_air_type
;------------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;********************
;Set the data types
;********************
'RATE_TYPE'     : state.rate_type     = event.index
'DURATION_TYPE' : state.duration_type = event.index
'T_AIR_TYPE'    : state.T_air_type    = event.index

;***********
'HELP' : $
;***********
Show_HTML_Help, 'precip_general.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Precip_event
;*****************************************************************
pro GUI_Precip, leader

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
;----------------------------------------------------------------
rate_str     = Var_Setting(mstate.precip_vars.rate_type,  $
                           mstate.precip_vars.rates, $
                           mstate.precip_vars.rate_file, $
                           FACTOR=3600000d)
duration_str = Var_Setting(mstate.precip_vars.duration_type,  $
                           mstate.precip_vars.durations, $
                           mstate.precip_vars.duration_file)
;----------------------------------------------------------------
T_air_str    = Var_Setting(mstate.met_vars.T_air_type, $
                           mstate.met_vars.T_air, $
                           mstate.met_vars.T_air_file)

;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
rate_ID:0L, rate_type: mstate.precip_vars.rate_type, $
duration_ID: 0L, duration_type: mstate.precip_vars.duration_type,  $
;--------------------------------------------------------------------
T_air_ID:0L, T_air_type: mstate.met_vars.T_air_type }

ngap  = 6
fsize = 20
XS    = 44
types = Model_Input_Types()
dur_types = ['Scalar', 'Time series']
dur_types = Str_Pad(dur_types, 1)

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Variables for Precipitation Method', $
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
RR = widget_base(B, /ROW, SPACE=ngap)
  RR1 = widget_label(RR, VALUE='rate: ', UVALUE='NONE')
  RR2 = widget_droplist(RR, VALUE=types, UVALUE='RATE_TYPE')
  RR3 = widget_text(RR, VALUE=rate_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  RR4 = widget_label(RR, VALUE='[mm/hr]', UVALUE='NONE')
  state.rate_ID = RR3
  widget_control, RR2, set_droplist_select=mstate.precip_vars.rate_type
;--------------------------------------------------------------------------
RD = widget_base(B, /ROW, SPACE=ngap)
  RD1 = widget_label(RD, VALUE='duration: ', UVALUE='NONE')
  RD2 = widget_droplist(RD, VALUE=dur_types, UVALUE='DURATION_TYPE')
  RD3 = widget_text(RD, VALUE=duration_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  RD4 = widget_label(RD, VALUE='[min]', UVALUE='NONE')
  state.duration_ID = RD3
  widget_control, RD2, set_droplist_select=mstate.precip_vars.duration_type
;--------------------------------------------------------------------------
PT = widget_base(B, /ROW, SPACE=ngap)
  PT1 = widget_label(PT, VALUE='T_air: ', UVALUE='NONE')
  PT2 = widget_droplist(PT, VALUE=types, UVALUE='T_AIR_TYPE')
  PT3 = widget_text(PT, VALUE=T_air_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  PT4 = widget_label(PT, VALUE='[deg C]', UVALUE='NONE')
  state.T_air_ID = PT3
  widget_control, PT2, set_droplist_select=mstate.met_vars.T_air_type
;--------------------------------------------------------------------------
BZ = widget_label(B, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, RR1, RD1, PT1]
Align_Text_Boxes, [A12, RR2, RD2, PT2]
Align_Text_Boxes, [A13, RR3, RD3, PT3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Precip', XOFF=480, TW=TW

END;  GUI_Precip
;*****************************************************************
pro GUI_Uniform_Precip_event, event

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
;----------------------------
;Read precip data from table
;(Maybe imported from file.)
;----------------------------
widget_control, state.table_ID, get_value=precip_table
rates     = precip_table[0,*]
durations = precip_table[1,*]
;---------------------------------------
;Remove entries with a duration of zero
;---------------------------------------
wd = where(durations gt 0, nd)
if (nd ne 0) then begin
    rates     = rates[wd]
    durations = durations[wd]
endif

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then begin
    print,'ERROR:  Group leader has died.'
    print,'state.leader_ID = ',TF_String(state.leader_ID)
    RETURN
endif
;----------------------------------------------------
;NB!  Rainrates are read with units of [mm/hr], but
;     must be converted here to [m/s] since *pv.rates
;     is used by infiltration routines, etc.
;-----------------------------------------------------
*mstate.precip_vars.rates     = rates / (1000d * 3600d)  ;[m/s]
*mstate.precip_vars.durations = durations
;-------------------------------------------------
;Next part is needed because user may have used
;another Precip method previously or used File >
;Load Input Vars.  This was added on 3/14/08.
;-------------------------------------------------
mstate.precip_vars.rate_type = 0b
mstate.precip_vars.duration_type = 0b
;--------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate

;--------------
;For debugging
;--------------
;Get_TLB_State, state.leader_ID, nstate, ALIVE
;if NOT(ALIVE) then RETURN
;rates     = *nstate.precip_vars.rates
;durations = *nstate.precip_vars.durations 
;print,'rates     = ',rates
;print,'durations = ',durations

Close_Dialog, event.top 
end

;********************
'CONVERT_UNITS' : $
;********************
begin
widget_control, state.table_ID, get_value=table
rates = table[0,*]
table[0,*] = (rates * 3600d * 1000d)   ;([m/s] -> [mm/hr])
widget_control, state.table_ID, set_value=table
end

;*******************
'IMPORT_TABLE' : $
;*******************
begin
ncols = 2
;--------------------------------------------
;Read precip rates with units of [mm/hr] and
;durations with units of [min] into table
;--------------------------------------------
Import_Table_From_File, state.table_ID, ncols, $
                        FILE='00_Rain_Data.txt'
end

;*****************
'SAVE_TABLE' : $
;*****************
;---------------------------------------------
;Write precip rates with units of [mm/hr] and
;durations with units of [min] from table
;---------------------------------------------
Save_Table_To_File, state.table_ID, FILE='00_Rain_Data.txt'

;******************
'CLEAR_TABLE' : $
;******************
begin
widget_control, state.table_ID, get_value=table
rates     = table[0,*]
durations = table[1,*]
w = where((rates gt 0) OR (durations gt 0), nw)
if (nw eq 0) then RETURN
table[*,0:w[nw-1]] = 0.0
widget_control, state.table_ID, set_value=table
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'precip_uniform.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Uniform_Precip_event
;*****************************************************************
pro GUI_Uniform_Precip, leader

if (n_elements(leader) eq 0) then leader=0L

;------------------------
;Get the button defaults
;------------------------
if (leader eq 0L) then RETURN
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-----------------------------------------------
;1/19/07.  Convert [m/s] to [mm/hr] for display
;-----------------------------------------------
rates     = *mstate.precip_vars.rates
rates     = (rates * 1000d * 3600d)
durations = *mstate.precip_vars.durations

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, table_ID:0L }

ngap = 6
fsize = 20
X1 = 10
XS = 36

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Variables for Uniform Precip Method', $
            /COLUMN, LEADER=leader
B = widget_base(MB, /COLUMN, /FRAME)

;-----------------------
;Second base parameters
;-----------------------
PP1 = widget_label(B, VALUE='Precipitation Rates and Durations by Interval:', $
                   UVALUE='NONE', /ALIGN_LEFT)
PP2 = widget_label(B, VALUE=' ', UVALUE='NONE')

;-------------------
;This doesn't work
;-------------------
;nmax = n_elements(rates)
;list = fltarr(2,nmax)

;----------------------
;Create a table widget
;------------------------------------------
;Must make as many slots as we ever expect
;to need.  See note just above.
;------------------------------------------
nr   = n_elements(rates)
nmax = (9000 > nr)               ;(3000 to 9000, 2/12/07)
list = fltarr(2, nmax)
list[0,0:nr-1] = rates           ;[mm/hr]
list[1,0:nr-1] = durations       ;[min]
;------------------------------------------
rlabels = sindgen(nmax + 1)
rlabels = strtrim(rlabels[1:nmax], 2)
clabels = ['Rate [mm/hr]: ', 'Duration [min]: ']
cwidths = [1.2, 1.2]
WT = widget_table(B, VALUE=list, UVALUE='TABLE', ALIGNMENT=1, $
                  COLUMN_LABELS=clabels, ROW_LABELS=rlabels, $
                  COLUMN_WIDTHS=cwidths, UNITS=1, $
                  /RESIZEABLE_COLUMNS, /EDITABLE, $
                  Y_SCROLL_SIZE=12, FORMAT='(F13.7)')
                  ;*** X_SCROLL_SIZE=3)
state.table_ID = WT
PP3 = widget_label(B, VALUE=' ', UVALUE='NONE')

;----------------------------------------------
;Option to convert rates from [m/s] to [mm/hr]
;----------------------------------------------
CU = widget_base(B, /ROW, SPACE=ngap, /ALIGN_CENTER)
  CU1 = widget_button(CU, VALUE='Convert rates', UVALUE='CONVERT_UNITS')
  CU2 = widget_label(CU, VALUE=' ( [m/s] to [mm/hr] )')

;-----------------------
;Read table from file ?
;-----------------------
IT = widget_base(B, /ROW, SPACE=ngap, /ALIGN_CENTER)
  IT1 = widget_button(IT, VALUE='Read table from file...', $
                      UVALUE='IMPORT_TABLE')
  IT2 = widget_button(IT, VALUE='Save table to file...', UVALUE='SAVE_TABLE')

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL, OTHER='Clear Table'

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Uniform_Precip', XOFF=480

END;  GUI_Uniform_Precip 
;*****************************************************************
;   3/14/08   These seem to be obsolete now.
;*****************************************************************
pro GUI_Precip_Formulas_event, event

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
result = GUI_Message(msg, /INFO, TITLE='Precipitation Help')
;** GUI_Help, msg, state.leader_ID, TITLE='Precipitation Help'
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

END;  GUI_Precip_Formulas_event
;*****************************************************************
pro GUI_Precip_Formulas, leader

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
Create_TLB, MB, TITLE='Formulas for Precipitation Method', $
            /COLUMN, LEADER=leader 
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Show the formulas
;-------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' Q[i] = R[i] * dA '
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F13 = widget_label(F1, VALUE='[m^3 / s]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = ' '
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F23 = widget_label(F2, VALUE='[m]', UVALUE='NONE')
;--------------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP   ;***, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Precip_Formulas', $
             XOFF=XOFF, TW=[F12, F22]

END;  GUI_Precip_Formulas
;*****************************************************************
pro GUI_Uniform_Precip_Formulas_event, event

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
result = GUI_Message(msg, /INFO, TITLE='Uniform Precip Help')
;** GUI_Help, msg, state.leader_ID, TITLE='Uniform Precip Help'
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

END;  GUI_Uniform_Precip_Formulas_event
;*****************************************************************
pro GUI_Uniform_Precip_Formulas, leader

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
Create_TLB, MB, TITLE='Formulas for Uniform Precip Method', $
            /COLUMN, LEADER=leader 
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Show the formulas
;-------------------
F1 = widget_base(A, /ROW, SPACE=ngap)

  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' Q[i] = R[i] * dA '
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F13 = widget_label(F1, VALUE='[m^3 / s]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = ' '
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F23 = widget_label(F2, VALUE='[m]', UVALUE='NONE')
;--------------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP   ;***, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Uniform_Precip_Formulas', $
             XOFF=XOFF, TW=[F12, F22]

END;  GUI_Uniform_Precip_Formulas
;*****************************************************************

