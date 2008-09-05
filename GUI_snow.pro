
;*****************************************************************
;   GUI_snow.pro

;   Copyright (c) 2001-2007, Scott D. Peckham 
;   Created:   Dec 2001 - Jan 2002
;   Modified:  June 2002, Feb 2004, July 2006
;   Modified:  Feb 2007 (e.g. Qnet -> Qn_SW & Qn_LW)
;   Modified:  Mar 2007 (met_vars, separated)
;   Modified:  Mar 2007 (added 4 new vars to save)

;*****************************************************************

;   GUI_Qm_Degree_Day_Formulas_event
;   GUI_Qm_Degree_Day_Formulas
;   GUI_Qm_Degree_Day_event
;   GUI_Qm_Degree_Day

;   GUI_Qm_Energy_Balance_Formulas_event
;   GUI_Qm_Energy_Balance_Formulas 
;   GUI_Qm_Energy_Balance_event
;   GUI_Qm_Energy_Balance

;   GUI_Save_Snow_Vars_event
;   GUI_Save_Snow_Vars

;*****************************************************************
pro GUI_Qm_Degree_Day_Formulas_event, event

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
'These are the equations used for the Degree-Day method ', $
'of estimating runoff due to snowmelt.  The variables are', $
'defined in the Help for the input variables.', ' ']
GUI_Help, msg, state.leader_ID, TITLE='Snowmelt: Degree-Day Formulas Help'
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

END;  GUI_Qm_Degree_Day_Formulas_event
;*****************************************************************
pro GUI_Qm_Degree_Day_Formulas, leader

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
Create_TLB, MB, TITLE='Snowmelt Formulas: Degree-Day Method', $
            /COLUMN, LEADER=leader   ;***, /MODAL
A = widget_base(MB, /COLUMN, /FRAME)

;------------------
;Show the formulas
;------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' M = (c0 / 86400) * (T_air - T0) '
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS) ;***, /EDITABLE)
  F13 = widget_label(F1, VALUE='[mm/sec]', UVALUE='NONE')
;---------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = ' M_max = (h_snow * 1000) * (rho_H2O / rho_snow) / dt '
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS) ;*** /EDITABLE)
  F23 = widget_label(F2, VALUE='[mm/sec]', UVALUE='NONE')
;---------------------------------------------------------------------
F3 = widget_base(A, /ROW, SPACE=ngap)
  F31 = widget_label(F3, VALUE='Formula 3:', UVALUE='NONE')
  v3  = ' dh_snow = M * (rho_H2O / rho_snow) * dt '
  F32 = widget_text(F3, VALUE=v3, UVALUE='NONE', XSIZE=XS) ;*** /EDITABLE)
  F33 = widget_label(F3, VALUE='[m]', UVALUE='NONE')
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
Realize_TLB, MB, state, 'GUI_Qm_Degree_Day_Formulas', $
             XOFF=480, TW=[F12, F22]

END;  GUI_Qm_Degree_Day_Formulas
;*****************************************************************
pro GUI_Qm_Degree_Day_event, event

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
Read_Input_Type, state.c0_type, state.c0_ID, c0, $
                 OK, filename=c0_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.T0_type, state.T0_ID, T0, $
                 OK, filename=T0_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.rho_snow_type, state.rho_snow_ID, $
                 rho_snow, OK, filename=rho_snow_file
if NOT(OK) then RETURN
;------------------------------------------------------------------
Read_Input_Type, state.h0_snow_type, state.h0_snow_ID, h0_snow, $
                 OK, filename=h0_snow_file
if NOT(OK) then RETURN
;------------------------------------------------------------------
Read_Input_Type, state.h0_swe_type, state.h0_swe_ID, h0_swe, $
                 OK, filename=h0_swe_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.snow_dt_ID, snow_dt, OK, /DOUBLE
if NOT(OK) then RETURN

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then begin
    print,'ERROR:  Group leader has died.'
    print,'state.leader_ID = ',TF_String(state.leader_ID)
    RETURN
endif
;-------------------------------------------------
*mstate.met_vars.T_air         = T_air
mstate.met_vars.T_air_file     = T_air_file
mstate.met_vars.T_air_type     = state.T_air_type
;-------------------------------------------------
;-------------------------------------------------
*mstate.snow_vars.c0           = c0
*mstate.snow_vars.T0           = T0
*mstate.snow_vars.rho_snow     = rho_snow
*mstate.snow_vars.h0_snow      = h0_snow
*mstate.snow_vars.h0_swe       = h0_swe
;-------------------------------------------------
mstate.snow_vars.c0_file       = c0_file
mstate.snow_vars.T0_file       = T0_file
mstate.snow_vars.rho_snow_file = rho_snow_file
mstate.snow_vars.h0_snow_file  = h0_snow_file
mstate.snow_vars.h0_swe_file   = h0_swe_file
;----------------------------------------------------
mstate.snow_vars.c0_type       = state.c0_type
mstate.snow_vars.T0_type       = state.T0_type
mstate.snow_vars.rho_snow_type = state.rho_snow_type
mstate.snow_vars.h0_snow_type  = state.h0_snow_type
mstate.snow_vars.h0_swe_type   = state.h0_swe_type
;----------------------------------------------------
mstate.snow_vars.dt          = snow_dt
;-------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;*****************
; VARIABLE TYPES
;*****************
'C0_TYPE'       : state.C0_type = event.index
'T0_TYPE'       : state.T0_type = event.index
'T_AIR_TYPE'    : state.T_air_type = event.index
'RHO_SNOW_TYPE' : state.rho_snow_type = event.index
'H0_SNOW_TYPE'  : state.h0_snow_type = event.index
'H0_SWE_TYPE'   : state.h0_swe_type = event.index

;***********
'HELP' : $
;***********
Show_HTML_Help, 'snowmelt_degree_day.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Qm_Degree_Day_event
;*****************************************************************
pro GUI_Qm_Degree_Day, leader

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
;------------------------------------------------------------------------------
c0_str    = Var_Setting(mstate.snow_vars.c0_type, $
                        mstate.snow_vars.c0, $
                        mstate.snow_vars.c0_file)
T0_str    = Var_Setting(mstate.snow_vars.T0_type, $
                        mstate.snow_vars.T0, $
                        mstate.snow_vars.T0_file)
T_air_str = Var_Setting(mstate.met_vars.T_air_type, $  ;*** met var ***
                        mstate.met_vars.T_air, $
                        mstate.met_vars.T_air_file)
rho_snow_str = Var_Setting(mstate.snow_vars.rho_snow_type, $
                         mstate.snow_vars.rho_snow,$
                         mstate.snow_vars.rho_snow_file)    ;(7/20/06 ****)
h0_snow_str = Var_Setting(mstate.snow_vars.h0_snow_type, $
                         mstate.snow_vars.h0_snow,$
                         mstate.snow_vars.h0_snow_file)
h0_swe_str  = Var_Setting(mstate.snow_vars.h0_swe_type, $
                         mstate.snow_vars.h0_swe,$
                         mstate.snow_vars.h0_swe_file)
tstr = TF_String(mstate.snow_vars.dt)   ;[seconds]

;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
c0_ID:0L,       c0_type: mstate.snow_vars.c0_type, $
T0_ID:0L,       T0_type: mstate.snow_vars.T0_type, $
T_air_ID:0L,    T_air_type: mstate.met_vars.T_air_type, $    ;(met var)
rho_snow_ID:0L, rho_snow_type: mstate.snow_vars.rho_snow_type, $
h0_snow_ID:0L,  h0_snow_type: mstate.snow_vars.h0_snow_type, $
h0_swe_ID:0L,   h0_swe_type: mstate.snow_vars.h0_swe_type, $
snow_dt_ID:0L}

ngap  = 6
fsize = 20
XS    = 44
types = Model_Input_Types()

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Snowmelt Variables: Degree-Day Method', $
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
C0 = widget_base(B, /ROW, SPACE=ngap)
  C01 = widget_label(C0, VALUE='c0: ', UVALUE='NONE')

  C02 = widget_droplist(C0, VALUE=types, UVALUE='C0_TYPE')
  C03 = widget_text(C0, VALUE=c0_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  C04 = widget_label(C0, VALUE='[mm/day/deg_C]', UVALUE='NONE')
  state.c0_ID = C03
  widget_control, C02, set_droplist_select=mstate.snow_vars.c0_type
;--------------------------------------------------------------------------
T0 = widget_base(B, /ROW, SPACE=ngap)
  T01 = widget_label(T0, VALUE='T0: ', UVALUE='NONE')
  T02 = widget_droplist(T0, VALUE=types, UVALUE='T0_TYPE')
  T03 = widget_text(T0, VALUE=T0_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  T04 = widget_label(T0, VALUE='[deg_C]', UVALUE='NONE')
  state.T0_ID   = T03
  widget_control, T02, set_droplist_select=mstate.snow_vars.T0_type 
;--------------------------------------------------------------------------
TA = widget_base(B, /ROW, SPACE=ngap)
  TA1 = widget_label(TA, VALUE='T_air: ', UVALUE='NONE')
  TA2 = widget_droplist(TA, VALUE=types, UVALUE='T_AIR_TYPE')
  TA3 = widget_text(TA, VALUE=T_air_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  TA4 = widget_label(TA, VALUE='[deg_C]', UVALUE='NONE')
  state.T_air_ID   = TA3
  widget_control, TA2, set_droplist_select=mstate.met_vars.T_air_type 
;-------------------------------------------------------------------------
RS = widget_base(B, /ROW, SPACE=ngap)
  RS1 = widget_label(RS, VALUE='rho_snow: ', UVALUE='NONE')
  RS2 = widget_droplist(RS, VALUE=types, UVALUE='RHO_SNOW_TYPE')
  RS3 = widget_text(RS, VALUE=rho_snow_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  RS4 = widget_label(RS, VALUE='[kg/m^3]', UVALUE='NONE')
  state.rho_snow_ID = RS3
;--------------------------------------------------------------------------
HS = widget_base(B, /ROW, SPACE=ngap)
  HS1 = widget_label(HS, VALUE='h0_snow: ', UVALUE='NONE')
  HS2 = widget_droplist(HS, VALUE=types, UVALUE='H0_SNOW_TYPE')
  HS3 = widget_text(HS, VALUE=h0_snow_str, /EDITABLE, XSIZE=fsize)
  HS4 = widget_label(HS, VALUE='[m]', UVALUE='NONE')
  state.h0_snow_ID = HS3
  widget_control, HS2, set_droplist_select=mstate.snow_vars.h0_snow_type
;--------------------------------------------------------------------------
HE = widget_base(B, /ROW, SPACE=ngap)
  HE1 = widget_label(HE, VALUE='h0_swe: ', UVALUE='NONE')
  HE2 = widget_droplist(HE, VALUE=types, UVALUE='H0_SWE_TYPE')
  HE3 = widget_text(HE, VALUE=h0_swe_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  HE4 = widget_label(HE, VALUE='[m]', UVALUE='NONE')
  state.h0_swe_ID = HE3
  widget_control, HE2, set_droplist_select=mstate.snow_vars.h0_swe_type 
;--------------------------------------------------------------------------
BZ = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='Snowmelt process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[seconds / timestep]', UVALUE='NONE')
  state.snow_dt_ID = TI2
;--------------------------------------------------------------------------

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, C01, T01, TA1, RS1, HS1, HE1]
Align_Text_Boxes, [A12, C02, T02, TA2, RS2, HS2, HE2]
Align_Text_Boxes, [A13, C03, T03, TA3, RS3, HS3, HE3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Qm_Degree_Day', XOFF=480, TW=TW

END;  GUI_Qm_Degree_Day
;*****************************************************************
pro GUI_Qm_Energy_Balance_Formulas_event, event

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
'These are the equations used for the Energy Balance method', $
'of estimating runoff due to snowmelt. The variables are', $
'defined in the Help for the input variables.',  ' ']
GUI_Help, msg, state.leader_ID, $
          TITLE='Snowmelt: Energy Balance Formulas Help'
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

END;  GUI_Qm_Energy_Balance_Formulas_event
;*****************************************************************
pro GUI_Qm_Energy_Balance_Formulas, leader

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
Create_TLB, MB, TITLE='Snowmelt Formulas: Energy Balance Method', $
            /COLUMN, LEADER=leader 
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Show the formulas
;-------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' M = (1000 * Qm) / (rho_w * Lf)'
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS) ;**, /EDITABLE)
  F13 = widget_label(F1, VALUE='[mm/sec]', UVALUE='NONE')
;--------------------------------------------------------------------------
F7 = widget_base(A, /ROW, SPACE=ngap)
  F71 = widget_label(F7, VALUE='Formula 2:', UVALUE='NONE')
  v7  = ' M_max = (h_snow * 1000) * (rho_H2O / rho_snow) / dt '
  F72 = widget_text(F7, VALUE=v7, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F73 = widget_label(F7, VALUE='[mm/sec]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 3:', UVALUE='NONE')
  ;*** v2  = ' Qm = Qn_SW + Qn_LW + Qh + Qe + Qa + Qc - Qcc'
  v2  = ' Qm = Qn_SW + Qn_LW + Qh + Qe - Qcc'
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F23 = widget_label(F2, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F3 = widget_base(A, /ROW, SPACE=ngap)
  F31 = widget_label(F3, VALUE='Formula 4:', UVALUE='NONE')
  v3  = ' Qh = rho_a * Cp_a * Dh * (T_air - T_surf)'
  F32 = widget_text(F3, VALUE=v3, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F33 = widget_label(F3, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F4 = widget_base(A, /ROW, SPACE=ngap)
  F41 = widget_label(F4, VALUE='Formula 5:', UVALUE='NONE')
  v4  = ' Qe = rho_a * Lv * De * (0.662 / p0) * (e_air - e_surf)'  
  F42 = widget_text(F4, VALUE=v4, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F43 = widget_label(F4, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
F5 = widget_base(A, /ROW, SPACE=ngap)
  F51 = widget_label(F5, VALUE='Formula 6:', UVALUE='NONE')
  v5  = ' Dh = De = kappa^2 * uz / LN[(z - h) / z0_air]^2'
  F52 = widget_text(F5, VALUE=v5, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F53 = widget_label(F5, VALUE='[m/s]', UVALUE='NONE')
;--------------------------------------------------------------------------
F6 = widget_base(A, /ROW, SPACE=ngap)
  F61 = widget_label(F6, VALUE='Formula 7:', UVALUE='NONE')
  v6  = ' e_air = RH * 6.11 * exp((17.3 * T_air) / (T_air + 237.3))'
  F62 = widget_text(F6, VALUE=v6, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F63 = widget_label(F6, VALUE='[mbar]', UVALUE='NONE')
;--------------------------------------------------------------------------
F8 = widget_base(A, /ROW, SPACE=ngap)
  F81 = widget_label(F8, VALUE='Formula 8:', UVALUE='NONE')
  v8  = ' e_surf = 6.11 * exp((17.3 * T_surf) / (T_surf + 237.3))'
  F82 = widget_text(F8, VALUE=v8, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F83 = widget_label(F8, VALUE='[mbar]', UVALUE='NONE')
;--------------------------------------------------------------------------
F9 = widget_base(A, /ROW, SPACE=ngap)
  F91 = widget_label(F9, VALUE='Formula 9:', UVALUE='NONE')
  v9  = ' Qcc[0] = h0_s * rho_s * Cp_s * (T0 - T_s)'
  F92 = widget_text(F9, VALUE=v9, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  F93 = widget_label(F9, VALUE='[W/m^2]', UVALUE='NONE')
;--------------------------------------------------------------------------
FA = widget_base(A, /ROW, SPACE=ngap)
  FA1 = widget_label(FA, VALUE='Formula 10:', UVALUE='NONE')
  vA  = ' dh_snow = M * (rho_H2O / rho_snow) * dt '
  FA2 = widget_text(FA, VALUE=vA, UVALUE='NONE', XSIZE=XS) ;** /EDITABLE)
  FA3 = widget_label(FA, VALUE='[m]', UVALUE='NONE')
;---------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21, F31, F41, F51, F61, F71, F81, F91, FA1]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP   ;***, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Qm_Energy_Balance_Formulas', $
             XOFF=XOFF, TW=[F12, F22, F32, F42, F52, F62]

END;  GUI_Qm_Energy_Balance_Formulas
;*****************************************************************
pro GUI_Qm_Energy_Balance_event, event

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
;---------------------------------------------------------------
Read_Input_Type, state.Qn_LW_type, state.Qn_LW_ID, Qn_LW, $
                 OK, filename=Qn_LW_file
if NOT(OK) then RETURN
;---------------------------------------------------------------
Read_Input_Type, state.T_air_type, state.T_air_ID, T_air, $
                 OK, filename=T_air_file
if NOT(OK) then RETURN
;---------------------------------------------------------------
Read_Input_Type, state.T_surf_type, state.T_surf_ID, T_surf, $
                 OK, filename=T_surf_file
if NOT(OK) then RETURN 
;---------------------------------------------------------------
Read_Input_Type, state.RH_type, state.RH_ID, RH, $
                 OK, filename=RH_file
if NOT(OK) then RETURN
;---------------------------------------------------------------
Read_Input_Type, state.p0_type, state.p0_ID, p0, $
                 OK, filename=p0_file
if NOT(OK) then RETURN 
;---------------------------------------------------------------
Read_Input_Type, state.uz_type, state.uz_ID, uz, $
                 OK, filename=uz_file
if NOT(OK) then RETURN 
;---------------------------------------------------------------
Read_Text_Box, state.z_ID, z, OK, /DOUBLE
if NOT(OK) then RETURN
z_file = ''      ;************ Always scalar now *************
;--------------------------------------------------------------
Read_Text_Box, state.z0_air_ID, z0_air, OK, /DOUBLE
if NOT(OK) then RETURN
z0_air_file = '' ;************ Always scalar now *************
;-----------------------------------------------------------------
Read_Input_Type, state.h0_snow_type, state.h0_snow_ID, h0_snow, $
                 OK, filename=h0_snow_file
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Input_Type, state.h0_swe_type, state.h0_swe_ID, h0_swe, $
                 OK, filename=h0_swe_file
if NOT(OK) then RETURN 
;--------------------------------------------------------------
Read_Input_Type, state.rho_snow_type, state.rho_snow_ID, $
                 rho_snow, OK, filename=rho_snow_file
if NOT(OK) then RETURN 
;--------------------------------------------------------------
Read_Text_Box, state.Cp_snow_ID, Cp_snow, OK, /DOUBLE
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Text_Box, state.rho_air_ID, rho_air, OK, /DOUBLE
if NOT(OK) then RETURN
;--------------------------------------------------------------
Read_Text_Box, state.Cp_air_ID, Cp_air, OK, /DOUBLE
if NOT(OK) then RETURN 
;--------------------------------------------------------------
Read_Text_Box, state.snow_dt_ID, snow_dt, OK, /DOUBLE
if NOT(OK) then RETURN

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;------------------------------------------
*mstate.met_vars.Qn_SW       = Qn_SW
*mstate.met_vars.Qn_LW       = Qn_LW
*mstate.met_vars.T_air       = T_air
*mstate.met_vars.T_surf      = T_surf
*mstate.met_vars.RH          = RH
*mstate.met_vars.p0          = p0
*mstate.met_vars.uz          = uz
*mstate.met_vars.z           = z
*mstate.met_vars.z0_air      = z0_air
*mstate.met_vars.rho_air     = rho_air
*mstate.met_vars.Cp_air      = Cp_air
;-------------------------------------------
mstate.met_vars.Qn_SW_file    = Qn_SW_file
mstate.met_vars.Qn_LW_file    = Qn_LW_file
mstate.met_vars.T_air_file    = T_air_file
mstate.met_vars.T_surf_file   = T_surf_file 
mstate.met_vars.RH_file       = RH_file
mstate.met_vars.p0_file       = p0_file
mstate.met_vars.uz_file       = uz_file
mstate.met_vars.z_file        = z_file         ;***
mstate.met_vars.z0_air_file   = z0_air_file    ;***
;-------------------------------------------------
mstate.met_vars.Qn_SW_type   = state.Qn_SW_type
mstate.met_vars.Qn_LW_type   = state.Qn_LW_type
mstate.met_vars.T_air_type   = state.T_air_type
mstate.met_vars.T_surf_type  = state.T_surf_type
mstate.met_vars.RH_type      = state.RH_type
mstate.met_vars.p0_type      = state.p0_type
mstate.met_vars.uz_type      = state.uz_type
mstate.met_vars.z_type       = state.z_type
mstate.met_vars.z0_air_type  = state.z0_air_type
;-------------------------------------------------
;-------------------------------------------------
*mstate.snow_vars.h0_snow      = h0_snow
*mstate.snow_vars.h0_swe       = h0_swe
*mstate.snow_vars.rho_snow     = rho_snow
*mstate.snow_vars.Cp_snow      = Cp_snow
;-------------------------------------------------
mstate.snow_vars.h0_snow_file  = h0_snow_file
mstate.snow_vars.h0_swe_file   = h0_swe_file
mstate.snow_vars.rho_snow_file = rho_snow_file
mstate.snow_vars.dt            = snow_dt
;----------------------------------------------------
mstate.snow_vars.h0_snow_type = state.h0_snow_type
mstate.snow_vars.h0_swe_type  = state.h0_swe_type
mstate.snow_vars.rho_snow_type= state.rho_snow_type
;----------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;*****************
; VARIABLE TYPES
;*****************
'QN_SW_TYPE'    : state.Qn_SW_type = event.index
'QN_LW_TYPE'    : state.Qn_LW_type = event.index
'T_AIR_TYPE'    : state.T_air_type = event.index
'T_SURF_TYPE'   : state.T_surf_type = event.index
'RH_TYPE'       : state.RH_type = event.index
'P0_TYPE'       : state.p0_type = event.index
'UZ_TYPE'       : state.uz_type = event.index
'Z_TYPE'        : state.z_type = event.index        ;(always scalar now)
'Z0_AIR_TYPE'   : state.z0_air_type = event.index   ;(always scalar now)
;--------------------------------------------------
'H0_SNOW_TYPE'  : state.h0_snow_type = event.index
'H0_SWE_TYPE'   : state.h0_swe_type = event.index
'RHO_SNOW_TYPE' : state.rho_snow_type = event.index    ;(7/20/06)

;***********
'HELP' : $
;***********
Show_HTML_Help, 'snowmelt_energy_balance.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Qm_Energy_Balance_event
;*****************************************************************
pro GUI_Qm_Energy_Balance, leader

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
;---------------------------------------------------
Qn_SW_str  = Var_Setting(mstate.met_vars.Qn_SW_type, mstate.met_vars.Qn_SW, $
                         mstate.met_vars.Qn_SW_file)
Qn_LW_str  = Var_Setting(mstate.met_vars.Qn_LW_type, mstate.met_vars.Qn_LW, $
                         mstate.met_vars.Qn_LW_file)
T_air_str  = Var_Setting(mstate.met_vars.T_air_type, mstate.met_vars.T_air, $
                         mstate.met_vars.T_air_file)
T_surf_str = Var_Setting(mstate.met_vars.T_surf_type, mstate.met_vars.T_surf,$
                         mstate.met_vars.T_surf_file)
RH_str     = Var_Setting(mstate.met_vars.RH_type, mstate.met_vars.RH, $
                         mstate.met_vars.RH_file)
p0_str     = Var_Setting(mstate.met_vars.p0_type, mstate.met_vars.p0, $
                         mstate.met_vars.p0_file)
uz_str     = Var_Setting(mstate.met_vars.uz_type, mstate.met_vars.uz, $
                         mstate.met_vars.uz_file)
z0_air_str = Var_Setting(mstate.met_vars.z0_air_type, mstate.met_vars.z0_air,$
                         mstate.met_vars.z0_air_file)
;------------------------------------------------
z_str       = TF_String(*mstate.met_vars.z)            ;(met vars)
rho_air_str = TF_String(*mstate.met_vars.rho_air)
Cp_air_str  = TF_String(*mstate.met_vars.Cp_air)
;-------------------------------------------------------------------------------
h0_snow_str = Var_Setting(mstate.snow_vars.h0_snow_type, $
                          mstate.snow_vars.h0_snow,      $
                          mstate.snow_vars.h0_snow_file)
h0_swe_str  = Var_Setting(mstate.snow_vars.h0_swe_type, $
                          mstate.snow_vars.h0_swe,      $
                          mstate.snow_vars.h0_swe_file)
rho_snow_str = Var_Setting(mstate.snow_vars.rho_snow_type, $   ;(7/20/06)
                           mstate.snow_vars.rho_snow,      $
                           mstate.snow_vars.rho_snow_file)
;----------------------------------------------------------
Cp_snow_str  = TF_String(*mstate.snow_vars.Cp_snow)
tstr         = TF_String(mstate.snow_vars.dt)   ;[seconds]

;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, $
Qn_SW_ID:0L,    Qn_SW_type: mstate.met_vars.Qn_SW_type, $
Qn_LW_ID:0L,    Qn_LW_type: mstate.met_vars.Qn_LW_type, $
T_air_ID:0L,    T_air_type: mstate.met_vars.T_air_type, $
T_surf_ID:0L,   T_surf_type: mstate.met_vars.T_surf_type, $
RH_ID:0L,       RH_type: mstate.met_vars.RH_type, $
p0_ID:0L,       p0_type: mstate.met_vars.p0_type, $
uz_ID:0L,       uz_type: mstate.met_vars.uz_type, $
z_ID:0L,        z_type: mstate.met_vars.z_type, $
z0_air_ID:0L,   z0_air_type: mstate.met_vars.z0_air_type, $
rho_air_ID:0L,  Cp_air_ID:0L, $
;----------------------------------------------------------------
h0_snow_ID:0L,  h0_snow_type: mstate.snow_vars.h0_snow_type,   $
h0_swe_ID:0L,   h0_swe_type:  mstate.snow_vars.h0_swe_type,    $
rho_snow_ID:0L, rho_snow_type: mstate.snow_vars.rho_snow_type, $
Cp_snow_ID:0L,  $
;-----------------------------------------------------------------
snow_dt_ID:0L}

ngap  = 6
fsize = 20
types = Model_Input_Types()
XS    = 44

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Snowmelt Variables: Energy Balance Method', $
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
  QS1 = widget_label(QS, VALUE='Qnet_SW: ', UVALUE='NONE')
  QS2 = widget_droplist(QS, VALUE=types, UVALUE='QN_SW_TYPE')
  QS3 = widget_text(QS, VALUE=Qn_SW_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=8)
  QS4 = widget_label(QS, VALUE='[W/m^2]', UVALUE='NONE')
  state.Qn_SW_ID = QS3
  widget_control, QS2, set_droplist_select=mstate.met_vars.Qn_SW_type
;--------------------------------------------------------------------------
QL = widget_base(B, /ROW, SPACE=ngap)
  QL1 = widget_label(QL, VALUE='Qnet_LW: ', UVALUE='NONE')
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
RH = widget_base(B, /ROW, SPACE=ngap)
  RH1 = widget_label(RH, VALUE='RH: ', UVALUE='NONE')
  RH2 = widget_droplist(RH, VALUE=types, UVALUE='RH_TYPE')
  RH3 = widget_text(RH, VALUE=RH_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  RH4 = widget_label(RH, VALUE='[none]  in [0,1]', UVALUE='NONE')
  state.RH_ID   = RH3
  widget_control, RH2, set_droplist_select=mstate.met_vars.RH_type
;--------------------------------------------------------------------------
P0 = widget_base(B, /ROW, SPACE=ngap)
  P01 = widget_label(P0, VALUE='p0: ', UVALUE='NONE')
  P02 = widget_droplist(P0, VALUE=types, UVALUE='P0_TYPE')
  P03 = widget_text(P0, VALUE=p0_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  P04 = widget_label(P0, VALUE='[mbar]', UVALUE='NONE')
  state.p0_ID = P03
  widget_control, P02, set_droplist_select=mstate.met_vars.p0_type
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
  ZZ2 = widget_droplist(ZZ, VALUE=[' Scalar '], UVALUE='Z_TYPE')
  ZZ3 = widget_text(ZZ, VALUE=z_str, UVALUE='NONE', /EDITABLE, XSIZE=fsize)
  ZZ4 = widget_label(ZZ, VALUE='[m]', UVALUE='NONE')
  state.z_ID = ZZ3
  ;** widget_control, ZZ2, set_droplist_select=mstate.met_vars.z_type
;--------------------------------------------------------------------------
Z0 = widget_base(B, /ROW, SPACE=ngap)
  Z01 = widget_label(Z0, VALUE='z0_air: ', UVALUE='NONE')
  Z02 = widget_droplist(Z0, VALUE=[' Scalar '], UVALUE='Z0_AIR_TYPE')
  Z03 = widget_text(Z0, VALUE=z0_air_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  Z04 = widget_label(Z0, VALUE='[m]', UVALUE='NONE')
  state.z0_air_ID = Z03
  widget_control, Z02, set_droplist_select=mstate.met_vars.z0_air_type
;--------------------------------------------------------------------------
HS = widget_base(B, /ROW, SPACE=ngap)
  HS1 = widget_label(HS, VALUE='h0_snow: ')
  HS2 = widget_droplist(HS, VALUE=types, UVALUE='H0_SNOW_TYPE')
  HS3 = widget_text(HS, VALUE=h0_snow_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  HS4 = widget_label(HS, VALUE='[m]', UVALUE='NONE')
  state.h0_snow_ID = HS3
  widget_control, HS2, set_droplist_select=mstate.snow_vars.h0_snow_type
;--------------------------------------------------------------------------
HE = widget_base(B, /ROW, SPACE=ngap)
  HE1 = widget_label(HE, VALUE='h0_swe: ')
  HE2 = widget_droplist(HE, VALUE=types, UVALUE='H0_SWE_TYPE')
  HE3 = widget_text(HE, VALUE=h0_swe_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  HE4 = widget_label(HE, VALUE='[m]', UVALUE='NONE')
  state.h0_swe_ID = HE3
  widget_control, HE2, set_droplist_select=mstate.snow_vars.h0_swe_type
;--------------------------------------------------------------------------
RS = widget_base(B, /ROW, SPACE=ngap)
  RS1 = widget_label(RS, VALUE='rho_snow: ', UVALUE='NONE')
  RS2 = widget_droplist(RS, VALUE=types, UVALUE='RHO_SNOW_TYPE')
  RS3 = widget_text(RS, VALUE=rho_snow_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  RS4 = widget_label(RS, VALUE='[kg/m^3]', UVALUE='NONE')
  state.rho_snow_ID = RS3
;--------------------------------------------------------------------------
CS = widget_base(B, /ROW, SPACE=ngap)
  CS1 = widget_label(CS, VALUE='Cp_snow: ', UVALUE='NONE')
  CS2 = widget_droplist(CS, VALUE=[' Scalar '], UVALUE='CP_SNOW')
  CS3 = widget_text(CS, VALUE=Cp_snow_str, UVALUE='NONE', /EDITABLE,  $
                    XSIZE=fsize)
  CS4 = widget_label(CS, VALUE='[J/(kg deg_C)]', UVALUE='NONE')
  state.Cp_snow_ID = CS3
;--------------------------------------------------------------------------
RA = widget_base(B, /ROW, SPACE=ngap)
  RA1 = widget_label(RA, VALUE='rho_air: ', UVALUE='NONE')
  RA2 = widget_droplist(RA, VALUE=[' Scalar '], UVALUE='RHO_AIR')
  RA3 = widget_text(RA, VALUE=rho_air_str, UVALUE='NONE', /EDITABLE, $
                    XSIZE=fsize)
  RA4 = widget_label(RA, VALUE='[kg/m^3]', UVALUE='NONE')
  state.rho_air_ID = RA3
;--------------------------------------------------------------------------
CA = widget_base(B, /ROW, SPACE=ngap)
  CA1 = widget_label(CA, VALUE='Cp_air: ', UVALUE='NONE')
  CA2 = widget_droplist(CA, VALUE=[' Scalar '], UVALUE='CP_AIR')
  CA3 = widget_text(CA, VALUE=Cp_air_str, UVALUE='NONE', /EDITABLE, $
                    XSIZE=fsize)
  CA4 = widget_label(CA, VALUE='[J/(kg deg_C)]', UVALUE='NONE')
  state.Cp_air_ID = CA3
;--------------------------------------------------------------------------
BA = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='Snowmelt process timestep: ', UVALUE='NONE')
  TI2 = widget_text(TI, VALUE=tstr, UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[seconds / timestep]', UVALUE='NONE')
  state.snow_dt_ID = TI2
;--------------------------------------------------------------------------

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11,QS1,QL1, TA1,TS1,RH1,P01, $
                      UZ1,ZZ1,Z01,HS1,HE1, RS1,CS1,RA1,CA1]
Align_Text_Boxes, [A12,QS2,QL2, TA2,TS2,RH2,P02, $
                      UZ2,ZZ2,Z02,HS2,HE2, RS2,CS2,RA2,CA2]
Align_Text_Boxes, [A13,QS3,QL3, TA3,TS3,RH3,P03, UZ3,ZZ3,Z03,HS3]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Qm_Energy_Balance', XOFF=XOFF, TW=TW

END;  GUI_Qm_Energy_Balance 
;*****************************************************************
pro GUI_Save_Snow_Vars_event, event

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
mstate.snow_vars.save_grid_dt   = save_grid_dt
mstate.snow_vars.save_pixels_dt = save_pixels_dt
;-------------------------------------------------------
mstate.snow_vars.save_mr_grids  = state.save_mr_grids
mstate.snow_vars.save_hs_grids  = state.save_hs_grids
mstate.snow_vars.save_sw_grids  = state.save_sw_grids
mstate.snow_vars.save_cc_grids  = state.save_cc_grids
mstate.snow_vars.save_ea_grids  = state.save_ea_grids
mstate.snow_vars.save_es_grids  = state.save_es_grids
;-------------------------------------------------------
mstate.snow_vars.save_mr_pixels = state.save_mr_pixels
mstate.snow_vars.save_hs_pixels = state.save_hs_pixels
mstate.snow_vars.save_sw_pixels = state.save_sw_pixels
mstate.snow_vars.save_cc_pixels = state.save_cc_pixels
mstate.snow_vars.save_ea_pixels = state.save_ea_pixels
mstate.snow_vars.save_es_pixels = state.save_es_pixels
;-------------------------------------------
ENERGY_BAL = (mstate.snow_vars.method eq 2)

;---------------------------------
;Collect the RTS output filenames
;---------------------------------
if (state.save_mr_grids) then begin
    Read_Text_Box, state.mr_rts_file_ID, mr_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, mr_rts_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.mr_rts_file = mr_rts_file
endif
;-----------------------------------------------------------------
if (state.save_hs_grids) then begin
    Read_Text_Box, state.hs_rts_file_ID, hs_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, hs_rts_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.hs_rts_file = hs_rts_file
endif
;-----------------------------------------------------------------
if (state.save_sw_grids) then begin
    Read_Text_Box, state.sw_rts_file_ID, sw_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, sw_rts_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.sw_rts_file = sw_rts_file
endif
;-----------------------------------------------------------------
if (ENERGY_BAL) then begin   ;***********
if (state.save_cc_grids) then begin
    Read_Text_Box, state.cc_rts_file_ID, cc_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, cc_rts_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.cc_rts_file = cc_rts_file
endif
;-----------------------------------------------------------------
if (state.save_ea_grids) then begin
    Read_Text_Box, state.ea_rts_file_ID, ea_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, ea_rts_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.ea_rts_file = ea_rts_file
endif
;-----------------------------------------------------------------
if (state.save_es_grids) then begin
    Read_Text_Box, state.es_rts_file_ID, es_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, es_rts_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.es_rts_file = es_rts_file
endif
endif

;-------------------------------------
;Collect the "pixel" output filenames
;-------------------------------------
if (state.save_mr_pixels) then begin
    Read_Text_Box, state.mr_out_file_ID, mr_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, mr_out_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.mr_out_file = mr_out_file
endif
;-----------------------------------------------------------------
if (state.save_hs_pixels) then begin
    Read_Text_Box, state.hs_out_file_ID, hs_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, hs_out_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.hs_out_file = hs_out_file
endif
;-----------------------------------------------------------------
if (state.save_sw_pixels) then begin
    Read_Text_Box, state.sw_out_file_ID, sw_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, sw_out_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.sw_out_file = sw_out_file
endif
;-----------------------------------------------------------------
if (ENERGY_BAL) then begin   ;***********************
if (state.save_cc_pixels) then begin
    Read_Text_Box, state.cc_out_file_ID, cc_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, cc_out_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.cc_out_file = cc_out_file
endif
;-----------------------------------------------------------------
if (state.save_ea_pixels) then begin
    Read_Text_Box, state.ea_out_file_ID, ea_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, ea_out_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.ea_out_file = ea_out_file
endif
;-----------------------------------------------------------------
if (state.save_es_pixels) then begin
    Read_Text_Box, state.es_out_file_ID, es_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, es_out_file, OK
    if NOT(OK) then RETURN
    mstate.snow_vars.es_out_file = es_out_file
endif
endif
;-----------------------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;********************
'SAVE_MR_GRIDS' : $
;********************
begin
state.save_mr_grids = (1b - state.save_mr_grids)
widget_control, state.mr_rts_file_ID, sensitive=state.save_mr_grids 
end

;********************
'SAVE_HS_GRIDS' : $
;********************
begin
state.save_hs_grids = (1b - state.save_hs_grids)
widget_control, state.hs_rts_file_ID, sensitive=state.save_hs_grids
end

;********************
'SAVE_SW_GRIDS' : $
;********************
begin
state.save_sw_grids = (1b - state.save_sw_grids)
widget_control, state.sw_rts_file_ID, sensitive=state.save_sw_grids
end

;********************
'SAVE_CC_GRIDS' : $
;********************
begin
state.save_cc_grids = (1b - state.save_cc_grids)
widget_control, state.cc_rts_file_ID, sensitive=state.save_cc_grids
end

;********************
'SAVE_EA_GRIDS' : $
;********************
begin
state.save_ea_grids = (1b - state.save_ea_grids)
widget_control, state.ea_rts_file_ID, sensitive=state.save_ea_grids
end

;********************
'SAVE_ES_GRIDS' : $
;********************
begin
state.save_es_grids = (1b - state.save_es_grids)
widget_control, state.es_rts_file_ID, sensitive=state.save_es_grids
end

;*********************
'SAVE_MR_PIXELS' : $
;*********************
begin
state.save_mr_pixels = (1b - state.save_mr_pixels)
widget_control, state.mr_out_file_ID, sensitive=state.save_mr_pixels 
end

;*********************
'SAVE_HS_PIXELS' : $
;*********************
begin
state.save_hs_pixels = (1b - state.save_hs_pixels)
widget_control, state.hs_out_file_ID, sensitive=state.save_hs_pixels 
end

;*********************
'SAVE_SW_PIXELS' : $
;*********************
begin
state.save_sw_pixels = (1b - state.save_sw_pixels)
widget_control, state.sw_out_file_ID, sensitive=state.save_sw_pixels 
end

;*********************
'SAVE_CC_PIXELS' : $
;*********************
begin
state.save_cc_pixels = (1b - state.save_cc_pixels)
widget_control, state.cc_out_file_ID, sensitive=state.save_cc_pixels 
end

;*********************
'SAVE_EA_PIXELS' : $
;*********************
begin
state.save_ea_pixels = (1b - state.save_ea_pixels)
widget_control, state.ea_out_file_ID, sensitive=state.save_ea_pixels 
end

;*********************
'SAVE_ES_PIXELS' : $
;*********************
begin
state.save_es_pixels = (1b - state.save_es_pixels)
widget_control, state.es_out_file_ID, sensitive=state.save_es_pixels 
end

;***********
'HELP' : $
;***********
begin
msg = [' ', $
'This dialog allows you to specify which computed snow ',$
'variables that you want to save as a grid sequence or time ',$
'series and the sampling timestep. ', ' ']
GUI_Help, msg, state.leader_ID, TITLE='Save Snow Variables Help'
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

END;  GUI_Save_Snow_Vars_event
;*****************************************************************
pro GUI_Save_Snow_Vars, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then RETURN

;--------------------
;Get dialog defaults
;--------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-------------------------------------------------------
gstr = TF_String(mstate.snow_vars.save_grid_dt   / 60d)
pstr = TF_String(mstate.snow_vars.save_pixels_dt / 60d)
;-------------------------------------------------------
save_mr_grids  = mstate.snow_vars.save_mr_grids
save_hs_grids  = mstate.snow_vars.save_hs_grids
save_sw_grids  = mstate.snow_vars.save_sw_grids
save_cc_grids  = mstate.snow_vars.save_cc_grids
save_ea_grids  = mstate.snow_vars.save_ea_grids
save_es_grids  = mstate.snow_vars.save_es_grids
;------------------------------------------------
save_mr_pixels = mstate.snow_vars.save_mr_pixels
save_hs_pixels = mstate.snow_vars.save_hs_pixels
save_sw_pixels = mstate.snow_vars.save_sw_pixels
save_cc_pixels = mstate.snow_vars.save_cc_pixels
save_ea_pixels = mstate.snow_vars.save_ea_pixels
save_es_pixels = mstate.snow_vars.save_es_pixels
;------------------------------------------------
mr_rts_file = mstate.snow_vars.mr_rts_file
hs_rts_file = mstate.snow_vars.hs_rts_file
sw_rts_file = mstate.snow_vars.sw_rts_file
cc_rts_file = mstate.snow_vars.cc_rts_file
ea_rts_file = mstate.snow_vars.ea_rts_file
es_rts_file = mstate.snow_vars.es_rts_file
;-------------------------------------------
mr_out_file = mstate.snow_vars.mr_out_file
hs_out_file = mstate.snow_vars.hs_out_file
sw_out_file = mstate.snow_vars.sw_out_file
cc_out_file = mstate.snow_vars.cc_out_file
ea_out_file = mstate.snow_vars.ea_out_file
es_out_file = mstate.snow_vars.es_out_file

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, $
save_mr_grids:save_mr_grids, mr_rts_file_ID:0L, $
save_hs_grids:save_hs_grids, hs_rts_file_ID:0L, $
save_sw_grids:save_sw_grids, sw_rts_file_ID:0L, $
save_cc_grids:save_cc_grids, cc_rts_file_ID:0L, $
save_ea_grids:save_ea_grids, ea_rts_file_ID:0L, $
save_es_grids:save_es_grids, es_rts_file_ID:0L, $
;----------------------------------------------------
save_mr_pixels:save_mr_pixels, mr_out_file_ID:0L, $
save_hs_pixels:save_hs_pixels, hs_out_file_ID:0L, $
save_sw_pixels:save_sw_pixels, sw_out_file_ID:0L, $
save_cc_pixels:save_cc_pixels, cc_out_file_ID:0L, $
save_ea_pixels:save_ea_pixels, ea_out_file_ID:0L, $
save_es_pixels:save_es_pixels, es_out_file_ID:0L, $
;----------------------------------------------------
save_grid_dt_ID:0L, $
save_pixels_dt_ID:0L }

ngap = 6
XS1 = 20
ENERGY_BAL = (mstate.snow_vars.method eq 2)   ;****************

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Save Snow Variables', $
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
        SGB_MR = widget_button(SG112, VALUE='Meltrate [mm/day]',  $
                              UVALUE='SAVE_MR_GRIDS')
        SGB_HS = widget_button(SG112, VALUE='Snow Depth [m]',  $
                              UVALUE='SAVE_HS_GRIDS')
        SGB_SW = widget_button(SG112, VALUE='SWE [m]',  $
                              UVALUE='SAVE_SW_GRIDS')
        ;-------------------------------------------------------------
        if (save_mr_grids) then widget_control,SGB_MR,/set_button
        if (save_hs_grids) then widget_control,SGB_HS,/set_button
        if (save_sw_grids) then widget_control,SGB_SW,/set_button
        ;-------------------------------------------------------------
        if (ENERGY_BAL) then begin
            SGB_CC = widget_button(SG112, VALUE='Cold content [J/m^2]',  $
                                  UVALUE='SAVE_CC_GRIDS')
            SGB_EA = widget_button(SG112, VALUE='e_air [mbar]',  $
                                  UVALUE='SAVE_EA_GRIDS')
            SGB_ES = widget_button(SG112, VALUE='e_surf [mbar]',  $
                                  UVALUE='SAVE_ES_GRIDS')
            ;----------------------------------------------------------
            if (save_cc_grids) then widget_control,SGB_CC,/set_button
            if (save_ea_grids) then widget_control,SGB_EA,/set_button
            if (save_es_grids) then widget_control,SGB_ES,/set_button
        endif
    ;---------------------------------------------------------------------
    SG12 = widget_base(SG1, /COL, SPACE=ngap)
      SG121 = widget_label(SG12, VALUE='Output filename  (*.rts): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SG122 = widget_base(SG12, /COL)
        SGF_MR = widget_text(SG122, VALUE=mr_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SGF_HS = widget_text(SG122, VALUE=hs_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SGF_SW = widget_text(SG122, VALUE=sw_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        ;------------------------------------------------
        widget_control, SGF_MR, sensitive=save_mr_grids
        widget_control, SGF_HS, sensitive=save_hs_grids
        widget_control, SGF_SW, sensitive=save_sw_grids
        ;-----------------------------
        state.mr_rts_file_ID = SGF_MR 
        state.hs_rts_file_ID = SGF_HS
        state.sw_rts_file_ID = SGF_SW
        ;-----------------------------
        if (ENERGY_BAL) then begin
            SGF_CC = widget_text(SG122, VALUE=cc_rts_file, $
                                 UVALUE='NONE', /EDITABLE, XSIZE=XS1)
            SGF_EA = widget_text(SG122, VALUE=ea_rts_file, $
                                 UVALUE='NONE', /EDITABLE, XSIZE=XS1)
            SGF_ES = widget_text(SG122, VALUE=es_rts_file, $
                                 UVALUE='NONE', /EDITABLE, XSIZE=XS1)
            ;------------------------------------------------
            widget_control, SGF_CC, sensitive=save_cc_grids
            widget_control, SGF_EA, sensitive=save_ea_grids
            widget_control, SGF_ES, sensitive=save_es_grids
            ;-------------------------------
            state.cc_rts_file_ID = SGF_CC
            state.ea_rts_file_ID = SGF_EA 
            state.es_rts_file_ID = SGF_ES
        endif
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
        SVB_MR = widget_button(SV112, VALUE='Meltrate [mm/day]',  $
                               UVALUE='SAVE_MR_PIXELS')
        SVB_HS = widget_button(SV112, VALUE='Snow Depth [m]',  $
                               UVALUE='SAVE_HS_PIXELS')
        SVB_SW = widget_button(SV112, VALUE='SWE [m]', $
                               UVALUE='SAVE_SW_PIXELS')
        ;-----------------------------------------------------------
        if (save_mr_pixels) then widget_control,SVB_MR,/set_button
        if (save_hs_pixels) then widget_control,SVB_HS,/set_button
        if (save_sw_pixels) then widget_control,SVB_SW,/set_button
        ;-----------------------------------------------------------
        if (ENERGY_BAL) then begin
            SVB_CC = widget_button(SV112, VALUE='Cold content [J/m^2]', $
                                   UVALUE='SAVE_CC_PIXELS')
            SVB_EA = widget_button(SV112, VALUE='e_air [mbar]',  $
                                   UVALUE='SAVE_EA_PIXELS')
            SVB_ES = widget_button(SV112, VALUE='e_surf [mbar]',  $
                                   UVALUE='SAVE_ES_PIXELS')
            ;-----------------------------------------------------------
            if (save_cc_pixels) then widget_control,SVB_CC,/set_button
            if (save_ea_pixels) then widget_control,SVB_EA,/set_button
            if (save_es_pixels) then widget_control,SVB_ES,/set_button
        endif
    ;---------------------------------------------------------------------
    SV12 = widget_base(SV1, /COL, SPACE=ngap)
      SV121 = widget_label(SV12, VALUE='Output filename  (*.txt): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SV122 = widget_base(SV12, /COL)
        SVF_MR = widget_text(SV122, VALUE=mr_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SVF_HS = widget_text(SV122, VALUE=hs_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SVF_SW = widget_text(SV122, VALUE=sw_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        ;-------------------------------------------------
        widget_control, SVF_MR, sensitive=save_mr_pixels
        widget_control, SVF_HS, sensitive=save_hs_pixels
        widget_control, SVF_SW, sensitive=save_sw_pixels
        ;-------------------------------------------------
        state.mr_out_file_ID = SVF_MR
        state.hs_out_file_ID = SVF_HS
        state.sw_out_file_ID = SVF_SW
        ;------------------------------
        if (ENERGY_BAL) then begin
            SVF_CC = widget_text(SV122, VALUE=cc_out_file, $
                                 UVALUE='NONE', /EDITABLE, XSIZE=XS1)
            SVF_EA = widget_text(SV122, VALUE=ea_out_file, $
                                UVALUE='NONE', /EDITABLE, XSIZE=XS1)
            SVF_ES = widget_text(SV122, VALUE=es_out_file, $
                                 UVALUE='NONE', /EDITABLE, XSIZE=XS1)
            ;-------------------------------------------------
            widget_control, SVF_CC, sensitive=save_cc_pixels
            widget_control, SVF_EA, sensitive=save_ea_pixels
            widget_control, SVF_ES, sensitive=save_es_pixels
            ;------------------------------
            state.cc_out_file_ID = SVF_CC
            state.ea_out_file_ID = SVF_EA
            state.es_out_file_ID = SVF_ES
        endif
  ;---------------------------------------------------------------------
  SV2 = widget_base(SV, /ROW, SPACE=ngap)
    SV21 = widget_label(SV2, VALUE='At this timestep: ', UVALUE='NONE')
    SV22 = widget_text(SV2, VALUE=pstr, UVALUE='NONE', $
                       /EDITABLE, XSIZE=XS1)
    SV23 = widget_label(SV2, VALUE='[min]', UVALUE='NONE')
    state.save_pixels_dt_ID = SV22

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Save_Snow_Vars', $
             XOFF=XOFF  ;***, TW=[F12, F22]

END;  GUI_Save_Snow_Vars 
;*****************************************************************


