
;*****************************************************************
;   GUI_gw.pro

;   Copyright (c) 2001-2008, Scott D. Peckham 
;   Created:  Dec 2001 - Jan 2002
;   Modified: Feb 2004, July 2006
;   Modified: Mar 2008  (New way to do event handling w/ layers)

;*****************************************************************

;   Read_Darcy_Layer_Vars
;   Darcy_Layer_Vars_OK           (function)
;   GUI_Subsurface_Vars_event     (new method: 3/14/08)
;   GUI_Darcy_Layer_Panel         (function)
;   GUI_Subsurface_Vars

;   GUI_Save_GW_Vars_event
;   GUI_Save_GW_Vars
 
;----------------------------------------------
;   No longer used;  obsolete soon?  (3/6/08)
;----------------------------------------------
;   GUI_Darcy_Law_Formulas_event
;   GUI_Darcy_Law_Formulas

;*****************************************************************
pro Read_Darcy_Layer_Vars, state, Ks, Ks_file, qs, qs_file, $
                           th, th_file, OK

OK = 1b

;-------------------------------
;Initialize the soil var arrays
;-------------------------------
Ks = dblarr(state.n_layers)
qs = dblarr(state.n_layers)
th = dblarr(state.n_layers)
;--------------------------------
Ks_file = strarr(state.n_layers)
qs_file = strarr(state.n_layers)
th_file = strarr(state.n_layers)

;----------------------------------------
;Read the soil properties for all layers
;----------------------------------------
for j=0,(state.n_layers-1) do begin
    Read_Input_Type, state.Ks_type[j], state.Ks_ID[j], Ksj, $
                 OK, filename=Ks_file_j
    if NOT(OK) then RETURN
    Ks[j]      = Ksj
    Ks_file[j] = Ks_file_j
    ;----------------------------------------------------------
    Read_Input_Type, state.qs_type[j], state.qs_ID[j], qsj, $
                 OK, filename=qs_file_j
    if NOT(OK) then RETURN
    qs[j]      = qsj
    qs_file[j] = qs_file_j
    ;----------------------------------------------------------
    Read_Input_Type, state.th_type[j], state.th_ID[j], thj, $
                 OK, filename=th_file_j
    if NOT(OK) then RETURN
    th[j]      = thj
    th_file[j] = th_file_j
endfor

end;  Read_Darcy_Layer_Vars
;*****************************************************************
function Darcy_Layer_Vars_OK, state, Ks, Ks_file, qs, qs_file, $
                              th, th_file

;-------------------------------------------------------
;NB!  These comparisons work because the returned vars
;     will be scalars even if the type is not scalar.
;     (state is passed as state.L1, etc.)
;-------------------------------------------------------

;----------------------------------------
;Check the soil properties for all layers
;----------------------------------------
for j=0,(state.n_layers-1) do begin
    heading = 'Input Error in Layer' + TF_String(j+1) + ':'
    ;----------------------------------------------------------
    if (state.Ks_type[j] eq 0b) AND (Ks[j] lt 0.0) then begin
        msg = [ $
        heading, ' ',$
        'Ks must be nonnegative.', ' ']
        result = GUI_Message(msg, /INFO)
        RETURN, 0b
    endif
    ;--------------------------------------------------
    if (state.qs_type[j] eq 0b) AND $
       ((qs[j] gt 1.0) OR (qs[j] lt 0.0)) then begin
        msg = [ $
        heading, ' ',$
        'S_y must be between 0 and 1 and must', $
        'be less than the soil porosity.', ' ']
        result = GUI_Message(msg, /INFO)
        RETURN, 0b
    endif
    ;--------------------------------------------------
    if (state.th_type[j] eq 0b) AND (th[j] le 0.0) then begin
        msg = [ $
        heading, ' ',$
        'Layer thickness must be greater than zero.', ' ']
        result = GUI_Message(msg, /INFO)
        RETURN, 0b
    endif
endfor

;--------------------------
;Everything seems to be OK
;--------------------------
RETURN, 1b

end;  Darcy_Layer_Vars_OK
;*****************************************************************
pro GUI_Subsurface_Vars_event, event

;----------------------------------------------------------------;Notes:  3/14/08.  New approach to reduce the amount of code;        and simplify event handling for multiple soil layers.;        Added a "visible_panel" variable to the state that;        allows each panel to use the same "uvalues" for events.;        Related changes made to GUI_Darcy_Layer_Panel.;----------------------------------------------------------------
;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state
PANEL_CHANGE = 0b    ;(default)
n = state.visible_panel

case (uvalue) of

;***********'NEXT' : $;***********beginPANEL_CHANGE = 1bstate.visible_panel = n + 1end;***********'BACK' : $;***********beginPANEL_CHANGE = 1bstate.visible_panel = n - 1end

;***************************
;SOIL LAYER VAR DATA TYPES
;***************************
'KS_TYPE' : state.Ks_type[n] = event.index
'QS_TYPE' : state.qs_type[n] = event.index
'TH_TYPE' : state.th_type[n] = event.index

;********************************
;ADDITIONAL VARIABLE DATA TYPES
;********************************
'ELEV_TYPE'      : state.elev_type      = event.index
'H0_TABLE_TYPE'  : state.h0_table_type  = event.index
'D_BEDROCK_TYPE' : state.d_bedrock_type = event.index
'D_FREEZE_TYPE'  : state.d_freeze_type  = event.index
'D_THAW_TYPE'    : state.d_thaw_type    = event.index

;*********
'OK' : $
;*********
begin
;---------------------------------
;Read Darcy variables into arrays
;---------------------------------
Read_Darcy_Layer_Vars, state, Ks, Ks_file, qs, qs_file, $
                       th, th_file, OK
if NOT(OK) then RETURN

;--------------------------------
;Check the Darcy variable arrays
;--------------------------------
OK = Darcy_Layer_Vars_OK(state, Ks, Ks_file, qs, qs_file, $
                         th, th_file)
if NOT(OK) then RETURN

;-------------------------------
;Read other filenames or values
;-------------------------------
Read_Input_Type, state.elev_type, state.elev_ID, $
                 elev, OK, filename=elev_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.h0_table_type, state.h0_table_ID, $
                 h0_table, OK, filename=h0_table_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Input_Type, state.d_bedrock_type, state.d_bedrock_ID, $
                 d_bedrock, OK, filename=d_bedrock_file
if NOT(OK) then RETURN
;-------------------------------------------------------------
;These are not used yet
;-----------------------
;Read_Input_Type, state.d_freeze_type, state.d_freeze_ID, $
;                 d_freeze, OK, filename=d_freeze_file
;if NOT(OK) then RETURN
;-------------------------------------------------------------
;Read_Input_Type, state.d_thaw_type, state.d_thaw_ID, $
;                 d_thaw, OK, filename=d_thaw_file
;if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.gw_dt_ID, gw_dt, OK, /DOUBLE
if NOT(OK) then RETURN
;--------------------------------------
;Convert gw_dt from hours to seconds ?
;--------------------------------------
;** gw_dt = (gw_dt * 3600d)  ;[hours -> seconds]

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;-------------------------------
mstate.gw_vars.dt = gw_dt

;----------------------------------
;Upload values for all soil layers
;----------------------------------
for j=0,(state.n_layers-1) do begin
    *(mstate.gw_vars.Ks[j])   = Ks[j]
    *(mstate.gw_vars.qs[j])   = qs[j]
    *(mstate.gw_vars.th[j])   = th[j]
    ;----------------------------------------
    mstate.gw_vars.Ks_file[j] = Ks_file[j]
    mstate.gw_vars.qs_file[j] = qs_file[j]
    mstate.gw_vars.th_file[j] = th_file[j]
    ;--------------------------------------------
    mstate.gw_vars.Ks_type[j] = state.Ks_type[j]
    mstate.gw_vars.qs_type[j] = state.qs_type[j]
    mstate.gw_vars.th_type[j] = state.th_type[j]
endfor

;----------------------------------
;Upload values for other variables
;----------------------------------
*mstate.gw_vars.elev         = elev
*mstate.gw_vars.h0_table     = h0_table    ;[m]
*mstate.gw_vars.d_bedrock    = d_bedrock  ;[m]
;*mstate.gw_vars.d_freeze    = d_freeze   ;[m]
;*mstate.gw_vars.d_thaw      = d_thaw     ;[m]
;-----------------------------------------------
mstate.gw_vars.elev_file      = elev_file
mstate.gw_vars.h0_table_file  = h0_table_file
mstate.gw_vars.d_bedrock_file = d_bedrock_file
;mstate.gw_vars.d_freeze_file = d_freeze_file
;mstate.gw_vars.d_thaw_file   = d_thaw_file
;----------------------------------------------------
mstate.gw_vars.elev_type      = state.elev_type
mstate.gw_vars.h0_table_type  = state.h0_table_type
mstate.gw_vars.d_bedrock_type = state.d_bedrock_type
;mstate.gw_vars.d_freeze_type = state.d_freeze_type
;mstate.gw_vars.d_thaw_type   = state.d_thaw_type
;----------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'GW_Darcy_parallel.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

;--------------------------------
;Change to a new panel  (remap)
;--------------------------------
if (PANEL_CHANGE) then begin
    onoff = bytarr(state.n_layers)
    onoff[state.visible_panel] = 1b
    for k=0,(state.n_layers-1) do begin
        widget_control, state.bases[k], MAP=onoff[k]
    endfor
endif

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Subsurface_Vars_event
;*****************************************************************
function GUI_Darcy_Layer_Panel, PARENT, state, mstate, j

FORWARD_FUNCTION Var_Setting

ngap  = 6
fsize = 20
XS    = 44
types = Model_Input_Types()
stype = ' Scalar  '
jstr  = strtrim(string(j+1), 2)

;------------------------------------------
;Get values from main state for this layer
;------------------------------------------
Ks_str = Var_Setting(mstate.gw_vars.Ks_type[j], $
                     mstate.gw_vars.Ks[j], $
                     mstate.gw_vars.Ks_file[j])
qs_str = Var_Setting(mstate.gw_vars.qs_type[j], $
                     mstate.gw_vars.qs[j], $
                     mstate.gw_vars.qs_file[j])
th_str = Var_Setting(mstate.gw_vars.th_type[j], $
                     mstate.gw_vars.th[j], $
                     mstate.gw_vars.th_file[j])

;--------------------------
;Create the top-level base
;and two "middle bases"
;--------------------------
MB = widget_base(PARENT, /COLUMN)
TB = widget_base(MB, /COLUMN)
CB = widget_base(MB, /COLUMN, /FRAME)

;-------------------------
;Use top base for heading
;-------------------------
v0 = ' Variables for Soil Layer ' + jstr + ':'
L0 = widget_label(TB, VALUE=v0, /ALIGN_LEFT)

;------------------------
;Get vars for this layer
;------------------------
A1 = widget_base(CB, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ')
  A12 = widget_label(A1, VALUE='Type: ')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ')
  A14 = widget_label(A1, VALUE='Units: ')
;--------------------------------------------------------------------------
KS = widget_base(CB, /ROW, SPACE=ngap)
  KS1 = widget_label(KS, VALUE='K_s: ', /ALIGN_LEFT)
  KS2 = widget_droplist(KS, VALUE=types, UVALUE='KS_TYPE')
  KS3 = widget_text(KS, VALUE=Ks_str, UVALUE='NONE', /EDITABLE, XSIZE=8)
  KS4 = widget_label(KS, VALUE='[m/s]')
  widget_control, KS2, set_droplist_select=mstate.gw_vars.Ks_type[j]
;--------------------------------------------------------------------------
QS = widget_base(CB, /ROW, SPACE=ngap)
  QS1 = widget_label(QS, VALUE='S_y: ', /ALIGN_LEFT)
  QS2 = widget_droplist(QS, VALUE=types, UVALUE='QS_TYPE')
  QS3 = widget_text(QS, VALUE=qs_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  QS4 = widget_label(QS, VALUE='[unitless]', UVALUE='NONE')
  widget_control, QS2, set_droplist_select=mstate.gw_vars.qs_type[j] 
;--------------------------------------------------------------------------
TH = widget_base(CB, /ROW, SPACE=ngap)
  TH1 = widget_label(TH, VALUE='thickness: ', /ALIGN_LEFT)
  TH2 = widget_droplist(TH, VALUE=types, UVALUE='TH_TYPE')
  TH3 = widget_text(TH, VALUE=th_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TH4 = widget_label(TH, VALUE='[meters]', UVALUE='NONE')
  widget_control, TH2, set_droplist_select=mstate.gw_vars.th_type[j]
;--------------------------------------------------------------------------
BZ = widget_label(CB, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [A11, KS1, QS1, TH1]
Align_Text_Boxes, [A12, KS2, QS2, TH2]
Align_Text_Boxes, [A13, KS3, QS3, TH3]

;----------------------
;Back and Next buttons
;----------------------
space = 7
BN = widget_base(CB, /ROW, SPACE=space)
  BN0 = widget_label(BN, VALUE='Soil layer ' + jstr + ':   ')
  BN1 = widget_button(BN, VALUE=Str_Pad('< Back'), UVALUE='BACK')
  BN2 = widget_button(BN, VALUE=Str_Pad('Next >'), UVALUE='NEXT')
  if (j eq 0) then widget_control, BN1, sensitive=0
  nL = mstate.gw_vars.nlayers
  if (j eq (nL-1)) then widget_control, BN2, sensitive=0

;-----------------------
;Store the text box IDs
;-----------------------
state.Ks_ID[j] = KS3
state.qs_ID[j] = QS3
state.th_ID[j] = TH3

;--------------------------------------;Initialize the "types", used for;input error checking  (Added 3/14/08);--------------------------------------state.Ks_type[j]  = mstate.gw_vars.Ks_type[j]state.qs_type[j]  = mstate.gw_vars.qs_type[j]state.th_type[j]  = mstate.gw_vars.th_type[j]

;------------------------
;Return the main base ID
;------------------------
RETURN, MB

end;  GUI_Darcy_Layer_Panel
;*****************************************************************
pro GUI_Subsurface_Vars, leader 

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if (n_elements(leader) eq 0) then leader=0L 
ngap  = 6
types = Model_Input_Types()

;-----------------------------------
;Get current values from main state
;-----------------------------------
Get_TLB_State, leader, mstate, ALIVE
if NOT(ALIVE) then RETURN
n_layers = mstate.gw_vars.nlayers

;-------------------------------
;Get "common variable" defaults
;-------------------------------
elev_str     = Var_Setting(mstate.gw_vars.elev_type, $
                           mstate.gw_vars.elev, $
                           mstate.gw_vars.elev_file)
h0_table_str = Var_Setting(mstate.gw_vars.h0_table_type, $
                           mstate.gw_vars.h0_table, $
                           mstate.gw_vars.h0_table_file)
d_freeze_str = Var_Setting(mstate.gw_vars.d_freeze_type, $
                           mstate.gw_vars.d_freeze, $
                           mstate.gw_vars.d_freeze_file)
d_thaw_str   = Var_Setting(mstate.gw_vars.d_thaw_type, $
                           mstate.gw_vars.d_thaw, $
                           mstate.gw_vars.d_thaw_file)
d_bedrock_str = Var_Setting(mstate.gw_vars.d_bedrock_type, $
                            mstate.gw_vars.d_bedrock, $
                            mstate.gw_vars.d_bedrock_file)

;---------------------------
;Get default timestep, etc.    (LATER TEST IF STABLE OR NOT)
;---------------------------
dt_str = TF_String(mstate.gw_vars.dt)   ;[seconds]

;------------------------------------------
;Store selected options in state structure
;------------------------------------------
state = { $
leader_ID:leader, visible_panel: 0, $
n_layers: n_layers, bases: lonarr(n_layers), $
gw_dt_ID:0L,  $
;----------------------------------------------
elev_ID:0L,       elev_type:mstate.gw_vars.elev_type,           $
h0_table_ID:0L,   h0_table_type:mstate.gw_vars.h0_table_type,     $
d_bedrock_ID:0L,  d_bedrock_type:mstate.gw_vars.d_bedrock_type, $
;d_freeze_ID:0L,  d_freeze_type:mstate.gw_vars.d_freeze_type,   $
;d_thaw_ID:0L,    d_thaw_type:mstate.gw_vars.d_thaw_type,       $
;-------------------------------------
;These soil layer vars are all arrays
;-------------------------------------
Ks_ID:lonarr(n_layers), Ks_type:mstate.gw_vars.Ks_type, $
qs_ID:lonarr(n_layers), qs_type:mstate.gw_vars.qs_type, $
th_ID:lonarr(n_layers), th_type:mstate.gw_vars.th_type  }

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Subsurface Flow Variables: Darcy Layers', $
            /COLUMN, LEADER=leader   ;***, /MODAL)

;--------------------------------
;Create the set of wizard panels
;--------------------------------
PP = widget_base(MB)
for j=0,(n_layers-1) do begin
    state.bases[j] = GUI_Darcy_Layer_Panel(PP, state, mstate, j)
endfor

;------------------------------
;Unmap all but the first panel
;------------------------------
onoff = bytarr(n_layers)
onoff[0] = 1b
for k=0,(n_layers-1) do widget_control, state.bases[k], MAP=onoff[k]

;------------------------------
;Use separate base for heading
;------------------------------
CB = widget_base(MB, /COLUMN)
HB = widget_base(CB)
v1 = ' Additional Variables:'
L1 = widget_label(HB, VALUE=v1, /ALIGN_LEFT)

;---------------------------
;Get the "common variables"
;---------------------------
B  = widget_base(CB, /COLUMN, /FRAME)
A1 = widget_base(B, /ROW, SPACE=ngap)
  A11 = widget_label(A1, VALUE='Variable: ')
  A12 = widget_label(A1, VALUE='Type: ')
  A13 = widget_label(A1, VALUE='Scalar or Grid Filename: ')
  A14 = widget_label(A1, VALUE='Units: ')
;-------------------------------------------------------------------------
EG = widget_base(B, /ROW, SPACE=ngap)
  EG1 = widget_label(EG, VALUE='Surface elevation: ', /ALIGN_LEFT)
  EG2 = widget_droplist(EG, VALUE=types, UVALUE='ELEV_TYPE')
  EG3 = widget_text(EG, VALUE=elev_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  EG4 = widget_label(EG, VALUE='[m]')
  state.elev_ID = EG3
  widget_control, EG2, set_droplist_select=mstate.gw_vars.elev_type 
;-------------------------------------------------------------------------
TG = widget_base(B, /ROW, SPACE=ngap)
  TG1 = widget_label(TG, VALUE='Init. water table elev.: ', /ALIGN_LEFT)
  TG2 = widget_droplist(TG, VALUE=types, UVALUE='H0_TABLE_TYPE')
  TG3 = widget_text(TG, VALUE=h0_table_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  TG4 = widget_label(TG, VALUE='[m]')
  state.h0_table_ID   = TG3
  widget_control, TG2, set_droplist_select=mstate.gw_vars.h0_table_type
;-------------------------------------------------------------------------
;DF = widget_base(B, /ROW, SPACE=ngap)
;  DF1 = widget_label(DF, VALUE='Init. depth to nonfrozen: ', /ALIGN_LEFT)
;  DF2 = widget_droplist(DF, VALUE=types, UVALUE='D_FREEZE_TYPE')
;  DF3 = widget_text(DF, VALUE=d_freeze_str, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=fsize)
;  DF4 = widget_label(DF, VALUE='[m]')
;  state.d_freeze_ID   = DF3
;  widget_control, DF2, set_droplist_select=mstate.gw_vars.d_freeze_type
;  widget_control, DF, sensitive=0  ;**************************
;-------------------------------------------------------------------------
;DT = widget_base(B, /ROW, SPACE=ngap)
;  DT1 = widget_label(DT, VALUE='Init. depth of thaw: ', /ALIGN_LEFT)
;  DT2 = widget_droplist(DT, VALUE=types, UVALUE='D_THAW_TYPE')
;  DT3 = widget_text(DT, VALUE=d_thaw_str, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=fsize)
;  DT4 = widget_label(DT, VALUE='[m]')
;  state.d_thaw_ID = DT3
;  widget_control, DT2, set_droplist_select=mstate.gw_vars.d_thaw_type
;-------------------------------------------------------------------------
DB = widget_base(B, /ROW, SPACE=ngap)
  DB1 = widget_label(DB, VALUE='Depth to bedrock: ', /ALIGN_LEFT)
  DB2 = widget_droplist(DB, VALUE=types, UVALUE='D_BEDROCK_TYPE')
  DB3 = widget_text(DB, VALUE=d_bedrock_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  DB4 = widget_label(DB, VALUE='[m]')
  state.d_bedrock_ID = DB3
  widget_control, DB2, set_droplist_select=mstate.gw_vars.d_bedrock_type
;-------------------------------------------------------------------------
G2 = widget_label(B, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
TI = widget_base(B, /ROW, SPACE=ngap)
  TI1 = widget_label(TI, VALUE='Subsurface flow timestep: ')
  TI2 = widget_text(TI, VALUE=dt_str, UVALUE='NONE', /EDITABLE, XSIZE=10)
  TI3 = widget_label(TI, VALUE='[seconds / timestep]')
  state.gw_dt_ID = TI2
;--------------------------------------------------------------------------
Align_Text_Boxes, [A11, EG1, TG1, DB1]
Align_Text_Boxes, [A12, EG2, TG2, DB2]
Align_Text_Boxes, [A13, EG3, TG3, DB3]

;-----------------------------
;Make framed bases same width
;-----------------------------
geom = widget_info(B, /geometry)
for k=0,(n_layers-1) do $
    widget_control, state.bases[k], SCR_XSIZE=(geom.scr_xsize + 6)

;------------------
;Bottom button bar
;------------------
space = 7
A = widget_base(MB, /ROW, SPACE=space)
  A1 = widget_button(A, VALUE=Str_Pad('OK'), UVALUE='OK')
  A2 = widget_button(A, VALUE=Str_Pad('Help'), UVALUE='HELP')
  A3 = widget_button(A, VALUE=Str_Pad('Cancel'), UVALUE='CANCEL')

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Subsurface_Vars', XOFF=480, TW=TW

END;  GUI_Subsurface_Vars
;*****************************************************************
;*****************************************************************
pro GUI_Save_GW_Vars_event, event

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
mstate.gw_vars.save_grid_dt   = save_grid_dt
mstate.gw_vars.save_pixels_dt = save_pixels_dt
;-----------------------------------------------------------
mstate.gw_vars.save_ht_grids  = state.save_ht_grids
mstate.gw_vars.save_df_grids  = state.save_df_grids
mstate.gw_vars.save_dt_grids  = state.save_dt_grids
;*** mstate.gw_vars.save_sm_grids  = state.save_sm_grids
;-----------------------------------------------------------
mstate.gw_vars.save_ht_pixels = state.save_ht_pixels
mstate.gw_vars.save_df_pixels = state.save_df_pixels
mstate.gw_vars.save_dt_pixels = state.save_dt_pixels
;*** mstate.gw_vars.save_sm_pixels = state.save_sm_pixels
 
;-----------------------------
;Collect the output filenames
;-----------------------------
if (state.save_ht_grids) then begin
    Read_Text_Box, state.ht_rts_file_ID, ht_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, ht_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.gw_vars.ht_rts_file = ht_rts_file
endif
;-------------------------------------------------------------------
if (state.save_df_grids) then begin
    Read_Text_Box, state.df_rts_file_ID, df_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, df_RTS_File, OK

    if NOT(OK) then RETURN
    mstate.gw_vars.df_rts_file = df_rts_file
endif
;-------------------------------------------------------------------
if (state.save_dt_grids) then begin
    Read_Text_Box, state.dt_rts_file_ID, dt_rts_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, dt_RTS_File, OK
    if NOT(OK) then RETURN
    mstate.gw_vars.dt_rts_file = dt_rts_file
endif
;-------------------------------------------------------------------
;if (state.save_sm_grids) then begin
;    Read_Text_Box, state.sm_rts_file_ID, sm_rts_file, OK, /FILE
;    if NOT(OK) then RETURN
;    Check_Overwrite, sm_RTS_File, OK
;    if NOT(OK) then RETURN
;    mstate.gw_vars.sm_rts_file = sm_rts_file
;endif
;-------------------------------------------------------------------
if (state.save_ht_pixels) then begin
    Read_Text_Box, state.ht_out_file_ID, ht_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, ht_out_file, OK
    if NOT(OK) then RETURN
    mstate.gw_vars.ht_out_file = ht_out_file
endif
;-------------------------------------------------------------------
if (state.save_df_pixels) then begin
    Read_Text_Box, state.df_out_file_ID, df_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, df_out_file, OK
    if NOT(OK) then RETURN
    mstate.gw_vars.df_out_file = df_out_file
endif
;-------------------------------------------------------------------
if (state.save_dt_pixels) then begin
    Read_Text_Box, state.dt_out_file_ID, dt_out_file, OK, /FILE
    if NOT(OK) then RETURN
    Check_Overwrite, dt_out_file, OK
    if NOT(OK) then RETURN
    mstate.gw_vars.dt_out_file = dt_out_file
endif
;-------------------------------------------------------------------
;if (state.save_sm_pixels) then begin
;    Read_Text_Box, state.sm_out_file_ID, sm_out_file, OK, /FILE
;    if NOT(OK) then RETURN
;    Check_Overwrite, sm_out_file, OK
;    if NOT(OK) then RETURN
;    mstate.gw_vars.sm_out_file = sm_out_file
;endif
;-------------------------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top
end

;******************
'SAVE_HT_GRIDS' : $
;******************
begin
state.save_ht_grids = (1b - state.save_ht_grids)
widget_control, state.ht_rts_file_ID, sensitive=state.save_ht_grids
end

;******************
'SAVE_DF_GRIDS' : $
;******************
begin
state.save_df_grids = (1b - state.save_df_grids)
widget_control, state.df_rts_file_ID, sensitive=state.save_df_grids 
end

;******************
'SAVE_DT_GRIDS' : $
;******************
begin
state.save_dt_grids = (1b - state.save_dt_grids)
widget_control, state.dt_rts_file_ID, sensitive=state.save_dt_grids 
end


;******************
;'SAVE_SM_GRIDS' : $
;******************
;begin
;state.save_sm_grids = (1b - state.save_sm_grids)
;widget_control, state.sm_rts_file_ID, sensitive=state.save_sm_grids
;end

;********************
'SAVE_HT_PIXELS' : $
;********************
begin
state.save_ht_pixels = (1b - state.save_ht_pixels)
widget_control, state.ht_out_file_ID, sensitive=state.save_ht_pixels 
end

;********************
'SAVE_DF_PIXELS' : $
;********************
begin
state.save_df_pixels = (1b - state.save_df_pixels)
widget_control, state.df_out_file_ID, sensitive=state.save_df_pixels 
end

;********************
'SAVE_DT_PIXELS' : $
;********************
begin
state.save_dt_pixels = (1b - state.save_dt_pixels)
widget_control, state.dt_out_file_ID, sensitive=state.save_dt_pixels 
end


;********************
;'SAVE_SM_PIXELS' : $
;********************
;begin
;state.save_sm_pixels = (1b - state.save_sm_pixels)
;widget_control, state.sm_out_file_ID, sensitive=state.save_sm_pixels 
;end


;***********
'HELP' : $
;***********
begin
msg = [' ',$
'This dialog allows you to specify which computed subsurface ',$
'flow variables that you want to save as a grid sequence or time ',$
'series and the sampling timestep. ', ' ']
GUI_Help, msg, state.leader_ID, TITLE='Save GW Variables Help'
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

END;  GUI_Save_GW_Vars_event
;*****************************************************************
pro GUI_Save_GW_Vars, leader

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
;-----------------------------------------------------
gstr = TF_String(mstate.gw_vars.save_grid_dt   / 60d)
pstr = TF_String(mstate.gw_vars.save_pixels_dt / 60d)
;-----------------------------------------------------
save_ht_grids  = mstate.gw_vars.save_ht_grids
save_df_grids  = mstate.gw_vars.save_df_grids
save_dt_grids  = mstate.gw_vars.save_dt_grids
;** save_sm_grids  = mstate.gw_vars.save_sm_grids 
;----------------------------------------------------
save_ht_pixels = mstate.gw_vars.save_ht_pixels
save_df_pixels = mstate.gw_vars.save_df_pixels
save_dt_pixels = mstate.gw_vars.save_dt_pixels
;*** save_sm_pixels = mstate.gw_vars.save_sm_pixels
;----------------------------------------------------
ht_rts_file = mstate.gw_vars.ht_rts_file
df_rts_file = mstate.gw_vars.df_rts_file
dt_rts_file = mstate.gw_vars.dt_rts_file
;** sm_rts_file = mstate.gw_vars.sm_rts_file
;--------------------------------------------
ht_out_file = mstate.gw_vars.ht_out_file
df_out_file = mstate.gw_vars.df_out_file
dt_out_file = mstate.gw_vars.dt_out_file
;** sm_out_file = mstate.gw_vars.sm_out_file

;--------------------
;Used before 7/27/06
;--------------------
;prefix = mstate.run_vars.run_prefix
;if (ht_rts_file eq '') then ht_rts_file=(prefix + '_2D-htable.rts')
;if (df_rts_file eq '') then df_rts_file=(prefix + '_2D-dfreeze.rts')
;if (dt_rts_file eq '') then dt_rts_file=(prefix + '_2D-dthaw.rts')
;;** if (sm_rts_file eq '') then sm_rts_file=(prefix + '_2D-q0.rts')
;if (ht_out_file eq '') then ht_out_file=(prefix + '_0D-htable.txt')
;if (df_out_file eq '') then df_out_file=(prefix + '_0D-dfreeze.txt')
;if (dt_out_file eq '') then dt_out_file=(prefix + '_0D-dthaw.txt')
;;** if (sm_out_file eq '') then sm_out_file=(prefix + '_0D-q0.txt')

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, $
save_ht_grids:save_ht_grids, ht_rts_file_ID:0L, $
save_df_grids:save_df_grids, df_rts_file_ID:0L, $
save_dt_grids:save_dt_grids, dt_rts_file_ID:0L, $
;*** save_sm_grids:save_sm_grids, sm_rts_file_ID:0L, $
;--------------------------------------------------------
save_ht_pixels:save_ht_pixels, ht_out_file_ID:0L, $
save_df_pixels:save_df_pixels, df_out_file_ID:0L, $
save_dt_pixels:save_dt_pixels, dt_out_file_ID:0L, $
;*** save_sm_pixels:save_sm_pixels, sm_out_file_ID:0L, $
;--------------------------------------------------------
save_grid_dt_ID:0L, $
save_pixels_dt_ID:0L }

ngap = 6
XS1 = 20

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Save GW Variables', /COLUMN, LEADER=leader
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
        SG1121 = widget_button(SG112, VALUE='Elev. of Water Table [m]',  $
                               UVALUE='SAVE_HT_GRIDS')
        SG1122 = widget_button(SG112, VALUE='Depth of freeze [m]',  $
                               UVALUE='SAVE_DF_GRIDS')
        SG1123 = widget_button(SG112, VALUE='Depth of thaw [m]',  $
                               UVALUE='SAVE_DT_GRIDS')
        ;*** SG1124 = widget_button(SG112, VALUE='Soil water content [none]',  $
        ;***                       UVALUE='SAVE_SM_GRIDS')
        if (save_ht_grids) then widget_control,SG1121,/set_button
        if (save_df_grids) then widget_control,SG1122,/set_button
        if (save_dt_grids) then widget_control,SG1123,/set_button
        ;*** if (save_sm_grids) then widget_control,SG1124,/set_button
    ;---------------------------------------------------------------------
    SG12 = widget_base(SG1, /COL, SPACE=ngap)
      SG121 = widget_label(SG12, VALUE='Output filename  (*.rts): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SG122 = widget_base(SG12, /COL)
        SG1221 = widget_text(SG122, VALUE=ht_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SG1222 = widget_text(SG122, VALUE=df_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SG1223 = widget_text(SG122, VALUE=dt_rts_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        ;*** SG1224 = widget_text(SG122, VALUE=sm_rts_file, $
        ;***                      UVALUE='NONE', /EDITABLE, XSIZE=XS1)

      widget_control, SG1221, sensitive=save_ht_grids
      widget_control, SG1222, sensitive=save_df_grids
      widget_control, SG1223, sensitive=save_dt_grids
      ;*** widget_control, SG1224, sensitive=save_sm_grids 
      state.ht_rts_file_ID = SG1221
      state.df_rts_file_ID = SG1222
      state.dt_rts_file_ID = SG1223
      ;*** state.sm_rts_file_ID = SG1224
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
        SV1121 = widget_button(SV112, VALUE='Elev. of Water Table [m]',  $
                               UVALUE='SAVE_HT_PIXELS')
        SV1122 = widget_button(SV112, VALUE='Depth of freeze [m]',  $
                               UVALUE='SAVE_DF_PIXELS')
        SV1123 = widget_button(SV112, VALUE='Depth of thaw [m]',  $
                               UVALUE='SAVE_DT_PIXELS')
        ;*** SV1124 = widget_button(SV112, VALUE='Soil water content [none]',  $
        ;***                        UVALUE='SAVE_SM_PIXELS')
        if (save_ht_pixels) then widget_control,SV1121,/set_button
        if (save_df_pixels) then widget_control,SV1122,/set_button
        if (save_dt_pixels) then widget_control,SV1123,/set_button 
        ;*** if (save_sm_pixels) then widget_control,SV1124,/set_button

    ;---------------------------------------------------------------------
    SV12 = widget_base(SV1, /COL, SPACE=ngap)
      SV121 = widget_label(SV12, VALUE='Output filename  (*.txt): ', $
                           UVALUE='NONE', /ALIGN_LEFT)
      SV122 = widget_base(SV12, /COL)
        SV1221 = widget_text(SV122, VALUE=ht_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SV1222 = widget_text(SV122, VALUE=df_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        SV1223 = widget_text(SV122, VALUE=dt_out_file, $
                             UVALUE='NONE', /EDITABLE, XSIZE=XS1)
        ;*** SV1224 = widget_text(SV122, VALUE=sm_out_file, $
        ;***                      UVALUE='NONE', /EDITABLE, XSIZE=XS1)

      widget_control, SV1221, sensitive=save_ht_pixels
      widget_control, SV1222, sensitive=save_df_pixels
      widget_control, SV1223, sensitive=save_dt_pixels
      ;*** widget_control, SV1224, sensitive=save_sm_pixels
      state.ht_out_file_ID = SV1221
      state.df_out_file_ID = SV1222
      state.dt_out_file_ID = SV1223
      ;*** state.sm_out_file_ID = SV1224
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
;Align_Text_Boxes, [SV21, SV31]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Save_GW_Vars', $
             XOFF=XOFF  ;***, TW=[F12, F22]

END;  GUI_Save_GW_Vars 
;*****************************************************************
;   No longer used;  obsolete soon?  (3/6/08)
;*****************************************************************
pro GUI_Darcy_Law_Formulas_event, event

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
'These are the equations used for the Darcy Law, uniform ', $
'soil layers method of estimating runoff due to seepage from ',$
'subsurface flow. ', ' ']
GUI_Help, msg, state.leader_ID, TITLE="Darcy's Law Help"
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

END;  GUI_Darcy_Law_Formulas_event
;*****************************************************************
pro GUI_Darcy_Law_Formulas, leader

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
Create_TLB, MB, TITLE="Formulas for Darcy's Law Method", $
               /COLUMN, LEADER=leader 
A = widget_base(MB, /COLUMN, /FRAME)

;-------------------
;Show the formulas
;-------------------
F1 = widget_base(A, /ROW, SPACE=ngap)
  F11 = widget_label(F1, VALUE='Formula 1:', UVALUE='NONE')
  v1  = ' Q = SUM_i ( Q[i] ) '
  F12 = widget_text(F1, VALUE=v1, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F13 = widget_label(F1, VALUE='[m^3 / s]', UVALUE='NONE')
;--------------------------------------------------------------------------
F2 = widget_base(A, /ROW, SPACE=ngap)
  F21 = widget_label(F2, VALUE='Formula 2:', UVALUE='NONE')
  v2  = 'Q[i] = K[i] * A[i] * Slope * Timestep'
  F22 = widget_text(F2, VALUE=v2, UVALUE='NONE', XSIZE=XS, /EDITABLE)
  F23 = widget_label(F2, VALUE='[m^3 / s]', UVALUE='NONE')
;--------------------------------------------------------------------------
FP = widget_label(A, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
N0 = widget_label(A, VALUE='NOTES: ', UVALUE='NONE', /ALIGN_LEFT)
;--------------------------------------------------------------------------
g1 = 'The ith layer is impermeable if K[i] = 0.'
N1 = widget_label(A, VALUE=g1, UVALUE='NONE', /ALIGN_LEFT)
;--------------------------------------------------------------------------
;g2 = 'Porosity values must be between 0 and 1.'
;N2 = widget_label(A, VALUE=g2, UVALUE='NONE', /ALIGN_LEFT)
;--------------------------------------------------------------------------
NP = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [F11, F21]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP  ;***, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Darcy_Law_Formulas', $
             XOFF=XOFF, TW=[F12, F22]

END;  GUI_Darcy_Law_Formulas
;*****************************************************************

