
;**********************************************************************
;   diversions.pro

;   Copyright (c) 2005-2007, Scott D. Peckham 
;   Created:   Feb 2004
;   Modified:  July 2005 (Added GUI.)

;**********************************************************************

;   Read_Source_Data   ;(read from prefix_sources.txt)
;   Read_Sink_Data     ;(read from prefix_sinks.txt)
;   Read_Canal_Data    ;(read from prefix_canals.txt)

;   Update_Sources     ;(call in route.pro near Update_Flow_Volume)
;   Update_Sinks
;   Update_Canals

;   GUI_Diversions_event  ;(7/20/05)
;   GUI_Diversions

;**********************************************************************
pro Read_Source_Data, source_file, source_IDs, dur_sum_sources, $
                      Q_sources, OK   ;**** nd_vals

;----------------------------------------------------------------
;Notes:  This assumes that source_file is organized as follows:
;            ID         (source pixel ID as long integer)
;            nd         (number of durations and Q values)
;            durations  (vector of durations in minutes)
;            Q_sources  (vector of discharges in m^3/sec)
;----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

OK = 1b

;---------------------------
;Can source_file be found ?
;---------------------------
f = findfile(source_file, count=c)
if (c eq 0) then begin
    msg = [ $
    'ERROR: source file not found. ', ' ', $
    'The file: ',$
    '  ' + source_file, $
    'was not found in the working directory. ',$
    ' ']
    result = GUI_Message(msg, /INFO)
    OK = 0b
    RETURN
endif

;---------------------------
;Count lines in source_file
;---------------------------
;Count_Lines, n_lines, source_file, /SILENT
;n_IDs = (n_lines / 4L)

;----------------------------------
;Read the lines with the nd values
;and find the max, called nd_max
;----------------------------------
nd     = 0L
nd_max = 0L
n_IDs  = 0L  ;******
line1  = ''
line3  = ''
line4  = ''
;-----------------------------
TF_Get_LUN, unit, source_file
openr, unit, source_file
while NOT(EOF(unit)) do begin
    readf, unit, line1
    readf, unit, nd
    readf, unit, line3
    readf, unit, line4
    ;----------------------
    nd_max = (nd_max > nd)
    n_IDs  = (n_IDs + 1L)   ;***
endwhile
free_lun, unit

;------------------
;Initialize arrays
;------------------
source_IDs  = lonarr(n_IDs)
nd_vals     = lonarr(n_IDs)   ;*****
dur_sources = fltarr(n_IDs, nd_max)
Q_sources   = fltarr(n_IDs, nd_max)

;------------------
;Open file to read
;------------------
TF_Get_LUN, unit, source_file
openr, unit, source_file

;--------------------
;Read data from file
;--------------------
ID = 0L
nd = 0L
k  = 0L
while NOT(EOF(unit)) do begin
    readf, unit, ID
    source_IDs[k] = ID
    ;-------------------
    readf, unit, nd
    nd_vals[k] = nd
    ;--------------------------------
    durs = fltarr(nd)
    readf, unit, durs
    dur_sources[k,0L:(nd-1L)] = durs
    ;--------------------------------
    Q = fltarr(nd)
    readf, unit, Q
    Q_sources[k,0L:(nd-1L)] = Q
    ;--------------------------------
    k = (k + 1L)
endwhile

;---------------------
;Close the input file
;---------------------
free_lun, unit

;----------------------------------
;Compute partial sums of durations
;----------------------------------
dur_sum_sources = fltarr(n_IDs, nd_max + 1L)
for k=0L,(n_IDs-1L) do begin
    for i=0L,(nd_max - 1L) do begin
        dur_sum_sources[k,i+1] = dur_sum_sources[k,i] + $
                                 dur_sources[k,i]
    endfor
endfor
dur_sum_sources = dur_sum_sources[*,1:nd_max]  ;(remove leading zero)

end;  Read_Source_Data
;**********************************************************************
pro Read_Sink_Data, sink_file, sink_IDs, dur_sum_sinks, $
                    Q_sinks, OK  ;*** nd_vals


;----------------------------------------------------------------
;Notes:  This assumes that sink_file is organized as follows:
;            ID         (sink pixel ID as long integer)
;            nd         (number of durations and Q values)
;            durations  (vector of durations in minutes)
;            Q_sinks    (vector of discharges in m^3/sec)
;----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

OK = 1b

;-------------------------
;Can sink_file be found ?
;-------------------------
f = findfile(sink_file, count=c)
if (c eq 0) then begin
    msg = [ $
    'ERROR: sink file not found. ', ' ', $
    'The file: ',$
    '  ' + sink_file, $
    'was not found in the working directory. ',$
    ' ']
    result = GUI_Message(msg, /INFO)
    OK = 0b
    RETURN
endif

;---------------------------
;Count lines in source_file
;---------------------------
;Count_Lines, n_lines, sink_file, /SILENT
;n_IDs = (n_lines / 4L)

;----------------------------------
;Read the lines with the nd values
;and find the max, called nd_max
;----------------------------------
nd     = 0L
nd_max = 0L
n_IDs  = 0L  ;******
line1  = ''
line3  = ''
line4  = ''
;---------------------------
TF_Get_LUN, unit, sink_file
openr, unit, sink_file
while NOT(EOF(unit)) do begin
    readf, unit, line1
    readf, unit, nd
    readf, unit, line3
    readf, unit, line4
    ;----------------------
    nd_max = (nd_max > nd)
    n_IDs  = (n_IDs + 1L)   ;***
endwhile
free_lun, unit

;------------------
;Initialize arrays
;------------------
sink_IDs  = lonarr(n_IDs)
nd_vals   = lonarr(n_IDs)   ;*****
dur_sinks = fltarr(n_IDs, nd_max)
Q_sinks   = fltarr(n_IDs, nd_max)

;------------------
;Open file to read
;------------------
TF_Get_LUN, unit, sink_file
openr, unit, sink_file 

;--------------------
;Read data from file
;--------------------
ID = 0L
nd = 0L
k  = 0L
while NOT(EOF(unit)) do begin
    readf, unit, ID
    sink_IDs[k] = ID
    ;-------------------
    readf, unit, nd
    nd_vals[k] = nd
    ;--------------------------------
    durs = fltarr(nd)
    readf, unit, durs
    dur_sinks[k,0L:(nd-1L)] = durs
    ;--------------------------------
    Q = fltarr(nd)
    readf, unit, Q
    Q_sinks[k,0L:(nd-1L)] = Q
    ;--------------------------------
    k = (k + 1L)
endwhile

;---------------------
;Close the input file
;---------------------
free_lun, unit

;----------------------------------
;Compute partial sums of durations
;----------------------------------
dur_sum_sinks = fltarr(n_IDs, nd_max + 1L)
for k=0L,(n_IDs-1L) do begin
    for i=0L,(nd_max - 1L) do begin
        dur_sum_sinks[k,i+1] = dur_sum_sinks[k,i] + $
                               dur_sinks[k,i]
    endfor
endfor
dur_sum_sinks = dur_sum_sinks[*,1:nd_max]  ;(remove leading zero)

end;  Read_Sink_Data
;**********************************************************************
pro Read_Canal_Data, canal_file, canal_in_IDs, canal_out_IDs, $
                     dur_sum_canals_in, Q_canals_in, $
                     canal_t_vals, OK  ;***** SILENT=SILENT
                     ;**** ,nd_vals

;------------------------------------------------------------------
;Note:  Q_canals is same at upstream and downstream ends, but the
;       downstream end lags the upstream end by the travel time
;       from in_ID to out_ID.  As a result, the duration and Q
;       vector for the downstream end are computed from those of
;       the upstream end, and the travel time, td, as:
;           Q_out   = [0,  Q_in]
;           dur_out = [td, dur_in]
;           dur_sum_out = [0, dur_sum_in] + td

;       Rather than create the dur_sum_canals_out and
;       Q_canals_out vectors, can construct them in Update_Canals.
;------------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

OK = 1b

;--------------------------
;Can canal_file be found ?
;--------------------------
f = findfile(canal_file, count=c)
if (c eq 0) then begin
    msg = [ $
    'ERROR: canal file not found. ', ' ', $
    'The file: ',$
    '  ' + canal_file, $
    'was not found in the working directory. ',$
    ' ']
    result = GUI_Message(msg, /INFO)
    OK = 0b
    RETURN
endif

;---------------------------
;Count lines in source_file
;---------------------------
;Count_Lines, n_lines, canal_file, /SILENT
;n_IDs = (n_lines / 6L)

;----------------------------------
;Read the lines with the nd values
;and find the max, called nd_max
;----------------------------------
nd     = 0L
nd_max = 0L
n_IDs  = 0L  ;******
line1  = ''
line2  = ''
line3  = ''
line5  = ''
line6  = ''
;-----------------------------
TF_Get_LUN, unit, canal_file
openr, unit, canal_file 
while NOT(EOF(unit)) do begin
    readf, unit, line1
    readf, unit, line2
    readf, unit, line3
    readf, unit, nd
    readf, unit, line5
    readf, unit, line6
    ;----------------------
    nd_max = (nd_max > nd)
    n_IDs  = (n_IDs + 1L)   ;***
endwhile
free_lun, unit

;------------------
;Initialize arrays
;------------------
canal_in_IDs  = lonarr(n_IDs)
canal_out_IDs = lonarr(n_IDs)
canal_t_vals  = lonarr(n_IDs)
nd_vals       = lonarr(n_IDs)   ;*****
dur_canals    = fltarr(n_IDs, nd_max)
Q_canals_in   = fltarr(n_IDs, nd_max)

;------------------
;Open file to read
;------------------
TF_Get_LUN, unit, canal_file
openr, unit, canal_file 

;--------------------
;Read data from file
;--------------------
ID = 0L
nd = 0L
k  = 0L
tval = 0.0
while NOT(EOF(unit)) do begin
    readf, unit, ID
    canal_in_IDs[k] = ID
    ;---------------------
    readf, unit, ID
    canal_out_IDs[k] = ID
    ;---------------------
    readf, unit, tval
    canal_t_vals[k] = tval
    ;-------------------------------
    readf, unit, nd
    nd_vals[k] = nd
    ;-------------------------------
    durs = fltarr(nd)
    readf, unit, durs
    dur_canals[k,0L:(nd-1L)] = durs
    ;-------------------------------
    Q = fltarr(nd)
    readf, unit, Q
    Q_canals_in[k,0L:(nd-1L)] = Q
    ;-------------------------------
    k = (k + 1L)
endwhile

;---------------------
;Close the input file
;---------------------
free_lun, unit

;----------------------------------
;Compute partial sums of durations
;----------------------------------
dur_sum_canals_in = fltarr(n_IDs, nd_max + 1L)
for k=0L,(n_IDs-1L) do begin
    for i=0L,(nd_max - 1L) do begin
        dur_sum_canals_in[k,i+1] = dur_sum_canals_in[k,i] + $
                                   dur_canals[k,i]
    endfor
endfor
dur_sum_canals_in = dur_sum_canals_in[*,1:nd_max]  ;(remove leading 0)

;----------------------------------------------
;Compute Q_canals_out (Q[t] at downstream end)
;See Notes;  now computed in Update_Canals
;----------------------------------------------
;*** Q_canals_out = 
;*** dum_sum_canals_out = 

end;  Read_Canal_Data
;**********************************************************************
pro Update_Sources, vol, time, dt, source_IDs, dur_sum_sources, $
                    Q_sources

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

n = n_elements(source_IDs)

for k = 0L, (n-1L) do begin
    w = where(time lt dur_sum_sources[k,*], nw)
    if (nw ne 0) then begin
        dvol = (dt * Q_sources[k, w[0]])
        vol[source_IDs[k]] = vol[source_IDs[k]] + dvol
    endif
endfor

end;  Update_Sources
;*******************************************************************
pro Update_Sinks, vol, time, dt, sink_IDs, dur_sum_sinks, Q_sinks

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

n = n_elements(sink_IDs)

for k = 0L, (n-1L) do begin
    w = where(time lt dur_sum_sinks[k,*], nw)
    if (nw ne 0) then begin
        dvol = (dt * Q_sinks[k, w[0]])
        vol[sink_IDs[k]] = (vol[sink_IDs[k]] - dvol) > 0.0
    endif
endfor

end;  Update_Sinks
;*******************************************************************
pro Update_Canals, vol, time, dt, canal_in_IDs, canal_out_IDs, $
                   dur_sum_canals_in, Q_canals_in, t_canals

;-----------------------------------------------------------------
;Note:  Q_canals is same at upstream and downstream ends, but the
;       downstream end lags the upstream end by the travel time
;       from in_ID to out_ID.  As a result, the duration and Q
;       vector for the downstream end are computed from those of
;       the upstream end, and the travel time, td, as:
;           Q_out   = [0,  Q_in]
;           dur_out = [td, dur_in]
;           dur_sum_out = [0, dur_sum_in] + td
;-----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

n = n_elements(canal_in_IDs)

;------------------------------
;Process upstream IDs as sinks
;------------------------------
for k = 0L, (n-1L) do begin
    w = where(time lt dur_sum_canals_in[k,*], nw)
    if (nw ne 0) then begin
        dvol = (dt * Q_canals_in[k, w[0]])
        vol[canal_in_IDs[k]] = (vol[canal_in_IDs[k]] - dvol) > 0.0
    endif
endfor

;----------------------------------
;Process downstream IDs as sources
;Must account for time lag in Q.
;----------------------------------
for k = 0L, (n-1L) do begin
    ;------------------------------------------
    ;Compute Q_canals_out & dur_sum_canals_out
    ;------------------------------------------
    dur_sum_canals_out = [0.0, dur_sum_canals_in[k,*]] + t_canals[k]
    Q_canals_out = [0.0, Q_canals_in[k,*]]
    ;------------------------------------------
    w = where(time lt dur_sum_canals_out, nw)
    if (nw ne 0) then begin
        dvol = (dt * Q_canals_out[w[0]])
        vol[canal_out_IDs[k]] = vol[canal_out_IDs[k]] + dvol
    endif
endfor

end;  Update_Canals
;*******************************************************************
pro GUI_Diversions_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;**************
'SOURCES' : $
;**************
state.sources = (1b - state.sources)

;************
'SINKS' : $
;************
state.sinks = (1b - state.sinks)

;*************
'CANALS' : $
;*************
state.canals = (1b - state.canals)

;*********
'OK' : $
;*********
begin
Read_Text_Box, state.source_file_ID, $
               source_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(source_file)
if NOT(OK) then RETURN
;-------------------------------------------------------
Read_Text_Box, state.sink_file_ID, $
               sink_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(sink_file)
if NOT(OK) then RETURN
;-------------------------------------------------------
Read_Text_Box, state.canal_file_ID, $
               canal_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(canal_file)
if NOT(OK) then RETURN

;------------------------
;Upload values to leader
;------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN
;--------------------------------------------------
mstate.diversion_vars.use_sources = state.sources
mstate.diversion_vars.use_sinks   = state.sinks
mstate.diversion_vars.use_canals  = state.canals
;--------------------------------------------------
mstate.diversion_vars.source_file = source_file
mstate.diversion_vars.sink_file   = sink_file
mstate.diversion_vars.canal_file  = canal_file
;--------------------------------------------------
widget_control, state.leader_ID, set_uvalue=mstate
Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'diversions.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Diversions_event
;*****************************************************************
pro GUI_Diversions, leader

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
;-------------------------------
source_file = mstate.diversion_vars.source_file
sink_file   = mstate.diversion_vars.sink_file
canal_file  = mstate.diversion_vars.canal_file

;------------------------------------
;Structure to store selected options
;------------------------------------
state = {leader_ID:leader, $
source_file_ID:0L, sources: 0b, $
sink_file_ID:0L,   sinks:   0b, $
canal_file_ID:0L,  canals:  0b }
ngap  = 6
fsize = 20

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Variables for Flow Diversions', $
               /COLUMN, LEADER=leader 
A = widget_base(MB, /COLUMN, /FRAME)

A0 = widget_label(A, VALUE='Flow Diversion Information: ', $
                  UVALUE='NONE', /ALIGN_LEFT)
A1 = widget_label(A, VALUE=' ', UVALUE='NONE')

;------------------------------
;Get the filenames and choices
;------------------------------
SO = widget_base(A, /ROW, SPACE=ngap)
  SO1 = widget_base(SO, /ROW, SPACE=ngap)
    SO11 = widget_label(SO1, VALUE='Sources filename: ')
    SO12 = widget_text(SO1, VALUE=source_file, UVALUE='NONE', $
                       /EDITABLE, XSIZE=fsize)
    SO13 = widget_label(SO1, VALUE=' ')
    state.source_file_ID = SO12
  SO2 = widget_base(SO, /ROW, /NONEXCLUSIVE, SPACE=ngap)
    SO21 = widget_button(SO2, VALUE='Use sources?', UVALUE='SOURCES')
;-------------------------------------------------------------------
SI = widget_base(A, /ROW, SPACE=ngap)
  SI1 = widget_base(SI, /ROW, SPACE=ngap)
    SI11 = widget_label(SI1, VALUE='Sinks filename: ')
    SI12 = widget_text(SI1, VALUE=sink_file, UVALUE='NONE', $
                      /EDITABLE, XSIZE=fsize)
    SI13 = widget_label(SI1, VALUE=' ')
    state.sink_file_ID = SI12
  SI2 = widget_base(SI, /ROW, /NONEXCLUSIVE, SPACE=ngap)
    SI21 = widget_button(SI2, VALUE='Use sinks?', UVALUE='SINKS')
;-------------------------------------------------------------------
CA = widget_base(A, /ROW, SPACE=ngap)
  CA1 = widget_base(CA, /ROW, SPACE=ngap)
    CA11 = widget_label(CA1, VALUE='Canals filename: ')
    CA12 = widget_text(CA1, VALUE=canal_file, UVALUE='NONE', $
                      /EDITABLE, XSIZE=fsize)
    CA13 = widget_label(CA1, VALUE=' ')
    state.canal_file_ID = CA12
  CA2 = widget_base(CA, /ROW, /NONEXCLUSIVE, SPACE=ngap)
    CA21 = widget_button(CA2, VALUE='Use canals?', UVALUE='CANALS')
;-------------------------------------------------------------------
Align_Text_Boxes, [SO11, SI11, CA11]

P0 = widget_label(A, VALUE=' ')

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL 

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Diversions', XOFF=XOFF

END;  GUI_Diversions 
;*****************************************************************


