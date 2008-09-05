
;*****************************************************************
;   channel_grids.pro

;   Copyright (c) 2005, Scott D. Peckham
;   Created:  July-August 2005

;*****************************************************************

;   Power_Curve_3p       (curve fitting functions)
;   Power_Curve_4p
;   Best_Power_Curve_Fit

;   GUI_Make_Channel_Grids_by_Area_event
;   GUI_Make_Channel_Grids_by_Area
;   Make_Channel_Grids_by_Area

;   GUI_Make_Channel_Grids_by_Order_event
;   GUI_Make_Channel_Grids_by_Order
;   Make_Channel_Grids_by_Order

;   GUI_Define_Channels_event   ;(NOT HERE YET)
;   GUI_Define_Channels

;*****************************************************************
pro Power_Curve_3p, x, params, f, partials

;----------------------------------------------------
;Notes:  For use with IDL's CURVEFIT function.

;        y = c*(x + b)^p
;----------------------------------------------------
;NB!     This power-law form is equivalent to the
;        following form up to a change of variables:
;
;        y = c2*(b2*x + 1)^p2

;        However, b2=0 implies b=Infinity and vice
;        versa, so this form cannot represent the
;        important special case of b=0 & y[0]=0.

;        Given c, b and p, we have:
;        p2=p, b2=(1/b), c2=c*(b^p)

;        Given c2, b2 and p2, we have:
;        p=p2, b=(1/b2), c=c2*(b2^p2)

;        For df_dp, note that:
;        (x + b)^p = exp(p * ln(x + b))
;----------------------------------------------------
c = params[0]
b = params[1]
p = params[2]

f = c*(x + b)^p

;----------------------------
;Compute partial derivatives
;-------------------------------
;n_params() refers to number of
;arguments to this procedure.
;-------------------------------
if (n_params() ge 4) then begin
    df_dc = (x + b)^p
    df_db = c * p * (x + b)^(p-1d)
    df_dp = c * ((x + b)^p) * alog(x + b)
    partials = [[df_dc], [df_db], [df_dp]]
endif

end;  Power_Curve_3p
;*****************************************************************
pro Power_Curve_4p, x, params, f, partials

;----------------------------------------------------
;Notes:  For use with IDL's CURVEFIT function.

;        y = c*(x + b)^p + a

;NB!     See Notes for Power_Curve_3p above.
;----------------------------------------------------
c = params[0]
b = params[1]
p = params[2]
a = params[3]

f = (c*(x + b)^p) + a

;----------------------------
;Compute partial derivatives
;-------------------------------
;n_params() refers to number of
;arguments to this procedure.
;-------------------------------
if (n_params() ge 4) then begin
    df_dc = (x + b)^p
    df_db = c * p * (x + b)^(p-1d)
    df_dp = c * ((x + b)^p) * alog(x + b)
    nx    = n_elements(x)
    df_da = dblarr(nx) + 1d    
    partials = [[df_dc], [df_db], [df_dp], [df_da]]
endif

end;  Power_Curve_4p
;*****************************************************************
function Best_Power_Curve_Fit, x,y, params, REPORT=REPORT, $
                    ITMAX=itmax, TOL=tol, WEIGHTS=weights

;-----------------------------------------------------------
;Notes:  This function uses IDL's CURVEFIT function and the
;        function Power_Curve1 (above) to find the best-fit
;        parameters for fitting the data vectors x and y.

;        x and y can have as few as 3 unique points, but
;        must contain 4 elements each to avoid an error
;        from IDL.  The 3rd value can simply be repeated.

;        Initial guesses are required for all of the power
;        curve parameters (a,c,p) and the choice of these
;        has a big impact on whether CURVEFIT converges to
;        a solution.  Some experimenting is necessary but
;        the initial guess for p must be a large negative
;        number like -10 if p is expected to be negative
;        and a small position number like 0.001 if p is
;        expected to be positive ??


        
;        This function can be tested with testing data
;        as in this example:
;             x = findgen(40)
;             y = 0.1d * (x + 1d)^(-0.8d)
;             plot, x, y
;             yfit = Best_Power_Curve_Fit(x,y)
;             oplot, x, yfit, psym=-1
;        and with random perturbations added:
;             u = randomu(seed, 40d) / 50d
;             y2 = y + u
;        and seems to work well for the cases tested.

;        The array of flags, fitvars, determines which
;        parameters are fixed and which ones to find.
;-----------------------------------------------------------
REPORT = keyword_set(REPORT)

nx = n_elements(x)
if NOT(keyword_set(ITMAX))   then itmax = 300
if NOT(keyword_set(TOL))     then tol   = 1e-7
if NOT(keyword_set(WEIGHTS)) then weights = (dblarr(nx) + 1d)

;----------------------------------------
;Points used to generate initial guesses
;----------------------------------------
x0=x[0]  &  x1=x[1]  &  x2=x[nx-1]
y0=y[0]  &  y1=y[1]  &  y2=y[nx-1]

;-------------------------------
;Degenerate if ymin equals ymax
;-------------------------------
ymin = min(y, max=ymax, /NAN)
FLAT = (ymin eq ymax)

;---------------------------------------------
;Find p0 and c0 as if b0 were zero, and then
;set b0 to a very small, positive, number.
;This has very good convergence behavior, for
;both negative and positive values of p.
;Note that b0=0d has very poor convergence.
;---------------------------------------------
;Seems that if b0 is too big, then method
;"slides" away from the solution.  That is,
;if b0 needs to be small, it can't get there
;from a larger value.
;---------------------------------------------
;NB! We could also set c0=y2/(x2^p0) or
;c0=y1/(x1^p0), but form here uses both pts.
;---------------------------------------------
;Power-law form is:    f = c*(x + b)^p
;---------------------------------------------
if NOT(FLAT) then begin
    p0 = alog(y2/y1) / alog(x2/x1)
    c0 = (y2-y1) / ((x2^p0) - (x1^p0))
    b0 = 1e-8
endif else begin
    p0 = 0d
    b0 = 0d
    c0 = y0
endelse

;-----------------------------------------
;Use CURVEFIT to find best-fit parameters
;-----------------------------------------
if NOT(FLAT) then begin
    params  = [c0, b0, p0]
    fitvars = [1,1,1]
    yfit    = curvefit(x, y, weights, params, sigma, /DOUBLE, $ 
                       FUNCTION_NAME='Power_Curve_3p', $ 
                       TOL=tol, ITMAX=itmax, YERROR=stderr, $
                       FITA=fitvars, STATUS=status, ITER=iter)
endif else begin
    yfit   = y
    params = [c0,b0,p0]
    status = 0
    iter   = 0
    stderr = 0
endelse

REPORT = 1b
if NOT(REPORT) then RETURN, yfit

print,'-----------------------------------'
print,'General power-law fit to profile'
print,'-----------------------------------'
print,'f(x)   = [c * (x + b)^p]'
print,'c0     = ' + TF_String(c0)
print,'b0     = ' + TF_String(b0)
print,'p0     = ' + TF_String(p0)
print,'---------------------------------'
print,'c      = ' + TF_String(params[0])
print,'b      = ' + TF_String(params[1])
print,'p      = ' + TF_String(params[2])
print,'stderr = ' + TF_String(stderr)
print,' '



;------------------------------
;Print status of the curve fit
;------------------------------
case (status) of
    0 : begin
        print,'Curve fit was successful!'
        print,'Number of iterations = ' + TF_String(iter)
        end
    1 : begin
        print,'Curve fit failed. Chi square was '
        print,'increasing without bounds.'
        end
    2 : begin
        print,'Curve fit failed to converge after'
        print,TF_String(itmax) + ' iterations.'
        end
endcase
print,' '

RETURN, yfit

end;  Best_Power_Curve_Fit
;*****************************************************************
pro GUI_Make_Channel_Grids_by_Area_event, event

;-----------
;Error trap
;-----------
CATCH, status

Trace_Error, status, event, OK
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*****************
'UPDATE_BCP' : $
;*****************
begin
;---------------------------------------
;Read the parameter values from table 1
;so they can be updated.
;---------------------------------------
widget_control, state.table_ID, get_value=table

;--------------------------------------
;Read the measured values from table 2
;--------------------------------------
widget_control, state.table2_ID, get_value=table2
nr = state.nrows
A  = float([ transpose(table2[0,*]), table2[0,nr-1] ])

;-------------------------------------
;Find best-fit values for Manning's N
;and update values shown in table 1
;-------------------------------------
N    = float([ transpose(table2[1,*]), table2[1,nr-1] ])
Nfit = Best_Power_Curve_Fit(A, N, params)
table[0,1:3] = TF_String(params)

;-------------------------------------
;Find best-fit values for bed width
;and update values shown in table 1
;-------------------------------------
w    = float([ transpose(table2[2,*]), table2[2,nr-1] ])
Nfit = Best_Power_Curve_Fit(A, w, params)
table[1,1:3] = TF_String(params)
;-----------------------------------------------------------
;These were for another fitting function. (7/19/05)
;-----------------------------------------------------------
;w    = float([ transpose(table2[2,*]), table2[2,nr-1] ])
;wfit = Best_Special_Curve_Fit(A, w, params)
;table[1,1:3] = TF_String(params)
;------------------------------------------------
;Use until problem with above method is resolved
;------------------------------------------------
;w    = float([ transpose(table2[2,*]), table2[2,nr-1] ])
;p2   = table[1,3]
;b2   = (1d / A[1])
;c2   = w[2] / A[2]^p2
;table[1,1:3] = TF_String([c2, b2, p2])

;-------------------------------------
;Find best-fit values for bank angle
;and update values shown in table 1
;-------------------------------------
t    = float([ transpose(table2[3,*]), table2[3,nr-1] ])
tfit = Best_Power_Curve_Fit(A, t, params)
table[2,1:3] = TF_String(params)

;------------------------------
;Replace table with new values
;------------------------------
widget_control, state.table_ID, set_value=table
end

;*********
'OK' : $
;*********
begin
;----------------------------------
;Read information from upper table
;----------------------------------
widget_control, state.table_ID, get_value=table
formulas = table[*,0]    ;(string)
c_vals   = table[*,1]    ;(float)
b_vals   = table[*,2]    ;(float)
p_vals   = table[*,3]    ;(float)
files    = table[*,4]    ;(string)

;----------------------------------
;Read information from lower table
;All we need now is Ac.
;(May have been imported.)
;----------------------------------
widget_control, state.table2_ID, get_value=table2
A_crit = table2[0,1]
;*** print,'A_crit = ' + TF_String(A_crit)

;-----------------------
;Read name of area file
;-----------------------
Read_Text_Box, state.area_file_ID, area_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(area_file)
if NOT(OK) then RETURN

;-------------------------------------
;Check overwrite of the new filenames
;-------------------------------------
manning_file    = table[0,4]
bed_width_file  = table[1,4]
bank_angle_file = table[2,4]
;** z0_file        = table[3,4]
;** D50_file       = table[4,4]
;--------------------------------
Check_Overwrite, manning_file, OK
if NOT(OK) then RETURN
Check_Overwrite, bed_width_file, OK
if NOT(OK) then RETURN
Check_Overwrite, bank_angle_file, OK
if NOT(OK) then RETURN



;--------------------------------
;Get dx from leader's state vars
;--------------------------------
Get_TLB_State, state.leader_ID, mstate, ALIVE
if NOT(ALIVE) then RETURN

;------------------------------------
;Call routine that creates the grids
;------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Make_Channel_Grids_by_Area, area_file, table, mstate, A_crit
widget_control, event.ID, sensitive=1    ;(enable button)

;------------------------
;Upload values to leader
;------------------------
;Set any new values here
;------------------------
;widget_control, state.leader_ID, set_uvalue=mstate

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating new grids.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")

;*** Close_Dialog, event.top 
end

;*******************
'IMPORT_TABLE' : $
;*******************
begin
ncols = 4
Import_Table_From_File, state.table2_ID, ncols
end


;*****************
'SAVE_TABLE' : $
;*****************
Save_Table_To_File, state.table2_ID

;***********
'HELP' : $
;***********
Show_HTML_Help, 'channel_grids_area.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $
   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

end;  GUI_Make_Channel_Grids_by_Area_event
;*****************************************************************
pro GUI_Make_Channel_Grids_by_Area, leader

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
prefix     = mstate.run_vars.prefix
area_file  = prefix + '_area.rtg'

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, table_ID:0L, table2_ID:0L, $
area_file_ID:0L, ncols:0, nrows:0 }

ngap = 6

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE="Channel Property Grids by Area Dialog", $
            /COLUMN, LEADER=leader

B = widget_base(MB, /COLUMN, /FRAME)

;--------------------
;Heading for dialog 
;--------------------
;PP1 = widget_label(B, UVALUE='NONE', /ALIGN_LEFT, $
;             VALUE='Channel Properties Parameterized by Area:')
;PP2 = widget_label(B, VALUE=' ', UVALUE='NONE')

;-------------------
;Area grid filename 
;-------------------
C = widget_base(B, /ROW)
AG = widget_base(C, /ROW, SPACE=ngap)
  AG1 = widget_label(AG, VALUE='Area grid filename: ', UVALUE='NONE')
  AG2 = widget_text(AG, VALUE=area_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=20)
  AG3 = widget_label(AG, VALUE='[km^2] ', UVALUE='NONE')
  AG4 = widget_label(AG, VALUE='  (used to parameterize N, w and t)', $
                     UVALUE='NONE')
  state.area_file_ID = AG2
;-----------------------------------------------------------
;AT = widget_base(B, /ROW, SPACE=ngap)
;  AT1 = widget_label(AT, VALUE=' Area threshold (A0): ',$
;                     UVALUE='NONE')
;  AT2 = widget_text(AT, VALUE='10.0', UVALUE='NONE', $
;                    /EDITABLE, XSIZE=8)
;  AT3 = widget_label(AT, VALUE='[km^2]', UVALUE='NONE')
;  state.area_threshold_ID = AT2

G2 = widget_label(B, VALUE=' ', UVALUE='NONE')


;---------------------------
;Get settings for the table
;---------------------------
n_props    = 3
n_rows     = 5
table      = strarr(n_props, n_rows)
aligns     = intarr(n_props, n_rows) + 1  ;(centered)
;** aligns[*,0]          = 0  ;(left justify)
aligns[*,n_rows-1,*] = 0  ;(left justify)
;----------------------------------------------------------------
formulas = [' N = c (A + b)^p ', $
            ' w = c (A + b)^p ', $
            ' t = c (A + b)^p ']
format0  = '(F14.8)'
c_vals   = string([0.0242, 2.96, 45.0],  format=format0)
b_vals   = string([0.000298, 0.0, 0.0],  format=format0)
p_vals   = string([-0.31, 0.497, 0.0], format=format0)
files    = prefix + ['_chan-n.rtg', '_chan-w.rtg', '_chan-a.rtg']
;-----------------------------------------------------------------
table[*,0] = formulas
table[*,1] = c_vals
table[*,2] = b_vals
table[*,3] = p_vals
table[*,4] = files
;-----------------------------------------------------------------
rlabels  = ['Formula: ', 'c: ', 'b: ', 'p: ', 'Filename: ']
clabels  = [" Manning's N ", " Bed width [m] ", " Bank angle [deg] "]
cwidths  = [1.33, 1.33, 1.33]

;----------------------------
;Table widget for parameters
;----------------------------
WT = widget_table(B, VALUE=table, UVALUE='TABLE', ALIGNMENT=aligns, $
                  COLUMN_LABELS=clabels, ROW_LABELS=rlabels, $
                  COLUMN_WIDTHS=cwidths, UNITS=1, $
                  /RESIZEABLE_COLUMNS, /EDITABLE )
                  ;*** X_SCROLL_SIZE=3)
state.table_ID = WT

;-------------------------------------------
;Middle strip with update parameters button
;-------------------------------------------
P1  = widget_label(B, VALUE=' ', UVALUE='NONE')
str = 'Measured values to constrain c, b and p:'
P2  = widget_label(B, VALUE=str, UVALUE='NONE', /ALIGN_LEFT)
;;P2 = widget_base(B, /ROW, SPACE=16) 
;;  P21 = widget_label(P2, VALUE=str, UVALUE='NONE', /ALIGN_LEFT)
;;  P22 = widget_button(P2, VALUE='Update c, b and p', UVALUE='UPDATE_BC')
P3  = widget_label(B, VALUE=' ', UVALUE='NONE')

;---------------------------
;Get settings for the table
;---------------------------
ncols = 4
nrows = 3
state.ncols = ncols
state.nrows = nrows
;--------------------------------
table2  = fltarr(ncols, nrows)
aligns2 = intarr(ncols, nrows) + 1    ;(centered)
;** aligns2[*,0]          = 0         ;(left justify)
;** aligns2[*,nrows-1,*] = 0          ;(left justify)
;----------------------------
A_vals = [0.0, 0.01, 0.5]
n_vals = [0.3, 0.1,  0.03]
w_vals = [0.0, 0.3, 2.1]
t_vals = [45.0, 45.0, 45.0]
;----------------------------
table2[0,*] = A_vals 
table2[1,*] = n_vals

table2[2,*] = w_vals
table2[3,*] = t_vals
;----------------------------------
rlabels2 = ['Overland', 'Transition', 'Channel']
clabels2 = ['Area [km^2]', 'Manning N', 'Width [m]', 'Angle [deg]']
cwidths2 = [1.0, 1.0, 1.0, 1.0]

;---------------------------------
;Table widget for measured values
;---------------------------------
WT2 = widget_table(B, VALUE=table2, UVALUE='NONE', ALIGNMENT=aligns2, $
                   COLUMN_LABELS=clabels2, ROW_LABELS=rlabels2, $
                   COLUMN_WIDTHS=cwidths2, UNITS=1, $
                   /RESIZEABLE_COLUMNS, /EDITABLE)
                   ;*** X_SCROLL_SIZE=3)
state.table2_ID = WT2
PP3 = widget_label(B, VALUE=' ', UVALUE='NONE')

;-----------------------


;Read table from file ?
;-----------------------
IT = widget_base(B, /ROW, SPACE=ngap, /ALIGN_CENTER)
  IT0 = widget_button(IT, VALUE='Update c, b and p', $
                      UVALUE='UPDATE_BCP')
  IT1 = widget_button(IT, VALUE='Read table from file...', $
                      UVALUE='IMPORT_TABLE')
  IT2 = widget_button(IT, VALUE='Save table to file...', $
                      UVALUE='SAVE_TABLE')

PP4 = widget_label(B, VALUE=' ', UVALUE='NONE')

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Make_Channel_Grids_by_Area', XOFF=480

end;  GUI_Make_Channel_Grids_by_Area
;*****************************************************************
pro Make_Channel_Grids_by_Area, area_file, table, mstate, Ac

;------------------------------------------
;Get information needed for leader's state
;------------------------------------------

RTI_file = mstate.grid_vars.RTI_file
;*** Get_Pixel_Sizes, dx,dy,dd,da, RTI_file, /METERS

;----------------------
;Read the D8 flow grid
;-----------------------------------
;Need to add this to the dialog ??? ********************
;-----------------------------------
;** TF_Print,'Reading D8 flow grid...'
Read_Grid, grid, mstate.channel_vars.code_file, TYPE='BYTE'

;-------------------------------------------
;Compute overland flow widths (pixel-based)
;-------------------------------------------
;** TF_Print,'Computing flow width grid...'
dw = Flow_Widths(grid, RTI_file, /METERS, /DOUBLE)   ;(2D array)
;** dw_min = min(dw, max=dw_max)
;** TF_Print,'    min(dw) = ' + TF_String(dw_min)
;** TF_Print,'    max(dw) = ' + TF_String(dw_max)

;---------------------------------------

;Read the area grid (honors byte order)
;---------------------------------------
Read_Grid, A, area_file, TYPE='FLOAT', /SILENT

;------------------------------
;Extract values from the table
;------------------------------
c_vals   = float(table[*,1])
b_vals   = float(table[*,2])
p_vals   = float(table[*,3])
;------------------------------
manning_file    = table[0,4]
bed_width_file  = table[1,4]
bank_angle_file = table[2,4]

;----------------------------------
;These are the formulas to be used
;----------------------------------
;formulas = [' N = c (A + b)^p ', $
;            ' w = c (A + b)^p', $
;            ' a = c (A + b)^p ']

;----------------------------
;Create the Manning's N grid
;----------------------------
c = c_vals[0]
b = b_vals[0]
p = p_vals[0]
N = c * (A + b)^p
;--------------------
Write_Grid, N, manning_file, TYPE='FLOAT', $
            RTI_FILE=RTI_file, /SILENT

;--------------------------
;Create the bed width grid
;----------------------------------
;This one is special, as explained
;in the Help button help message.
;----------------------------------
;w will typically be less than dw.
;----------------------------------
c = c_vals[1]
b = b_vals[1]
p = p_vals[1]
w = c * (A + b)^p
;----------------------------------
;Replace values of w where A lt Ac
;----------------------------------
hillslopes = where(A le Ac, nh)
if (nh ne 0) then w[hillslopes] = dw[hillslopes]
;-------------------------------------------------
Write_Grid, w, bed_width_file, TYPE='FLOAT', $
            RTI_FILE=RTI_file, /SILENT

;---------------------------
;Create the bank angle grid
;---------------------------
c = c_vals[2]
b = b_vals[2]
p = p_vals[2]
t = c * (A + b)^p
;-------------------
Write_Grid, t, bank_angle_file, TYPE='FLOAT', $
            RTI_FILE=RTI_file, /SILENT

end;  Make_Channel_Grids_by_Area
;*****************************************************************
pro GUI_Make_Channel_Grids_by_Order_event, event

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
;Read information from table
;(May have been imported.)
;----------------------------
widget_control, state.table_ID, get_value=table

;-------------------
;Read the filenames

;-------------------
Read_Text_Box, state.order_file_ID, order_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(order_file)
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.manning_file_ID, manning_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, manning_file, OK
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.width_file_ID, width_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, width_file, OK
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.angle_file_ID, angle_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, angle_file, OK
if NOT(OK) then RETURN

;------------------------------------
;Call routine that creates the grids
;------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Make_Channel_Grids_by_Order, order_file, table, $
             manning_file, width_file, angle_file

widget_control, event.ID, sensitive=1    ;(enable button)

;------------------------
;Upload values to leader
;------------------------
;Get_TLB_State, state.leader_ID, mstate, ALIVE
;if NOT(ALIVE) then RETURN
;------------------------
;Set any new values here
;------------------------
;widget_control, state.leader_ID, set_uvalue=mstate

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating new grids.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")
end

;*******************
'IMPORT_TABLE' : $
;*******************
begin
ncols = 3
Import_Table_From_File, state.table_ID, ncols
end

;*****************
'SAVE_TABLE' : $
;*****************
Save_Table_To_File, state.table_ID

;***********
'HELP' : $
;***********
Show_HTML_Help, 'channel_grids_order.htm'

;************
'CANCEL' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CANCEL') AND $

   (uvalue ne 'OK') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_Make_Channel_Grids_by_Order_event
;*****************************************************************
pro GUI_Make_Channel_Grids_by_Order, leader

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
prefix       = mstate.run_vars.prefix

order_file   = prefix + '_order.rtg'
manning_file = prefix + '_chan-n-ord.rtg'
width_file   = prefix + '_chan-w-ord.rtg'
angle_file   = prefix + '_chan-a-ord.rtg'

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, table_ID:0L, $
order_file_ID:0L, manning_file_ID:0L, $
width_file_ID:0L, angle_file_ID:0L} 

;----------------
;Initialize vars
;----------------
nvals_by_order  = dblarr(15)
z0vals_by_order = dblarr(15) + 0.01d   ;******
widths_by_order = dblarr(15)
angles_by_order = dblarr(15) + 45d
nvals_by_order[0:8]  = [0.3, 0.2, 0.1, 0.05, 0.03, 0.03, 0.03, 0.03, 0.03]
widths_by_order[0:8] = [30, 20, 10, 1, 2, 4, 8, 16, 32]

ngap = 6
fsize = 20
X1 = 10
XS = 36

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE=' Channel Property Grids by Order Dialog ', $
               /COLUMN, LEADER=leader
B = widget_base(MB, /COLUMN, /FRAME)
PP1 = widget_label(B, UVALUE='NONE', /ALIGN_LEFT, $ 
             VALUE='Channel Properties Parameterized by Strahler Order:  ')

PP2 = widget_label(B, VALUE=' ', UVALUE='NONE')


;----------------------
;Get all the filenames 
;----------------------
C = widget_base(B, /COLUMN)
OG = widget_base(C, /ROW, SPACE=ngap)
  OG1 = widget_label(OG, VALUE='HS order grid filename: ', $
                     UVALUE='NONE')
  OG2 = widget_text(OG, VALUE=order_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=20)
  OG3 = widget_label(OG, VALUE='[none]', UVALUE='NONE')
  state.order_file_ID = OG2
;------------------------------------------------------------------

MG = widget_base(C, /ROW, SPACE=ngap)
  MG1 = widget_label(MG, VALUE="New Manning's n grid filename: ", $

                     UVALUE='NONE')
  MG2 = widget_text(MG, VALUE=manning_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=20)
  MG3 = widget_label(MG, VALUE='[none]', UVALUE='NONE')
  state.manning_file_ID = MG2
;------------------------------------------------------------------
WG = widget_base(C, /ROW, SPACE=ngap)
  WG1 = widget_label(WG, VALUE='New bed width grid filename: ', $
                     UVALUE='NONE')

  WG2 = widget_text(WG, VALUE=width_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=20)
  WG3 = widget_label(WG, VALUE='[meters]', UVALUE='NONE')
  state.width_file_ID = WG2
;------------------------------------------------------------------
AG = widget_base(C, /ROW, SPACE=ngap)
  AG1 = widget_label(AG, VALUE='New bank angle grid filename: ', $
                     UVALUE='NONE')
  AG2 = widget_text(AG, VALUE=angle_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=20)
  AG3 = widget_label(AG, VALUE='[degrees]', UVALUE='NONE')
  state.angle_file_ID = AG2
;------------------------------------------------------------------
PP1 = widget_label(C, VALUE=' ', UVALUE='NONE')


Align_Text_Boxes, [OG1, MG1, WG1, AG1]

;-------------
;Table widget
;-------------
omax = 15
vals      = fltarr(3,omax)
vals[0,*] = nvals_by_order
vals[1,*] = widths_by_order
vals[2,*] = angles_by_order
;----------------------------

rlabels = sindgen(omax + 1)
rlabels = strtrim(rlabels[1:omax], 2)
rlabels = 'Order ' + rlabels

clabels = ["Manning's N: ", 'Bed width [m]: ', 'Bank angle [deg]']
cwidths = [1.0, 1.0, 1.1]
WT = widget_table(B, VALUE=vals, UVALUE='TABLE', ALIGNMENT=1, $
                  COLUMN_LABELS=clabels, ROW_LABELS=rlabels, $
                  COLUMN_WIDTHS=cwidths, UNITS=1, $
                  /RESIZEABLE_COLUMNS, /EDITABLE, $
                  Y_SCROLL_SIZE=8)
                  ;*** X_SCROLL_SIZE=3)
state.table_ID = WT
PP3 = widget_label(B, VALUE=' ', UVALUE='NONE')

;-----------------------
;Read table from file ?
;-----------------------
IT = widget_base(B, /ROW, SPACE=ngap, /ALIGN_CENTER)
  IT1 = widget_button(IT, VALUE='Read table from file...', $
                      UVALUE='IMPORT_TABLE')
  IT2 = widget_button(IT, VALUE='Save table to file...', UVALUE='SAVE_TABLE')

L1 = widget_label(B, VALUE=' ', UVALUE='NONE')

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, MB, /OK, /HELP, /CANCEL

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
Realize_TLB, MB, state, 'GUI_Make_Channel_Grids_by_Order', XOFF=480

END;  GUI_Make_Channel_Grids_by_Order
;*****************************************************************
pro Make_Channel_Grids_by_Order, order_file, table, $
                 manning_file, width_file, angle_file

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;----------------------
;Get values from table
;----------------------
nvals_by_order  = table[0,*]
widths_by_order = table[1,*]
angles_by_order = table[2,*]
;----------------------------
;z0vals_by_order = table[3,*]

;---------------------------
;Read the grid of HS orders
;---------------------------
Read_Grid, order_grid, order_file, TYPE='BYTE', /SILENT

;---------------------------
;Create the grids as "maps"
;---------------------------
indices = (order_grid - 1)
n_grid  = nvals_by_order[indices]

w_grid  = widths_by_order[indices]
a_grid  = angles_by_order[indices]
;--------------------------------------
;z0_grid = z0vals_by_order[indices]    ;********************

;----------------------------------
;Save the Manning's n grid to file
;----------------------------------
Write_Grid, n_grid, manning_file, TYPE='FLOAT', /SILENT

;--------------------------------
;Save the bed width grid to file
;--------------------------------
Write_Grid, w_grid, width_file, TYPE='FLOAT', /SILENT

;---------------------------------
;Save the bank angle grid to file
;---------------------------------
Write_Grid, a_grid, angle_file, TYPE='FLOAT', /SILENT

END;  Make_Channel_Grids_by_Order
;*****************************************************************

