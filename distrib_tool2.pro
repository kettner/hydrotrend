
;***************************************************************
;   distrib_tool2.pro

;   Copyright (c) 2005, Scott D. Peckham
;   Created:  8/2/05, starting from Bob Bolton's distrib_tool.

;***************************************************************

;   Make_RTS_via_IDM     ;(Inverse Distance Method)

;   GUI_Make_RTS_via_IDM_event
;   GUI_Make_RTS_via_IDM

;***************************************************************
pro Make_RTS_via_IDM, MET_file, RTI_file, RTS_file, $
                      N_HEADER=n_header, P=P, $
                      ASCII_FILE=ASCII_file, SHOW=SHOW, $
                      MSG_BOX_ID=MSG_BOX_ID

;-------------------------------------------------------------
;NOTES:  The purpose of this preprocessing tool is to read a
;        time series of meteorological data for multiple
;        stations from a multi-column text file (*.dat) and
;        then to use the Inverse Distance Method (IDM) to 
;        create a grid sequence of spatially-distributed
;        values (in RTS format) for use by TopoFlow.

;        This is a modified version of a similar tool
;        that was developed by Bob Bolton, for inclusion in
;        TopoFlow's graphical interface.

;        For the IDM method there must be > 3 data points
;        for each station (n_series gt 3) ??

;        Setting the ASCII_FILE keyword causes the grid
;        sequence to also be written to a text file (ASCII)
;        so that other computer programs can read the data
;        (e.g. SURFER, EXCEL, etc).

;        The input file called met_file is now expected to
;        contain the station x and y coordinates as the first
;        two values in the column of data values for a given
;        met station.  The number of stations is determined
;        automatically by counting lines in met_file.

;        The format of met_file is as follows (space-delimited)

;        Time  Station_1_Val  Station_2_Val ... Station_n_Val

;        -9999 indicates missing data and will not be used 
;        Please do not use blanks, --, for missing data,
;        otherwise errors will occur.
;-------------------------------------------------------------
SHOW = keyword_set(SHOW)
WRITE_ASCII = keyword_set(ASCII_FILE)
if NOT(keyword_set(N_HEADER)) then n_header=3
if NOT(keyword_set(P)) then p=2d else p=double(p)
nodata = -9999.0

;----------------------------
;Read DEM info from RTI_file
;----------------------------
if NOT(File_Found(RTI_file)) then RETURN
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows

;----------------------------
;Prepare for plotting option
;----------------------------
if (SHOW) then begin
    device, decomposed=0
    loadct, 39, /silent
    window, /free, xsize=nx, ysize=ny
endif

;-------------------------------
;Count lines in met_file to get
;the number of met stations
;-------------------------------
Count_Lines, n_lines, MET_file, /SILENT

;-----------------------------------
;Skip over header lines in met_file
;-----------------------------------
TF_Get_LUN, MET_unit, MET_file
openr, MET_unit, MET_file 
line = ''
for k=0,(n_header-1) do readf,MET_unit,line

;---------------------------------------
;Read the station x & y coordinates and
;determine the number of met stations
;---------------------------------------
readf, MET_unit, line
x_station  = strsplit(line, COUNT=n_stations, /EXTRACT)
n_stations = n_stations-1
x_station  = x_station[1:n_stations]   ;(Remove leading X)
x_station  = double(x_station)
;-----------------------------
readf, MET_unit, line
y_station = strsplit(line, /EXTRACT)
y_station = y_station[1:n_stations]    ;(Remove leading Y)
y_station = double(y_station)

;------------------------------------------------
;Read all station data from met_file
; data[1,*] = Values for station 1 through time.
; data[2,*] = Values for station 2 through time.
; data[3,*] = ...............
;------------------------------------------------
n_series = (n_lines - n_header - 2)
z_data = dblarr(n_stations + 1, n_series)
readf, MET_unit, z_data
free_lun, MET_unit

;--------------------------------------
;First column of z_data is the time
;but we don't use it for anything here
;--------------------------------------
times = z_data[0,*]

;-----------------------------------
;Make grids of pixel x and y values
;-----------------------------------
Get_Pixel_Sizes, dx_vec,dy_vec,dd,da, RTI_file, /METERS
if (info.pixel_geom eq 1b) then begin
    ;---------------------------------------------
    ;Compute planar coordinates (e.g. UTM coords)
    ;---------------------------------------------
    dx     = dx_vec[0]
    dy     = dy_vec[0]
    x      = (dindgen(nx) * dx) + info.x_west_edge  + (dx/2d) 
    y      = (dindgen(ny) * dy) + info.y_south_edge + (dy/2d)
    y      = rotate(y, 2)          ;(same as REVERSE)
    ones_y = dblarr(ny) + 1d
    ones_x = dblarr(nx) + 1d
    x_grid = (x # ones_y)               ;(a 2D array)
    y_grid = (ones_x # y)               ;(a 2D array)
endif else begin
    ;-------------------------------------------------
    ;Compute "distances" in lon/lat space (Geographic
    ;coords, units of degrees).  This does not give
    ;actual distances, which would require computing
    ;distances between (lon,lat) pairs on the surface
    ;of a spherical or ellipsoid model of the Earth,
    ;but does seem reasonable for our purpose.
    ;-------------------------------------------------
    ;minlon = info.x_west_edge
    ;minlat = info.y_south_edge
    ;----------------------------
    dx     = (info.xres / 3600d)  ;[arcsecs -> degrees]
    dy     = (info.yres / 3600d)  ;[arcsecs -> degrees]
    xlons  = (dindgen(nx) * dx) + info.x_west_edge  + (dx/2d)
    ylats  = (dindgen(ny) * dy) + info.y_south_edge + (dy/2d)
    ylats  = rotate(ylats, 2)     ;(same as REVERSE)
    ones_y = dblarr(ny) + 1d
    ones_x = dblarr(nx) + 1d
    ;** lons   = (xlons # ones_y)
    ;** lats   = (ones_x # ylats)

    x_grid = (xlons # ones_y)     ;(a 2D array)
    y_grid = (ones_x # ylats)     ;(a 2D array)
endelse

;-----------------------
;Open RTS file to write
;-----------------------
TF_Get_LUN, RTS_unit, RTS_file
openw, RTS_unit, RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)

;---------------------------
;Open ASCII file to write ?
;---------------------------
if (WRITE_ASCII) then begin
    TF_Get_LUN, ASC_unit, ASCII_file
    openw, ASC_unit, ASCII_file
endif

;-----------------------------------------------
;Start interpolation of the met data to create
;the RTS file.  The number of frames in the RTS
;file will be equal to n_series.  Assume here
;that n_stations is two or more, otherwise, we
;should use the Time Series option in TopoFlow.
;-----------------------------------------------
for j=0L,(n_series-1L) do begin
    ;-----------------------------
    ;Reset vars for this timestep
    ;-----------------------------
    top_sum = dblarr(nx, ny)
    bot_sum = top_sum
    ;------------------------------------
    station_IDs = lonarr(n_stations)-1L
    station_z   = dblarr(n_stations)

    ;-----------------------
    ;Loop over the stations
    ;-----------------------
    for k=0L,(n_stations-1L) do begin

        ;--------------------------------------
        ;Write progress message to message box
        ;--------------------------------------
        if (keyword_set(MSG_BOX_ID)) then begin
            jstr = 'Time = ' + TF_String(j)
            kstr = 'Station = ' + TF_String(k+1)
            mstr = jstr + ', ' + kstr
            widget_control, MSG_BOX_ID, set_value=mstr
        endif

        ;----------------------------------------
        ;Create a grid which gives each pixel's
        ;distance to the kth station.  Any math
        ;that involves d_k results in a grid.
        ;----------------------------------------
        x_k = x_station[k]
        y_k = y_station[k]
        z_k = z_data[k+1,j]
        d_k = sqrt((x_grid - x_k)^2d + (y_grid - y_k)^2d)

        ;----------------------------------------------
        ;Pixels where (d_k eq 0) for some k correspond
        ;to the met stations and should be assigned
        ;the value that was measured for them.
        ;Value of ns should always be zero or one.
        ;----------------------------------------------
        ID = where(d_k eq 0d, ns)
        if (ns ne 0) then begin
            station_IDs[k] = ID
            station_z[k]   = z_k
            ;----------------------------------
            ;Do this to avoid a divide by zero
            ;error message until re-assigned.
            ;----------------------------------
            d_k[ID] = 1d
        endif

        ;----------------------------------
        ;Apply the Inverse Distance Method
        ;----------------------------------
        if (z_k ne nodata) then begin
            top_sum = top_sum + (z_k / d_k^p)
            bot_sum = bot_sum + (1d  / d_k^p)
        endif else begin
            ;----------------------------------
            ;Don't increment top or bottom sum
            ;----------------------------------
        endelse

    endfor

    ;----------------------------------------
    ;Assign the new z-value, with station
    ;pixels receiving their measured value
    ;and others receiving interpolated value
    ;----------------------------------------
    z = (top_sum / bot_sum)
    w = where((station_IDs ne -1L) AND $
              (station_z ne nodata), nw)
    if (nw ne 0) then z[station_IDs[w]] = station_z[w]

    ;------------------------------------
    ;Write another frame to the RTS file
    ;------------------------------------
    writeu, RTS_unit, float(z)

    ;----------------------------------------
    ;Write another frame to the ASCII file ?
    ;----------------------------------------
    if (WRITE_ASCII) then begin
        printf, ASC_unit, float(z)
    endif

    ;------------------------------------
    ;Option to display frame in a window
    ;------------------------------------
    if (SHOW) then begin
        tvscl, float(z)
        ;** surface, float(z)
        wait, 0.2   ;[seconds]
    endif
endfor

;-------------------
;Close the RTS file
;-------------------
free_lun, RTS_unit


;--------------------------------------
;Write finished message to message box
;--------------------------------------
if (keyword_set(MSG_BOX_ID)) then begin
    widget_control, MSG_BOX_ID, set_value='Finished.'
endif

;-----------------------
;Close the ASCII file ?
;-----------------------
if (WRITE_ASCII) then free_lun, ASC_unit

end;  Make_RTS_via_IDM
;***************************************************************
pro GUI_Make_RTS_via_IDM_event, event

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
;--------------------------
;Read name of new RTS file
;--------------------------
Read_Text_Box, state.RTS_file_ID, RTS_file, OK, /TEXT
if NOT(OK) then RETURN

Check_Overwrite, RTS_file, OK
if NOT(OK) then RETURN
;----------------------------
;Read name of input RTS file
;----------------------------
Read_Text_Box, state.RTI_file_ID, RTI_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(RTI_file)
if NOT(OK) then RETURN
;----------------------------
;Read name of input MET file
;----------------------------
Read_Text_Box, state.MET_file_ID, MET_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(MET_file)
if NOT(OK) then RETURN

;----------------------------
;Read the value of p for IDM
;----------------------------
Read_Text_Box, state.p_ID, p, OK, /DOUBLE
if NOT(OK) then RETURN

;--------------------------------------------
;Read number of header lines in profile file
;--------------------------------------------
Read_Text_Box, state.n_header_ID, n_header, OK, /INTEGER
if NOT(OK) then RETURN

;--------------------------------------
;Call routines that create the new DEM
;--------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Make_RTS_via_IDM, MET_file, RTI_file, RTS_file, $
                  N_HEADER=n_header, P=p, $
                  MSG_BOX_ID=state.msg_box_ID  
widget_control, event.ID, sensitive=1    ;(enable button)

;-------------------------
;Show a "finished" dialog
;-------------------------
msg = ['Finished creating RTS file.', ' ']
result = GUI_Message(msg, /INFO, TITLE="Finished")
;*** Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'distrib_tool.htm'

;************
'CLOSE' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CLOSE') AND $
   (uvalue ne 'START') then $
    widget_control, event.top, set_uvalue=state 

end;  GUI_Make_RTS_via_IDM_event 
;****************************************************************
pro GUI_Make_RTS_via_IDM, leader

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
RTI_file     = prefix + '.rti'
MET_file     = prefix + '_metdata.txt'
RTS_file     = prefix + '_metdata.rts'
n_header_str = ' 2 '
p_str        = ' 2 '

;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, msg_box_ID:0L, $
RTI_file_ID:0L, MET_file_ID:0L, RTS_file_ID:0L, $
n_header_ID:0L, p_ID:0L}

ngap = 6
XS   = 30
XS2  = 8

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='Make RTS via IDM Tool', $
            /COLUMN, LEADER=leader
B1 = widget_base(MB, /COLUMN, /FRAME)
B2 = widget_base(MB, /ROW)

;-------------------------
;Get name of new RTS file
;-------------------------
FF = widget_base(B1, /ROW, SPACE=ngap)
  FF1 = widget_label(FF, VALUE='Name of new RTS file: ')
  FF2 = widget_text(FF, VALUE=RTS_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTS_file_ID = FF2

;-------------------------
;Get name of the RTI file
;-------------------------
RF = widget_base(B1, /ROW, SPACE=ngap)
  RF1 = widget_label(RF, VALUE='Existing RTI filename: ')
  RF2 = widget_text(RF, VALUE=RTI_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.RTI_file_ID = RF2

;-------------------------
;Get name of the MET file
;-------------------------
MF = widget_base(B1, /ROW, SPACE=ngap)
  MF1 = widget_label(MF, VALUE='Station data filename: ')
  MF2 = widget_text(MF, VALUE=MET_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.MET_file_ID = MF2

;---------------------------------
;Get number of header lines, etc.
;---------------------------------
NH = widget_base(B1, /ROW, SPACE=ngap)
  NH1 = widget_label(NH, VALUE='Station data header lines: ')
  NH2 = widget_text(NH, VALUE=n_header_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.n_header_ID = NH2

;-----------------------
;Get value of p for IDM
;-----------------------
PV = widget_base(B1, /ROW, SPACE=ngap)
  PV1 = widget_label(PV, VALUE='Value of p for IDM: ')
  PV2 = widget_text(PV, VALUE=p_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS2)
  state.p_ID = PV2

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [FF1, RF1, MF1, NH1, PV1]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, B2, /START, /HELP, /CLOSE 


;---------------------
;A status message box
;---------------------
MS = widget_base(B2, /ROW, SPACE=ngap)
  MS1 = widget_label(MS, VALUE='Status: ')
  MS2 = widget_text(MS, VALUE='Ready.', XSIZE=16)
  state.msg_box_ID = MS2

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Make_RTS_via_IDM', XOFF=XOFF

end;  GUI_Make_RTS_via_IDM
;****************************************************************

