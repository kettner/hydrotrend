
;***************************************************************
;   utils_TF.pro

;   Copyright (c) 2001-2007, Scott D. Peckham 
;   Created:   Oct 2001 - Jan 2002
;   Modified:  Jul-Dec 2005, May 2006
;   Modified:  July 2006

;   Notes:     There is a line in the Flow_Widths function
;              that can be used to fully test different
;              error handlers.  

;***************************************************************

;   TF_String           (function)
;   TF_Print
;   Clear_Log_Window
;   Count_Lines
;   Current_Directory   (function)
;   Resize              (function)
;   Make_Savefile

;   Trace_Error
;   Check_Error_Status
;   No_Catch

;   Read_RTI_Value
;   Read_RTI_File
;   Write_RTI_File      (8/2/05)
;   Get_RTI_Filename
;   Get_Data_Prefix
;   Get_Run_Prefix      (7/15/06)
;   Not_Same_Byte_Order (function)

;   Read_XYZ_as_Grid    (8/3/05)
;   Read_Grid
;   Write_Grid          (7/15/05)
;   Open_RTS_File
;   Number_of_Frames    (3/9/07)
;   Open_RT3_File       (7/14/06)

;   Get_Flow_Codes
;   Convert_Flow_Grid

;   Parent_IDs          (function)
;   Get_Flux_Indices    (10/8/01)
;   Flow_Widths         (function, 10/9/01)
;   Flow_Lengths        (function, 10/9/01)
;   Get_FS_Slope
;   Get_FS_Slope2

;   Courant_Condition_OK
;   Stable_Timestep
;   TF_Tan              (function, 12/22/05)
;   TF_Get_Lun          (procedure, 5/30/06)

;********************************************************************
function TF_String, number, FORMAT=FORMAT

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,''

RETURN, strtrim(string(number, FORMAT=FORMAT), 2)

END;    TF_String
;*****************************************************************
pro TF_Print, string

COMMON LOGWIN, LW

if (LW.n_lines ge LW.max_lines) then begin
    ;-----------------------------------
    ;Delete half of the lines in buffer
    ;-----------------------------------
    widget_control, LW.log_win_ID, get_value=all_lines
    start = (LW.max_lines / 2)
    stop  = (LW.max_lines - 1L)
    widget_control, LW.log_win_ID, set_value=all_lines[start:stop]
    widget_control, LW.log_win_ID, set_text_top_line=start
    LW.n_lines = start
endif

;------------------------------------------
;Append string to log window on a new line
;------------------------------------------
widget_control, LW.log_win_ID, set_value=string, /append
;---------------------------------------------------------
;(3/18/08)  On a Mac, at least, text in Output Log looks
;           better if we comment out the next line
;---------------------------------------------------------
;** widget_control, LW.log_win_ID, set_text_top_line=LW.n_lines
;** widget_control, LW.log_win_ID, set_text_top_line=(LW.n_lines - 10) > 0
LW.n_lines = (LW.n_lines + 1L)

;------------------------------
;To revert to simple IDL PRINT
;comment out all but this line
;------------------------------
;print, string

end;  TF_Print
;*****************************************************************
pro Clear_Log_Window

COMMON LOGWIN, LW

;------------------------
;This isn't what we want
;------------------------
;*** array = strarr(LW.max_lines)
;*** array = replicate('', LW.max_lines)  ;(same thing)

array = strarr(1)
widget_control, LW.log_win_ID, set_value=array

;------------------------------------
;Don't need this with current method
;------------------------------------
;** widget_control, LW.log_win_ID, set_text_top_line=0L

LW.n_lines = 0L

end;  Clear_Log_Window
;*****************************************************************
pro Count_Lines, n_lines, filename, SILENT=SILENT

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;--------------
;Open the file
;--------------
TF_Get_LUN, unit, filename
openr, unit, filename

;----------------
;Count the lines
;----------------
n_lines = 0L
n_total = 0L
line = ''
while NOT(EOF(unit)) do begin
    readf, unit, line
    n_total = (n_total + 1L)
    ;-------------------------
    ;Count the nonblank lines
    ;-------------------------
    len = strlen(strtrim(line, 2))
    if (len ne 0) then n_lines = (n_lines + 1L)
endwhile

;---------------
;Close the file
;---------------
free_lun, unit

;------------------
;Print a message ?
;------------------
if NOT(keyword_set(SILENT)) then begin
    print,'For the file: ' + filename
    print,'Total number of lines    = ' + TF_String(n_total)
    print,'Number of nonblank lines = ' + TF_String(n_lines)
    print,' '
endif

END;  Count_Lines
;*****************************************************************
function Current_Directory

;-----------------------------------------------------------
;NOTES:  IDL's CD routine behaves differently on different
;        platforms.  This routine provides a wrapper around
;        CD to avoid this problem.  On Macs, the directory
;        separator is ":" and it is automatically appended.

;        With the separator, filepaths can be constructed
;        as follows:   filepath = (directory + filename)
;-----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,'Null'

cd, CURRENT=directory

;-------------------------------
;Append a directory separator ?
;-------------------------------
case (!d.name) of
    'WIN' : directory = (directory + '\')
    'X'   : directory = (directory + '/')
    'MAC' : dum = 0   ;(no action needed on Macs)
     else : dum = 0   
endcase

RETURN, directory
END;    Current_Directory
;*****************************************************************
function Resize, array,factor,REDUCE, new_ncols,new_nrows, $
                 SAMP_TYPE=SAMP_TYPE

;-------------------------------------------------------------
;NOTES:  This routine allows any 2D array to be "rebinned"
;        to n times larger or smaller than its original size,
;        where n is a positive integer.

;        If (REDUCE eq 1b) then the array is reduced in size
;        by sampling at regular, equal-spaced intervals.

;        If (REDUCE eq 0b) then the array is enlarged in size
;        by pixel replication.

;        This routine reduces to IDL's REBIN routine if:
;           (1) (REDUCE eq 0), or
;           (2) (REDUCE eq 1) AND the requested array
;                dimensions are multiples of the original
;                dimensions.
;        When these conditions are not met, the original
;        array is cropped by the smallest amount necessary
;        to meet them.  This amount will never exceed n.

;        Resizing by arbitrary scale factors is avoided

;        because it distorts the data too much.
;-------------------------------------------------------------

if (factor le 0) then begin
    GUI_Error_Message,['Scale factor must be greater than zero.']
    RETURN,0
endif

if NOT(keyword_set(SAMP_TYPE)) then SAMP_TYPE=0

;------------------------
;Get dimensions of array
;------------------------
s     = size(array, /DIMENSIONS)
ncols = s[0]
nrows = s[1]

if (factor ne 1) then begin
    if (REDUCE) then begin
        new_ncols = long(ncols / factor) > 1L
        new_nrows = long(nrows / factor) > 1L

        x_rem = (ncols mod new_ncols)
        y_rem = (nrows mod new_nrows)
        ;print,'x_rem = ' + string(x_rem)
        ;print,'y_rem = ' + string(y_rem)
        ;print,' '


        if ((x_rem eq 0) AND (y_rem eq 0)) then $
            a=rebin(array, new_ncols, new_nrows, sample=SAMP_TYPE) $
        else $
            a=rebin(array(0:(ncols - 1L - x_rem), $
                          0:(nrows - 1L - y_rem)), $
                    new_ncols, new_nrows, sample=SAMP_TYPE)
    endif else begin
        new_ncols = long(factor) * ncols
        new_nrows = long(factor) * nrows
        a=rebin(array, new_ncols, new_nrows, sample=SAMP_TYPE)
    endelse
endif else begin
    new_ncols = ncols
    new_nrows = nrows
    RETURN, array
endelse

RETURN, a
END;    Resize
;*****************************************************************
pro Make_Savefile

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;------------------------------------
;Get name and location for save file
;------------------------------------
save_file = dialog_pickfile(FILTER='*.sav', FILE='topoflow.sav')
if (save_file eq '') then RETURN

;---------------------
;Create the save file     ;(But this picks up RiverTools routines, too.)
;---------------------
print,' '
print,'Saving TopoFlow model code to: '
print, save_file + '...'

save, filename=save_file, /routines
print,'Finished.'
print,' '

;--------------------------------
;Print the size of the save file
;--------------------------------
TF_Get_LUN, unit, save_file
openr, unit, save_file 
info = fstat(unit)
free_lun, unit
print, 'Size of new SAV file is ' + $
        TF_String(info.size) + ' bytes.'
print,' '

END;  Make_Savefile
;***************************************************************
pro Trace_Error, status, event, OK, ABORT=ABORT

;---------------------------
;Return if everything is OK
;---------------------------
if (status eq 0) then begin
    OK=1b  &  RETURN
endif else OK=0b

ABORT = keyword_set(ABORT)
if (ABORT) then begin
   msg = ['TopoFlow simulation aborted. ', ' ']
endif else msg = ['']

TRACEBACK = 1b
if (TRACEBACK) then begin
    ;----------------------------------------
    ;Print info that is useful for debugging
    ;----------------------------------------
    help, OUTPUT=traceback, /LAST_MESSAGE
    msg = [msg, 'ERROR MESSAGE FROM IDL:', ' ', traceback]
    GUI_Error_Message, msg
endif else begin
    GUI_Error_Message, msg, /IDL_ERROR
endelse

;-----------------------
;Undim the Start button
;-----------------------
VALID = widget_info(event.ID, /VALID_ID)
if (VALID) then widget_control, event.ID, /sensitive

;---------------------------------
;Close all output files (7/19/06)
;May impact other IDL programs.
;---------------------------------
close, /ALL

;--------------------------------
;Print message if aborting a run 
;--------------------------------
if (ABORT) then begin
    TF_Print,'************************************'
    TF_Print,'TopoFlow simulation aborted.'
    TF_Print,'************************************'
    TF_Print,' '
endif

end;  Trace_Error
;***************************************************************
pro Check_Error_Status, status, OK, EVENT_ID=EVENT_ID, $
                        TRACEBACK=TRACEBACK, SILENT=SILENT

;--------------------------------------------------------
;Notes:  Avoid calling other routines from this one,
;        so they can all call this one without producing
;        an infinite loop.

;        Another option is to disable this error routine
;        entirely, and use only a single error routine
;        in the GUI_TopoFlow_event procedure.
;--------------------------------------------------------

;------------------------------------------------------
;DANGER:  LINES HERE ARE RECURSIVE AND CAUSE LOCK-UP!!
;------------------------------------------------------
;*** No_Catch, status
;*** Check_Error_Status, status, OK
;*** if NOT(OK) then RETURN

FORWARD_FUNCTION GUI_Message

if (n_elements(status) eq 0) then status=0
SILENT = keyword_set(SILENT)

;TRACEBACK = keyword_set(TRACEBACK)
TRACEBACK = 1b

;----------------------------------------
;To disable this error handler, remove
;comment characters from next two lines.
;There is another error handling option
;in the GUI_TopoFlow_event routine.
;----------------------------------------
OK = 1b
RETURN

;----------------------------------------
;To have calling routine return to it's
;caller when there's an error, similar
;to IDL's ON_ERROR,2, do this.
;----------------------------------------
;Can't even start TopoFlow with this on.  ************
;----------------------------------------
;OK = 0b
;RETURN

if (status ne 0) then begin
    OK=0b

    ;-------------------------------------
    ;Cancel CATCH to avoid infinite loops
    ;(Ineffective here ?? See IDL docs.)
    ;-------------------------------------
    ;catch, /CANCEL

    ;------------------------------------
    ;Resensitize a dimmed "Start" button
    ;------------------------------------
    if (keyword_set(EVENT_ID)) then begin
        VALID = widget_info(EVENT_ID, /VALID_ID)
        if (VALID) then widget_control,eventID,sensitive=1
    endif

    ;--------------------------
    ;Get the IDL error message
    ;--------------------------
    if (TRACEBACK) then begin
        help, OUTPUT=traceback, /LAST_MESSAGE
        error_message = ['ERROR MESSAGE FROM IDL:', ' ', traceback]
    endif else begin
        error_message = [!ERR_STRING]
    endelse

    ;------------------------------------------
    ;Display error message in a dialog.
    ;SILENT keyword is for recoverable errors.
    ;------------------------------------------
    if NOT(SILENT) then begin
       result = GUI_Message(error_message, /ERROR)
    endif ;*** else begin
    ;   n = n_elements(error_message)
    ;   for k=0,(n-1) do print,error_message
    ;endelse

    ;-------------------------------------------------
    ;Experimental, to avoid a series of error dialogs
    ;by returning to caller and continuing.
    ;-------------------------------------------------
    ;RETALL
endif else OK=1b

END;  Check_Error_Status
;***************************************************************
pro No_Catch, status

;--------------------------------------------------------------
;Notes:  This routine replaces a call to "CATCH, status" in
;        every subroutine.  There is then a single CATCH in
;        the TopoFlow main event handler: GUI_TopoFlow_event.
;        This results in more graceful error handling, because
;        only a single error dialog (the first one) is
;        produced, instead of a cascade of error dialogs.

;        Also, since status is always returned as zero, the
;        Check_Error_Status routine above is never called.
;        One disadvantage is that local cleanup, like closing
;        open files, etc., does not occur.
;--------------------------------------------------------------
status = 0

end;  No_Catch
;****************************************************************
pro Read_RTI_Value, value, unit, TYPE, UPCASE=UPCASE

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

line=''
while NOT(EOF(unit)) AND (line eq '') do begin
   readf, unit, line
   line = strtrim(line, 2)
   c1   = strmid(line,0,1)
   pos  = strpos(line, ':')
   if (c1 ne ';') AND (pos ne -1) then begin
       s=strmid(line, pos+1)
   endif else begin
       line=''  &  s='-1'
   endelse 
endwhile

case (TYPE) of
     'BYTE'    : value=byte(fix(s))
     'INTEGER' : value=fix(s)
     'FLOAT'   : value=float(s)
     'DOUBLE'  : value=double(s)
     'LONG'    : value=long(s)
     'STRING'  : begin
                 value=strtrim(s,2)
                 if (keyword_set(UPCASE)) then $
                     value = strupcase(value)
                 end
endcase

END;  Read_RTI_Value
;*****************************************************************
pro Read_RTI_File, RTI_file, info, OK, REPORT=REPORT

;----------------------------------------------------------
;NOTES:  This routine reads information from a RiverTools
;        Information (RTI) file that describes the grids
;        in the current directory.

;        If the value -9999 is used for unknown values in
;        the RTI file, then it will cause this routine to
;        generate a warning message in cases where this
;        value is not reasonable.  However, emin and emax
;        are not checked for validity.

;        "type" must be INTEGER, FLOAT, LONG, or DOUBLE.
;        "byte_order" must be MSB or LSB.
;        Type and byte order are converted to upper case.
;----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status 
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;----------------------
;Does RTI file exist ?
;----------------------
OK = 1b
result = findfile(RTI_file, count=count)
if (count eq 0) then begin
    msg = [ $
    'ERROR: RTI file not found. ', ' ', $
    'The file: ',$
    '  ' + RTI_file, $
    'was not found in the working directory. ',$
    ' ']
    result = GUI_Message(msg, /INFO)
    OK = 0b
    RETURN
endif
TF_Get_LUN, unit, RTI_file
openr, unit, RTI_file

;-----------------------------
;Does first line look right ?
;-----------------------------
line1 = ''
readf, unit, line1
line1 = strtrim(strupcase(line1),2)

if (line1 ne 'RIVERTOOLS INFO FILE') then begin
    msg = [ $
    'ERROR:  Invalid RTI file. ', ' ', $
    'The file: ',$
    '  ' + RTI_file, $
    'is not a valid RTI file. ',$
    ' ']
    result = GUI_Message(msg, /INFO)
    OK = 0b
    RETURN
endif

;------------------------------
;Read values from the RTI file
;------------------------------
Read_RTI_Value, DEM_file,   unit, 'STRING'
Read_RTI_Value, source,     unit, 'STRING' 
Read_RTI_Value, ncols,      unit, 'LONG'
Read_RTI_Value, nrows,      unit, 'LONG'
Read_RTI_Value, data_type,  unit, 'STRING', /UPCASE
Read_RTI_Value, byte_order, unit, 'STRING', /UPCASE
Read_RTI_Value, pixel_geom, unit, 'BYTE'
Read_RTI_Value, xres,       unit, 'DOUBLE'  ;(9/14/99)
Read_RTI_Value, yres,       unit, 'DOUBLE'  ;(9/14/99)
Read_RTI_Value, zres,       unit, 'FLOAT'
Read_RTI_Value, z_units,    unit, 'STRING', /UPCASE
Read_RTI_Value, y_south,    unit, 'DOUBLE'
Read_RTI_Value, y_north,    unit, 'DOUBLE'
Read_RTI_Value, x_east,     unit, 'DOUBLE'
Read_RTI_Value, x_west,     unit, 'DOUBLE'
Read_RTI_Value, box_units,  unit, 'STRING', /UPCASE
Read_RTI_Value, emin,       unit, 'FLOAT'
Read_RTI_Value, emax,       unit, 'FLOAT'
Read_RTI_Value, UTM_zone,   unit, 'STRING', /UPCASE
if (UTM_zone eq '-1') then UTM_zone = 'unknown'
free_lun,unit

;-----------------------------
;Store info in info structure
;-----------------------------
info = { $
DEM_file: DEM_file, $
RTI_file: RTI_file, $
data_source: source, $
;----------------------------
ncols:ncols, nrows:nrows, $
data_type: data_type, $
byte_order: byte_order, $
;----------------------------
pixel_geom: pixel_geom, $
xres: xres, yres: yres, zres: zres, $
z_units: z_units, $
;----------------------------
y_north_edge: y_north, $
y_south_edge: y_south, $
x_west_edge:  x_west,  $
x_east_edge:  x_east, $
box_units: box_units, $
;----------------------------
emin:emin, emax:emax, $
UTM_zone: UTM_zone}

;----------------
;Optional report
;----------------
if keyword_set(REPORT) then begin
   print,'----------------'
   print,'DEM Information'
   print,'----------------'
   print,'filename  = ' + gridfile
   print,'source    = ' + source
   print,'ncols     = ' + TF_String(ncols)
   print,'nrows     = ' + TF_String(nrows)
   print,'data_type = ' + data_type
   print,'byte_ord  = ' + byte_order 
   print,'pix_geom  = ' + TF_String(pixel_geom)
   print,'xres      = ' + TF_String(xres)
   print,'yres      = ' + TF_String(yres)
   print,'zres      = ' + TF_String(zres)
   print,'z_units   = ' + TF_String(z_units)
   print,'S_edge    = ' + TF_String(y_south)
   print,'N_edge    = ' + TF_String(y_north)
   print,'E_edge    = ' + TF_String(x_east)
   print,'W_edge    = ' + TF_String(x_west)
   print,'box_units = ' + TF_String(box_units)
   print,'emin      = ' + TF_String(emin)
   print,'emax      = ' + TF_String(emax)
   print,'UTM_zone  = ' + UTM_zone
   print,' '
endif

END;  Read_RTI_File
;*****************************************************************
pro Write_RTI_File, RTI_file, info, SILENT=SILENT

SILENT = keyword_set(SILENT)

f = '(D22.12)'
y_south_string = strtrim(string(info.y_south_edge, format=f), 2)
y_north_string = strtrim(string(info.y_north_edge, format=f), 2)
x_east_string  = strtrim(string(info.x_east_edge,  format=f), 2)
x_west_string  = strtrim(string(info.x_west_edge,  format=f), 2)

;-----------------------
;Open RTI file to write
;-----------------------
TF_Get_LUN, A, RTI_file
openw, A, RTI_file

;-----------------------
;Write info to RTI file
;-----------------------
printf,A,'RiverTools Info File'
printf,A,' '
printf,A,';--------------------'
printf,A,';Description of Grid'
printf,A,';--------------------'
printf,A,'DEM filename:  ' + info.DEM_file
printf,A,'DEM source:    ' + info.data_source
printf,A,' '
printf,A,';----------------------'
printf,A,';Grid dimensions, etc.'
printf,A,';----------------------'
printf,A,'Number of columns:  ' + TF_String(info.ncols)
printf,A,'Number of rows:     ' + TF_String(info.nrows)
printf,A,'Data type:          ' + info.data_type
printf,A,'Byte order:         ' + info.byte_order
printf,A,' '
printf,A,';--------------------'
printf,A,';Pixel geometry info'
printf,A,';--------------------'
printf,A,'Pixel geometry code:  ' + TF_String(fix(info.pixel_geom))
printf,A,'Pixel x-resolution:   ' + TF_String(info.xres)
printf,A,'Pixel y-resolution:   ' + TF_String(info.yres)
printf,A,'Pixel z-resolution:   ' + TF_String(info.zres)
printf,A,'Z-resolution units:   ' + info.z_units
printf,A,' '
printf,A,';--------------------------------------------------'
printf,A,';Bounding box coordinates (degrees lat/lon or UTM)'
printf,A,';--------------------------------------------------'
printf,A,'South edge y-coordinate:  ' + y_south_string
printf,A,'North edge y-coordinate:  ' + y_north_string
printf,A,'East  edge x-coordinate:  ' + x_east_string
printf,A,'West  edge x-coordinate:  ' + x_west_string
printf,A,'Measurement units:        ' + info.box_units
printf,A,' '
printf,A,';----------------------'
printf,A,';Min and max elevation'
printf,A,';----------------------'
printf,A,'Minimum elevation:  ' + TF_String(info.emin)
printf,A,'Maximum elevation:  ' + TF_String(info.emax)
printf,A,' '
printf,A,';---------'
printf,A,';UTM zone'
printf,A,';---------'
printf,A,'UTM zone: ' + info.UTM_zone
printf,A,' '
printf,A,' '

;-----------------------
;Write RTI file trailer
;-----------------------
printf,A,';----------------------------------'
printf,A,';Notes About RiverTools Info Files'
printf,A,';----------------------------------'
printf,A,";(1)  RiverTools grid info filenames end with '.rti'."
printf,A,';(2)  The first line should be:  RiverTools Info File'
printf,A,';(3)  Lines starting with a semi-colon are ignored.'
printf,A,';(4)  Colons are used to delimit labels from values.'  
printf,A,';(5)  The order of the numbers above is important.'
printf,A,';(6)  Number of rows and columns are required.'
printf,A,';(7)  Pixel geometry codes are: 0=Fixed-Angle, ' + $
         '1=Fixed-Length.'
printf,A,';(8)  Pixel x-resolution and y-resolution are required.'
printf,A,';(9)  Measurement units must be METERS or DEGREES.'
printf,A,';(10) Elevation data type is required.'
printf,A,';     Allowed types are BYTE, INTEGER, LONG, FLOAT, DOUBLE.'
printf,A,';(11) Byte order is required; either LSB or MSB.'
printf,A,';     (LSB = Least Significant Byte = little-endian)'
printf,A,';     (MSB = Most Significant Byte  = big-endian)'
printf,A,";(12) For 'fixed-angle' pixels, bounding lats and lons"
printf,A,';     are required to compute lengths and areas correctly.'
printf,A,';(13) Latitudes south of equator are negative and'
printf,A,';     longitudes west of prime meridian are negative.'
printf,A,';(14) This file is best modified with the View DEM Info'
printf,A,';     dialog in the File menu.'
printf,A,' '
printf,A,' '

;-------------------
;Close the RTI file
;-------------------
free_lun, A

;---------------------------
;Print or display a message
;---------------------------
if NOT(SILENT) then begin
    ;print,'DEM info written to: '
    ;print, infofile
    ;print,' '
    ;--------------------------------
    msg = [ $
    'DEM information written to: ', ' ', RTI_file, ' ']
    result = GUI_Message(msg, /INFO)
endif

end;  Write_RTI_File 
;*****************************************************************
pro Get_RTI_Filename, RTG_file, RTI_file, PREFIX=prefix

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;------------------------------------------
;Remove directory part, which may contain
;underscores and dots, etc.
;------------------------------------------
Get_Data_Prefix, RTG_file, prefix, filename

;-----------------------------------------
;Construct RTI filename from RTG filename
;-----------------------------------------
b = byte(filename)
w = where(b eq 95b, nw)  ;(look for underscores)
if (nw eq 0) then w = where(b eq 46b, nw)  ;(look for dots)
if (nw ne 0) then begin
    ;--------------------------------------------
    ;Extract string up to last underscore or dot
    ;--------------------------------------------
    p = w[nw-1]
    prefix = strmid(filename, 0, p)
endif else begin
    prefix = filename
endelse

RTI_file = (prefix + '.rti')

END;  Get_RTI_Filename
;*****************************************************************
pro Get_Data_Prefix, filepath, prefix, filename

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;--------------------------------
;Get file separator for platform
;--------------------------------
case (!d.name) of
    'WIN' : sep = 92b    ;byte('\')
    'X'   : sep = 47b    ;byte('/')
    'MAC' : sep = 58b    ;byte(':')
     else : sep = 92b
endcase

;------------------------------------
;Extract filename from filepath as
;everything after last dir separator
;------------------------------------
b = byte(filepath)
w = where(b eq sep, ns)
if (ns ne 0) then begin
    start  = w[ns-1L] + 1L
    filename = strmid(filepath, start)
endif else begin
    filename = filepath
endelse

;-----------------------------------
;Extract prefix from filename as
;everything up to underscore or dot
;-----------------------------------
b = byte(filename)
w = where(b eq 95b, nw)
if (nw eq 0) then w = where(b eq 46b, nw)  ;(look for dots)
if (nw ne 0) then begin
    p = w[nw-1]
    prefix = strmid(filename, 0, p)
endif else begin
    prefix = filename
endelse

END;  Get_Data_Prefix
;*****************************************************************
pro Get_Run_Prefix, run_prefix

;-----------------------------------------------------------------
;Notes:   7/13/06.  Get a default run prefix as the lowest unused
;         Case number. This currently handles numbers 1 to 999.

;         3/8/07.  Fixed bug by using a new method.
;-----------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;----------------------------------------
;Doesn't matter now whether FILE_SEARCH
;returns sorted names, but it seems to
;----------------------------------------
digits = ['0','1','2','3','4','5','6','7','8','9']
files  = file_search('Case*')

;print,'files = ', files
;;files  = files[sort(files)]
;;print,'files = ', files

;-----------------------------
;Find the maximum case number
;-----------------------------
max_case = -1
n_files  = n_elements(files)
for k=0L,(n_files - 1L) do begin
    ;------------------------
    ;Get characters 5, 6 & 7
    ;------------------------
    char5 = strmid(files[k], 4, 1)
    char6 = strmid(files[k], 5, 1)
    char7 = strmid(files[k], 6, 1)
    ;--------------------------------
    w1 = where(digits eq char5, n1)
    if (n1 ne 0) then begin
        str = char5
        w2  = where(digits eq char6, n2)
        if (n2 ne 0) then begin
            str = str + char6
            w3  = where(digits eq char7, n3)
            if (n3 ne 0) then str = str + char7
        endif
        max_case = (max_case > fix(str))
    endif  
endfor

;------------------------------------------
;Construct run prefix from max case number
;------------------------------------------
if (max_case eq -1) then begin
    run_prefix = 'Case1' 
endif else begin
    run_prefix = 'Case' + TF_String(max_case + 1)
endelse

;print, 'run_prefix = ' + run_prefix

end;  Get_Run_Prefix
;*****************************************************************
function Not_Same_Byte_Order, byte_order

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,-1

if (n_elements(byte_order) eq 0) then begin
    print, 'ERROR: Byte order argument is required in'
    print,'        the Not_Same_Byte_Order function.'
    print,' '
    RETURN, -1
endif

;----------------------------------
;Get byte order of user's computer
;----------------------------------
big_endian = (byte(1,0,2))[0] eq 0b
if (big_endian) then machine_byte_order='MSB' $
                else machine_byte_order='LSB'

;---------------------------------------
;Compare machine byte order to the byte
;order of the current binary grid.
;---------------------------------------
NOT_SAME = (machine_byte_order ne byte_order)

RETURN, NOT_SAME
END;  Not_Same_Byte_Order
;***************************************************************
pro Read_XYZ_as_Grid, XYZ_file, RTG_file

;------------------------------------------
;Notes:  It is assumed that X and Y values
;are for the center of the pixel.
;------------------------------------------
Count_Lines, n_lines, XYZ_file

;------------------------------
;Read XYZ data into a 2D array
;------------------------------
data = fltarr(3,n_lines)
xyz  = fltarr(3)
k    = 0L
TF_Get_LUN, unit, XYZ_file
openr, unit, XYZ_file 
while NOT(EOF(unit)) do begin
    readf, unit, xyz
    data[*,k] = xyz
    k = (k + 1L)
endwhile
free_lun, unit
data = data[*,0L:k-1L]   ;(discard extra slots)

;------------------------------
;Extract x, y & z as 1D arrays
;---------------------------------
;xres,xmin,xmax will have DOUBLE
;type, and so will yres,ymin,ymax
;---------------------------------
x = double(data[0,*])
y = double(data[1,*])
z = float(data[2,*])

;---------------------------
;Compute the mins and maxes
;---------------------------
xmin = min(x, max=xmax)
ymin = min(y, max=ymax)
zmin = min(z, max=zmax)

;---------------------------
;How many cols are there ?
;Assume row major ordering.
;Compute number of rows.
;---------------------------
w = where(y eq y[0], ncols)
n = n_elements(x)
nrows = (n / ncols)

;-----------------------
;Find xres, yres & zres
;-----------------------
yres = abs(y[ncols] - y[0])
xres = abs(x[1] - x[0])
zres = 1.0

;------------------
;This isn't needed
;------------------
;x = reform(x, ncols, nrows)
;y = reform(y, ncols, nrows)
;z = reform(z, ncols, nrows)

;--------------------------------
;Write data as a binary RTG file
;--------------------------------
TF_Get_LUN, unit, RTG_file
openw, unit, RTG_file 
writeu, unit, z
free_lun, unit

;----------------------------------
;Get byte order of user's computer
;----------------------------------
big_endian = (byte(1,0,2))[0] eq 0b
if (big_endian) then byte_order='MSB' $
                else byte_order='LSB'

;-------------------------
;Get name of new RTI file
;-------------------------
Get_RTI_Filename, RTG_file, RTI_file

;-------------------------------
;Create an RTI file for new DEM
;-------------------------------
info = { $
DEM_file: RTG_file, $
RTI_file: RTI_file, $
data_source: 'Created by XYZ to Grid routine.', $
;----------------------------
ncols:ncols, nrows:nrows, $
data_type: 'FLOAT', $
byte_order: byte_order, $
;----------------------------
pixel_geom: 1b, $
xres: xres, yres: yres, zres: zres, $
z_units:  'METERS', $
;----------------------------
y_north_edge: ymax + (yres/2d), $
y_south_edge: ymin - (yres/2d), $
x_west_edge:  xmin - (xres/2d), $
x_east_edge:  xmax + (xres/2d), $
box_units:    'METERS', $
;----------------------------
emin:zmin, emax:zmax, $
UTM_zone: 'UNKNOWN'}

;-----------------------
;Write the new RTI file
;-----------------------
Write_RTI_File, RTI_file, info, /SILENT

;--------------------
;Print final message
;--------------------
print,'Finished creating RTG file.'
print,' '

end;  Read_XYZ_as_Grid
;***************************************************************
pro Read_Grid, grid, RTG_file, TYPE=type, REPORT=REPORT, $
               SILENT=SILENT

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

SILENT = keyword_set(SILENT)

;-----------------------
;Construct RTI filename
;-----------------------
Get_RTI_Filename, RTG_file, RTI_file

;------------------------
;Read info from RTI file 
;------------------------
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows

;----------------------------
;Use DEM type if unspecified
;----------------------------
if NOT(keyword_set(TYPE)) then type=info.data_type

;---------------------
;Prepare to read grid
;---------------------
NOZERO = 1b 
case (type) of
    'BYTE'    : grid = bytarr(nx, ny, NOZERO=NOZERO)
    'INTEGER' : grid = intarr(nx, ny, NOZERO=NOZERO)
    'LONG'    : grid = lonarr(nx, ny, NOZERO=NOZERO)
    'FLOAT'   : grid = fltarr(nx, ny, NOZERO=NOZERO)
    'DOUBLE'  : grid = dblarr(nx, ny, NOZERO=NOZERO)
endcase

;-----------------
;Read in the grid
;-----------------
if NOT(SILENT) then TF_Print,'Reading grid values...'
TF_Get_LUN, unit, RTG_file
openr, unit, RTG_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
readu, unit, grid
free_lun, unit
if NOT(SILENT) then begin
    TF_Print,'Finished reading grid from:'
    TF_Print,'  ' + RTG_file
endif

;----------------
;Optional report
;----------------
if (keyword_set(REPORT)) then begin
    gmin = min(grid, max=gmax)
    TF_Print,'    min(grid) = ' + TF_String(gmin)
    TF_Print,'    max(grid) = ' + TF_String(gmax)
    ;TF_Print,' '

endif

END;  Read_Grid
;********************************************************************
pro Write_Grid, grid, RTG_file, TYPE=type, REPORT=REPORT, $
                SILENT=SILENT, RTI_FILE=RTI_file

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

SILENT = keyword_set(SILENT)

if NOT(keyword_set(RTI_FILE)) then begin
    ;--------------------------------------------
    ;Construct RTI filename, but note that it
    ;may end up something like Treynor_chan.rti.
    ;--------------------------------------------
    Get_RTI_Filename, RTG_file, RTI_file
endif


;------------------------
;Read info from RTI file 
;------------------------
Read_RTI_File, RTI_file, info

;------------------------------
;Use FLOAT type if unspecified
;------------------------------
if NOT(keyword_set(TYPE)) then type='FLOAT' 

;-------------------------------
;Convert to specified data type
;-------------------------------
case (type) of
    'BYTE'    : grid = byte(fix(grid))
    'INTEGER' : grid = fix(grid)
    'LONG'    : grid = long(grid)
    'FLOAT'   : grid = float(grid)
    'DOUBLE'  : grid = double(grid)
endcase

;-----------------
;Read in the grid
;------------------------------------
;Note that we use same byte order as
;the other binary files in data set
;------------------------------------
if NOT(SILENT) then TF_Print,'Writing grid values...'
TF_Get_LUN, unit, RTG_file
openw, unit, RTG_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
writeu, unit, grid
free_lun, unit

if NOT(SILENT) then begin
    TF_Print,'Finished writing grid to:'
    TF_Print,'  ' + RTG_file
endif

;----------------
;Optional report
;----------------
if (keyword_set(REPORT)) then begin
    gmin = min(grid, max=gmax)
    TF_Print,'    min(grid) = ' + TF_String(gmin)
    TF_Print,'    max(grid) = ' + TF_String(gmax)
    ;TF_Print,' '
endif

END;  Write_Grid
;********************************************************************
pro Open_RTS_File, RTS_file, unit, grids, $
                   READ=READ, WRITE=WRITE, VERBOSE=VERBOSE,  $
                   RTI_FILE=RTI_FILE, ASSOCIATED=ASSOCIATED, $
                   NX=NX, NY=NY, N_GRIDS=n_grids

;-------------------------------------------------------------
;NOTE:  7/14/06.  Modified with ASSOCIATED keyword so that
;       IDL's "associated I/O" is only used when this keyword
;       is set.  The "grids" argument can only be used when
;       this keyword is set.  Writing RTG and RT3 files is a
;       simple, sequential process and there is no benefit to
;       using associated I/O in this case.

;       NX, NY and N_GRIDS keywords are used only to return
;       values.
;-------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

WRITE      = keyword_set(WRITE)
READ       = keyword_set(READ) OR NOT(WRITE)
VERBOSE    = keyword_set(VERBOSE)
ASSOCIATED = keyword_set(ASSOCIATED)

;-----------------------------------
;Construct RTI filename, if missing
;-----------------------------------
if NOT(keyword_set(RTI_FILE)) then begin
    Get_RTI_Filename, RTS_file, RTI_file
endif

if (VERBOSE) then begin
    TF_Print,'RTS_file = ' + RTS_file
    TF_Print,'Reading RTI_file: ' + RTI_file
endif

;------------------------
;Read info from RTI file 
;------------------------
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows
byte_order = info.byte_order

;-------------------------------
;Open RTS file to read or write
;-------------------------------
TF_Get_LUN, unit, RTS_file
if (WRITE) then begin
    openw, unit, RTS_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
endif else begin
    openr, unit, RTS_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
endelse

;----------------------------------
;How many (nx x ny) grids in file
;assuming data type is FLOAT ?
;----------------------------------
if (READ) then begin
    temp     = fstat(unit)
    filesize = temp.size 
    n_grids  = (filesize / (4L * long(nx) * long(ny)))
endif else begin
    filesize = 0L
    n_grids  = 1L
endelse

;-------------------------------------------
;Is this an RTG file with 2-byte integers ?
;-------------------------------------------
if (filesize eq (2L * long(nx) * long(ny))) then begin
    n_grids   = 1      ;(replaces value of 0)
    data_type = 'INTEGER'
endif else begin
    data_type = 'FLOAT'
endelse

;----------------------------------------
;Use IDL's ASSOC function for file I/O ?
;----------------------------------------
if NOT(ASSOCIATED) then RETURN

if (data_type eq 'FLOAT') then begin
    grids = assoc(unit, fltarr(nx, ny))
endif else begin
    grids = assoc(unit, intarr(nx, ny))
endelse

;-------------------------
;Is this ever necessary ?
;-------------------------
if (WRITE) then begin
    if (data_type eq 'FLOAT') then begin
        grids[0] = fltarr(nx, ny)
    endif else begin
        grids[0] = intarr(nx, ny)
    endelse
endif

END;  Open_RTS_File
;********************************************************************
function Number_of_Frames, RTS_file, RTI_file, NX=nx, NY=ny

;------------------------------------------------------
;Notes:  RTS_files only contain floating-point values.
;        NX and NY keywords only return values.
;------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1

;------------------------
;Read info from RTI file 
;------------------------
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows
byte_order = info.byte_order

;------------------------------
;Open the RTS file for reading
;------------------------------
TF_Get_LUN, RTS_unit, RTS_file
openr, RTS_unit, RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
temp = fstat(RTS_unit)
free_lun, RTS_unit

;-------------------------
;Compute number of frames
;-------------------------
filesize = temp.size 
n_frames = (filesize / (4L * long(nx) * long(ny)))
RETURN, n_frames

end;  Number_of_Frames
;********************************************************************
pro Open_RT3_File, RT3_file, unit, nz, n_stacks, $
                   READ=READ, WRITE=WRITE, NX=NX, NY=NY, $
                   RTI_FILE=RTI_FILE, VERBOSE=VERBOSE

;----------------------------------------------------------
;NOTE:  NX and NY keywords are used only to return values.
;----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

WRITE   = keyword_set(WRITE)
READ    = keyword_set(READ) OR NOT(WRITE)
VERBOSE = keyword_set(VERBOSE)

;-----------------------------------
;Construct RTI filename, if missing
;-----------------------------------
if NOT(keyword_set(RTI_FILE)) then begin
    Get_RTI_Filename, RT3_file, RTI_file
endif

if (VERBOSE) then begin
    TF_Print,'RT3_file = ' + RT3_file
    TF_Print,'Reading RTI_file: ' + RTI_file
endif

;------------------------
;Read info from RTI file 
;------------------------
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows
byte_order = info.byte_order

;-------------------------------
;Open RT3 file to read or write
;-------------------------------
TF_Get_LUN, unit, RT3_file
if (WRITE) then begin
    openw, unit, RT3_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
endif else begin
    openr, unit, RT3_file, $
           SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
endelse

;-----------------------------------
;How many (nx x ny x nz) stacks in
;file assuming data type is FLOAT ?
;-----------------------------------
if (READ) then begin
    temp     = fstat(unit)
    filesize = temp.size 
    n_stacks = (filesize / (4L * long(nx) * long(ny) * long(nz)))
endif else begin
    filesize = 0L
    n_stacks = 1L
endelse

;-------------------------------------
;Don't need to use ASSOCIATED I/O.
;There is no advantage for writing
;since all file access is sequential.
;-------------------------------------
;data_type = 'FLOAT'
;stacks = assoc(unit, fltarr(nx, ny, nz))
;if (WRITE) then stacks[0] = fltarr(nx, ny, nz)

END;  Open_RT3_File
;********************************************************************
pro Get_Flow_Codes, codes, map, nx, ARC=ARC   ;** ,incs, opps

;-------------------------------------------
;NOTES:  RT flow codes  = | 64 128 1 |
;                         | 32  x  2 |
;                         | 16  8  4 |

;        ARC/INFO codes = | 32 64 128 |
;                         | 16  x   1 |
;                         |  8  4   2 |
;-------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if NOT(keyword_set(ARC)) then begin
     codes = [1,   2,  4,   8, 16, 32, 64, 128]
     ;opps  = [16, 32, 64, 128,  1,  2,  4,   8]
endif else begin
     codes = [128,  1,  2,  4,   8, 16, 32, 64]
     ;opps  = [  8, 16, 32, 64, 128,  1,  2,  4]
endelse

incs = long([-nx+1, 1, nx+1, nx, nx-1, -1, -nx-1, -nx])

map  = lonarr(129)
map[codes] = incs

END;  Get_Flow_Codes
;********************************************************************
pro Convert_Flow_Grid, infile, outfile, itype, otype, $
                       byte_order, icodes, ocodes, $
                       REPORT=REPORT

;--------------------------------------------------------
;NOTES:  The input filename "infile" and output filename
;        "outfile" must be in single quotes.

;        Any flow codes other than incodes get mapped
;        to zero.

;        Conversion is applied in blocks for speed.

;        RiverTools flow codes are:
;        codes = [1,   2,  4,   8, 16, 32, 64, 128]

;        ARC/INFO flow codes are:
;        codes = [128,  1,  2,  4,   8, 16, 32, 64]

;        TOPAZ flow codes are:
;        codes = ???????????
;--------------------------------------------------------

REPORT = keyword_set(REPORT)

;----------------------------
;Is outfile same as infile ?
;----------------------------
if (infile eq outfile) then begin
    msg = [ $
    'SORRY, ', $
    ' ', $
    'You must use different names for the input ', $
    'and output files. ', $
    ' ']
    GUI_Error_Message, msg
    RETURN
endif

;------------------------------------------
;Default is to convert from ARC flow codes
;------------------------------------------
if (n_elements(icodes) eq 0) then begin
    icodes = [128, 1, 2, 4, 8, 16, 32, 64]
endif

;---------------------------------------
;Default is to convert to RT flow codes
;---------------------------------------
if (n_elements(ocodes) eq 0) then begin
    ocodes = [1, 2, 4, 8, 16, 32, 64, 128]
endif

;-----------------------------------
;Convert type strings to upper case
;-----------------------------------
itype = strupcase(itype)
otype = strupcase(otype)

;---------------
;Create the map
;---------------
map = bytarr(255)
map[icodes] = ocodes

;---------------
;Open the files
;---------------
TF_Get_LUN, iunit, infile
openr, iunit, infile,  $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
TF_Get_LUN, ounit, outfile
openw, ounit, outfile, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
print,'Writing new flow grid to ' + outfile + '...'

;-----------------------------------
;Compute number of blocks in infile
;-----------------------------------
blocksize = 1024L
temp      = fstat(iunit)
filesize  = temp.size
n_blocks  = (filesize  / blocksize)
rem_size  = (filesize mod blocksize)

if (REPORT) then begin
    print,' > blocksize = ' + TF_String(blocksize)
    print,' > filesize  = ' + TF_String(filesize)
    print,' > n_blocks  = ' + TF_String(n_blocks)
    print,' > rem_size  = ' + TF_String(rem_size)
    print,' >'
endif

;-----------------------
;Initialize block array
;-----------------------
case (itype) of
    'BYTE'    : block = Make_Array(blocksize, /byte)
    'INTEGER' : block = Make_Array(blocksize, /int)
    'LONG'    : block = Make_Array(blocksize, /long)
     ELSE     : block = Make_Array(blocksize, /byte)
endcase

;------------------------------
;Initialize last block array ?
;------------------------------
if (rem_size ne 0) then begin
    case (itype) of
        'BYTE'    : last_block = Make_Array(rem_size, /byte)
        'INTEGER' : last_block = Make_Array(rem_size, /int)
        'LONG'    : last_block = Make_Array(rem_size, /long)
         ELSE     : last_block = Make_Array(rem_size, /byte)
    endcase
endif

;---------------------------------
;Apply conversion, block-by-block
;---------------------------------
for k=1,n_blocks do begin
    readu, iunit, block
    block = map[block]    ;(block must have integer type)
    case (otype) of
        'BYTE'    : block = byte(block)
        'INTEGER' : block = fix(block)
        'LONG'    : block = long(block)
         ELSE     : dum=0
    endcase
    writeu, ounit, block
endfor
;----------------
;Remainder block
;----------------
if (rem_size ne 0) then begin
    readu, iunit, last_block
    last_block = map[last_block]
    case (otype) of
        'BYTE'    : last_block = byte(last_block)
        'INTEGER' : last_block = fix(last_block)
        'LONG'    : last_block = long(last_block)
         ELSE     : dum=0
    endcase
    writeu, ounit, last_block
endif

;----------------
;Close the files
;----------------
print,'Finished.'
print,' '
free_lun, iunit, ounit

END;  Convert_Flow_Grid
;*****************************************************************
function Parent_IDs, grid, NON_PARENTS=non_parents

;---------------------------------------
;NB!  The use of 0's here is important.
;     If iterating, p[0]=0.
;---------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,-1 

;-------------------------
;Get flow grid dimensions
;-------------------------
s = size(grid, /dimensions)
nx = s[0]
ny = s[1]


;-----------------------------
;Get RiverTools D8 flow codes
;-----------------------------
Get_Flow_Codes, codes, map, nx
pIDs = lindgen(nx,ny) + map[grid]

;-------------------------------------
;Pixels with invalid flow directions,
;like edges, get assigned a pID of 0.
;-------------------------------------
wbad = where(grid le 0b, nw)
if (nw ne 0) then pIDs[wbad] = 0L

;-------------------------------------
;Return the IDs of non-parent pixels,
;such as ridges, but exclude pixels
;with flow code of 0, such as edges
;and nodata pixels.
;-------------------------------------
base = bytarr(nx,ny)
base[pIDs] = 1b
base[wbad] = 1b
wnot = where(base eq 0b, nnot)
if (nnot ne 0) then non_parents=wnot else non_parents=-1L

RETURN, pIDs

END;    Parent_IDs
;***************************************************************
pro Get_Flux_Indices, grid, w1,w2,w3,w4,w5,w6,w7,w8, $
                      n1,n2,n3,n4,n5,n6,n7,n8, $
                      w0,n0

;--------------------------------------------------------
;NOTES:  This procedure returns the 4-byte long-integer
;        array IDs of pixels that flow in a particular
;        direction.  RiverTools flow codes are assumed.
;--------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status

Check_Error_Status, status, OK
if NOT(OK) then RETURN

w1 = where(grid eq   1, n1)   ;(northeast)
w2 = where(grid eq   2, n2)   ;(east)
w3 = where(grid eq   4, n3)   ;(southeast)
w4 = where(grid eq   8, n4)   ;(south)
w5 = where(grid eq  16, n5)   ;(southwest)
w6 = where(grid eq  32, n6)   ;(west)
w7 = where(grid eq  64, n7)   ;(northwest)
w8 = where(grid eq 128, n8)   ;(north)

w0 = where(grid le 0, n0)     ;(undefined)

END;  Get_Flux_Indices
;***************************************************************
function Flow_Widths, grid, RTI_file, METERS=METERS, $
                      DOUBLE=DOUBLE, METHOD2=METHOD2

;-------------------------------------------------------------
;NOTES:  This routine returns the flow widths for each
;        pixel in the DEM.  The extra width of flowing to a
;        diagonal pixel is taken into account, as well as the
;        lat/lon-dependence for DEMs with fixed-angle pixels
;        (Geographic lat/lon).

;        METHOD2 version ensures that the sum of all flow
;        widths around a pixel is equal to 2*(dx + dy), but
;        is incorrect for case of a plane and others.

;        Flow widths are zero where (flow grid eq 0).
;-------------------------------------------------------------


;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,-1

METERS  = keyword_set(METERS)
DOUBLE  = keyword_set(DOUBLE)
METHOD2 = keyword_set(METHOD2)

;-------------------------
;Get flow grid dimensions
;-------------------------
s = size(grid, /dimensions)
nx = s[0]
ny = s[1]

Get_Pixel_Sizes, dx,dy,dd,da, RTI_file, METERS=METERS

;-----------------------
;Double or Float type ?
;-----------------------
if (DOUBLE) then begin
    dw = dblarr(nx,ny)
endif else begin
    dw = fltarr(nx,ny)
    dx = float(dx)
    dy = float(dy)
    dd = float(dd)
endelse

for row=0L,(ny - 1L) do begin
    g = grid[*,row]
    ;--------------
    ;Diagonal flow
    ;--------------
    wd = where((g eq 1) OR (g eq 4) OR (g eq 16) OR (g eq 64), nwd)
    if (nwd ne 0) then begin
        if NOT(METHOD2) then begin
            dw[wd, row] = dd[row]
        endif else begin
            dw[wd, row] = (dx[row] + dy[row])/4
        endelse
    endif

    ;-------------------
    ;East and west flow
    ;-------------------
    wh = where((g eq 2) OR (g eq 32), nwh)
    if (nwh ne 0) then begin
        dw[wh, row] = dy[row]
        if (METHOD2) then dw[wh,row ] = dw[wh,row]/2
    endif

    ;---------------------
    ;North and south flow
    ;---------------------
    wv = where((g eq 8) OR (g eq 128), nwv)
    if (nwv ne 0) then begin
        dw[wv, row] = dx[row]
        if (METHOD2) then dw[wv,row ] = dw[wv,row]/2
    endif

endfor

RETURN, dw
END;    Flow_Widths
;***************************************************************
function Flow_Lengths, grid, RTI_file, METERS=METERS, $
                       DOUBLE=DOUBLE

;-------------------------------------------------------------
;NOTES:  This routine returns the flow lengths for each
;        pixel in the DEM.  The extra length of flowing to a
;        diagonal pixel is taken into account, as well as the
;        latitude-dependence for DEMs with fixed-angle pixels
;        (Geographic lat/lon).

;        The only difference between this and the Flow_Widths
;        function is that roles of dx and dy are switched.

;        Flow lengths are set to dx[0] for the pixels where
;       (flow grid eq 0), such as on the edges of the DEM.
;-------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,-1

METERS = keyword_set(METERS)
DOUBLE = keyword_set(DOUBLE)

;-------------------------
;Get flow grid dimensions
;-------------------------
s = size(grid, /dimensions)
nx = s[0]
ny = s[1]

;-------------------------------------
;Use next line to test error handlers
;-------------------------------------
;;Get_Pixel_Sizes, dx,dy,dd, RTI_file, METERS=METERS

Get_Pixel_Sizes, dx,dy,dd,da, RTI_file, METERS=METERS

;-----------------------
;Double or Float type ?
;-----------------------
if (DOUBLE) then begin
    ds = dblarr(nx,ny)
endif else begin
    ds = fltarr(nx,ny)
    dx = float(dx)
    dy = float(dy)
    dd = float(dd)
endelse

;--------------------------------------------
;Initialize to default value that is used
;for pixels with flow code of zero.  This is
;done to avoid "divide by zero" errors.
;--------------------------------------------
ds = ds + dx[0]

for row=0L,(ny - 1L) do begin
    g = grid[*,row]
    ;--------------
    ;Diagonal flow
    ;--------------
    wd = where((g eq 1) OR (g eq 4) OR (g eq 16) OR (g eq 64), nwd)
    if (nwd ne 0) then ds[wd, row] = dd[row]

    ;-------------------
    ;East and west flow
    ;-------------------
    wh = where((g eq 2) OR (g eq 32), nwh)
    if (nwh ne 0) then ds[wh, row] = dx[row]

    ;---------------------
    ;North and south flow
    ;---------------------
    wv = where((g eq 8) OR (g eq 128), nwv)
    if (nwv ne 0) then ds[wv, row] = dy[row]
endfor

RETURN, ds
END;    Flow_Lengths
;***************************************************************
pro Get_FS_Slope, d, pIDs, ds, gS_bed, gS_free

;----------------------------------------------------
;NOTES:  This routine assumes that:

;        (1) flow is hydrostatic,
;        (2) the flow direction is always given by
;            the flow grid, and
;        (3) the magnitude of the free-surface slope
;        is given by the difference in depth between
;        a pixel and its parent, divided by the
;        distance between the pixel and its parent.

;        Using the bed-surface slope in an earlier
;        version led to "isolated" pixels with very
;        large depths.

;        If a pixel's depth is greater than it's
;        parent's depth, then want the slope to
;        be higher, so flow toward parent will be
;        more rapid.
;----------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN 

;-----------------------------------
;1st try: Failed at time 3100

;-----------------------------------

;(Might work if z0 constant vs. f.)
;-----------------------------------
;diffs = (d - d(PIDs)) > 0
;gS_free   = gS_bed + (9.81d * (diffs / ds))

;------------------------------------
;Compute difference in depth between
;each pixel and the one it flows to.
;------------------------------------
diffs = (d - d[pIDs])
gS_free   = gS_bed + (9.81d * (diffs / ds))

;---------------------
;Is this justified ?    ;************************************
;---------------------
gS_free = (gS_free > 0D)   ;(exclude negative slopes)
;** gS_free = (gS_free < 98D)  ;(exclude really big (g * slopes))

;------------
;Check diffs
;------------
;w = where(diffs lt 0, nw)
;if (nw ne 0) then begin
;   NWSTR = strtrim(string(nw),2)
;   print,'--------------------------------------'
;   print,'WARNING:  Depth decreases downstream'
;   print,'          at ' + NWSTR + ' locations.'
;   print,'--------------------------------------'
;   print,' '
;endif

END;  Get_FS_Slope
;***************************************************************
pro Get_FS_Slope2, d, pIDs, ds, S_bed, S_free

;----------------------------------------------------

;NOTES:  This routine assumes that:

;        (1) flow is hydrostatic,
;        (2) the flow direction is always given by
;            the flow grid, and
;        (3) the magnitude of the free-surface slope
;        is given by the difference in depth between
;        a pixel and its parent, divided by the
;        distance between the pixel and its parent.

;        Using the bed-surface slope in an earlier
;        version led to "isolated" pixels with very
;        large depths.

;        If a pixel's depth is greater than it's
;        parent's depth, then want the slope to
;        be higher, so flow toward parent will be
;        more rapid.
;----------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;------------------------------------
;Compute difference in depth between
;each pixel and the one it flows to.
;------------------------------------
diffs  = (d - d[pIDs])
S_free = S_bed + (diffs / ds)

;---------------------
;Is this justified ?    ;************************************
;---------------------
S_free = (S_free > 0D)   ;(exclude negative slopes)
;** S_free = (S_free < 98D)  ;(exclude really big (g * slopes))

;------------
;Check diffs
;------------
;w = where(diffs lt 0, nw)
;if (nw ne 0) then begin
;   NWSTR = strtrim(string(nw),2)
;   print,'--------------------------------------'
;   print,'WARNING:  Depth decreases downstream'
;   print,'          at ' + NWSTR + ' locations.'
;   print,'--------------------------------------'
;   print,' '
;endif

end;  Get_FS_Slope2
;***************************************************************
function Courant_Condition_OK, dx, dt, vmax

;-------------------------------------------------------------
;Notes:  This condition for numerical stability dictates that
;        the maximum distance travelled by water anywhere in
;        the basin in one timestep (vmax * dt) must be less
;        than one pixel width (dx).

;        vmax will often be an estimate.
;        vmax has units of meters/second.
;        dx has units of meters & dt has units of seconds.

;        Typically, vmax will have a built-in factor
;        of safety, so set factor = 1d here.

;NB!     We must use LE vs. LT here and the same factor
;        as in Stable_Timestep function.
;-------------------------------------------------------------
factor = 1d

OK = ((vmax * dt * factor) le dx)

RETURN, OK

end;  Courant_Condition_OK
;***************************************************************
function Stable_Timestep, dx, vmax

;-----------------------------------------------------
;Notes:  See notes for Courant_Condition_OK function.
;        Typically, vmax will have a built-in factor
;        of safety, so set factor = 1d here.
;-----------------------------------------------------
factor = 1d

dt = dx / (vmax * factor)

RETURN, dt

end;  Stable_Timestep
;***************************************************************
function TF_Tan, angle

;---------------------------------------------------------
;Notes:  IDL for 64-bit Linux has problem with the TAN
;        function so use SIN()/COS() instead  (B. Bolton)

;        Seems to only be a problem for 64-bit Linux, so
;        this function tests for that platform and uses
;        TAN() for all other platforms.
;---------------------------------------------------------
LINUX_OS = (strupcase(!version.os) eq 'LINUX')

if (LINUX_OS) then begin
    RETURN, sin(angle) / cos(angle)
endif else begin
    RETURN, tan(angle)
endelse

end;  TF_Tan
;***************************************************************
pro TF_Get_LUN, unit, filename

;-------------------------------------------------------------
;Note:  IDL's GET_LUN procedure can only allocate file unit
;       numbers in the range 100 to 128, even if there are
;       unused unit numbers between 1 and 99.  This leads to
;       a problem for more complex model runs that utilize a
;       large number of input files (6/5/06).

;       Replace:  openr, unit, filename, /get_lun
;          with:  TF_Get_Lun, unit, filename
;                 openr, unit, filename
;-------------------------------------------------------------
on_ioerror, err_num
OK = 0b
get_lun, unit
OK = 1b   ;(we won't get here if GET_LUN throws an error)

;----------------------------------------
;GET_LUN can't find a free file unit, so
;we try to find one between 33 and 100.
;----------------------------------------
err_num : if NOT(OK) then begin
    unit = 33L
    while NOT(OK) do begin
        info = fstat(unit)
        if (info.open eq 0b) then begin
            ;------------------------------
            ;This file unit number is free
            ;------------------------------
            OK = 1b
        endif else begin
            unit = (unit + 1L)
        endelse

        if (unit eq 100) then begin
            OK = 1b
            TF_Print,'***************************************'
            TF_Print,'  ERROR:  All file units are in use.'
            TF_Print,'***************************************'
            TF_Print,' '
        endif
    endwhile
endif

end;  TF_Get_LUN
;***************************************************************

