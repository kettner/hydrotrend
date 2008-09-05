
;*****************************************************************
;   GUI_utils_tf.pro

;   Copyright (c) 2001-2008, Scott D. Peckham
;   Created:   Dec 2001 - Jan 2002
;   Modified:  Jul-Aug 2005
;   Modified:  Mar 2007
;   Modified:  Mar 2008  (Initialize_Hydrograph_Window)

;*****************************************************************

;   Create_TLB
;   Realize_TLB
;   Get_TLB_State
;   Get_Event_Uvalue
;   Str_Pad  (function)
;   Str_Resize (function)   (10/8/07)
;   CW_Button_Bar
;   Align_Text_Boxes
;   Equate_Widget_Sizes     (10/8/07)
;   Read_Text_Box
;   Read_Input_Type

;   Model_Input_Types  (function)
;   Import_Table_From_File
;   Save_Table_To_File
;   Close_Dialog

;   GUI_Help_event     (8/26/05)
;   GUI_Help

;   GUI_Message_event
;   GUI_Message        (function)
;   GUI_Error_Message

;   File_Found         (function)
;   Check_Overwrite

;   Read_BMP_for_Mac   (July 2005; needed for GUI_Navigate)

;   Set_Grid_Name      (currently unused, see Notes)

;   Check_Interrupt    (Mar 2007, used to stop model runs)

;   Initialize_Hydrograph_Window   (separate, 3/14/08)

;*****************************************************************
pro Create_TLB, MB, MBAR_ID, TITLE=TITLE, LEADER=LEADER, $
                ROW=ROW, COLUMN=COLUMN, SIZEABLE=SIZEABLE, $
                MODAL=MODAL, ADD_MBAR=ADD_MBAR

;----------------------------------------------------------
;Notes:  TLB stands for Top-Level Base.

;        The MBAR_ID argument is optional and is used only
;        if the ADD_MBAR keyword is set to return the ID
;        of the menu bar.

;        XOFFSET and YOFFSET are arguments to the routine
;        called Realize_TLB.
;----------------------------------------------------------
if NOT(keyword_set(LEADER)) then LEADER=0L
if NOT(keyword_set(TITLE)) then TITLE='Dialog'
ROW      = keyword_set(ROW)
COLUMN   = keyword_set(COLUMN)
SIZEABLE = keyword_set(SIZEABLE)
MODAL    = keyword_set(MODAL)
FLOATING = keyword_set(FLOATING)
ADD_MBAR = keyword_set(ADD_MBAR)

VALID_LEADER = widget_info(LEADER, /VALID_ID)
TLB_ATT_CODE = (1 - SIZEABLE)

if (VALID_LEADER) then begin
    if (ADD_MBAR) then begin
        MB = widget_base(TITLE=TITLE, ROW=ROW, COLUMN=COLUMN, $
                         MAP=0, SCROLL=0, FLOATING=FLOATING, $
                         GROUP_LEADER=LEADER, MBAR=MBAR_ID, $
                         TLB_FRAME_ATTR=TLB_ATT_CODE, $
                         TLB_SIZE_EVENTS=SIZEABLE, MODAL=MODAL)
    endif else begin
        MB = widget_base(TITLE=TITLE, ROW=ROW, COLUMN=COLUMN, $
                         MAP=0, SCROLL=0, FLOATING=FLOATING, $
                         GROUP_LEADER=LEADER, $
                         TLB_FRAME_ATTR=TLB_ATT_CODE, $
                         TLB_SIZE_EVENTS=SIZEABLE, MODAL=MODAL)
    endelse
endif else begin
    ;-----------------------------------------------------
    ;Avoid any use of FLOATING and GROUP_LEADER keywords.
    ;-----------------------------------------------------
    if (ADD_MBAR) then begin
        MB = widget_base(TITLE=TITLE, ROW=ROW, COLUMN=COLUMN, $
                         MAP=0, SCROLL=0, MBAR=MBAR_ID, $
                         TLB_FRAME_ATTR=TLB_ATT_CODE, $
                         TLB_SIZE_EVENTS=SIZEABLE, MODAL=MODAL)
    endif else begin
        MB = widget_base(TITLE=TITLE, ROW=ROW, COLUMN=COLUMN, $
                         MAP=0, SCROLL=0, $
                         TLB_FRAME_ATTR=TLB_ATT_CODE, $
                         TLB_SIZE_EVENTS=SIZEABLE, MODAL=MODAL)
    endelse
endelse

END;  Create_TLB  
;*****************************************************************
pro Realize_TLB, MB, state, handler, SILENT=SILENT, $
                 LEADER=LEADER, TW=TW, ATW=ATW, $
                 XOFFSET=XOFFSET, YOFFSET=YOFFSET, $
                 MODAL=MODAL, CENTER=CENTER, RIGHT=RIGHT, $
                 NO_BLOCK=NO_BLOCK, $
                 CLEANUP=CLEANUP

;------------------------------------------------------------
;NOTE: The variable "handler" is the 1st part of the event
;      handler routine's name.

;      It is assumed that every main base
;      is created with the setting MAP=0, and that it has
;      to be mapped when it is realized.  Otherwise, the
;      base must move from the upper left corner to its
;      mapped position.

;      If the TW keyword is set, its value is taken to be
;      the ID of a text widget and this text widget is
;      made un-editable.  This allows them to have white
;      (vs. gray) backgrounds, even if they aren't editable.

;      NO_BLOCK must equal same value for all dialogs ??
;      Here, it is hard-wired to 1b.  This allows the IDL
;      prompt to be used at the same time.
;------------------------------------------------------------

;---------
;Defaults
;---------
if NOT(keyword_set(LEADER))  then LEADER=0L
if NOT(keyword_set(XOFFSET)) then XOFFSET=100 
if NOT(keyword_set(YOFFSET)) then YOFFSET=50
MODAL    = keyword_set(MODAL)
;** NO_BLOCK = keyword_set(NO_BLOCK)
NO_BLOCK = 1b

;---------------------------
;Get the size of the screen
;---------------------------
device, get_screen_size=screen_size
screen_xsize = screen_size[0]
screen_ysize = screen_size[1]

;----------------------------------
;Center the top-level base (TLB) ?
;----------------------------------
if (keyword_set(CENTER)) then begin
    ;--------------------------------------
    ;This is not a true centering, since
    ;dialog dimensions are only estimated. 
    ;--------------------------------------
    XOFFSET = ((screen_xsize - 300)/2 > XOFFSET)
    YOFFSET = ((screen_ysize - 250)/2 > YOFFSET)
endif

;-------------------------------
;Position TLB on the right side
;(Default is on the left.)
;-------------------------------
if (keyword_set(RIGHT)) then begin
    XOFFSET=((screen_xsize - 300) > XOFFSET)
endif

;------------------------
;Realize and map the TLB
;------------------------
if NOT(MODAL) then begin
    widget_control, MB, set_uvalue=state, /REALIZE, /MAP, $
                    XOFFSET=XOFFSET, YOFFSET=YOFFSET, $
                    /CLEAR_EVENTS ;***, /MANAGED
endif else begin
    widget_control, MB, set_uvalue=state, /REALIZE, $
                    XOFFSET=XOFFSET, YOFFSET=YOFFSET, $
                    /CLEAR_EVENTS ;***, /MANAGED
endelse

;----------------------
;Assign a group leader
;----------------------
LEADER_OK = widget_info(LEADER, /VALID_ID)
if (LEADER_OK) then widget_control,MB,GROUP_LEADER=leader

;----------------------------------
;Trick to make a non-editable text
;widget white on Windows OS
;----------------------------------
if (keyword_set(TW)) then begin
    ntw = n_elements(TW)
    if (ntw gt 1) then begin
        for k=0,(ntw-1) do widget_control,TW[k],EDITABLE=0

    endif else widget_control,TW,EDITABLE=0
endif
if (keyword_set(ATW)) then widget_control,ATW,EDITABLE=0

;----------------------------------
;Use WIDGET_EVENT to handle events
;----------------------------------
;result = widget_event()

;------------------------------
;Use XMANAGER to handle events
;------------------------------
;NO_BLOCK = 0b;  (Need for IDL Virtual Machine ??)
if (NO_BLOCK) then $
     xmanager, handler, MB, /NO_BLOCK $
else xmanager, handler, MB

;if NOT(keyword_set(CLEANUP)) then cleanup='Remove_Dialog'
;if (NO_BLOCK) then $
;     xmanager, handler, MB, CLEANUP=cleanup, /NO_BLOCK $
;else xmanager, handler, MB, CLEANUP=cleanup

END;  Realize_TLB
;******************************************************************
pro Get_TLB_State, base_ID, state, ALIVE

if (n_elements(base_ID) eq 0) then begin
    ALIVE=0b  &  RETURN
endif

;----------------------------
;This is necessary sometimes
;----------------------------
base_ID = long(base_ID)

ALIVE = widget_info(base_ID, /VALID_ID)
if (ALIVE) then begin
    widget_control, base_ID, get_uvalue=state
endif
 
END;  Get_TLB_State
;*****************************************************************
pro Get_Event_Uvalue, event, uvalue, state

widget_control, event.top, get_uvalue=state

if (event.id ne event.top) then begin
    widget_control, event.id, get_uvalue=uvalue
endif else begin
    ;-----------------------
    ;Check for KILL request
    ;-------------------------------------------------
    ;Does nothing unless the TLB_KILL_REQUEST_EVENTS
    ;keyword was set when the TLB was created.
    ;-------------------------------------------------
    if (tag_names(event, /structure_name) eq $
        'WIDGET_KILL_REQUEST') then begin
         uvalue = 'KILL'
         RETURN
    endif

endelse

;----------------------------
;Make sure uvalue is defined
;----------------------------
if (n_elements(uvalue) eq 0) then uvalue = 'NULL'

END;  Get_Event_Uvalue
;******************************************************************
function Str_Pad, str, n, RHS=RHS

RHS = keyword_set(RHS)  ;(pad only on right-hand side?)

if (n_elements(n) eq 0) then n=2L
nn = n_elements(n)

if (nn eq 1) then begin
   pad  = string(replicate(32b, n))
   if (RHS) then pstr = str + pad + pad $
            else pstr = pad + str + pad
endif else begin
   pstr = strarr(nn)
   for k=0,(nn-1) do begin
       pad = string(replicate(32b, n[k]))
       if (RHS) then pstr[k] = str[k] + pad + pad $
                else pstr[k] = pad + str[k] + pad
   endfor
endelse

RETURN, pstr
END;    Str_Pad
;******************************************************************
function Str_Resize, str, len, RHS=RHS

;---------------------------------------------------------------
;Note:  This function pads "str" to have the specified length.
;       It works even if "str" is a string array.  In that
;       case the padding parameter, n, is an array.
;       Set the RHS keyword to pad only on the right-hand side.
;---------------------------------------------------------------
RHS = keyword_set(RHS)
olen = strlen(str)       ;(maybe an array)
n = (len - olen) / 2     ;(maybe an array)

RETURN, Str_Pad(str, n > 1, RHS=RHS)

END;    Str_Resize
;******************************************************************
pro CW_Button_Bar, MB, START=START, OK=OK, BROWSE=BROWSE, $
                   HELP=HELP, CLOSE=CLOSE, CANCEL=CANCEL, $
                   OTHER=OTHER, SPACE=SPACE

if NOT(keyword_set(SPACE)) then SPACE=7

A = widget_base(MB, /ROW, SPACE=SPACE)

if (keyword_set(START)) then $
    A1 = widget_button(A, VALUE=Str_Pad('Start'), UVALUE='START')
if (keyword_set(OK)) then $
    A2 = widget_button(A, VALUE=Str_Pad('OK'), UVALUE='OK')
if (keyword_set(BROWSE)) then $
    A3 = widget_button(A, VALUE=Str_Pad('Browse'), UVALUE='BROWSE')
if (keyword_set(HELP)) then $
    A4 = widget_button(A, VALUE=Str_Pad('Help'), UVALUE='HELP')

;-------------------------------
;Option for user-defined button
;-------------------------------
if (n_elements(OTHER) ne 0) then begin
    b = byte(OTHER)
    ;--------------------------------
    ;Replace spaces with underscores
    ;--------------------------------
    w = where(b eq 32b, nw)
    if (nw ne 0) then b[w]=95b
    UVALUE = strupcase(string(b))
    OB = widget_button(A, VALUE=Str_Pad(OTHER), UVALUE=UVALUE)
endif

if (keyword_set(CLOSE)) then $
    A5 = widget_button(A, VALUE=Str_Pad('Close'), UVALUE='CLOSE')
if (keyword_set(CANCEL)) then $
    A6 = widget_button(A, VALUE=Str_Pad('Cancel'), UVALUE='CANCEL')

END;  CW_Button_Bar
;*****************************************************************
pro Align_Text_Boxes, label_IDs, MIN_SIZE=MIN_SIZE

;---------------------------------------------------------------
;NOTES:  This routine is for aligning a column of
;        text widgets in a dialog box as the dialog is being
;        created.  The idea is to get the size of the label
;        to the left of the box, and then to resize this label
;        to the max size of any of the labels.  This method
;        appears to work across IDL-supported platforms.

;        10/21/00.  Added MIN_SIZE keyword for Reach Info tool.
;---------------------------------------------------------------

if NOT(keyword_set(MIN_SIZE)) then MIN_SIZE=0L

;--------------------------
;Get size of longest label
;--------------------------
xs = MIN_SIZE
n  = n_elements(label_IDs)
for k=0L, (n - 1L) do begin
    gg = widget_info(label_IDs[k], /geometry)
    xs = (xs > gg.scr_xsize)
endfor

;------------------------------------
;Resize label widgets to align boxes
;------------------------------------
for k=0L, (n - 1L) do begin
    widget_control, label_IDs[k], scr_xsize=xs
endfor

END;  Align_Text_Boxes
;*****************************************************************
pro Equate_Widget_Sizes, IDs

;-----------------------------------------------------------------
;NOTES:  This routine is for making several widgets have the
;        same screen xsize and ysize.  For example, it is used
;        to make all of the wizard panel bases in TopoFlow the
;        same size.  This also makes the Back and Next buttons
;        line up between panels.

;        This method may not work across IDL-supported platforms.
;-----------------------------------------------------------------
xs = 1
ys = 1
n  = n_elements(IDs)

;---------------------------
;Get size of biggest widget
;---------------------------
for k=0L, (n - 1L) do begin
    gg = widget_info(IDs[k], /geometry)
    xs = (xs > gg.scr_xsize)
    ys = (ys > gg.scr_ysize)
endfor

;--------------------------------------
;Make all of the widgets the same size
;--------------------------------------
for k=0L, (n - 1L) do begin
    widget_control, IDs[k], scr_xsize=xs
    widget_control, IDs[k], scr_ysize=ys
endfor

END;  Equate_Widget_Sizes
;*****************************************************************
pro Read_Text_Box, widget_ID, value, OK, $
              TYPE=TYPE, BYTE=BYTE, LONG=LONG, $
              INTEGER=INTEGER, FLOAT=FLOAT, $
              DOUBLE=DOUBLE, FILENAME=FILENAME, $
              TEXT=TEXT, STRING=STRING, $
              ARRAY=ARRAY  ;*** (2/16/04) ***

;-----------------------------------------------------
;Note:  The TEXT & STRING keywords do the same thing.
;       and are handled by the ELSE branch.
;-----------------------------------------------------

;------------------------
;What is the data type ?
;------------------------
if (keyword_set(BYTE))     then type='BYTE'     else $
if (keyword_set(INTEGER))  then type='INTEGER'  else $
if (keyword_set(LONG))     then type='LONG'     else $
if (keyword_set(FLOAT))    then type='FLOAT'    else $
if (keyword_set(DOUBLE))   then type='DOUBLE'   else $
if (keyword_set(FILENAME)) then type='FILENAME' else $
    type='STRING'  ;(default)

;-------------------------------
;Read text from the text widget
;-------------------------------
widget_control, widget_ID, get_value=v
if NOT(keyword_set(ARRAY)) then begin
    text = strtrim(v[0], 2)
    BLANK = (strlen(text) eq 0)
endif else begin
    text = strtrim(v, 2)
    BLANK = (max(strlen(text)) eq 0)
endelse

;----------------------------
;What if text box is blank ?
;----------------------------
if (BLANK) then begin
    case (type) of
        'BYTE'     : word = 'Byte integer'
        'INTEGER'  : word = 'Integer'
        'LONG'     : word = 'Integer'
        'FLOAT'    : word = 'Number'
        'DOUBLE'   : word = 'Number'
        'FILENAME' : word = 'Filename'
         ELSE      : word = 'Text'
    endcase

    msg = ['ERROR: ', ' ', $
    word + ' expected in blank text box. ']
    GUI_Error_Message, msg
    value = text
    OK = 0b
    RETURN
endif

;---------------------------------
;Try to convert to specified type
;---------------------------------
case (type) of
    'BYTE'     : value = byte(text)
    'INTEGER'  : value = fix(text)
     'LONG'    : value = long(text)
    'FLOAT'    : value = float(text)
   'DOUBLE'    : value = double(text)
      ELSE     : value = text
endcase

;--------------------------
;Did the conversion work ?
;--------------------------
OK = (!err ne -96)

END;  Read_Text_Box
;*****************************************************************
pro Read_Input_Type, type, box_ID, value, OK, filename=filename

;---------------------------------------------------------
;Notes:  Modified to read scalar DOUBLE from text box
;        instead of FLOAT.  Without this, a number like
;        0.5 entered by a user can change to 0.50000001.
;        Any side effects ?? (7/7/06)
;---------------------------------------------------------
FORWARD_FUNCTION File_Found

case (type) of

;*******
0b : $
;*******
begin
;--------------------------
;Read scalar from text box
;--------------------------
filename=''
Read_Text_Box, box_ID, value, OK, /DOUBLE
;*** Read_Text_Box, box_ID, value, OK, /FLOAT
if NOT(OK) then RETURN
end

;*********
ELSE : $
;*********
begin
;----------------------------
;Read filename from text box
;----------------------------
value = 0d
Read_Text_Box, box_ID, filename, OK, /FILE
if NOT(OK) then RETURN
OK = File_Found(filename)
if NOT(OK) then RETURN
end

endcase

END;  Read_Input_Type
;*****************************************************************
function Model_Input_Types

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, -1L

types = ['Scalar', 'Time series', 'Grid', 'Grid sequence']
types = Str_Pad(types, 1)

RETURN, types
END;    Model_Input_Types
;*****************************************************************
pro Import_Table_From_File, table_ID, ncols, FILE=FILE, $
                 COL1_SCALE=COL1_SCALE, COL2_SCALE=COL2_SCALE

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if NOT(keyword_set(FILE)) then FILE=''
TITLE = 'Select data file to read: '
filename = dialog_pickfile(FILE=FILE, /MUST_EXIST, TITLE=TITLE)
if (filename eq '') then RETURN

;------------------------------
;Read the table data from file
;------------------------------
Count_Lines, nlines, filename, /SILENT
table = fltarr(ncols, nlines)
TF_Get_LUN, unit, filename
openr, unit, filename 
readf, unit, table
free_lun, unit

;---------------------------
;Scale values in column 1 ?
;---------------------------
if (keyword_set(COL1_SCALE)) then begin
    table[0,*] = COL1_SCALE * table[0,*]
endif

;---------------------------
;Scale values in column 2 ?
;---------------------------
if (keyword_set(COL2_SCALE)) then begin
    table[1,*] = COL2_SCALE * table[1,*]
endif 

;-------------------------------------
;Copy the data into the table
;Can original table size be changed ?
;-------------------------------------
widget_control, table_ID, set_value=table

;----------------
;Message to user
;----------------
msg = [ $
'Finished reading table from: ', $
filename, ' ']
result = GUI_Message(msg, /INFO)

;** print, table

END;  Import_Table_From_File
;*****************************************************************
pro Save_Table_To_File, table_ID, filename, FILE=FILE, $
               COL1_SCALE=COL1_SCALE, COL2_SCALE=COL2_SCALE, $
               REMOVE_ZEROS=REMOVE_ZEROS

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if NOT(keyword_set(FILE)) then FILE=''
REMOVE_ZEROS = keyword_set(REMOVE_ZEROS)

TITLE = 'Select data file to write: '
filename = dialog_pickfile(TITLE=TITLE, FILE=FILE)
if (filename eq '') then RETURN
Check_Overwrite, filename, OK
if NOT(OK) then RETURN

;** print,'filename = ',filename

;----------------------------
;Read data from table widget
;----------------------------
widget_control, table_ID, get_value=table

;---------------------------
;Scale values in column 1 ?
;---------------------------
if (keyword_set(COL1_SCALE)) then begin
    table[0,*] = COL1_SCALE * table[0,*]
endif

;---------------------------
;Scale values in column 2 ?
;---------------------------
if (keyword_set(COL2_SCALE)) then begin
    table[1,*] = COL2_SCALE * table[1,*]
endif

;---------------------------------------
;Remove rows with all zero entries ?
;(2/25/04) Not a good idea in general;
;make as a keyword option ?
;---------------------------------------
if (REMOVE_ZEROS) then begin
    s  = size(table, /dimensions)
    n  = s[0]
    nw = 0L
    case (n) of
        0 : RETURN
        1 : w = where((table[0,*] ne 0), nw)
        2 : w = where((table[0,*] ne 0)  AND $
                      (table[1,*] ne 0), nw)
     else : w = where((table[0,*] ne 0)  AND $
                      (table[1,*] ne 0)  AND $
                      (table[2,*] ne 0), nw)
    endcase
    if (nw ne 0) then table = table[*,w]
endif

;--------------------------
;Save the table to a file
;Method for wide printing.
;--------------------------
nt = n_elements(table[0,*])
TF_Get_LUN, unit, filename
openw, unit, filename 
for row=0L,(nt-1L) do begin
    printf, unit, table[*,row]
endfor
free_lun, unit

;-------------------------
;Save the table to a file
;Original, faster method.
;-------------------------
;TF_Get_LUN, unit, filename
;openw, unit, filename 
;printf, unit, table
;free_lun, unit

;----------------
;Message to user
;----------------
msg = [ $
'Finished saving table to: ', $
filename, ' ']
result = GUI_Message(msg, /INFO)

END;  Save_Table_To_File
;*****************************************************************
pro Close_Dialog, ID

;on_error, 2

VALID = widget_info(ID, /VALID_ID)
if (VALID) then widget_control,ID,/destroy

END;  Close_Dialog
;*****************************************************************
pro GUI_Help_event, event

;-----------
;Error trap
;-----------
CATCH, status
Trace_Error, status, event, OK, /ABORT
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state

case (uvalue) of

;*********
'OK' : $
;*********
Close_Dialog, event.top

;*************
ELSE : dum=0
;*************
endcase

end;  GUI_Help_event
;*****************************************************************
pro GUI_Help, msg, leader, TITLE=TITLE

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN 

if NOT(keyword_set(TITLE)) then title='TopoFlow Help'

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE=title, /COLUMN, LEADER=leader

;-------------------------
;Text widget with message
;-------------------------
msg2 = '      ' + msg
maxlen = max(strlen(msg2))
WT = widget_text(MB, VALUE=msg2, YSIZE=n_elements(msg2), $
                 XSIZE=maxlen, /ALIGN_LEFT, /EDITABLE)

;------------------------------
;Store info in state structure
;------------------------------
state = {base_ID: MB}

;-------------------
;Bottom button base
;-------------------
BB = widget_base(MB, /ROW, SPACE=7, /ALIGN_CENTER)
  BB1 = widget_button(BB, VALUE=Str_Pad('OK',4), UVALUE='OK', $
                      /ALIGN_CENTER)

;------------------------------------
;Realize widgets and wait for events
;------------------------------------
Realize_TLB, MB, state, 'GUI_Help', XOFF=250, YOFF=80

end;  GUI_Help
;*****************************************************************
pro GUI_Message_event, event

;-----------
;Error trap
;-----------
;Don't call error handler to avoid recursion
;;No_Catch, status
;;Check_Error_Status, status, OK
;;if NOT(OK) then RETURN,-1

;---------------------
;Located in this file
;---------------------
FORWARD_FUNCTION Str_Pad

Get_Event_Uvalue,event,uvalue,state 

if (event.ID eq event.top) then begin 
    geom  = widget_info(event.top, /geometry)
    xsize = (event.x - 2*geom.xpad)
    ysize = (event.y - 2*geom.ypad - 40)

    widget_control, state.text_ID, SCR_YSIZE=ysize, $
                    SCR_XSIZE=xsize
    RETURN
endif

case (uvalue) of
    'OK' : Close_Dialog, event.top
    else : dum=0
endcase

END;  GUI_Message_event
;****************************************************************
function GUI_Message, message, INFO=INFO, TITLE=TITLE, $
         QUESTION=QUESTION, CANCEL=CANCEL, ERROR=ERROR, $
         DEFAULT_CANCEL=DEFAULT_CANCEL, DEFAULT_NO=DEFAULT_NO

;---------------------------------------------------------------
;NOTES:  This routine is a wrapper around IDL's DIALOG_MESSAGE
;        routine, since that routine does not handle newlines
;        correctly on Macs.

;        Don't use the PAD function; not defined yet.

;        The dialog must be MODAL to prevent some problems,
;        as with the startup message about byte order on a Mac.
;        If it isn't MODAL, program execution continues while
;        user is reading the dialog message.  Note that resize
;        events on Mac aren't allowed for a modal dialog. 
;---------------------------------------------------------------

;-----------
;Error trap
;-----------
;Don't call error handler to avoid recursion
;;No_Catch, status
;;Check_Error_Status, status, OK
;;if NOT(OK) then RETURN,-1

if NOT(keyword_set(TITLE)) then TITLE='Information'

INFO     = keyword_set(INFO)
QUESTION = keyword_set(QUESTION)
CANCEL   = keyword_set(CANCEL)
ERROR    = keyword_set(ERROR)
DEFAULT_CANCEL = keyword_set(DEFAULT_CANCEL)
DEFAULT_NO     = keyword_set(DEFAULT_NO)

SPECIAL = (QUESTION OR CANCEL OR ERROR OR DEFAULT_CANCEL)
NOT_MAC = (!d.name ne 'MAC')

;-----------------
;FOR TESTING ONLY
;-----------------
;*** NOT_MAC = 0b

;------------------------------
;USE DIALOG_MESSAGE EXCEPT FOR
;INFO DIALOGS ON A MAC
;------------------------------
if (NOT_MAC OR SPECIAL) then begin
    RETURN, dialog_message(message, INFO=INFO, $
                   QUESTION=QUESTION, CANCEL=CANCEL, $
                   ERROR=ERROR, DEFAULT_NO=DEFAULT_NO, $
                   TITLE=TITLE)
endif

;--------------------------------------------
;Display a message dialgo that works on Macs
;--------------------------------------------
state = {text_ID:0L}
Create_TLB, MB, TITLE=TITLE, /COLUMN, /SIZEABLE, /MODAL

numlines = n_elements(message) + 1
maxlen   = max(strlen(message)) + 5
TW = widget_text(MB, VALUE=message, YSIZE=numlines, $
                 ;***** YSIZE=(numlines < 24), $
                 /EDITABLE, XSIZE=maxlen)
state.text_ID = TW

OK = widget_base(MB, /ROW, /ALIGN_CENTER)
  OK1 = widget_button(OK, VALUE='   OK   ', UVALUE='OK', $
                      /ALIGN_CENTER)

device, get_screen_size=screen_size
xscrsize = screen_size[0]
yscrsize = screen_size[1]
geom    = widget_info(MB, /geometry)
XOFFSET = (xscrsize - geom.scr_xsize) / 2
YOFFSET = (yscrsize - geom.scr_ysize) / 2

Realize_TLB, MB, state, 'GUI_Message', TW=TW, /MODAL, $
             XOFFSET=XOFFSET, YOFFSET=YOFFSET

RETURN, MB
END;    GUI_Message
;*****************************************************************
pro GUI_Error_Message, string_array, IDL_ERROR=IDL_ERROR

;---------------------------------------------------
;NOTE:  This new version prints the error string in
;       GUI_Message dialog. 
;---------------------------------------------------

if (keyword_set(IDL_ERROR)) then begin
    s = [string_array, $
         'ERROR MESSAGE FROM IDL: ', ' ', $
         !error_state.msg, ' ']
endif else begin
    s = string_array
endelse

result = GUI_Message(s, /ERROR)

END;  GUI_Error_Message
;*****************************************************************
function File_Found, filename, SILENT=SILENT

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,0b

;-----------------------------------------------------
;FILE_TEST was introduced in IDL 5.4 and is better
;than FINDFILE since it handles spaces in paths, etc.
;-----------------------------------------------------
SILENT = keyword_set(SILENT)
FOUND  = file_test(filename)

;** f  = findfile(filename, count=c)
;** FOUND = (c ne 0)

if NOT(FOUND) AND NOT(SILENT) then begin
    msg = [ $
    'Could not find the file: ', ' ', filename, ' ']
    result = GUI_Message(msg, /INFO)
endif

RETURN, FOUND
END;    File_Found
;*****************************************************************
pro Check_Overwrite, filename, OK

;--------------------------
;Does file already exist ?
;--------------------------
f = findfile(filename, count=c)
if (c eq 0) then begin
    OK=1b  &  RETURN
endif

;-----------------------------
;If so, can we overwrite it ?
;----------------------------- 
msg = [ ' ', $
'WARNING, ', ' ', $
'The current directory already contains', $
'a file with the name: ', $
' ', filename, ' ', $
'Do you really want to overwrite it?', ' ']

answer = GUI_Message(msg, /CANCEL, /DEFAULT_CANCEL)
OK = (answer eq 'OK')

;---------------------------------
;If OK, then test to see if it is
;possible to write to directory.
;e.g. Could be data on a CD-ROM.
;---------------------------------
if (OK) then begin
    TF_Get_LUN, unit, filename
    openw, unit, filename, ERROR=err
    if (n_elements(unit) ne 0) then free_lun,unit
    if (err ne 0) then begin
        OK = 0b
        msg = [ $
        'SORRY, ', ' ', $
        'No write permission at specified output location. ',$
        ' ',$
        'You may be able edit the output filepath or ',$
        'otherwise select another output location. ',$
        ' ']
        ;*** !ERROR_STATE.MSG ]
        result = GUI_Message(msg, /INFO)
        RETURN
    endif 
endif

END;  Check_Overwrite
;*****************************************************************
pro Read_BMP_for_Mac, BMP_file, image

;-----------------------------------------------------
;Notes:  Needed this to fix bug on Mac OS X (IDL 6.0)
;        This should work for all platforms.
;-----------------------------------------------------
im = read_bmp(BMP_file, /rgb)

;----------------------------
;Get dimensions of the image
;----------------------------
dims = size(im, /dimensions)
nx   = dims[1]
ny   = dims[2]

image = bytarr(nx, ny, 3)
for kc=0,2 do image[*,*,kc] = im[kc,*,*]

end;  Read_BMP_for_Mac
;*****************************************************************
pro Set_Grid_Name, box_ID, type_code, run_prefix, mstring

;------------------------------------------------------
;Notes:  This is currently unused but could be used to
;        write the default filename to text box when
;        type is changed from scalar to grid.
;------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

case (type_code) of
    0: widget_control, box_ID, set_value=''
    1: begin
       filename = (run_prefix + mstring + '.rtg')
       widget_control, box_ID, set_value=filename
       end
    2: dum=0
endcase

END;  Set_Grid_Name
;*****************************************************************
pro Check_Interrupt, STOP_ID, OK, SILENT=SILENT
                     ;*** START_MSG=start_msg, $
                     ;*** STOP_MSG=stop_msg, EDITABLE=EDITABLE

SILENT    = keyword_set(SILENT)
start_msg = ''
stop_msg  = ''
 
;-------------------
;Check for valid ID
;-------------------
OK=1b
VALID = widget_info(STOP_ID, /VALID_ID)
if NOT(VALID) then RETURN

;-----------------------------
;Need a short pause (3/20/07)
;-----------------------------
;0.001 doesn't work well
;-----------------------------
wait, 0.01    ;[seconds]

;-------------------------------
;Get current string in text box
;-------------------------------
widget_control, STOP_ID, GET_VALUE=value, /INPUT_FOCUS
                ;*** /KBRD_FOCUS_EVENTS
;*** print,'current string = ', value[0]

;--------------------------------------
;If user has pressed a key, then the
;current string will not be the same
;as the start_msg, so we stop the run.
;--------------------------------------
if (value[0] ne start_msg) then begin
    widget_control, STOP_ID, SET_VALUE=stop_msg, /EDITABLE
    ;--------------------------------
    ;Does user really want to quit ?
    ;--------------------------------
    msg = ['Are you sure you want to stop this model run?', ' ']
    answer = GUI_Message(msg, /QUESTION)
    if (strlowcase(answer) eq 'yes') then begin
        OK = 0b
        if NOT(SILENT) then begin
            TF_Print,' '
            TF_Print,'Model run stopped.'
        endif
    endif
endif

;--------------------------------
;Default start and stop messages
;--------------------------------
;if (n_elements(start_msg) eq 0) then $
;    start_msg = 'Ready.'
;if (n_elements(stop_msg) eq 0) then $
;    stop_msg  = 'Run aborted.'

;------------------------------------
;Compare current string to start_msg
;------------------------------------
;if (value[0] ne start_msg) then begin
;    EDITABLE = keyword_set(EDITABLE)
;    widget_control, STOP_ID, SET_VALUE=stop_msg, EDITABLE=EDITABLE
;    if (TF_String(stop_msg) ne '') then begin
;        if NOT(EDITABLE) then TF_Print, stop_msg $
;                         else if NOT(SILENT) then begin
;                                  TF_Print,' '
;                                  TF_Print,'Model run stopped.'
;                              endif
;    endif
;    OK=0b
;endif else OK=1b

END;  Check_Interrupt
;*****************************************************************
pro Initialize_Hydrograph_Window, DRAW_ID, win_num

;-----------------------------------------------------
;Note:  win_num is returned and is need for plotting
;-----------------------------------------------------
widget_control, DRAW_ID, get_value=win_num
wset, win_num

erase, 255
plot, [0], [0], XRANGE=[0,1], YRANGE=[0,1], $
      XMARGIN=[0,0.1], YMARGIN=[0,0.1], $
      color=0, background=255

end;  Initialize_Hydrograph_Window
;*****************************************************************


