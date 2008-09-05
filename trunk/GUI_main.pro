
;*****************************************************************
;   GUI_main.pro

;   Copyright (c) 2001-2008, Scott D. Peckham 
;   Created:   Dec 2001 - Jan 2002
;   Modified:  May 2003 (wizard-style version)
;   Modified:  Aug 2003 (button bitmap version)
;   Modified:  Feb 2004 (improvements)
;   Modified:  Jul 2005 (major improvements)
;   Modified:  Apr 2006 (minor changes)
;   Modified:  Feb 2007 (Qnet SW and LW in Create menu)
;   Modified:  Mar 2007 (met_vars, separated)
;   Modified:  Dec 2007 (help system, no Eqns buttons)
;   Modified:  Mar 2008

;*****************************************************************

;   TF_Build_Date     (function)
;   TopoFlow_Version  (function)
;   TF
;   TopoFlow

;   GUI_Navigate   (function)
;   GUI_Run_Info   (function)
;   GUI_Grid_Info  (function)
;   GUI_Methods    (function)
;   GUI_Basin_Info (function)
;   GUI_Stopping   (function)
;   GUI_Preprocess (function)

;   Panel_Number            (function; 7/05)
;   Read_Run_Info_Panel     (7/05)
;   Read_Grid_Info_Panel    (OBSOLETE ??)
;   T_Stop_Model_Estimate   (function;  NEEDS MORE TESTING !!)
;   Read_Basin_Info_Panel

;   GUI_TopoFlow_event
;   GUI_TopoFlow
;   GUI_No_Method   (12/27/07)
;   Confirm_Exit

;*****************************************************************
function TF_Build_Date

;-------------------------------------------------------
;Notes:  Update this whenever a new version is released
;-------------------------------------------------------
RETURN, '3/18/08'

end;  TF_Build_Date
;*****************************************************************
function TopoFlow_Version, TITLE_BAR=title_bar

date_str   = '(' + TF_Build_Date() + ')'
title_bar  = 'TopoFlow 1.5 beta Dialog  ' + date_str
ver_string = 'TopoFlow Version 1.5 beta ' + date_str

RETURN, ver_string
END;    TopoFlow_Version
;*****************************************************************
pro TF, leader

;----------------------------------
;Don't include an error trap here.
;----------------------------------
GUI_TopoFlow, leader

END;  TF
;*****************************************************************
pro TopoFlow, leader

;----------------------------------
;Don't include an error trap here.
;----------------------------------
GUI_TopoFlow, leader

END;  TopoFlow
;*****************************************************************
function GUI_Navigate, PARENT, state


;-------------------------------------------------------
;Notes:  FILE_TEST was introduced in IDL 5.4 and works
;        with blank spaces in file and folder names,
;        while FINDFILE does not.
;-------------------------------------------------------
;ngap = 6
ngap = 20

;--------------------------------------
;Find directory with the button images
;June 20, 2005 (for RTSP 3) 
;--------------------------------------
OS        = strupcase(!version.os)
OS_FAMILY = strupcase(!version.os_family)
if (OS_FAMILY eq 'WINDOWS') then begin
    base  = 'C:\Program Files\'
    paths = ['TopoFlow\Images\', $
             'RIVIX\RiverTools_3.0\basins\TopoFlow\Images\', $
             'RiverTools_3.0\basins\TopoFlow\Images\', $
             'RIVIX\RiverTools_3.0\basins\tf\Images\', $
             'RiverTools_3.0\basins\tf\Images\']
    paths = (base + paths)
endif else if (OS eq 'DARWIN') then begin
    base  = '/Applications/'
    paths = ['TopoFlow/Images/', $
             'RIVIX/RiverTools_3.0/basins/TopoFlow/Images/', $
             'RIVIX/RiverTools_3.0/basins/tf/Images/']
    paths = (base + paths)
endif else paths=['']

dir_found=0b  &  k=0L  &  np=n_elements(paths)
while NOT(dir_found) AND (k le (np-1L)) do begin
    button_path = paths[k]
    dir_found = file_test(button_path, /directory)
    k = (k + 1L)
endwhile

;*** button_path = 'C:\Program Files\TopoFlow\Images\'

;---------------------------------
;Build the button image filenames
;---------------------------------
b1_file = button_path + 'TF_Button1.bmp'
b2_file = button_path + 'TF_Button2.bmp'
b3_file = button_path + 'TF_Button3.bmp'
b4_file = button_path + 'TF_Button4.bmp'

;-----------------------------------
;Are the button bitmaps available ?
;-----------------------------------
FOUND_b1_file = file_test(b1_file)
FOUND_b2_file = file_test(b2_file)
FOUND_b3_file = file_test(b3_file)
FOUND_b4_file = file_test(b4_file)
FOUND_BITMAPS = (FOUND_b1_file AND FOUND_b2_file AND $
                 FOUND_b3_file AND FOUND_b4_file)

;----------------------------------------
;For debugging problem on Macs (6/27/05)
;----------------------------------------
;print,'OS = ', OS
;print,'FOUND_BITMAPS = ', FOUND_BITMAPS
;print,'b1_file = ', b1_file
;print,'b2_file = ', b2_file
;print,'b3_file = ', b3_file
;print,'b4_file = ', b4_file
;print,' '

;-----------------------------------
;Are the button bitmaps available ?
;-----------------------------------
;res1 = findfile(b1_file, count=c1)
;FOUND_b1_file = (c1 ne 0)
;res2 = findfile(b2_file, count=c2)
;FOUND_b2_file = (c2 ne 0)
;res3 = findfile(b3_file, count=c3)
;FOUND_b3_file = (c3 ne 0)
;res4 = findfile(b4_file, count=c4)
;FOUND_b4_file = (c4 ne 0)
;FOUND_BITMAPS = (FOUND_b1_file AND FOUND_b2_file AND $
;                 FOUND_b3_file AND FOUND_b4_file)

;--------------------------
;Create the top-level base
;--------------------------
MB = widget_base(PARENT, /COLUMN)

;---------------------------------
;Create a base for button bitmaps
;---------------------------------
if (FOUND_BITMAPS) then begin
    NP = widget_base(MB, ROW=2, SPACE=ngap)
endif else begin
    NP = widget_base(MB, /COL, SPACE=ngap)
endelse
NP1 = widget_base(NP, /ROW, SPACE=20)
NP2 = widget_base(NP, /ROW, SPACE=20)
NP3 = widget_base(NP, /ROW, SPACE=20)
NP4 = widget_base(NP, /ROW, SPACE=20)

if (FOUND_BITMAPS) then begin
    OS_FAMILY = !version.os_family
    if (OS_FAMILY eq 'WINDOWS') then begin
        NP11 = widget_button(NP1, VALUE=b1_file, /BITMAP, $
                             UVALUE='GO_RUN_INFO')
        NP21 = widget_button(NP2, VALUE=b2_file, /BITMAP, $
                             UVALUE='GO_PREPROCESS')
        NP31 = widget_button(NP3, VALUE=b3_file, /BITMAP, $
                             UVALUE='GO_PLOT_RESULTS')
        NP41 = widget_button(NP4, VALUE=b4_file, /BITMAP, $
                             UVALUE='EXIT')
    endif else begin
        ;-----------------------------------
        ;Needed this to fix bug on Mac OS X
        ;Defined in GUI_utils_tf.pro
        ;Should work on all platforms.
        ;-----------------------------------
        Read_BMP_for_Mac, b1_file, im1
        Read_BMP_for_Mac, b2_file, im2
        Read_BMP_for_Mac, b3_file, im3
        Read_BMP_for_Mac, b4_file, im4
        ;-------------------------------
        NP11 = widget_button(NP1, VALUE=im1, UVALUE='GO_RUN_INFO')
        NP21 = widget_button(NP2, VALUE=im2, UVALUE='GO_PREPROCESS')
        NP31 = widget_button(NP3, VALUE=im3, UVALUE='GO_PLOT_RESULTS')
        NP41 = widget_button(NP4, VALUE=im4, UVALUE='EXIT')
    endelse
endif else begin
    NP11 = widget_button(NP1, VALUE='New Run', UVALUE='GO_RUN_INFO')
    NP12 = widget_label(NP1, VALUE='Setup to run a new simulation', $
                        UVALUE='NONE')
    ;------------------------------------------------------------------
    NP21 = widget_button(NP2, VALUE='Preprocessing', $
                         UVALUE='GO_PREPROCESS')
    NP22 = widget_label(NP2, VALUE='Gridding and data prep. tools',$
                        UVALUE='NONE')
    ;------------------------------------------------------------------
    NP31 = widget_button(NP3, VALUE='Plot Results', $
                         UVALUE='GO_PLOT_RESULTS')
    NP32 = widget_label(NP3, UVALUE='NONE', $
                        VALUE='Graphical tools for visualization')
    ;------------------------------------------------------------------
    NP41 = widget_button(NP4, VALUE='Exit', UVALUE='EXIT')
    NP42 = widget_label(NP4, VALUE='Exit the TopoFlow application',$
                        UVALUE='NONE')

    ;------------------
    ;Align the widgets
    ;------------------
    Align_Text_Boxes, [NP11,NP21,NP31,NP41]
    Align_Text_Boxes, [NP12,NP22,NP32,NP42]
endelse

RETURN, MB
END;  GUI_Navigate
;*****************************************************************
function GUI_Run_Info, PARENT, state, B1=B1

;--------------------
;Initialize GUI vars
;--------------------
npad = 1
ngap = 6
XS   = 32

MB = widget_base(PARENT, /COLUMN)

;---------------------------
;Get main model run options
;---------------------------
B1 = widget_base(MB, /COLUMN, /FRAME)
  P0 = widget_label(B1, VALUE='Model run information:', $
                    /ALIGN_LEFT)
  P00= widget_label(B1, VALUE=' ', UVALUE='NONE')
  ;-------------------------------------------------------------
  RD = widget_base(B1, /ROW, SPACE=ngap)
    RD1 = widget_label(RD, VALUE='Model run directory:', /ALIGN_LEFT)
    RD2 = widget_text(RD, VALUE=state.run_vars.directory,  $
                      UVALUE='NONE', /EDITABLE, XSIZE=46)
    RD3 = widget_button(RD, VALUE=Str_Pad('Browse...'),  $
                        UVALUE='BROWSE_DIR')
    RD4 = widget_label(RD, VALUE='   ', UVALUE='NONE')
    state.run_vars.directory_ID = RD2  
  ;-------------------------------------------------------------
  DP = widget_base(B1, /ROW, SPACE=ngap)
    DP1 = widget_label(DP, VALUE='Data set prefix:', /ALIGN_LEFT)
    DP2 = widget_text(DP, VALUE=state.run_vars.prefix, $
                      UVALUE='NONE', /EDITABLE, XSIZE=XS)
    state.run_vars.prefix_ID = DP2
  ;------------------------------------------------------------------
  RP = widget_base(B1, /ROW, SPACE=ngap)
    RP1 = widget_label(RP, VALUE='Model run prefix:', /ALIGN_LEFT)
    RP2 = widget_text(RP, VALUE=state.run_vars.run_prefix, $
                      /EDITABLE, XSIZE=XS)
    state.run_vars.run_prefix_ID = RP2
    RP3 = widget_button(RP, VALUE=Str_Pad('Apply'),  $
                        UVALUE='APPLY_PREFIX')
    ;RP3 = widget_button(RP, VALUE=Str_Pad('Options...'),  $
    ;                    UVALUE='FILE_OPTIONS')
    ;widget_control, RP3, sensitive=0
  ;------------------------------------------------------------------
  LF = widget_base(B1, /ROW, SPACE=ngap)
    LF1 = widget_label(LF, VALUE='Log file name:', /ALIGN_LEFT)
    LF2 = widget_text(LF, VALUE=state.run_vars.log_file, $
                      UVALUE='NONE', /EDITABLE, XSIZE=XS)
    state.run_vars.log_file_ID = LF2
  ;------------------------------------------------------------------
  CF = widget_base(B1, /ROW, SPACE=ngap)
    CF1 = widget_label(CF, VALUE='Comment file name:', /ALIGN_LEFT)
    CF2 = widget_text(CF, VALUE=state.run_vars.comment_file, $
                      UVALUE='NONE', /EDITABLE, XSIZE=XS)
    state.run_vars.comment_file_ID = CF2
  ;------------------------------------------------------------------
  RC = widget_base(B1, /ROW, SPACE=ngap)
    RC1 = widget_label(RC, VALUE='Run comments:', /ALIGN_LEFT)
                       ;*** /ALIGN_TOP)
    RC2 = widget_text(RC, VALUE='None', UVALUE='NONE', $
                      /EDITABLE, XSIZE=60, YSIZE=6)
    RC3 = widget_label(RC, VALUE='     ', UVALUE='NONE')
    state.run_vars.comments_ID = RC2
  ;------------------------------------------------------------------
  L0 = widget_label(B1, VALUE='  ', UVALUE='NONE')
  Align_Text_Boxes, [RD1,DP1,RP1,LF1,CF1,RC1]

;------------------
;Bottom button bar
;------------------
BB = widget_base(MB, /ROW, SPACE=7)
  BB1 = widget_button(BB, VALUE=Str_Pad('< Back'), $
                      UVALUE='RUN_INFO_BACK')
  BB2 = widget_button(BB, VALUE=Str_Pad('Next >'), $
                      UVALUE='RUN_INFO_NEXT')

RETURN, MB
END;  GUI_Run_Info
;*****************************************************************
function GUI_Grid_Info, PARENT, state

;-----------------------------------------
;Build RTI filename from data prefix.
;Try to find RTI file and fill in values.
;-----------------------------------------
prefix = state.run_vars.prefix
if (prefix ne 'Null') then begin
    RTI_file = prefix + '.rti'
    Read_RTI_File, RTI_file, info
endif
if (n_elements(info) eq 0) then begin

    info = {ncols:1L, nrows:1L, xres:1d, yres:1d, $
            data_type:'INTEGER', pixel_geom:0b}
endif

;----------------
;Initialize vars
;----------------
typelist  = ['BYTE', 'INTEGER', 'LONG', 'FLOAT', 'DOUBLE']
w = where(typelist eq info.data_type, nw)
if (nw ne 0) then type_index=w[0] else type_index=0

;npad = 1
;ngap = 6

MB = widget_base(PARENT, /COLUMN)

B0 = widget_base(MB, /ROW, /FRAME)
LCOL = widget_base(B0, /COLUMN, /FRAME)
RCOL = widget_base(B0, /COLUMN, /FRAME)


L1 = widget_label(LCOL, VALUE='Grid information:', $
                  UVALUE='NONE', /ALIGN_LEFT)
L2 = widget_label(LCOL, VALUE=' ', UVALUE='NONE')

;----------------------
;Number of columns box
;----------------------
LS = widget_base(LCOL, /COLUMN)  ;****, /FRAME)

LS1 = widget_base(LS, /ROW)
  LS11 = widget_label(LS1, VALUE='Number of cols / samps: ')
  LS12 = widget_text(LS1, VALUE=TF_String(info.ncols),  $
                     UVALUE='NONE', XSIZE=8, /EDITABLE)
  state.grid_vars.ncols_ID = LS12

;-------------------
;Number of rows box
;-------------------
LS2 = widget_base(LS, /ROW)
  LS21 = widget_label(LS2, VALUE='Number of rows / lines: ')
  LS22 = widget_text(LS2, VALUE=TF_String(info.nrows), $
                     UVALUE='NONE', XSIZE=8, /EDITABLE)
  state.grid_vars.nrows_ID = LS22

;-------------------
;Data type droplist
;-------------------
LS3 = widget_base(LS, /ROW)
  LS31 = widget_label(LS3, VALUE='DEM data type: ')
  typenames = [' Byte (1-byte) ', ' Integer (2-byte) ', $
               ' Long (4-byte) ', ' Float (4-byte) ', $
               ' Double (8-byte) ']
  LS32 = widget_droplist(LS3, VALUE=typenames, $
                UVALUE='DEM_DATA_TYPE')
  state.grid_vars.data_type_ID = LS32
  widget_control, LS32, set_droplist_select=type_index
  
D0 = widget_label(LCOL, VALUE=' ', UVALUE='NONE')

;------------------------------------
;Pixel geometry type, xres, and yres
;------------------------------------
if (info.pixel_geom eq 0) then punitstr2='  [arcseconds]   ' $
                          else punitstr2='  [meters]       '
PG = widget_base(LCOL, /COLUMN)  ;****, /FRAME)
  PG1 = widget_base(PG, /ROW)
    PG11 = widget_label(PG1, VALUE='Pixel geometry: ')
    pixtypes = [' Fixed-angle ',' Fixed-length ']
    PG12 = widget_droplist(PG1, VALUE=pixtypes, $

                  UVALUE='DEM_PIXEL_GEOM')
    state.grid_vars.pixel_geom_ID = PG12
    widget_control, PG12, set_droplist_select=info.pixel_geom
  ;--------------------------------------------------------
  Align_Text_Boxes, [LS31, PG11]
  ;--------------------------------------------------------
  PG2 = widget_base(PG, /ROW)
    PG21 = widget_label(PG2, VALUE='X-size: ')
    PG22 = widget_text(PG2, VALUE=TF_String(info.xres), $

                       UVALUE='NONE', XSIZE=10, /EDITABLE)

    state.grid_vars.xres_ID = PG22 
    PG23 = widget_label(PG2, VALUE=punitstr2)
    state.grid_vars.xres_units_ID = PG23
  ;--------------------------------------------------------
  PG3 = widget_base(PG, /ROW)
    PG31 = widget_label(PG3, VALUE='Y-size: ')
    PG32 = widget_text(PG3, VALUE=TF_String(info.yres), $
                       UVALUE='NONE', XSIZE=10, /EDITABLE)
    state.grid_vars.yres_ID = PG32
    PG33 = widget_label(PG3, VALUE=punitstr2)
    state.grid_vars.yres_units_ID = PG33

D1 = widget_label(LCOL, VALUE=' ', UVALUE='NONE')
D2 = widget_label(LCOL, VALUE=' ', UVALUE='NONE')
D3 = widget_label(LCOL, VALUE=' ', UVALUE='NONE')

;-------------------
;Align text widgets
;-------------------
Align_Text_Boxes, [LS11, LS21]

;----------------------------
;Construct default filenames
;----------------------------
prefix    = state.run_vars.prefix
directory = state.run_vars.directory
;--------------------------------------
DEM_file       = prefix + '_DEM.rtg'
flow_grid_file = prefix + '_flow.rtg'
slope_file     = prefix + '_slope.rtg'
area_file      = prefix + '_area.rtg'
;--------------------------------------
fsize = 20

;-------------------
;Get grid filenames
;-------------------
L1 = widget_label(RCOL, VALUE='Required grid filenames:', $
                  UVALUE='NONE', /ALIGN_LEFT)
D1 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------------------------------
EG = widget_base(RCOL, /ROW, SPACE=ngap)
  EG1 = widget_label(EG, VALUE='DEM filename: ', UVALUE='NONE')
  EG2 = widget_text(EG, VALUE=DEM_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  EG3 = widget_label(EG, VALUE=' [meters]', UVALUE='NONE')
  state.grid_vars.DEM_file_ID = EG2
;--------------------------------------------------------------------------
FG = widget_base(RCOL, /ROW, SPACE=ngap)
  FG1 = widget_label(FG, VALUE='D8 flow grid filename: ', UVALUE='NONE')
  FG2 = widget_text(FG, VALUE=flow_grid_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  FG3 = widget_label(FG, VALUE=' [none]', UVALUE='NONE')
  state.grid_vars.flow_grid_file_ID = FG2
;--------------------------------------------------------------------------
SG = widget_base(RCOL, /ROW, SPACE=ngap)
  SG1 = widget_label(SG, VALUE='Slope grid filename: ', UVALUE='NONE')
  SG2 = widget_text(SG, VALUE=slope_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  SG3 = widget_label(SG, VALUE=' [none]', UVALUE='NONE')
  state.grid_vars.slope_file_ID = SG2
;--------------------------------------------------------------------------
AG = widget_base(RCOL, /ROW, SPACE=ngap)
  AG1 = widget_label(AG, VALUE='Area grid filename: ', UVALUE='NONE')
  AG2 = widget_text(AG, VALUE=area_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=fsize)
  AG3 = widget_label(AG, VALUE=' [km^2]', UVALUE='NONE')
  state.grid_vars.area_file_ID = AG2
;--------------------------------------------------------------------------
D2 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
D3 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
D4 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
D5 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
D6 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------
;mstr = ["Note: The area or order grid may be used to parameterize",$
;        "channel variables such as Manning's N or bed width."] 
;N1 = widget_label(RCOL, VALUE=mstr[0], UVALUE='NONE', /ALIGN_LEFT)
;N2 = widget_label(RCOL, VALUE=mstr[1], UVALUE='NONE', /ALIGN_LEFT)
;--------------------------------------------------
D7 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
D8 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
D9 = widget_label(RCOL, VALUE=' ', UVALUE='NONE')
;--------------------------------------------------
Align_Text_Boxes, [EG1, FG1, SG1, AG1]
Align_Text_Boxes, [EG2, FG2, SG2, AG2]

;------------------
;Bottom button bar
;------------------
BB = widget_base(MB, /ROW, SPACE=7)
  BB1 = widget_button(BB, VALUE=Str_Pad('< Back'), $
                      UVALUE='GRID_INFO_BACK')
  BB2 = widget_button(BB, VALUE=Str_Pad('Next >'), $
                      UVALUE='GRID_INFO_NEXT')

RETURN, MB

end;  GUI_Grid_Info
;*****************************************************************
function OLD_CW_Method_Buttons, pID, ustr1, ustr2, ustr3, $
                            NGAP=ngap, DESENS=DESENS, $
                            ID1=ID1, ID2=ID2, ID3=ID3

if NOT(keyword_set(NGAP)) then ngap=4

fbutton = Str_Pad('Eqns..', 1)
ibutton = Str_Pad('In..', 1)
obutton = Str_Pad('Out..', 1)

WIDER = 0b

if NOT(WIDER) then begin
    ;--------------------------------------
    ;Use a button base, which results in
    ;buttons that are less wide vertically
    ;--------------------------------------
    B   = widget_base(pID, /ROW, SPACE=ngap)
    ID1 = widget_button(B, VALUE=fbutton, UVALUE=ustr1)
    ID2 = widget_button(B, VALUE=ibutton, UVALUE=ustr2)
    if (n_elements(ustr3) ne 0) then $
        ID3 = widget_button(B, VALUE=obutton, UVALUE=ustr3)

    ;------------------------------
    ;Desensitize the button base ?
    ;------------------------------
    if keyword_set(DESENS) then widget_control, B, sensitive=0
    RETURN, B
endif else begin
    ID1 = widget_button(pID, VALUE=fbutton, UVALUE=ustr1)
    ID2 = widget_button(pID, VALUE=ibutton, UVALUE=ustr2)
    if (n_elements(ustr3) ne 0) then $
    ID3 = widget_button(pID, VALUE=obutton, UVALUE=ustr3)

    ;--------------------------
    ;Desensitize the buttons ?
    ;--------------------------
    if keyword_set(DESENS) then begin
        widget_control, ID1, sensitive=0
        widget_control, ID2, sensitive=0
        if (n_elements(ustr3) ne 0) then $
            widget_control, ID3, sensitive=0
    endif
    RETURN, ID1
endelse

end;  OLD_CW_Method_Buttons
;*****************************************************************
function CW_Method_Buttons, pID, ustr1, ustr2, $
                            NGAP=ngap, DESENS=DESENS, $
                            ID1=ID1, ID2=ID2

if NOT(keyword_set(NGAP)) then ngap=4

ibutton = Str_Pad('In..', 1)
obutton = Str_Pad('Out..', 1)

WIDER = 0b

if NOT(WIDER) then begin
    ;--------------------------------------
    ;Use a button base, which results in
    ;buttons that are less wide vertically
    ;--------------------------------------
    B   = widget_base(pID, /ROW, SPACE=ngap)
    ID1 = widget_button(B, VALUE=ibutton, UVALUE=ustr1)
    if (n_elements(ustr2) ne 0) then $
        ID2 = widget_button(B, VALUE=obutton, UVALUE=ustr2)

    ;------------------------------
    ;Desensitize the button base ?
    ;------------------------------
    if keyword_set(DESENS) then widget_control, B, sensitive=0
    RETURN, B
endif else begin
    ID1 = widget_button(pID, VALUE=ibutton, UVALUE=ustr1)
    if (n_elements(ustr2) ne 0) then $
    ID2 = widget_button(pID, VALUE=obutton, UVALUE=ustr2)

    ;--------------------------
    ;Desensitize the buttons ?
    ;--------------------------
    if keyword_set(DESENS) then begin
        widget_control, ID1, sensitive=0
        if (n_elements(ustr2) ne 0) then $
            widget_control, ID2, sensitive=0
    endif
    RETURN, ID1
endelse

end;  CW_Method_Buttons
;*****************************************************************
function GUI_Methods, PARENT, state, B1=B1

;--------------------
;Initialize GUI vars
;--------------------
npad = 1
ngap = 4
XS   = 32

MB = widget_base(PARENT, /COLUMN)

;--------------------------------------------------------------
;(10/8/07) In order to get all the droplists to have the same
;width under X11 (e.g. Mac OS X), we need to make the lists
;themselves have the same width, as done here.
;--------------------------------------------------------------
len = 42
p_methods   = Str_Resize(Precip_Methods(), len, /RHS)
s_methods   = Str_Resize(Snow_Methods(), len, /RHS)
e_methods   = Str_Resize(ET_Methods(), len, /RHS)
i_methods   = Str_Resize(Infil_Methods(), len, /RHS)
g_methods   = Str_Resize(GW_Methods(), len, /RHS)
;o_methods  = Str_Resize(Overland_Methods(), len, /RHS)
c_methods   = Str_Resize(Channel_Methods(), len, /RHS)
sed_methods = Str_Resize(Sediment_Methods(), len, /RHS)
d_methods   = Str_Resize(Diversion_Methods(), len, /RHS)

;---------------------------------------
;Get a method for each physical process
;---------------------------------------
B1 = widget_base(MB, /COLUMN, /FRAME)
  ;------------------------------------------------------------------
  PP = widget_base(B1, /ROW, SPACE=ngap)
    PP1 = widget_label(PP, VALUE='Physical process:', /ALIGN_LEFT)
    PP2 = widget_label(PP, VALUE='  Method to model process: ', $
                       /ALIGN_LEFT)
  ;------------------------------------------------------------------
  ;PPP = widget_label(B1, VALUE=' ', UVALUE='NONE')
  ;------------------------------------------------------------------
  PM = widget_base(B1, /ROW, SPACE=ngap) 
    PM1 = widget_label(PM, VALUE='Precipitation:', /ALIGN_LEFT)
    PM2 = widget_droplist(PM, VALUE=p_methods, $
                          UVALUE='PRECIP_METHOD')
    widget_control, PM2, set_droplist_select=1
    state.precip_vars.method_ID = PM2
    PM3 = CW_Method_Buttons(PM, 'PRECIP_VARS')
  ;------------------------------------------------------------------
  SM = widget_base(B1, /ROW, SPACE=ngap) 
    SM1 = widget_label(SM, VALUE='Snowmelt:', /ALIGN_LEFT)
    SM2 = widget_droplist(SM, VALUE=s_methods, $
                          UVALUE='SNOW_METHOD')
    state.snow_vars.method_ID = SM2
    SM3 = CW_Method_Buttons(SM, 'SNOW_VARS', 'SNOW_OVARS')
  ;------------------------------------------------------------------
  EM = widget_base(B1, /ROW, SPACE=ngap) 
    EM1 = widget_label(EM, VALUE='Evapotranspiration:', /ALIGN_LEFT)
    EM2 = widget_droplist(EM, VALUE=e_methods,  UVALUE='ET_METHOD')
    state.ET_vars.method_ID = EM2
    EM3 = CW_Method_Buttons(EM, 'ET_VARS', 'ET_OVARS')
  ;------------------------------------------------------------------
  IM = widget_base(B1, /ROW, SPACE=ngap) 
    IM1 = widget_label(IM, VALUE='Infiltration:', /ALIGN_LEFT)
    IM2 = widget_droplist(IM, VALUE=i_methods,  $
                          UVALUE='INFIL_METHOD')
    state.infil_vars.method_ID = IM2
    IM3 = CW_Method_Buttons(IM, 'INFIL_VARS', 'INFIL_OVARS')
  ;------------------------------------------------------------------
  GM = widget_base(B1, /ROW, SPACE=ngap) 
    GM1 = widget_label(GM, VALUE='Subsurface flow:', /ALIGN_LEFT)
    GM2 = widget_droplist(GM, VALUE=g_methods,  $
                          UVALUE='GW_METHOD')
    state.GW_vars.method_ID = GM2
    GM3 = CW_Method_Buttons(GM, 'GW_VARS', 'GW_OVARS')
  ;------------------------------------------------------------------
  ;OM = widget_base(B1, /ROW, SPACE=ngap) 
  ;  OM1 = widget_label(OM, VALUE='Overland flow:', /ALIGN_LEFT)
  ;  OM2 = widget_droplist(OM, VALUE=o_methods,  $
  ;                        UVALUE='OVERLAND_METHOD')
  ;  state.overland_vars.method_ID = OM2
  ;  OM3 = CW_Method_Buttons(OM, 'OVERLAND_VARS', 'OVERLAND_OVARS')
  ;widget_control, OM, sensitive=0  ;*******
  ;-------------------------------------------------------------------
  CM = widget_base(B1, /ROW, SPACE=ngap) 
    CM1 = widget_label(CM, VALUE='Channel flow:', /ALIGN_LEFT)
    CM2 = widget_droplist(CM, VALUE=c_methods,  $
                          UVALUE='CHANNEL_METHOD')
    widget_control, CM2, set_droplist_select=1
    state.channel_vars.method_ID = CM2
    CM3 = CW_Method_Buttons(CM, 'CHANNEL_VARS', 'CHANNEL_OVARS')
    state.channel_vars.method_ID = CM2
  ;-------------------------------------------------------------------
  SED = widget_base(B1, /ROW, SPACE=ngap) 
    SED1 = widget_label(SED, VALUE='Sediment flux:', /ALIGN_LEFT)
    SED2 = widget_droplist(SED, VALUE=sed_methods,  $
                          UVALUE='SED_METHOD')
    state.sed_vars.method_ID = SED2
    SED3 = CW_Method_Buttons(SED, 'SED_VARS', 'SED_OVARS', /DESENS)
    widget_control, SED, sensitive=0
  ;-------------------------------------------------------------------
  DM = widget_base(B1, /ROW, SPACE=ngap)
    DM1 = widget_label(DM, VALUE='Diversions:', /ALIGN_LEFT)
    DM2 = widget_droplist(DM, VALUE=d_methods,  $
                          UVALUE='DIVERSION_METHOD')
    widget_control, DM2, set_droplist_select=0
    state.diversion_vars.method_ID = DM2
    DM3 = CW_Method_Buttons(DM, 'DIVERSION_VARS', ID1=ID1)
    ;widget_control, ID1, sensitive=0

; PAD1 = widget_label(B1, VALUE=' ', UVALUE='NONE')
; PAD2 = widget_label(B1, VALUE=' ', UVALUE='NONE')

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [PP1, PM1,SM1,EM1, IM1,GM1, CM1,SED1,DM1]
Align_Text_Boxes, [PP2, PM2,SM2,EM2, IM2,GM2, CM2,SED2,DM2]

;------------------
;Bottom button bar
;------------------
BB = widget_base(MB, /ROW, SPACE=7)
  BB1 = widget_button(BB, VALUE=Str_Pad('< Back'), $
                      UVALUE='METHODS_BACK')
  BB2 = widget_button(BB, VALUE=Str_Pad('Next >'), $
                      UVALUE='METHODS_NEXT')

RETURN, MB
END;  GUI_Methods
;*****************************************************************
function GUI_Basin_Info, PARENT, state, B1=B1

;--------------------
;Initialize GUI vars
;--------------------
npad = 1
ngap = 6
XS   = 32

MB = widget_base(PARENT, /COLUMN)

;-----------------------------------
;Table to enter basin geometry info
;-----------------------------------
B1 = widget_base(MB, /COLUMN, /FRAME)
L1 = widget_label(B1, VALUE='Info for monitored basins:', $
                  UVALUE='NONE', /ALIGN_LEFT)
L2 = widget_label(B1, VALUE=' ', UVALUE='NONE')

;--------------------------------------------------
;Initialize table with values in mstate.  But note
;that this wizard panel is initialized prior to
;the possibility of a "File > Load Input Vars"
;--------------------------------------------------
ncols = state.grid_vars.ncols
nm    = n_elements(*state.grid_vars.outlet_IDs)
nmax  = (nm > 100)
list  = fltarr(4, nmax)
if (max(*state.grid_vars.outlet_IDs) gt 0) then begin
    list[0,0:nm-1] = (*state.grid_vars.outlet_IDs mod ncols)
    list[1,0:nm-1] = (*state.grid_vars.outlet_IDs  / ncols)
    list[2,0:nm-1] = *state.grid_vars.basin_areas     ;[km^2]
    list[3,0:nm-1] = *state.grid_vars.basin_reliefs   ;[m]
endif
rlabels = sindgen(nmax + 1)
rlabels = strtrim(rlabels[1:nmax], 2)
clabels = [' Outlet Col ', ' Outlet Row ', ' Area [km^2] ', ' Relief [m] ']
if (strupcase(!version.os_family) eq 'WINDOWS') then begin
    cwidths = [1.1, 1.1, 1.1, 1.1]
endif else begin
    cwidths = [1.7, 1.7, 1.7, 1.7]
endelse
WT = widget_table(B1, VALUE=list, UVALUE='TABLE', ALIGNMENT=1, $
                  COLUMN_LABELS=clabels, ROW_LABELS=rlabels, $
                  COLUMN_WIDTHS=cwidths, UNITS=1, $
                  /RESIZEABLE_COLUMNS, /EDITABLE, $
                  Y_SCROLL_SIZE=7, FORMAT='(F13.7)')
                  ;*** X_SCROLL_SIZE=3)
state.grid_vars.basin_table_ID = WT
L3 = widget_label(B1, VALUE=' ', UVALUE='NONE')

;-----------------------
;Read table from file ?
;-----------------------
IT = widget_base(B1, /ROW, SPACE=ngap, /ALIGN_CENTER)
  IT1 = widget_button(IT, VALUE=' Read table from file... ', $
                      UVALUE='IMPORT_BASIN_TABLE')
  IT2 = widget_button(IT, VALUE=' Save table to file... ', $
                      UVALUE='SAVE_BASIN_TABLE')
L3 = widget_label(B1, VALUE=' ', UVALUE='NONE')
L4 = widget_label(B1, VALUE=' ', UVALUE='NONE')

;------------------------------
;Get RTM mask file for basin 1
;for Rain_Volume calculation.
;------------------------------
GM = widget_base(B1, /ROW, SPACE=ngap)
   ;GM2 = widget_base(GM, /ROW, /NONEXCLUSIVE, SPACE=ngap)
   ;  GM21 = widget_button(GM2, VALUE='Check mass balance for basin 1?', $
   ;                       UVALUE='COMPUTE_PVOLUME')
   ;  widget_control, GM21, set_button=state.grid_vars.get_pvolume
   ;  state.grid_vars.get_pvolume_ID = GM21
   ;-----------------------------------------------------------------------
   GM1 = widget_base(GM, /ROW, SPACE=ngap) 
     GM11 = widget_label(GM1, VALUE='RTM mask file for basin 1: ')
     GM12 = widget_text(GM1, VALUE=state.grid_vars.RTM_file, $
                        UVALUE='NONE', /EDITABLE, XSIZE=20)
     state.grid_vars.RTM_file_ID = GM12
     GM13 = widget_label(GM1, VALUE=' ')
   ;-----------------------------------------------------------------------
   GM2 = widget_base(GM, /ROW, /NONEXCLUSIVE, SPACE=ngap)
     GM21 = widget_button(GM2, VALUE='Check mass balance for basin 1?', $
                          UVALUE='COMPUTE_PVOLUME')
     widget_control, GM21, set_button=state.grid_vars.get_pvolume
     state.grid_vars.get_pvolume_ID = GM21

;** L5 = widget_label(B1, VALUE=' ', UVALUE='NONE')

;------------------
;Bottom button bar
;------------------
BB = widget_base(MB, /ROW, SPACE=7)
  BB1 = widget_button(BB, VALUE=Str_Pad('< Back'), $
                      UVALUE='BASIN_INFO_BACK')
  BB2 = widget_button(BB, VALUE=Str_Pad('Next >'), $
                      UVALUE='BASIN_INFO_NEXT')

RETURN, MB
END;  GUI_Basin_Info
;*****************************************************************
function GUI_Stopping, PARENT, state, B1=B1

COMMON LOGWIN, LW

;*** max_lines = 200    ;(for test; worked fine)
max_lines = 1200
LW = {log_win_ID:0L, n_lines:0L, max_lines:max_lines}

;--------------------
;Initialize GUI vars
;--------------------
npad = 1
ngap = 6
XS   = 32

MB = widget_base(PARENT, /COLUMN)
B1 = widget_base(MB, /COLUMN, /FRAME)

;-----------------------
;Set stopping criterion
;-----------------------
ST = widget_base(B1, /ROW, SPACE=ngap) 
  ST1 = widget_label(ST, VALUE='Stopping criterion: ')
  ST2 = widget_droplist(ST, VALUE=Stop_Methods(),  $
                        UVALUE='STOP_METHOD')
  state.stop_vars.method_ID = ST2
  ST3 = widget_button(ST, VALUE=Str_Pad('Options...'),  $
                      UVALUE='STOP_VARS')

;** L2 = widget_label(B1, VALUE='  ', UVALUE='NONE')

;------------------
;Output log window
;------------------
OL = widget_base(B1, /ROW, SPACE=ngap)
  OL1 = widget_base(OL, /COLUMN, SPACE=ngap)
    ;** OL10 = widget_label(OL1, VALUE='  ', UVALUE='NONE')
    ;** OL11 = widget_label(OL1, VALUE='  ', UVALUE='NONE')
    OL12 = widget_label(OL1, VALUE='Output log window:', $
                        /ALIGN_TOP, UVALUE='NONE')
    OL13 = widget_label(OL1, VALUE='  ', UVALUE='NONE')
    OL14 = widget_label(OL1, VALUE='  ', UVALUE='NONE')
    OL15 = widget_label(OL1, VALUE='  ', UVALUE='NONE')
    ;OL16 = widget_label(OL1, VALUE='  ', UVALUE='NONE')

    ;-----------------------------------
    ;Draw widget for plotting (3/22/07)
    ;----------------------------------------------------
    ;Later use this to get window number for WSET:
    ;  widget_control, mstate.draw_ID, get_value=win_num
    ;----------------------------------------------------
    device, decomposed=0
    loadct, 39, /silent  ;(rainbow + white)
    ;** WL = widget_label(OL1, VALUE='Hydrograph')
    WL = widget_label(OL1, VALUE='Hydrograph:', /ALIGN_LEFT)
    WD = widget_draw(OL1, SCR_XSIZE=100, SCR_YSIZE=100, RETAIN=2)
    state.draw_ID = WD

    ;--------------------------
    ;Text widget for interrupt
    ;--------------------------
    SR = widget_base(OL1, /COLUMN)
      ;** SR1 = widget_label(SR, VALUE='Run status: ', /ALIGN_LEFT)
      ;** SR2 = widget_text(SR, /ALL, /EDITABLE, VALUE='Ready.', $
      SR2 = widget_text(SR, /ALL_EVENTS, /EDITABLE, VALUE='', $
                        ;*** XSIZE=12, YSIZE=1,  $   ;(to see it)
                        SCR_XSIZE=1, SCR_YSIZE=1, $  ;(to hide it)
                        /KBRD_FOCUS_EVENTS, $        ;(need for Macs?)
                        UVALUE='STOP')
      state.stop_ID = SR2

  ;----------------------------
  ;Text widget for Output Log
  ;----------------------------
  ;OL1 = widget_label(OL, VALUE='Output log:', UVALUE='NONE', $
  ;                   /ALIGN_TOP)
  ;-------------------------------------------------------------
  OL2 = widget_text(OL, VALUE='  ', UVALUE='NONE', $
                    /EDITABLE, XSIZE=60, YSIZE=max_lines, $
                    SCR_XSIZE=450, $    ;(10/9/7, on Mac OS X)
                    SCR_YSIZE=300, /SCROLL)
  LW.log_win_ID = OL2

  ;*** state.log_window_ID = OL2
  ;*** OL3 = widget_label(OL, VALUE='     ', UVALUE='NONE')

;------------------------
;Clear log window button 
;------------------------
;CL = widget_base(B1, /ROW, SPACE=7)
;  CL1 = widget_label(CL, VALUE='  ', UVALUE='NONE')
;  CL2 = widget_button(CL, VALUE=Str_Pad('Clear log window'), $
;                      UVALUE='CLEAR_LOG_WIN')

L2 = widget_label(B1, VALUE='  ', UVALUE='NONE')
;** L3 = widget_label(B1, VALUE='  ', UVALUE='NONE')

;Align_Text_Boxes, [ST1, OL1, CL1]

Align_Text_Boxes, [ST1, OL1]

;--------------------
;Enter log file name
;--------------------
;log_file = 'Case1_LOG.txt'
;LF = widget_base(B1, /ROW, SPACE=ngap)
;  LF1 = widget_label(LF, VALUE='Log file name:', UVALUE='NONE')
;  LF2 = widget_text(LF, VALUE=log_file, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=XS)
;  state.log_file_ID = LF2

;------------------------
;Enter comment file name
;------------------------
;comment_file = 'Case1_README.txt'
;CF = widget_base(B1, /ROW, SPACE=ngap)
;  CF1 = widget_label(CF, VALUE='Comment file name:', UVALUE='NONE')
;  CF2 = widget_text(CF, VALUE=comment_file, UVALUE='NONE', $
;                    /EDITABLE, XSIZE=XS)
;  state.comment_file_ID = CF2

;-----------------------
;Enter comments to save
;-----------------------
;RC = widget_base(B1, /ROW, SPACE=ngap)
;  RC1 = widget_label(RC, VALUE='Run comments:', UVALUE='NONE', $
;                     /ALIGN_TOP)
;  RC2 = widget_text(RC, VALUE='None', UVALUE='NONE', $
;                    /EDITABLE, XSIZE=60, YSIZE=7)
;  RC3 = widget_label(RC, VALUE='     ', UVALUE='NONE')
;  state.comments_ID = RC2
;L0 = widget_label(B1, VALUE='  ', UVALUE='NONE')

;Align_Text_Boxes, [ST1, LF1, CF1, RC1] 

;------------------
;Bottom button bar
;------------------
BB = widget_base(MB, /ROW, SPACE=7)
  BB1 = widget_button(BB, VALUE=Str_Pad('< Back'), $
                      UVALUE='STOP_INFO_BACK')
  BB2 = widget_button(BB, VALUE=Str_Pad('Start Model Run'), $
                      UVALUE='START')
  state.start_ID = BB2  ;***** 2/12/07
  BB3 = widget_button(BB, VALUE=Str_Pad('Clear Window'), $
                      UVALUE='CLEAR_LOG_WIN')
  BB4 = widget_button(BB, VALUE=Str_Pad('Get Outfile Size'), $
                      UVALUE='OUTFILE_SIZE')
  BB5 = widget_button(BB, VALUE=Str_Pad('Stop'), /MENU, $
                      UVALUE='STOP_RUN')
    BB51 = widget_button(BB5, VALUE=Str_Pad('Press any key to stop'), $
                         UVALUE='STOP_INFO')
  ;----------------------------------------------
  ;Not needed, since done by Trace_Error routine
  ;----------------------------------------------
  ;BB5 = widget_button(BB, VALUE=Str_Pad('Close All Files'), $
  ;                    UVALUE='CLOSE_FILES')
RETURN, MB

END;  GUI_Stopping
;*****************************************************************
function GUI_Preprocess, PARENT, state

;--------------------
;Initialize GUI vars
;--------------------
npad = 1
ngap = 6
XS   = 32

;------------------------
;Create a top-level base
;------------------------
MB = widget_base(PARENT, /COLUMN)

;------------------------------
;Buttons for plotting routines
;------------------------------
B = widget_base(MB, /COLUMN, SPACE=ngap)
  CG = widget_base(B, /ROW, SPACE=ngap)
  CG1 = widget_button(CG, VALUE=Str_Pad('Create channel geometry grids...'), $
                      /MENU, UVALUE='NONE')
    CG11 = widget_button(CG1, VALUE=Str_Pad('Parameterize with area grid'), $
                         UVALUE='MAKE_CHAN_GRIDS_AREA')
    CG12 = widget_button(CG1, VALUE=Str_Pad('Parameterize with HS order grid'),$
                         UVALUE='MAKE_CHAN_GRIDS_ORDER')
  CG2 = widget_label(CG, UVALUE='NONE', $
               VALUE='Create grids for static channel properties.')
  ;--------------------------------------------------------------------------
  ;CG = widget_base(B, /ROW, SPACE=ngap)
  ;  CG1 = widget_button(CG, VALUE=Str_Pad('Create channel grids by area'), $
  ;                      UVALUE='MAKE_CHAN_GRIDS_AREA')
  ;  CG2 = widget_label(CG, UVALUE='NONE', $
  ;        VALUE='Create grids for static channel properties using areas.')
  ;--------------------------------------------------------------------------
  ;CO = widget_base(B, /ROW, SPACE=ngap)
  ;  CO1 = widget_button(CO, VALUE=Str_Pad('Create channel grids by order'), $
  ;                      UVALUE='MAKE_CHAN_GRIDS_ORDER')
  ;  CO2 = widget_label(CO, UVALUE='NONE', $
  ;        VALUE='Create grid for static channel properties using orders.')
  ;--------------------------------------------------------------------------
  PS = widget_base(B, /ROW, SPACE=ngap)
    PS1 = widget_button(PS, VALUE=Str_Pad('Create profile-smoothed DEM'), $
                        UVALUE='MAKE_SMOOTH_DEM')
    PS2 = widget_label(PS, UVALUE='NONE', $
          VALUE='Create a profile-smoothed DEM from an input DEM.')
  ;--------------------------------------------------------------------------
  DG = widget_base(B, /ROW, SPACE=ngap)
    DG1 = widget_button(DG, VALUE=Str_Pad('Create RTG file for Initial Depth'),$
                        UVALUE='MAKE_INIT_DEPTH_GRID')
    DG2 = widget_label(DG, UVALUE='NONE', $
          VALUE='Create RTG file for steady-state initial flow depth')
  ;--------------------------------------------------------------------------
  DT = widget_base(B, /ROW, SPACE=ngap)
    DT1 = widget_button(DT, VALUE=Str_Pad('Create RTS file from Station Data'),$
                        UVALUE='MAKE_RTS_VIA_IDM')
    DT2 = widget_label(DT, UVALUE='NONE', $
          VALUE='Create RTS file from time series via Inverse Distance Method')
  ;--------------------------------------------------------------------------
  QN = widget_base(B, /ROW, SPACE=ngap)
    QN1 = widget_button(QN, VALUE=Str_Pad('Create RTS file for Qnet_SW Flux'),$
                        UVALUE='MAKE_QNET-SW_FILE')
    QN2 = widget_label(QN, UVALUE='NONE', $
          VALUE='Create RTS file for the shortwave radiation flux')
  ;--------------------------------------------------------------------------
  LW = widget_base(B, /ROW, SPACE=ngap)
    LW1 = widget_button(LW, VALUE=Str_Pad('Create RTS file for Qnet_LW Flux'),$
                        UVALUE='MAKE_QNET-LW_FILE')
    LW2 = widget_label(LW, UVALUE='NONE', $
          VALUE='Create RTS file for the longwave radiation flux')
  ;--------------------------------------------------------------------------
  CR = widget_base(B, /ROW, SPACE=ngap)
    CR1 = widget_button(CR, VALUE=Str_Pad('Create RTS file for Fractal Rain'),$
                        UVALUE='MAKE_CASC_RAIN_RTS')
    CR2 = widget_label(CR, UVALUE='NONE', $
          VALUE='Create RTS file of cascade-simulated rainfall')
  ;--------------------------------------------------------------------------
  GM = widget_base(B, /ROW, SPACE=ngap)
    GM1 = widget_button(GM, VALUE=Str_Pad('Return to Top'), $
                        UVALUE='GO_NAVIGATION')
    GM2 = widget_label(GM, VALUE='Return to top level of program', $
                       UVALUE='NONE')
  ;--------------------------------------------------------------------------
  ;ET = widget_base(B, /ROW, SPACE=ngap)
  ;  ET1 = widget_button(ET, VALUE=Str_Pad('Exit TF'), UVALUE='EXIT')
  ;  ET2 = widget_label(ET, VALUE='Exit the TopoFlow program', $
  ;                     UVALUE='NONE')
  ;--------------------------------------------------------------------------
  Align_Text_Boxes, [CG1, PS1, DG1, DT1, QN1, CR1, GM1]  ;**** ,ET1]

RETURN, MB
END;  GUI_Preprocess
;*****************************************************************
function GUI_Plot_Results, PARENT, state


;--------------------
;Initialize GUI vars
;--------------------
npad = 1
ngap = 6
XS   = 32

;------------------------
;Create a top-level base
;------------------------
MB = widget_base(PARENT, /COLUMN)

;------------------------------
;Buttons for plotting routines
;------------------------------
B = widget_base(MB, /COLUMN, SPACE=ngap)
  B1 = widget_base(B, /ROW, SPACE=ngap)
    B11 = widget_button(B1, VALUE=Str_Pad('Plot Function'), $
                        UVALUE='PLOT_FCN')
    B12 = widget_label(B1, $
                       VALUE='Plot a function of time (e.g. a hydrograph)', $
                       UVALUE='NONE')
  ;--------------------------------------------------------------------------
  B2 = widget_base(B, /ROW, SPACE=ngap)
    B21 = widget_button(B2, VALUE=Str_Pad('Plot RTS'), $
                        UVALUE='PLOT_RTS')
    B22 = widget_label(B2, VALUE='Plot a grid sequence as animation', $
                       UVALUE='NONE')
  ;--------------------------------------------------------------------------
  B3 = widget_base(B, /ROW, SPACE=ngap)
    B31 = widget_button(B3, VALUE=Str_Pad('RTS to MPG'), $
                        UVALUE='RTS_TO_MPG')
    B32 = widget_label(B3, VALUE='Convert an RTS file to MPEG movie', $
                       UVALUE='NONE')
  ;--------------------------------------------------------------------------
  B4 = widget_base(B, /ROW, SPACE=ngap)
    B41 = widget_button(B4, VALUE=Str_Pad('Return to Top'), $
                        UVALUE='GO_NAVIGATION')
    B42 = widget_label(B4, VALUE='Return to top level of program', $
                       UVALUE='NONE')
  ;--------------------------------------------------------------------------
  ;B5 = widget_base(B, /ROW, SPACE=ngap)
  ;  B51 = widget_button(B5, VALUE=Str_Pad('Goto Output Log'), $
  ;                      UVALUE='GO_STOP_INFO')
  ;  B52 = widget_label(B5, VALUE='Return to the output log window.', $
  ;                     UVALUE='NONE')
  ;--------------------------------------------------------------------------
  ;B6 = widget_base(B, /ROW, SPACE=ngap)
  ;  B61 = widget_button(B6, VALUE=Str_Pad('Exit TF'), UVALUE='EXIT')
  ;  B62 = widget_label(B6, VALUE='Exit the TopoFlow program', $
  ;                     UVALUE='NONE')
  ;--------------------------------------------------------------------------
  Align_Text_Boxes, [B11, B21, B31, B41]  ;*** B51, B61]
  
RETURN, MB
END;  GUI_Plot_Results
;*****************************************************************
function Panel_Number, name_str

name_str = strupcase(name_str)

case (name_str) of
    'NAVIGATE'   : number = 0
    'RUN_INFO'   : number = 1
    'METHODS'    : number = 2
    ;**** 'GRID_INFO'  : dum = 0
    'BASIN_INFO' : number = 3
    'STOP_INFO'  : number = 4
    'PREPROCESS' : number = 5
    'PLOT'       : number = 6
endcase

RETURN, number

end;  Panel_Number
;*****************************************************************
pro Read_Run_Info_Panel, state, OK

;---------------------------------------------------------
;Notes:  State is updated at bottom of GUI_TopoFlow_event
;        Update_Grid_Vars is defined in getvars.pro.
;---------------------------------------------------------
OK = 1b

;------------------------------------
;Read prefix, run_prefix & directory
;------------------------------------
Read_Text_Box, state.run_vars.prefix_ID, prefix, OK, /TEXT
if NOT(OK) then begin
   msg = [ $
   'Please enter a valid data prefix.', ' ']
   result = GUI_Message(msg, /INFO, TITLE='Missing Data Set Prefix')
   RETURN
endif

OK = (strtrim(prefix,2) ne 'Null') 

if NOT(OK) then begin
   msg = [ $
   'The data set prefix is set to default of "Null". ', ' ', $
   'Please enter a valid prefix or click the Browse ',$
   'button and select a DEM file.', ' ']
   result = GUI_Message(msg, /INFO, TITLE='Missing Data Set Prefix')
   RETURN
endif
;------------------------------------------------------------------
Read_Text_Box, state.run_vars.run_prefix_ID, run_prefix, OK, /TEXT
if NOT(OK) then begin
   msg = [ $
   'Please enter a valid run prefix.', ' ']
   result = GUI_Message(msg, /INFO, TITLE='Missing Run Prefix')
   RETURN
endif
;------------------------------------------------------------------
Read_Text_Box, state.run_vars.directory_ID, directory, OK, /TEXT
if NOT(OK) then begin
   msg = [ $
   'Please enter a directory in the Run Info dialog.', ' ']
   result = GUI_Message(msg, /INFO, TITLE='Missing Run Prefix')
   RETURN
endif

;-------------------------------------
;Change working directory *before*
;working with log_file & comment_file
;-------------------------------------
cd, directory

;-------------------------------------------
;Read filenames for log_file & comment_file
;-------------------------------------------
Read_Text_Box, state.run_vars.log_file_ID, log_file, OK, /TEXT
log_file = strtrim(log_file, 2)
Check_Overwrite, log_file, OK
if NOT(OK) then RETURN
;--------------------------------------------------------
Read_Text_Box, state.run_vars.comment_file_ID, comment_file, OK, /TEXT
comment_file = strtrim(comment_file, 2)
Check_Overwrite, comment_file, OK
if NOT(OK) then RETURN

;----------------------------------
;Comments are not written to file
;until the Start button is clicked
;----------------------------------
;Read_Text_Box, state.run_vars.comments_ID, comments, OK, /TEXT, /ARRAY
;if NOT(OK) then RETURN
;state.run_vars.comments = comments
 
;---------------------------------
;Use RTI file to update grid_vars
;---------------------------------
Update_Grid_Vars, prefix, state

;---------------------------------------------
;Compute new stable timesteps with pixel size
;---------------------------------------------
;Only chan_dt and gw_dt should be affected by
;the grid spacing (horizontal resolution).
;---------------------------------------------
;(3/16/07) These should only be set here if
;they have not already been set by user, as
;with File > Load Settings.
;---------------------------------------------
chan_dt = state.channel_vars.dt
gw_dt   = state.gw_vars.dt
if ((chan_dt le 0) OR (gw_dt le 0)) then begin
    RTI_file = prefix + '.rti'
    Get_Pixel_Sizes, dx_vec, dy,dd,da, RTI_file, /METERS
    dx = min(dx_vec)   ;[meters]
    if (chan_dt le 0) then begin
        chan_vmax = 5d            ;[m/s] 
        chan_dt   = Stable_Timestep(dx, chan_vmax)
        chan_dt   = double(long(chan_dt))
    endif
    if (gw_dt le 0) then begin
        gw_vmax   = 0.005d        ;[m/s]
        gw_dt     = Stable_Timestep(dx, gw_vmax)
        gw_dt     = 3600d * double(long(gw_dt / 3600d) > 1L)
    endif
endif

;--------------------------------------------------------
;These timesteps don't seem to be affected by horizontal
;grid spacing, but depend on vertical resolution such as
;min soil layer thickness.
;--------------------------------------------------------
;infil_vmax = 0.1d          ;[m/s]
;infil_dt   = Stable_Timestep(dx, infil_vmax)
;infil_dt   = 60d * double(long(infil_dt / 60d))
;------------------------------------------------
;ET_vmax    = 0.005d        ;[m/s]
;ET_dt      = Stable_Timestep(dx, ET_vmax)
;ET_dt      = 3600d * double(long(ET_dt / 3600d) > 1L)
;--------------------------------------------------------
;snow_vmax  = 0.005d        ;[m/s]
;snow_dt    = Stable_Timestep(dx, snow_vmax)
;snow_dt    = 3600d * double(long(snow_dt / 3600d) > 1L)


;-----------------------------------
;Save new values in state structure
;See the Notes above.
;-----------------------------------
state.run_vars.prefix         = prefix
state.run_vars.run_prefix     = run_prefix
state.run_vars.directory      = directory
state.run_vars.log_file       = log_file
state.run_vars.comment_file   = comment_file
;--------------------------------------------------------
state.channel_vars.dt         = chan_dt   ;[secs]
state.channel_vars.code_file  = prefix + '_flow.rtg'
state.channel_vars.slope_file = prefix + '_slope.rtg'
state.channel_vars.nval_file  = prefix + '_chan-n.rtg'
state.channel_vars.width_file = prefix + '_chan-w.rtg'
state.channel_vars.angle_file = prefix + '_chan-a.rtg'
state.channel_vars.d0_file    = prefix + '_chan-d0.rtg'
state.channel_vars.sinu_file  = prefix + '_sinu.rtg'
state.channel_vars.z0val_file = prefix + '_chan-z0.rtg'
;--------------------------------------------------------
state.gw_vars.elev_file       = prefix + '_DEM.rtg'
;state.gw_vars.h0_table_file   = run_prefix + '_H2Otable.rtg'
;state.gw_vars.d_bedrock_file  = run_prefix + '_dbedrock.rtg'
;state.gw_vars.d_freeze_file   = run_prefix + '_dfreeze.rtg'  ;(not used yet)
;state.gw_vars.d_thaw_file     = run_prefix + '_dthaw.rtg'
;----------------------------------------------------------
state.diversion_vars.source_file = prefix + '_sources.txt'
state.diversion_vars.sink_file   = prefix + '_sinks.txt'
state.diversion_vars.canal_file  = prefix + '_canals.txt'
;----------------------------------------------------------
; Could update other default filenames here, too.
;-------------------------------------------------------
state.gw_vars.dt              = gw_dt     ;[secs]
;------------------------------------------
;Not affected by horizontal grid spacing ?
;------------------------------------------
;state.ET_vars.dt            = ET_dt     ;[secs]
;state.snow_vars.dt          = snow_dt   ;[secs]
;state.infil_vars.dt         = infil_dt  ;[secs]

;------------------------------------------
;Update all of the outfile names (7/27/06)
;------------------------------------------
Update_Outfile_Names, run_prefix, state
;*** state.channel_vars.Q_out_file = run_prefix + '_0D-Q.txt'


;--------------------------------------------
;If the Read_Grid_Info_Panel is included in
;the wizard, then update its default values
;--------------------------------------------
GRID_INFO_PANEL = 0b
if NOT(GRID_INFO_PANEL) then RETURN
;*************************************************************************

;--------------------------------------
;Change the ncols and nrows text boxes
;and the xres and yres text boxes
;--------------------------------------
widget_control, state.grid_vars.ncols_ID, $
                set_value=TF_String(info.ncols)
widget_control, state.grid_vars.nrows_ID, $
                set_value=TF_String(info.nrows)
widget_control, state.grid_vars.xres_ID, $
                set_value=TF_String(info.xres)
widget_control, state.grid_vars.yres_ID, $
                set_value=TF_String(info.yres)

;-----------------------------
;Change xres and yres units ?
;-----------------------------
m_unit_str = '  [meters]       '   ;(must match orig.)
a_unit_str = '  [arcseconds]   '
widget_control, state.grid_vars.xres_units_ID, $
                get_value=label
if (info.pixel_geom eq 0) AND $
   (label ne a_unit_str) then begin
   widget_control, state.grid_vars.xres_units_ID, $
                   set_value=a_unit_str
   widget_control, state.grid_vars.yres_units_ID, $
                   set_value=a_unit_str
endif

if (info.pixel_geom eq 1) AND $
   (label ne m_unit_str) then begin
   widget_control, state.grid_vars.xres_units_ID, $
                   set_value=m_unit_str
   widget_control, state.grid_vars.yres_units_ID, $
                   set_value=m_unit_str
endif


;----------------------------------------
;Change data type & pixel geom droplists
;----------------------------------------
typelist  = ['BYTE', 'INTEGER', 'LONG', 'FLOAT', 'DOUBLE']
w = where(typelist eq info.data_type, nw)
if (nw ne 0) then type_index=w[0] else type_index=0
widget_control, state.grid_vars.data_type_ID, $
                set_droplist_select=type_index
widget_control, state.grid_vars.pixel_geom_ID, $
                set_droplist_select=info.pixel_geom

;-------------------------------------------
;Change the default filenames in that panel
;-------------------------------------------
;NB! This will no longer work as written.
;slope_file and flow_grid_file have been
;moved to channel_vars, and DEM_file was
;moved to gw_vars. (7/18/05)
;-------------------------------------------
widget_control, state.grid_vars.DEM_file_ID, $
                set_value=prefix + '_DEM.rtg'
widget_control, state.grid_vars.flow_grid_file_ID, $
                set_value=prefix + '_flow.rtg'
widget_control, state.grid_vars.slope_file_ID, $
                set_value=prefix + '_slope.rtg'
widget_control, state.grid_vars.area_file_ID, $
                set_value=prefix + '_area.rtg'

end;  Read_Run_Info_Panel
;*****************************************************************
pro Read_Grid_Info_Panel, state, OK, SKIP_FILES=SKIP_FILES

;---------------------------------------------------------
;Notes:  State is updated at bottom of GUI_TopoFlow_event
;---------------------------------------------------------
OK = 1b
SKIP_FILES = keyword_set(SKIP_FILES)

;-------------------------------
;Read ncols, nrows, xres & yres
;-------------------------------
Read_Text_Box, state.grid_vars.ncols_ID, ncols, OK, /INTEGER
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.nrows_ID, nrows, OK, /INTEGER
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.xres_ID, xres, OK, /DOUBLE
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.yres_ID, yres, OK, /DOUBLE
if NOT(OK) then RETURN

;----------------------------------
;Data type and pixel geometry are
;updated with every droplist event
;----------------------------------

;----------------------
;Check the filenames ?
;----------------------
if (SKIP_FILES) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.DEM_file_ID, $
               DEM_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(DEM_file)
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.flow_grid_file_ID, $
               flow_grid_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(flow_grid_file)
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.slope_file_ID, $
               slope_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(slope_file)
if NOT(OK) then RETURN
;-------------------------------------------------------------
Read_Text_Box, state.grid_vars.area_file_ID, $
               area_file, OK, /FILE
if NOT(OK) then RETURN else OK=File_Found(area_file)
if NOT(OK) then RETURN

;-----------------------------------
;Save new values in state structure
;See the Notes above.
;-----------------------------------
state.grid_vars.ncols          = ncols
state.grid_vars.nrows          = nrows
state.grid_vars.xres           = xres
state.grid_vars.yres           = yres
state.grid_vars.DEM_file       = DEM_file
state.grid_vars.flow_grid_file = flow_grid_file
state.grid_vars.slope_file     = slope_file
state.grid_vars.area_file      = area_file

end;  Read_Grid_Info_Panel
;*****************************************************************
function T_Stop_Model_Estimate, basin_areas, durations, $
                                Qp_fraction, REPORT=REPORT

;---------------------------------------------
;Use max travel time to get a dynamic upper
;bound for T_stop_model and save in stop_vars
;---------------------------------------------
;Two empirical factors are used here:
;  (1) factor in L_max expression
;  (2) factor in T_stop_model expression.
;---------------------------------------------
REPORT  = keyword_set(REPORT)

v_avg   = 0.5d                           ;[m/s]
factor  = 1000000d                       ;[m^2 / km^2]
A_max   = max(basin_areas) * factor      ;[m^2]
L_max   = 2d * sqrt(A_max)               ;[meters]
T_drain = (L_max / v_avg) / 60d          ;[mins]
;-------------------------------------------------
d_mins  = durations                      ;[mins]
wd      = where(d_mins ne 0d, nwd)
if (nwd ne 0) then $
d_mins  = d_mins[0L : wd[nwd-1L]]        ;[mins]
T_Pstop = total(d_mins, /double)         ;[mins]
;-------------------------------------------------        
T_peak_est = (T_Pstop + T_drain)         ;[mins]

;-------------------------------------
;This factor gives a factor of safety
;of about 2 for the Treynor data set.
;-------------------------------------
;Needs much more testing.  *************************************
;-------------------------------------
;** factor       = (0.5d / Qp_fraction)  ;(didn't work)
factor       = (1.0d / Qp_fraction)

T_stop_model = T_peak_est * factor

;----------------
;Optional report
;----------------
;** REPORT = 1b
if (REPORT) then begin
    print,'L_max        = ', L_max
    print,'A_max        = ', A_max
    print,'T_drain      = ', T_drain
    print,'T_Pstop      = ', T_Pstop
    print,'T_stop_Model = ', T_stop_model
    print,' '
endif

RETURN, T_stop_model

end;  T_Stop_Model_Estimate
;*****************************************************************
pro Read_Basin_Info_Panel, state, OK, SKIP_RTM_OK=SKIP_RTM_OK

;---------------------------------------------------------
;Notes:  State is updated at bottom of GUI_TopoFlow_event
;---------------------------------------------------------
OK = 1b
ncols = state.grid_vars.ncols
GET_PVOLUME = state.grid_vars.get_pvolume
SKIP_RTM_OK = keyword_set(SKIP_RTM_OK)

;--------------------------------
;Read the RTM filename for basin
;--------------------------------
if (GET_PVOLUME) then begin
    Read_Text_Box, state.grid_vars.RTM_file_ID, $
                   RTM_file, OK, /FILE
    if NOT(SKIP_RTM_OK) then begin
        if NOT(OK) then RETURN else OK=File_Found(RTM_file)
        if NOT(OK) then RETURN
        state.grid_vars.RTM_file = RTM_file   ;(put here, 3/16/07)
    endif
endif

;---------------------------------
;Read basin attributes from table
;---------------------------------
widget_control, state.grid_vars.basin_table_ID, $
                get_value=basin_table
outlet_cols   = long(basin_table[0,*])
outlet_rows   = long(basin_table[1,*])
basin_areas   = basin_table[2,*]
basin_reliefs = basin_table[3,*]
;--------------------------------------------
outlet_IDs = (outlet_rows * ncols) + outlet_cols
wb = where(outlet_IDs ne 0L, nwb)
;------------------------------------
;If no monitored pixels, then RETURN
;------------------------------------
if (nwb eq 0) then RETURN

;-----------------------------------
;Save new values in state structure
;See the Notes above.
;-----------------------------------
;** outlet_cols   = outlet_cols[wb]
;** outlet_rows   = outlet_rows[wb]
outlet_IDs    = outlet_IDs[wb]
basin_areas   = basin_areas[wb]
basin_reliefs = basin_reliefs[wb]
;-----------------------------------------------
*state.grid_vars.outlet_IDs    = outlet_IDs
*state.grid_vars.basin_areas   = basin_areas
*state.grid_vars.basin_reliefs = basin_reliefs

;---------------------------------------------
;Use max travel time to get a dynamic upper
;bound for T_stop_model and save in stop_vars
;---------------------------------------------
;This function doesn't work well yet, but a
;bigger problem is that this may overwrite
;something the user has just set (2/13/07)
;---------------------------------------------
;(3/7/07) There is also a connection to the
;Number_of_Samples function in route.pro.
;---------------------------------------------
;durations    = *state.precip_vars.durations
;Qp_fraction  = state.stop_vars.Qp_fraction
;T_stop_model = T_Stop_Model_Estimate(basin_areas, durations, $
;                                     Qp_fraction)
;state.stop_vars.T_stop_model = T_stop_model

end;  Read_Basin_Info_Panel
;*****************************************************************
pro GUI_TopoFlow_event, event

;-----------------------------------------------------
;Alternate, single error trap that prevents TopoFlow
;from trying to keep going after an error, which can
;generate an annoying series of error dialogs.
;-----------------------------------------------------
;For this to work as intended, error traps in all
;other routines must be disabled.  This cannot be
;done by defining a new CATCH function.  It also
;cannot be done by modifying Check_Error_Status.
;However, using a text editor to replace:
;"CATCH, status" with "No_Catch, status" in all
;of the source code files does the trick.
;------------------------------------------------------
;NB!  We need to do this in every TLB event handler.
;------------------------------------------------------
;NB!  Currently, Trace_Error closes all files using
;     the IDL command "close,/ALL" (7/19/06).
;------------------------------------------------------
CATCH, status
Trace_Error, status, event, OK, /ABORT
if NOT(OK) then RETURN

Get_Event_Uvalue, event, uvalue, state
leader = state.base_ID
CHANGE_PANEL = 0b        ;(default)

case (uvalue) of

;**************
'ABOUT_TF': $
;**************
Show_HTML_Help, 'about_TF.htm'

;****************
'LICENSE_TF': $
;****************
Show_HTML_Help, 'TF_license_agreement.htm'

;***************
'WHATS_NEW': $
;***************
Show_HTML_Help, 'TF15b_Release_Notes.htm'

;**************
'TUTORIAL': $
;**************
Show_HTML_Help, 'TF_tutorial.htm'

;********************
'GO_NAVIGATION' : $
;********************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('NAVIGATE')
end

;******************
'GO_RUN_INFO' : $
;******************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('RUN_INFO')
end

;*******************
'GO_STOP_INFO' : $
;*******************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('STOP_INFO')
end

;********************
'GO_PREPROCESS' : $
;********************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('PREPROCESS')
end

;**********************
'GO_PLOT_RESULTS' : $
;**********************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('PLOT')
end

;********************
'RUN_INFO_BACK' : $
;********************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('NAVIGATE')
end

;********************
'RUN_INFO_NEXT' : $
;********************
begin
Read_Run_Info_Panel, state, OK
if NOT(OK) then RETURN
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('METHODS')
;*** PANEL_NUMBER = Panel_Number('GRID_INFO')
end

;*********************
'GRID_INFO_BACK' : $
;*********************
begin
Read_Grid_Info_Panel, state, OK, /SKIP_FILES
if NOT(OK) then RETURN
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('RUN_INFO')
end

;*********************
'GRID_INFO_NEXT' : $
;*********************
begin
Read_Grid_Info_Panel, state, OK
if NOT(OK) then RETURN
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('METHODS')
end

;*******************
'METHODS_BACK' : $
;*******************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('RUN_INFO')
;*** PANEL_NUMBER = Panel_Number('GRID_INFO')
end

;*******************
'METHODS_NEXT' : $
;*******************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('BASIN_INFO')
;----------------------------------------------
;Note that Read_Run_Info_Panel makes sure that
;all the filenames for default input and output
;options are defined even if the user hasn't
;opened their input and output var dialogs.
;Especially for channel vars.
;---------------------------------------------
end

;********************
'SAVE_SETTINGS' : $
;********************
Save_All_TF_Vars, state

;********************
'LOAD_SETTINGS' : $
;********************
Load_All_TF_Vars, state

;**********************
'BASIN_INFO_BACK' : $
;**********************
begin
Read_Basin_Info_Panel, state, OK, /SKIP_RTM_OK
if NOT(OK) then RETURN
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('METHODS')
end

;**********************
'BASIN_INFO_NEXT' : $
;**********************
begin
Read_Basin_Info_Panel, state, OK
if NOT(OK) then RETURN
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('STOP_INFO')
end

;*********************
'STOP_INFO_BACK' : $
;*********************
begin
CHANGE_PANEL = 1b
PANEL_NUMBER = Panel_Number('BASIN_INFO')
end

;********************
'CLEAR_LOG_WIN' : $
;********************
begin
Clear_Log_Window
Initialize_Hydrograph_Window, state.draw_ID
widget_control, state.start_ID, /sensitive   ;(start button)
;-------------------
;Close all files ??
;-------------------
for LUN=1,128 do free_lun,LUN
close, /all
end

;*******************
'OUTFILE_SIZE' : $
;*******************
begin
size    = Total_Outfile_Size(state)
str1    = TF_String(size)
size_MB = round(size / 1000000d)
str2    = TF_String(size_MB)    
TF_Print,'Estimated total size of output files = ' + $
          str1 + ' bytes.  (' + str2 + ' MB)'
TF_Print,' '
end

;*****************
'CLOSE_FILES' : $
;*****************
begin
status = bytarr(129)
names  = strarr(129)
for LUN=1,128 do begin
    temp        = fstat(LUN)
    status[LUN] = temp.open
    names[LUN]  = temp.name
    free_lun, LUN, /FORCE
endfor
w = where((status eq 1b), nw)
if (nw eq 0) then begin
    msg = [' ', ' There are no open files.   ', ' ']
    result = GUI_Message(msg, /INFO, TITLE='Close All Files')
    RETURN
endif
;--------------------------------
;Tell user which files were open
;--------------------------------
names = names[w]
msg = [' ', ' The following files were closed: ', ' ']
for k=0,(nw-1) do msg = [msg, '   ' + names[k] ]
msg = [msg, ' ']
result = GUI_Message(msg, /INFO, TITLE='Close All Files')
;---------------------------------------------------------
TF_Print,'All files have been closed.'
TF_Print,' '
;*** close, /all
end

;*****************
'BROWSE_DIR' : $
;*****************
begin
filepath = dialog_pickfile(GET_PATH=directory)
if (filepath eq '') then RETURN
widget_control, state.run_vars.directory_ID, set_value=directory

;-----------------
;Change directory
;-----------------
cd, directory

;------------------------------------------
;Get default prefix from selected filepath
;------------------------------------------
Get_Data_Prefix, filepath, prefix
widget_control, state.run_vars.prefix_ID, set_value=prefix

;---------------------------------
;Get default run prefix (7/15/06)
;using files in current directory
;---------------------------------
Get_Run_Prefix, run_prefix
widget_control, state.run_vars.run_prefix_ID, set_value=run_prefix
log_file     = (run_prefix + '_LOG.txt')
comment_file = (run_prefix + '_README.txt')
widget_control, state.run_vars.log_file_ID,     set_value=log_file
widget_control, state.run_vars.comment_file_ID, set_value=comment_file

;---------------------------------
;Use RTI file to update grid vars
;---------------------------------
Update_Grid_Vars, prefix, state

;-----------------------------------------
;Save new prefix and directory (2/11/04)
;-----------------------------------------
;This may not be needed anymore (7/15/06)
;-----------------------------------------
state.run_vars.prefix          = prefix
state.run_vars.directory       = directory
state.run_vars.run_prefix      = run_prefix    ;(7/15/06)
state.run_vars.log_file        = log_file
state.run_vars.comment_file    = comment_file
;--------------------------------------------------------
state.channel_vars.code_file = prefix + '_flow.rtg'
;** state.gw_vars.DEM_file   = prefix + '_DEM.rtg'
end

;*******************
'APPLY_PREFIX' : $
;*******************
begin
widget_control, state.run_vars.run_prefix_ID, get_value=run_prefix
log_file     = (run_prefix + '_LOG.txt')
comment_file = (run_prefix + '_README.txt')
widget_control, state.run_vars.log_file_ID,     set_value=log_file
widget_control, state.run_vars.comment_file_ID, set_value=comment_file
end

;******************
'STOP_METHOD' : $
;******************
state.stop_vars.method = event.index

;****************
'STOP_VARS' : $
;****************
case (state.stop_vars.method) of
    0b : GUI_Stop_By_Qpeak_Fraction, leader
    1b : GUI_Stop_By_Model_Time, leader
    ;** 2b : GUI_Stop_By_Real_Time, leader
    2b : GUI_Stop_By_Steps, leader 
  else : GUI_No_Method
endcase

;******************
'SNOW_METHOD' : $
;******************
state.snow_vars.method = event.index

;********************
'SNOW_FORMULAS' : $
;********************
case (state.snow_vars.method) of
    1b : GUI_Qm_Degree_Day_Formulas, leader
    2b : GUI_Qm_Energy_Balance_Formulas, leader
  else : dum=0
endcase

;****************
'SNOW_VARS' : $
;****************
case (state.snow_vars.method) of
    1b : GUI_Qm_Degree_Day, leader
    2b : GUI_Qm_Energy_Balance, leader
  else : GUI_No_Method
endcase

;*****************
'SNOW_OVARS' : $
;*****************
case (state.snow_vars.method) of
    0b : GUI_No_Method
  else : GUI_Save_Snow_Vars, leader 
endcase

;****************
'ET_METHOD' : $
;****************
state.ET_vars.method = event.index

;******************
'ET_FORMULAS' : $
;******************
case (state.ET_vars.method) of
    1b : GUI_Qet_Priestley_Taylor_Formulas, leader
    2b : GUI_Qet_Energy_Balance_Formulas, leader
  else : GUI_No_Method
endcase

;**************
'ET_VARS' : $
;**************
case (state.ET_vars.method) of
    1b : GUI_Qet_Priestley_Taylor, leader
    2b : GUI_Qet_Energy_Balance, leader
  else : GUI_No_Method
endcase

;***************
'ET_OVARS' : $
;;**************
case (state.ET_vars.method) of
    0b : GUI_No_Method
  else : GUI_Save_ET_Vars, leader 
endcase

;********************
'PRECIP_METHOD' : $
;********************
state.precip_vars.method = event.index

;**********************
'PRECIP_FORMULAS' : $
;**********************
case (state.precip_vars.method) of
    1b : GUI_Precip_Formulas, leader
    2b : GUI_Precip_Formulas, leader
  else : GUI_No_Method
endcase


;******************
'PRECIP_VARS' : $
;******************
case (state.precip_vars.method) of
    1b : GUI_Uniform_Precip, leader
    2b : GUI_Precip, leader

;    1b : GUI_Uniform_Precip, leader
;    2b : GUI_RTS_Precip_Fixed, leader
;    3b : GUI_RTS_Precip_Durations, leader
  else : GUI_No_Method
endcase

;**********************
'COMPUTE_PVOLUME' : $
;**********************
begin
state.grid_vars.get_pvolume = (1b - state.grid_vars.get_pvolume)
widget_control, state.grid_vars.RTM_file_ID, $
       sensitive=state.grid_vars.get_pvolume
end

;*******************
'INFIL_METHOD' : $
;*******************
begin
state.infil_vars.method = event.index
RICHARDS = (event.index eq 4)
if (RICHARDS) then begin
    ;------------------------------------------------------
    ;7/15/06.  Testing with Treynor data set shows that
    ;the Richards method requires a time step close to
    ;that of the channel process for stability, and has a
    ;much smaller mass error if it has the same time step.
    ;------------------------------------------------------
    state.infil_vars.dt = state.channel_vars.dt
endif
end
;--------------------------------------
;if (event.index ne 4) then begin
;    state.infil_vars.method = event.index
;endif else begin
;    msg = [ $
;    "Sorry, but the Richards' equation method is not yet ", $
;    'finished.  Please select another infiltration method. ',$
;    ' ',$
;    "The Richards' equation method should be finished for ",$
;    'the full release version 1.5.', ' ']
;    result = GUI_Message(msg, /INFO, TITLE='Feature Not Ready')
;endelse

;*********************
'INFIL_FORMULAS' : $
;*********************
case (state.infil_vars.method) of
    2b : GUI_Green_Ampt1_Formulas, leader
    3b : GUI_Smith_Parlange_Formulas, leader
    4b : GUI_Richards_Eqn_Formulas, leader
  else : GUI_No_Method
endcase

;*****************
'INFIL_VARS' : $
;*****************
case (state.infil_vars.method) of
    2b : GUI_Infiltration_Vars, leader
    3b : GUI_Infiltration_Vars, leader, /GAMMA
    4b : GUI_Richards_Eqn_Vars, leader
  else : GUI_No_Method
endcase

;******************
'INFIL_OVARS' : $
;******************
case (state.infil_vars.method) of
    0b : GUI_No_Method
    4b : GUI_Save_Infil_Vars, leader, /RICHARDS
  else : GUI_Save_Infil_Vars, leader 
endcase

;****************
'GW_METHOD' : $
;****************
state.GW_vars.method = event.index

;******************
'GW_FORMULAS' : $
;******************
case (state.GW_vars.method) of
    1b : GUI_Darcy_Law_Formulas, leader
  else : GUI_No_Method
endcase

;**************
'GW_VARS' : $
;**************
case (state.GW_vars.method) of
    1b : GUI_Subsurface_Vars, leader
    ;** 1b : GUI_Darcy_Law, leader
  else : GUI_No_Method
endcase

;***************
'GW_OVARS' : $
;***************
case (state.gw_vars.method) of
    0b : GUI_No_Method
  else : GUI_Save_GW_Vars, leader 
endcase

;**********************
;'OVERLAND_METHOD' : $
;**********************
;state.overland_vars.method = event.index

;************************
;'OVERLAND_FORMULAS' : $
;************************
;case (state.overland_vars.method) of
;    ;1b : GUI_Overland_Flow_Formulas, leader
;  else : GUI_No_Method
;endcase

;********************
;'OVERLAND_VARS' : $
;********************
;case (state.overland_vars.method) of
;    1b : GUI_Overland_Flow, leader
;  else : GUI_No_Method
;endcase

;*********************
'CHANNEL_METHOD' : $
;*********************
begin
if (event.index eq 0) then begin
    msg = [' ', 'This option is not yet available.', ' ']
    result = GUI_Message(msg, /INFO)
    widget_control, state.channel_vars.method_ID, $
           set_droplist_select=state.channel_vars.method
    RETURN
endif
state.channel_vars.method = event.index
M_options = [0b,1b,0b,1b,0b,1b,0b]   ;(manning)
L_options = [0b,0b,1b,0b,1b,0b,1b]   ;(law of wall)
K_options = [0b,1b,1b,0b,0b,0b,0b]   ;(kinematic)
F_options = [0b,0b,0b,1b,1b,0b,0b]   ;(diffusive)
D_options = [0b,0b,0b,0b,0b,1b,1b]   ;(dynamic)
;-----------------------------------------------------------
state.channel_vars.manning        = M_options[event.index]
state.channel_vars.law_of_wall    = L_options[event.index]
state.channel_vars.kinematic_wave = K_options[event.index]
state.channel_vars.diffusive_wave = F_options[event.index]
state.channel_vars.dynamic_wave   = D_options[event.index]
end

;***********************
'CHANNEL_FORMULAS' : $
;***********************
case (state.channel_vars.method) of
    1b : GUI_Kinematic_Wave_Formulas, leader
  else : begin
         msg = [' ', 'This feature is not yet available.', ' ']
         result = GUI_Message(msg, /INFO)
         end 
endcase

;*******************
'CHANNEL_VARS' : $
;*******************
case (state.channel_vars.method) of
    1b : GUI_Kinematic_Wave_Manning, leader
    2b : GUI_Kinematic_Wave_z0, leader
    ;-------------------------------------------------
    3b : GUI_Kinematic_Wave_Manning, leader, $
         TITLE='Variables for Diffusive Wave Method'
    4b : GUI_Kinematic_Wave_z0, leader, $
         TITLE='Variables for Diffusive Wave Method'
    ;-------------------------------------------------
    5b : GUI_Kinematic_Wave_Manning, leader, $
         TITLE='Variables for Dynamic Wave Method'
    6b : GUI_Kinematic_Wave_z0, leader, $
         TITLE='Variables for Dynamic Wave Method'
    ;-------------------------------------------------
  else : GUI_No_Method
endcase

;********************
'CHANNEL_OVARS' : $
;********************
case (state.channel_vars.method) of
    0b : GUI_No_Method
  else : GUI_Save_Channel_Vars, leader
endcase

;*****************
'SED_METHOD' : $
;*****************
state.sed_vars.method = event.index

;*******************
'SED_FORMULAS' : $
;*******************
case (state.sed_vars.method) of
    1b : GUI_Qs_Slope_Area_Formulas, leader
  else : GUI_No_Method
endcase

;**************
'SED_VARS' : $
;**************
case (state.sed_vars.method) of
    1b : GUI_Qs_Slope_Area, leader
  else : GUI_No_Method
endcase

;***********************
'DIVERSION_METHOD' : $
;***********************
state.diversion_vars.method = event.index
 
;;***********************
;'DIVERSION_FORMULAS' : $
;;***********************
;case (state.diversion_vars.method) of
;    1b : GUI_Diversion_Formulas, leader
;  else : begin
;         msg = [' ', 'This feature is not yet available.', ' ']
;         result = GUI_Message(msg, /INFO)
;         end 
;endcase

;*********************
'DIVERSION_VARS' : $
;*********************
case (state.diversion_vars.method) of
    1b : GUI_Diversions, leader
  else : GUI_No_Method
endcase

;*************************
'IMPORT_BASIN_TABLE' : $
;*************************
begin
ncols = 4
;------------------------------------------------
;Read outlet col/rows, areas, & relief
; with units: [none, none, km^2, m]
;------------------------------------------------
;**** Read outlet IDs, areas, lengths and relief
;**** with units: [none, km^2, km, m]
;------------------------------------------------
table_ID = state.grid_vars.basin_table_ID
Import_Table_From_File, table_ID, ncols, FILE='00_Basin_Data.txt'
end

;***********************
'SAVE_BASIN_TABLE' : $
;***********************
;---------------------------------------
;Write outlet col/rows, areas, & relief
; with units: [none, none, km^2, m]
;---------------------------------------
begin
table_ID = state.grid_vars.basin_table_ID
Save_Table_To_File, table_ID, FILE='00_Basin_Data.txt', $
                    /REMOVE_ZEROS   ;******
end

;********************
'DEM_DATA_TYPE' : $
;********************
begin
typelist = ['BYTE', 'INTEGER', 'LONG', 'FLOAT', 'DOUBLE']
state.grid_vars.data_type = typelist[event.index]
end

;*********************
'DEM_PIXEL_GEOM' : $
;*********************
begin
state.grid_vars.pixel_geom = byte(event.index)
;---------------------------
;Change xres and yres units
;---------------------------
m_unit_str = '  [meters]       '   ;(must match orig.)
a_unit_str = '  [arcseconds]   '
if (event.index eq 0) then begin
   widget_control, state.grid_vars.xres_units_ID, $
                   set_value=a_unit_str
   widget_control, state.grid_vars.yres_units_ID, $
                   set_value=a_unit_str
endif else begin
   widget_control, state.grid_vars.xres_units_ID, $
                   set_value=m_unit_str
   widget_control, state.grid_vars.yres_units_ID, $
                   set_value=m_unit_str
endelse
end

;***********
'START': $
;***********
begin
;--------------------------------------
;Read model run comments from text box
;--------------------------------------
Read_Text_Box, state.run_vars.comments_ID, $
               comments, OK, /TEXT, /ARRAY
if NOT(OK) then RETURN

;-----------------------------------
;Write model run comments to a file
;-----------------------------------
comment_file = state.run_vars.comment_file
TF_Get_LUN, unit, comment_file 
openw, unit, comment_file 
ncom = n_elements(comments)
for kc=0L,(ncom-1L) do printf, unit, comments[kc]
free_lun, unit
TF_Print,'Comments written to file: '
TF_Print,'   ' + comment_file

;-------------------------------
;Start the Route_Flow procedure
;-------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Route_Flow, state.grid_vars, state.stop_vars, $
            state.run_vars, state.precip_vars, $
            state.channel_vars, state.met_vars, $
            state.snow_vars, state.ET_vars, state.GW_vars, $
            state.infil_vars, state.overland_vars, $
            state.diversion_vars, time, $  ;***************
            DIRECTORY=state.run_vars.directory, $
            LOG_FILE=state.run_vars.log_file, $
            STOP_ID=state.stop_ID, DRAW_ID=state.draw_ID
widget_control, event.ID, sensitive=1    ;(enable button)

;------------------------------------
;Save the actual run time as the new
;default for T_stop_model (2/13/07)
;Note that this assumes that a prior
;run ran out into hydrograph tail
;------------------------------------
;if (stop_vars.method ne 1b) then begin
;    state.stop_vars.T_stop_model = time   ;[minutes]
;endif
;------------------------------------
;Don't call Free_Pointers here since
;user may want to do another run.
;------------------------------------
end

;***************
'PLOT_RTS' : $
;***************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
Plot_Grid_Sequence
widget_control, event.ID, sensitive=1    ;(enable button)
end

;*****************
'RTS_TO_MPG' : $
;*****************
begin
HAVE_LICENSE = LMGR('idl_mpeg')
if NOT(HAVE_LICENSE) then begin
    msg = ['Sorry, but you need a special MPEG license ',$
           'from RSI in order to use this feature. ', ' ', $
           'You can also save RTS files as AVI movies ', $
           'with RiverTools 3.0.', ' ']
    result = GUI_Message(msg, /INFO, TITLE='No MPEG License')

    RETURN
endif
;-------------------------------------
widget_control, event.ID, sensitive=0    ;(disable button)
Plot_Grid_Sequence, /MPEG
widget_control, event.ID, sensitive=1    ;(enable button)
end

;***************
'PLOT_FCN' : $
;***************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
Plot_Function 
widget_control, event.ID, sensitive=1    ;(enable button)
end

;***************************
'MAKE_CHAN_GRIDS_AREA' : $
;***************************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Channel_Grids_by_Area, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;****************************
'MAKE_CHAN_GRIDS_ORDER' : $
;****************************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Channel_Grids_by_Order, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;**********************
'MAKE_SMOOTH_DEM' : $
;**********************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Smooth_DEM, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;***********************
'MAKE_RTS_VIA_IDM' : $
;***********************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_RTS_via_IDM, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;************************
'MAKE_QNET-SW_FILE' : $
;************************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Qnet_SW_File, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;************************
'MAKE_QNET-LW_FILE' : $
;************************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Qnet_LW_File, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;*************************
'MAKE_CASC_RAIN_RTS' : $
;*************************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Cascade_Rain_RTS, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;***************************
'MAKE_INIT_DEPTH_GRID' : $
;***************************
begin
widget_control, event.ID, sensitive=0    ;(disable button)
GUI_Make_Init_Depth_Grid, leader
widget_control, event.ID, sensitive=1    ;(enable button)
end

;***********
'EXIT' : $
;***********
begin
Confirm_Exit, OK
if (NOT(OK)) then RETURN
;------------------------
Free_Pointers, state        ;(Better here vs. 'OK')
Close_Input_Files, state  
Close_Dialog, event.top
end

ELSE : dum=0
endcase

;------------------------------------------
;Change the visible panel via Next or Back
;button and hide all of the others ?
;------------------------------------------
if (CHANGE_PANEL) then begin
    onoff = bytarr(state.npanels)
    onoff[PANEL_NUMBER] = 1b
    for k=0,(state.npanels-1) do $
        widget_control, state.bases[k], MAP=onoff[k]
endif

if (uvalue ne 'EXIT') then $
    widget_control, event.top, set_uvalue=state 

END;  GUI_TopoFlow_event
;*****************************************************************
pro GUI_TopoFlow, leader

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN 

;---------------------------------
;Needed under Unix to prevent IDL
;from creating a blank window
;---------------------------------
;June 27, 2005.  Still needed
;or is it causing problem on Mac?
;---------------------------------
;if (!d.name eq 'X') AND $
;   (!d.window eq -1L) then begin
;     window, /free, /pixmap
     ;----------------------
     ;Save this ID in state
     ;----------------------
;     temp_winID = !d.window
;endif else temp_winID = -1L
temp_winID = -1L  ;******************

;---------------------------------
;Get structures with var defaults
;---------------------------------
Get_Run_Vars,       run_vars
Get_Grid_Vars,      grid_vars
Get_Stop_Vars,      stop_vars
Get_Precip_Vars,    precip_vars
Get_Channel_Vars,   channel_vars
Get_Met_Vars,       met_vars         ;(new, 3/13/07)
Get_Snow_Vars,      snow_vars
Get_ET_Vars,        ET_vars
Get_GW_Vars,        GW_vars
Get_Infil_Vars,     infil_vars
Get_Overland_Vars,  overland_vars
Get_Sediment_Vars,  sed_vars
Get_Diversion_Vars, diversion_vars

;*************************
;Set the total number of
;wizard panels here
;*************************
npanels = 7

;print,'Initializing state structure...'
;-------------------------------------
;Structure to store selected options
;------------------------------------------------------------
;Data collected from other dialogs, including grid filenames
;and pointers to 2D arrays are uploaded back to here.
;------------------------------------------------------------
state = { $
leader_ID:      0L, $
base_ID:        0L, $
start_ID:       0L, $                ;(start button ID)
stop_ID:        0L, $                ;(stop/status text box ID)
draw_ID:        0L, $
npanels:        npanels, $
bases:          lonarr(npanels), $
temp_winID:     temp_winID, $
;---------------------
;For basin info panel
;---------------------
run_vars:       run_vars,   $
grid_vars:      grid_vars,  $
stop_vars:      stop_vars,  $
precip_vars:    precip_vars, $
channel_vars:   channel_vars, $
met_vars:       met_vars, $
snow_vars:      snow_vars, $
ET_vars:        ET_vars, $
GW_vars:        GW_vars, $
infil_vars:     infil_vars, $
overland_vars:  overland_vars, $
sed_vars:       sed_vars, $
diversion_vars: diversion_vars }
;--------------------------------------
;print,'Finished initializing state.'
;print,' '

;-----------------
;Main base widget
;-----------------
ver = TopoFlow_Version(TITLE_BAR=title_bar)
Create_TLB, MB, TB, TITLE=title_bar, /ADD_MBAR, $
            /COLUMN, /SIZEABLE, LEADER=leader
state.base_ID = MB

;------------------
;Create a menu bar
;------------------
F_menu = [' Open Data Set ', ' Load Input Vars ', $
          ' Save Input Vars ', ' Close Any Open Files ', ' Exit TopoFlow ']
G_menu = [' Main Level ', ' Preprocessing', $
          ' Output Log ', ' Plot Results ']
C_menu = [' Channel Geometry Grids ', ' Profile-smoothed DEM ', $
          ' RTG file for Initial Depth ', $
          ' RTS file from Station Data ', $
          ' RTS file for Qnet Shortwave Flux ', $
          ' RTS file for Qnet Longwave Flux ', $
          ' RTS file for Fractal Rain ' ]
P_menu = [' Function ', ' RTS File ', ' RTS to MPG ']
H_menu = [' About TopoFlow ', ' License Agreement ', $
          " What's New in 1.5 ", ' Short Tutorial ']
;--------------------------------------------------------------------- 
FM = widget_button(TB, VALUE=' File ', /MENU, UVALUE='FILE_MENU')
  FM1 = widget_button(FM, VALUE=F_menu[0], UVALUE='BROWSE_DIR')
  FM2 = widget_button(FM, VALUE=F_menu[1], UVALUE='LOAD_SETTINGS')
  FM3 = widget_button(FM, VALUE=F_menu[2], UVALUE='SAVE_SETTINGS')
  FM4 = widget_button(FM, VALUE=F_menu[3], UVALUE='CLOSE_FILES')
  FM6 = widget_button(FM, VALUE=F_menu[4], UVALUE='EXIT', /SEPARATOR)
;---------------------------------------------------------------------
GM = widget_button(TB, VALUE=' Goto ', /MENU, UVALUE='GOTO_MENU')
  GM1 = widget_button(GM, VALUE=G_menu[0], UVALUE='GO_NAVIGATION')
  GM2 = widget_button(GM, VALUE=G_menu[1], UVALUE='GO_PREPROCESS')
  GM3 = widget_button(GM, VALUE=G_menu[2], UVALUE='GO_STOP_INFO')
  GM4 = widget_button(GM, VALUE=G_menu[3], UVALUE='GO_PLOT_RESULTS')
;-----------------------------------------------------------------------
CM = widget_button(TB, VALUE=' Create ', UVALUE='CREATE_MENU', /MENU)
  CM1 = widget_button(CM, VALUE=C_menu[0], UVALUE='NONE', /MENU)
    CM11 = widget_button(CM1, VALUE=' With Area Grid ', $
                         UVALUE='MAKE_CHAN_GRIDS_AREA')
    CM12 = widget_button(CM1, VALUE=' With HS Order Grid ', $
                         UVALUE='MAKE_CHAN_GRIDS_ORDER')
  CM2 = widget_button(CM, VALUE=C_menu[1], UVALUE='MAKE_SMOOTH_DEM')
  CM3 = widget_button(CM, VALUE=C_menu[2], UVALUE='MAKE_INIT_DEPTH_GRID')
  CM4 = widget_button(CM, VALUE=C_menu[3], UVALUE='MAKE_RTS_VIA_IDM')
  CM5 = widget_button(CM, VALUE=C_menu[4], UVALUE='MAKE_QNET-SW_FILE')
  CM6 = widget_button(CM, VALUE=C_menu[5], UVALUE='MAKE_QNET-LW_FILE')
  CM7 = widget_button(CM, VALUE=C_menu[6], UVALUE='MAKE_CASC_RAIN_RTS')
;-----------------------------------------------------------------------
PM = widget_button(TB, VALUE=' Plot ', /MENU, UVALUE='PLOT_MENU')
  PM1 = widget_button(PM, VALUE=P_menu[0], UVALUE='PLOT_FCN')
  PM2 = widget_button(PM, VALUE=P_menu[1], UVALUE='PLOT_RTS')
  PM3 = widget_button(PM, VALUE=P_menu[2], UVALUE='RTS_TO_MPG')
;---------------------------------------------------------------------
HM = widget_button(TB, VALUE=' Help ', /MENU, UVALUE='HELP_MENU')
  HM1 = widget_button(HM, VALUE=H_menu[0], UVALUE='ABOUT_TF')
  HM2 = widget_button(HM, VALUE=H_menu[1], UVALUE='LICENSE_TF')
  HM3 = widget_button(HM, VALUE=H_menu[2], UVALUE='WHATS_NEW', /SEPARATOR)
  HM4 = widget_button(HM, VALUE=H_menu[3], UVALUE='TUTORIAL')

;------------------------------------
;Initialize the set of wizard panels
;------------------------------------
PP = widget_base(MB, /FRAME)
for k=0,(npanels - 1) do $
    state.bases[k] = widget_base(PP, /COLUMN)

;--------------------------------
;Create the set of wizard panels
;--------------------------------
P0 = GUI_Navigate(state.bases[Panel_Number('NAVIGATE')], state)
P1 = GUI_Run_Info(state.bases[Panel_Number('RUN_INFO')], state, B1=BP1)
;*** P2 = GUI_Grid_Info(state.bases[Panel_Number('GRID_INFO')], state) 
P3 = GUI_Methods(state.bases[Panel_Number('METHODS')],  state, B1=BP3) 
P4 = GUI_Basin_Info(state.bases[Panel_Number('BASIN_INFO')], state, B1=BP4) 
P5 = GUI_Stopping(state.bases[Panel_Number('STOP_INFO')], state, B1=BP5) 
P6 = GUI_Preprocess(state.bases[Panel_Number('PREPROCESS')],   state)
P7 = GUI_Plot_Results(state.bases[Panel_Number('PLOT')], state)

;---------------------------------
;Make panels all the same size ??
;---------------------------------
Equate_Widget_Sizes, [BP1, BP3, BP4, BP5]   ;(10/8/07)

;------------------------------
;Unmap all but the first panel
;------------------------------
onoff = bytarr(npanels)
onoff[0] = 1b
for k=0,(npanels - 1) do $
    widget_control, state.bases[k], MAP=onoff[k]

;------------------------------------
;Realize widgets and wait for events
;------------------------------------
Realize_TLB, MB, state, 'GUI_TopoFlow'

;---------------------------------
;Prepare draw window for plotting
;---------------------------------------
;Must happen after realize step above.
;Assume color table is Rainbow + white.
;---------------------------------------
PLOT = 1b
if (PLOT) then begin
    widget_control, state.draw_ID, get_value=win_num
    wset, win_num
    erase, 255
    plot, [0], [0], XRANGE=[0,1], YRANGE=[0,1], $
          XMARGIN=[0,0.1], YMARGIN=[0,0.1], $
          color=0, background=255
endif

END;  GUI_TopoFlow
;******************************************************************
pro GUI_No_Method

msg = [' ', $
       'Sorry,', ' ', $
       'The selected method of "None" has', $
       'no input or output variables.', ' ']
result = GUI_Message(msg, /INFO, TITLE='No Method Selected')

END;  GUI_No_Method
;******************************************************************
pro Confirm_Exit, OK

msg = [' ', 'Exit TopoFlow ?', ' ']
answer = GUI_Message(msg, /QUESTION, TITLE='Confirm Exit')
OK = (answer eq 'Yes')

END;  Confirm_Exit
;*****************************************************************

