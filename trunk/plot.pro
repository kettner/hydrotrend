
;*****************************************************************
;   plot.pro

;   Copyright (c) 2001-2007, Scott D. Peckham
;   Created:   Dec 2001 to Jan 2002
;   Modified:  July 2005
;   Notes:     This is the only TopoFlow code file that contains
;              some calls to RiverTools routines, but then only
;              when RiverTools is present and running.
;              Names of RiverTools routines always start with "RT_":
;                RT_Load_Colors, RT_Color_Start, RT_Color_Width,
;                RT_Color_Index, RT_Read_DEM, RT_Get_Master_Size
;                & RT_Open_Window may be used.

;*****************************************************************

;   RT_Running   (function)

;   Color_Start  (function)
;   Color_Width  (function)
;   Load_Colors

;   Get_Window_Size
;   Open_Window
;   Resize        (moved to utils_TF.pro)

;   Grids_In_File
;   RTS_File_Min  (function)
;   Plot_Grid_Sequence
;   Plot_Function

;   Read_Profile_IDs
;   Plot_Flood_Wave

;   Make_PS_Plots

;*****************************************************************
function RT_Running

;-------------------------------
;Is RiverTools already running?
;-------------------------------
COMMON RTV, RT
if (n_elements(RT) ne 0) then begin
    RUNNING = (RT.main_ID ne -1L)
endif else begin
    RUNNING = 0b 
endelse

RETURN, RUNNING
END;    RT_Running
;*****************************************************************
function Color_Start, palette

FORWARD_FUNCTION RT_Color_Start

if (n_elements(palette) eq 0) then palette='IDLCT'

;--------------------------
;If RiverTools is running,
;then call RT_Color_Start
;--------------------------
if (RT_Running()) then begin
    start = RT_Color_Start(palette)  ;(e.g. palette = 'IDLCT')
endif else begin
    start = 0L
endelse

RETURN, start
END;    Color_Start
;*****************************************************************
function Color_Width, palette

FORWARD_FUNCTION RT_Color_Width

if (n_elements(palette) eq 0) then palette='IDLCT'

;--------------------------
;If RiverTools is running,
;then call RT_Color_Width
;--------------------------
if (RT_Running()) then begin
    width = RT_Color_Width(palette)
endif else begin
    width = !d.table_size
endelse

RETURN, width
END;    Color_Width 
;*****************************************************************
pro Load_Colors, CT

FORWARD_FUNCTION RT_Load_Colors

;--------------------------
;If RiverTools is running,
;then call RT_Load_Colors
;--------------------------
if (RT_Running()) then begin
    ;RT_Load_Colors, CT, 'DEM'
    RT_Load_Colors, CT, 'ALL'
endif else begin
    ;----------------------
    ;Is this a good idea ?
    ;----------------------
    device, get_visual_depth = depth
    if (depth gt 8) then device, decomposed=0

    ;---------------------
    ;Load the color table
    ;---------------------
    loadct, CT, /silent
endelse

END;  Load_Colors
;*****************************************************************
pro Get_Window_Size, ncols, nrows, xwin, ywin, $
                     factor, REDUCE, scale

;--------------------------------------------------
;NOTE:  Must use ROUND vs. FIX when
;       computing FACTOR from SCALE.
;       For example, the OK_Mosaic1 DEM has size
;       4801 x 3601, and in this case using FIX
;       causes scale to be 1./7 and FACTOR=6.
;---------------------------------------------------

;-----------------------------------------
;Get screen dimensions and set maxwinsize
;-----------------------------------------
device, get_screen_size=scr_size
xscr_size = scr_size[0]
yscr_size = scr_size[1]
maxwinsize = fix(0.8 * min(scr_size))

;-------------------------------------------------------
;Create vector [300, 299,..., 1, 1/2,..., 1/300, 1/301]
;Note:  This approach is not completely robust.
;-------------------------------------------------------
;f = findgen(300) + 1.0
;a = rotate(f,2)    ;(Reverse the order)
;b = (1 / (f + 1))
;factors = [a,b]
;maxdim  = max([ncols, nrows])
;wsizes  = (maxdim * factors)
;spot    = min(where(wsizes le maxwinsize))
;scale   = factors[spot]

;--------------------------------
;A more robust approach
;Does it work for (scale lt 1) ?
;--------------------------------
maxdim  = max([ncols, nrows])
scale   = float(maxwinsize) / maxdim 

;------------------------------
;Must use ROUND vs. FIX here !
;------------------------------
if (scale lt 1) then begin
   REDUCE=1b  &  factor=round(1./scale)
endif else begin
   REDUCE=0b  &  factor=fix(scale)
endelse

;----------------------
;Compute xwin and ywin
;----------------------
if (factor ne 1) then begin
    if (REDUCE) then begin
        xwin = fix(ncols / factor) > 1
        ywin = fix(nrows / factor) > 1
    endif else begin
        xwin=fix(factor) * ncols
        ywin=fix(factor) * nrows
    endelse
endif else begin
    xwin = ncols
    ywin = nrows
endelse

;--------------------------------
;Modify for RT map projections ?
;--------------------------------

;xwin = (xwin < fix(0.98 * xscr_size))
;ywin = (ywin < fix(0.98 * yscr_size))

END;  Get_Window_Size
;*****************************************************************
pro Open_Window, XSIZE=xsize, YSIZE=ysize, TITLE=title, $
                 SIZEABLE=SIZEABLE

FORWARD_FUNCTION RT_Open_Window
SIZEABLE = keyword_set(SIZEABLE)

;----------------------------------
;OK here, or only in Load_Colors ?
;----------------------------------
device, decomposed=0

if (RT_Running()) then begin
    RT_Open_Window, XSIZE=xsize, YSIZE=ysize, TITLE=title, $
                    SIZEABLE=SIZEABLE
endif else begin
    window, /free, XSIZE=xsize, YSIZE=ysize, TITLE=title
endelse

END;  Open_Window
;*****************************************************************
function Grids_In_File, unit, nx, ny

;----------------------------------
;How many (nx x ny) grids in file
;assuming data type is FLOAT ?
;----------------------------------
temp     = fstat(unit)
filesize = temp.size
n_grids  = (filesize / (4L * long(nx) * long(ny)))

RETURN, n_grids
END;    Grids_In_File 
;***************************************************************
function RTS_File_Min, RTS_file, MAX=smax, $
         DIFF_GRID=DIFF_GRID, REPORT=REPORT, IMAX=imax, $
         VERBOSE=VERBOSE
 
REPORT    = keyword_set(REPORT)
DIFF_GRID = keyword_set(DIFF_GRID)
VERBOSE   = keyword_set(VERBOSE)

;-------------------------------------
;Open RTS/RTG file for associated I/O
;-------------------------------------
Open_RTS_File, RTS_file, RTS_unit, grids, N_GRIDS=n_grids, /ASSOCIATED

;--------------------------------
;Option to use "difference grid"
;--------------------------------
if (DIFF_GRID) then begin
    im0 = grids[1] - grids[0]
endif else begin
    im0 = grids[0]
endelse

;--------------------------------
;Find min and max over all grids
;--------------------------------
imin = 0L
imax = 0L
smin = min(im0, max=smax)
for k=1L,(n_grids - 1L) do begin
    if (DIFF_GRID) then begin
        im = grids[k] - grids[k-1]
    endif else begin
        im = grids[k]
    endelse

    ;-------------------------------
    ;Find min and max of this frame
    ;-------------------------------
    gmin = min(im, max=gmax)
    if (VERBOSE) then begin
        print,'(gmin, gmax) = ', gmin, gmax
    endif

    ;--------------------
    ;Update smin value ?
    ;--------------------
    if (gmin lt smin) then begin
        smin = gmin
        imin = k
    endif

    ;--------------------
    ;Update smax value ?
    ;--------------------
    if (gmax gt smax) then begin
        smax = gmax
        imax = k     ;(index of "peak" grid)
    endif
endfor

;----------------
;Optional report
;----------------
if (REPORT) then begin
    nstr    = TF_String(n_grids)
    sstr    = TF_String(smin) + ', ' + TF_String(smax)
    iminstr = TF_String(imin)
    imaxstr = TF_String(imax)
    ;-------------------------------------------------
    print,'Number of grids in RTS file = ' + nstr
    print,'(Min, Max) over all grids   = ' + sstr
    print,'Min occurs in grid number   = ' + iminstr
    print,'Max occurs in grid number   = ' + imaxstr 
    print,' '
endif

;-------------------
;Close the RTS file
;-------------------
free_lun, RTS_unit

RETURN, smin

END;  RTS_File_Min
;***************************************************************
pro Plot_Grid_Sequence, DT=DT, LAST=LAST, MPEG=MPEG, $
                        DIFF_GRID=DIFF_GRID, $
                        CONTOURS=CONTOURS

;------------------------------------------------------------
;Notes:  This routine displays a sequence of grids that are
;        stored in a file with extension ".rts" as a simple
;        animation.  Each grid is assumed to have type of
;        FLOAT.  Can also be used to view a FLOAT-type RTG
;        file.  DT is wait time between frames.  The LAST
;        keyword can be used to show the last grid.
;------------------------------------------------------------

;-----------------
;Keyword defaults
;-----------------
if NOT(keyword_set(DT)) then DT=0.02
DIFF_GRID = keyword_set(DIFF_GRID)
CONTOURS  = keyword_set(CONTOURS)
;CONTOURS  = 1b

;---------------------------
;Open file selection dialog
;---------------------------
RTS_file = dialog_pickfile(/MUST_EXIST, FILTER=['*.rts','*.rtg'])
if (RTS_file eq '') then RETURN

;-----------------------------
;Find min and max in RTS file
;before it is opened below.
;-----------------------------
smin = RTS_File_Min(RTS_file, MAX=smax, DIFF_GRID=DIFF_GRID)
print,'(smin, smax) = ', smin, smax

;-------------------------------------
;Open RTS/RTG file for associated I/O
;-------------------------------------
Open_RTS_File, RTS_file, RTS_unit, grids, $
               NX=nx, NY=ny, N_GRIDS=n_grids, /ASSOCIATED
if (keyword_set(LAST)) then n0=(n_grids - 1L) else n0=0L

;------------------------------
;Get optimal window dimensions
;------------------------------
Get_Window_Size, nx, ny, xwin, ywin, factor, REDUCE, scale

;----------------------------
;Make window and movie small
;----------------------------
;;FACTOR = 1 ;(Means xwin=nx, ywin=ny.  Smallest MPEG.) 
;FACTOR = 10 
;REDUCE = 0

;-------------------------------------
;Prepare to display sequence of grids
;--------------------------------------
;Assume image size is enlarged for now
;--------------------------------------
;xwin = fix(FACTOR * nx)
;ywin = fix(FACTOR * ny)

;------------------------------
;Prepare a window for plotting
;------------------------------
Open_Window, XSIZE=xwin, YSIZE=ywin    ;*****
win_index = !d.window
!order = 1
;-----------------------------------
;Must load colors after Open_Window
;-----------------------------------
;Load_Colors, 15    ;(Stern special)
;Load_Colors, 23    ;(Purple-red + stripes)   ;(Not bad)
;Load_Colors, 28    ;(Hardcandy)
;Load_Colors, 31    ;(Peppermint)
;Load_Colors, 34    ;(rainbow)
;Load_Colors, 37    ;(waves)
;Load_Colors, 38    ;(rainbow 18)
Load_Colors, 39    ;(rainbow + white)
;Load_Colors, 40    ;(rainbox + black)

;--------------------------------------
;Make special color table from Rainbow
;that has evenly-spaced white stripes
;--------------------------------------
tvlct, R,G,B, /get
;rev_R = reverse(R)
;rev_G = reverse(G)
;rev_B = reverse(B)
;R = reverse(R)   ;(for reverse rainbow)
;G = reverse(G)
;B = reverse(B)
ramp = indgen(Color_Width()) + Color_Start()
w = where(((ramp mod 5) eq 0) AND (ramp gt 121), nw)
if (nw ne 0) then begin
    ;R[w] = rev_R[w]
    ;G[w] = rev_G[w]
    ;B[w] = rev_B[w]
    ;---------------------------------------------
    ;R[w]=255  &  G[w]=255  &  B[w]=255   ;(white)
    ;---------------------------------------------
    R[w]=255  &  G[w]=0    &  B[w]=0     ;(red)
    ;---------------------------------------------
    ;R[w]=0    &  G[w]=0    &  B[w]=0     ;(black)
    ;---------------------------------------------
    tvlct, R,G,B
    modifyct, 41, 'Rainbow_Red_Pulse', R,G,B
endif

;-------------------------------------
;Prepare to create an MPEG movie from
;sequence of grids in a RTS file.
;-------------------------------------
;Requires special MPEG license
;Can't give RATE before SET.
;-------------------------------------
;MPEG = 1b
MPEG = keyword_set(MPEG)
if (MPEG) then begin
    rate    =  10  ;(between 1 and 100)
    quality = 100  ;(between 1 and 100)
    xinteranimate, rate, set=[xwin, ywin, n_grids], $
                   ;**/showload, $
                   /mpeg_open, mpeg_filename='F:\TEST.mpg', $
                   mpeg_quality=quality, $
                   mpeg_bitrate=429496729200.0d
    ;--------------------------------------
    ;Can't give rate in separate call here
    ;--------------------------------------
    ;rate = 10   ;(between 1 and 100)
    ;xinteranimate, rate
endif 

;-------------------------------------
;Prepare to display contour overlay.
;Make smooth, resized version of DEM.
;-------------------------------------
if (CONTOURS) then begin
    RT_Read_DEM, dem
    dem = rotate(temporary(dem), 7)   ;(flip y-axis)
    dem = Resize(dem, FACTOR, REDUCE, SAMP_TYPE=0)

    ;-----------------------
    ;Compute contour levels
    ;-----------------------
    emin = min(dem, max=emax)
    print,'(emin, emax) = ', emin, emax
    n_conts = 5
    ;n_conts = 12
    LEVELS = (findgen(n_conts) + 1) / float(n_conts)
    range  = (emax - emin)
    LEVELS = (LEVELS * range) + emin

    ;--------------------------
    ;Compute position argument
    ;--------------------------
    SAMP_TYPE = 1b
    if (SAMP_TYPE eq 1b) then begin
        position = [0,0,(xwin - 1),(ywin - 1)]
    endif else begin
        ;-----------------------------------------------
        ;Slight correction for vector contours overlaid
        ;on filled contours for Small; not perfect.
        ;-----------------------------------------------
        ff = (FACTOR - 1)
        position=[0,ff,(xwin - 1),(ywin + ff - 1)]
    endelse
 
    ;----------------------------
    ;Set contour line attributes
    ;----------------------------
    ;c_colors  = [RT_Color_Index('white')]
    c_colors   = [RT_Color_Index('gray')]
    c_thick    = [1]
    c_labels   = [0]
    c_styles   = [0]   ;(solid)
    ;c_styles   = [1]    ;(dots)
    ;c_styles   = [2]   ;(dashes)
    background = RT_Color_Index('black')

endif

;------------------------------
;Display the sequence of grids
;------------------------------
;n0 = 100  ;*******************

for n=n0,(n_grids - 1) do begin
  
    ;--------------------------
    ;Get image to display
    ;NB! Edge pixels are zero.
    ;--------------------------
    if (DIFF_GRID) then begin
        im = grids[n] - grids[(n-1)>0]
    endif else begin
        im = grids[n]
    endelse

    ;----------------------------------
    ;Store locations of "max" pixels ?
    ;----------------------------------
    ;gmax = max(im, min=gmin)
    ;wm = where(im eq gmax)

    ;-----------------------
    ;Optional info messages
    ;-----------------------
    ;gmin = min(im, max=gmax)
    ;mstr = '(' + TF_String(gmin) + ', ' + TF_String(gmax) + ')'
    ;nstr  = TF_String(n) + ' of ' + TF_String(n_grids)

    ;print,'Displaying grid number: ' + nstr
    ;print,'(Min, Max) values in grid = ' + mstr
    ;print,'-----------------------------------------------------'

    ;--------------------------
    ;Zero out undefined values
    ;--------------------------
    ;wf = where(NOT(FINITE(im)), nf)
    ;if (nf ne 0) then im[wf] = 0.0

    ;-------------------------------------
    ;Save pixels with non-positive values
    ;Can't use this with the change grid
    ;-------------------------------------
    ;zeros = where(im le 0, n_zeros)
    n_zeros = 0L

    ;-------------------------------------------
    ;Rescale values to [0,1] via gmin and gmax.
    ;-------------------------------------------
    ;gmin = min(im, max=gmax)
    ;im = (im - gmin) / (gmax - gmin)

    ;-------------------------------------------
    ;Rescale values to [0,1] via smin and smax.
    ;-------------------------------------------
    im = (im - smin) / (smax - smin)

    ;--------------------------------------------
    ;Color pixels with alternating black & white
    ;--------------------------------------------
    ;inc = 0.05
    ;;t   = (inc / 2.0)
    ;t   = (inc / 4.0)
    ;w = where((im mod inc) lt t, nw, COMP=w2, NCOMP=nw2)
    ;if (nw ne 0)  then im[w]  = RT_Color_Index('black')
    ;if (nw2 ne 0) then im[w2] = RT_Color_Index('white')

    ;-------------------------------------------
    ;Stretch image with a function (e.g. power
    ;law) that maps [0,1] to [0,1]. 
    ;-------------------------------------------
    ;im = abs(im)^0.3d
    ;im = abs(im)^0.4d 
    im = abs(im)^0.25d  ;*****
    ;im = abs(im)^0.2d 
    ;im = abs(im)^0.1d
    ;im = alog(abs(im + 1))/alog(2)
    ;im = (alog(abs(im + 1))/alog(2))^2d
    ;im = (alog(abs(im + 1))/alog(2))^0.5d
    ;im = (alog(abs(im + 1))/alog(2))^0.3d
    ;im = (alog(abs(im + 1))/alog(2))^0.25d
    ;im = (alog(abs(im + 1))/alog(2))^0.2d
 
    ;imin = min(im, max=imax)
    ;print,'(imin, imax) = ',imin, imax
    
    ;------------------------------------
    ;Scale values for the RT color table
    ;------------------------------------
    imin = Color_Start('IDLCT')
    ifac = Color_Width('IDLCT')
    im = (im * ifac) + imin

    ;--------------------------------------------
    ;Set pixel with max value to be red or white
    ;--------------------------------------------
    ;;im[wm] = RT_Color_Index('red')
    ;im[wm] = RT_Color_Index('white')

    ;----------------------------------
    ;Set pixels with zeros to be white
    ;----------------------------------
    if (n_zeros ne 0) then im[zeros] = RT_Color_Index('white')

    ;-----------------------
    ;Create an MPEG movie ?
    ;(before resizing)
    ;-----------------------
    if (MPEG) then begin
        ;----------------------
        ;Rate is now set above
        ;----------------------
        ;rate = 10   ;(between 1 and 100)

        ;-----------------------------
        ;Get image from grid, but
        ;this doesn't get the colors.
        ;Don't use ORDER keyword
        ;-----------------------------
        ;xinteranimate, rate, frame=n, image=im

        ;-----------------------------------
        ;This gets colors but sometimes get
        ;out of memory errors from TVRD
        ;because of all the pixmaps.
        ;-----------------------------------
        im2 = tvrd(true=1)
        xinteranimate, rate, frame=n, image=im2

        ;-----------------------------
        ;Capture image from window;
        ;Need this to get colors
        ;Don't use KEEP_PIXMAPS here
        ;-----------------------------
        ;Gets only a single frame ???
        ;-----------------------------
        ;xinteranimate, rate, window=win_index, /order
    endif

    ;------------------------
    ;Display grid as image ?
    ;------------------------
    ;** im = Resize(im, FACTOR, REDUCE, SAMP_TYPE=0) ;(bilinear)
    im = Resize(im, FACTOR, REDUCE, SAMP_TYPE=1)    ;(nearest neigh.)
    if (MPEG) then wset, win_index
    tv, im   ;****
    ;wait, DT

    ;--------------------------
    ;Overlay smooth contours ?
    ;--------------------------
    if (CONTOURS) then begin
        contour, dem, FOLLOW=1b, CLOSED=0, /DEVICE, $
                 /OVERPLOT, /NOERASE, $
                 levels = LEVELS, $
                 position = position,  $
                 background = background, $ 
                 xstyle=5, ystyle=5, $
                 c_colors=c_colors, c_thick=c_thick, $
                 c_labels=c_labels, c_linestyle=c_styles
        ;** print,'Contours drawn; returning...'
        ;** RETURN  ;********
    endif

endfor

;-------------------------------
;Option to create an MPEG movie
;Need to have done KEEP_PIXMAPS
;Don't use ORDER keyword
;-------------------------------
;if (MPEG) then begin
;    rate = 10   ;(between 1 and 100)
;    xinteranimate, rate, /mpeg_open, $
;                   mpeg_filename='F:\TEST.mpg'
;    xinteranimate, /mpeg_close
;endif

;------------------
;Close MPEG file ?
;------------------
if (MPEG) then begin
    xinteranimate, /mpeg_close
    print,'Finished writing MPEG movie to:'

    print,'   ' + 'F:\TEST.mpg'
endif

;---------------
;Close RTS file
;---------------
print,'Finished plotting grid sequence.'
print,' '
free_lun, RTS_unit

END;  Plot_Grid_Sequence
;***************************************************************
pro Plot_Function

;---------------------------
;Open file selection dialog
;---------------------------
file = dialog_pickfile(/MUST_EXIST, FILTER=['*.txt'])
if (file eq '') then RETURN

;-----------------------------
;Use filename as window title
;-----------------------------
Get_Data_Prefix, file, prefix, filename
title = filename

Count_Lines, n_lines, file, /SILENT

;----------------------------
;Open the file and strip off
;the 2-line header
;----------------------------
TF_Get_LUN, unit, file
openr, unit, file 
line = ''
for k=0,1 do readf, unit, line

;--------------------------
;Read the times and values
;--------------------------
t = 0.0
f = 0.0
x = fltarr(n_lines - 2)
y = fltarr(n_lines - 2)

for k=2,(n_lines - 1) do begin
    readf, unit, t, f
    x[k-2] = t
    y[k-2] = f
endfor

;---------------------
;Close the input file
;---------------------
free_lun, unit

;-------------
;Plot x vs. y
;-------------
Open_Window, xsize=600, ysize=400, TITLE=title
     ;**** /SIZEABLE (Not ready yet)
Load_Colors, 39    ;(rainbow + white)
plot, x, y, psym=-1, XTITLE='Time (min)', YTITLE='Value'


END;  Plot_Function
;***************************************************************
pro Read_Profile_IDs, p_file, ID_file, ncols

;------------------------
;Count lines in the file
;------------------------
;Count_Lines, n_lines, p_file, /SILENT

;---------------
;Open the files
;---------------
TF_Get_LUN, unit, p_file
openr, unit,  p_file
TF_Get_LUN, unit2, ID_file 
openw, unit2, ID_file 

;---------------------
;Strip off the header
;---------------------
line=''
for k=1,6 do readf,unit,line

;-----------------------
;Read the cols and rows
;-----------------------
lon  = 0.0
lat  = 0.0
elev = 0.0
col  = 0L
row  = 0L
dist = 0.0

;----------------------------
;Method that uses while loop
;----------------------------
while NOT(EOF(unit)) do begin
    readf, unit, lon, lat, elev, col, row, dist
    ID = long((row * ncols) + col)
    printf, unit2, ID
endwhile

;--------------------------
;Method that uses for loop
;--------------------------
;IDs  = lonarr(n_lines - 6)
;for k=0L,(n_lines - 6 - 1) do begin
;    readf, unit, lon, lat, elev, col, row, dist
;    IDs[k] = long((row * ncols) + col)
;endfor
;printf, unit2, IDs

;----------------
;Close the files
;----------------
free_lun, unit, unit2
print, 'Finished.'

END;  Read_Profile_IDs
;***************************************************************
pro Plot_Flood_Wave, RTS_file, ID_file, DIFF_GRID=DIFF_GRID

DIFF_GRID = keyword_set(DIFF_GRID)
if (DIFF_GRID) then ytitle = 'Change in Discharge (m^3 / s)' $
               else ytitle = 'Discharge (m^3 / s)'

;--------------------
;Get the profile IDs
;--------------------
Count_Lines, n_lines, ID_file, /SILENT
IDs = lonarr(n_lines)
TF_Get_LUN, I_unit, ID_file
openr, I_unit, ID_file 
readf, I_unit, IDs
free_lun, I_unit
IDs = reverse(IDs)

;-----------------------------
;Find min and max in RTS file
;before opening it below
;-----------------------------
smin = RTS_File_Min(RTS_file, MAX=smax, DIFF_GRID=DIFF_GRID)
yrange = [smin, smax]

;--------------------------------
;Open window for flood wave plot
;--------------------------------
;plot_color = 19   ;**** RT.plotcolor, white
;back_color = 0    ;**** RT.backcolor, black 
Open_Window, xsize=600, ysize=400
;window, 0, xsize=600, ysize=400
Load_Colors, 39      ;(gives white on black, unless RT_Running)

;---------------------------------
;Open RTS file for associated I/O
;---------------------------------
Open_RTS_file, RTS_file, RTS_unit, grids, N_GRIDS=n_grids, /ASSOCIATED

;------------------------------
;Plot the flood wave over time
;------------------------------
for n=0L,(n_grids - 1L) do begin
    if (DIFF_GRID) then begin
        grid = grids[n] - grids[(n-1)>0L]
    endif else begin
        grid = grids[n]
    endelse
    wave = grid[IDs]

    ;erase
    plot, wave, psym=-3, yrange=yrange, $
          xtitle = 'Distance (pixels)', ytitle=ytitle
          ;** color = plot_color, background = back_color
    wait, 0.1
endfor

;-------------------
;Close the RTS file
;-------------------
free_lun, RTS_unit

END;  Plot_Flood_Wave
;***************************************************************
pro Make_PS_Plots, Q_out, u_out, d_out, t_out, $
                   Q_file, u_file, d_file

if (n_elements(Q_file) eq 0) then Q_file='Q_NEW.ps'
if (n_elements(u_file) eq 0) then u_file='U_NEW.ps'
if (n_elements(d_file) eq 0) then d_file='D_NEW.ps'

set_plot,'ps'
device, filename=Q_file
plot, t_out, Q_out, XTITLE='Time (minutes)', $
                    YTITLE='Discharge (m^3/s)'
device, /close

device, filename=u_file
plot, t_out, u_out, XTITLE='Time (minutes)', $
                    YTITLE='Velocity (m/s)'
device, /close

device, filename=d_file
plot, t_out, d_out, XTITLE='Time (minutes)', $
                    YTITLE='Depth (m)'
device, /close

set_plot,'win'

END;  Make_PS_Plots
;***************************************************************

