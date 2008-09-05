
;***************************************************************
;   preprocess.pro

;   Copyright (c) 2001-2005, Scott D. Peckham
;   Created:  March 2004
;   Notes:    Usable, but not yet added to the GUI.
;             Some RiverTools routines are used, so these
;             routines require RiverTools to be running.

;***************************************************************

;   Make_RTS_From_Avg_Temp_Grid

;   Total_Precip_at_Gauge            ;(function)
;   Make_RTS_From_Total_Precip_Grid
;   Check_RTS_File

;   Regrid_Climate_Grid

;***************************************************************
pro Make_RTS_From_Avg_Temp_Grid, avg_grid_file, ID, $
                           series_file, RTS_file, $
                           LINES_TO_SKIP=lines_to_skip, $
                           COL_TO_READ=col_to_read, $
                           FACTOR=factor

;---------------------------------------------------------------
;Notes:  This routine builds an RTS file from a time-averaged
;        grid (in an RTG file) and a time-series of values at
;        a particular (gauged) pixel.  For example, the time-
;        averaged grid might give the annual average of temp
;        or precip from PRISM as a single spatial grid. This
;        spatial grid may show elevation effects.

;        If we then have measurements at a particular location
;        (via pixel ID) throughout the year, these can be used
;        via simple vertical translation to create a grid
;        sequence (RTS file) with the same spatial structure
;        as the average grid that agrees with values at the
;        measured location.

;        This routine may only make sense for temperature
;        grids since there are many days with no precip.
;---------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if NOT(keyword_set(LINES_TO_SKIP)) then lines_to_skip=0L
if NOT(keyword_set(COL_TO_READ)) then col_to_read=0L

;--------------------------------
;Construct RTI filename and read
;DEM info to get the byte_order
;for possible byte-swapping
;--------------------------------
Get_RTI_Filename, avg_grid_file, RTI_file
Read_RTI_File, RTI_file, info

;----------------------------
;Read the time-averaged grid
;----------------------------
Read_Grid, avg_grid, avg_grid_file, TYPE='FLOAT'  ;**  /REPORT

;--------------------------------
;Count lines in time series file
;--------------------------------
Count_Lines, n_lines, series_file, /SILENT
n = (n_lines - lines_to_skip)
vals = fltarr(n)

;** print,'n_lines = ', n_lines

;---------------------------------------
;Open series file and skip header lines
;---------------------------------------
TF_Get_LUN, unit, series_file
openr, unit, series_file 
line = ''
for i=0L,(lines_to_skip - 1L) do readf,unit,line

;------------------------------------
;Read the specified column from file
;------------------------------------
line_vals = fltarr(col_to_read + 1L)
for j=0L,(n-1L) do begin
    readf, unit, line_vals
    vals[j] = line_vals[col_to_read]
endfor
free_lun, unit

;------------------------------------
;Read the specified column from file
;Doesn't work if extra blank lines.
;------------------------------------
;line_vals = fltarr(col_to_read + 1L)
;j = 0L
;while NOT(EOF(unit)) do begin
;    readf, unit, line_vals
;    vals[j] = line_vals[col_to_read]
;    j = (j + 1L)
;endwhile
;vals = vals[0:j-1L]
;free_lun, unit

;-------------------------------------
;Multiply by unit conversion factor ?
;-------------------------------------
if (keyword_set(FACTOR)) then vals = (vals * factor)

;---------------------------
;Open the RTS file to write
;---------------------------
TF_Get_LUN, unit, RTS_file
openw, unit, RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
for k=0L,(n-1L) do begin
    z_shift = (vals[k] - avg_grid[ID])
    grid = (avg_grid + z_shift)
    writeu, unit, grid

    ;-----------------------------
    ;Print out the grid min & max
    ;-----------------------------
    ;gmin = min(grid, max=gmax)
    ;print,'(gmin, gmax) = ', gmin, gmax
endfor

;-----------------------
;Close the new RTS file
;-----------------------
free_lun, unit

end;  Make_RTS_From_Avg_Temp_Grid
;***************************************************************
function Total_Precip_at_Gauge, series_file, $
                      LINES_TO_SKIP=lines_to_skip, $
                      COL_TO_READ=col_to_read

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN, 0d

if NOT(keyword_set(LINES_TO_SKIP)) then lines_to_skip=0L
if NOT(keyword_set(COL_TO_READ)) then col_to_read=0L

;--------------------------------
;Count lines in time series file
;--------------------------------
Count_Lines, n_lines, series_file, /SILENT
n = (n_lines - lines_to_skip)
vals = fltarr(n)

;---------------------------------------
;Open series file and skip header lines
;---------------------------------------
TF_Get_LUN, unit, series_file
openr, unit, series_file 
line = ''
for i=0L,(lines_to_skip - 1L) do readf,unit,line

;------------------------------------
;Read the specified column from file
;------------------------------------
line_vals = fltarr(col_to_read + 1L)
for j=0L,(n-1L) do begin
    readf, unit, line_vals
    vals[j] = line_vals[col_to_read]
endfor
free_lun, unit

;--------------
;Print the sum
;--------------
sum = total(vals, /double)
print,'sum = ' + TF_String(sum)

RETURN, sum

end;  Total_Precip_at_Gauge
;***************************************************************
pro Make_RTS_From_Total_Precip_Grid, total_grid_file, ID, $
                        series_file, RTS_file, $
                        LINES_TO_SKIP=lines_to_skip, $
                        COL_TO_READ=col_to_read, $
                        FACTOR=factor, EVENT_DUR=event_dur

;---------------------------------------------------------------
;Notes:  This routine builds an RTS file from a total precip
;        grid (in an RTG file) and a time-series of values at
;        a particular (gauged) pixel.

;        If we then have measurements at a particular location
;        (via pixel ID) throughout the year, these can be used
;        via simple vertical translation to create a grid
;        sequence (RTS file) with the same spatial structure
;        as the average grid that agrees with values at the
;        measured location.

;        Assume that all precip events last for 3 hours,
;        (10800 seconds) or change this duration using the
;        EVENT_DUR keyword.  This is needed to compute rates.

;---------------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

if NOT(keyword_set(LINES_TO_SKIP)) then lines_to_skip=0L
if NOT(keyword_set(COL_TO_READ)) then col_to_read=0L
if NOT(keyword_set(EVENT_DUR)) then event_dur = 10800.0  ;[sec]

;--------------------------------------------
;Sum to get total precip at gauge for 1 year
;For our case, read column 4 (starting at 0)
;Convert from mm to meters.
;--------------------------------------------
sum = Total_Precip_at_Gauge(series_file, $
                            COL_TO_READ=col_to_read, $
                            LINES_TO_SKIP=lines_to_skip)
sum = (sum / 1000d)   ;[mm -> meters]

;--------------------------------
;Construct RTI filename and read
;DEM info to get the byte_order
;for possible byte-swapping
;--------------------------------
Get_RTI_Filename, total_grid_file, RTI_file
Read_RTI_File, RTI_file, info
nx = info.ncols
ny = info.nrows

;----------------------------------------------
;Read the total precip grid: INTEGER type [mm]
;----------------------------------------------
Read_Grid, total_grid, total_grid_file, TYPE='INTEGER'
           ;**  /REPORT

;-----------------------------------------
;Convert units from total mm to meters
;Rescale total grid to match sum at gauge
;-----------------------------------------
total_grid = (total_grid / 1000d)
total_grid = total_grid * (sum / total_grid[ID])

;--------------------------------
;Count lines in time series file
;--------------------------------
Count_Lines, n_lines, series_file, /SILENT
n = (n_lines - lines_to_skip)
vals = fltarr(n)

;** print,'n_lines = ', n_lines

;---------------------------------------
;Open series file and skip header lines
;---------------------------------------
TF_Get_LUN, unit, series_file
openr, unit, series_file 
line = ''
for i=0L,(lines_to_skip - 1L) do readf,unit,line

;------------------------------------
;Read the specified column from file
;Convert from mm to meters (1 day)
;------------------------------------
line_vals = fltarr(col_to_read + 1L)
for j=0L,(n-1L) do begin
    readf, unit, line_vals
    vals[j] = line_vals[col_to_read]
endfor
free_lun, unit
vals = (vals / 1000d)  ;[mm -> meters]

;------------------------------------
;Read the specified column from file
;Doesn't work if extra blank lines.
;------------------------------------
;line_vals = fltarr(col_to_read + 1L)
;j = 0L
;while NOT(EOF(unit)) do begin
;    readf, unit, line_vals
;    vals[j] = line_vals[col_to_read]
;    j = (j + 1L)
;endwhile
;vals = vals[0:j-1L]
;free_lun, unit

;-------------------------------------
;Multiply by unit conversion factor ?
;-------------------------------------
;if (keyword_set(FACTOR)) then vals = (vals * factor)

;---------------------------
;Open the RTS file to write
;---------------------------
zero_grid = fltarr(nx, ny)
TF_Get_LUN, unit, RTS_file
openw, unit, RTS_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(info.byte_order)
for k=0L,(n-1L) do begin
    if (vals[k] le 0.0) then begin
        ;-------------------------------------
        ;Whole grid is zero if gauge has zero
        ;-------------------------------------
        P = zero_grid
    endif else begin
        daily_total = total_grid * (vals[k] / total_grid[ID])
        P = float(daily_total / event_dur)  ;[m/s]
    endelse

    writeu, unit, P

    ;------------------------------------
    ;Print out the precip grid min & max
    ;------------------------------------
    pmin = min(P, max=pmax)
    print,'(pmin, pmax) = ', pmin, pmax
endfor

;-----------------------
;Close the new RTS file
;-----------------------
free_lun, unit

end;  Make_RTS_From_Total_Precip_Grid
;***************************************************************
pro Regrid_Climate_Grid, new_prefix, DEM_RTI_file, $
                         NEAREST=NEAREST, FACTOR=factor 

;----------------------------------------------------------
;Notes:  This assumes that you have already imported the
;        climate grid (e.g. precip or temp) as if it were
;        a DEM into RiverTools, and that you have opened
;        the data set associated with this "DEM".

;        DEM_RTI_file is the RTI file for the DEM that
;        we want the new climate grid to be registered to.
;        The bounding box of this DEM must be contained
;        within the bounding box of the climate grid.

;NB!     If both the DEM and the climate grid have the
;        same pixel geometry (Geographic or UTM) then do
;        all calculations with that pixel geometry.

;        Use the FACTOR keyword to specify a unit
;        conversion factor.
;----------------------------------------------------------
BILINEAR = NOT(keyword_set(NEAREST))
print,'Creating climate grid registered to DEM...'

;---------------------------------
;Read the info for the actual DEM
;---------------------------------
RT_Read_DEM_Info, info, RTI_FILE=DEM_RTI_file ;***, /REPORT

;-----------------------------------
;Read the climate grid and its info
;-----------------------------------
RT_Read_DEM, cgrid


RT_Read_DEM_Info, cgrid_info ;***, /REPORT
;*** COMMON RTV, RT
;*** Get_RTI_Filename, RT.DEM_file, cgrid_RTI_file

;-------------------------------------
;Convert xres, yres to fixed-length ?
;-------------------------------------
if (info.pixel_geom eq 0b) then begin
    ;---------------------------------
    ;Compute average pixel dimensions
    ;---------------------------------
    ;*** Get_Pixel_Sizes, dx,dy,dd, da, DEM_RTI_file, /METERS
    RT_Get_Pixel_Sizes, dx, dy, /METERS, $
                        RTI_FILE=DEM_RTI_file
    xres = total(dx, /double) / n_elements(dx)
    yres = total(dy, /double) / n_elements(dy)
endif else begin
    xres = info.xres
    yres = info.yres
endelse

if (cgrid_info.pixel_geom eq 0b) then begin
    ;---------------------------------
    ;Compute average pixel dimensions
    ;---------------------------------
    ;*** Get_Pixel_Sizes, dx2,dy2,dd2, da2, cgrid_RTI_file, /METERS
    RT_Get_Pixel_Sizes, dx2, dy2, /METERS
    cgrid_xres = total(dx2, /double) / n_elements(dx2)
    cgrid_yres = total(dy2, /double) / n_elements(dy2)
endif else begin
    cgrid_xres = cgrid_info.xres
    cgrid_yres = cgrid_info.yres
endelse

;--------------------------------
;Compute the new ncols and nrows
;--------------------------------
new_ncols = ceil(cgrid_info.ncols * (cgrid_xres / xres))
new_nrows = ceil(cgrid_info.nrows * (cgrid_yres / yres))
;print,'new_ncols = ', new_ncols
;print,'new_nrows = ', new_nrows

;--------------------------------------
;Regrid to new dimensions with CONGRID
;--------------------------------------
if (BILINEAR) then begin
   new_grid = congrid(cgrid, new_ncols, new_nrows, /INTERP)
endif else begin
   new_grid = congrid(cgrid, new_ncols, new_nrows)
endelse

;----------------------------------------------------
;Convert regridded climate grid to decimal degrees ?
;----------------------------------------------------
if (cgrid_info.pixel_geom eq 1b) then begin
    czone = cgrid_info.UTM_zone
    RT_Convert_UTM_to_Geo, cgrid_info.x_west_edge, $
                           cgrid_info.y_south_edge, $
                           czone, cgrid_minlon, cgrid_minlat
endif else begin
    cgrid_minlon = cgrid_info.x_west_edge
    cgrid_minlat = cgrid_info.y_south_edge
endelse

;----------------------------------------------
;Convert DEM bounding box to decimal degrees ?
;----------------------------------------------
if (info.pixel_geom eq 1b) then begin
    RT_Convert_UTM_to_Geo, info.x_west_edge, info.y_south_edge, $
                           info.UTM_zone, minlon, minlat
    x0 = (info.x_west_edge  + info.x_east_edge)  / 2d
    y0 = (info.y_south_edge + info.y_north_edge) / 2d
    RT_Convert_UTM_to_Geo, x0, y0, info.UTM_zone, lon0, lat0
    RT_Convert_UTM_to_Geo, (x0 + info.xres), (y0 + info.yres), $
                           info.UTM_zone, lon1, lat1 
    xres_deg = (lon1 - lon0)   ;(degrees)
    yres_deg = (lat1 - lat0)
endif else begin
    minlon   = info.x_west_edge
    minlat   = info.y_south_edge
    xres_deg = (info.xres / 3600d)    ;(arcsec -> deg)
    yres_deg = (info.yres / 3600d)
endelse

;---------------------------------------------
;Subset regridded grid to bounding box of DEM
;---------------------------------------------
start_col = round((minlon - cgrid_minlon) / xres_deg)
start_row = round((minlat - cgrid_minlat) / yres_deg)
stop_col  = (start_col + info.ncols - 1L)
stop_row  = (start_row + info.nrows - 1L)
sub_grid  = new_grid[start_col:stop_col, start_row:stop_row]
;print,'start_col = ', start_col
;print,'start_row = ', start_row

;---------------------------------
;Apply a unit conversion factor ?
;---------------------------------
if (keyword_set(FACTOR)) then begin
    sub_grid = float(sub_grid * factor)
    info.data_type = 'FLOAT'
endif else begin
    info.data_type = cgrid_info.data_type
endelse

;------------------------------------------
;Save new climate grid with DEM dimensions
;------------------------------------------
new_file = (new_prefix + '_DEM.rtg')
TF_Get_LUN, unit, new_file
openw, unit, new_file 
writeu, unit, sub_grid
free_lun, unit

;--------------------------------------------
;Create an RTI file for the new climate grid
;--------------------------------------------
;Most info will now be same as the DEM
;--------------------------------------------
gmin = min(sub_grid, max=gmax)
info.emin        = gmin
info.emax        = gmax
info.data_source = 'Regridded climate grid'
;-------------------------------------------
Get_RTI_Filename, new_file, new_RTI_file
RT_Make_RTI_File, new_RTI_file, info

;--------------
;Final message
;--------------
print,'Finished.'
print,' '

end;  Regrid_Climate_Grid
;***************************************************************


