
;***************************************************************
;   postprocess.pro

;   Copyright (c) 2004-2006, Scott D. Peckham
;   Created:  March 2004
;   Notes:    Usable, but not yet implemented in the GUI.

;***************************************************************

;   Extract_RTG_From_RTS
;   Extract_Series_From_RTS

;***************************************************************
pro Extract_RTG_From_RTS, RTS_file, RTG_file, n

;------------------------------
;Does RTG file exist already ?
;------------------------------
Check_Overwrite, RTG_file, OK
if NOT(OK) then RETURN

;--------------------------
;Open the RTS file to read
;--------------------------
;RTI_file = ????
Open_RTS_File, RTS_file, unit, grids, N_GRIDS=n_grids, $
               /READ, RTI_FILE=RTI_file, /ASSOCIATED

;-----------------------------
;Is the value of n in range ?
;-----------------------------
if (n gt (n_grids-1L)) then begin
    nstr = TF_String(n_grids)
    TF_Print,'Sorry:  The specified RTS file contains'
    TF_Print,'only ' + nstr + ' grids.  Please specify'
    TF_Print,'a smaller frame number.'
    TF_Print,' '
    free_lun, unit
    RETURN
endif

;---------------------------
;Extract the specified grid
;and write it to RTG file
;---------------------------
TF_Print,'Creating new RTG file...'
TF_Get_LUN, unit2, RTG_file
openw, unit2, RTG_file    ;** SWAP_ENDIAN=???
writeu, unit2, grids[n]
free_lun, unit2

;-------------------
;Close the RTS file
;-------------------
free_lun, unit
TF_Print,'Finished.'
TF_Print,' '

end;  Extract_RTG_From_RTS
;***************************************************************
pro Extract_Series_From_RTS, RTS_file, series_file, ID 

;---------------------------------
;Does series file exist already ?
;---------------------------------
Check_Overwrite, series_file, OK
if NOT(OK) then RETURN

;--------------------------
;Open the RTS file to read
;--------------------------
;RTI_file = ????
Open_RTS_File, RTS_file, unit, grids, N_GRIDS=n_grids, $
               /READ, NX=nx, NY=ny, RTI_FILE=RTI_file, /ASSOCIATED

;----------------
;Initialize vars
;----------------
series = fltarr(n_grids)

;---------------------------------
;Extract time series for pixel ID
;---------------------------------
for k=0L,(n_grids-1L) do begin
    grid = grids[k]
    series[k] = grid[ID]
endfor

;-------------------
;Close the RTS file
;-------------------
free_lun, unit

;--------------------------------
;Write the series to series file
;--------------------------------
TF_Print,'Creating time series file...'
TF_Get_LUN, unit2, series_file
openw, unit2, series_file 
printf, unit2, series
;-------------------------
;Write one value per line
;-------------------------
;for k=0L,(n_grids – 1L) do begin
;    printf, unit2, series[k]
;endfor

;----------------------
;Close the series file
;----------------------
free_lun, unit2
TF_Print,'Finished.'
TF_Print,' '

end;  Extract_Series_From_RTS
;***************************************************************

