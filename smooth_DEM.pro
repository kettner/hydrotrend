
;*****************************************************************
;   smooth_DEM.pro

;   Copyright (c) 2005-2007, Scott D. Peckham 
;   Created:   May 2004
;   Modified:  Jul-Aug 2005 

;*****************************************************************

;   Read_Profile_Data    (Step 1)
;   Test1
;   SA_Curve
;   Best_SA_Curve_Fit    (Step 2)

;   Make_Smooth_DEM      (Step 3)

;   GUI_Make_Smooth_DEM_event
;   GUI_Make_Smooth_DEM
 
;*****************************************************************

;   The last 3 routines are based on those used to compute flow
;   distances to a set of masked pixels (e.g. pixels with
;   undefined flow codes.  They use a type of raster recursion,
;   but unlike most others, they work upstream, from parents to
;   their kids, instead of downstream from kids to parent pixels.

;*****************************************************************
pro Read_Profile_Data, A, z, ds, DEM_file, $
                       profile_file, area_file, flow_file, $
                       N_HEADER=n_header

;--------------------------------------------------------
;Notes:  This routine gets pixel IDs for a main channel
;        streamline from profile_file and uses them to
;        get elevations, areas and pixel-to-pixel flow
;        lengths along the main channel for use by the
;        Best_SA_Curve_Fit routine.

;        The vector of flow lengths is stored in a
;        COMMON block at the end so that it can be
;        accessed by the SA_Curve procedure.
;--------------------------------------------------------
if NOT(keyword_set(N_HEADER)) then n_header=6

;-------------------------


;Get name of the RTI file
;-------------------------
Get_RTI_Filename, DEM_file, RTI_file, PREFIX=prefix

;--------------------------------
;Option to build other filenames
;--------------------------------
if (n_elements(profile_file) eq 0) then $
    profile_file = prefix + '_prof1.txt'
if (n_elements(area_file) eq 0) then begin
    area_file = prefix + '_area.rtg'
    ;--------------------------------------
    ;These may not be monotonic increasing
    ;--------------------------------------
    ;*** area_file = prefix + '_dinf-area.rtg'
endif
if (n_elements(flow_file) eq 0) then $
    flow_file = prefix + '_flow.rtg'

;----------------------------
;Count lines in profile file
;----------------------------
Count_Lines, n_lines, profile_file, /SILENT
n_lines  = (n_lines - n_header)
;-------------------------------
cols = lonarr(n_lines)
rows = lonarr(n_lines)

;----------------------
;Open file to read IDs
;----------------------
TF_Get_LUN, unit, profile_file
openr, unit, profile_file 

;---------------------------
;Skip over the header lines
;---------------------------
line = ''
for k=0,(n_header-1) do readf,unit,line

;--------------------------------
;Read the column and row vectors
;--------------------------------
dist=0.0  &  elev=0
col=0L    &  row=0L
for k=0L,(n_lines-1L) do begin
    readf, unit, dist, elev, col, row
    cols[k] = col
    rows[k] = row
endfor

;-------------------
;Close profile_file
;-------------------
free_lun, unit

;----------------------------
;Read the DEM and area grids
;----------------------------
Read_Grid, elevs, DEM_file,  /SILENT
Read_Grid, areas, area_file, TYPE='FLOAT', /SILENT

;---------------------------------------
;Compute the along-channel flow lengths
;---------------------------------------
Read_Grid, codes, flow_file, TYPE='BYTE', /SILENT
lens = Flow_Lengths(codes, RTI_file, /METERS, /DOUBLE)

;----------------------------------------
;Construct calendar-style streamline IDs
;----------------------------------------
dims  = size(elevs, /dimensions)
ncols = dims[0]
IDs   = (long(ncols) * rows) + cols

;-----------------------------------
;Get the profile elevations & areas
;-----------------------------------
A  = areas[IDs]   ;[km^2]
z  = elevs[IDs]   ;[meters]
ds = lens[IDs]    ;[meters]

;-----------------------------------
;Reverse the vectors so that values
;start at outlet and work upstream
;-----------------------------------
A  = rotate(A,2)
z  = rotate(z,2)
ds = rotate(ds,2)

;---------------------------------------------
;Place ds in a COMMON block for access
;by the SA_Curve procedure.  See Notes there.
;---------------------------------------------
COMMON Curve_Fit_Data, data
data = {ds:ds}

end;  Read_Profile_Data
;*****************************************************************
pro Test1

;----------------------------------------------------------------
;Notes:  This testing procedure shows that the Best_SA_Curve_Fit
;        routine works, but typically does not give the p-value
;        to high accuracy.
;----------------------------------------------------------------

;----------------------
;Starting on a divide
;and moving downstream
;----------------------
;** x0   = 0.001d  ;(doesn't converge)
;** x0   = 0.01d   ;(doesn’t converge)
;** x0 = 0.1d          ;(converges; large stderr)
x0   = 1d
x    = dindgen(100) + x0           ;(distance [km];  NB! x[0]=x0)
xmin = min(x, max=xmax)
Amax = 625d                    ;[km^2]
ca   = Amax / xmax^2d          ;[unitless]
A    = ca * x^2                ;(area [km^2])

;--------------------------------
;If eps is small, then expect:
;p = (b – 1)/2  or  b = (1 + 2p)
;--------------------------------
;b = -1.0d   ;(p = -1.00)
;b = -0.9d   ;(p = -0.95)
;b = -0.7d   ;(p = -0.85)
;b = -0.5d    ;(p = -0.75)
b = -0.3d   ;(p = -0.65)     ;(closest to actual for KY_Sub?)
;b = -0.1d   ;(p = -0.55)

;-----------------------------------------
;Make sure that z[x0] = z0.  Note that
;if x0=0, then alog(0) will occur in fit
;and fitting procedure will fail.
;-----------------------------------------
z0 = 600d
z  = z0 * (x - x0 + 1d)^b     ;(elevation [meters]

;** eps = 1e-6
;** z   = z0 * (x + eps)^b     ;(elevation [meters])
;** z   = z / (1d + eps)^b     ;(so that z[0] = z0)

;** z   = -1d * 0.01d * alog(x + 1d)   ;(elevation [meters])

;-------------------------------
;Doesn't perform well for these
;-------------------------------
;** z = 600d - (5.9d * x^2d)
;** z = 600d - (5.9d * x^0.5)
;** z = 600d - (5.9d * x)

;-----------------------------------
;Reverse the vectors so that we
;start at outlet and move upstream
;-----------------------------------
x2 = rotate(x,2)
A2 = rotate(A,2)
z2 = rotate(z,2)

;------------------------
;Find the best-fit curve
;------------------------
yfit = Best_SA_Curve_Fit(A2, z2)
print,'yfit[0] = ', yfit[0]

;--------------------------------
;Print expected curve-fit values
;--------------------------------
pe = (b - 1d)/2d
ce = abs((z0 * b) / (ca^pe))   ;(abs since S>0 by convention)
print,'ce = ', ce
print,'pe = ', pe
print,' '

;-------------------------
;Create a plot to compare
;fitted curve to original
;-------------------------
device, decomposed=0
window, 0, xsize=800, ysize=600
loadct, 39, /silent
black = 0
white = 255
red   = 220
plot, x2, z2, color=black, back=white
oplot, x2, yfit, psym=-1, color=red

;** cyan  = 100
;** oplot, x2,
 
end;  Test1
;*****************************************************************

pro SA_Curve, A, params, z, partials

;-----------------------------------------------------------
;Notes:  For use with IDL's CURVEFIT function.

;        CUMULATIVE keyword to TOTAL gives partial sums and
;        returns a vector vs. a scalar.
;-----------------------------------------------------------
;NB!     z0 is the elevation of the parent pixel of the
;        outlet pixel.  It is not the elevation of the
;        outlet pixel and A is max (not zero) at the outlet
;        pixel.
;-----------------------------------------------------------
;NB!     Procedures called by IDL's CURVEFIT must conform
;        to strict rules.  This means that we have no way
;        to pass an additional vector argument like ds.
;        However, we can use a COMMON block, as done here.
;-----------------------------------------------------------
COMMON Curve_Fit_Data, data
ds = data.ds

;----------------------------
;Read ds vector from file ??
;----------------------------
;TF_Get_LUN, unit, 'KY_Sub_prof1-ds.txt'
;openr, unit, 'KY_Sub_prof1-ds.txt' 
;ds = dblarr(727)
;readf, unit, ds
;free_lun, unit

c  = params[0]
p  = params[1]
z0 = params[2]   ;(see Notes above)

z = z0 + (c * total(ds * A^p, /CUMULATIVE, /DOUBLE))

;----------------------------
;Compute partial derivatives
;-------------------------------
;n_params() refers to number of
;arguments to this procedure.
;-------------------------------
if (n_params() ge 4) then begin
    dz_dc  = total(ds * A^p, /CUMULATIVE, /DOUBLE)
    dz_dp  = c * total(ds * alog(A) * A^p, /CUMULATIVE, /DOUBLE)
    nA     = n_elements(A)
    dz_dz0 = dblarr(nA) + 1d
    partials = [[dz_dc], [dz_dp], [dz_dz0]]
endif

end;  SA_Curve
;*****************************************************************
function Best_SA_Curve_Fit, A,z, c,p, params, REPORT=REPORT, $
                 ITMAX=itmax, TOL=tol, WEIGHTS=weights

;------------------------------------------------------------
;Notes:  This function uses IDL's CURVEFIT function and the
;        procedure SA_Curve (above) to find the best-fit
;        parameters for fitting the data vectors A and z.

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
        
;        The array of flags, fitvars, determines which
;        parameters are fixed and which ones to find, we
;        don't need to find z0, but we need to pass it.
;-----------------------------------------------------------
FORWARD_FUNCTION TF_String

nA = n_elements(A)

REPORT = keyword_set(REPORT)
if NOT(keyword_set(ITMAX))   then itmax = 300
if NOT(keyword_set(TOL))     then tol   = 1e-20
if NOT(keyword_set(WEIGHTS)) then begin
    ;----------------------------------------------------
    ;NB! A leading constant seems to make no difference.
    ;----------------------------------------------------

    ;----------------------------------------
    ;Use equal weights; gives smaller stderr
    ;but further from computed p value
    ;----------------------------------------

    ;weights = (dblarr(nA) + 1d)
    ;--------------------------------------------


    ;Poisson statistical weighting, gives a
    ;smaller stderr, but further from computed p
    ;--------------------------------------------
    ;weights = 1d / z
    ;------------------------------------------
    ;Attempt to weight by areas: improved fit.
    ;Recall that we call this with x=A, y=z.
    ;------------------------------------------
    weights = A
    ;weights = A^1.1d
    ;weights = sqrt(A)    ;(good compromise ?)
    ;weights = A^0.75d
    ;----------------------------------------
    ;Combination of above two methods, gives
    ;worst stderr but closest to computed p
    ;----------------------------------------
    ;weights = A / z
endif
w0 = where(weights eq 0, nw0)
if (nw0 ne 0) then weights[w0]=1d

;----------------------------------------
;Points used to generate initial guesses
;----------------------------------------
A0=A[0]  &  A1=A[1]  &  A2=A[nA-1]
z0=z[0]  &  z1=z[1]  &  z2=z[nA-1]

;-------------------------------------------
;Need initial guesses with good convergence
;properties; not necessarily close to value
;-------------------------------------------
COMMON Curve_Fit_Data, data
ds = data.ds
;*** ds = 1d
;---------------
p0 = -0.9d
;** p0 = -0.4d
;** p0 = -0.9d
;** p0 = -0.01d
;** p0 = -1e-8
;** p0 = 1e-6
;** p0 = -0.5d
;** p0 = -0.99d

c0 = (z2 - z0) / total(ds * A^p0, /DOUBLE)

;-----------------------------------------
;Use CURVEFIT to find best-fit parameters
;-----------------------------------------
params  = [c0, p0, z0]
fitvars = [1, 1, 0]
zfit    = curvefit(A, z, weights, params, sigma, /DOUBLE, $ 
                   FUNCTION_NAME='SA_Curve', $ 
                   TOL=tol, ITMAX=itmax, YERROR=stderr, $
                   FITA=fitvars, STATUS=status, ITER=iter)

c = params[0]  ;(these are passed back out)
p = params[1]

REPORT = 1b
if NOT(REPORT) then RETURN, yfit
print,'-----------------------------------'
print,'Best curve fit to profile'
print,'-----------------------------------'
print,'z(A)   = z0 + (c * total(ds * A^p))'
print,'c0     = ' + TF_String(c0)
print,'p0     = ' + TF_String(p0)
print,'z0     = ' + TF_String(z0)
print,'---------------------------------'
print,'c      = ' + TF_String(params[0])
print,'p      = ' + TF_String(params[1])
;** print,'z0     = ' + TF_String(params[2])
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
RETURN, zfit
end;  Best_SA_Curve_Fit
;*****************************************************************
pro Make_Smooth_DEM, new_DEM_file, new_RTI_file, new_raw_file, $
                     new_flow_file, new_slope_file, c1,p1,Ah, $
                     DEM_file, flow_file, area_file, $
                     MSG_BOX_ID=MSG_BOX_ID

;--------------------------------------------------------------
;NOTES:  This routine uses a slope-area relationship, an area
;        grid and a D8 flow grid to create a new, smoother DEM
;        from an old one.  The reason for wanting to do some-
;        thing like this is that slopes in channels are very
;        poorly resolved by local methods, even though slopes
;        on hillslopes can be computed reasonably well.

;        It operates on a principle of "raster recursion" and
;        should be adaptable to the computation of other
;        recursively-defined quantities.  That is, it:

;        (1) initializes the raster file, and
;        (2) makes several passes through the file (line by
;            line), looking for pixels whose _parent_ has a
;            known value, and
;        (3) assigns these pixels a value which is determined
;            from the value of the parent.
;--------------------------------------------------------------

;--------------------------
;Get name of the RTI file
;and then read in the info
;--------------------------
Get_RTI_Filename, DEM_file, RTI_file, PREFIX=prefix
Read_RTI_File, RTI_File, info
nx = info.ncols
ny = info.nrows
byte_order = info.byte_order

;------------------------------------
;Read the DEM, flow grid & area grid
;------------------------------------
Read_Grid, DEM,   DEM_file, /SILENT
Read_Grid, codes, flow_file, TYPE='BYTE', /SILENT
Read_Grid, areas, area_file, TYPE='FLOAT', /SILENT

;---------------------------------
;Get the grid of parent pixel IDs
;---------------------------------
pID_grid = Parent_IDs(codes)

;-----------------------------------------------------
;Find the pixels that flow to a nodata or edge pixel;
;the parents of these pixels have a flow code of 0.
;-----------------------------------------------------
parent_codes = codes[pID_grid]
w = where(parent_codes eq 0, nw)

;-----------
;OLD METHOD
;------------------------------------------------
;Before 2/14/07; can't handle nodata pixels)
;------------------------------------------------
;** w = where(codes eq 0, nw)
;** w = where((codes eq 0) AND (DEM gt nodata), nw)

;------------------------------------
;Are there any pixels to work with ?
;------------------------------------
if (nw eq 0) then begin
    print,'ERROR: '
    print,'No pixels to initialize recursion.'
    print,' '
    RETURN
endif

;-----------------------------------------------
;Initialize values in new DEM to be same as in
;old DEM for pixels whose parent flow code = 0
;and nodata value otherwise
;----------------------------------------------- 
nodata = -9999.0
new_DEM = fltarr(nx, ny) + nodata
new_DEM[w] = DEM[w]

;-----------------------------------------
;Get grid of pixel-to-parent flow lengths
;-----------------------------------------
ds = Flow_Lengths(codes, RTI_file, /METERS, /DOUBLE)   ;[meters]

NUMREPS = 0L
DONE    = 0b

;----------------------------------------
;Recursively assign new elevation values
;----------------------------------------
repeat begin
    ;----------------------------
    ;Write status to message box
    ;----------------------------
    if (keyword_set(MSG_BOX_ID)) then begin
        mstr = 'Iteration = ' + TF_String(NUMREPS+1)
        widget_control, MSG_BOX_ID, set_value=mstr
    endif

    STILLACTIVE = 0b
    UNKNOWN = where((new_DEM eq nodata), NUM_UNKNOWN)
    NUMREPS = (NUMREPS + 1L)

    if (NUM_UNKNOWN ne 0) then begin
        ;--------------------------------
        ;Get elevations of parent pixels
        ;--------------------------------
        dvals = codes[UNKNOWN]
        pIDs  = pID_grid[UNKNOWN]
        pvals = new_DEM[pIDs]

        ;--------------------------
        ;If parent value is known,
        ;then assign value to kids
        ;--------------------------
        wp = where(pvals ne nodata, NUM_ASSIGNED)
        if (NUM_ASSIGNED ne 0) then begin
            ;------------------------------------
            ;Get upstream areas of parent's kids
            ;------------------------------------
            avals = areas[UNKNOWN[wp]]

            ;------------------------------
            ;Compute new slopes from areas
            ;--------------------------------
            ;S(0)=0  and  S(Inf)=0
            ;Smax = (1-exp(-1)) * Ah^p1 * c1
            ;--------------------------------
            ;** svals = c1 * (avals^p1) * (1.0 - exp(-avals/Ah))
            svals = c1 * (avals^p1)
            
            ;---------------------------------
            ;Try to capture convex hillslopes
            ;with a second power-law curve.
            ;---------------------------------
            ;Can force continuity, but can't
            ;match derivatives or get p2=p1.
            ;This can be seen with a figure.
            ;Expect 0 < p2 < 1, so we'll just
            ;fix p2 and compute c2 from cont.
            ;---------------------------------
            ;ww = where(avals lt Ah, nww)
            ;if (nww ne 0) then begin
            ;    Smax = c1 * (Ah^p1)
                ;** p2 = 0.1d
                ;** p2 = 0.5d
            ;    p2 = 0.8d
                ;** p2 = 1d
                ;** p2 = 2d
                ;** p2 = 4d
            ;    c2   = Smax / Ah^p2
            ;    svals[ww] = c2 * (avals[ww]^p2)
            ;endif

            ;----------------------------
            ;Update the elevation values
            ;----------------------------
            dH = svals * ds[dvals[wp]]    ;[meters]
            new_DEM[UNKNOWN[wp]] = (pvals[wp] + dH)
            STILLACTIVE = 1b
        endif

        ;----------------------
        ;Are we finished yet ?
        ;----------------------
        DONE = (NUM_ASSIGNED eq NUM_UNKNOWN)
    endif

endrep until (DONE OR NOT(STILLACTIVE))

;---------------------
;Save new DEM in file
;---------------------
TF_Get_LUN, unit, new_DEM_file
openw, unit, new_DEM_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
writeu, unit, float(new_DEM)
free_lun, unit
emin2 = min(new_DEM, max=emax2, /NAN)

;------------------------------------
;Create and save new slope grid file
;---------------------------------------
;Subpixel sinuosity, if any, is applied
;later in Route_Flow.  Both ds and the
;pID_grid were computed above.
;---------------------------------------
slopes = (new_DEM - new_DEM[pID_grid]) / ds
TF_Get_LUN, unit, new_slope_file
openw, unit, new_slope_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
writeu, unit, float(slopes)
free_lun, unit



;----------------------------
;Create RTI file for new DEM
;----------------------------
info.data_type = 'FLOAT'
Write_RTI_File, new_RTI_file, info, /SILENT

;------------------------------------
;Save FLOAT version of original DEM
;as the rawDEM for the new DEM
;------------------------------------
TF_Get_LUN, unit, new_raw_file
openw, unit, new_raw_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
writeu, unit, float(DEM)
free_lun, unit
emin = min(DEM, max=emax, /NAN)

;----------------------------------
;Save D8 flow grid of original DEM
;as the flow grid of the new DEM
;---------------------------------------
;Check that flow grid hasn't changed ??      ;**********************
;---------------------------------------
TF_Get_LUN, unit, new_flow_file
openw, unit, new_flow_file, $
       SWAP_ENDIAN=Not_Same_Byte_Order(byte_order)
writeu, unit, codes
free_lun, unit

;--------------------
;Print final message
;--------------------
msg = [ $
'Finished with new DEM. ', ' ', $
'Number of iterations = ' + TF_String(NUMREPS), $
' ',$
'Min/Max of orig. DEM = ' + TF_String(emin) + ', ' + TF_String(emax), $
'Min/Max of new  DEM  = ' + TF_String(emin2) + ', '+ TF_String(emax2), $
' ',$
'(c, p) = ' + TF_String(c1) + ', ' + TF_String(p1), ' ']
result = GUI_Message(msg, /INFO)

end;  Make_Smooth_DEM
;****************************************************************
pro GUI_Make_Smooth_DEM_event, event

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
;---------------------------------
;Read names of the new grid files
;---------------------------------
Read_Text_Box, state.new_DEM_file_ID, new_DEM_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, new_DEM_file, OK
if NOT(OK) then RETURN
;---------------------------------
Read_Text_Box, state.new_RTI_file_ID, new_RTI_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, new_RTI_file, OK
if NOT(OK) then RETURN
;---------------------------------
Read_Text_Box, state.new_raw_file_ID, new_raw_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, new_raw_file, OK
if NOT(OK) then RETURN
;---------------------------------
Read_Text_Box, state.new_flow_file_ID, new_flow_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, new_flow_file, OK
if NOT(OK) then RETURN
;---------------------------------
Read_Text_Box, state.new_slope_file_ID, new_slope_file, OK, /TEXT
if NOT(OK) then RETURN
Check_Overwrite, new_slope_file, OK
if NOT(OK) then RETURN

;-----------------------------------
;Read names of the input grid files
;-----------------------------------
Read_Text_Box, state.DEM_file_ID, DEM_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(DEM_file)
if NOT(OK) then RETURN


;-----------------------
Read_Text_Box, state.flow_file_ID, flow_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(flow_file)
if NOT(OK) then RETURN
;-----------------------
Read_Text_Box, state.area_file_ID, area_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(area_file)
if NOT(OK) then RETURN

;--------------------------------


;Read name of input profile file
;--------------------------------
Read_Text_Box, state.profile_file_ID, profile_file, OK, /TEXT
if NOT(OK) then RETURN else OK=File_Found(profile_file)
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
Read_Profile_Data, A, z, ds, DEM_file, $
                   profile_file, area_file, flow_file, $
                   N_HEADER=n_header  ;******************
zfit = Best_SA_Curve_Fit(A,z, c1,p1)
Ah = 0.0  ;(not used now)
Make_Smooth_DEM, new_DEM_file, new_RTI_file, new_raw_file, $
                 new_flow_file, new_slope_file, c1,p1,Ah, $
                 DEM_file, flow_file, area_file, $
                 MSG_BOX_ID=state.msg_box_ID
widget_control, event.ID, sensitive=1    ;(enable button)

;-------------------------
;Show a "finished" dialog
;-------------------------
;msg = ['Finished creating new grids.', ' ']
;result = GUI_Message(msg, /INFO, TITLE="Finished")
;*** Close_Dialog, event.top 
end

;***********
'HELP' : $
;***********
Show_HTML_Help, 'smooth_DEM.htm'

;************
'CLOSE' : $
;************
Close_Dialog, event.top

ELSE : dum=0
endcase

if (uvalue ne 'CLOSE') AND $
   (uvalue ne 'START') then $
    widget_control, event.top, set_uvalue=state 

end;  GUI_Make_Smooth_DEM_event
;****************************************************************
pro GUI_Make_Smooth_DEM, leader

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
DEM_file     = prefix + '_DEM.rtg'
flow_file    = prefix + '_flow.rtg'
area_file    = prefix + '_area.rtg'
profile_file = prefix + '_prof1.txt'
n_header_str = ' 6 '
;-----------------------------------------


new_prefix     = prefix + '2'
new_DEM_file   = new_prefix + '_DEM.rtg'
new_RTI_file   = new_prefix + '.rti'
new_raw_file   = new_prefix + '_rawDEM.rtg'
new_flow_file  = new_prefix + '_flow.rtg'
new_slope_file = new_prefix + '_slope.rtg'



;------------------------------------
;Structure to store selected options
;------------------------------------
state = { $
leader_ID:leader, msg_box_ID:0L, $
new_DEM_file_ID:0L, new_RTI_file_ID:0L, new_raw_file_ID:0L, $
new_flow_file_ID:0L, new_slope_file_ID:0L, $


DEM_file_ID:0L, flow_file_ID:0L, area_file_ID:0L, $
profile_file_ID:0L, n_header_ID:0L}

ngap = 6
XS   = 20

;-----------------
;Main base widget
;-----------------
Create_TLB, MB, TITLE='DEM Profile Smoothing Tool', $
            /COLUMN, LEADER=leader
B1 = widget_base(MB, /COLUMN, /FRAME)
B2 = widget_base(MB, /COLUMN, /FRAME)
B3 = widget_base(MB, /ROW, /FRAME)

;----------------------
;Get the new filenames
;----------------------
ND = widget_base(B1, /ROW, SPACE=ngap)
  ND1 = widget_label(ND, VALUE='New DEM filename: ')
  ND2 = widget_text(ND, VALUE=new_DEM_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.new_DEM_file_ID = ND2
;------------------------------------------------------------

NI = widget_base(B1, /ROW, SPACE=ngap)
  NI1 = widget_label(NI, VALUE='New RTI file: ')
  NI2 = widget_text(NI, VALUE=new_RTI_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.new_RTI_file_ID = NI2
;------------------------------------------------------------
NR = widget_base(B1, /ROW, SPACE=ngap)
  NR1 = widget_label(NR, VALUE='New raw DEM file: ')
  NR2 = widget_text(NR, VALUE=new_raw_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.new_raw_file_ID = NR2
;------------------------------------------------------------
NF = widget_base(B1, /ROW, SPACE=ngap)
  NF1 = widget_label(NF, VALUE='New D8 flow grid file: ')
  NF2 = widget_text(NF, VALUE=new_flow_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.new_flow_file_ID = NF2
;------------------------------------------------------------
NS = widget_base(B1, /ROW, SPACE=ngap)
  NS1 = widget_label(NS, VALUE='New slope grid file: ')
  NS2 = widget_text(NS, VALUE=new_slope_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.new_slope_file_ID = NS2

;-----------------------
;Get input DEM filename 
;-----------------------
EG = widget_base(B2, /ROW, SPACE=ngap)
  EG1 = widget_label(EG, VALUE='Input DEM filename: ')
  EG2 = widget_text(EG, VALUE=DEM_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  EG3 = widget_label(EG, VALUE='[meters] ')
  state.DEM_file_ID = EG2

;-----------------------
;Get flow grid filename 
;-----------------------
FG = widget_base(B2, /ROW, SPACE=ngap)
  FG1 = widget_label(FG, VALUE='D8 flow grid filename: ')
  FG2 = widget_text(FG, VALUE=flow_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.flow_file_ID = FG2

;-----------------------
;Get area grid filename 
;-----------------------
AG = widget_base(B2, /ROW, SPACE=ngap)
  AG1 = widget_label(AG, VALUE='D8 area grid filename: ')
  AG2 = widget_text(AG, VALUE=area_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  AG3 = widget_label(AG, VALUE='[km^2] ')
  state.area_file_ID = AG2

;--------------------------
;Get profile file filename 
;--------------------------
PF = widget_base(B2, /ROW, SPACE=ngap)
  PF1 = widget_label(PF, VALUE='Channel profile filename: ')
  PF2 = widget_text(PF, VALUE=profile_file, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.profile_file_ID = PF2

;---------------------------------
;Get number of header lines, etc.
;---------------------------------
NH = widget_base(B2, /ROW, SPACE=ngap)
  NH1 = widget_label(NH, VALUE='Number of header lines: ')
  NH2 = widget_text(NH, VALUE=n_header_str, UVALUE='NONE', $
                    /EDITABLE, XSIZE=XS)
  state.n_header_ID = NH2

;------------------
;Align the widgets
;------------------
Align_Text_Boxes, [ND1, NI1, NR1, NF1, NS1, EG1, FG1, AG1, PF1, NH1]

;------------------
;Bottom button bar
;------------------
CW_Button_Bar, B3, /START, /HELP, /CLOSE 


;---------------------
;A status message box
;---------------------
MS = widget_base(B3, /ROW, SPACE=ngap)
  MS1 = widget_label(MS, VALUE='Status: ')
  MS2 = widget_text(MS, VALUE='Ready.', XSIZE=14)
  state.msg_box_ID = MS2

;------------------------------------
;Realize widgets and wait for events 
;------------------------------------
XOFF = 480
Realize_TLB, MB, state, 'GUI_Make_Smooth_DEM', XOFF=XOFF

end;  GUI_Make_Smooth_DEM
;****************************************************************

