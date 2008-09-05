
;*****************************************************************
;    pixel_size.pro

;    Copyright (c) 2002-2005, Scott D. Peckham 
;    Created:   Jun 2002  (Similar to RiverTools method)

;*****************************************************************

;    Meters_Per_Degree_Lon,
;    Meters_Per_Degree_Lat,
;    Get_Pixel_Sizes

;*****************************************************************
function Meters_Per_Degree_Lon, lat_deg, a_radius, b_radius, $
                                mean_elev 

;----------------------------------------------------------
;NOTES:  This formula comes from both:
;        Snyder, J.P. (1987) Map projections - A working
;        manual, USGS Prof. Paper 1395, p. 25.
;        and

;        Ewing, C.E., and Mitchell, M.M. (1970)
;        Introduction to Geodesy, American Elsevier
;        Publishing Company, New York, New York, pp. 8-26

;        The above references give a formula for the
;        radius of curvature of a parallel of latitude,
;        Rp, on page 19.  Arclength between two longitudes
;        lam1 and lam2 is then:  Rp(lat) * (lam2 - lam1).
;        Note that the argument, lat_deg, is the geodetic
;        latitude in decimal degrees, and not the
;        geocentric latitude; otherwise the square root
;        term wouldn't be needed.  Every parallel of
;        latitude is a circle.

;        Based on the diagram on page 14, it can be seen
;        that if N is extended by the mean_elevation, then
;        we must add (mean_elev * cos(lat)) to Rp.

;        Checked with an example from an ESRI manual.
;        For Clarke 1866 ellipsoid, one degree of latitude
;        at Equator is 111.321 km, and at 60 degrees north
;        it is 55.802 km.

;        a_radius = equatorial radius of ellipsoid (meters)
;        b_radius = polar radius of ellipsoid (meters)

;        For WGS_1984 ellipsoid:
;            a_radius = 6378.1370, b_radius = 6356.7523
;        For CLARKE_1866 ellipsoid:
;            a_radius = 6378.2064, b_radius = 6356.5838
;-----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,-1

;------------------------------------
;Defaults are for WGS_1984 ellipsoid
;------------------------------------
if (n_elements(a_radius)  eq 0) then a_radius=6378137.0d  ;[meters]
if (n_elements(b_radius)  eq 0) then b_radius=6356752.3d  ;[meters] 
if (n_elements(mean_elev) eq 0) then mean_elev=0d

;-----------------------------
;Compute flattening ratio, f,
;and eccentricity, e
;-----------------------------
f = (a_radius - b_radius) / a_radius
e = sqrt((2d*f) - f^2d)

;------------------
;Compute the value
;------------------
lat_rad = (lat_deg * !DPI / 180d)
p1 = mean_elev * cos(lat_rad)
p2 = a_radius * cos(lat_rad) / sqrt(1d - (e * sin(lat_rad))^2d)

RETURN, (!DPI / 180d) * (p1 + p2)

END;    Meters_Per_Degree_Lon
;*****************************************************************
function Meters_Per_Degree_Lat, lat_deg, a_radius, b_radius, $
                                mean_elev

;----------------------------------------------------------
;NOTES:  This formula comes from both:
;        Snyder, J.P. (1987) Map projections - A working
;        manual, USGS Prof. Paper 1395, p. 25.
;        and

;        Ewing, C.E., and Mitchell, M.M. (1970)
;        Introduction to Geodesy, American Elsevier
;        Publishing Company, New York, New York, pp. 8-26.

;        The above references give a formula for the
;        radius of curvature of a meridian of longitude,
;        R, on page 19.  Arclength along a meridian is
;        given by the integral of R(phi) * d_phi between
;        two geodetic latitudes, phi1 and phi2.
;        Note that the argument, lat_deg, is the geodetic
;        latitude in decimal degrees, and not the
;        geocentric latitude. Every meridian of longitude
;        is the same ellipse.

;        Based on the diagram on page 18, it can be seen
;        that if the mean elevation of a region is nonzero,
;        then R should be replaced by (R + mean_elev).
;----------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN,-1

;------------------------------------
;Defaults are for WGS_1984 ellipsoid
;------------------------------------
if (n_elements(a_radius)  eq 0) then a_radius=6378137.0d  ;[meters]
if (n_elements(b_radius)  eq 0) then b_radius=6356752.3d  ;[meters] 
if (n_elements(mean_elev) eq 0) then mean_elev=0d

;-----------------------------
;Compute flattening ratio, f,
;and eccentricity, e
;-----------------------------
f = (a_radius - b_radius) / a_radius
e = sqrt((2d*f) - f^2d)

;------------------
;Compute the value
;------------------
lat_rad = (lat_deg * !DPI / 180d)
p = a_radius *(1d - e^2d) / (1d - (e * sin(lat_rad))^2d)^1.5d

RETURN, (!DPI / 180d) * (mean_elev + p)

END;  Meters_Per_Degree_Lat
;*****************************************************************
pro Get_Pixel_Sizes, dx,dy,dd,da, RTI_file, REPORT=REPORT, $
                     METERS=METERS

;---------------------------------------------------------
;NOTES:  This routine returns the xsize, ysize, diagonal
;        size and area of pixels, based on the pixel
;        geometry associated with the current DEM, in
;        kilometers.  RTI_file contains georef info.

;        For fixed-angle pixels, the lat/lon-dependence
;        of both xsizes and ysizes (on an ellipsoid)
;        is taken into account.

;        The calculations are done efficiently using
;        IDL array operations, so dx, dy, dd, and da
;        are returned as 1D arrays of length NROWS.

;        The vars dx,dy,dd,da are returned as DOUBLES.
;        This is necessary.

;        Note that:
;            (minlat, maxlat-yresdeg) -> y=(ny-1, 0) and
;            (minlon, maxlon-xresdeg) -> x=(nx-1, 0).
;        This is related to the fact that !order=1 in RT.
;        Lats are for bottom edge of pixel.
;        Note that 3600 arcsecs = 1 degree.

;        NOTE that dd=sqrt( dx^2 + dy^2) and da=(dx * dy)
;        are very good approximations as long as dx and
;        dy are not too large.
;---------------------------------------------------------

;-----------
;Error trap
;-----------
No_Catch, status
Check_Error_Status, status, OK
if NOT(OK) then RETURN

;-----------------------
;Kilometers or Meters ?
;-----------------------
if (keyword_set(METERS)) then ufactor=1d else ufactor=1000d

;------------------------------
;Get georef info from RTI file
;------------------------------
Read_RTI_File, RTI_file, info

if (info.pixel_geom eq 0) then begin
    ;-------------
    ;Compute lats
    ;-------------
    DTORD   = (!DPI / 180d)
    ycoords = indgen(info.nrows)
    yresdeg = (info.yres / 3600d)    ;(arcsecs -> degrees)
    lats    = (info.y_north_edge - (yresdeg * (ycoords + 1d)))

    ;--------------------------------------
    ;Compute pixel sizes using lats & lons
    ;--------------------------------------
    ;NB!  dy becomes a vector !!
    ;     Also for (pixel_geom eq 1) below !
    ;------------------------------------------
    MPD_LON = Meters_Per_Degree_Lon(lats)         ;(vector)
    MPD_LAT = Meters_Per_Degree_Lat(lats)         ;(vector)
    dx = (info.xres / 3600d * MPD_LON / ufactor)  ;(vector)
    dy = (info.yres / 3600d * MPD_LAT / ufactor)  ;(vector)
    dd = sqrt( dx^2 + dy^2)
    da = (dx * dy)

endif else begin
    dx = (info.xres / ufactor)   ;(meters or km)
    dy = (info.yres / ufactor)
    dd = sqrt( dx^2 + dy^2)
    da = (dx * dy)
    ;----------------------------
    dx = dblarr(info.nrows) + dx
    dy = dblarr(info.nrows) + dy
    dd = dblarr(info.nrows) + dd
    da = dblarr(info.nrows) + da
endelse

;----------------
;Optional report
;----------------
if keyword_set(REPORT) then begin
   if (info.pixel_geom eq 0) then begin
        RADEG = (180d / !DPI)
        ;---------------------
        print,'Pixel geometry = Fixed-angle '
        print,' '
        print,'Actual south edge lat   = ' + $
                  string(info.y_south_edge)
        print,'Computed south edge lat = ' + $
                  string(LATS(info.nrows-1) * RADEG)
        print,'Actual north edge lat   = ' + $
                  string(info.y_north_edge)
        print,'Computed north edge lat = ' + $
                  string((LATS(0) + yresdeg) * RADEG)
   endif else begin
        print,'Pixel geometry = Fixed-length '
        print,' '
        print,'Actual south edge y   = ' + $
                  string(info.y_south_edge)
        print,'Actual north edge y   = ' + $
                  string(info.y_north_edge)
   endelse
   print,' '
   print,'Min(dx), Max(dx) = ' + string(min(dx)) + string(max(dx))
   print,'Min(dy), Max(dy) = ' + string(min(dy)) + string(max(dy))
   print,'Min(dd), Max(dd) = ' + string(min(dd)) + string(max(dd))
   print,'Min(da), Max(da) = ' + string(min(da)) + string(max(da))
   print,' '
endif

END;  Get_Pixel_Sizes
;*****************************************************************

