/*-------------------------------------------------------------------------------------------
 *	HydroSetParams.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	In hydroreadinput.c, units are converted as follows:
 *  Multiply totalarea by 1e6.  (km^2 -> m^2)
 *  Multiply basinlength by 1000.  (km -> m)
 *  Divide Ko by 1000.  (mm/day -> m/day)
 *  Divide Lapse rate by 1000.  (mm/day -> m/day)
 *  Some others are done by this routine.
 * 
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydroparams.h"
#include "hydroclimate.h"

/*---------------------------
 *  Start of HydroSetParams
 *---------------------------*/
void hydrosetparams() {

/*---------------------------------------
 *  Hardwired Parameters for all Epochs
 *---------------------------------------*/
    rhowater     = 1000.0;
    rhosed       = 2670.0;
    alphac       = 0.98;    /* saturation excess coeff */
    betac        = 1.00;    /* saturation excess exponent */
    alphag       = -0.0001;    /* groundwater precip offset (m/day) */
	betag        = 0.85;    /* groundwater precip slope */

    alphagwe     = 0.0020;     /* groundwater evap coeff (m/day)*/
    betagwe      = 1.0;     /* groundwater evap exponent */
    pmax         = 0.400;   /* precip need to reach max cond. (m/day) */
    Meltrate     = 0.003;   /* (m/degC) */
    percentgw    = 0.15;    /* percent of nival&ice as groundwater */
    pcr          = 0.010;     /* crit. precip for infilt. excess (m/day) */
	bethaexpo		 = 1.38;	/* volume-surface area exponent glaciers */
	bethaglacier	 = 31.11716; /* volume-surface area multiplier glaciers */
	hyperpycnalvalue = 40.0;	/* (in kg/m3) rough value; above this value and river will go hyperpycnal */
return;
}  /* end of HydroSetParams */
