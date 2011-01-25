#if !defined( HYDROTIMESER_H )
#define HYDROTIMESER_H

/*-------------------------------------------------------------------------------------------
 *	hydrotimeser.h
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Contains arrays of daily values.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 * 
 *-------------------------------------------------------------------------------------------*/

/*  Maxday = 365+50 = 415; amazon=3,900km at 1 m/s => 45 days */

#define  maxday  415
#define  daysiy  365
#define  wrapday  50            /* Same as maxshoulder in HydroClimate.h */

extern int FLAindex[daysiy];
extern long *daysievent;

/*-------------------------------
 *  Daily suspended and bedload
 *-------------------------------*/
extern double Cs[daysiy], **Csoutlet, Qb[daysiy], **Qboutlet, Qs[daysiy],
  Qspsi[daysiy], Qsglacier[daysiy], **Qsoutlet;

/*---------------------------------------------------------
 *  Daily Temperature, Precipitation and Snow time series
 *---------------------------------------------------------*/
extern double rainarea[daysiy], Ecanopy[daysiy];
extern double Pdaily[daysiy], Tdaily[daysiy], **Snowelevday;

/*--------------------------------------------------------------------
 *  Arrays for each type of daily discharge (rain,snow,ice,total,GW)
 *--------------------------------------------------------------------*/
extern double Qrain[maxday], Qice[maxday], Qnival[maxday], Qsumtot[maxday], **Qsum,
  Qss[maxday];

/*--------------------------------------------------
 *  Arrays for carryover from one year to the next
 *--------------------------------------------------*/
extern double Qrainwrap[wrapday], Qicewrap[wrapday], Qnivalwrap[wrapday];
extern double Qsswrap[wrapday], *Snowcarry;

/*-------------------------------------------
 *  Arrays for the groundwater storage pool
 *-------------------------------------------*/
extern double gwstore[daysiy], Qicetogw[daysiy], Qnivaltogw[daysiy];
extern double Egw[daysiy], Qexceedgw[daysiy];

/*
 *
 * Variable		Def.Location	Type	Units	Usage
 * --------		------------	----	-----	-----
 *
 * Cs[]		HydroTimeser.h	double	kg/m^3	daily suspended load concentration
 * Ecanopy		HydroTimeser.h	double	m/day	canopy evaporation
 * Egw[]		HydroTimeser.h	double	m/day	groundwater evaporation
 * FLAindex[]	HydroTimeser.h	int	-	Elevation (elevbins) index of the Freezing Line Altitude
 * Pdaily[]		HydroTimeser.h	double	m/day	Actual daily rainfall at the river mouth
 * Qb[]		HydroTimeser.h	double	kg/s	daily bedload flux
 * Qexceedgw[]	HydroTimeser.h	double	m^3/s	daily discharge from Ice and Nival which exceeds GW capacity
 * Qice[]		HydroTimeser.h	double	m^3/s	daily glacially derived discharge
 * Qicetogw[]	HydroTimeser.h	double	m^3/s	daily glacially derived groundwater discharge
 * Qicewrap[]	HydroTimeser.h	double	m^3/s	daily glacial discharge overflowing to the next year
 * Qnival[]		HydroTimeser.h	double	m^3/s	daily snow derived discharge
 * Qnivaltogw[]	HydroTimeser.h	double	m^3/s	daily snow derived groundwater discharge
 * Qnivalwrap[]	HydroTimeser.h	double	m^3/s	daily snow discharge overflowing to the next year
 * Qrain[]		HydroTimeser.h	double	m^3/s	daily rain derived discharge
 * Qrainwrap[]	HydroTimeser.h	double	m^3/s	daily rain discharge overflowing to the next year
 * Qs[]		HydroTimeser.h	double	kg/s	daily suspended load flux
 * Qss[]		HydroTimeser.h	double	m^3/s	subsurface storm flow to river
 * Qsswrap[]	HydroTimeser.h	double	m^3/s	daily GW overflowing to the next year
 * Qsum[]		HydroTimeser.h	double	m^3/s	summed daily discharge
 * Snowcarry[]	HydroTimeser.h	double	m	snow carried over from previous year
 * Snowelevday[]	HydroTimeser.h	double	m	snow cover (m water equiv) for that elev/day
 * Tdaily[]		HydroTimeser.h	double	degC	Actual daily Temperature at the river mouth
 * daysiy		HydroTimeser.h	define	-	number of days/year
 * gwstore[]	HydroTimeser.h	double	m^3	actual groundwater storage on a given day
 * maxday						allow for overflow to next year
 * maxday		HydroTimeser.h	define	-	number of days in time series arrays
 * rainarea[]	HydroTimeser.h	double	m^2	basin area over which rain falls
 * wrapday		HydroClimate.h	define	-	maximum number of shoulder days
 *
 */

#endif

