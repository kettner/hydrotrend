/*-------------------------------------------------------------------------------------------
 *	hydroclimate.h
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Contains declarations for monthly and annual climate and discharge variables.
 *  and discharge values and total annual values.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 * ***baseflow       	HydroClimate.h	dbl	m^3/s	Lower limit of discharge for the river, at least this amount will always occur
 * dayendm[12]        	HydroClimate.h	int	-	ending year day of each month
 * dayends[4]        	HydroClimate.h	int	-	ending year day of each season
 * daysim[12]        	HydroClimate.h	int	-	number of days in each month
 * daysis[4]        	HydroClimate.h	int	-	number of days in each season
 * daystrm[12]        	HydroClimate.h	int	-	starting year day of each month
 * daystrs[4]        	HydroClimate.h	int	-	starting year day of each season
 * dmatrix()        	HydroClimate.h	dbl	-	allocate a 2D double matrix, from Numerical Recipes
 * Eiceannual        	HydroClimate.h	dbl	m^3/a	Total annual evaporation from nival and ice
 * Enivalannual        	HydroClimate.h	dbl	m^3/a	Total annual evaporation from nival and ice
 * ELAchange[]        	HydroClimate.h	dbl	degC/a  ELA change/year for an epoch
 * ELAstart[]        	HydroClimate.h	dbl	m	Starting Glacial Equilibrium Line Altitude (ELA) for an epoch
 * Ewetannual        	HydroClimate.h	dbl	m^3	Total annual evaporation from rain and groundwater
 * free_dmatrix        	HydroClimate.h	void	-	free a 2D double matrix, from Numerical Recipes
 * glacierelev        	HydroClimate.h	dbl	m	minimum elevation of the glacier
 * glacierarea        	HydroClimate.h	dbl	m^2	modeled glacier area for each year
 * lapserate[]        	HydroClimate.h	dbl	degC/km	Temperature lapse rate for an epoch
 * maxepoch		HydroClimate.h	define	-	maximum number of epochs
 * maxshoulder        	HydroClimate.h	define	-	maximum number of shoulder days
 * Tmaxstd		HydroClimate.h	define	-	maximum STD for annual T climate values
 * Pmaxstd		HydroClimate.h	define	-	maximum STD for annual P climate values
 * MEtotal		HydroClimate.h	dbl	m^3/a	total evaporation
 * Minput		HydroClimate.h	dbl	m^3	total mass input
 * Moutput		HydroClimate.h	double	m^3	total mass output
 * MQnext		HydroClimate.h	double	m^3/a	carryover to the next year
 * MQprevious        	HydroClimate.h	double	m^3/a	carryover from the previous year
 * nelevbins        	HydroClimate.h	int	#	number of calculated elevation bins
 * nmonth		HydroClimate.h	define	-	number of months/year
 * nrerror()        	HydroClimate.h	void	-	standard error handler, from Numerical Recipes
 * Pannual		HydroClimate.h	double	m/y	Actual total rainfall for a year
 * Pchange[]        	HydroClimate.h	double	m/y	Precipitation change/year for an epoch
 * Pmassbal[]        	HydroClimate.h	double	m	Rain mass balance Coefficient
 * Pexponent[]        	HydroClimate.h	double	m	Rainfall distribution exponent
 * Pmonth[]		HydroClimate.h	double	m	Actual monthly total rainfall
 * Pnominal[][]        	HydroClimate.h	double	m	Nominal monthly rainfall
 * Pnomstd[][]        	HydroClimate.h	double	m	STD of the monthly total rainfall
 * Prange[]		HydroClimate.h	double	-	Rainfall distribution range
 * Pstart[]		HydroClimate.h	double	m/a	Total annual Precipitation start value for an epoch
 * Pstd[]		HydroClimate.h	double	m	Standard Deviation of the annual Precipitation
 * Qbedannual        	HydroClimate.h	double	kg/a	Total bedload/year
 * Qgrandtotal        	HydroClimate.h	double	m^3	Total Runoff in the model run
 * Qpeak		HydroClimate.h	double	m^3/s	Peak flood occuring in a year
 * Qsgrandtotal[ep]   	HydroClimate.h	double	kg	Total suspended load/ep run
 * Qsannual		HydroClimate.h	double	kg/a	Total suspended load/year
 * Qtotal		HydroClimate.h	double	m^3/a	Total Runoff in a year
 * shoulderright[]      HydroClimate.h	double	%	shoulder array for days following an event
 * shoulderleft        	HydroClimate.h	double	%	shoulder value for the day preceeding an event
 * shouldermain	        HydroClimate.h	double	%	shoulder value for the main event
 * shouldern	        HydroClimate.h	int	-	actual number of shoulder days
 * Snowremains	        HydroTimeser.h	double	m^3	snow remaining at end of summer (Aug 31)
 * Tannual		HydroClimate.h	double	degC	Actual mean Temperature for a year
 * Tchange[]	        HydroClimate.h	double	degC/y	Temperature change/year for an epoch
 * Televday[][]	        HydroClimate.h	double	degC	Daily Temp. at each elevation bin
 * Tmonth[]		HydroClimate.h	double	degC	Actual monthly mean Temperature
 * Tnominal[][]	        HydroClimate.h	double	degC	Nominal monthly mean Temperature
 * Tnomstd[][]	        HydroClimate.h	double	degC	STD of the monthly mean Temperature
 * Tstd[]		HydroClimate.h	double	degC	Standard Deviation of the annual Temperature
 * Tstart[]		HydroClimate.h	double	degC	Mean annual Temperature start value for an epoch
 *  
 *-------------------------------------------------------------------------------------------*/

#if !defined( HYDROCLIMATE_H )
#define HYDROCLIMATE_H

#include <stdio.h>

#define nmonth		12
#define Tmaxstd		3.0
#define Pmaxstd		3.0
#define maxshoulder	50      /* same as wrapday in HydroTimeSeries.h */

extern int nelevbins, shouldern;

extern double Eiceannual, Enivalannual, *ELAchange;
extern double *ELAstart, Ewetannual;
extern double glacierarea, glacierelev, *lapserate, MEtotal;
extern double Minput, Moutput, MQprevious, MQnext, Pannual;
extern double *Pchange, *Pmassbal, *Pexponent;
extern double Pmonth[nmonth];
extern double **Pnominal, **Pnomstd;
extern double *Prange;
extern double *Pstart, *Pstd;
extern double Qbedannual, *Qbedannualoutlet, *Qgrandtotal, ***Qgrandtotaloutlet, Qpeak,
  *Qpeakevents, *Qpeakperoutlet, **Qpeakperoutletall, Qtotal, **Qtotaloutlet,
  *Qtotaloutletannual, ***Qbar, *Qbartotal, *Qicebartotal, *Qpeakall,
  **Qpeakallevents, *Qicetotal, *Qsglaciertotal;
extern double *Qsgrandtotal, *Qsgrandtotaldelta, **Qsgrandtotaloutlet,
  **Csgrandtotaloutlet, *Qsbarnew, *Qsbarnew1, Qsannual, *Qsannualoutlet,
  *Csannualoutlet, *Qsbartot, *Qsglacierbar, *Qsbar, *Qsmean, *Qsglaciersmean,
  **Coutlettotal;
extern double *baseflowtot, Csannual, *Csgrandtotal, *totpercentageQ;
extern double shoulderright[maxshoulder], shoulderleft, shouldermain;
extern double Snowremains;
extern double Tannual, *Tchange;
extern double **Televday, Tmonth[nmonth], **Tnominal;
extern double **Tnomstd, *Tstart, *Tstd;
extern double **Qpeakfloodtemp;
extern double *Qsglaciersgrandtotal, *Qspsigrandtotal;
extern double Qsglacierannual, Qspsiannual;
extern double *GlacierMstorage, *GlacierMinput, *fractionglaciersediment;

#endif


