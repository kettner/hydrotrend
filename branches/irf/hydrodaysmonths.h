/*-------------------------------------------------------------------------------------------
 *	hydrodaysmonths.h
 * 
 *	Author: Albert Kettner, March 2006
 *
 *	Declaration of nr. of days per month for a year
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 * 
 *-------------------------------------------------------------------------------------------*/
   
#ifndef HYDRODAYSMONTHS_H_
#define HYDRODAYSMONTHS_H_
static int daystrm[12] =
  { 1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 };
static int dayendm[12] =
  { 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
static int daysim[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };


#endif  /*  */
  
