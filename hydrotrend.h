/*-------------------------------------------------------------------------------------------
 *	hydrotrend.h
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Declaration of parameters.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#ifndef HYDROTREND_H_
#define HYDROTREND_H_
#include "hydroreadclimate.h"

int hydrocommandline (int *argc, char **argv);
void hydrosetparams ();
int hydrosecurityinputcheck ();
int hydroreadinput (char* file);
int hydroreadhypsom (char* path, char* prefix);
int hydroreadclimate (gw_rainfall_etc * gw_rain);
int hydrosetglobalpar ();
int hydrocheckinput ();
int hydroopenfiles ();
int hydrosetgeoparams (gw_rainfall_etc * gw_rain);
int hydrorandom ();
int hydroshoulder ();
int hydroclimate (gw_rainfall_etc * gw_rain);
int hydroweather (gw_rainfall_etc * gw_rain);
int hydrohypsom ();
int hydroglacial ();
int hydrosnow ();
int hydrorain ();
int hydrosumflow ();
int hydromaxevents ();
int hydrosedload (gw_rainfall_etc * gw_rain);
int hydrooutput ();
int hydroprintannual ();
int hydrocalqsnew ();
int hydroprintstat ();
int hydroswap ();
int hydrooutletfraction (int x);
int hydrosetnumberoutlet (int x);
int hydroqfractionshuffle (int k);
void hydroallocmemoutlet (int ep, int nepochs);
void hydroallocmemoutlet1 (int ep, int nepochs);
void hydrofreememoutlet (int j);
void hydrofreememoutlet1 (int ep);
int hydroshuffle (int dvals[31], int mnth);
int hydroexpdist (double pvals[31], int mnth);
void hydroinputalloc (int ep);

#endif /*  */
