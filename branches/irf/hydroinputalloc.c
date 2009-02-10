/*-------------------------------------------------------------------------------------------
 *	hydroinputalloc.c	Dynamic allocation of all input variables
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Dynamic allocation of input variables
 * 
 * Variable		Def.Location	Type	Units	Usage
 * --------		------------	----	-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#include <stdlib.h>
#include "hydroparams.h"
#include "hydroalloc_mem.h"
#include "hydroclimate.h"

/*----------------------------
 *  Start of HydroInputAlloc
 *----------------------------*/
void
hydroinputalloc (int nepochs)
{

  syear = malloc1d (nepochs, int);
  nyears = malloc1d (nepochs, int);
  grainpct = malloc2d (maxgrn, nepochs, double);
  Tstart = malloc1d (nepochs, double);
  Tchange = malloc1d (nepochs, double);
  Tstd = malloc1d (nepochs, double);
  Pstart = malloc1d (nepochs, double);
  Pchange = malloc1d (nepochs, double);
  Pstd = malloc1d (nepochs, double);
  Pmassbal = malloc1d (nepochs, double);
  Pexponent = malloc1d (nepochs, double);
  Prange = malloc1d (nepochs, double);
  baseflowtot = malloc1d (nepochs, double);
  Tnominal = malloc2d (nmonth, nepochs, double);
  Tnomstd = malloc2d (nmonth, nepochs, double);
  Pnominal = malloc2d (nmonth, nepochs, double);
  Pnomstd = malloc2d (nmonth, nepochs, double);
  lapserate = malloc1d (nepochs, double);
  ELAstart = malloc1d (nepochs, double);
  ELAchange = malloc1d (nepochs, double);
  dryevap = malloc1d (nepochs, double);
  rslope = malloc1d (nepochs, double);
  basinlength = malloc1d (nepochs, double);
  Rvol = malloc1d (nepochs, double);
  Rparamcheck = malloc1d (nepochs, char);
  Ralt = malloc1d (nepochs, double);
  Rarea = malloc1d (nepochs, double);
  velcof = malloc1d (nepochs, double);
  velpow = malloc1d (nepochs, double);
  widcof = malloc1d (nepochs, double);
  widpow = malloc1d (nepochs, double);
  depcof = malloc1d (nepochs, double);
  deppow = malloc1d (nepochs, double);
  avgvel = malloc1d (nepochs, double);
  gwmax = malloc1d (nepochs, double);
  gwmin = malloc1d (nepochs, double);
  alphass = malloc1d (nepochs, double);
  betass = malloc1d (nepochs, double);
  Ko = malloc1d (nepochs, double);
  eventsnr = malloc1d (nepochs, int);
  floodvalue = malloc1d (nepochs, double);
  sedfilter = malloc1d (nepochs, double);
  Qsbarformulaflag = malloc1d (nepochs, int);
  TEsubbasin = malloc1d (nepochs, double);
  TE = malloc1d (nepochs, double);
  Qsgrandtotal = malloc1d (nepochs, double);
  Qspsigrandtotal = malloc1d (nepochs, double);
  Csgrandtotal = malloc1d (nepochs, double);
  Qsgrandtotaldelta = malloc1d (nepochs, double);
  Qgrandtotal = malloc1d (nepochs, double);
  Qsbarnew = malloc1d (nepochs, double);
  Qsbarnew1 = malloc1d (nepochs, double);
  Qsbartot = malloc1d (nepochs, double);
  Qsbar = malloc1d (nepochs, double);
  Qsmean = malloc1d (nepochs, double);
  Qsglaciersmean = malloc1d (nepochs, double);
  Qbartotal = malloc1d (nepochs, double);
  Qicebartotal = malloc1d (nepochs, double);
  Qpeakall = malloc1d (nepochs, double);
  Qicetotal = malloc1d (nepochs, double);
  GlacierMinput = malloc1d (nepochs, double);
  GlacierMstorage = malloc1d (nepochs, double);
  fractionglaciersediment = malloc1d (nepochs, double);
  Qsglaciertotal = malloc1d (nepochs, double);
  Qsglacierbar = malloc1d (nepochs, double);
  lithology = malloc1d (nepochs, double);
  anthro = malloc1d (nepochs, double);
  return;
}
