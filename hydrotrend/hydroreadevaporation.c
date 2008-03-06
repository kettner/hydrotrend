/*
 *  Hydroreadevaporation.c
 *
 *
 *  Author:   A.J. Kettner (January 2005)
 *
 *
 * Variable		Def.Location		Type		Units	Usage
 * --------		------------		----		-----	-----
 * err			various				int			-		error flag, halts program
 * count		Read_Rainfall_Etc	int			-
 * dummyT		Read_Rainfall_Etc	double		deg.C
 * dummyTtot	Read_Rainfall_Etc	double		deg.C
 * dummyT2		Read_Rainfall_Etc	double		deg.C
 * dummyR		Read_Rainfall_Etc	double		mm
 * dummyRtot	Read_Rainfall_Etc	double		mm
 * HOURSINDAY	Read_Rainfall_Etc	-			hours	define the hours in a day
 * i			Read_Rainfall_Etc	int			-
 * k			Read_Rainfall_Etc	int			-
 * line[200]	Read_Rainfall_Etc	char		-
 * n			Read_Rainfall_Etc	long		-		counter for the number of years
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "hydroalloc_mem.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroreadevaporation.h"
#include "hydrotimeser.h"
#include "hydroclimate.h"

/*---------------------------
 *  Start of HydroReadInput
 *---------------------------*/
int hydroreadevaporation() {

/*-------------------
 *  Local Variables
 *-------------------*/
int k,err;

/*-----------------------
 *  Set local Variables
 *-----------------------*/
err=0;


    return;
}  /* end Read_Evaporation */
  

