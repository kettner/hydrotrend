/*
 *  hydrotrend_API.h
 *  
 *
 *  Created by Albert Kettner on 2/5/09.
 *  Copyright 2009 Univ of CO. All rights reserved.
 *
 */

#ifndef HYDROTREND_API_H_
#define HYDROTREND_API_H_

#include "hydrofree_mem.h"
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroalloc_mem.h"
#include "hydroreadclimate.h"
#include "hydrotrend.h"
#include <stdlib.h>
#include <time.h>
#include <string.h>

typedef struct { } ht_state;

ht_state *ht_initialize (char* prefix, char* dir);
ht_state *ht_finalize (ht_state * s);
ht_state *ht_run_until (ht_state * s, double time);

double ht_get_velocity (ht_state * s);
double ht_get_width (ht_state * s);
double ht_get_depth (ht_state * s);
double ht_get_water_discharge (ht_state * s);
double ht_get_sediment_discharge (ht_state * s);
double ht_get_bedload_flux (ht_state * s);

#endif
