/*
 *  hydrotrend_api.h
 *  
 *
 *  Created by Albert Kettner on 2/5/09.
 *  Copyright 2009 Univ of CO. All rights reserved.
 *
 */

#ifndef HYDROTREND_API_H_
#define HYDROTREND_API_H_

#define HT_MAJOR_VERSION (3)
#define HT_MINOR_VERSION (0)
#define HT_MICRO_VERSION (5)

typedef struct { } ht_state;

ht_state *ht_initialize (char* in_dir, char* prefix, char* dir);
ht_state *ht_finalize (ht_state * s);
ht_state *ht_run_until (ht_state * s, double time);

const const char** ht_get_exchange_items (void);
double ht_get_value (ht_state * s, char* value);

double ht_get_current_time (ht_state * s);
double ht_get_start_time (ht_state * s);
double ht_get_end_time (ht_state * s);
double ht_get_velocity (ht_state * s);
double ht_get_width (ht_state * s);
double ht_get_depth (ht_state * s);
double ht_get_water_discharge (ht_state * s);
double ht_get_sediment_discharge (ht_state * s);
double ht_get_bedload_flux (ht_state * s);

#endif
