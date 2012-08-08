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

void *BMI_Initialize (const char *);
void BMI_Update_until (void *, double);
void BMI_Finalize (void *);

const char **BMI_Get_output_var_names (void *);

ht_state *ht_initialize (char* in_dir, char* prefix, char* dir);
//ht_state *ht_finalize (ht_state * s);
//ht_state *ht_run_until (ht_state * s, double time);

const const char** ht_get_exchange_items (void);
double ht_get_value (ht_state * s, char* value);

double * BMI_Get_double (void * s, const char * val_s, int * n_dim,
                         int ** shape);

int * BMI_Get_grid_shape (void * s, const char * val_s, int * n_dim);
double * BMI_Get_grid_spacing (void * s, const char * val_s, int * n_dim);
double * BMI_Get_grid_lower_left_corner (void * s, const char * val_s, int * n_dim);

double BMI_Get_current_time (void *);
double BMI_Get_start_time (void *);
double BMI_Get_end_time (void *);

double ht_get_velocity (ht_state * s);
double ht_get_width (ht_state * s);
double ht_get_depth (ht_state * s);
double ht_get_water_discharge (ht_state * s);
double ht_get_sediment_discharge (ht_state * s);
double ht_get_bedload_flux (ht_state * s);
double ht_get_precipitation (ht_state * s);
double ht_get_temperature (ht_state * s);

#endif
