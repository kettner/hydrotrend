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

//typedef ht_state void;
//typedef state ht_state;

#define BMI_SUCCESS (0)
#define BMI_FAILURE (1)

#define BMI_HYDROTREND_COMPONENT_NAME_MAX (2048)
#define BMI_HYDROTREND_VAR_NAME_MAX (2048)
#define BMI_HYDROTREND_UNIT_NAME_MAX (2048)


int BMI_HYDROTREND_Initialize (const char *, void **);
int BMI_HYDROTREND_Update_until (void *, double);
int BMI_HYDROTREND_Update (void * s);
int BMI_HYDROTREND_Finalize (void *);

int BMI_HYDROTREND_Get_component_name (void * s, char *name);
int BMI_HYDROTREND_Get_output_var_names (void * s, char **names);
int BMI_HYDROTREND_Get_output_var_name_count (void * s, int *output_var_count);
int BMI_HYDROTREND_Get_input_var_names (void * s, char **names);
int BMI_HYDROTREND_Get_input_var_name_count (void * s, int *input_var_count);

int BMI_HYDROTREND_Get_var_type (void * s, const char *name, char *type);
int BMI_HYDROTREND_Get_var_rank (void * s, const char *name, int *rank);
int BMI_HYDROTREND_Get_var_stride (void * s, const char *name, int *stride);
int BMI_HYDROTREND_Get_var_size (void * s, const char *name, int *size);

ht_state *ht_initialize (char* in_dir, char* prefix, char* dir);
//ht_state *ht_finalize (ht_state * s);
//ht_state *ht_run_until (ht_state * s, double time);

const char** ht_get_exchange_items (void);
double ht_get_value (ht_state * s, char* value);

int BMI_HYDROTREND_Get_value (void * s, const char * val_s, void *dest);
int BMI_HYDROTREND_Get_value_ptr (void *s, const char *name, void **dest);

int BMI_HYDROTREND_Get_grid_shape (void * s, const char * val_s, int * shape);
int BMI_HYDROTREND_Get_grid_spacing (void * s, const char * val_s, double * spacing);
int BMI_HYDROTREND_Get_grid_origin (void * s, const char * val_s, double * origin);

int BMI_HYDROTREND_Get_current_time (void *, double *);
int BMI_HYDROTREND_Get_start_time (void *, double *);
int BMI_HYDROTREND_Get_end_time (void *, double *);
int BMI_HYDROTREND_Get_time_units (void *s, char *units);

#define NO_BMI_HYDROTREND_SET_DOUBLE
#define NO_BMI_HYDROTREND_GET_GRID_CONNECTIVITY
#define NO_BMI_HYDROTREND_GET_GRID_OFFSET
#define NO_BMI_HYDROTREND_GET_GRID_X
#define NO_BMI_HYDROTREND_GET_GRID_Y
#define NO_BMI_HYDROTREND_GET_GRID_Z

double ht_get_velocity (ht_state * s);
double ht_get_width (ht_state * s);
double ht_get_depth (ht_state * s);
double ht_get_water_discharge (ht_state * s);
double ht_get_sediment_discharge (ht_state * s);
double ht_get_bedload_flux (ht_state * s);
double ht_get_precipitation (ht_state * s);
double ht_get_temperature (ht_state * s);

#endif
