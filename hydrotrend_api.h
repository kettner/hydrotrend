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

typedef ht_state BMI_Model;
//typedef state ht_state;

#define BMI_SUCCESS (0)
#define BMI_FAILURE (1)

#define BMI_COMPONENT_NAME_MAX (2048)
#define BMI_VAR_NAME_MAX (2048)
#define BMI_UNIT_NAME_MAX (2048)

typedef enum {
  BMI_VAR_TYPE_UNKNOWN = 0,
  BMI_VAR_TYPE_CHAR,
  BMI_VAR_TYPE_UNSIGNED_CHAR,
  BMI_VAR_TYPE_INT,
  BMI_VAR_TYPE_LONG,
  BMI_VAR_TYPE_UNSIGNED_INT,
  BMI_VAR_TYPE_UNSIGNED_LONG,
  BMI_VAR_TYPE_FLOAT,
  BMI_VAR_TYPE_DOUBLE,
  BMI_VAR_TYPE_COUNT
}
BMI_Var_type;

typedef enum {
  BMI_GRID_TYPE_UNKNOWN = 0,
  BMI_GRID_TYPE_UNIFORM,
  BMI_GRID_TYPE_RECTILINEAR,
  BMI_GRID_TYPE_STRUCTURED,
  BMI_GRID_TYPE_UNSTRUCTURED,
  BMI_GRID_TYPE_COUNT
}
BMI_Grid_type;

int BMI_Initialize (const char *, BMI_Model **);
int BMI_Update_until (BMI_Model *, double);
int BMI_Update (BMI_Model * s);
int BMI_Finalize (BMI_Model *);

int BMI_Get_component_name (BMI_Model * s, char *name);
int BMI_Get_output_var_names (BMI_Model * s, char **names);
int BMI_Get_output_var_name_count (BMI_Model * s, int *output_var_count);
int BMI_Get_input_var_names (BMI_Model * s, char **names);
int BMI_Get_input_var_name_count (BMI_Model * s, int *input_var_count);

int BMI_Get_var_type (BMI_Model * s, const char *name, BMI_Var_type *type);
int BMI_Get_var_rank (BMI_Model * s, const char *name, int *rank);
int BMI_Get_var_point_count (BMI_Model * s, const char *name, int *count);

ht_state *ht_initialize (char* in_dir, char* prefix, char* dir);
//ht_state *ht_finalize (ht_state * s);
//ht_state *ht_run_until (ht_state * s, double time);

const char** ht_get_exchange_items (void);
double ht_get_value (ht_state * s, char* value);

int BMI_Get_double (BMI_Model * s, const char * val_s, double *dest);
int BMI_Get_double_ptr (BMI_Model *s, const char *name, double **dest);

int BMI_Get_grid_shape (BMI_Model * s, const char * val_s, int * shape);
int BMI_Get_grid_spacing (BMI_Model * s, const char * val_s, double * spacing);
int BMI_Get_grid_origin (BMI_Model * s, const char * val_s, double * origin);

int BMI_Get_current_time (BMI_Model *, double *);
int BMI_Get_start_time (BMI_Model *, double *);
int BMI_Get_end_time (BMI_Model *, double *);
int BMI_Get_time_units (BMI_Model *s, char *units);

#define NO_BMI_SET_DOUBLE
#define NO_BMI_GET_GRID_CONNECTIVITY
#define NO_BMI_GET_GRID_OFFSET
#define NO_BMI_GET_GRID_X
#define NO_BMI_GET_GRID_Y
#define NO_BMI_GET_GRID_Z

double ht_get_velocity (ht_state * s);
double ht_get_width (ht_state * s);
double ht_get_depth (ht_state * s);
double ht_get_water_discharge (ht_state * s);
double ht_get_sediment_discharge (ht_state * s);
double ht_get_bedload_flux (ht_state * s);
double ht_get_precipitation (ht_state * s);
double ht_get_temperature (ht_state * s);

#endif
