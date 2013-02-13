/*
 *  hydrotrend_api.c
 *  
 *
 *  Created by Albert Kettner on 2/5/09.
 *  Copyright 2009 Univ of CO. All rights reserved.
 *
 */

#include "hydrotrend_irf.h"
#include "hydrotrend_api.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef state _BMI_Model;

int
BMI_HYDROTREND_Get_current_time (BMI_Model * s, double * time)
{
  state* _s = (state*)s;
  *time = _s->day;
  return BMI_SUCCESS;
}

int
BMI_HYDROTREND_Get_start_time (BMI_Model * s, double * time)
{
  *time = 0;
  return BMI_SUCCESS;
}

int
BMI_HYDROTREND_Get_end_time (BMI_Model * s, double * time)
{
  state* _s = (state*)s;
  *time  = _s->n_days;
  return BMI_SUCCESS;
}

int
BMI_HYDROTREND_Get_time_units (BMI_Model *s, char *units)
{
  if (s && units) {
    strcpy (units, "d");
    return BMI_SUCCESS;
  }
  else
    return BMI_FAILURE;
}

int
BMI_HYDROTREND_Get_component_name (BMI_Model * s, char *name)
{
  strcpy (name, "HydroTrend");
  return BMI_SUCCESS;
}

#define OUTPUT_VAR_NAME_COUNT (8)
const char* exchange_items[OUTPUT_VAR_NAME_COUNT] =
{
  "channel_outflow_end_water__speed",
  "channel_outflow_end__width",
  "channel_outflow_end_water__depth",
  "channel_outflow_end_water__discharge",
  "channel_outflow_end_suspended_sediment__discharge",
  "channel_outflow_end_bed_load_sediment__mass_flow_rate",
  "mean_over_domain_of_water__precipitation_rate",
  "mean_over_domain_of_air__temperature",
};

const const char**
ht_get_exchange_items (void)
{
  return exchange_items;
}

int
BMI_HYDROTREND_Get_output_var_name_count (BMI_Model * s, int *output_var_count)
{
  if (output_var_count) {
    *output_var_count = OUTPUT_VAR_NAME_COUNT;
    return BMI_SUCCESS;
  }
  return BMI_FAILURE;
}

int
BMI_HYDROTREND_Get_output_var_names (BMI_Model * s, char **names)
{
  int rtn = BMI_FAILURE;
  if (names) {
    int i;
    for (i=0; i<OUTPUT_VAR_NAME_COUNT; i++)
      strncpy (names[i], exchange_items[i], BMI_HYDROTREND_VAR_NAME_MAX);
    rtn = BMI_SUCCESS;
  }
  return rtn;
}

int
BMI_HYDROTREND_Get_input_var_name_count (BMI_Model * s, int *input_var_count)
{
  if (input_var_count) {
    *input_var_count = 0;
    return BMI_SUCCESS;
  }
  return BMI_FAILURE;
}

int
BMI_HYDROTREND_Get_input_var_names (BMI_Model * s, char **names)
{
  return -BMI_FAILURE;
}

double
ht_get_value (ht_state * s, char* value)
{
  if (strcmp (value, "channel_outflow_end_water__speed")==0)
    return ht_get_velocity (s);
  else if (strcmp (value, "channel_outflow_end__width")==0)
    return ht_get_width (s);
  else if (strcmp (value, "channel_outflow_end_water__depth")==0)
    return ht_get_depth (s);
  else if (strcmp (value, "channel_outflow_end_water__discharge")==0)
    return ht_get_water_discharge (s);
  else if (strcmp (value, "channel_outflow_end_suspended_sediment__discharge")==0)
    return ht_get_sediment_discharge (s);
  else if (strcmp (value, "channel_outflow_end_bed_load_sediment__mass_flow_rate")==0)
    return ht_get_bedload_flux (s);
  else if (strcmp (value, "mean_over_domain_of_water__precipitation_rate")==0)//A.KETTNER JULY 28TH, 2011
    return ht_get_precipitation (s);              //A.KETTNER JULY 28TH, 2011
  else if (strcmp (value, "mean_over_domain_of_air__temperature")==0)  //A.KETTNER JULY 28TH, 2011
    return ht_get_temperature (s);                //A.KETTNER JULY 28TH, 2011
  else
    fprintf (stderr, "ERROR: %s: Bad value string.", value);

  return 0.;
}

int
BMI_HYDROTREND_Get_var_type (BMI_Model * s, const char *name, BMI_Var_type *type)
{
  if (type) {
    *type = BMI_VAR_TYPE_DOUBLE;
    return BMI_SUCCESS;
  }
  else
    return  BMI_FAILURE;
}

int
BMI_HYDROTREND_Get_var_rank (BMI_Model * s, const char *name, int *rank)
{
  if (rank) {
    *rank = 0;
    return BMI_SUCCESS;
  }
  else
    return  BMI_FAILURE;
}

int
BMI_HYDROTREND_Get_var_point_count (BMI_Model * s, const char *name, int *count)
{
  if (count) {
    *count = 1;
    return BMI_SUCCESS;
  }
  else
    return  BMI_FAILURE;
}

int
BMI_HYDROTREND_Get_var_stride (BMI_Model * s, const char *name, int *stride)
{
  return -BMI_FAILURE;
}

double
ht_get_velocity (ht_state * s)
{
  state* _s = (state*)s;
  return _s->velocity[_s->day];
}

double
ht_get_width (ht_state * s)
{
  state* _s = (state*)s;
  return _s->width[_s->day];
}

double
ht_get_depth (ht_state * s)
{
  state* _s = (state*)s;
  return _s->width[_s->day];
}

double
ht_get_water_discharge (ht_state * s)
{
  state* _s = (state*)s;
  return _s->q[_s->day];
}

double
ht_get_sediment_discharge (ht_state * s)
{
  state* _s = (state*)s;
  return _s->qs[_s->day];
}

double
ht_get_bedload_flux (ht_state * s)
{
  state* _s = (state*)s;
  return _s->qb[_s->day];
}
  //A.KETTNER JULY 28TH, 2011
double
ht_get_precipitation (ht_state * s)
{
  state* _s = (state*)s;
  return _s->prec[_s->day];
}

double
ht_get_temperature (ht_state * s)
{
  state* _s = (state*)s;
  return _s->temp[_s->day];
}
  //END //A.KETTNER JULY 28TH, 2011
//ht_state *
//ht_run_until (ht_state * s, double time)
int
BMI_HYDROTREND_Update_until (BMI_Model * s, double time)
{
  run ( (state*)s, time );
  return BMI_SUCCESS;
}

int
BMI_HYDROTREND_Update (BMI_Model * s)
{
  double day;

  BMI_HYDROTREND_Get_current_time (s, &day);
  run ( (state*)s, day+1 );

  return BMI_SUCCESS;
}

ht_state *
ht_initialize (char* in_dir, char *prefix, char *out_dir )
{
  ht_state *s = (ht_state*) initialize (in_dir, prefix, out_dir);
  return s;
}


#include <string.h>

char *
__str_strip (char *str) {
  char * end;

  while (isspace (*str))
    str++;

  end = str + strlen (str)-1;

  while (end > str && isspace (*end))
    end--;

  *(end+1) = '\0';

  return str;
}

int
BMI_HYDROTREND_Initialize (const char * file, BMI_Model ** handle)
{
  int rtn = BMI_FAILURE;

  if (handle) {
    ht_state * self = NULL;
    char *in_dir = NULL;
    char *prefix = NULL;
    char *out_dir = NULL;

    if (file) {
      FILE * fp = fopen (file, "r");

      if (fp) {
        char args[2048];

        if (fgets (args, 2048, fp)==args) {
          in_dir = __str_strip (strdup (args));
        }
        if (fgets (args, 2048, fp)==args) {
          prefix = __str_strip (strdup (args));
        }
        if (fgets (args, 2048, fp)==args) {
          out_dir = __str_strip (strdup (args));
        }
      }
    }
    else {
      in_dir = strdup ("/scratch/huttone/cca-projects/cem/0.1/internal/share/hydrotrend/input");
      prefix = strdup ("HYDRO");
      out_dir = strdup ("/scratch/huttone/cca-projects/cem/0.1/internal");
    }

    if (in_dir && prefix && out_dir) {
      self = (ht_state *)initialize (in_dir, prefix, out_dir);
    }

    if (self) {
      *handle = self;
      rtn = BMI_SUCCESS;
    }
  }

  return rtn;
}

//ht_state *
//ht_finalize (ht_state * s)
int
BMI_HYDROTREND_Finalize (BMI_Model * s)
{
  finalize ((state*)s);
  return BMI_SUCCESS;
}

#include <stdlib.h>

int
BMI_HYDROTREND_Get_grid_shape (BMI_Model * s, const char * val_s, int * shape)
{
  int rtn = BMI_FAILURE;

  if (shape) {
    shape[0] = 1;
    rtn = BMI_SUCCESS;
  }
  return rtn;
}

int
BMI_HYDROTREND_Get_grid_spacing (BMI_Model * s, const char * val_s, double * spacing)
{
  int rtn = BMI_FAILURE;

  if (spacing) {
    spacing[0] = 1;
    rtn = BMI_SUCCESS;
  }
  return rtn;
}

int
BMI_HYDROTREND_Get_grid_origin (BMI_Model * s, const char * val_s, double *origin)
{
  int rtn = BMI_FAILURE;

  if (origin) {
    origin[0] = 1;
    rtn = BMI_SUCCESS;
  }
  return rtn;
}

int
BMI_HYDROTREND_Get_double (BMI_Model *s, const char * val_s, double *dest)
{
  int rtn = BMI_FAILURE;

  if (dest) {
    double val;

    if (strcmp (val_s, "channel_outflow_end_water__speed")==0)
      val = ht_get_velocity (s);
    else if (strcmp (val_s, "channel_outflow_end__width")==0)
      val = ht_get_width (s);
    else if (strcmp (val_s, "channel_outflow_end_water__depth")==0)
      val = ht_get_depth (s);
    else if (strcmp (val_s, "channel_outflow_end_water__discharge")==0)
      val = ht_get_water_discharge (s);
    else if (strcmp (val_s, "channel_outflow_end_suspended_sediment__discharge")==0)
      val = ht_get_sediment_discharge (s);
    else if (strcmp (val_s, "channel_outflow_end_bed_load_sediment__mass_flow_rate")==0)
      val = ht_get_bedload_flux (s);
    else if (strcmp (val_s, "mean_over_domain_of_water__precipitation_rate")==0)
      val = ht_get_precipitation (s);
    else if (strcmp (val_s, "mean_over_domain_of_air__temperature")==0)
      val = ht_get_temperature (s);
    else {
      return BMI_FAILURE;
    }

    dest[0] = val;
    rtn = BMI_SUCCESS;
  }

  return rtn;
}

int
BMI_HYDROTREND_Get_double_ptr (BMI_Model *s, const char *name, double **dest)
{
  return -BMI_FAILURE;
}

