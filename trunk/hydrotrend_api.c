/*
 *  hydrotrend_api.c
 *  
 *
 *  Created by Albert Kettner on 2/5/09.
 *  Copyright 2009 Univ of CO. All rights reserved.
 *
 */

#include "hydrotrend_api.h"
#include "hydrotrend_irf.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double
BMI_Get_current_time (void * s )
{
  state* _s = (state*)s;
  return _s->day;
}

double
BMI_Get_start_time (void * s)
{
  return 0;
}

double
BMI_Get_end_time (void * s)
{
  state* _s = (state*)s;
  return _s->n_days;
}

const char* exchange_items[] =
{
  "VELOCITY",
  "WIDTH",
  "DEPTH",
  "WATER_DISCHARGE",
  "SEDIMENT_DISCHARGE",
  "BEDLOAD_FLUX",
  "PRECIPITATION",
  "TEMPERATURE",
  "NULL"
};

const const char**
ht_get_exchange_items (void)
{
  return exchange_items;
}

const char **
BMI_Get_output_var_names (void * s)
{
 return exchange_items;
}

double
ht_get_value (ht_state * s, char* value)
{
  if (strcasecmp (value, "velocity")==0)
    return ht_get_velocity (s);
  else if (strcasecmp (value, "width")==0)
    return ht_get_width (s);
  else if (strcasecmp (value, "depth")==0)
    return ht_get_depth (s);
  else if (strcasecmp (value, "water_discharge")==0)
    return ht_get_water_discharge (s);
  else if (strcasecmp (value, "sediment_discharge")==0)
    return ht_get_sediment_discharge (s);
  else if (strcasecmp (value, "bedload_flux")==0)
    return ht_get_bedload_flux (s);
  else if (strcasecmp (value, "precipitation")==0)//A.KETTNER JULY 28TH, 2011
    return ht_get_precipitation (s);              //A.KETTNER JULY 28TH, 2011
  else if (strcasecmp (value, "temperature")==0)  //A.KETTNER JULY 28TH, 2011
    return ht_get_temperature (s);                //A.KETTNER JULY 28TH, 2011
  else
    fprintf (stderr, "ERROR: %s: Bad value string.", value);

  return 0.;
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
void
BMI_Update_until (void * s, double time)
{
  (ht_state*) run ( (state*)s, time );
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

void *
BMI_Initialize (const char * file)
{
  ht_state * self = NULL;

  FILE * fp = fopen (file, "r");

  if (fp)
  {
    char args[2048];
    char *in_dir = NULL;
    char *prefix = NULL;
    char *out_dir = NULL;

    if (fgets (args, 2048, fp)==args) {
      in_dir = __str_strip (strdup (args));
    }
    if (fgets (args, 2048, fp)==args) {
      prefix = __str_strip (strdup (args));
    }
    if (fgets (args, 2048, fp)==args) {
      out_dir = __str_strip (strdup (args));
    }

    if (in_dir && prefix && out_dir) {
      self = (void *)initialize (in_dir, prefix, out_dir);
    }
  }

  return (void *)self;
}

//ht_state *
//ht_finalize (ht_state * s)
void
BMI_Finalize (void * s)
{
  finalize ((state*)s);
}

#include <stdlib.h>

int *
BMI_Get_grid_shape (void * s, const char * val_s, int * n_dim)
{
  int * shape = (int *) malloc (sizeof (double));
  *n_dim = 1;
  shape[0] = 1;
  return shape;
}

double *
BMI_Get_grid_spacing (void * s, const char * val_s, int * n_dim)
{
  double * spacing = (double *) malloc (sizeof (double));
  *n_dim = 1;
  spacing[0] = 1.;
  return spacing;
}

double *
BMI_Get_grid_lower_left_corner (void * s, const char * val_s, int * n_dim)
{
  double * corner = (double *) malloc (sizeof (double));
  *n_dim = 1;
  corner[0] = 0.;
  return corner;
}

double *
BMI_Get_double (void * s, const char * val_s, int * n_dim, int ** shape)
{
  double * val = (double*)malloc (sizeof (double));

  *shape = BMI_Get_grid_shape (s, val_s, n_dim);
 
  if (strcasecmp (val_s, "velocity")==0)
    *val = ht_get_velocity (s);
  else if (strcasecmp (val_s, "width")==0)
    *val = ht_get_width (s);
  else if (strcasecmp (val_s, "depth")==0)
    *val = ht_get_depth (s);
  else if (strcasecmp (val_s, "water_discharge")==0)
    *val = ht_get_water_discharge (s);
  else if (strcasecmp (val_s, "sediment_discharge")==0)
    *val = ht_get_sediment_discharge (s);
  else if (strcasecmp (val_s, "bedload_flux")==0)
    *val = ht_get_bedload_flux (s);
  else if (strcasecmp (val_s, "precipitation")==0)
    *val = ht_get_precipitation (s);
  else if (strcasecmp (val_s, "temperature")==0)
    *val = ht_get_temperature (s);
  else {
    fprintf (stderr, "ERROR: %s: Bad value string.", val_s);
    free (val);
    val = NULL;
  }

  return val;
}

