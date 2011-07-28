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
ht_get_current_time (ht_state * s)
{
  state* _s = (state*)s;
  return _s->day;
}

double
ht_get_start_time (ht_state * s)
{
  return 0;
}

double
ht_get_end_time (ht_state * s)
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
ht_state *
ht_run_until (ht_state * s, double time)
{
  return (ht_state*) run ( (state*)s, time );
}

ht_state *
ht_initialize (char* in_dir, char *prefix, char *out_dir )
{
  ht_state *s = (ht_state*) initialize (in_dir, prefix, out_dir);
  return s;
}

ht_state *
ht_finalize (ht_state * s)
{
  finalize ( (state*)s);
  return NULL;
}

