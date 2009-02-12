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
ht_get_bedload (ht_state * s)
{
  state* _s = (state*)s;
  return _s->qb[_s->day];
}

ht_state *
ht_run_until (ht_state * s, double time)
{
  return (ht_state*) run ( (state*)s, time );
}

ht_state *
ht_initialize ( char *prefix, char *out_dir )
{
  ht_state *s = (ht_state*) initialize (prefix, out_dir);
  return s;
}

ht_state *
ht_finalize (ht_state * s)
{
  finalize ( (state*)s);
  return NULL;
}

