%module hydrotrend_mod
%{
#include "hydrotrend_api.h"
%}

%include hydrotrend_api.h

#define HT_MAJOR_VERSION (3)
#define HT_MINOR_VERSION (0)
#define HT_MICRO_VERSION (5)

ht_state *ht_initialize (char* in_dir, char* prefix, char* dir);
void BMI_Finalize (ht_state * s);
void BMI_Update_until (void * s, double time);

double ht_get_velocity (ht_state * s);
double ht_get_width (ht_state * s);
double ht_get_depth (ht_state * s);
double ht_get_water_discharge (ht_state * s);
double ht_get_sediment_discharge (ht_state * s);
double ht_get_bedload_flux (ht_state * s);
double ht_get_precipitation (ht_state * s);
double ht_get_temperature (ht_state * s);

