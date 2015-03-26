#if !defined(HYDROTREND_IRF_H)
#define HYDROTREND_IRF_H

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct
{
  long day;
  long n_days;
  double *q;
  double *velocity;
  double *width;
  double *depth;
  double *qs;
  double *qb;
  double *temp;
  double *prec;
} state;


extern state * hydro_initialize (char* in_dir, char *in_file_prefix,
                                 char *out_dir);
extern state * hydro_run (state * s, double time);
extern state * hydro_finalize (state * s);

extern double * hydro_get_velocity_ptr (state * self);
extern double hydro_get_velocity (state * self);
extern double * hydro_get_width_ptr (state * self);
extern double hydro_get_width (state * self);
extern double * hydro_get_depth_ptr (state * self);
extern double hydro_get_depth (state * self);
extern double * hydro_get_water_discharge_ptr (state * self);
extern double hydro_get_water_discharge (state * self);
extern double * hydro_get_sediment_discharge_ptr (state * self);
extern double hydro_get_sediment_discharge (state * self);
extern double * hydro_get_bedload_flux_ptr (state * self);
extern double hydro_get_bedload_flux (state * self);
extern double * hydro_get_precipitation_ptr (state * self);
extern double hydro_get_precipitation (state * self);
extern double * hydro_get_temperature_ptr (state * self);
extern double hydro_get_temperature (state * self);

#if defined(__cplusplus)
}
#endif

#endif
