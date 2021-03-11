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
  double *cs;
  double *qb;
  double *temp;
  double *prec;
} HydrotrendData;


extern HydrotrendData* new_data();
extern void initialize_data(HydrotrendData* data, int n_days);

extern void hydro_initialize(HydrotrendData *self, char* in_dir, char* in_file_prefix, char* out_dir);
// extern HydrotrendData * hydro_initialize (char* in_dir, char *in_file_prefix, char *out_dir);
extern HydrotrendData * hydro_run (HydrotrendData * s, double time);
extern HydrotrendData * hydro_finalize (HydrotrendData * s);

extern double * hydro_get_velocity_ptr (HydrotrendData * self);
extern double hydro_get_velocity (HydrotrendData * self);
extern double * hydro_get_width_ptr (HydrotrendData * self);
extern double hydro_get_width (HydrotrendData * self);
extern double * hydro_get_depth_ptr (HydrotrendData * self);
extern double hydro_get_depth (HydrotrendData * self);
extern double * hydro_get_water_discharge_ptr (HydrotrendData * self);
extern double hydro_get_water_discharge (HydrotrendData * self);
extern double * hydro_get_sediment_discharge_ptr (HydrotrendData * self);
extern double hydro_get_sediment_discharge (HydrotrendData * self);
extern double * hydro_get_sediment_concentration_ptr(HydrotrendData * self);
extern double hydro_get_sediment_concentration(HydrotrendData * self);
extern double * hydro_get_bedload_flux_ptr (HydrotrendData * self);
extern double hydro_get_bedload_flux (HydrotrendData * self);
extern double * hydro_get_precipitation_ptr (HydrotrendData * self);
extern double hydro_get_precipitation (HydrotrendData * self);
extern double * hydro_get_temperature_ptr (HydrotrendData * self);
extern double hydro_get_temperature (HydrotrendData * self);

#if defined(__cplusplus)
}
#endif

#endif
