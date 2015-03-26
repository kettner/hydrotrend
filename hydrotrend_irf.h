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

#if defined(__cplusplus)
}
#endif

#endif
