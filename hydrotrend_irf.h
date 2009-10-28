#if !defined( HYDROTREND_IRF_H )
#define HYDROTREND_IRF_H

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
} state;

state *ht_state_new (long n_days);
state *ht_state_destroy (state * s);

state * initialize (char* in_dir, char *in_file_prefix , char *out_dir );
state * run ( state * s, double time );
state * finalize ( state * s);

#endif
