/*
 *  hydrotrend_1.c
 *  
 *
 *  Created by Albert Kettner on 1/23/09.
 *  Copyright 2009 Univ of CO. All rights reserved.
 *
 */

#include "hydrotrend_cli.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#include "hydrotrend_irf.h"
#include "bmi_hydrotrend.h"


int fprint_current_time( FILE* file, char* label );
int fprint_header( FILE* fp );
int fprint_footer( FILE* fp );

int
main (int argc, char **argv)
{
  char *in_prefix = NULL;
  char *in_dir = NULL;
  char *out_dir = NULL;

  /*-----------------------------------------------------------------
   *  Check the command line input; get file name or directory name
   *-----------------------------------------------------------------*/
  {
    ht_args_st* args = parse_command_line ( argc, argv);

    if ( !args )
      {
        fprintf (stderr, " ERROR in HydroCommandLine: HydroTrend Aborted \n\n");
        exit (EXIT_FAILURE);
      }

    in_prefix = args->in_file;
    in_dir = args->in_dir;
    out_dir = args->out_dir;

    free( args );
  }

  fprint_header ( stdout );
  fprint_current_time ( stdout, "Start" );

  {
    BMI_Model * model = (BMI_Model*)malloc(sizeof(BMI_Model));

    register_bmi_hydrotrend(model);

    hydro_initialize(model->data, in_dir, in_prefix, out_dir);

    model->update_until(model, 10.5);
    model->finalize(model);

    free(model);
  }

  fprint_current_time( stdout, "Stop" );
  fprint_footer ( stdout );

  free( in_prefix );
  free( in_dir );
  if (out_dir)
    free (out_dir);

  return EXIT_SUCCESS;
}

int
fprint_header( FILE* fp )
{
  int n = 0;
  {
    n += fprintf (fp, " =====================================\n\n");
    n += fprintf (fp, " ----- HydroTrend %d.%d Model Run ----- \n\n",
                  HT_MAJOR_VERSION, HT_MINOR_VERSION );
  }
  return n;
}

int
fprint_footer( FILE* fp )
{
  int n = 0;
  {
    n += fprintf (fp, " =====================================\n\n");
  }
  return n;
}

#define TMLEN (100) /* length of the time stamp */

int
fprint_current_time( FILE* fp, char* label )
{
  int n = 0;

  {
    char pst[TMLEN];
    struct tm *timeptr;
    time_t tloc;

    time (&tloc);
    timeptr = localtime (&tloc);

    /*--------------------------------
     *  Print the program start time
     *--------------------------------*/
    strftime (pst, TMLEN, "%X  %x", timeptr);
    n += fprintf (fp, " \t%s: %s \n\n", label, pst);
  }

  return n;
}

