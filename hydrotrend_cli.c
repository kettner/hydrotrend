/*-------------------------------------------------------------------------------------------
 *	hydrocommandLine.c
 *
 *	Author:    Albert Kettner, March 2006
 * 
 *  HydroCommandline handles the commandline imput to set 1) the output file name, 2) the output
 *	directory structure. If the program starts without any extra commandline options, all file
 *	names with start with HYDRO and the output will be written to a HYDRO_OUTPUT directory.
 *  
 * Variable		Def.Location		Type		Units	Usage
 * --------		------------		----		-----	-----
 * 
 * 
 *-------------------------------------------------------------------------------------------*/

#include "hydrotrend_cli.h"
#include "hydroparams.h"
#include "bmi_hydrotrend.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef WITH_GETOPT
# include <getopt.h>
#endif


#define MAXLENGTH (80)

#define HT_DEFAULT_PREFIX  "HYDRO"
#define HT_DEFAULT_IN_DIR  "./HYDRO_IN"
#define HT_DEFAULT_OUT_DIR "HYDRO_OUTPUT"

#define TRUE  (1)
#define FALSE (0)

int is_valid_str (char *str);
char *to_upper_str (char *str);

#ifndef WITH_GETOPT
static char *help_msg[] = {
  "Usage: hydrotrend [indir [outdir]]",
  "Options:",
  "  -v       Print version number and exit",
  "  -?,-h    Print this information and exit",
  NULL
};

ht_args_st *
parse_command_line (int argc, char *argv[])
{
  ht_args_st *args = malloc(sizeof(ht_args_st));
  int arg;
  int help_flag = 0;
  int version_flag = 0;

  for (arg=0; arg<argc; arg++) {
    if (strcmp(argv[arg], "-h") == 0 || strcmp(argv[arg], "-?") == 0) {
      help_flag = 1;
    }
    if (strcmp(argv[arg], "-v") == 0) {
      version_flag = 1;
    }
  }
  if (help_flag) {
    char **str;
    for (str = help_msg; *str; str++)
      fprintf (stdout, "%s\n", *str);
    exit (EXIT_SUCCESS);
  }

  if (version_flag) {
    fprintf(
        stdout,
        "HydroTrend version %d.%d.%d\n",
        HT_MAJOR_VERSION, HT_MINOR_VERSION, HT_MICRO_VERSION
    );
    exit (EXIT_SUCCESS);
  }

  args->in_file = strdup(HT_DEFAULT_PREFIX);

  if (argc == 1) {
    args->in_dir = strdup(HT_DEFAULT_IN_DIR);
    args->out_dir = strdup(HT_DEFAULT_OUT_DIR);
  } else if (argc == 2) {
    args->in_dir = strdup(argv[1]);
    args->out_dir = strdup(HT_DEFAULT_OUT_DIR);
  } else if (argc == 3 ) {
    args->in_dir = strdup(argv[1]);
    args->out_dir = strdup(argv[2]);
  } else {
    fprintf(stderr, "incorrect arguments. %s -h for help", argv[0]);
    exit(EXIT_FAILURE);
  }

  return args;
}
#else
static int verbose_flag = 0;
static int version_flag = 0;
static int help_flag = 0;

static struct option ht_long_opts[] = {
  {"verbose", no_argument, &verbose_flag, 1},
  {"brief", no_argument, &verbose_flag, 0},
  {"version", no_argument, &version_flag, 1},
  {"help", no_argument, &help_flag, 1},
  {"prefix", required_argument, NULL, 'p'},
  {"in-dir", required_argument, NULL, 'S'},
  {"out-dir", required_argument, NULL, 'D'},
  {NULL, 0, NULL, 0}
};

static char *help_msg[] = {
  "Usage: hydrotrend [options] [prefix [directory]]",
  "Options:",
  "  -V or --verbose      Be verbose",
  "  --brief              Be terse",
  "  -v or --version      Print version number and exit",
  "  -?,-h or --help      Print this information and exit",
  "  -p, --prefix=PREFIX  Use PREFIX.IN as input file",
  "  -S, --in-dir=DIR     Path to input file(s)",
  "  -D, --out-dir=DIR    Put output in directory DIR",
  NULL
};

/** Parse command line options for hydrotrend.

Usage: hydrotrend [options] [prefix [directory]]

file is the prefix for the HydroTrend output files.  If not given, HYDRO is
used.  If directory is given output files will be written to that directory.
If not given, HYDRO_OUT is used.

Use: 'hydrotrend --help' for a full list of command line options.
*/
ht_args_st *
parse_command_line (int argc, char *argv[])
{
  ht_args_st *args = NULL;

  if (argv)
    {
      int ch;
      char *in_file_prefix = NULL;
      char *in_dir = NULL;
      char *out_dir = NULL;

      while ((ch =
              getopt_long (argc, argv, "vVh?p:D:S:", ht_long_opts,
                           NULL)) != -1)
        {
          switch (ch)
            {
            case 'p':
              in_file_prefix = strdup (optarg);
              break;
            case 'S':
              in_dir = strdup (optarg);
              break;
            case 'D':
              out_dir = strdup (optarg);
              break;
            case 'v':
              version_flag = 1;
              break;
            case 'V':
              verbose_flag = 1;
              break;
            case '?':
            case 'h':
              help_flag = 1;
              break;
            case 0:
              break;
            default:
              fprintf (stderr, "Error: Unknown option %c\n", ch);
              exit (EXIT_FAILURE);
            }
        }

      if (help_flag)
        {
          char **str;
          for (str = help_msg; *str; str++)
            fprintf (stdout, "%s\n", *str);
          exit (EXIT_SUCCESS);
        }

      if (version_flag)
        {
          fprintf (stdout, "HydroTrend version %d.%d.%d\n",
                   HT_MAJOR_VERSION, HT_MINOR_VERSION, HT_MICRO_VERSION);
          exit (EXIT_SUCCESS);
        }

      if (optind < argc)
        {
          if (!in_file_prefix)
            in_file_prefix = strdup (argv[optind++]);
          else
            {
              fprintf (stderr, "Error: Prefix specifed twice\n");
              exit (EXIT_FAILURE);
            }
        }
      if (!in_file_prefix)
        in_file_prefix = strdup (HT_DEFAULT_PREFIX);

      if (optind < argc)
        {
          fprintf (stderr, "optind = %d\n", optind);
          if (!out_dir)
            out_dir = strdup (argv[optind++]);
          else
            {
              fprintf (stderr, "Error: Output directory specifed twice\n");
              exit (EXIT_FAILURE);
            }
        }
      if (!out_dir)
        out_dir = NULL;
        //out_dir = strdup (HT_DEFAULT_OUT_DIR);

      if (!in_dir)
        in_dir = strdup (HT_DEFAULT_IN_DIR);

      if (verbose_flag)
        verbose = 1;
      else
        verbose = 0;
/*
      if (!is_valid_str (in_file_prefix) || !is_valid_str (out_dir))
        {
          fprintf (stderr, "  HydroTrend ERROR: Incorrect command line \n");
          fprintf (stderr,
                   "    Prefix/directory can only contain alpha numeric characters\n");
          exit (EXIT_FAILURE);
        }
      else
        {
          to_upper_str (in_file_prefix);
          to_upper_str (out_dir);
        }
*/
      if (verbose)
        {
          fprintf (stderr, "Using input file %s.IN\n", in_file_prefix);
          fprintf (stderr, "Using input directory %s\n", in_dir);
          if (out_dir)
            fprintf (stderr, "Using output directory %s\n", out_dir);
          else
            fprintf (stderr, "Using output directory specified in input file\n");
        }

      args = malloc( sizeof(ht_args_st) );
      args->in_file = in_file_prefix;
      args->in_dir = in_dir;
      args->out_dir = out_dir;

      strcpy (commandlinearg[0], in_file_prefix);
      if (out_dir)
        strcpy (commandlinearg[1], out_dir);
      else
        commandlinearg[1][0] = '\0';
    }

  return args;
}
#endif


int
is_valid_str (char *str)
{
  int is_valid = 1;

  if (strchr (str, '%') || strchr (str, '*') ||
      strchr (str, '#') || strchr (str, '@') ||
      strchr (str, '-') || strchr (str, '^') ||
      strchr (str, '"') || strchr (str, '?') ||
      strchr (str, '!') || strchr (str, '.') ||
      strchr (str, ',') || strchr (str, '$'))
    is_valid = 0;

  return is_valid;
}

char *
to_upper_str (char *str)
{
  if (str)
    {
      int i;
      for (i = 0; i < strlen (str); i++)
        str[i] = toupper (str[i]);
    }
  return str;
}
