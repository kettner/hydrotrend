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

#include <string.h>
#include <stdlib.h>
#include <getopt.h>
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroalloc_mem.h"
#include "hydrotrend.h"
#define MAXLENGTH (80)

#define HT_DEFAULT_PREFIX  "HYDRO"
#define HT_DEFAULT_OUT_DIR "HYDRO_OUT"

#define TRUE  (1)
#define FALSE (0)

int is_valid_str (char *str);
char *to_upper_str (char *str);

static int verbose_flag = 0;
static int version_flag = 0;
static int help_flag = 0;

static struct option ht_long_opts[] = {
  {"verbose", no_argument, &verbose_flag, 1},
  {"brief", no_argument, &verbose_flag, 0},
  {"version", no_argument, &version_flag, 1},
  {"help", no_argument, &help_flag, 1},
  {"in-file", required_argument, NULL, 'i'},
  {"out-dir", required_argument, NULL, 'o'},
  {NULL, 0, NULL, 0}
};

static char *help_msg[] = {
  "Usage: hydrotrend [options] [prefix [directory]]",
  "Options:",
  "  -V or --verbose      Be verbose",
  "  --brief              Be terse",
  "  -v or --version      Print version number and exit",
  "  -?,-h or --help      Print this information and exit",
  "  -i, --in-file=PREFIX Use PREFIX.IN as input file",
  "  -o, --out-dir=DIR    Put output in directory DIR",
  NULL
};

/** Parse command line options for hydrotrend.

Usage: hydrotrend [options] [prefix [directory]]

file is the prefix for the HydroTrend output files.  If not given, HYDRO is
used.  If directory is given output files will be written to that directory.
If not given, HYDRO_OUT is used.

Use: 'hydrotrend --help' for a full list of command line options.
*/
int
parse_command_line (int argc, char *argv[])
{
  int success = TRUE;

  if (argv)
    {
      int ch;
      char *in_file_prefix = NULL;
      char *out_dir = NULL;
      int option_index;

      while ((ch =
              getopt_long (argc, argv, "vVh?i:o:", ht_long_opts,
                           &option_index)) != -1)
        {
          switch (ch)
            {
            case 'i':
              in_file_prefix = strdup (optarg);
              break;
            case 'o':
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
              fprintf (stderr, "Error: Unknown option %d\n", ch);
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
              fprintf (stderr, "Error: Prefix specifed twice\n", ch);
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
              fprintf (stderr, "Error: Output directory specifed twice\n", ch);
              exit (EXIT_FAILURE);
            }
        }
      if (!out_dir)
        out_dir = strdup (HT_DEFAULT_OUT_DIR);


      if (verbose_flag)
        verbose = 1;
      else
        verbose = 0;

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

      if (verbose)
        {
          fprintf (stderr, "Using input file %s.IN\n", in_file_prefix);
          fprintf (stderr, "Using output directory %s\n", out_dir);
        }

      strcpy (commandlinearg[1], in_file_prefix);
      strcpy (commandlinearg[2], out_dir);

      free (in_file_prefix);
      free (out_dir);
    }

  return success;
}

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
