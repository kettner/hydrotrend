#if !defined( HYDROTREND_CLI_H )
#define HYDROTREND_CLI_H

typedef struct
{
   char *in_file;
   char *out_dir;
}
ht_args_st;

ht_args_st *parse_command_line (int argc, char *argv[]);

#endif

