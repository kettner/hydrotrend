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
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroalloc_mem.h"
#define MAXLENGTH (80)
 
/*-----------------------------
 *  Start of HydroCommandLine
 *-----------------------------*/
int hydrocommandline(int *argc,char **argv)
{ 

/*-------------------
 *  Local Variables
 *-------------------*/
int err, jj;

/*------------------------
 *  Initialize Variables
 *------------------------*/
err = 0;

	for (jj=0; jj<*argc; jj++){
		strcpy(commandlinearg[jj],argv[jj]);
	}

	if (*argc == 1){
   /*---------------------------------------------------
    *  argc = 1 creats standard output filenames named
    *  HYDRO.*
    *---------------------------------------------------*/
		strcpy(commandlinearg[1],DUMMY);
		fprintf(stderr, "\n  Hydrotrend started without given file name. Output will \n");
		fprintf(stderr, "  be written to standard output files, named %s.\n",commandlinearg[1]);
		fprintf(stderr, "  Old files will be overwritten!!\n\n");
	}
	
	if (*argc == 2)
   /*------------------------------------------------
    *  argc = 2 makes it possible to make different
    *  filenames per each run.
    *------------------------------------------------*/
		for( jj=0; jj<MAXLENGTH; jj++ ){
			if (commandlinearg[1][jj] == '%' || commandlinearg[1][jj] == '*' || commandlinearg[1][jj] == '#'
					|| commandlinearg[1][jj] == '@' || commandlinearg[1][jj] == '-' || commandlinearg[1][jj] == '^'
					|| commandlinearg[1][jj] == '"' || commandlinearg[1][jj] == '?' || commandlinearg[1][jj] == '!'
					|| commandlinearg[1][jj] == '.' || commandlinearg[1][jj] == ',' || commandlinearg[1][jj] == '$'){
				fprintf(stderr, "  HydroTrend ERROR: Incorrect command line \n");
				fprintf(stderr, "    You can use only characters and numbers to name the project. \n");
				fprintf(stderr, "Don't use %c.\n",commandlinearg[1][jj]);
				err++;
			}
			commandlinearg[1][jj]=toupper(commandlinearg[1][jj]);
		}
		
	if (*argc == 3){
   /*------------------------------------------------
    *  argc = 3 makes it possible to display all the
    *  subroutines the program goes through
    *------------------------------------------------*/
		for( jj=0; jj<MAXLENGTH; jj++ ){
			if (commandlinearg[1][jj] == '%' || commandlinearg[1][jj] == '*' || commandlinearg[1][jj] == '#'
					|| commandlinearg[1][jj] == '@' || commandlinearg[1][jj] == '-' || commandlinearg[1][jj] == '^'
					|| commandlinearg[1][jj] == '"' || commandlinearg[1][jj] == '?' || commandlinearg[1][jj] == '!'
					|| commandlinearg[1][jj] == '.' || commandlinearg[1][jj] == ',' || commandlinearg[1][jj] == '$'){
				fprintf(stderr, "  HydroTrend ERROR: Incorrect command line \n");
				fprintf(stderr, "    You can use only characters and numbers to name the project. \n");
				fprintf(stderr, "Don't use %c.\n",commandlinearg[1][jj]);
				err++;
			}
			commandlinearg[1][jj]=toupper(commandlinearg[1][jj]);
		}
		for ( jj=0; jj<MAXLENGTH; jj++ ){
			commandlinearg[2][jj]=toupper(commandlinearg[2][jj]);
		}
    	if (strncmp(commandlinearg[2],verbosearg,2) == 0){
    		verbose = 1;
		}
	}
    
	if (*argc >3){
   /*-----------------------------------------------
    *  argc > 4 does not exist. It will handle the
    *  output as if argc = 1.
    *-----------------------------------------------*/
		strcpy(commandlinearg[1],DUMMY);
		fprintf(stderr, "  HydroTrend ERROR: Incorrect command line \n");
		fprintf(stderr, "    argc should equal 1 or 2 \n");
		fprintf(stderr, "    argc = %d \n", *argc);
		fprintf(stderr, "    HydroTrend does not use more than 2 command line arguments. \n");
		fprintf(stderr, "    Ignoring all arguments. \n");
		fprintf(stderr, "    Output will be written to standard output files, named %s.\n",commandlinearg[1]);
	}

	return(err);
} /* HydroCommandLine.c */ 
