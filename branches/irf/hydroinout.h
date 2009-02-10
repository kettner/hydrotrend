/*-------------------------------------------------------------------------------------------
 *  hydroinout.h
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Contains fid's, filenames and the header string.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 * ffnameq[]            hydroinout.h    char    -       char. array to set filename + filepath
 * ffnameqs[]           hydroinout.h    char    -       char. array to set filename + filepath
 * ffnametrend1[]       hydroinout.h    char    -       char. array to set filename + filepath
 * ffnametrend2[]       hydroinout.h    char    -       char. array to set filename + filepath
 * ffnametrend3[]       hydroinout.h    char    -       char. array to set filename + filepath
 * ffnamedis[]          hydroinout.h    char    -       char. array to set filename + filepath
 * ffnamelog[]          hydroinout.h    char    -       char. array to set filename + filepath
 * ffidasc[]            hydroinout.h    char    -       char. array to set filename + filepath
 * ffidasc1[]           hydroinout.h    char    -       char. array to set filename + filepath
 * ffidasc2[]           hydroinout.h    char    -       char. array to set filename + filepath
 * ffidasc3[]           hydroinout.h    char    -       char. array to set filename + filepath
 * ffidasc4[]           hydroinout.h    char    -       char. array to set filename + filepath
 * fiddis		hydroinout.h	FILE	-	binary discharge and sedload file id
 * fidhyps		hydroinout.h	FILE	-	input hypsometric integral file id
 * fidinput		hydroinout.h	FILE	-	input data file id
 * fidlog		hydroinout.h	FILE	-	log file id
 * fidtrend1    	hydroinout.h	FILE	-	annual trend file #1 id
 * fidtrend2    	hydroinout.h	FILE	-	annual trend file #2 id
 * fidtrend3	        hydroinout.h	FILE	-	annual trend file #3 id
 * outp                 hydroinout.h    FILE    -       output daily vel, wid en dep file id
 * outp1                hydroinout.h    FILE    -       output daily Q file id
 * outp2                hydroinout.h    FILE    -       output daily Qs average file id
 * outp3                hydroinout.h    FILE    -       output daily Qb average file id
 * outp4                hydroinout.h    FILE    -       output daily Cs file id
 * fnamedis		hydroinout.h	define  -	binary discharge and sedload file name
 * fnamehyps	        hydroinout.h	define  -	input hypsometric integral file name
 * fnameinput	        hydroinout.h	define  -	input data file name
 * fnamelog		hydroinout.h	define  -	log file name
 * fnametrend1	        hydroinout.h	define  -	annual trend file #1 name
 * fnametrend2	        hydroinout.h	define  -	annual trend file #2 name
 * fnametrend3	        hydroinout.h	define  -	annual trend file #3 name
 * fidasc               hydroinout.h    define  -       ascii file   name for daily vel, wid en dep
 * fidasc1              hydroinout.h    define  -       ascii file 1 name for daily Q
 * fidasc2              hydroinout.h    define  -       ascii file 5 name for daily Qs average
 * fidasc3              hydroinout.h    define  -       ascii file 3 name for daily Qb average
 * fidasc4              hydroinout.h    define  -       ascii file 4 name for daily Cs
 * MAXCH                hydroinout.h    define  -       maximum # of char. to set file name + path
 * moname[][]	        hydroinout.h	char	-	month name
 * title[]	        hydroinout.h	char	-	user specified text identifier
 *-------------------------------------------------------------------------------------------*/

#define	fnameinput			"HYDRO_INPUT/HYDRO.IN"
#define	fnametrend1			".TRN1"
#define	fnametrend2			".TRN2"
#define	fnametrend3			".TRN3"
#define	fnamestat			".STAT"
#define	fnamedis			".DIS"
#define	fnameconvdis		".CONVDIS"
#define	fnamehyps			"HYDRO_INPUT/HYDRO"
#define fnamehypsext		".HYPS"
#define	fnamelog			".LOG"
#define	fnamelapserate		"HYDRO_PROGRAM_FILES/HYDRO_LAPSERATE.LUT"
#define	fnameinputgw_r		"HYDRO_INPUT/HYDRO.CLIMATE"
#define	fidasc				"ASCII.VWD"
#define	fidasc1				"ASCII.Q"
#define	fidasc2				"ASCII.QS"
#define	fidasc3				"ASCII.QB"
#define	fidasc4				"ASCII.CS"
#define	MAXCH				(300)

FILE *fidinput;
FILE *fidtrend1;
FILE *fidtrend2;
FILE *fidtrend3;
FILE *fidstat;
FILE *fiddistot;
FILE **fiddis;
FILE *fidconvdistot;
FILE **fidconvdis;
FILE *fidinputgw_r;
FILE **fidhyps;
FILE *fidlog;
FILE *fidlapserate;
FILE *outp, *outp1, *outp2, *outp3, *outp4;

char title[MAXCH];
char moname[12][4];
char ffnametrend1[MAXCH];
char ffnametrend2[MAXCH];
char ffnametrend3[MAXCH];
char ffnamestat[MAXCH];
char ffnamedis[MAXCH];
char ffnamedistot[MAXCH];
char ffnameconvdis[MAXCH];
char ffnameconvdistot[MAXCH];
char ffnamehyps[MAXCH];
char ffnameinputgw_r[MAXCH];
char ffnamelog[MAXCH];
char ffidasc[MAXCH];
char ffidasc1[MAXCH];
char ffidasc2[MAXCH];
char ffidasc3[MAXCH];
char ffidasc4[MAXCH];
