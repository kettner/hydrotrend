EXTRA_DIST         = bootstrap

bin_PROGRAMS       = hydrotrend
lib_LTLIBRARIES    = libhydrotrend.la

libhydrotrend_la_SOURCES = \
   hydrotrend_api.c \
   hydrotrend_irf.c \
   hydroalloc_mem.c   \
   hydrocalqsnew.c    \
   hydrocheckinput.c   \
   hydroclimate.c      \
   hydrotrend_cli.c	\
   hydroexpdist.c      \
   hydrofree_mem.c    \
   hydroglacial.c      \
   hydrohypsom.c      \
   hydroinputalloc.c	\
   hydromaxevents.c	 \
   hydroopenfiles.c   \
   hydrooutlet.c	  \
   hydrooutput.c      \
   hydroprintannual.c   \
   hydroprintstat.c    \
   hydroprinttable.c   \
   hydrorain.c      \
   hydroran2.c      \
   hydroran2sediment.c		\
   hydrorandom.c      \
   hydrorandomsediment.c	\
   hydroreadclimate.c    \
   hydroreadevaporation.c	\
   hydroreadhypsom.c   \
   hydroreadinput.c   \
   hydrosecurityinputcheck.c  \
   hydrosedload.c  \
   hydrosetgeoparams.c \
   hydrosetglobalpar.c \
   hydrosetparams.c  \
   hydroshoulder.c      \
   hydroshuffle.c      \
   hydrosnow.c      \
   hydrosumflow.c   \
   hydroswap.c   \
   hydroweather.c \
   hydrotimeser.c

#libhydrotrend_a_SOURCES = $(libhydrotrend_la_SOURCES)

hydrotrend_SOURCES = \
   hydrotrend_main.c

hydrotrend_DEPENDENCIES = libhydrotrend.la
hydrotrend_LDADD        = -lhydrotrend

noinst_HEADERS = \
   hydrotrend_irf.h \
   hydroalloc_mem.h \
   hydroclimate.h \
   hydrodaysmonths.h \
   hydroinout.h \
   hydroparams.h \
   hydroreadclimate.h \
   hydroreadevaporation.h	\
   hydrornseeds.h \
   hydrotrend.h \
   hydrotimeser.h \
   hydrofree_mem.h

include_HEADERS = \
   hydrotrend_api.h \
   hydrotrend_cli.h

INCLUDES = -I$(top_srcdir)

LDADD = -lm

