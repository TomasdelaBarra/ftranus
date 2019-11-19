##############################################################################
#  Copyright (C) 1980-2011 Modelistica, Caracas, Venezuela
# 
#  This work is licensed under the Creative Commons 
#  "Attribution-ShareAlike 2.0" license
#  http://$(OBJ)/creativecommons.org/licenses/by-sa/2.0/
#  
#  You are free:
#  
#      * to copy, distribute, display, and perform the work
#      * to make derivative works
#      * to make commercial use of the work
#  
#  Under the following conditions:
# 
#     Attribution: You must give the original author credit.
# 
#     Share Alike: If you alter, transform, or build upon this work, 
#     you may distribute the resulting work only under a license identical 
#     to this one.
#  
#     * For any reuse or distribution, you must make clear to others the 
#       license terms of this work.
#     * Any of these conditions can be waived if you get permission from 
#       the copyright holder.
#  
#  For the full legal text see:
#      http://$(OBJ)/creativecommons.org/licenses/by-sa/2.0/legalcode
# 
##############################################################################


# Makefile made to work with GNU Make

.DEFAULT:

BIN=bin
OBJ=obj

vpath= %$(OBJ)/.o    $(OBJ)
vpath= %.mod  $(OBJ)

EXES= pasos   \
      trans   \
      cost    \
      lcal    \
      fluj    \
      loc     \
      eval    \
      impas   \
      imptra  \
      mats    \
      imploc  \
      matesp  \
      dimen

FC=gfortran
LN=gfortran

FCFLAGS=$(USER_FCFLAGS) -g3 -B $(OBJ) -J $(OBJ) @fc_options @fc_warnings

LFLAGS=$(USER_LFLAGS) -static

ifdef LIBRARY_PATH
	LFLAGS:=$(LFLAGS) -L$(LIBRARY_PATH)
endif

ifdef SystemRoot
	dotexe=.exe
endif

ifdef F_PROFILE
	FCFLAGS:=$(FCFLAGS) -pg
    LFLAGS:=$(LFLAGS) -pg
endif

ifdef F_TEST
	FCFLAGS:=-O0 -fbounds-check $(FCFLAGS)
else
	FCFLAGS:=-O3 $(FCFLAGS)
endif

COMPILE=$(FC) $(FCFLAGS) -o $@ -c $< 
LINK=$(LN) $(LFLAGS) -o $@ -Wl,-Map=$@.map $^

%.o : %.f90
	$(COMPILE)

$(OBJ)/%.o : %.f90
	$(COMPILE)


all: prepare $(EXES)

prepare: $(BIN) $(OBJ)

$(BIN):
	mkdir $(BIN)

$(OBJ):
	mkdir $(OBJ)


clean:
	rm -rf $(OBJ) $(BIN)
  
pasos: prepare $(BIN)/pasos$(dotexe)
	@echo $@

$(BIN)/pasos$(dotexe) : $(OBJ)/pasos.o \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
		$(OBJ)/rcomm.o \
		$(OBJ)/pcomm.o \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/heapq.o  \
        $(OBJ)/lists.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/pcomm.o  \
		$(OBJ)/mensamod.o \
		$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o	
	$(LINK)

pasosm: prepare $(BIN)/pasosm$(dotexe)
	@echo $@

$(BIN)/pasosm$(dotexe) : $(OBJ)/pasosm.o \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
		$(OBJ)/rcomm.o \
		$(OBJ)/pcomm.o \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/heapm.o  \
        $(OBJ)/lists.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/pcomm.o  \
		$(OBJ)/mensamod.o \
		$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o	
	$(LINK)

trans : prepare $(BIN)/trans$(dotexe)
	@echo $@

$(BIN)/trans$(dotexe) : $(OBJ)/trans.o \
        $(OBJ)/tcomm.o  \
        $(OBJ)/io_list.o  \
	$(OBJ)/nodes.o \
        $(OBJ)/rcomm.o  \
        $(OBJ)/waiting.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
        $(OBJ)/control.o  \
        $(OBJ)/ipcomm.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/mpowit.o  \
        $(OBJ)/tmath.o  \
        $(OBJ)/rtrans.o  \
        $(OBJ)/debugm.o \
	$(OBJ)/mensamod.o \
	$(OBJ)/distribm.o
	$(LINK)

cost: prepare $(BIN)/cost$(dotexe)
	@echo $@

$(BIN)/cost$(dotexe) : $(OBJ)/cost.o \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/fcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)

fluj: prepare $(BIN)/fluj$(dotexe)
	@echo $@

$(BIN)/fluj$(dotexe) : $(OBJ)/fluj.o \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
		$(OBJ)/rcomm.o \
        $(OBJ)/getoptm.o  \
        $(OBJ)/fcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
		$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
		$(LINK)

lcal: prepare $(BIN)/lcal$(dotexe)
	@echo lcal

$(BIN)/lcal$(dotexe): $(OBJ)/lcal.o \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
		$(OBJ)/mpowit.o \
		$(OBJ)/distribm.o \
        $(OBJ)/lcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/debugm.o
	$(LINK)

loc: prepare $(BIN)/loc$(dotexe)
	@echo loc

$(BIN)/loc$(dotexe) : $(OBJ)/loc.o \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
		$(OBJ)/mpowit.o \
		$(OBJ)/distribm.o \
        $(OBJ)/lcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/exogalloc.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/debugm.o
	$(LINK)

eval: prepare $(BIN)/eval$(dotexe)
	@echo $@

$(BIN)/eval$(dotexe) : $(OBJ)/eval.o \
        $(OBJ)/mensamod.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
	$(OBJ)/optionsm.o \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/debugm.o
	$(LINK)

impas: prepare $(BIN)/impas$(dotexe)
	@echo $@

$(BIN)/impas$(dotexe) : $(OBJ)/impas.o  \
        $(OBJ)/io_list.o  \
	$(OBJ)/nodes.o \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/pcomm.o  \
	$(OBJ)/mensamod.o \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)

imptra: prepare $(BIN)/imptra$(dotexe)
	@echo $@

$(BIN)/imptra$(dotexe) : $(OBJ)/imptra.o \
        $(OBJ)/tindics.o  \
        $(OBJ)/tcomm.o  \
        $(OBJ)/io_list.o  \
	$(OBJ)/nodes.o \
	$(OBJ)/zcomm.o \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/ipcomm.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
	$(OBJ)/mensamod.o \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)

imploc: prepare $(BIN)/imploc$(dotexe)
	@echo $@

$(BIN)/imploc$(dotexe) : $(OBJ)/imploc.o \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
	$(OBJ)/mpowit.o \
	$(OBJ)/distribm.o \
        $(OBJ)/lcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/imploc.o  \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)

mats: prepare $(BIN)/mats$(dotexe)
	@echo $@

$(BIN)/mats$(dotexe) : $(OBJ)/mats.o \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
	$(OBJ)/rcomm.o \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/docout.o  \
	$(OBJ)/mensamod.o \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)

matesp: prepare $(BIN)/matesp$(dotexe)
	@echo $@

$(BIN)/matesp$(dotexe) : $(OBJ)/matesp.o \
        $(OBJ)/tcomm.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/io_list.o  \
	$(OBJ)/nodes.o \
	$(OBJ)/mlogit.o \
	$(OBJ)/mpowit.o \
	$(OBJ)/distribm.o \
        $(OBJ)/mecomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/mpowit.o  \
        $(OBJ)/rtrans.o  \
	$(OBJ)/mensamod.o \
	$(OBJ)/getoptm.o \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)

dimen: prepare $(BIN)/dimen$(dotexe)
	@echo $@

$(BIN)/dimen$(dotexe) : $(OBJ)/dimen.o \
        $(OBJ)/getoptm.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
	$(OBJ)/mensamod.o \
	$(OBJ)/optionsm.o \
        $(OBJ)/debugm.o
	$(LINK)


$(OBJ)/pasos.o : pasos.f90 \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/heapq.o  \
        $(OBJ)/lists.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/pcomm.o  \
        $(OBJ)/debugm.o

$(OBJ)/pasosm.o : pasosm.f90 \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/heapm.o  \
        $(OBJ)/lists.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/pcomm.o  \
        $(OBJ)/debugm.o

$(OBJ)/trans.o : trans.f90 \
        $(OBJ)/tcomm.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/waiting.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
        $(OBJ)/control.o  \
        $(OBJ)/ipcomm.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/mpowit.o  \
        $(OBJ)/tmath.o  \
        $(OBJ)/rtrans.o  \
        $(OBJ)/debugm.o

$(OBJ)/cost.o : cost.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/fcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/debugm.o

$(OBJ)/fluj.o : fluj.f90 \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/fcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/debugm.o

$(OBJ)/lcal.o : lcal.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
        $(OBJ)/lcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/debugm.o

$(OBJ)/loc.o : loc.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
        $(OBJ)/lcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/exogalloc.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/debugm.o

$(OBJ)/eval.o : eval.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/debugm.o

$(OBJ)/impas.o : impas.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/pcomm.o  \
        $(OBJ)/debugm.o

$(OBJ)/imptra.o : imptra.f90 \
	$(OBJ)/tindics.o \
        $(OBJ)/tcomm.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/ipcomm.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/debugm.o

$(OBJ)/tindics.o : tindics.f90 \
        $(OBJ)/tcomm.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/ipcomm.o  \
        $(OBJ)/param.o  \
        $(OBJ)/debugm.o

$(OBJ)/imploc.o : imploc.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/mlogit.o  \
        $(OBJ)/lcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/debugm.o

$(OBJ)/mats.o : mats.f90 \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/ascii.o  \
        $(OBJ)/docout.o  \
        $(OBJ)/debugm.o

$(OBJ)/matesp.o : matesp.f90 \
        $(OBJ)/tcomm.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/mecomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/mpowit.o  \
        $(OBJ)/rtrans.o  \
        $(OBJ)/debugm.o

$(OBJ)/dimen.o : dimen.f90 \
        $(OBJ)/getoptm.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o  \
        $(OBJ)/debugm.o



$(OBJ)/nodes.o : nodes.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/param.o  \
        $(OBJ)/debugm.o
	$(COMPILE)

$(OBJ)/heap.o : heap.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/param.o

$(OBJ)/heapq.o : heapq.f90 \
        $(OBJ)/lists.o  \
        $(OBJ)/debugm.o

$(OBJ)/heapm.o : heapm.f90 \
        $(OBJ)/lists.o  \
        $(OBJ)/debugm.o

$(OBJ)/lists.o : lists.f90 \
        $(OBJ)/debugm.o

$(OBJ)/pcomm.o : pcomm.f90 \
        $(OBJ)/param.o
        
$(OBJ)/ipcomm.o : ipcomm.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o

$(OBJ)/rcomm.o : rcomm.f90 \
        $(OBJ)/nodes.o  \
        $(OBJ)/io_list.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/control.o  \
        $(OBJ)/param.o

$(OBJ)/mlogit.o : mlogit.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/distribm.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/mpowit.o  \
        $(OBJ)/debugm.o

$(OBJ)/mpowit.o : mpowit.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/distribm.o  \
        $(OBJ)/gener.o  \
        debugm.f90

$(OBJ)/distribm.o : distribm.f90

$(OBJ)/tcomm.o : tcomm.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/rcomm.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o

$(OBJ)/tparc.o : tparc.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o
        
$(OBJ)/zcomm.o : zcomm.f90 \
       	$(OBJ)/io_list.o \
        $(OBJ)/mensamod.o  \
        $(OBJ)/debugm.o \
	$(OBJ)/gener.o \
	$(OBJ)/param.o

$(OBJ)/lcomm.o : lcomm.f90 \
        $(OBJ)/io_list.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/mlogit.o  \
        $(OBJ)/control.o  \
        $(OBJ)/param.o  \
        $(OBJ)/lparc.o  \
        $(OBJ)/debugm.o

$(OBJ)/lparc.o : lparc.f90 \
        $(OBJ)/param.o

$(OBJ)/ascii.o : ascii.f90 \
        $(OBJ)/rcomm.o  \
        $(OBJ)/mensamod.o  \
        $(OBJ)/tparc.o  \
        $(OBJ)/zcomm.o  \
        $(OBJ)/control.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o

$(OBJ)/io_list.o : io_list.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/debugm.o

$(OBJ)/control.o : control.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/optionsm.o  \
        $(OBJ)/gener.o  \
        $(OBJ)/param.o

$(OBJ)/optionsm.o : optionsm.f90

$(OBJ)/gener.o : gener.f90 \
        $(OBJ)/mensamod.o  \
        $(OBJ)/getoptm.o  \
        $(OBJ)/param.o 
        
$(OBJ)/getoptm.o : getoptm.f90 \
        $(OBJ)/optionsm.o  \
        $(OBJ)/release.o  \
        $(OBJ)/debugm.o

$(OBJ)/param.o : param.f90 \
	$(OBJ)/release.o
	$(COMPILE)
	
$(OBJ)/mensamod.o : mensamod.f90 \
	$(OBJ)/optionsm.o \
	$(OBJ)/debugm.o
        
$(OBJ)/debugm.o : debugm.f90
	$(COMPILE)

$(OBJ)/release.o : release.f90 
	$(COMPILE)
      
