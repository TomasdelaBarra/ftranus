! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE STAT

character(80) STAT_RCS_ID & 
/  "$Id$" /

CONTAINS


! ======================================================================
! NIST Guide to Available Math Software.
! Fullsource for module POICDF from package DATAPAC.
! Retrieved from CAMSUN on Mon Jul 14 16:39:43 1997.
! ======================================================================
! POICDF
      SUBROUTINE POICDF(X,ALAMBA,CDF)
      real X
      double precision ALAMBA, CDF
      
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE AT THE SINGLE PRECISION VALUE X
!              FOR THE POISSON DISTRIBUTION
!              WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = ALAMBA.
!              THE POISSON DISTRIBUTION USED
!              HEREIN HAS MEAN = ALAMBA 
!              AND STANDARD DEVIATION = SQRT(ALAMBA).
!              THIS DISTRIBUTION IS DEFINED FOR
!              ALL DISCRETE NON-NEGATIVE INTEGER  X--X = 0, 1, 2, ... .
!              THIS DISTRIBUTION HAS THE PROBABILITY FUNCTION
!              F(X) = EXP(-ALAMBA) * ALAMBA**X / X!.
!              THE POISSON DISTRIBUTION IS THE
!              DISTRIBUTION OF THE NUMBER OF EVENTS
!              IN THE INTERVAL (0,ALAMBA) WHEN
!              THE WAITING TIME BETWEEN EVENTS
!              IS EXPONENTIALLY DISTRIBUTED
!              WITH MEAN = 1 AND STANDARD DEVIATION = 1.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE 
!                                AT WHICH THE CUMULATIVE DISTRIBUTION 
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE AND
!                                INTEGRAL-VALUED. 
!                     --ALAMBA = THE SINGLE PRECISION VALUE 
!                                OF THE TAIL LENGTH PARAMETER.
!                                ALAMBA SHOULD BE POSITIVE. 
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF
!             FOR THE POISSON DISTRIBUTION
!             WITH TAIL LENGTH PARAMETER = ALAMBA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
!     RESTRICTIONS--X SHOULD BE NON-NEGATIVE AND INTEGRAL-VALUED.
!                 --ALAMBA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF. 
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN. 
!     COMMENT--THE SINGLE PRECISION TAIL LENGTH
!              PARAMETER ALAMBA IS     NOT     RESTRICTED
!              TO ONLY INTEGER VALUES.
!              ALAMBA CAN BE SET TO ANY POSITIVE REAL
!              VALUE--INTEGER OR NON-INTEGER.
!            --NOTE THAT EVEN THOUGH THE INPUT
!              TO THIS CUMULATIVE
!              DISTRIBUTION FUNCTION SUBROUTINE
!              FOR THIS DISCRETE DISTRIBUTION
!              SHOULD (UNDER NORMAL CIRCUMSTANCES) BE A
!              DISCRETE INTEGER VALUE,
!              THE INPUT VARIABLE X IS SINGLE
!              PRECISION IN MODE.
!              X HAS BEEN SPECIFIED AS SINGLE
!              PRECISION SO AS TO CONFORM WITH THE DATAPAC
!              CONVENTION THAT ALL INPUT ****DATA****
!              (AS OPPOSED TO SAMPLE SIZE, FOR EXAMPLE)
!              VARIABLES TO ALL
!              DATAPAC SUBROUTINES ARE SINGLE PRECISION.
!              THIS CONVENTION IS BASED ON THE BELIEF THAT
!              1) A MIXTURE OF MODES (FLOATING POINT
!              VERSUS INTEGER) IS INCONSISTENT AND
!              AN UNNECESSARY COMPLICATION
!              IN A DATA ANALYSIS; AND
!              2) FLOATING POINT MACHINE ARITHMETIC
!              (AS OPPOSED TO INTEGER ARITHMETIC) 
!              IS THE MORE NATURAL MODE FOR DOING 
!              DATA ANALYSIS. 
!     REFERENCES--JOHNSON AND KOTZ, DISCRETE
!                 DISTRIBUTIONS, 1969, PAGES 87-121,
!                 ESPECIALLY PAGE 114, FORMULA 93.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 112.
!               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
!                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5,
!                 AND PAGE 929.
!               --FELLER, AN INTRODUCTION TO PROBABILITY
!                 THEORY AND ITS APPLICATIONS, VOLUME 1,
!                 EDITION 2, 1957, PAGES 146-154. 
!               --COX AND MILLER, THE THEORY OF STOCHASTIC
!                 PROCESSES, 1965, PAGE 7.
!               --GENERAL ELECTRIC COMPANY, TABLES OF THE
!                 INDIVIDUAL AND CUMULATIVE TERMS OF POISSON
!                 DISTRIBUTION, 1962.
!               --OWEN, HANDBOOK OF STATISTICAL
!                 TABLES, 1962, PAGES 259-261.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. !. 20234
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--NOVEMBER  1975. 
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX,PI,CHI,SUM,TERM,AI,DGCDF
      DOUBLE PRECISION DSQRT,DEXP
      DATA PI/3.14159265358979D0/
!
      IPR=6
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(ALAMBA.LE.0.0)GOTO 50 
      IF(X.LT.0.0)GOTO 55
      INTX=X+0.0001 
      FINTX=INTX
      DEL=X-FINTX
      IF(DEL.LT.0.0)DEL=-DEL
      IF(DEL.GT.0.001)GOTO 60
      GOTO 90
   50 WRITE(IPR,15) 
      WRITE(IPR,46)ALAMBA
      CDF=0.0
      RETURN
   55 WRITE(IPR,4)
      WRITE(IPR,46)X
      CDF=0.0
      RETURN
   60 WRITE(IPR,5)
      WRITE(IPR,46)X
   90 CONTINUE
    4 FORMAT('***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE POICDF SUBROUTINE IS NEGATIVE *****')
    5 FORMAT('***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE POICDF SUBROUTINE IS NON-INTEGRAL *****')
   15 FORMAT('***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE POICDF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,'*****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'*****')
!
!-----START POINT-----------------------------------------------------
!
!     EXPRESS THE POISSON CUMULATIVE DISTRIBUTION 
!     FUNCTION IN TERMS OF THE EQUIVALENT CHI-SQUARED
!     CUMULATIVE DISTRIBUTION FUNCTION, 
!     AND THEN EVALUATE THE LATTER.
!
      DX=ALAMBA
      DX=2.0D0*DX
      NU=X+0.0001
      NU=2*(1+NU)
!
  110 CHI=DSQRT(DX) 
      IEVODD=NU-2*(NU/2)
      IF(IEVODD.EQ.0)GOTO 120
!
      SUM=0.0D0
      TERM=1.0/CHI
      IMIN=1
      IMAX=NU-1
      GOTO 130
!
  120 SUM=1.0D0
      TERM=1.0D0
      IMIN=2
      IMAX=NU-2
!
  130 IF(IMIN.GT.IMAX)GOTO 160 
      DO 100 I=IMIN,IMAX,2
      AI=I
      TERM=TERM*(DX/AI)
      SUM=SUM+TERM
  100 CONTINUE
!
  160 SUM=SUM*DEXP(-DX/2.0D0) 
      IF(IEVODD.EQ.0)GOTO 170
      SUM=(DSQRT(2.0D0/PI))*SUM
      SPCHI=CHI
      CALL NORCDF(SPCHI,GCDF) 
      DGCDF=GCDF
      SUM=SUM+2.0D0*(1.0D0-DGCDF)
  170 CDF=SUM
!
      RETURN
      END SUBROUTINE POICDF


      SUBROUTINE NORCDF(X,CDF)
      real X
      real CDF
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)
!              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1. 
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2). 
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN. 
!     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
!                 SERIES 55, 1964, PAGE 932, FORMULA 26.2.17.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. !. 20234
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--JUNE      1972. 
!     UPDATED         --SEPTEMBER 1975. 
!     UPDATED         --NOVEMBER  1975. 
!
!---------------------------------------------------------------------
!
      DATA B1,B2,B3,B4,B5,P/.319381530,-0.356563782,1.781477937,-1.821255978,1.330274429,.2316419/
!
      IPR=6
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS.
!     NO INPUT ARGUMENT ERRORS POSSIBLE 
!     FOR THIS DISTRIBUTION.
!
!-----START POINT-----------------------------------------------------
!
      Z=X 
      IF(X.LT.0.0)Z=-Z
      T=1.0/(1.0+P*Z)
      CDF=1.0-((0.39894228040143  )*EXP(-0.5*Z*Z))*(B1*T+B2*T**2+B3*T**3+B4*T**4+B5*T**5)
      IF(X.LT.0.0)CDF=1.0-CDF 
!
      RETURN
      END SUBROUTINE NORCDF


  ! Interface to NIST POICDF subroutine
  double precision function DPOIDF(K, L)
      integer K
      real L
      real X
      double precision ALAMBA
      double precision CDF

      X = K
      ALAMBA = L
      call POICDF(X, ALAMBA, CDF)
      DPOIDF = CDF
  end function

END MODULE STAT
