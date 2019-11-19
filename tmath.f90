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
MODULE TMATH

    character(80) :: TMATH_RCS_ID = & 
      "#(@)$Id$" 

CONTAINS

      real(8) FUNCTION DSECH(X)
         real(8) :: X
         DSECH = 1/ DCOSH(X)
      END FUNCTION

      real(8) FUNCTION DASECH(X)
         real(8) :: X
         DASECH= DACOSH(1/X)
      END FUNCTION


END MODULE TMATH

