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
MODULE DISTRIBM
USE GENER
USE MENSAMOD
USE DEBUGM

    character(80) :: DISTRIBM_RCS_ID = & 
      "$Id$" 

! Errores reportados por rutina LOGIT
     integer, parameter ::              &
       disterr_GenCostLtZero      = -1,   &
       disterr_ElasticityTooHigh  = -2,   &
       disterr_NegativeCost       = -3,   &
       disterr_GenCostArgGtOne    = -4,   &
       disterr_GenCostGtCMin      = -5,   &
       disterr_NoOptions          = -6

CONTAINS

      SUBROUTINE DistribError(num)
        real(8), intent(in) :: num
        integer :: errno
        errno = num
        write(*,*) 'Code:', abs(errno)
        select case(errno)
          case (disterr_NoOptions)
            call mensa(msg_LogitNoOptions, mensa_Aborta)
          case (disterr_GenCostLtZero)
            call mensa(msg_LogitCostLtZero, mensa_Aborta)
          case (disterr_ElasticityTooHigh)
            call mensa(msg_LogitElastTooHigh, mensa_Aborta)
          case (disterr_NegativeCost)
            call mensa(msg_LogitNegativeCost, mensa_Aborta)
          case (disterr_GenCostArgGtOne)
            call mensa(msg_LogitError, mensa_Aborta)
          case default
            call mensa(msg_LogitError, mensa_Aborta)
        end select
        return
      end subroutine

END MODULE DISTRIBM
