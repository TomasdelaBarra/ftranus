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
MODULE MPOWIT
USE GENER
USE MENSAMOD
USE DEBUGM
USE DISTRIBM

    character(80) :: MPOWIT_RCS_ID = & 
      "$Id$" 

CONTAINS

  SUBROUTINE POWIT(n, distpar,a,c,p,cgen)
       integer n    ! in:  numero de opciones
       real  distpar! in:  parametro de distribucion
       real    a(*) ! in:  atractor de cada opcion
       real(8) ::  c(*) ! in:  costo de cada opcion
       real(8) ::  p(*) ! out: probabilidad de cada opcion
       real(8) ::  cgen ! out: costo generalizado
       integer i
       real(8) ::  ci,total
       integer numopts

       if(n <= 0) then
          cgen = disterr_NoOptions
          return
       endif

       if(distpar < 0) call mensa(10007, -1)

       numpots = 0
       do i=1, n
         if (a(i) /= 0) then
           numopts = numopts + 1
         endif
       enddo

       if(distpar == 0) then
         cgen = 0.0
         ! probabilidades identicas
         do i=1, n
           if(a(i) /= 0.0) then
             p(i) = 1.0/numopts
           else
             p(i) = 0.0
           endif
         enddo
       elseif(n.eq.1) then
         ! no esta claro que pasa con a(1).eq.0 en este caso
         cgen = c(1)
         p(1) = 1.0
       else
         total=0.0
         do i=1,n
           if (a(i) == 0 .or. IsInfD(c(i))) then
             p(i) = 0
           else
             ci = c(i)
             if (ci == 0.0) ci = 1e-20
             p(i) = (ci/a(i))**(-distpar)
             total  = total + p(i)
           endif
         enddo

         cgen = total**(-1.0/distpar)
         do i=1, n
           if (a(i) == 0) then
             p(i) = 0
           else
             p(i) = p(i)/total
           endif
         enddo
       endif
      RETURN
  END SUBROUTINE

END MODULE MPOWIT

