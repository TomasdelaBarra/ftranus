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
MODULE MLOGIT
USE GENER
USE MENSAMOD
USE DEBUGM
USE MPOWIT
USE DISTRIBM
USE OPTIONSM

    character(80) :: mlogit_RCS_ID = & 
      "$Id$" 
    
CONTAINS

SUBROUTINE LOGIT(n, distpar,LgScale,a,c,p,cgen,utmin)
       integer, intent(in), value :: n   ! in:  numero de opciones
       real  distpar! in:  parametro de distribucion
       real  LgScale  ! in:  parametro de escalamiento del logit
       real    a(*) ! in:  atractor de cada opcion
       real(8) ::  c(*) ! in:  costo de cada opcion
       real(8) ::  p(*) ! out: probabilidad de cada opcion
       real(8) ::  cgen ! out: costo generalizado
       real(8), optional :: utmin ! costo a ser usado al desescalar
     if (use_powit) then
         call powit(n, distpar, a, c, p, cgen)
     elseif (present(utmin)) then
         call actual_logit(n, distpar, LgScale, a, c, p, cgen, utmin)
     else
         call actual_logit(n, distpar, LgScale, a, c, p, cgen)
     endif
END SUBROUTINE

SUBROUTINE actual_logit(n, distpar,LgScale,a,c,p,cgen,utmin)
       integer n    ! in:  numero de opciones
       real  distpar! in:  parametro de distribucion
       real  LgScale  ! in:  parametro de escalamiento del logit
       real    a(*) ! in:  atractor de cada opcion
       real(8) ::  c(*) ! in:  costo de cada opcion
       real(8) ::  p(*) ! out: probabilidad de cada opcion
       real(8) ::  cgen ! out: costo generalizado
       real(8), optional :: utmin ! costo a ser usado al desescalar
       integer i, nmin
       real(8) ::  cmin, cescal
       real(8) ::  sum, cg, cm, descal

       if(n.le.0) then
          cgen = disterr_NoOptions
          return
       endif
    
       if(distpar.lt.0) call mensa(10007, -1)
       if(LgScale < 0) call mensas('Logit: LgScale < 0', -1)

       if(distpar.eq.0) then
         ! caso trivial 1, elasticidad 0
         ! usuario insensible al costo
         ! J.95.03.28 no esta claro que pasa con a(i).eq.0 en este caso
         cgen = 0.0
         ! probabilidades identicas
         do i=1, n
           p(i) = 1.0/n
         enddo
       elseif(n.eq.1) then
         ! caso trivial 2, una sola opcion
         ! J.95.03.28 no esta claro que pasa con a(1).eq.0 en este caso
         cgen = c(1)
         p(1) = 1.0
       else
         ! busqueda del costo minimo
         if(LgScale.le.0) then
           cmin=1
         else
           cmin = -1 ! No hay minimo
           do i=1,n
             if(cmin.lt.0.or.c(i).lt.cmin) then
               if(a(i).ne.0) then ! no incluir opciones con atractor = 0
                  if (c(i) >= 0) then 
                     cmin=c(i)
                  else
                     cgen = disterr_NegativeCost
                     return
                  endif
               endif
             endif
           enddo
           cmin = cmin
           if(cmin.lt.0) then
             ! alguna desutilidad negativa, o
             ! todos los atractores son cero (poco probable), error
             cgen = disterr_NegativeCost
             return
           endif
         endif
         ! calculo de las exponenciales
         sum=0.0
         if(cmin.le.0.0)then
           cm = 1.0  ! necesario para costos 0 alinicio de Lcal
         else
           cm = cmin
         endif
         cm = cm**LgScale
         do i=1,n
           if (IsInfD(c(i)) .or. a(i) == 0.) then
             p(i) = 0
           else
             cescal=-distpar * c(i)/cm
             p(i) = dexp(cescal)
             sum  = sum + a(i)*p(i)
           endif
         enddo

         if (sum.le.0) then
           nmin = 0
           do i=1, n
             if (c(i) == cmin) then
                nmin = nmin+1
             endif
           enddo
           if(present(utmin)) then
              if(utmin <= 0) stop 'UTMIN <= 0'     
              cgen = utmin
           else
              cgen = cmin
           endif
           do i=1, n
             if (c(i) == cmin) then
                p(i) = 1/nmin
             else
                p(i) = 0
             endif
           enddo
           return
         endif
         ! calculo del costo generalizado
         cg = 0
         do i=1, n
           cg = cg + (1.0 - cg) * p(i)
           ! calculo de las probabilidades
           p(i) = a(i)*p(i)/sum
         enddo
         if (debugging >= dbg_debug) then
           if (cg.lt.0.or.cg.gt.1) then
             do i=1, n
               write(*,'(''*** DIAG Logit  i,c(i),p(i),a(i):'',I2,3(1X,F6.4))')i, c(i),p(i),a(i)
             enddo
             write(*,'(''*** DIAG Logit cg,sum:'',2F8.5)') cg, sum
           endif
         endif
         if (cg.gt.1) then
           cgen = disterr_GenCostArgGtOne
           if (debugging >= dbg_debug) then
             write(*,*) 2, disterr_GenCostArgGtOne, cgen
           endif
           return
         else
           ! Factor para des-escalar
           if (present(utmin)) then
                descal = utmin
           else
                descal = cmin
           endif
           cg = descal * (-log(cg)/distpar)
           if (cg < 0) then
             if (debugging >= dbg_debug) then
               write(*,*) 3, disterr_GenCostLtZero, cg, cmin, descal
             endif
             cgen = disterr_GenCostLtZero
             return
           endif                        
           if (LgScale == 1. .and. cg > descal*1.000001) then
             if (debugging >= dbg_debug) then
                write(*,*) 3, disterr_GenCostGtCMin, cg, cmin, descal
             endif
             cgen = disterr_GenCostGtCMin
             return
           endif                        
           cgen = cg
         endif
       endif
      RETURN
END SUBROUTINE

END MODULE MLOGIT
