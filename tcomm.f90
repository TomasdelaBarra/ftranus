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
MODULE TCOMM
USE PARAM
USE RCOMM
USE TPARC
USE GENER
USE MENSAMOD
USE IO_LIST

    character(80) :: TCOMM_RCS_ID = & 
      "$Id$" 


!  GLOBALES PARA EL PROGRAMA TRANS, TPAR, MATS Y CALCO

      real(8) :: PROP,P,FF,C,TRIPS,PUTIL,COST,CM
      REAL   IngresosRuta

      DIMENSION                  &
       FF(MXZON),                & ! Viajes por zona
       C(MXZON),                 & ! Costo compuesto por zona
       CM(MXZON),                & ! Costo monetario promedio por zona
       COST(MXZON,MXMOD),        & ! Costo compuesto por zona y modo
       TRIPS(MXZON,MXMOD),       & ! Viajes por zona y modo
       RETORNO(MXZON),           & ! Retornos por zona
       DISTAN(MXPROP),           & ! Distancia promedio por categor¡a
       TOTV(MXPROP,MXMOD),       & ! Viajes totales por categor¡a y modo
       TTIEMV(MXPROP),           & ! Tiempo de viaje total por categor¡a
       TTIEME(MXPROP),           & ! Tiempo de espera total por categor¡a
       TUTIL(MXPROP),            & ! Utilidad total por categor¡a
       COP(MXOPER),              & ! Costo de operaci¢n total por oper
       PK(MXOPER),               & ! Unidades-Km totales por operador
       REPRIMIDA(MXPROP,MXMOD),  & ! Viajes 1a iteraci¢n por cat y modo
       EN(MXOPER),               & ! Consumo energ¡a por operador
       OPING(MXOPER),            & ! Ingresos totales por operador
       VIAJ(MXOPER),             & ! Viajes totales por operador
       ADING(MXADM),             & ! Ingresos totales por administrador
       CMAN(MXADM),              & ! Costo mantenimiento por administr
       P(MXMOD),                 & ! Probabilidad seleccion modal
       PROP(MXPAS,MXMOD),        & ! Proporci¢n de cada paso y modo
       PUTIL(MXPAS,MXMOD),       & ! Desutilidad de cada paso y modo
       TIEMV(MXPAS,MXMOD),       & ! Tiempo de viaje por paso y modo
       TIEME(MXPAS,MXMOD),       & ! Tiempo de espera por paso y modo
       COBRO(MXPAS,MXOPER),      & ! Cobro por paso y operador
       CPROM(MXPROP),            & ! Pago total por categor¡a
       DISTIP(MXTIP),            & ! Km totales de v¡a por tipo arco
       IngresosRuta(MXLNRUT),    & ! Ingresos por enlace-ruta
       Puestos(MXLNRUT),         & ! Puestos vacios en un enlace-ruta
       VOLAnt(MXLNRUT)             ! Volumen por enlace-ruta en iteracion aterior

       integer(2) :: nps(mxmod)
       integer(2) :: ncol(mxpas, mxmod)
       integer   matpas(mxpas,mxarc, mxmod)
       real(4) ::    path_delay(MXPAS)   ! Delay en intersecciones acumulado

       integer(2) :: NPUB(MXPROP)      ! Número de modos públicos a que tiene acceso cada categoría

       real :: VIAJ, TOTV
CONTAINS

!  --------------------------------------------------------
      SUBROUTINE INDIC(ITER1,IAN,MES,IDIA,IHR,MINS,CONV,CONV2, &
        MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES)
!  --------------------------------------------------------
       integer :: IAN,MES,IDIA
       integer :: IHR,MINS
       integer :: status
       integer :: l

!  GRABA INDICADORES DE EVALUACION

      CALL WRTPAR(3,status,ITER1,IAN,MES,IDIA,IHR,MINS)
      call CheckStatus(status)
      WRITE(3)CONV,CONV2,MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES

!  Viajes totales por categor¡a y modo

      call WriteListBegin(3, NPROP)
      do ip=1, NPROP
         call WriteListItem(3, ip)
         call WriteListBegin(3, NTM)
         do k=1, NTM
            call WriteListItem(3, k)
            WRITE(3) TOTV(IP,K),REPRIMIDA(IP,K)
         enddo
         call WriteListEnd(3, NTM)
      enddo
      call WriteListEnd(3, NPROP)

!  Distancia, costo, tiempos y utilidad x categor¡a

      call WriteListBegin(3, NPROP)
      do ip=1, NPROP
         call WriteListItem(3, ip)
         write(3) DISTAN(IP),CPROM(IP),TTIEMV(IP),TTIEME(IP),TUTIL(IP)
      enddo
      call WriteListEnd(3, NPROP)

!  Viajes, unids-km, energ, costos e ingresos x opererador

      call WriteListBegin(3, NOPER)
      do ip=1, NOPER
         call WriteListItem(3, ip)
         write(3) VIAJ(IP),PK(IP),EN(IP),COP(IP),OPING(IP)
      enddo
      call WriteListEnd(3, NOPER)

!  kilometros de v¡a por tipo

      distip=0
      do l=1,nlink
        distip(itip(l))=distip(itip(l))+dis(l)
      enddo

      call WriteListBegin(3, NTIP)
      do k=1, NTIP
         call WriteListItem(3, k)
         write(3) DISTIP(K)
      enddo
      call WriteListEnd(3, NTIP)

! Contabilidad por administrador (costo e ingreso)

      call WriteListBegin(3, ADMIN)
      do k=1, ADMIN
         call WriteListItem(3, k)
         write(3) CMAN(K),ADING(K)
      enddo
      call WriteListEnd(3, ADMIN)

      END SUBROUTINE

!  --------------------------------------------------------
      SUBROUTINE ReadT2S(IUN,ITER1,IAN,MES,IDIA,IHR,MINS,CONV,CONV2, &
        MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES)
!  --------------------------------------------------------
       integer :: IUN
       integer(2) :: IAN,MES,IDIA
       integer(2) :: IHR,MINS
       integer :: status
       integer :: l

!  GRABA INDICADORES DE EVALUACION

      CALL RDTPAR(IUN,status,ITER1,IAN,MES,IDIA,IHR,MINS)
      call CheckStatus(status)
      READ(IUN)CONV,CONV2,MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES

!  Viajes totales por categor¡a y modo

      call ReadListBegin(IUN, NPROP)
      do ip=1, NPROP
         call CheckListItem(IUN, ip)
         call ReadListBegin(IUN, NTM)
         do k=1, NTM
            call CheckListItem(IUN, k)
            READ(IUN) TOTV(IP,K),REPRIMIDA(IP,K)
         enddo
         call CheckListEnd(IUN, NTM)
      enddo
      call CheckListEnd(IUN, NPROP)

!  Distancia, costo, tiempos y utilidad x categor¡a

      call ReadListBegin(IUN, NPROP)
      do ip=1, NPROP
         call CheckListItem(IUN, ip)
         READ(IUN) DISTAN(IP),CPROM(IP),TTIEMV(IP),TTIEME(IP),TUTIL(IP)
      enddo
      call CheckListEnd(IUN, NPROP)

!  Viajes, unids-km, energ, costos e ingresos x opererador

      call ReadListBegin(IUN, NOPER)
      do ip=1, NOPER
         call CheckListItem(IUN, ip)
         READ(IUN) VIAJ(IP),PK(IP),EN(IP),COP(IP),OPING(IP)
      enddo
      call CheckListEnd(IUN, NOPER)

!  kilometros de v¡a por tipo

      distip=0
      do l=1,nlink
        distip(itip(l))=distip(itip(l))+dis(l)
      enddo

      call ReadListBegin(IUN, NTIP)
      do k=1, NTIP
         call CheckListItem(IUN, k)
         READ(IUN) DISTIP(K)
      enddo
      call CheckListEnd(IUN, NTIP)

! Contabilidad por administrador (costo e ingreso)

      call ReadListBegin(IUN, ADMIN)
      do k=1, ADMIN
         call CheckListItem(IUN, k)
         READ(IUN) CMAN(K),ADING(K)
      enddo
      call CheckListEnd(IUN, ADMIN)

      END SUBROUTINE
      
END MODULE TCOMM
