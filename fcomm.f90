! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE FCOMM
USE PARAM
USE TPARC
USE GENER

      character(80) :: FCOMM_RCS_ID = & 
        "$Id$" 


      logical(1) :: &
            XOR(MXZON,MXPROP),     & !  Or¡genes/prop¢sitos con xviajes
            XORM(MXZON,MXPROP),    & !  Or¡genes/prop¢sitos con xviamod
            TIPO(MXPROP,MXSEC),    & ! .T.=flujo normal .F.= habitual
            LFLU(MXSEC)              !  indica sectores s-e que generan flujos

      real(8) :: &
            FLUTRA(MXZON,MXZON)      !  Flujos x categ transporte
      real :: &
            FLUSEC(MXZON,MXZON),   & !  Flujos x categ s-econ¢mica
            XVIAJES(MXZON,MXZON),  & !  Viajes xgenos CatTrans
            XVIAMOD(MXZON,MXZON,MXMOD),  & !  Viajes xgenos CatTrans y modo
            FTIEMP(MXPROP,MXSEC),  & !  TiempoTransp/TiempoUsoSuelo
            FVOLUM(MXPROP,MXSEC),  & !  UnidadesTransp/UnidadesUsoSuelo
            PROUT(MXPROP,MXSEC),   & !  Proporci¢n de la cat s-e en sentido LOC
            PROIN(MXPROP,MXSEC),   & !  Proporci¢n de la cat s-e en sentido contrario
            COSIN1,COSIN2,         & !  Par metros de costo interno
            ZonCosIn(MXZON)          !  Parámetro de costo interno para cada zona

     integer :: &
            NUMSEC(MXSEC),         & !  No externo de cada SectSE
            NFLU,                  & !  Numero de sectores que generan flujos
            NS                       !  No SectSE

     character(32) :: NOMSEC(MXSEC)  !  Nombre de cada SectSE


CONTAINS
      SUBROUTINE LEEF1E(HAYFLUJOS)
!     =================
      LOGICAL HAYFLUJOS
      LOGICAL HayConversion

! LECTURA DE PARAMETROS DEL ARCHIVO F1E

! REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!   MENSA  (Subr)   Emite los mensajes
!   INTNUM (Func)   Busca el n£mero interno de una categor¡a

! INICIALIZACION DE LAS VARIABLES

      PROUT=RCERO
      PROIN=RCERO

! SECCION 1.0 - FORMACION DE CATEGORIAS DE TRANSPORTE A PARTIR DE S-ECS
! TIPO   Indica si es un flujo normal (.T.) o si es habitual (.F.)
! FTIEMP Tiempo transporte/Tiempo uso del suelo
! FVOLUM Unidades transporte/Unidades uso del suelo
! PROUT  es la proporci¢n de la cat s-e en el sentido que viene de LOC
! PROIN  es la proporci¢n de la cat s-e en sentido contrario


      READ(3,'(////)',END=999)
      HayConversion=.FALSE.
100   READ(3,*,END=999,ERR=101,IOSTAT=IOS)I1,J1,IP,P1,P2,P3,P4
      I=INTNUM(I1,NUMCAT,NPROP)
      IF(I.GT.NPROP)THEN
         WRITE(*,*)'F1E(1.0) - Cat:',I1,NPROP
         CALL MENSA(6,-1)
      ENDIF
      J=INTNUM(J1,NUMSEC,NS)
      IF(J.GT.NS)THEN
         WRITE(*,*)'F1E(1.0) - Sect:',J1,NS
         CALL MENSA(6,-1)
      ENDIF
      IF(.NOT.LFLU(J))THEN
        WRITE(*,'(6H  Sect,I6)')NUMSEC(J)
        CALL MENSA(2007,-1)
      ENDIF
      TIPO(I,J)=.TRUE.
      IF(IP.NE.ICERO)TIPO(I,J)=.FALSE.
      FTIEMP(I,J)=P1
      FVOLUM(I,J)=P2
      PROUT(I,J)=P3
      PROIN(I,J)=P4
      HayConversion=.TRUE.
      GO TO 100
101   CALL CHECK(1.0,IOS,'F1E')
      if(lxor(HayFlujos, HayConversion)) then
        call mensa( 2005, mensa_Aborta)
      endif



!  SECCION 2.0 - Par metros de costo interno (Cosin1 y Cosin2)

      READ(3,'(/)',END=999)
      do while(.true.)
         READ(3,*,END=999,ERR=201,IOSTAT=IOS)IZ, C1
         if (C1 < 0 .or. C1 > 1) then
            WRITE(*,*)'F1E(2.0) - :',IZ, C1
            CALL MENSA(10017,-1)
         endif
         if (IZ == 0) then 
            COSIN1 = C1
            COSIN2 =  0 !!!!!
            ! All zones have the same internal cost factor by default
            ZonCosIn(1:NZ1) = COSIN1
         else
            izi = iFindNum(IZ, NumZon, NZ1)
            if (izi <= 0) then
               WRITE(*,*)'F1E(2.0) - :',IZ, C1
               CALL MENSA(10016,-1)
            endif
            ZonCosIn(izi) = C1
         endif
      enddo
201   CALL CHECK(2.0,IOS,'F1E')

      RETURN
999   CALL MENSA(2002,-1)
      END SUBROUTINE
      
END MODULE FCOMM
