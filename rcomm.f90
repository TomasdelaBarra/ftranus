! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo A๑ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE RCOMM
USE PARAM
USE TPARC
USE CONTROL
USE IO_LIST
USE NODES

    character(80) :: RCOMM_RCS_ID = & 
      "$Id$" 

        integer   NRUTIN
        integer   ITIP
        integer   IROUTE
        INTEGER   RUTPRI, RUTULT
        integer(4) :: CAP
        logical(1) ::  NoStops, TwoWayLink

       integer                &
        NLINK,                & ! Nฃmero total de enlaces
        NLINKRUT                ! Numero totak de enlace-rutas's

       DIMENSION              &
        LINKID(NLMAX),        & ! Identificador externo (GIS) del enlace
        NRUTIN(NLMAX),        & ! Numero de rutas que pasan por cada enlace
        RUTPRI(NLMAX),        & ! Apuntador al primer enlace-ruta
                                ! que pasan por cada enlace
        RUTULT(NLMAX),        & ! Apuntador al ultimo enlace-ruta
                                ! que pasan por cada enlace
        ITIP(NLMAX),          & ! Tipo de cada enlace
        DIS(NLMAX),           & ! Distancia de cada enlace
        CAP(NLMAX),           & ! Capacidad de la via
        VEL(NLMAX),           & ! Velocidad de cada enlace
        IOR(NLMAX),           & ! Origen de cada enlace
        IDES(NLMAX),          & ! Destino de cada enlace
        NOGIR(NLMAX,MXGIR),   & ! Giros prohibidos de un enlace
        turn_delay(NLMAX,MXGIR),& ! Intersection delay, parallel to NOGIR
        TwoWayLink(NLMAX)

       real :: LinkDelay(NLMAX) ! Retraso por semแforos, etc.

       DIMENSION              &
        ILINK(MXLNRUT),       & ! Enlace de un enlace-ruta
        IROUTE(MXLNRUT),      & ! Ruta de un elnace-ruta
        NoStops(MXLNRUT)

       DIMENSION              &
        COSLINK(MXLNRUT),     & ! Costo operaciขn de cada enlace/RCOMMuta
        TARIFA(MXLNRUT),      & ! Tarifa de cada enlace/RCOMMuta
        ESPERA(MXLNRUT),      & ! Espera en el enlace
        VOL(MXLNRUT),         & ! Volumen en unids de demanda por enlace
        VEH(MXLNRUT)            ! Vehกculos por enalce

      DIMENSION               & ! Calculo de costo de los pasos
           CTME(MXLNRUT),     & ! Costos asociados al tiempo de espera
           CTMV(MXLNRUT),     & ! Costos asociados al tiempo de viaje
           TMV(MXLNRUT),      & ! Tiempo de viaje
           LRVEL(MXLNRUT)       ! Velocidad de la ruta en el enlace

      REAL :: &
           LnFlow(NLMAX),     & ! Flujo en un enlace en VehStd
           Congest(NLMAX),    & ! Flujo exedente en un enlace en VehStd
           Queue(NLMAX),      & ! Flujo por cola en un enlace-ruta en VehStd
           QTime(NLMAX),      & ! Tiempo en cola para entrar en el enlace
           Suben(MXLNRUT),    & ! Abordajes en el elnace-ruta 
           Bajan(MXLNRUT)  ! 

! Definiciones para los nodos
      character(32) ::         &
        NOMNOD(5),        &
        NOMNOT(5)
      integer ::              &
        NUMNOT(5),        &
        NUMNOD(5),        &
        XNOD(5),          &
        YNOD(5)
      integer(1) ::            &
        NODTIP,               &
        TIPNOD(5)         
      integer(2) :: NODOS

CONTAINS
!   ษออออออออออออออออออออออออออออออออออป
      SUBROUTINE RDREDANT(IUN, NX, IOX, IDX) 
         integer   IUN
         integer, optional :: NX, IOX(*),IDX(*)
      integer   status
!   ศออออออออออออออออออออออออออออออออออผ
      ! En esta rutina se pueden introducir sopotocientas
      ! modificaciones


         integer    nnogir, ng(MXGIR)
         integer   L,LR, IX, NL,IL
         LOGICAL SALTA
         integer   check, mcheck, check2, mcheck2,irec
!  Si repite, lee todos los datos de T3S(3) y regresa

         status = msg_ReadError
         READ(IUN, ERR=999)NL, mcheck
         NODTIP=0
         NODOS=0
         NLINK=0
         NLINKRUT=0
         NRUTIN=0
         RUTPRI=0
         RUTULT=-1
         NOGIR=0
         Congest  = 0 
         Queue    = 0
         QTime    = 0
         Suben = 0
         LnFlow    = 0
         Espera    = 0
         L = 1
         DO IL=1, NL
             NG = 0
             READ(IUN, ERR=999) &
             irec,                                 &
             IOR(L),IDES(L),                       &
             LINKID(L),                            &
             TwoWayLink(L),ITIP(L),                &
             DIS(L),CAP(L),VEL(L),                 &
             LinkDelay(L),                         &
             Congest(L),    & ! Flujo exedente en un enlace en VehStd
             Queue(L),      & ! Flujo por cola en un enlace-ruta en VehStd
             QTime(L)         ! Tiempo en cola para entrar en el enlace
             ! Buscar si esta en exepciones
             SALTA=.FALSE.
             if (PRESENT(NX)) then
                DO IX=1, NX
                  IF (IOR(L).EQ.IOX(IX).AND.IDES(L).EQ.IDX(IX))THEN
                     SALTA=.TRUE.
                     EXIT
                  ENDIF
                ENDDO
             endif 
             ITIP(L) = IntNum(ITIP(L), NUMTIP, NTIP)

             READ(IUN, ERR=999) &
             NRUTIN(L),mcheck2, &
             (IROUTE(LR),ESPERA(LR), VOL(LR), VEH(LR),NoStops(LR), &
             LRVEL(LR), TMV(LR), &
             Suben(LR),          &   ! Abordajes en el elnace-ruta 
             Bajan(LR),          &
             LR=NLINKRUT+1,NLINKRUT+NRUTIN(L)), &
             check2
             call checkRead(nrutin(l),mcheck2,check2, status)

             turn_delay(l,:) = 0
             READ(IUN, ERR=999)    &
             nnogir,mcheck2,       &
             (NG(I), turn_delay(L,I), I=1, nnogir), &
             check2
             call checkRead(nnogir,mcheck2,check2, status)

             if(SALTA) CYCLE

             NLINK=L
             RUTPRI(L)=NLINKRUT+1
             RUTULT(L)=NLINKRUT+NRUTIN(L)
             DO LR=RUTPRI(L),RUTULT(L)
               ILINK(LR)=L
               IRoute(LR) = IntNum(IRoute(LR),NumRut,NRUTAS)
             ENDDO
             NLINKRUT=NLINKRUT+NRUTIN(L)
             RUTULT(L)=NLINKRUT
             DO I=1,NNOGIR
                NOGIR(L,I)=NG(I)
             ENDDO
             L=L+1
         ENDDO
100    read(iun) check
       call checkRead(NL,mcheck,check, status)

       !call ReadNodesFile(iun)
       status = msg_Ok

999   call CheckStatus(status)
      RETURN ! aqui NLINK deberia ser igual a NL
      END SUBROUTINE

      SUBROUTINE WRREDANT(IUN,status)
        INTEGER IUN, status
      ! En esta rutina se pueden introducir sopotocientas
      ! modificaciones

         INTEGER    L,LR
         integer    nnogir
    !  Si repite, lee todos los datos de T3S(3) y regresa

         status = msg_WriteError
    ! Escribe encabezamiento con numeros de zonas, para GUS
         WRITE(IUN,ERR=999)nlink,-nlink
         DO L=1, NLINK
           nnogir=0
           do i=mxgir,1,-1
             if (nogir(l,i) /= 0) then
                nnogir=i
                exit
             endif
           enddo
           WRITE(IUN,ERR=999) &
           L,                                            &
           IOR(L),IDES(L),                               &
           LINKID(L),                                    &
           TwoWayLink(L),NumTip(ITIP(L)),                &
           DIS(L),CAP(L),VEL(L),                         &
           LinkDelay(L),                                 &
           Congest(L),    & ! Flujo exedente en un enlace en VehStd
           Queue(L),      & ! Flujo por cola en un enlace-ruta en VehStd
           QTime(L)         ! Tiempo en cola para entrar en el enlace

           WRITE(IUN,ERR=999)    &
           NRUTIN(L),-nrutin(l), &
           (NumRut(IROUTE(LR)),  &
           ESPERA(LR), VOL(LR), VEH(LR),NoStops(LR), &
           LRVEL(LR), TMV(LR), &
           Suben(LR),          &   ! Abordajes en el elnace-ruta 
           Bajan(LR),          &
           LR=RUTPRI(L),RUTULT(L)), &
           nrutin(l)

           WRITE(IUN,ERR=999) &
           nnogir,-nnogir, &
           (NOGIR(L,I), turn_delay(L,I), I=1, nnogir), &
           nnogir
         ENDDO
         WRITE(IUN,ERR=999)nlink

         call WriteNodesFile(iun)

      status = msg_Ok
999   RETURN
      END SUBROUTINE

      SUBROUTINE WritePathHeader(iun, mode)
        integer iun, mode
         integer l, lnr

         ! numeros de version
         write(iun) FileMajor, FileMinor, FileRelease, ifmt_PxS

         ! politica
         write(iun) pol, NUMMOD(mode)

         !enlaces ruta
         call WriteListBegin(iun, nlink)
         do l=1, nlink
            call WriteListItem(iun, l)
            write(iun) ior(l), ides(l), &
                       nrutin(l), &
                       (numrut(iroute(lnr)),lnr=rutpri(l),rutult(l))
         enddo
         call WriteListEnd(iun, nlink)
      RETURN
      END SUBROUTINE

      SUBROUTINE SkipPathHeader(iun)
        integer iun
         integer     l,lnr, nl, nr, nada
         integer(2) ::   nada2
         integer     mode
         character(3) cpol

         ! numeros de version
         read(iun) nada2, nada2, nada2, nada

         ! politica
         read(iun) cpol, mode

         !enlaces ruta
         call ReadListBegin(iun, nl)
         do l=1, nl
            call CheckListItem(iun, l)
            read(iun) nada, nada, &
                     nr, &
                     (nada,lnr=1,nr)
         enddo
         call CheckListEnd(iun, nl)
      RETURN
      END SUBROUTINE


      SUBROUTINE CALCKCOST(K)
        integer :: K
        integer :: LnR, iofrom, io
        
        DO LnR=1, NLINKRUT
          L = ILINK(LnR)
          IO = IOPRUT(IROUTE(LnR))
          CTME(LnR) = VOTME(K)*ESPERA(LnR)
          CTMV(LnR) = VOTMV(k)*(TMV(LnR)+LinkDelay(L))*PENLACE(IO,ITIP(L))*CONSTM(IO) &
                      + TARIFA(LnR)
          if (CTMV(LnR) < 0) then
             print *, 'ERROR: NEGATIVE COST'
             print *, 'IOR(L)', IOR(L)
             print *, 'IDES(L)', IDES(L)
             print *, 'NUMRUT(IROUTE(Lnr))', NUMRUT(IROUTE(Lnr))
             print *, 'VOTMV(k)', VOTMV(k)
             print *, 'TMV(LnR)', TMV(LnR)
             print *, 'LinkDelay(L)', LinkDelay(L)
             print *, 'PENLACE(IO,ITIP(L))', PENLACE(IO,ITIP(L))
             print *, 'CONSTM(IO)', CONSTM(IO)
             print *, 'TARIFA(LnR)', TARIFA(LnR)
          endif
        ENDDO
        DO iofrom = 0, NOPER
            DO io = 1, NOPER
               ! Costo de transferencia
               COSMIN(iofrom,io) = OPTARIFF(iofrom,io)+COPMIN(io)*TARCOP(io) &
                                   + OperPathASC(IO) 
            ENDDO
        ENDDO
      RETURN
      END SUBROUTINE

! Calculo de los costos de cada enlace-ruta, COSLINK, ESPERA, TMV,...
      SUBROUTINE LINKCOSTS()
      integer :: LR, IR, L, IO, iofrom
      real    :: sp


      COSLINK=RINF
      TARIFA =RINF
      TMV    =RINF
      CTME   =RINF
      CTMV   =RINF
      COSMIN =RINF

      DO iofrom = 0, NOPER
          DO io = 1, NOPER
             ! Costo de transferencia
             COSMIN(iofrom,io)=OPTARIFF(iofrom,io)+COPMIN(io)*TARCOP(io)
          ENDDO
      ENDDO


      DO LR=1,NLINKRUT
         IR=IROUTE(LR)    ! Identifica la ruta
         L =ILINK(LR)
         IO=IOPRUT(IR)       ! Identifica al operador de la ruta
         K = MODOPER(IO)
         SP=VEL(L)*SPEED(IO,ITIP(L))
         IF(ITIPOP(IO).EQ.4)SP=VELTIP(ITIP(L))*SPEED(IO,ITIP(L))
         IF(SP.LE.CEROMAS)THEN
100         format(' Or',I6,'  Des',I6,'  Tip',I4,'  Op',I4, 'SP', G18.9)
            WRITE(*,100)IOR(L),IDES(L),NUMTIP(ITIP(L)), NUMOP(IO), SP
             CALL MENSA(msg_SpeedIsZero,mensa_Aborta)  ! Hay inconsistencias
         ENDIF
         TIEMPO=DIS(L)/SP
         TMV(LR)=TIEMPO
         LRVEL(LR)=SP
!  Costo de la energกa por unidad de distancia
         ENER=(PAREN1(IO)+PAREN2(IO)*EXP(-PAREN3(IO)*SP))*COSTEN(IO)
!  Costo operaciขn = tiempo + distancia
         COSLINK(LR)=TIEMPO*COPTIE(IO)+ &
                  DIS(L)* (CARGOS(IO,ITIP(L))+COPDIS(IO,ITIP(L))+ENER)
!  Tarifa  = CostoOper*TARCOP + tiempo + distancia (TARCOP ya viene dividido por TOC)
         TARIFA(LR)=COSLINK(LR)*TARCOP(IO)+ &
                  TIEMPO*COSTIEM(IO) + DIS(L)*DREL(IO)
         CTME(LR)=0 ! Hya que caluclar en funcion de un Modo o Categoria
         CTMV(LR)=0
       ENDDO
       RETURN
       END SUBROUTINE

       integer function FindLink(o, d)
          integer :: o, d
          FindLink = 0

          do l = 1, nLink
            if (ior(l) == o .and. ides(l) == d) then
               FindLink = l
               return
            endif
          enddo
          return
       end function

END MODULE RCOMM
