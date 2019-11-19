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
MODULE RTRANS
USE PARAM
USE GENER
USE ZCOMM
USE TPARC
USE RCOMM
USE TCOMM
USE DISTRIBM
USE MLOGIT

    character(80) :: RTRANS_RCS_ID = & 
      "$Id$" 
    
CONTAINS
      SUBROUTINE ITERACJ(IP,I,IUNFLUJ,IUNPAS,iuncosts,lconv,ASIG,SoloCostos)
       INTEGER   IP,I, IUNFLUJ, IUNPAS(*)
       LOGICAL    LCONV, SoloCostos

      REAL XVIAJES(MXZON),XVIAMOD(MXZON,MXMOD)

      double precision :: MDELTA, CDELTA, XDELTA, GENER
      integer :: j
      real(4) ::  NP

!  Lee los flujos y viajes exogenos de F1S, excepto si SoloCostos


      IF(.NOT.SoloCostos)THEN
         FF=0
         RETORNO=0
         XVIAJES=0
         XVIAMOD=0
         READ(IUNFLUJ) &
              (FF(J),XVIAJES(J),RETORNO(J), &
               (XVIAMOD(J,K), K=1, NTM),    &
              J=1,NZN)
      ENDIF

!  Iteraciones re zona de destino J

      DO J=1,NZN
        C(J)=RCERO             ! Desutilidad
        CM(J)=RCERO            ! Costo monetario
        DO K=1,NTM
          TRIPS(J,K)=RCERO     ! Viajes por modo
          COST(J,K)=RCERO      ! Desutilidad por modo
        ENDDO

!  Calculo de costos

      IF(I.EQ.J.OR.JER1(J).NE.JER2(J)) CYCLE
      CALL COSTOS(iunpas, IP,I,J,lconv)

!  Generacion elastica

      MDELTA = DELTA(IP)
      CDELTA = MDELTA/150.0
      XDELTA = 0.  ! CDELTA/10.0 propone Juanca
      GENER=GENMIN(IP)*EXP(-CDELTA*C(J))+GENMAX(IP)*EXP(-MDELTA*C(J))
      CM(J)=CM(J)*GENER
      IF(SoloCostos)CYCLE
      FF(J)=FF(J)*GENER+XVIAJES(J)*EXP(-XDELTA*C(J))
      RETORNO(J)=RETORNO(J)*GENER

!  Separacion modal

      TOTV(IP,K) = 0
      DO K=1,NTM
         NP=0
         IF(MODO(IP,K))THEN
            if(LPUB(K))then
               if (NPUB(ip) > 0) then
                   NP = 1./NPUB(ip)
               else
                   NP = 0
               endif
            endif
            PROBK=(P(K)*PCAU(IP)+(1.-PCAU(IP))*NP)     ! Probab modo k
            TRIPS(J,K)=FF(J)*PROBK                     ! Demanda modo k
            TRIPS(J,K)=TRIPS(J,K)+XVIAMOD(J,K)         ! + viajes exogen
            TOTV(IP,K)=TOTV(IP,K)+TRIPS(J,K)           ! viajes totales
            TUTIL(IP)=TUTIL(IP)+TRIPS(J,K)*COST(J,K)   ! desutilidad total
         ENDIF
      ENDDO

!  Asignacion
      if (lconv) then
         CALL ASIG(IP,I,J,iuncosts)
      else
         CALL ASIG(IP,I,J,0)
      endif

      ENDDO  ! Zonas de destino J
      END SUBROUTINE


      SUBROUTINE CALCIPCOST(IP)
      INTEGER IP
!     =========================
! Calula los costos de un enlace-ruta en funcion de los valores del tiempo
! de una Categoria
     integer :: L, LnR, iofrom, io

!  Llama a la rutina LINKCOSTS que precalcula el costo de cada enlace
      CALL LINKCOSTS()

      DO LnR=1,NLINKRUT
         L = ILINK(LnR)
         IO = IOPRUT(IROUTE(LnR))

         TARIFA(LnR) = CatTariffFactor(ip, io) * TARIFA(LnR)

         CTME(LnR) = VOTE(IP)*ESPERA(LnR)
         CTMV(LnR) = VOTV(IP)*(TMV(LnR)+LinkDelay(L))*PENLACE(IO, ITIP(L))*CONSTM(IO)
         CTMV(LnR) = CTMV(LnR) * CatPenalFactor(ip, io)
      ENDDO

      DO iofrom = 1, NOPER
          DO io = 1, NOPER
             ! Costo de transferencia
             COSMIN(iofrom,io)=CatTariffFactor(ip, io) * COSMIN(iofrom, io)
          ENDDO
      ENDDO

      RETURN
      END SUBROUTINE

!  Lee los pasos entre i y j por el modo k
      subroutine LeePaso(iunpas, k, ioverl)
       integer   iunpas, k
       integer(1) ::  ioverl(MXPAS,MXARC)

       read(iunpas) &
       nps(k), &
       (  &
          path_delay(l), &
          ncol(l,k), &
          ( matpas(l,ic, k), &
            ioverl(l,ic), &
          ic=1,ncol(l,k)), &
       l=1,nps(k))
       return
       END SUBROUTINE


!   --------------------------------------------
      SUBROUTINE COSTOS(iunpas, IP,I,J, lconv)
!   --------------------------------------------
      integer iunpas(*), ip, i, j
      logical lconv
      integer(1) ::  ioverl(MXPAS,MXARC)
      REAL CMPASO(MXPAS),CMOD(MXMOD)
      REAL UTIMOD,atracp(MXPAS),atracm(MXMOD)
      real(8) :: COSK(MXMOD), UTILOV(MXPAS), UtilNorm(MXPAS)
      real(8) :: probab(MXPAS), UTMIN
      real(8) :: cosNorm, cosOvr, LinkCos, CMP,TTE,TTV
      real(8) :: costoCat
      logical :: OperSeen(NOPER)
      integer :: nps4

      I=I  ! Para eliminar WARNING del compilador

      atracp=1.
      atracm=1.

!  CMPASO es el costo monetario de cada paso

!  Iteraciones respecto a modo

      COBRO    = 0
      UTIMOD   = RINF
      do K=1,NTM
      COSK(K)=RINF

!  Si la categoria no puede utilizar el modo busca otro modo
      if(.not.MODO(IP,K))then
        atracm(k) = 0
        continue
      endif

      call LeePaso(iunpas(k), k, ioverl)

!  Iteraciones respecto a cada paso
      UTMIN  = RINF
      DO 42 L=1,nps(k)

      IRANT  = 0    ! Ruta anterior en el paso
      TTE    = 0
      TTV    = 0
      CMP    = 0
      cosOvr = 0
      cosNorm= 0
      OperSeen = .FALSE.

! Recorre cada enlace/ruta del paso
      DO K1=1,ncol(l,k)
         LnR=matpas(l,k1,k)   ! Arco del paso
         LnRa = ABS(LnR)
         KARCO=ILINK(LnRa)
         IR   =IROUTE(LnRa)
         IO=IOPRUT(IR)               ! Operador al que pertenece
         ctransf = 0
         IF(IRANT.NE.IR.OR.LnR.LT.ICERO)THEN ! trasbordo
            IF(IRANT.EQ.0)THEN
               IOA=IO ! operador anterior se hace IO en el primer abordaje
            ELSE
               IOA=IOPRUT(IRANT) !operador de ruta anterior
            ENDIF
            ctransf=cosmin(ioa,io)+ctme(lnRa)

            if (.not. OperSeen(io)) then
               ctransf = ctransf + OpCatASC(ip, io)
               OperSeen(io) = .TRUE.
            endif
         ENDIF ! fin de trasbordo o primer abordaje

         LinkCos = ctransf                                             ! Costos de transferencia
         LinkCos = LinkCos + QTime(Abs(KARCO))*VOTV(IP)                ! Tiempo en cola
         cosNorm = cosNorm + LinkCos + tarifa(lnra)+ctmv(LnRa)         ! acumula desutilidad 
         cosOvr  = cosOvr + LinkCos + (tarifa(lnra)+ctmv(LnRa))*ioverl(L,K1)  ! Overlapping
         if (lconv) then
            IF(IRANT.NE.IR.OR.LnR.LT.ICERO)THEN ! trasbordo
              TTE=TTE+ESPERA(LnRa) !acumula tiempo de espera por paso
              CMP=CMP+COSMIN(IOA,IO) ! acumula costo monetario del paso
              COBRO(L,IO)=COBRO(L,IO)+COSMIN(IOA,IO) ! indicador
            endif
            TTV=TTV+TMV(LnRa) ! acumula tiempo de viaje en el paso
            CMP=CMP+TARIFA(LnRa) ! agrega tarifas por enlace al costo
            COBRO(L,IO)=COBRO(L,IO)+TARIFA(LnRa)
         endif
         IRANT=IR
      ENDDO   ! Fin do enlaces K1 del paso L

!  Desutilidad del paso (costo generalizado) con overlapping
!  más delays al costo del tiempo de viaje
      cosNorm  = cosNorm  + path_delay(l) * votv(ip)
      UTILOV(L)= cosOvr   + path_delay(l) * votv(ip)
      UtilNorm(L) = cosNorm
      if (cosNorm < UTMIN) then
         UTMIN = cosNorm
      endif
      if (lconv) then
         CMPASO(L)=CMP ! Costo monetario del paso L al usuario
         TIEME(L,K)=TTE ! indicador
         TIEMV(L,K)=TTV ! indicador
      endif

42    CONTINUE   ! Fin do pasos L

      nps4 = nps(k)
      CALL LOGIT(nps4,lamas(ip),PATHLgSc(ip), &
                       atracp,utilov,probab,cosk(k),UTMIN)

      if (cosk(k).lt.0) then
        ! ERROR
        write(*,*)
        write(*,'('' Path Selection: Cat:'', I4, '' Orig:'',I4,'' Dest:'',I4)') &
           numcat(ip),numzon(i), numzon(j)
        if (debugging >= dbg_debug) then
          write(*,*) nps(k),'c=', utilov(1:nps(k))
        endif
        call DistribError(cosk(k))
      endif
      COST(J,K)=COSK(K)

      ! Sumar Alternative Specific Constant aquí
      ! La ASC es parte del cómputo del logit, pero no de la desutilidad del modo
      COSK(K) = COSK(K) + MODE_ASC(K)

      CMOD(K)=0
      do l=1, nps(k)
         prop(l,k) = probab(l)
         putil(l,k) = utilov(l)
      enddo
      if (lconv) then
         do l=1, nps(k)
            CMOD(K)=CMOD(K)+CMPASO(L)*PROP(L,K)
         enddo
      endif
      enddo  ! Fin do modo k


!  Iteraciones r/modo para probabilidades (solo modos dispon a categ IP)
!  P(K) es la probabilidad
!  CM(J) es el costo monetario de I a J para el usuario

      CALL LOGIT( NTM, lamb(ip), ModeLgSc(ip), atracm, cosk, p, costoCat)
      c(j) = costoCat

      if (c(j).lt.0) then
        ! ERROR
        write(*,*)
        write(*,'('' Mode Selection: Cat:'', I4, '' Orig:'',I4,'' Dest:'',I4)') &
           numcat(ip), numzon(i), numzon(j) 
        call DistribError(c(j))
      endif

      if(c(j).lt.0.0) then
         write(*,*)
         write(*,'(3H Or,I4,4H Des,I4,4H Cat, I2)') i,j,ip
         call mensa(msg_DesAgNegativa, mensa_Aborta)
      endif

      if(lconv) then
        CM(J)= 0
        DO K=1,NTM
           IF(MODO(IP,K))THEN
              CM(J)=CM(J)+CMOD(K)*P(K)
           ENDIF
        ENDDO
      endif

      RETURN
      END SUBROUTINE


!  Calcula el overlapping
      SUBROUTINE OVERLAPPING(k, ioverl)
      integer(1) ::  ioverl(MXPAS, MXARC)

      integer ::  INX, JNX, L, L1, NP, NPABS

         ioverl= 1
         DO L=1, nps(k)-1
            DO INX= 1, ncol(l,k)
               NP=ABS(MATPAS(L,INX, k))
               DO L1=l+1, nps(k)
                  DO JNX=1, NCOL(L1,k)
                     NPABS=ABS(MATPAS(L1, JNX, k))
                     IF(NPABS.EQ.NP)THEN
                        ioverl(L,INX)=ioverl(L,INX)+1
                        ioverl(L1,JNX)=ioverl(L1,JNX)+1
                        EXIT
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      RETURN
      END SUBROUTINE

      SUBROUTINE SaveZoneCosts(iuncosts, ip, i, j, cost, vol,retorno)
        integer, intent(in) :: iuncosts, ip, i, j
        real(8), intent(in) :: cost, vol
        real(4), intent(in) :: retorno

          if (iuncosts == 0) RETURN

          write(iuncosts) 'Z',int2(ip), int2(i), int2(j),REAL(cost), REAL(vol), REAL(retorno)
100       format(A1,1X,3I8,2(8X),G12.4,G12.2, ' /')
      END SUBROUTINE

      SUBROUTINE SaveModeCosts(iuncosts, ip, i, j, k,cost, vol, prob)
        integer, intent(in) :: iuncosts, ip, i, j, k
        real(8), intent(in) :: cost, vol, prob

          if (iuncosts == 0) RETURN

          write(iuncosts) 'M',int2(k), REAL(cost), REAL(vol), prob 
100       format(A1,1X,4I8,8X,G12.4,3G12.2,G18.9, ' /')
      END SUBROUTINE

      SUBROUTINE SavePathCosts(iuncosts, ip, i, j, k, l, cost, vol, prob)
        integer, intent(in) :: iuncosts, ip, i, j, k, l
        real(8), intent(in) :: cost, vol, prob

          if (iuncosts == 0) RETURN

          write(iuncosts) 'P',int2(l), REAL(cost), REAL(vol), prob
100       format(A1,1X,5I8,G12.4,G12.2,24X,G18.9, ' /')
      END SUBROUTINE
      

      SUBROUTINE LoadZoneCosts(iuncosts, ip, i, j, cost, vol,retorno)
        integer, intent(in) :: iuncosts, ip, i, j
        real(8), intent(out) :: cost, vol
        real(4), intent(out) :: retorno
        integer(2) :: iip, ii, jj
        real(4) :: cost4, vol4
        character(1) :: typ

          if (debugging >= dbg_debug) then
            print *, 'LoadZoneCosts', ip, i, j
          endif

          read(iuncosts) typ, iip, ii, jj, cost4, vol4, retorno
          cost = cost4
          vol  = vol4
100       format(A1,1X,3I8,2(8X),G12.4,G12.2, ' /')
          call assertCharEq('Z', typ, 'LoadZoneCosts typ ' // typ)
          call assertIntEq(ip, 1*iip, 'LoadZoneCosts ip ' // typ)
          call assertIntEq(i,  1*ii,  'LoadZoneCosts i '  // typ)
          call assertIntEq(j,  1*jj,  'LoadZoneCosts j '  // typ)
      END SUBROUTINE

      SUBROUTINE LoadModeCosts(iuncosts, ip, i, j, k,cost, vol,prob)
        integer, intent(in) :: iuncosts, ip, i, j, k
        real(8), intent(out) :: cost, vol, prob
        integer(2) :: kk
        character(1) :: typ
        real(4) :: cost4, vol4

          if (debugging >= dbg_debug) then
            print *, 'LoadModeCosts', ip, i, j, k
          endif

          read(iuncosts) typ, kk, cost4, vol4, prob 
          cost   = cost4
          vol    = vol4
100       format(A1,1X,4I8,8X,G12.4,3G12.2,G18.9, ' /')
          call assertCharEq('M', typ, 'LoadModeCosts typ ' // typ)
          !call assertIntEq(ip, iip, 'LoadModeCosts ip ' // typ)
          !call assertIntEq(i,  ii,  'LoadModeCosts i '  // typ)
          !call assertIntEq(j,  jj,  'LoadModeCosts j '  // typ)
          call assertIntEq(k,  1*kk,  'LoadModeCosts k '  // typ)
      END SUBROUTINE

      SUBROUTINE LoadPathCosts(iuncosts, ip, i, j, k, l, cost, vol,prob)
        integer, intent(in) :: iuncosts, ip, i, j, k, l
        real(8), intent(out) :: cost, vol, prob
        integer(2) :: ll
        character*1 :: typ
        real(4) :: cost4, vol4

          if (debugging >= dbg_debug) then
            print *, 'LoadPathCosts', ip, i, j, k, l
          endif

          read(iuncosts) typ, ll, cost4, vol4, prob
          cost   = cost4
          vol    = vol4
100       format(A1,1X,5I8,G12.4,G12.2,24X,G18.9, ' /')
          call assertCharEq('P', typ, 'LoadPathCosts typ ' // typ)
          !call assertIntEq(ip, iip, 'LoadPathCosts ip ' // typ)
          !call assertIntEq(i,  ii,  'LoadPathCosts i '  // typ)
          !call assertIntEq(j,  jj,  'LoadPathCosts j '  // typ)
          !call assertIntEq(k,  kk,  'LoadPathCosts k '  // typ)
          call assertIntEq(l,  1*ll,  'LoadPathCosts l '  // typ)
      END SUBROUTINE

END MODULE RTRANS
