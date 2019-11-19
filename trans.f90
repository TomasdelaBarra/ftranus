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
PROGRAM TRANS
USE DEBUGM
USE OPTIONSM
USE GETOPTM
USE PARAM
USE RCOMM
USE TCOMM
USE ZCOMM
USE TPARC
USE CONTROL
USE WAITING
USE GENER
USE IO_LIST
USE ASCII
USE RTRANS
USE IPCOMM
USE TMATH
USE MPOWIT
USE MLOGIT

    character(80) :: TRANS_RCS_ID = &
      "$Id$" 

      character, parameter :: TRANS_OPTIONS*(*) = 'NAIn:z' // STD_OPTIONS
      integer :: IUNCOSTS = 200
      integer   IUNPAS(MXMOD)
      CHARACTER IIP*1
      LOGICAL   LCONV,Nueva,Adicional,SoloCostos
      LOGICAL   LFLAG
      EXTERNAL  ASIGNA,USAGE
      INTEGER   IPOL
      integer :: IAN,MES,IDIA
      integer :: IHR,MINS,ISEC,MILESM
      integer(2) :: I2
      integer   status
      logical :: doCounts = .false.
      logical :: GenerateIntraZoneCosts = .false.
      character*(128) counts_file, fileName
      integer :: desiredIters = 0

!  IIP controla si va a leer una red nueva (=N) o anterior (=A)
      ITERAN=ICERO
      ! por defecto, nuevo conjunto de iteraciones
      IIP='N' 
      CALL INIT(POL,IIP,.TRUE.,4000,Usage,TRANS_OPTIONS)
      call InitPol(POL,IPOL)
      
      use_powit = use_powit_trans

      Nueva = .true.

      if (.not. GetOptions(Nueva,Adicional,SoloCostos)) then
         Nueva=IIP.EQ.'N'
         Adicional=IIP.EQ.'A'
         SoloCostos=IIP.EQ.'I'
      endif

!  LEE ARCHIVO DE ZONAS

      CALL GetCurrentDate(IAN,MES,IDIA)
      call GetCurrentTime(IHR, MINS, ISEC, MILESM)
      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)
!
!  LECTURA DE PARAMETROS
!
      if (debugging >= dbg_normal) then
         WRITE(*,*)'_________________________________________'
         CALL MENSA(1004,0)  ! LECTURA DE PARAMETROS Y DATOS
      endif

!  SI ES EJECUCION ADICIONAL, LEE T2S PARA ENTERARSE DE LA DEM REPRIMIDA

      IF(Adicional)THEN
         call OpenPolFile(3, IPOL, 'T2S', IO_BIN)
         CALL ReadT2S(3,I,I2,I2,I2,I2,I2,A,A,I,I,A,I,I)
         CLOSE(3)
      ENDIF

!  LECTURA DE LA RED DE P0S SI Nueva o SoloCostos, O DE T3S SI Adicional

      IF(Nueva.OR.SoloCostos)THEN
         call OpenPolFile(3, IPOL, 'P0S', IO_BIN)
      ELSE
         call OpenPolFile(3, IPOL, 'T3S', IO_BIN)
      ENDIF
!  Si Adicional lee el 1er registro de T3S y se entera la ultima iteracion
      IF(Adicional)THEN
         CALL RDTPAR(3,status,ITERAN,I2,I2,I2,I2,I2)
      ELSE   ! Alternativamente lee el encabezamiento de P0S
         CALL RDTPAR(3,status,I,I2,I2,I2,I2,I2)
      ENDIF
      call CheckStatus(status)
      CALL RDREDANT(3)
      call CheckStatus(status)
      CLOSE(3)

!!!! PARCHE PARCHE PARCHE !!!
! J.95.08.29
! Para resolver problema de Tomas en guatemala con
! costos marginales de mantenimiento
      call FindPolFile(3, IPOL, 'P0E', IO_FMT,pol_Any)
      CALL LEEP0E
      CLOSE(3)
!!!! PARCHE PARCHE PARCHE !!!

!  Lectura del archivo de parametros de transporte T1E

      call FindPolFile(3, IPOL, 'T1E', IO_FMT,pol_Any)
      CALL LEET1E
      CLOSE(3)

      if (desiredIters /= 0) then
        NIT = desiredIters
      endif
      
      IF(Nueva)THEN
         ! Multiplicar las capacidades for el factor de capacidad
         ! del tipo de via, solo si la red viene de P0S
         ! T3S ya tiene las capacidades aumentadas
         ! A SoloCostos le da igual porque no hace restriccion de capacidad
         DO L=1,NLINK
            CV=CAP(L)
            IF(CV.GT.0)then
               it = itip(l)
               CAP(L)=CV*FFCAP(it)
            endif
         ENDDO
      !else, los datos ya vienen en T3S
      ENDIF


!  ABRE LOS ARCHIVOS DE FLUJOS (F1S) -excepto SoloCostos- Y DE PASOS (P_S)

      call info('_________________________________________')
      IF(.NOT.SoloCostos)THEN
         call FindPolFile(4, IPOL, 'F1S', IO_BIN, pol_SameYear)
      ENDIF
      NPUB = 0 ! Número de modos públicos
      DO K=1,NTM
         ! Abre los archivos de pasos y calcula el número de modos públicos
         IUNPAS(K)=K+100
         call FindPolFile( IUNPAS(K), IPOL,'P'// trim(IntegerToString(k))//'S', IO_BIN, pol_Any)
         call SkipPathHeader(IUNPAS(K))
         do ip=1, NPROP
            if (LPUB(K).and.MODO(ip,k)) NPUB(ip) = NPUB(ip) + 1
         enddo
      ENDDO

! ABRE ARCHIVO CON CONTEOS
      !! call Initcounts

      if (.not. SoloCostos .and. PolFileExists(IPOL,'T2E')) then
         call OpenPolFile(3, IPOL, 'T2E', IO_FMT)
         !! call LoadCounts(3)
         CLOSE(3)
      endif
      ! No hay conteos en la iteración anterior
      VOLANT = 0


!  COMIENZO DE ITERACIONES

      Puestos = -1 ! No usar en primera iteración
      Congest = 0 ! Blanquea congestion por enlace
      QTime   = 0
      LCONV=.FALSE.
      !! doCounts = .TRUE.
      FreqNew = FreqMin
      if (debugging >= dbg_normal) then
        CALL MENSA(4016,0)  ! Iter  Categoria   Origen  ConvFlujos  ConvVel
      endif
      IF(SoloCostos)NIT=1
      DO 999 ITER=1,NIT
      ITER1=ITER+ITERAN
      IF(ITER.ge.NIT)LCONV=.TRUE.
!  Si es la ultima iteracion, abre T1S para grabar los costos
!  y T4S para las matrices de viajes (excepto SoloCostos, porque no hay viajes)
      IF(LCONV)THEN
         call NewPolFile(3, IPOL, 'T1S', IO_BIN)
         CALL WRTPAR(3,status,ITER1,IDIA,MES,IAN,IHR,MINS)
         call CheckStatus(status)
         IF(.NOT.SoloCostos)THEN
            call NewPolFile(5, IPOL, 'T4S', IO_BIN)
            CALL WRTPAR(5,status,ITER1,IDIA,MES,IAN,IHR,MINS)
            call CheckStatus(status)
            call NewPolFile(IUNCOSTS, IPOL, 'T6S', IO_BIN)
            if (.not.GenerateIntraZoneCosts) then
                CLOSE(IUNCOSTS, STATUS='DELETE')
                IUNCOSTS = 0
            endif
         ENDIF
      ENDIF

      IF(.NOT.SoloCostos)THEN
! Blanquea indicadores de evaluaci¢n
         DISTAN=0.     ! Unidades-km por categor¡a
         TTIEMV=0.     ! Tiempo viaje total por categor¡a
         TTIEME=0.     ! Tiempo espera total por categor¡a
         CPROM=0.      ! Costo total por categor¡a
         TOTV=0.       ! Viajes totales por categor¡a y modo
         VIAJ=0.       ! Viajes totales por operador
         TUTIL=0.      ! Utilidad total por categor¡a
         OPING=0.      ! Ingresos por operador
         COP=0.        ! Costo de operaci¢n por operador

         call LimpiaLNRIndic
 

         ! Leere encabezamiento de F1S
         REWIND 4
         READ(4)I,I2,I2,I2,I2,I2
         call CheckListBegin(4, NPROP, 'Categories')
         do m=1, NPROP
           call CheckListItem(4, m)
           read(4) NUMCAT(M),NOMCAT(M)
         enddo
         call CheckListEnd(4, NPROP, 'Categories')

         call CheckListBegin(4, NTM, 'Modes')
         do k=1, NTM
           call CheckListItem(4, k)
           read(4) NUMMOD(K),NOMMOD(K)
           call CheckListBegin(4, NPROP)
           do m=1, NPROP
             call CheckListItem(4, m)
             read(4) MODO(M,K)
           enddo
           call CheckListEnd(4, NPROP)
         enddo
         call CheckListEnd(4, NTM, 'Modes')

      ENDIF

!  Iteraciones respecto a categorias IP

      ! comienzo de lista de zonas en T1S
      if(LCONV) then
         call WriteListBegin(3, NPROP)
         if(.not.SoloCostos) then
           call WriteListBegin(5, NPROP)
         endif
      endif
      if(.not.SoloCostos)call CheckListBegin(4, NPROP)
      VOLANT     = VOL
      VOL        = 0 ! Blanquea volumen por enlace
      VEH        = 0 ! Blanquea vehiculos por enlace
      Queue      = 0 ! Blanquea cola por enlace
      Suben      = 0 ! Personas abordando en un enlace-ruta
      Bajan      = 0
      IngresosRuta = 0
      DO IP=1,NPROP
        if (debugging >= dbg_normal) then
          WRITE(*,'(A1,1X,I3,2X,I3,1X,A8,'' ('',I3,''%)''$)') &
             13,ITER+ITERAN,NUMCAT(IP),NOMCAT(IP),(100*IP)/NPROP
        endif

        if(.not.SoloCostos)call CheckListItem(4, ip)
        if(LCONV) then
           call WriteListItem(3, ip)
           if(.not.SoloCostos) then
              call WriteListItem(5, ip)
           endif
        endif

        ! Precalcular los costos de los enlace-rutas
        ! para la categoria
        CALL CALCIPCOST(IP)
!  Iteraciones respecto a zonas de origen I

        ! comienzo de lista de zonas en T1S
      if(LCONV) then
         call WriteListBegin(3, NZN)
         if (.not.SoloCostos) then
           call WriteListBegin(5, NZN)
         endif
      endif

      if(.not.SoloCostos)call CheckListBegin(4, NZN)
      DO I=1,NZN
		
        if(.not.SoloCostos) then
           call CheckListItem(4, i, 'Zone LFLAG')
           read(4) LFLAG
        endif
        if(LCONV) then
            call WriteListItem(3, i)
            if(.not.SoloCostos) then
               call WriteListItem(5, i)
            endif
        endif
        IF(JER1(I).ne.JER2(I)) GOTO 70

!  Lee los flujos y viajes exogenos de F1S, excepto si SoloCostos


!  Iteraciones respecto a zonas de destino J
        CALL ITERACJ(IP,I,4,IUNPAS,IUNCOSTS,LCONV,ASIGNA, SoloCostos)
        ! Iteraciones r/destino J

!  Si es la ultima iterac graba los costos en T1S(3) y viajes en T4S(5) excepto SoloCostos

70      IF(LCONV)THEN
           call GraT1SData(3,ip)
           if(.not.SoloCostos) then
             call GraT4SData(5, ip)
           endif

        ENDIF

        enddo         !  Iteraciones r/origen I
        if(.not.SoloCostos) call CheckListEnd(4, NZN)
        if(LCONV) then
           call WriteListEnd(3, NZN) ! fin lista zonas T1S
           if(.not.SoloCostos) then
              call WriteListEnd(5, NZN) ! fin lista zonas T4S
           endif
        endif

!  Rewind los archivos de pasos

        DO K=1,NTM
           REWIND IUNPAS(K)
           call SkipPathHeader(IUNPAS(K))
        ENDDO

      enddo
!  Fin de las categorias IP.
      if(.not.SoloCostos) call CheckListEnd(4, NPROP)

      if(LCONV)then
         Call WriteListEnd(3, NPROP)
         if(.not.SoloCostos) then
             Call WriteListEnd(5, NPROP)
             close(5)
             if(IUNCOSTS /= 0) then
                 close(IUNCOSTS)
             endif
         endif
         CLOSE(3)
      endif


!  Restriccion de capacidad, excepto SoloCostos
!  Abre P0S (red) y,si es la ult iter, abre T3S para grabar asignacion

      if(.NOT.SoloCostos) then
          CALL RESCAP(CONV2,MAXOR,MAXDES,CONVEL, MAXVOR,MAXVDES,ITER+ITERAN)
          if (doCounts .or. CONV2 < CONV) then 
             !! call CalculateCounts
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! OJOOOOO !!!!
             call PolFileName(IPOL, 'T9S', counts_file)
             !! call SaveCounts(counts_file)
!  FIN DE LAS ITERACIONES
             !! doCounts = .TRUE.
          endif 
      endif

!     Si venia con la iteracion ya convergida, se va
      IF(LCONV)GO TO 444

      IF(CONV2.LT.CONV)LCONV=.TRUE.
      !! LCONV = max(CONV2, abs(CountConvergence())) < CONV
      LCONV = CONV2 < CONV
      
!     Si es la 1a iteraci¢n, guarda los viajes en reprimida
      IF(ITER1.EQ.1)THEN
         DO IP=1,NPROP
            DO K=1,NTM
               REPRIMIDA(IP,K)=TOTV(IP,K)
            ENDDO
         ENDDO
      ENDIF



999   CONTINUE

444   CLOSE(3)

!  Escribir resultados asignacion en T3S e indicadores en T2S, excepto SoloCostos

      IF(.NOT.SoloCostos)THEN
         call NewPolFile(10, IPOL, 'T3S', IO_BIN)
         CALL WRTPAR(10,status,ITER1,IDIA,MES,IAN,IHR,MINS)
         call CheckStatus(status)
         CALL WRREDANT(10,status)
         call CheckStatus(status)
         CLOSE(10)
         call NewPolFile(3, IPOL, 'T2S', IO_BIN)
         CALL INDIC(ITER1,IAN,MES,IDIA,IHR,MINS,CONV,CONV2,MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES)
         call NewPolFile(10, IPOL, 'T5S', IO_BIN)
         call GrabaLNRIndic(10)
         close(10)
      ENDIF

      IF(.NOT.SoloCostos)THEN
          !! call PolFileName(IPOL, 'T9S', counts_file)
          !! call SaveCounts(counts_file)
      ELSE
         ! Dejar rastro de opción 'I' para que funcione el Makefile
         call NewPolFile(3, IPOL, 'T0S', IO_BIN)
         CLOSE(3)
      ENDIF
      

      if (debugging >= dbg_normal) then
        CALL TIEJEC(IHR,MINS,ISEC,MILESM)
        CALL MENSA(8,0)  ! FINAL NORMAL DE
        print *, ' T R A N S'
      endif
      STOP
CONTAINS





!   -----------------------------------
      real(8) FUNCTION RESTRI(A,G,A1,VMin, SP)
        REAL A,G,A1,SP
!   -----------------------------------
      real(8) ::    X, ROH, AS, BS

! A, y G son los par ms de la restricci¢n de capacidad alfa,beta,gama
! A1 es la relaci¢n VehStd/Cap
! SP es la velocidad de referencia a flujo libre
! Calculo de ROH, que haga RESTRI(1)=(1-ALFA)
        AS=(1.-A) ! 0.30
        ROH = DASECH(AS)
! Calculo de un BETA que haga RESTRI(GAMMA)= casi 0
        X  = (1.-VMin)
        AS = G ! G es 1.2 el V/C que hace la veloc=0
        BS=DLOG(DASECH(X)/ROH)/DLOG(AS)  ! ????????
! Calculo de la secante hiperbolica
        AS=A1  ! A1 es V/C
        X = ROH*(AS**BS)
        RESTRI = SP*DSECH(X) ! velocidad resultante
      END FUNCTION



! /\/\/\/\//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      real function Colas(l)
      integer, intent(in) :: l
         real VM, T, F
         real S

      Colas = 0
      if (cap(l).le.0 .or. cap(l).ge. RINF) return

      it = itip(l)
      ! Velocidad de saturacion
      VM = VELTIP(it) * ALFA(it)

      ! Tiempo para moverse la distancia D = tiempo de servicio
      T = DIS(l)/VM

      F = LnFlow(l)

      Colas = NewQueueTime(F, 1/T, CAP(l)/FFCAP(it), FFCAP(it))

      end function



SUBROUTINE CalculateRouteQueues
  integer :: lnr, ir, io, l, nfrom, nto
  integer :: lcan, lnrcan, ircan
  integer :: irant, ioant
  real(8) :: vstan, qfactor

  do lnr =1, NLinkRut
    l     = ilink(lnr)
    nfrom = ior(l)
    nto   = ides(l)

    ir = iroute(lnr)
    io = ioprut(ir)

    if (itipop(io) /= 3) cycle

    do lcan = 1, NLINK
      if (ior(lcan) /= nto)    cycle
      if (ides(lcan) == nfrom) cycle
      if (Congest(lcan) <= 0)  cycle

      do lnrcan = RutPri(lcan), RutUlt(lcan)
        ircan = iroute(lnrcan)
        if (ircan /= ir) cycle

        vstan = FacTiem(io)*FreqNew(ir)*VS(io, itip(lcan) )
        qfactor = vstan/LnFlow(lcan)
        Queue(l) = Queue(l) + qfactor*Congest(lcan)
      enddo
    enddo

    irant = ir
    ioant = io
  enddo

END SUBROUTINE

!   -----------------------------------------------
      SUBROUTINE RESCAP(CONV2,MAXOR,MAXDES,CONVEL, &
        MAXVOR,MAXVDES,ITTER)
!   -----------------------------------------------
      REAL CONV2,CONVEL
      INTEGER MAXOR, MAXDES, MAXVOR, MAXVDES, ITTER
      integer(4) :: CV, INF4
      INTEGER L, LnR
      LOGICAL OverSaturation
      REAL Excess, MaxCap
      double precision W
      real Frq(MXRUT), Frn(MXRUT), TiemRuta(MXRUT)
      real Irl, FQrl, FPr, FMr, Fr

! RESTRICCION DE CAPACIDAD Y CALCULO DE CONVERGENCIA

! Inicializaci¢n de los indicadores
! LCONV es .T. si converg/£ltima iter, caso en el cual graba resultados
! VEL es veloc referncia;  en la 1a iter=valores leidos de P0S

      call CalculateRouteQueues


      INF4=999999        ! Infinito para integer(4) ::
      PESO=0.5
!  Blanquea los indicadores
      PK=0.
      EN=0.
      ADING=0.
      CONV2=0.
      CMAN=0.

!  Do-loop para leer cada enlace y procesar

      CONV1=0
      CONVEL=0
      OverSaturation=.FALSE. ! Sin saturacion
      ! Busqueda de enlaces criticos para espera
      Frq      = 0
      Frn      = 0
      TiemRuta = 0
      DO LnR=1, NLINKRUT
         L = ILINK(LnR)
         IR= IROUTE(LnR)     ! Identifica una ruta en enlace L
         IO=IOPRUT(IR)      ! Identifica al operador de la ruta IR
         TiemRuta(IR) = TiemRuta(IR) + TMV(LnR)
         if(ITIPOP(IO) == 3) then
              Irl  = IngresosRuta(LnR)
              FQrl = Vol(LnR)/(TOC(IO)*FACTIEM(IO)) ! Frecuencia demandada en enlace l
              FQrl = FQrl/TOCPromedio(ir)           ! aumentar segun ocup. promedio
              Frq(ir)=Frq(ir) + Irl*FQrl            ! Frecuencia demandada ponderada por el ingreso
              Frn(ir)=Frn(ir) + Irl                 ! Total Ingreso de la ruta ir
         end if
      ENDDO 
  
      do IR=1, NRUTAS
        io = IOPRUT(ir)
        if (ITIPOP(io) == 3) then
             ! calcular frecuencia objetivo FMr
             if (FreqMin(ir) == FreqMax(ir)) then
                FreqNew(ir) = FreqMin(ir)! Fruecuencia fija
             else
                if (Frn(ir) == 0) then
                   FMr = FreqMin(ir)           ! no hay demanda, o no hay ingresos
                else
                   FPr = Frq(ir)/ Frn(ir)      ! frecuencia ponderada
                   FMr = Max(FreqMin(ir), FPr) ! limitar al minimo
                   if (FreqMax(ir) > 0 .and. FMr > FreqMax(ir)) then
                       FMr = FreqMax(ir)       ! limitar al maximo
                   endif
                endif 
                if (FleetMax(IR) <= 0) then
                   Fr = FMr       ! flota no aplica
                else
                   ! limitar a flota maxima
                   Fr = Min(FMr, FleetMax(IR)/TiemRuta(IR))
                endif
                FreqNew(ir) = (Fr + FreqNew(ir)*TransDampFactor) / (TransDampFactor + 1)
             endif
        endif
      enddo

      DO L=1,NLINK

         CV=CAP(L)
         IT=ITIP(L)
         IA=IAD(IT)                            ! Administrador del enlace
         CMAN(IA)=CMAN(IA)+DIS(L)*CMINMAN(IT)  ! Costo m¡n mantenimien

!  Calcula los VehStd (VSTAN);  si son cero, no hay restric. de capac.
         VSTAN=0.
         DO LnR=RUTPRI(L),RUTULT(L)  ! Recorre c/ruta q pasa por el enlace
            IR=IROUTE(LnR)           ! Identifica la ruta en la posici¢n LnR
            IO=IOPRUT(IR)           ! Identifica al operador de la ruta IR
            if(ITIPOP(IO) == 3) then
                VEH(LnR)= FreqNew(IR)*FACTIEM(IO)
            endif
            VSTAN=VSTAN + VEH(LnR)*VS(IO,IT)
            CMAN(IA)=CMAN(IA)+DIS(L)*VEH(LnR)*CMANMAR(IO,IT)
         ENDDO  ! Fin do LnR

        ! convergencia por flujos
        IF(VSTAN > 0. .or. LnFlow(L) > 0.) THEN
               CONV1=ABS(VSTAN-LnFlow(L))/abs(max(VSTAN, LnFlow(L)))
        ENDIF

         VSTAN     = (VSTAN+LnFlow(L)*TransDampFactor)/(TransDampFactor+1)
         LnFlow(l) = VSTAN


        !
        ! Convergencia

         IF(CONV1.GT.CONV2)THEN
            CONV2=CONV1
            MAXOR=IOR(L)
            MAXDES=IDES(L)
         ENDIF

!  Si la capac no es infinita, calcula la nueva veloc de referen VEL1, y
!  la promedia con la de la iteracti¢n ant (VEL)

         VELANT = VEL(L)             ! Guarda la VelRef de la iteración anterior
         VEL1   = VELTIP(IT)         ! mantiene veloc inicial si no hay restricción
         IF(CV.GT.0)THEN
            VC1=(VSTAN+Queue(L))/CV
            IF(VC1.GT.2.0*GAMA(IT))THEN
               !!write(*,992) 13,IOR(L), IDES(L)
               OverSaturation=.TRUE.
               VC1=2.0*GAMA(IT)
               VEL1=1e-5
            ELSE
               VEL1 = RESTRI(ALFA(IT),GAMA(IT),VC1,REDVELMAX(IT),VELTIP(IT))
            ENDIF

         ENDIF  !fin de ajuste de velocidades IF(CV


         VEL(L)=VELANT+PESO*(VEL1-VELANT) ! Se hace en COLAS


         CVEL=ABS(VEL(L)-VELANT)/VELANT
         IF(CVEL >= CONVEL) THEN
            CONVEL=CVEL
            MAXVOR=IOR(L)
            MAXVDES=IDES(L)
         ENDIF

         ! calculo de congestion por colas
         QTime(l) = (QTime(l)+ Colas(l))/2

         Excess = QTime(l)*VSTAN
         Congest(l) = (Congest(l)+Excess)/2

      ENDDO  ! Fin L, para cada enlace
      ! Blanquear colas para asignación


!  Recorre cada enlace-ruta para calcular la espera si es tipo 3
      DO LnR=1, NLINKRUT
         L = ILINK(LnR)
         IR= IROUTE(LnR)     ! Identifica una ruta en enlace L
         IO=IOPRUT(IR)      ! Identifica al operador de la ruta IR
         IF(ITIPOP(IO) == 3) then
              Passing = Vol(Lnr)-Suben(Lnr)
              if (Veh(LnR) > 0) then
                 !Puestos vacíos por unidad de tiempo 
                 Q = Max(0., (TOC(io)*Veh(Lnr) - Passing)/FACTIEM(io))
              else
                 Q = 0
              endif
              if (Puestos(LnR) >= 0) then     
                     Q = (Puestos(LnR) + Q)/2.
              endif
              Puestos(LnR) = Q
              W = NewQueueTime(Suben(LnR), FreqNew(ir), Q, FACTIEM(io))
              if (W.lt.0) then
                   write(*,*) 'Link    =', IOR(L), IDES(L)
                   write(*,*) 'Route   =', NumRut(ir)
                   write(*,*) 'Wait    =', W
                   write(*,*) 'Freq    =', FREQNew(ir)
                   write(*,*) 'OcRate  =', TOC(io)
                   write(*,*) 'TmFact  =', FACTIEM(io)
                   write(*,*) 'Seats   =', Q
                   write(*,*) 'Board   =', Suben(Lnr)
                   STOP 'Wait Time LT 0'
              endif 
              W = W + EspMin(io)
              if (.not. RutScheduled(IR)) then
                    W = W + (1/(2*FreqNew(IR)) )
              endif
             !No promediar esperas
              if (ITTER >= 8) then
                 Espera(LnR) = (W + Espera(LnR)*TransDampFactor) / (TransDampFactor+1)
              else
                 Espera(LnR) = W
              endif
         ELSE
              W = EspMin(io)
              Espera(LnR) = W
         ENDIF

!  Indicadores
          
7328     IT=ITIP(L)
         IA=IAD(IT)
         PK(IO)=PK(IO)+VOL(LnR)*DIS(L)
         !!!!! 
         ! Es incorrecto sumar el costo de operación constante en cada enlace
         ! Eliminado hasta tener esto mejor craneado
         ! COP(IO)=COP(IO)+COPMIN(IO)*VEH(LnR)
         SP=VEL(L)*SPEED(IO,IT)
         IF(ITIPOP(IO).EQ.4)SP=VELTIP(IT)*SPEED(IO,IT)
         ENER=(PAREN1(IO)+PAREN2(IO)*EXP(-PAREN3(IO)*SP))
         EN(IO)=EN(IO)+ENER*DIS(L)*VEH(LnR)
         COP(IO)=COP(IO)+VEH(LnR)*COSLINK(LnR)
         ADING(IA)=ADING(IA)+VEH(LnR)*DIS(L)*CARGOS(IO,IT)
      ENDDO      ! Fin do LnR de cada enlace-RUTA

      if (debugging >= dbg_normal) then
         WRITE(*,991) 13,ITTER,NPROP,NZN,CONV,CONV2,MAXOR,MAXDES, CONVEL,MAXVOR,MAXVDES
      endif

991   FORMAT(A1,1X,I3,2X,I3,2X,I5,2X,F10.7,' F ',F8.5,' (',2I5,')',' V ',F8.5,' (',2I6,')')
992   FORMAT(A1,62X,' (',2I6,')!')

      IF(CONVEL.GT.CONV2)CONV2=CONVEL
      RETURN
      END SUBROUTINE

      SUBROUTINE GraT1SData(iun, ip)
        integer iun
        integer ip
         ip=ip ! use argument

         call WriteListBegin(iun, NZN)
         do j=1, NZN
            call WriteListItem(iun, j)
            WRITE(iun)  &
                C(J),   & ! Desutilidad
                CM(J)     ! Costo Monetario
            call WriteListBegin(iun, NTM)
            do k=1,NTM
               call WriteListItem(iun, k)
               write(iun) COST(j,k) ! Desut x modo
            enddo
            call WriteListEnd(iun, NTM)
         enddo
         call WriteListEnd(iun, NZN)
      END SUBROUTINE

      SUBROUTINE GraT4SData(iun, ip)
        integer iun
        integer ip
         ip=ip ! use argument

         call WriteListBegin(iun, NZN)
         do j=1, NZN
            call WriteListItem(iun, j)
            call WriteListBegin(iun, NTM)
            do k=1,NTM
               call WriteListItem(iun, k)
               write(iun) TRIPS(J,K)
            enddo
            call WriteListEnd(iun, NTM)
         enddo
         call WriteListEnd(iun, NZN)
      RETURN
      END SUBROUTINE

 
 logical function GetOptions(Nueva,Adicional,SoloCostos)
 logical Nueva, Adicional, SoloCostos
 external Usage
      character  flag
      
      if (.not. hasopts()) then
          GetOptions = .FALSE.
          return
      endif

      GetOptions = .TRUE.

      Nueva = .true.
      Adicional = .false.
      SoloCostos  = .false.
      desiredIters = 0

      do i = 1, optc()
          select case(optv(i))
              case ('N')
                 ! do nothing
              case ('A')
                 Adicional = .true.
                 Nueva = .false.
              case ('I')
                 SoloCostos = .true.
                 Nueva = .false.
                 Adicional = .false.
              case ('n')
                 desiredIters = ioptarg(i)
              case ('z')
                 GenerateIntraZoneCosts = .true.
              case default
                 call doStdOpts(Usage)
          end select
      enddo
 return
 end function
 

END PROGRAM TRANS

!   ----------------------------------------
      SUBROUTINE ASIGNA(IP,I,J,iuncosts)
!   ----------------------------------------
      USE PARAM
      USE TPARC
      USE RCOMM
      USE TCOMM
      USE IPCOMM
      !! USE COUNTS
      USE RTRANS
      INTEGER IP, J

      INTEGER L, LnR, LnRa, NextL, LnRAnt
      real(8) ::  PROB, VStd, Q, A 
      REAL Ingr, adj_trips, kadj
      real(8) :: vacios, consol
      logical :: turnProhibition
      logical :: obeysTurnProhibitions
!  Asignaci¢n

      !! adj_trips = CountAdjustment(i,j,ip)
      adj_trips = 0.0
      if (IUNCOSTS /= 0) then      
          call SaveZoneCosts(iuncosts, ip, i, j, c(j), ff(j),retorno(j))
      endif
      DO K=1,NTM
         if (IUNCOSTS /= 0) then      
            call SaveModeCosts(iuncosts, ip, i, j, k, cost(j,k), trips(j,k),p(k))
         endif
         vacios=RETORNO(J)*p(k)               ! Retornos
         consol=MIN(TRIPS(J,K),vacios)        ! Demanda consolid
         IF(MODO(IP,K))THEN
            kadj = adj_trips * P(k) ! ajuste por conteo segun probabilidad del modo
            DO J2=1,nps(k)          ! Recorre los pasos
!  OJO:  la siguiente division no esta controlada
               PROB=PROP(J2,K)
               A=TRIPS(J,K)*PROB     ! Demanda llena del paso J2
               A=max(0.0D0,A+kadj*PROB)                ! Demanda ajustada
               if (IUNCOSTS /= 0) then      
                  call SavePathCosts(iuncosts, ip, i, j, k, j2, putil(j2,k), A, prop(j2,k))
               endif
               VAC=vacios*PROB    ! Demanda vac¡a en el paso J2
               CON=consol*PROB    ! Parte consolidable en el paso j2
               IRANT=ICERO
               IOANT=0
               LNRAnt = 0
               DO JJ=1,NCOL(J2,k)    ! Recorre los enlaces
                  LnR=MATPAS(J2,JJ,k) ! Identifica el enlace
                  LnRa=ABS(LnR)
                  L =ILINK(LnRa)
                  IR=IROUTE(LnRa)   ! Identifica la ruta
                  IO=IOPRUT(IR)
118               VOL(LnRa)=VOL(LnRa)+A  ! Acumula demanda del arco
                  LNRIndic(LnRa,ip)%trips = LNRIndic(LnRa,ip)%trips + A
                  !! call UpdateCounts(i,j,ip,lnra, lnrant, REAL(A))
!  Calcula los vehiculos dependiendo del tipo de operador
                  VV=A/TOC(IO)
!  Si hay trasbordo, acumula los viajes por oper y suma costop minimos
                  obeysTurnProhibitions = itipop(io) < 3 
                  turnProhibition = (MATPAS(J2,JJ,k) < 0) .and. obeysTurnProhibitions
                  Ingr = 0
                  IF(IR /= IRANT.or. turnProhibition )THEN
                     if(IOANT == 0) IOANT = IO
                     VIAJ(IO)=VIAJ(IO)+A
                     Suben(LnRa) = Suben(LnRa) + A
                     LNRIndic(LnRa,ip)%board = LNRIndic(LnRa,ip)%board + A
                     Ingr = COSMIN(IOANT, IO)
                     LNRIndic(LnRa,ip)%income = LNRIndic(LnRa,ip)%income + A*Ingr
                     if (LnRAnt /= 0) then
                       Bajan(LnRAnt) = Bajan(LnrAnt)+A
                       LNRIndic(LnRAnt,ip)%unboard = LNRIndic(LnRAnt,ip)%unboard + A
                     endif
                  ENDIF
                  Ingr = A*(Ingr + TARIFA(LnRa))
                  LNRIndic(LnRa,ip)%income = LNRIndic(LnRa,ip)%income + A*Tarifa(LnRa)  
                  !Costos de operacion excluidos hasta saber como considerar
                  !subsidios, y paises donde los costos de operacion no son "reales"
                  !if (CosLink(LnR) > Ingr) then
                      !Ingr = 0
                  !else
                      !  Ingr = Ingr-CosLink(LnR)
                  !endif
                  IngresosRuta(LnRa) = IngresosRuta(LnRa) +  Ingr
!  Si hay retornos, los agrega a los veh¡culos
                  IF(PARCON(IO).GT.CEROMEN)VV=VV+(VAC-CON*PARCON(IO))/TOC(IO)
!  Acumula los veh¡culos en los enlaces
                  IF(ITIPOP(IO) /= 3 .and. ITIPOP(IO) /= 4) THEN
                       VEH(LnRa)=VEH(LnRa)+VV ! 0 acumulados para rutas. Se suman en la asignación
                  !else los vehículos se calculan durante la restricción de capacidad
                  ENDIF
!  Acumula la distancia por categor¡a de transporte
                  DISTAN(IP)=DISTAN(IP)+A*DIS(L)
                  ! Calculamos la cola
                  if (JJ.LT.NCOL(J2,k)) then
                    NextLR = Abs(MATPAS(J2,JJ+1,k))
                    NextL  = ILINK(NextLR)
                    if(Congest(NextL) > 0    .and. &
                       ITIPOP(io) /= 3       .and. &
                       IROUTE(NextLR) == IR) then
                       ! Hay un exedente
                       ! Asi contribuye este paso a congestion
                       VStd = VV*VS(IO,ITIP(L))
                       Q    = VStd/LnFlow(NextL)
                       ! Asi castigamos al enlace
                       Queue(L) = Queue(L) + Q*Congest(NextL)
                    endif
                  endif
                  LnRAnt = LnRa
                  IRANT =IR
                  IOANT =IOPRUT(IR)
               ENDDO       !  Fin Do enlaces JJ

               IF (A > 0.) THEN
                  TTIEMV(IP)=TTIEMV(IP)+A*TIEMV(J2,K)
                  TTIEME(IP)=TTIEME(IP)+A*TIEME(J2,K)
                  DO IO=1,NOPER
                     IF(MODOPER(IO).NE.K)CYCLE
                     CPROM(IP)=CPROM(IP)+A*COBRO(J2,IO)
                     OPING(IO)=OPING(IO)+A*COBRO(J2,IO)
                  ENDDO
               ENDIF

            ENDDO          !  Paso J2
         ENDIF             !  Fin If modo
      ENDDO                !  Modo K

      RETURN
      END
      
 subroutine Usage
 USE GETOPTM
 character(32) prog
    prog = argv(0)

    print *
    print '(A,'' - TRANUS(r) Transport Model'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options]'')', trim(prog)
    print '(4X, A,''  <scen> -N [options]'')', trim(prog)
    print '(4X, A,''  <scen> -A [options]'')', trim(prog)
    print '(4X, A,''  <scen> -I [options]'')', trim(prog)
    print *
    print *, 'If no commands or optios are given, the program enters interactive mode.'


    print *
    print *, 'Commands are:'
    print *, '  -N        : Perform new run on the scenario (the default).'
    print *, '  -A        : Perforn additional iterations on the scenario.'
    print *, '  -I        : Perform initial run without assignment.'

    print *
    print *, 'Options are:'
    print *, '  -n <num>     : Run <num> iterations.'
    print *, '  -z           : Generate intra-zone costs file (T6S).'
    call ExplainStdOptions
 
    STOP 2
 end subroutine

