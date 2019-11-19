! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2010 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo A~nez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM LOC
USE DEBUGM
USE OPTIONSM
USE GETOPTM
USE PARAM
USE GENER
USE MENSAMOD
USE CONTROL
USE IO_LIST
USE MLOGIT
USE ZCOMM
USE LPARC
USE LCOMM
USE EXOGALLOC

      CHARACTER IIP*1
      INTEGER IPOL, IPANT
      LOGICAL ADICIONAL
      LOGICAL LFLAG
      integer :: IAN,MES,IDIA
      integer :: IHR,MINS,ISEC,MILESM
      integer(2) :: I2
      logical   LastIter

      character(80) :: LOC_RCS_ID = & 
        "$Id$" 

      character, parameter :: LOC_OPTIONS*(*) = STD_OPTIONS // 'NA'
      external Usage


!  DENOMINACION DE LOS ARCHIVOS
!  Unidad Nombre Tipo Formato    Contenido
!    1    MENSA.DAT     con      Mensajes
!    3      L1E  Entr.  con      Parametros
!    3      L1S  Entr.  con      Datos periodo anterior
!    3      L2E  Entr.  con      Datos politica presente
!    3      L1S  Entr.  sin      Actvidades. per¡odo previo
!    3      C1S  Entr.  sin      Costos de transporte p/cat.s-e
!    4      L2S  Slda.  sin      Flujos intersectoriales
!    4      L1S  Slda.  sin      Resultados localizaci¢n

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    ABRE    (Subr)  Abre un archivo OLD con o sin formato
!    CONTROL (Subr)  Lee el archivo de control CONTROL.DAT
!    LEEZ1E  (Subr)  Lee el archivo de zonas Z1E
!    LEEL1E  (Subr)  Lee el archivo de par metros L1E
!    CHECK   (Subr)  Verifica los finales de secci¢n
!    INTNUM  (Fun)   Busca el n£mero interno de una categor¡a externa
!    TIEJEC  (Subr)  Calcula el tiempo de ejecuci¢n del programa
!    MENSA   (Subr)  Emite los mensajes (0=adv, 1=int, 2=graba3, 3=err)
!
!  LEE EL ARCHIVO DE CONTROL

      IIP = 'N'
      CALL INIT(POL,IIP,.TRUE.,1002, Usage, LOC_OPTIONS)

      use_powit = use_powit_loc

      if (.not. hasOpts()) then
         Adicional = (IIP == 'A')
      else
         call doStdOpts(Usage)
         Adicional = hasOpt('a')
      endif

      call InitPol(POL,IPOL)
      call PrevPol(IPOL,IPANT,pol_Any)

      
      call GetCurrentDate(IAN,MES,IDIA)
      call GetCurrentTime(IHR, MINS, ISEC, MILESM)
!
!  LECTURA DEL ARCHIVO DE ZONAS Z1E

      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)


!  Lectura de datos de L1S del per¡odo anterior si ejecucion nueva
!  del periodo actual si ejecucion adicional

      if (debugging >= dbg_normal) then     
          CALL MENSA(1004,0)  ! LECTURA DE PARAMETROS Y DATOS
      endif

      if (ADICIONAL) then
         call OpenPolFile(3, IPOL,  'L1S', IO_BIN)
      else
         call OpenPolFile(3, IPANT, 'L1S', IO_BIN)
      endif
      CALL LEEL1S(3, IT, .not. ADICIONAL)
      CLOSE(3)
      
      call FindPolFile(3, IPOL, 'L1E', IO_FMT, pol_Any) !!!
      CALL LEEL1E
      CLOSE(3)
      if (.not.ADICIONAL) then
         DEM = 0
      endif

!  Si la ejecuci¢n es nueva ejecuta lectura de L2E y el incremental

      IF(.not.ADICIONAL)THEN
         ! Allocation of Changes in Exogenous Variables
         call ACXV_ApplyChanges(IPOL)

         call OpenPolFile(3, IPOL, 'L2E', IO_FMT)
         CALL LEEL2E
         CLOSE(3)

!  Incrementos de variables exogenas entre periodos anterior y actual

         CALL CRECIM

!  Verifica que las demandas m¡nimas y m ximas cumplan restricciones
!  Solo se puede verificar haciendo antes un I-O aespacial

!        CALL CONSISTE

!  Calcula atractores de producci¢n inducida para sectores transportables
!  ATRAIN posee los valores por zona leidos de L2E (MULTIPLICATIVOS)

         call debug('ATTRACTOR CALCULATION')

         DO I=1,NZ2
            NIVEL=1
            IF(I.GT.NZ1)NIVEL=2
            DO M=1,NS
               ATRAC(I,M)=0.
               IF(LFLU(M).AND.RMAX(I,M).GT.CEROMAS)THEN
                  DO N=1,NS
                     ATRAC(I,M)=ATRAC(I,M)+PROAN(I,N)*ALFA(M,N,NIVEL)
                  ENDDO
                  ATRAC(I,M) = ATRAIN(I,M) * ATRAC(I,M)**FactorAtrac(M)
               ENDIF
               IF(ATRAC(I,M).LT.0.)THEN
                  WRITE(*,'('' Zon:'',I6,''   Sect:'',I6)')NUMZON(I),NUMSEC(M)
                  CALL MENSA(1008,-1)  ! ERROR L02:  Atractor negativo
               ENDIF
            ENDDO  ! Fin de M
         ENDDO ! Fin de I
! Blanquea PROAN porque la demanda parte de producci•n ex•gena
!        DO N=1,NS
!           DO I=1,NZN
!              PROAN(I,N)=RCERO
!           ENDDO
!        ENDDO
      ENDIF  ! ejecuci¢n nueva

!  LOCALIZACION DE LA PRODUCCION INDUCIDA

!  Abre C1S para leer costos y L2S para grabar flujos

      if (debugging >= dbg_normal) then     
          CALL MENSA(1006,0)  ! LOCALIZACION DE LA PRODUCCION
      endif
      call FindPolFile(3, IPANT, 'C1S', IO_BIN, pol_SameYear)
      call NewPolFile(4,  IPOL, 'L2S', IO_BIN)

!  Comienzo de las iteraciones

      IF(IIP.EQ.'N')THEN
         IT=1
         PROAN=PROAN-XPRO
         PRO=0.
      ELSE
         NIT=IT+NIT
         IT=IT+IUNO
      ENDIF

      LastIter = .FALSE.
      DO 333 ITER=IT,NIT

      LastIter = LastIter .or. ITER >= NIT

      if (LastIter) then
         WRITE(4)NZN,NFLU,int2(IAN),int2(MES),int2(IDIA),int2(IHR),int2(MINS)
         call WriteListBegin(4,NS)
         do m=1,NS
            call WriteListItem(4, m)
            write(4) NUMSEC(M),NOMSEC(M),LFLU(M)
         enddo
         call WriteListEnd(4, NS)
      endif
      READ(3)NN,N,I2,I2,I2,I2,I2
      IF(NN.NE.NZN.OR.N.NE.NFLU)CALL MENSA(1001,-1)  ! L01: no coinciden
      call CheckListBegin(3,NS)
      do ip=1,NS
        call CheckListItem(3, ip)
        read(3) NUMSEC(IP),NOMSEC(IP),LFLU(IP)
      enddo
      call CheckListEnd(3, NS)

      if (debugging >= dbg_Normal) WRITE(*,*)
      COSPRO=0.

!  Lee los costos, distribuye la producci¢n para cada sector M
!  y graba los flujos

!     DEM=0 !  Ojo, instruccion que no esta en LCAL, pruebo eliminarla
      call CheckListBegin(3, NS)
      if(LastIter) call WriteListBegin(4, NS)
      DO M=1,NS  ! localiza la demanda de M y graba los flujos
         call CheckListItem(3, M)
         if(LastIter) call WriteListItem(4, M)
         read(3) lflag ! igual a LFLU(M), para ObjTranus
         LFLAG = LFLU(M)
         if(LastIter) write(4) LFLAG
         IF(LFLU(M))THEN
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, I)
               READ(3) (UTRA(I,J), COSTRA(I,J), J=1,NZN)
            END DO  ! I
            call CheckListEnd(3, NZN)
         ENDIF

! Calcula la demanda y los costos de producci¢n

         CALL DEMANDA(M)
         DO I=1,NZN
            PRO(I,M)=0.
         ENDDO
         CALL LOCIN(M, LastIter) ! LOCIN llama a DISTRI
      END DO  ! M Sectores
      call CheckListEnd(3, NS)
      if(LastIter) call WriteListEnd(4, NS)

!  Llama a la rutina de restricciones y convergencia

      CALL RESTPRO(CONV,CVPREC,CVPROD,ITER,.TRUE.) ! TRUE es LOC

!  Eval£a convergencia global de la iteraci¢n (todos los sectores)

      if(LastIter) GOTO 444
      LastIter = LastIter .or. (CVPREC.LT.CONV.AND.CVPROD.LT.CONV) 

      REWIND 3
      REWIND 4
333   CONTINUE
      ITER=ITER-1
444   CLOSE (3)
      CLOSE (4)

!  Graba los resultados en el archivo L1S

      IF(CVPROD.GT.CONV)CALL MENSA(1015,0) ! L07:  Ultima iter..
      call NewPolFile(3, IPOL, 'L1S', IO_BIN)
      CALL GRAL1S(3, ITER, IDIA,MES,IAN,IHR,MINS)
      CLOSE(3)

      if (debugging >= dbg_Normal) then
        WRITE(*,*)'_________________________________________'
        CALL TIEJEC(IHR,MINS,ISEC,MILESM)
        CALL MENSA(8,0)  ! FINAL NORMAL DE
        STOP 'L O C'
      endif
      STOP 0

CONTAINS


      SUBROUTINE LEEL2E
!     =================

!  LEE INCREMENTOS DE LAS VARIABLES EN RELACION AL PERIODO ANTERIOR

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    CHECK   (Subr)  Verifica los finales de secci¢n
!    INTNUM  (Fun)   Busca el n£mero interno de una categor¡a externa
!    MENSA   (Subr)  Emite los mensajes

!   Inicializacion de los icrementos globales por sector

      DO N=1,NS
         CREPRO(N)=RCERO
         CRECON(N)=RCERO
         CREMAX(N)=RCERO
         CREMIN(N)=RCERO
      END DO

!  SECCION 1.1 - INCREMENTOS GLOBALES DE VARIABLES EXOGENAS
      READ(3,'(/////)')
101   READ(3,*,END=999,ERR=100,IOSTAT=IOS)N1,A1,A2,A3,A4
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(1.1) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      CREPRO(N)=A1
      CRECON(N)=A2
      CREMIN(N)=A3
      CREMAX(N)=A4
      GO TO 101
100   CALL CHECK(1.1,IOS,'L2E')

!  SECCION 1.2 - INCREMENTOS DE PRODUCCION EXOGENA POR ZONA

      READ(3,'(/)')
121   READ(3,*,END=999,ERR=120,IOSTAT=IOS)N1,I1,A1
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(1.2) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZ2)
      IF(I.GT.NZ2)THEN
         WRITE(*,*)'L2E(1.2) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      IF(JER1(I).NE.JER2(I))THEN
         WRITE(*,*)'L2E(1.2) - Sect:',N1,'  Zon:',I1
         CALL MENSA (1016,-1) ! incremento en zona desagregada
      ENDIF
      XPRO(I,N)=XPRO(I,N)+A1
      IF(XPRO(I,N).LT.RCERO)XPRO(I,N)=RCERO
      GO TO 121
120   CALL CHECK(1.2,IOS,'L2E')

!  SECCION 1.3 - INCREMENTOS DE DEMANDA EXOGENA POR ZONA

      READ(3,'(/)')
131   READ(3,*,END=999,ERR=130,IOSTAT=IOS)N1,I1,A1
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(1.3) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZ2)
      IF(I.GT.NZ2)THEN
         WRITE(*,*)'L2E(1.3) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      IF(JER1(I).NE.JER2(I))THEN
         WRITE(*,*)'L2E(1.3) - Sect:',N1,'  Zon:',I1
         CALL MENSA (1016,-1) ! incremento en zona desagregada
      ENDIF
      XDEM(I,N)=XDEM(I,N)+A1
      IF(XDEM(I,N).LT.RCERO)XDEM(I,N)=RCERO
      GO TO 131
130   CALL CHECK(1.3,IOS,'L2E')


!  SECCION 2.1 - INCREMENTO EN EXPORTACIONES POR ZONA EXTERNA

      READ(3,'(//)')
211   READ(3,*,END=999,ERR=210,IOSTAT=IOS)N1,I1,A1
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(2.1) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZN)
      IF(I.LE.NZ2.OR.I.GT.NZN)THEN
         WRITE(*,*)'L2E(2.1) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      XDEM(I,N)=XDEM(I,N)+A1
      IF(XDEM(I,N).LT.RCERO)XDEM(I,N)=RCERO
      GO TO 211
210   CALL CHECK(2.1,IOS,'L2E')


!  SECCION 2.2 - INCREMENTO EN IMPORTACIONES POR ZONA EXTERNA

      READ(3,'(/)')
221   READ(3,*,END=999,ERR=220,IOSTAT=IOS)N1,I1,AImportMinIncr, AImportMaxIncr, AtrainIncr
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(2.2) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZN)
      IF(I.LE.NZ2.OR.I.GT.NZN)THEN
         WRITE(*,*)'L2E(2.2) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      RMIN(I,N)=RMIN(I,N)+AImportMinIncr 
      RMAX(I,N)=RMAX(I,N)+AImportMaxIncr 
      IF(RMAX(I,N).LE.RCERO)RMAX(I,N)=RCERO
      IF(RMIN(I,N).LE.RCERO)RMIN(I,N)=RCERO
      IF(RMIN(I,N) > RMAX(I,N)) THEN
         WRITE(*,*)'L2E(2.2) - Zon:',I1,' Sect:', N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      ATRAIN(I,N) = ATRAIN(I,N)+AtrainIncr
      if (ATRAIN(I,N) < 0) then
          ATRAIN(I,N) = 0
      endif
      ! Calculo del atractor
      ATRAC(I,N)=ATRAIN(I,N)*RMAX(I,N)**FactorAtrac(N)

      GO TO 221
220   CALL CHECK(2.2,IOS,'L2E')


!  SECCION 3.1 - INCREMENTO EN ATRACTORES EXOGENOS POR ZONA

      READ(3,'(//)')
311   READ(3,*,END=999,ERR=310,IOSTAT=IOS)N1,I1,A1
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(3.1) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZN)
      IF(I.GT.NZN)THEN
         WRITE(*,*)'L2E(3.1) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      ATRAIN(I,N)=ATRAIN(I,N)+A1
      IF(ATRAIN(I,N).LE.RCERO)ATRAIN(I,N)=RCERO
      GO TO 311
310   CALL CHECK(3.1,IOS,'L2E')

!  SECCION 3.2 - INCREMENTO RESTRICCIONES A LA PRODUCCION INTERNA POR ZONA

      READ(3,'(/)')
331   READ(3,*,END=999,ERR=330,IOSTAT=IOS)N1,I1,A1,A2
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(3.2) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZ2)
      IF(I.GT.NZ2)THEN
         WRITE(*,*)'L2E(3.2) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      IF(JER1(I).NE.JER2(I))THEN
         WRITE(*,*)'L2E(3.2) - Sect:',N1,'  Zon:',I1
         CALL MENSA (1016,-1) ! incremento en zona desagregada
      ENDIF
      RMIN(I,N)=RMIN(I,N)+A1
      IF(RMAX(I,N).GE.RINF)THEN
        RMAX(I,N)=A2
      ELSE
        RMAX(I,N)=RMAX(I,N)+A2
      ENDIF
      IF (RMIN(I,N).LE.RCERO)RMIN(I,N)=RCERO
      IF (RMAX(I,N).LE.RCERO)RMAX(I,N)=RCERO
      IF (RMAX(I,N).LT.RMIN(I,N))RMAX(I,N)=RMIN(I,N)
      IF(SectorType(N) == 1 .and. RMAX(I,N) < STOCK(I,N)) then
         WRITE(*,*)'L2E(2.2) - Zon:',I1,' Sect:', N1
         WRITE(*,*)'           RMAX < STOCK'
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      endif
      GO TO 331
330   CALL CHECK(3.2,IOS,'L2E')

!  SECCION 3.3 - INCREMENTO EN EL VALOR AGREGADO A LA PRODUCCION

      READ(3,'(//)')
341   READ(3,*,END=999,ERR=340,IOSTAT=IOS)N1,I1,A1
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L2E(3.3) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      IF(I1.EQ.ICERO)THEN
        DO IT=1,NZN
           VALAG(IT,N)=VALAG(IT,N)+A1
           IF(VALAG(IT,N).LE.RCERO)VALAG(IT,N)=RCERO
        ENDDO
      ELSE
         I=INTNUM(I1,NUMZON,NZN)
         IF(I.GT.NZN)THEN
            WRITE(*,*)'L2E(3.3) - Zon:',I1
            CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      IF(JER1(I).NE.JER2(I))THEN
         WRITE(*,*)'L2E(3.3) - Sect:',N1,'  Zon:',I1
         CALL MENSA (1016,-1) ! incremento en zona desagregada
      ENDIF
         ENDIF
         VALAG(I,N)=VALAG(I,N)+A1
         IF(VALAG(I,N).LE.RCERO)VALAG(I,N)=RCERO
      ENDIF
      GO TO 341
340   CALL CHECK(3.3,IOS,'L2E')


      RETURN
999   WRITE(*,*)' L2E'
      CALL MENSA(10,-1)
      END SUBROUTINE




      SUBROUTINE CRECIM
!     =================

!  DISTRIBUYE LOS INCREMENTOS GLOBALES DE VARIABLES EXOGENAS
!  CREPRO CRECON CREMAX CREMIN S¢lo zonas internas


!  OJO:  los incrementos puntuales por zona ya fueron asignados a las
!      variables correspondientes al leerse de L2E
!      aqu¡ s¢lo se distribuyen los incrementos globales "adicionalmente"

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!  MENSA (Subr)  Emite mensajes de LOC

      real :: FPRO, FPRE, FCAPA

      if (debugging >= dbg_Normal) then
        WRITE(*,*)'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
        CALL MENSA(1005,0)  ! INCREMENTO DE VARIABLES EXOGENAS
        WRITE(*,*)
      endif

!  se suma prod ex¢gena e inducida para tener el peso como atractor
! OJO: si hubo un incremento puntual de prod ex¢gena XPRO ya lo tiene
!      e incidir  en el peso como atractor de la variable correspondiente

      DO 50 I=1,NZN
         DO 51 M=1,NS
            PROAN(I,M)=PROAN(I,M)+XPRO(I,M)
51       CONTINUE
50    CONTINUE

!  Distribuye el incremento global sector a sector, guardandolo en PRO

      DO 20 M=1,NS
         if (debugging >= dbg_Debug) WRITE(*,'(''+ Sect:'',I5)')M
         DO 33 K=1,4
            IF(K.EQ.1)CRECE=CREPRO(M)
            IF(K.EQ.2)CRECE=CRECON(M)
            IF(K.EQ.3)CRECE=CREMAX(M)
            IF(K.EQ.4)CRECE=CREMIN(M)
            SUM1=RCERO ! guarda atractores del primer nivel
            DO 30 I=1,NZ2
               NIVEL=1
               IF(I.GT.NZ1)NIVEL=2
               XATRAC=RCERO
               DO N=1,NS
                  A=XALFA(M,N,NIVEL)
                  IF(A.GT.RCERO)THEN
                     IF(CRECE.LT.RCERO) A=1./A
! desde aqui modificacion de Bea al incremental
! se sustituye esto   XATRAC=XATRAC+PROAN(I,N)*A por esto:

                      FPRO=PROAN(I,N)*XALFAPRO(M,N)
                      IF(FPRO.LT.0.)FPRO=-1./FPRO
                      FPRE=PREAN(I,N)*XALFAPRE(M,N)
                      IF(FPRE.LT.0.)FPRE=-1./FPRE
                      FCAPA=(RMAX(I,N)-PROAN(I,N))*XALFACAP(M,N)
                      IF(FCAPA.LT.0.)FCAPA=-1./FCAPA
                      XATRAC=XATRAC+A*(FPRO+FPRE+FCAPA)

! hasta aqui modificaciones, mas declaracion de FPRO FPRE y FCAPA
                  ENDIF
               ENDDO  ! N sector que atrae a M
! OJO a continuaci¢n se est  usando el ATRAIN incrementado
               XATRAC=XATRAC*ATRAIN(I,M)
               IF(NIVEL.EQ.1)SUM1=SUM1+XATRAC

!  El incremento se guarda provisionalmente en PRO, antes de asignarlos
!  a las variables ex¢genas, para que unos sectores no afecten a otros

               PRO(I,M)=XATRAC ! peso de la zona I en atraer M
30          CONTINUE
            IF(SUM1.LE.RCERO)GO TO 20
            DO  40 I=1,NZ2
               IF(I.LE.NZ1)THEN
                  PRO(I,M)=CRECE*(PRO(I,M)/SUM1)
                  IF(JER1(I).NE.JER2(I))THEN
                     SUM2=RCERO ! suma los atractores de las subz de I
                     DO J=JER1(I),JER2(I)
                        SUM2=SUM2+PRO(J,M)
                     ENDDO
                     DO J=JER1(I),JER2(I)
                        PRO(J,M)=PRO(I,M)*(PRO(J,M)/SUM2)
                     ENDDO
                  ENDIF ! zona desagregada
               ENDIF ! primer nivel

!  Suma el incremento a la variable ex¢gena correspondiente
!  Si alguna variable ex¢gena se hace negativa se igualar  a cero

               IF(JER1(I).NE.JER2(I))CYCLE
               IF(K.EQ.1)THEN
                  XPRO(I,M)=XPRO(I,M)+PRO(I,M)
                  IF(XPRO(I,M).LT.RCERO)XPRO(I,M)=RCERO
               ELSE IF(K.EQ.2)THEN
                  XDEM(I,M)=XDEM(I,M)+PRO(I,M)
                  IF(XDEM(I,M).LT.RCERO)XDEM(I,M)=RCERO
               ELSE IF(K.EQ.3)THEN
                  RMAX(I,M)=RMAX(I,M)+PRO(I,M)
                  IF(RMAX(I,M).LT.RCERO)RMAX(I,M)=RCERO
               ELSE IF(K.EQ.4)THEN
                  RMIN(I,M)=RMIN(I,M)+PRO(I,M)
                  IF(RMIN(I,M).LT.RCERO)RMIN(I,M)=RCERO
                  IF(RMIN(I,M).GT.RMAX(I,M))THEN
                     WRITE(*,'('' Zon'',I5,''  Sec'',I4,''  RMin'',' &
                        // 'F10.1,''  RMax'',F10.1)')                &
                        NUMZON(I),NUMSEC(M),RMIN(I,M),RMAX(I,M)
                     CALL MENSA(1030,-1)
                  ENDIF
               ENDIF
40          CONTINUE  ! I=1,NZ2  
33       CONTINUE  !  K=1,4 variable que crece: prod cons o restric
20    CONTINUE  !  M=1,NS  sector que se incrementa

!  calcula valores de macrozonas desagregadas a partir de subzonas

      DO N=1,NS
         DO I=1,NZ1
            IF(JER1(I).EQ.JER2(I))CYCLE
            XPRO(I,N)=RCERO
            XDEM(I,N)=RCERO
            RMIN(I,N)=RCERO
            RMAX(I,N)=RCERO
            PROAN(I,N)=RCERO
            DO J=JER1(I),JER2(I)
               XPRO(I,N)=XPRO(I,N)+XPRO(J,N)
               XDEM(I,N)=XDEM(I,N)+XDEM(J,N)
               RMIN(I,N)=RMIN(I,N)+RMIN(J,N)
               RMAX(I,N)=RMAX(I,N)+RMAX(J,N)
               PROAN(I,N)=PROAN(I,N)+PROAN(J,N)
            ENDDO  ! subzonas J de I
            IF(RMAX(I,N).GE.RINF)RMAX(I,N)=RINF
         ENDDO  ! zona desagregada I
      ENDDO  !  sectores N
      RETURN
      END SUBROUTINE


END PROGRAM LOC


 subroutine Usage
 USE GETOPTM
 character*32 prog
    prog = argv(0)

    print *
    print '(A,'' - TRANUS(r) Location Model'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options]'')', trim(prog)
    print '(4X, A,''  <scen> -N [options]'')', trim(prog)
    print '(4X, A,''  <scen> -A [options]'')', trim(prog)
    print *
    print *, 'If no command is given, the program enters interactive mode'

    print *
    print *, 'Commands are:'
    print *, '  -N        : Perform new run on the scenario'
    print *, '  -A        : Perforn additional iterations on the scenario'

    print *
    print *, 'Options are:'
    call ExplainStdOptions

    STOP 2
 end subroutine

