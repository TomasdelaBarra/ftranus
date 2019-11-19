! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2010 Modelistica, Caracas
! * Copyright (C) 1983-2010 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM LCAL
USE DEBUGM
USE OPTIONSM
USE GETOPTM
USE PARAM
USE CONTROL
USE MENSAMOD
USE GENER
USE IO_LIST
USE ZCOMM
USE LPARC
USE LCOMM
USE MLOGIT

    character(80) :: LCAL_RCS_ID = &
      "$Id$" 

      character, parameter :: LCAL_OPTS*(*) = STD_OPTIONS // 'NAf'
      external Usage
      
      CHARACTER IIP*1
      INTEGER   IPOL
      LOGICAL   LFLAG
      integer :: IAN,MES,IDIA
      integer :: IHR,MINS,ISEC,MILESM
      integer(2) :: I2
      logical :: LastIter, Adicional
      logical :: Freeze ! Freeze transportable production. Make it all exogenous.
      integer :: s1, s2

      IIP = 'N'
      CALL INIT(POL,IIP,.TRUE.,1003, Usage,LCAL_OPTS)
      call InitPol(POL,IPOL)

      use_powit = use_powit_loc

      if (.not. hasOpts()) then
         Adicional = (IIP == 'A')
         Freeze = .false.
      else
         call doStdOpts(Usage)
         Adicional = hasOpt('A')
         Freeze = hasOpt('f')
      endif

      call GetCurrentDate(IAN,MES,IDIA)
      call GetCurrentTime(IHR, MINS, ISEC, MILESM)
!
!  LECTURA DEL ARCHIVO DE ZONAS Z1E

      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)

!  Lectura de parametros L1E


!  Lectura de datos de ejeci�n anterior L1S
!  S�lo para ejecuciones adicionales

      if (debugging >= dbg_normal) then          
        CALL MENSA(1004,0)  ! LECTURA DE PARAMETROS Y DATOS
      endif
      call OpenPolFile(3, IPOL, 'L1E', IO_FMT)
      CALL LEEL1E
      CLOSE(3)

      IF(Adicional)THEN
         call OpenPolFile(3, IPOL, 'L1S', IO_BIN)
         CALL LEEL1S(3,IT, .FALSE.)
         CLOSE(3)
      ELSE
         AJUSTE = 0
         PRECIO = 0
      ENDIF
      call OpenPolFile(3, 0, 'L0E', IO_FMT)
      CALL LEEL0E(Adicional, Freeze)
      CLOSE(3)
      
      if (Freeze) then
          ! No demand allowed between transportable sectors
          do s1 = 1, NS
              do s2=1, NS
                  if (LFLU(s1)) then
                      DEMIN(s1, s2) = 0.0
                      DEMAX(s1, s2) = 0.0
                  endif
              enddo
          enddo
      endif

!  Verifica que las demandas m�nimas y m�ximas cumplan restricciones

      IF(.not. Adicional)THEN
         CALL CONSISTE

!  Calcula atractores de producción inducida para sectores transportables
!  ATRAIN posee los valores por zona leidos de L0E (MULTIPLICATIVOS)

         DO I=1,NZ2
            NIVEL=1
            IF(I.GT.NZ1)NIVEL=2
            DO M=1,NS
               ATRAC(I,M)=0.
               IF(LFLU(M).AND.RMAX(I,M).GT.CEROMAS)THEN
                  DO N=1,NS
                     if(RMAX(I,N).LT.RINF) then ! no puede definirse como
                        !  atractor una variable sin valores observados en L0E
                        ATRAC(I,M)=ATRAC(I,M)+RMAX(I,N)*ALFA(M,N,NIVEL)
                     endif
                  ENDDO
                  ATRAC(I,M) = ATRAIN(I,M) * ATRAC(I,M)**FactorAtrac(M)
               ENDIF
               IF(ATRAC(I,M).LT.0.)THEN
                  WRITE(*,'('' Zon:'',I6,''   Sect:'',I6)')NUMZON(I),NUMSEC(M)
                  CALL MENSA(1008,-1)  ! ERROR L02:  Atractor negativo
               ENDIF
            ENDDO ! Fin de M
         ENDDO   !Fin de I
      ENDIF  ! Fin de Iteración nueva .not. Adicional


!  LOCALIZACION DE LA PRODUCCION INDUCIDA


      if (debugging >= dbg_normal) then     
          CALL MENSA(1006,0)  ! LOCALIZACION DE LA PRODUCCION
      endif

!  Comienzo de las iteraciones

      IF(.not. Adicional)THEN
         IT=1
         PRO=0.
      ELSE
         NIT=IT+NIT
         IT=IT+1
      ENDIF
      LastIter = .FALSE.
      DO 333 ITER=IT,NIT
      LastIter = LastIter .or. ITER >= NIT

!  Abre el archivo de costos de transporte C1S para leer,
!  y el de flujos L2S para grabar
      if(.not.Freeze) then
          call OpenPolFile(3, IPOL, 'C1S', IO_BIN)
      endif
      call NewPolFile(4, IPOL,  'L2S', IO_BIN)
      if (Freeze) then
          ! Delete the L2S file
          close(4, status='delete')    
      endif
      if (LastIter.and..not.Freeze) then 
         WRITE(4)NZN,NFLU,int2(IAN),int2(MES),int2(IDIA),int2(IHR),int2(MINS)
         call WriteListBegin(4,NS)
         do m=1,NS
            write(4) NUMSEC(M),NOMSEC(M),LFLU(M)
         enddo
         call WriteListEnd(4, NS)
      endif

      if(.not.Freeze) then
          READ(3)NN,N,I2,I2,I2,I2,I2
          IF(NN.NE.NZN.OR.N.NE.NFLU) then
             write(*, 9907) NN, NZN, N, NFLU
9907         format(' Z=',I3,'/',I3,' S=',I3,'/',I3)
             CALL MENSA(1001,-1) ! ERR L01: no coinciden ...
          endif
          call CheckListBegin(3,NS, 'SECTORS')
          do ip=1,NS
              read(3) NUMSEC(IP),NOMSEC(IP),LFLU(IP)
          enddo
          call CheckListEnd(3, NS)
      endif

      COSPRO=0.

!  Lee los costos, distribuye la producci�n para cada sector M
!  y graba los flujos

      if(.not.Freeze) then
          call CheckListBegin(3, NS)
          if(LastIter) call WriteListBegin(4, NS)
      endif
      DO M=1,NS  ! localiza la demanda de M y graba los flujos
         if(Freeze)then
            ! We don't want the production to be transported
            UTRA = 0
            COSTRA = 0
         else
             read(3) lflag ! igual a LFLU(M), para ObjTranus
             LFLAG = LFLU(M)
             if(LastIter) write(4) LFLAG
             IF(LFLU(M))THEN
                call CheckListBegin(3, NZN)
                DO I=1,NZN
                    READ(3) (UTRA(I,J), COSTRA(I,J), J=1, NZN)
                END DO  ! I
                call CheckListEnd(3, NZN)
             ENDIF
         endif

! Calcula la demanda y los costos de producci�n

         CALL DEMANDA(M)
         DO I=1,NZN
            PRO(I,M)=0.
         ENDDO
         CALL LOCIN(M, LastIter)
      END DO  ! M Sectores
      if(.not.Freeze) call CheckListEnd(3, NS)
      if(LastIter.and..not.Freeze) call WriteListEnd(4, NS)

!  Llama a la rutina de restricciones y convergencia

      CALL RESTPRO(CONV, CVPREC,CVPROD,ITER,.FALSE.) ! .FALSE. No es LOC

!  Eval�a convergencia global de la iteraci�n (todos los sectores)

      if (LastIter) GOTO 444 
      LastIter = LastIter .or. (CVPREC.LT.CONV.AND.CVPROD.LT.CONV) 

      if (.not.Freeze) then
         rewind 3
         rewind 4
      endif
333   CONTINUE
      ITER=ITER-1
444   if (.not.Freeze) then
          close(3)
          close(4)
      endif

!  Graba los resultados en el archivo L1S

      IF(CVPROD.GT.CONV)CALL MENSA(1015,0) ! L07:  Ultima iter..
      call OpenPolFile(3, 0, 'L0E', IO_FMT)
      call CalculoRestricciones
      CLOSE(3)
      call NewPolFile(3, IPOL, 'L1S', IO_BIN)
      CALL GRAL1S(3, ITER, IDIA,MES,IAN,IHR,MINS)
      CLOSE(3)

      if (debugging >= dbg_normal) then
        WRITE(*,*)'_________________________________________'
        CALL TIEJEC(IHR,MINS,ISEC,MILESM)
        CALL MENSA(8,0)  ! FINAL NORMAL DE
        write(*,*) ' L C A L '
      endif
      STOP

CONTAINS

      SUBROUTINE LEEL0E(Adicional, Freeze)
      logical :: Adicional, Freeze

!  LEE LOS DATOS DE USO DEL SUELO EN EL A�O BASE DE L0E

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    CHECK   (Subr)  Verifica los finales de secci�n
!    INTNUM  (Fun)   Busca el n�mero interno de una categor�a externa
!    MENSA   (Subr)  Emite los mensajes

!  Inicializaci�n de las variables si la ejecuci�n es nueva

      IF(.not. Adicional)THEN  !Ejecuci�n nueva, se inicializan variables
         DO N=1,NS
             RESTOT(N)=0.
             DO I=1,NZN
                XPRO(I,N)=0.
                PROAN(I,N)=0.
                PRO(I,N)=0.
                XDEM(I,N)=0.
                DEM(I,N)=0.
                RMIN(I,N)=0.
                RMAX(I,N)=0.
                PRECIO(I,N)=0.
                PREBASE(I,N)=0.
                PREAN(I,N)=0.
                ATRAC(I,N)=0.
                ATRAIN(I,N)=0.
                COSCON(I,N)=0.
                UTCON(I,N)=0.
                VALAG(I,N)=0.
                PROBASE(I,N) = 0
                STOCK(I,N) = 0
                UNSTOCK(I,N) = 0
             ENDDO
         ENDDO
      ENDIF ! fin inicializaci�n de variables si ejecuci�n es nueva

!  SECCION 1.1 - DATOS OBSERVADOS DEL A�O BASE, ZONAS INTERNAS
      READ(3,'(/////)')
101   READ(3,*,END=999,ERR=100,IOSTAT=IOS)N1,I1,A1,A2,A3,A4,A5,A6
! Verifica consistencia de sector y zona
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L0E(1.1) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definición ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZ2)
      IF(I.GT.NZ2)THEN
         WRITE(*,*)'L0E(1.2) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definición ilegal
      ENDIF

!  Siempre se lee L0E aunque la ejecución sea adicional
!  para colocar la producción observada como restricciones ficticias
!  las restricciones reales del sig. grupo se sobreponen a estas
!  se lee L0E después de L1S en las ejecuciones adicionales

      RMIN(I,N)=A1+A2
      RMAX(I,N)=RMIN(I,N)

!  El resto de las variables sólo se leen si la ejecución es nueva
!  porque si es adicional ya estarán leídos de L1S

      IF(.not. Adicional)THEN ! ejecuci�n nueva
         if(LFLU(N).AND.I.LE.NZ2.AND.JER1(I).EQ.JER2(I)) then
            RESTOT(N)=RESTOT(N)+A2
         endif 
         XPRO(I,N)=A1
         PRO(I,N)=A2  ! para evaluar consistencia de DMIN y DMAX
         PROBASE(I,N)=A2
         if(Freeze.and.LFLU(N)) then
            ! Make exogenous the production of transportable sectors
            XPRO(I,N) = XPRO(I,N) + PRO(I,N)
            PRO(I,N) = 0
            PROBASE(I,N) = 0
         endif
         XDEM(I,N)=A3
         PREAN(I,N)=A4
         if (.not. Adicional) then
            ! Read Prices
            PRECIO(I,N)=A4
         endif
         if(.not.lflu(n))utcon(i,n)=A4
         PREBASE(I,N)=A4
         VALAG(I,N)=A5
         ATRAIN(I,N)=A6 ! valor multiplicarivo inicial del atractor
         !  La siguiente instruccion permite introducir la produccion en cero y*  el atractor en 1., 
         !  con lo cual la restriccion se hace infinita.
         !  el modelo calcula la demanda necesaria sin restrici�n (caso sup const)
         IF(RMAX(I,N).LE.0..AND.ATRAIN(I,N).GT.CEROMAS)THEN
            RMAX(I,N)=RINF
         ENDIF
      ENDIF ! fin ejecuci�n nueva en lectura de secci�n 1.1 de L0E
      GO TO 101
100   CALL CHECK(1.1,IOS,'L0E')

!  SECCION 2.1 - EXPORTACIONES POR ZONA EXTERNA

      READ(3,'(//)')
211   READ(3,*,END=999,ERR=210,IOSTAT=IOS)N1,I1,A1
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L0E(2.1) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZN)
      IF(I.LE.NZ2.OR.I.GT.NZN)THEN
         WRITE(*,*)'L0E(2.1) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      XDEM(I,N)=A1
      GO TO 211
210   CALL CHECK(2.1,IOS,'L0E')

!  SECCION 2.2 - IMPORTACIONES POR ZONA EXTERNA

      READ(3,'(/)')
221   READ(3,*,END=999,ERR=220,IOSTAT=IOS)N1,I1,AImportMin,AImportMax,A2,A3
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L0E(2.2) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZN)
      IF(I.LE.NZ2.OR.I.GT.NZN)THEN
         WRITE(*,*)'L0E(2.2) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      RMIN(I,N)=AImportMin
      RMAX(I,N)=AImportMax
      IF(RMIN(I,N) > RMAX(I,N)) THEN
         WRITE(*,*)'L0E(2.2) - Zon:',I1,' Sect:', N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      IF(.not. Adicional)THEN ! Si la ejecuci�n es nueva (no adicional)
         VALAG(I,N)=A2
         ATRAIN(I,N)=A3
         ATRAC(I,N)=ATRAIN(I,N)*RMAX(I,N)**FactorAtrac(N)
      ENDIF
      IF(LFLU(N))RESTOT(N)=RESTOT(N)+AImportMax
      GO TO 221
220   CALL CHECK(2.1,IOS,'L0E')

!  SECCION 3.0 - RESTRICCIONES A LA PRODUCCION INTERNA

      READ(3,'(/)')
301   READ(3,*,END=999,ERR=300,IOSTAT=IOS)N1,I1,A1,A2
      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L0E(3.0) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZ2)
      IF(I.GT.NZ2)THEN
         WRITE(*,*)'L0E(3.0) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      IF(A1.GT.A2)THEN
         WRITE(*,*)'L0E(3.0) - Zon:',I1,'  Sec:',N1,'  RMin:',A1,'  RMax:',A2
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      IF(A2 < STOCK(I,N))THEN
         WRITE(*,*)'L0E(3.0) - Zon:',I1,'  Sec:',N1,'  RMax:',A2,'  Stock:',STOCK(I,N)
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      IF(RMAX(I,N).GE.RINF)THEN
         RMIN(I,N)=A1
         RMAX(I,N)=A2
      ENDIF
      GO TO 301
300   CALL CHECK(3.0,IOS,'L0E')


! Calcula variables de macrozona a partir de subzonas para ATRAC y DISTRIB
      DO N=1,NS
         DO I=1,NZ1
            IF(.not.ConSubZonas(i))CYCLE
            RMIN(I,N)=0.
            RMAX(I,N)=0.
            PRECIO(I,N)=0.
            PRO(I,N)=0.
            XPRO(I,N)=0.
            XDEM(I,N)=0.
            ATRAUX=ATRAIN(I,N)
            DO J=JER1(I),JER2(I)
               PRO(I,N)=PRO(I,N)+PRO(J,N)
               XPRO(I,N)=XPRO(I,N)+XPRO(J,N)
               XDEM(I,N)=XDEM(I,N)+XDEM(J,N)
               RMIN(I,N)=RMIN(I,N)+RMIN(J,N)
               RMAX(I,N)=RMAX(I,N)+RMAX(J,N)
               PRECIO(I,N)=PRECIO(I,N)+PRECIO(J,N)*RMAX(J,N)
! Si se quiere que el atractor de la macrozona sea calculado en funci�n
! de los atractores de las subzonas, se omite la macrozona en L0E
! Si la macrozona se incluye en L0E su atractor prevalece, el resto de
! las variables se calculan por agregaci�n de las subzonas.
               if(ATRAUX.LE.CEROMAS) then
                   ATRAIN(I,N)=ATRAIN(I,N)+ATRAIN(J,N)*RMAX(J,N)
               endif
            ENDDO !J subzonas de I
            IF(RMAX(I,N).GT.CEROMAS)THEN
               PRECIO(I,N)=PRECIO(I,N)/RMAX(I,N)
               if(ATRAUX.LE.CEROMAS)then
                  ATRAIN(I,N)=ATRAIN(I,N)/RMAX(I,N)
               endif
            ENDIF
            IF(RMAX(I,N).GE.RINF)RMAX(I,N)=RINF
         ENDDO
      ENDDO
      RETURN
999   WRITE(*,*)'****L0E****'
      CALL MENSA(10,-1)
      END SUBROUTINE


      SUBROUTINE CalculoRestricciones
      CHARACTER*32 NOM

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!     MENSA (Subr)  Emite los mensajes

!  Inicializa restricciones DE ZONAS INTERNAS para grabar

!  Todas las actividades estuvieron restringidas de
!  manera ficticia a los montos observados de producci�n
!  pero en L1S solo se graban las restriciones reales de la secci�n 3
!  de L0E, el resto se graba como: m�nima 0 y m�xima RINF

      do n = 1, NS
         do i = 1, NZ2
            RMIN(i,n) = 0
            RMAX(i,n) = RINF
         enddo
      enddo

!  Salta grupos de L0E hasta encontrar restricciones verdaderas

      N=ICERO
303   READ(3,'(A)',END=999)NOM
      IF(NOM(3:6).EQ.'----')N=N+1
      IF(N.LT.4)GO TO 303  ! salta secciones de L0E hasta la 3.0
      READ(3,'(/)')
101   READ(3,*,END=999,ERR=107,IOSTAT=IOS)N1,I1,A1,A2

      N=INTNUM(N1,NUMSEC,NS)
      IF(N.GT.NS)THEN
         WRITE(*,*)'L0E(3.0) - Sect:',N1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      I=INTNUM(I1,NUMZON,NZ2)
      IF(I.GT.NZ2)THEN
         WRITE(*,*)'L0E(3.0) - Zon:',I1
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      RMIN(I,N)=A1
      RMAX(I,N)=A2
      GO TO 101
107   CALL CHECK(3.0,IOS,'L0E')
      CLOSE(3)

!  recalcula las restricciones de macrozonas a partir de las subzonas

      DO N=1,NS
         DO I=1,NZ1
            IF(JER1(I).EQ.JER2(I))CYCLE
            RMIN(I,N)=0.
            RMAX(I,N)=0.
            DO J=JER1(I),JER2(I)
               RMIN(I,N)=RMIN(I,N)+RMIN(J,N)
               RMAX(I,N)=RMAX(I,N)+RMAX(J,N)
            ENDDO ! J
            IF(RMAX(I,N).GE.RINF)RMAX(I,N)=RINF
         ENDDO ! I
      ENDDO ! N

      RETURN
999   WRITE(*,*)'****L0E****'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto

      END SUBROUTINE



      SUBROUTINE CONSISTE
!     ------------------------
!  Verifica que las demandas m�nimas y m�ximas cumplan restricciones

      LOGICAL LISTO(MXSEC)
      DO M=1,NS
!  Si el sector es basico, no se verifica consistencia
         IF(NUMSEC(M).LT.ICERO)CYCLE
         DMIN=0.  ! demanda m�nima global de M en el sistema
         DMAX=0.  ! demanda m�xima global de M en el sistema
         DO 30 I=1,NZN
            D1=0.  ! demanda m�nima de M en la zona I
            D2=0.  ! demanda m�xima de M en la zona I
            IF(I.GT.NZ2)GO TO 29
            DO N=1,NS
               D1=D1+DEMIN(M,N)*(PRO(I,N)+XPRO(I,N))
               D2=D2+(DEMAX(M,N)+DEMIN(M,N))*(PRO(I,N)+XPRO(I,N))
            END DO
29          D1=D1+XDEM(I,M)
            D2=D2+XDEM(I,M)
            IF(.NOT.LFLU(M).AND.JER1(I).EQ.JER2(I))THEN
               IF(D2.LE.0.)D2=D1
               IF(D1.GT.RMAX(I,M)*1.01)THEN
!  si la demanda minima de M no transportable excede la oferta en una zona
!  se determina DD proporci�n de demanda que falta por satisfacer

                  DD=1.-(RMAX(I,M)/D1)

!  se verifica si lo que falta puede satisfacerse con sustitutos de M

                  LISTO=.FALSE.
                  DO N=1,NS
                     DO M2=1,MXSUST
                        M1=NSUST(M,N,M2) !sector sustituto de M para N
                        IF(M1.EQ.ICERO)CYCLE
                        IF(LISTO(M1))CYCLE
                        D11=0.
                        DO N1=1,NS
                           DO N2=1,MXSUST
                             IF(NSUST(M1,N1,N2).EQ.M1)THEN
                                 D11=D11+DEMIN(M1,N)*(PRO(I,N)+XPRO(I,N))*DD
                             ENDIF
                           ENDDO ! N2
                        ENDDO ! N1
                        LISTO(M1)=.TRUE.
                        IF(D11.LE.RMAX(I,M1))GO TO 20
                        DD=DD*(1.-(RMAX(I,M1)/D11))
                     ENDDO ! M2
                  ENDDO ! N
                  WRITE(*,*)' ZON',NUMZON(I),' SEC',NUMSEC(M),NOMSEC(M)
                  WRITE(*,*)' DEM',D1,' PROD',RMAX(I,M)
                  CALL MENSA(1022,0) ! L11:  La dem m�n excede la oferta
               ENDIF ! IF D1 .GT. RMAX(I,M)
20             CONTINUE

!  se verifica ahora si la demanda maxima cubre la restricci�n minima

               IF(D1.GT.0..AND.D2.LT.RMIN(I,M)*0.99)THEN
                  WRITE(*,*)' ZON',NUMZON(I),' SEC',NUMSEC(M),NOMSEC(M)
                  WRITE(*,*)' DEM',D2,' PROD',RMIN(I,M)
                  CALL MENSA(1023,0) ! L12:  La dem max no alcanza la producci�n
               ENDIF
            ELSE IF(I.LE.NZ1.OR.I.GT.NZ2)THEN  ! Factor transportable
               DMIN=DMIN+D1 ! acumula la demanda de la zona en la global
               DMAX=DMAX+D2
            ENDIF
30       CONTINUE
         IF(LFLU(M))THEN
            IF(DMIN.GT.RESTOT(M)*1.01)THEN
               WRITE(*,*)' SECT',NUMSEC(M)
               WRITE(*,*)' DEM',DMIN,' PROD',RESTOT(M)
               CALL MENSA(1020,0) ! L09: La dem min tot excede la prod total
            ENDIF
            IF(DMIN.GT.0..AND.DMAX.LT.RESTOT(M)*0.99)THEN
               WRITE(*,*)' SECT',NUMSEC(M)
               WRITE(*,*)' DEM',DMAX,' PROD',RESTOT(M)
               CALL MENSA(1021,0) ! L10:  La dem max tot no cubre la prod total
            ENDIF
         ENDIF
      ENDDO  ! Fin de sector m

      RETURN
      END SUBROUTINE

END PROGRAM LCAL

 subroutine Usage
 USE GETOPTM
 character(32) prog
    prog = argv(0)

    print *
    print '(A,'' - TRANUS(r) Base Year Location Model'')', trim(prog)

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
    print *, '  -f        : Freeze transportable production (experimental).'
    call ExplainStdOptions

    STOP 2
 end subroutine

