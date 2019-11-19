!$Id$
!   ------------------------------------------------------
!   T R A N U S  -  M A T S       V 4.0     (c)MODELISTICA
!   ------------------------------------------------------
!   PROGRAMA IMPRIMIR MATRICES DE LOC, FLUJ, TRANS Y COST
!   ------------------------------------------------------
!   Desarrollado por T. de la Barra y B. Perez    Marzo 90
!   ------------------------------------------------------
PROGRAM MATS
USE DEBUGM
USE GETOPTM
USE PARAM
USE ZCOMM
USE TPARC
USE CONTROL
USE GENER
USE IO_LIST
USE ASCII
USE NODES
USE DOCOUT

     character(80) :: MATS_RCS_ID = & 
       "$Id$" 

      
      CHARACTER*128 NOMAG(MXZON)
      REAL   :: MAT(MXZON,MXZON)
      real(8) :: DMAT(MXZON,MXZON)
      real(8) :: DMATCELL, DA
      real(8) :: MATAG(MXZON,MXZON), FF(MXZON), CC(MXZON), TTT
      REAL   :: AUX1(MXZON,MXMOD)
      real(8) :: TRIPS(MXZON)
      INTEGER:: IAG(MXZON), NUMAG(MXZON)
      logical(1) :: LFLU(MXSEC),IMP(15)
      INTEGER NUMSEC(MXSEC)
      CHARACTER*(32) :: SALIDA
      CHARACTER*(32) :: NOMSEC(MXSEC)
      CHARACTER SI*1,IIP*1
      LOGICAL   NE,OPSAL
      INTEGER   IPOL, status
      LOGICAL   LFLAG
      integer(2) :: IDIA,MES,IAN,IHR,MINS

      common /mats_common/ nomag, mat, dmat, matag, ff, cc, aux1, trips, &
                           iag, numag

      character, parameter :: MATS_OPTS*(*) = 'DMSPQRTOEFFCKXYdo:x' // STD_OPTIONS
      external Usage
      logical :: UseMatsDat  = .false.
      logical :: docOutput   = .false.

      MATAG=0   ! Inicializa matriz agregada
      MAT=0.

      CALL INIT(POL,IIP,.FALSE.,8030, Usage, MATS_OPTS)
      call InitPol(POL,IPOL)

!  Lectura archivo de zonas

      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)

! ESPECIFICA EL ARCHIVO DE SALIDA

      OPSAL=.FALSE.
      if (docOutput) then
        SALIDA = trim(AREA) // trim(POL) // '.xls'
      else
        SALIDA = trim(AREA) // trim(POL) // '.MTX'
      endif
      SI=CHAR(15)

      call GetOptions(SALIDA)

!  Opciones de ingreso de datos

      if(.not. hasOpts()) then
          WRITE(*,*)
          WRITE(*,*)'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          CALL MENSA(8031,0)  ! Opciones para ingreso de datos:...
          WRITE(*,*)'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
10        CALL MENSA(7,1)  !     Opci¢n --->
          READ(*,'(I2)')IP
          IF(IP.LT.0.OR.IP.GT.1)GO TO 10
          IF(IP.EQ.1) THEN
            INQUIRE(FILE='MATS.DAT',EXIST=NE)
            IF(.NOT.NE)THEN
               CALL MENSA(8032,0)
               GO TO 10
            ENDIF
            UseMatsDat = .true.
          ENDIF
      endif

      if (.not. hasOpts()) then
         CALL SALI(SALIDA)
      else
         CALL SALI(SALIDA, .false.)
      endif
      IF(SALIDA(1:3).EQ.'PRN')OPSAL=.TRUE.
      IF(OPSAL)WRITE(2,'('' '',A)')SI

!  Lee las opciones de mats.dat si es que existe

      DO I=1,NZN     ! Inicializa vector de agregaci¢n
         IAG(I)=I
      ENDDO
      NZAG=0
      IF(UseMatsDat)THEN
         OPEN(3,FILE='MATS.DAT',STATUS='OLD')
         CALL LEEDAT(NZAG,IMP,ESVER)

!  Si no existe mats.dat lee las opciones de la pantalla

      ELSEIF(.not. hasOpts()) THEN
         IMP=.FALSE.
         WRITE(*,'(///)')
         WRITE(*,*)'________________________________________________'
         CALL MENSA(8033,0)  ! Opciones de impresi¢n de matrices:..
         WRITE(*,*)'________________________________________________'
40       CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,'(I2)')IPP
         IF(IPP.LT.1.OR.IPP.GT.15)GO TO 40
         IMP(IPP)=.TRUE.
         IF(IMP(8))THEN
            WRITE(*,*)
            WRITE(*,*)
            CALL MENSA(8034,1)  !  Intervalos en la escala de costos ?
            READ(*,'(F10.0)')ESVER
         ENDIF
      ENDIF

      IF(NZAG.EQ.0)THEN ! no hubo agregaci¢n de zonas
         NZAG=NZN
         DO J=1,NZN
            NUMAG(J)=NUMZON(J) ! a efectos de la rutina ESCRIBE
            NOMAG(J)=NOMZON(J)
         ENDDO
      ENDIF


      if (docOutput) call DOCStart(2, 'MATS -- ' // NOMPOL(IPOL) // ' ' // DESCPOL(IPOL) )

! OPCION 1 - Desutilidades por categor¡a de transporte

      IF(IMP(1))THEN
         call OpenPolFile(3, IPOL, 'T1S', IO_BIN)
         CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)  ! Encabezamiento
         call CheckListBegin(3,NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip)
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            if (docOutput) call DocFormattedStart(2)
            CALL MENSA(8035,2)  ! DESUTILIDADES POR CATEG DE TRANSPORTE
            WRITE(2,'(I3,1X,A8,6X,I4,6X,A3,2X,A3,I6,''-'',I2,'// &
                    '''-'',I4,I6,'':'',I2.2)') &
                    NUMCAT(IP),NOMCAT(IP),ITER, &
                    AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            if (docOutput) call DocFormattedEnd(2)
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, i)
               call CheckListBegin(3, NZN)
               do j=1, NZN
                 call CheckListItem(3, j, 'desut i j')
                 READ(3) DMATCELL    ! Desutilidades
                 MAT(I,J) = DMATCELL 
                 READ(3) DA
                 call CheckListBegin(3, NTM, 'desut i j k')
                 DO K=1,NTM
                    call CheckListItem(3, k, 'desut i j k')
                    READ(3) DA
                 ENDDO
                 call CheckListEnd(3, NTM, 'end desut i j k')
               enddo
               call CheckListEnd(3, NZN)
            ENDDO
            call CheckListEnd(3, NZN)
            if (docOutput) then
               call EscDocTable(2, 0, NZN, TTT)
            elseif (TAB.EQ.' ') then
               CALL ESCRIBE(0,TTT,NZN)
            ELSE
               CALL ESCTAB(0,NZN,TTT,TAB)
            ENDIF
         ENDDO  ! Fin do categor¡as IP
         call CheckListEnd(3, NPROP)
         CLOSE(3)
      ENDIF

!  OPCION 2 - Desutilidades por modo y categor¡a de transporte de TRANS

      IF(IMP(2))THEN
         call OpenPolFile(3, IPOL, 'T1S', IO_BIN)
!Lee el encabezamiento primero para enterarse de NTM
         CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)
         REWIND 3
         DO K=1,NTM
            CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)
            call CheckListBegin(3,NPROP)
            DO IP=1,NPROP
               call CheckListItem(3, ip)
               IF(OPSAL)WRITE(2,'(1X,A)')SI
               IF(MODO(IP,K))THEN
                  WRITE(2,'(/)')
                  CALL MENSA(8036,2)  ! DESUT X MODO Y CAT DE TRANSPORTE
                  WRITE(2,'(I3,1X,A8,I6,1X,A8,6X,I4,6X,A3,2X,A3,'//  &
                          'I6,''-'',I2,''-'',I4,I6,'':'',I2.2)')     &
                           NUMMOD(K),NOMMOD(K),                      &
                           NUMCAT(IP),NOMCAT(IP),ITER,               & 
                           AREA,POL,IDIA,MES,IAN,IHR,MINS
                  WRITE(2,'(/)')
               ENDIF
               call CheckListBegin(3, NZN)
               DO I=1,NZN
                  call CheckListItem(3, i)
                  call CheckListBegin(3, NZN)
                  do j=1, NZN
                     call CheckListItem(3, j)
                     READ(3)DA
                     READ(3)DA
                     call CheckListBegin(3, NTM)
                     DO K1=1,NTM
                        call CheckListItem(3, k1, 'i j k')
                        READ(3) DMATCELL
                        IF(K1.EQ.K.AND.MODO(IP,K1))THEN
                              MAT(I,J) = DMATCELL
                        ENDIF
                     ENDDO
                     call CheckListEnd(3, NTM)
                  enddo
                  call CheckListEnd(3, NZN)
               ENDDO   ! Fin do zonas I
               call CheckListEnd(3, NZN)
               IF(MODO(IP,K))THEN
                  IF(TAB.EQ.' ')THEN
                     CALL ESCRIBE(0,TTT,NZN)
                  ELSE
                     CALL ESCTAB(0,NZN,TTT,TAB)
                  ENDIF
               ENDIF
            ENDDO      ! Fin do categor¡a IP
            call CheckListEnd(3, NPROP)
            REWIND(3)
         ENDDO         ! Fin do modo K
         CLOSE(3)
      ENDIF            ! Fin opci¢n 2

!  OPCION 3 - Desutilidades por sector s-e de COST

      IF(IMP(3))THEN
          call OpenPolFile(3, IPOL, 'C1S', IO_BIN)
          READ(3)NN,NFLU,IAN,MES,IDIA,IHR,MINS
          call ReadListBegin(3, NS)
          do ip=1, NS
             call CheckListItem(3, ip)
             read(3) NUMSEC(IP),NOMSEC(IP),LFLU(IP)
          enddo
          call CheckListEnd(3, NS)
          call CheckListBegin(3, NS)
          DO IP=1,NS
             call CheckListItem(3, ip)
             read(3) LFLAG ! repetido aqui para ObjTranus
             IF(.NOT.LFLU(IP))CYCLE
             IF(OPSAL)WRITE(2,'(1X,A)')SI
             WRITE(2,'(/)')
             CALL MENSA(8044,2)  ! DESUTILIDADES POR SECTOR SOCIO-ECONOM
             WRITE(2,'(I3,2X,A6,4X,A3,2X,A3,I6,''-'',I2,''-'',I4,I6,' // &
                     ''':'', I2.2)') &
                     NUMSEC(IP),NOMSEC(IP),AREA,POL,IDIA,MES,IAN,IHR,MINS
             WRITE(2,'(/)')
             call CheckListBegin(3, NZN)
             DO I=1,NZN
                call CheckListItem(3, i)
                READ(3) (MAT(I,J),A, J=1, NZN)
             ENDDO
             call CheckListEnd(3, NZN)
             IF(TAB.EQ.' ')THEN
                CALL ESCRIBE(0,TTT,NZN)
             ELSE
                CALL ESCTAB(0,NZN,TTT,TAB)
             ENDIF
          ENDDO
          call CheckListEnd(3, NS)
          CLOSE(3)
      ENDIF

!  OPCION 4 - Viajes por modo de transporte de TRANS

      IF(IMP(4))THEN
         call OpenPolFile(3, IPOL, 'T4S', IO_BIN)
!Lee el encabezamiento primero para enterarse de NTM
         CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)
         REWIND 3
         DO K=1,NTM
            MATAG=0
            CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8038,2)  ! VIAJES POR MODO DE TRANSPORTE...
            WRITE(2,'(I3,1X,A,6X,6X,A3,2X,A3,2X,I3,I6,''-'',I2.2,''-'',' // &
                    'I4,I6,'':'',I2.2, 2A)')                                &
                    NUMMOD(K),trim(NOMMOD(K)),AREA,POL,ITER,                &
                    IDIA,MES,IAN,IHR,MINS, ReleaseStr, ReleaseName
            WRITE(2,'(/)')
            call CheckListBegin(3, NPROP)
            DO IP=1,NPROP
               call CheckListItem(3, ip)
               call CheckListBegin(3, NZN)
               DO I=1,NZN
                  call CheckListItem(3, i)
                  call CheckListBegin(3, NZN)
                  do j=1, NZN
                    call CheckListItem(3, j)
                    call CheckListBegin(3, NTM)
                    DO K1=1,NTM
                       call CheckListItem(3, k1)
                       READ(3)TRIPS(J)
                       IF(K1.EQ.K)THEN
                          IF(IAG(I).ne.0.and.IAG(J).ne.0)then
                              MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+TRIPS(J)
                          ENDIF
                       ENDIF                ! Fin if modo
                    ENDDO                      ! Ultimo modo k1
                    call CheckListEnd(3, NTM)
                  enddo ! destinos J
                  call CheckListEnd(3, NZN)
               ENDDO                         ! Ultimo origen I
               call CheckListEnd(3, NZN)
            ENDDO                            ! Ultima categor¡a IP
            call CheckListEnd(3, NPROP)
            REWIND(3)
            CALL TOTAL(TTT,NZAG)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(1,TTT,NZAG)
            ELSE
               CALL ESCTAB(1,NZAG,TTT,TAB)
            ENDIF
         ENDDO                               ! Ultimo modo K
         CLOSE(3)
      ENDIF

!  OPCION 5 - Viajes por categor¡a de transporte de TRANS

      IF(IMP(5))THEN
         call OpenPolFile(3, IPOL, 'T4S', IO_BIN)
         CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)
         call CheckListBegin(3, NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip)
            MATAG=0
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8039,2)  ! VIAJES POR CATEGORIA DE TRANSPORTE
            WRITE(2,'(I3,1X,A8,6X,I4,6X,A3,2X,A3,I6,''-'',I2,''-'',I4,I6,' &
                     // ''':'',I2.2)')NUMCAT(IP),NOMCAT(IP),ITER, &
                     AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, i)
               call CheckListBegin(3, NZN)
               DO j=1,NZN
                  call CheckListItem(3, j)
                  call CheckListBegin(3, NTM)
                  DO K=1,NTM
                     call CheckListItem(3, k)
                     READ(3)TRIPS(J)
                     if(IAG(I).ne.0.and.IAG(J).ne.0)then
                       MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+TRIPS(J)
                     endif
                  ENDDO        ! Fin do modo k
                  call CheckListEnd(3, NTM)
               ENDDO ! Destinos J
               call CheckListEnd(3, NZN)
            ENDDO           ! Ultimo origen I
            call CheckListEnd(3, NZN)
            CALL TOTAL(TTT,NZAG)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(1,TTT,NZAG)
            ELSE
               CALL ESCTAB(1,NZAG,TTT,TAB)
            ENDIF
         ENDDO                                 ! Ultima categor¡a IP
         call CheckListEnd(3, NPROP)
         CLOSE(3)
      ENDIF

!  OPCION 6 - Viajes por categor¡a y modo de transporte de TRANS

      IF(IMP(6))THEN
         call OpenPolFile(3, IPOL, 'T4S', IO_BIN)
!Lee el encabezamiento primero para enterarse de NTM
         CALL RDTPAR(3, status,ITER,IDIA,MES,IAN,IHR,MINS)
         REWIND 3
         DO K=1,NTM
            CALL RDTPAR(3, status,ITER,IDIA,MES,IAN,IHR,MINS)
            call CheckListBegin(3, NPROP)
            DO IP=1,NPROP
               call CheckListItem(3, ip)
               MATAG=0

               IF(MODO(IP,K))THEN
               IF(OPSAL)WRITE(2,'(1X,A)')SI
               WRITE(2,'(/)')
               CALL MENSA(8040,2)  ! VIAJES POR MODO Y CAT DE TRANSPORTE
               WRITE(2,'(I3,1X,A8,I6,1X,A8,6X,I4,6X,A3,2X,A3,I6,''-'',I2,' // &
                        '''-'',I4,I6,'':'',I2.2)') &
                        NUMMOD(K),NOMMOD(K),NUMCAT(IP), &
                        NOMCAT(IP),ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS 
               WRITE(2,'(/)')
               ENDIF   ! Fin if modo

               call CheckListBegin(3, NZN)
               DO I=1,NZN
                  call CheckListItem(3, i)
                  call CheckListBegin(3, NZN)
                  do j=1,NZN
                     call CheckListItem(3, j)
                     call CheckListBegin(3, NTM)
                     DO K1=1,NTM
                        call CheckListItem(3, k1)
                        READ(3)TRIPS(J)
                        IF(K1.EQ.K)THEN
                           IF(IAG(I).le.0.OR.IAG(J).le.0)CYCLE
                           MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+TRIPS(J)
                        ENDIF
                     ENDDO             ! Ultimo modo K1
                     call CheckListEnd(3, NTM)
                  enddo                ! destinos j
                  call CheckListEnd(3, NZN)
               ENDDO                ! Ultimo origen I
               call CheckListEnd(3, NZN)
               IF(MODO(IP,K))THEN
                  CALL TOTAL(TTT,NZAG)
                  IF(TAB.EQ.' ')THEN
                     CALL ESCRIBE(1,TTT,NZAG)
                  ELSE
                     CALL ESCTAB(1,NZAG,TTT,TAB)
                  ENDIF
               ENDIF
            ENDDO                   ! Ultima categor¡a IP
            call CheckListEnd(3, NPROP)
         REWIND(3)
         ENDDO                      ! Ultimo modo K
         CLOSE(3)
      ENDIF


!  OPCION 7 - Viajes totales de TRANS

      IF(IMP(7))THEN
         MATAG=0
         call OpenPolFile(3, IPOL, 'T4S', IO_BIN)
         CALL RDTPAR(3, status,ITER,IDIA,MES,IAN,IHR,MINS)
         IF(OPSAL)WRITE(2,'(1X,A)')SI
         WRITE(2,'(/)')
         CALL MENSA(8041,2)  ! VIAJES TOTALES
         WRITE(2,'(I8,6X,A3,2X,A3,I6,''-'',I2,''-'',I4,'// &
                 'I6,'':'',I2.2)')                         &
                 ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS
         WRITE(2,'(/)')
         call CheckListBegin(3, NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip)
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, i)
               call CheckListBegin(3, NZN)
               DO j=1,NZN
                  call CheckListItem(3, j)
                  call CheckListBegin(3, NTM)
                  DO K=1,NTM
                     call CheckListItem(3, k)
                     READ(3)TRIPS(J)
                     IF(IAG(I).le.0.OR.IAG(J).le.0)CYCLE
                     MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+TRIPS(J)
                  ENDDO            ! Ultimo modo K
                  call CheckListEnd(3, NTM)
               enddo ! destinos j
               call CheckListEnd(3, NZN)
            ENDDO               ! Ultimo origen I
            call CheckListEnd(3, NZN)
         ENDDO                  ! Ultima categor¡a IP
         call CheckListEnd(3, NPROP)
         CALL TOTAL(TTT,NZAG)
         IF(TAB.EQ.' ')THEN
            CALL ESCRIBE(1,TTT,NZAG)
         ELSE
            CALL ESCTAB(1,NZAG,TTT,TAB)
         ENDIF
         CLOSE(3)
      ENDIF

!  OPCION 8 - Distribuci¢n de frecuencias por modo

      IF(IMP(8))THEN
         call OpenPolFile(3, IPOL, 'T4S', IO_BIN)
         CALL RDTPAR(3, status,ITER,IDIA,MES,IAN,IHR,MINS)
         call OpenPolFile(4, IPOL, 'T1S', IO_BIN)
         CALL RDTPAR(4,status,ITER,IDIA,MES,IAN,IHR,MINS)
         CALL FRECUEN(ESVER,OPSAL,AREA,POL,IDIA,MES,IAN,IHR,MINS,TAB,ITER)
         CLOSE(3)
      ENDIF  !  Fin opci¢n 8

!  OPCION 9 -  Flujos funcionales de LOC por sector socio-econ¢mico

      IF(IMP(9))THEN
         call OpenPolFile(3, IPOL, 'L2S', IO_BIN)
         READ(3)NZN,NFLU,IAN,MES,IDIA,IHR,MINS
         call ReadListBegin(3, NS)
         do m=1, NS
            call CheckListItem(3, m)
            read(3) NUMSEC(M),NOMSEC(M),LFLU(M)
         enddo
         call CheckListEnd(3, NS)
         call CheckListBegin(3, NS)
         DO IP=1,NS
            call CheckListItem(3, IP)
            read(3) LFLAG
            IF(.NOT.LFLU(IP))CYCLE
            MATAG=0
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8046,2)  ! FLUJOS POR SECTOR SOCIO-ECONOMICO
            WRITE(2,'(I3,2X,A6,4X,A3,2X,A3,I6,''-'',I2,''-'',I4,I6,'':'',' // &
                    'I2.2)')                                                  &
                    NUMSEC(IP),NOMSEC(IP),AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, i)
               read(3) LFLAG
               IF(JER1(I).NE.JER2(I))CYCLE
               DO J=1,NZN
                  READ(3)MAT(I,J)
                  IF(IAG(I).le.0.OR.IAG(J).le.0)CYCLE
                  MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+MAT(I,J)
               ENDDO
            ENDDO
            call CheckListEnd(3, NZN)
            CALL TOTAL(TTT,NZAG)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(1,TTT,NZAG)
            ELSE
               CALL ESCTAB(1,NZAG,TTT,TAB)
            ENDIF
         ENDDO
         call CheckListEnd(3, NS)
         CLOSE(3)
      ENDIF

!  OPCION 10 - Flujos por categor¡a de transporte (de FLUJ)

      IF(IMP(10))THEN
         call OpenPolFile(3, IPOL, 'F1S', IO_BIN)
         READ(3)NZN,IAN,MES,IDIA,IHR,MINS

         call ReadListBegin(3, NPROP)
         do m=1, NPROP
           call CheckListItem(3, m)
           read(3) NUMCAT(M),NOMCAT(M)
         enddo
         call CheckListEnd(3, NPROP)

         call ReadListBegin(3, NTM)
         do k=1, NTM
           call CheckListItem(3, k)
           read(3) NUMMOD(K),NOMMOD(K)
           call CheckListBegin(3, NPROP)
           do m=1, NPROP
             call CheckListItem(3, m)
             read(3) MODO(M,K)
           enddo
           call CheckListEnd(3, NPROP)
         enddo
         call CheckListEnd(3, NTM)

         call CheckListBegin(3, NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip, 'ip')
            MAT=0.
            MATAG=0
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8047,2)  ! FLUJOS POR CATEGORIA DE TRANSPORTE
            WRITE(2,'(I3,1X,A8,6X,A3,2X,A3,I6,''-'',I2,''-'',I4,I6,'':'','//  &
                    'I2.2)') &
                    NUMCAT(IP),NOMCAT(IP),AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, i, 'ip i')
               read(3) LFLAG           ! para ObjTranus
               IF(JER1(I).NE.JER2(I))CYCLE
               DO J=1,NZN
                 read(3) DMATCELL, A, A, (A, K=1, NTM)
                 MAT(I,J) = DMATCELL
               ENDDO
               DO J=1,NZN
                  IF(IAG(I).le.0.OR.IAG(J).le.0)CYCLE
                  MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+MAT(I,J)
               ENDDO
            ENDDO
            call CheckListEnd(3, NZN)
            CALL TOTAL(TTT,NZAG)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(1,TTT,NZAG)
            ELSE
               CALL ESCTAB(1,NZAG,TTT,TAB)
            ENDIF
         ENDDO
         call CheckListEnd(3, NPROP)
         CLOSE(3)
      ENDIF

!  OPCION 11 - Costos monetarios por categor¡a de transporte

      IF(IMP(11))THEN
         call OpenPolFile(3, IPOL, 'T1S', IO_BIN)
         CALL RDTPAR(3, status,ITER,IDIA,MES,IAN,IHR,MINS)
         call CheckListBegin(3, NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip)
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8043,2)  ! COSTOS MONETARIOS POR CAT DE TRANSPORTE
            WRITE(2,'(I3,1X,A8,6X,I4,6X,A3,2X,A3,I6,''-'',I2,''-'',I4,I6,' // &
                    ''':'',I2.2)')                                            &
                    NUMCAT(IP),NOMCAT(IP),ITER, &
                    AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            call CheckListBegin(3, NZN)
            DO I=1,NZN
               call CheckListItem(3, i)
               call CheckListBegin(3, NZN)
               do j=1,NZN
                  call CheckListItem(3, j)
                  READ(3)DA, DMATCELL
                  MAT(I,J) = DMATCELL
                  call CheckListBegin(3, NTM)
                  DO K=1,NTM
                     call CheckListItem(3, k)
                     READ(3) DA
                  ENDDO
                  call CheckListEnd(3, NTM)
               enddo
               call CheckListEnd(3, NZN)
            ENDDO   ! Fin do zonas I
            call CheckListEnd(3, NZN)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(0,TTT,NZN)
            ELSE
               CALL ESCTAB(0,NZN,TTT,TAB)
            ENDIF
         ENDDO      ! Fin do categor¡as IP
         call CheckListEnd(3, NPROP)
         CLOSE(3)
      ENDIF

!  OPCION 12 - Costos monetarios por sector s-e de COST

      IF(IMP(12))THEN
         call OpenPolFile(3, IPOL, 'C1S', IO_BIN)
         READ(3)NN,NFLU,IAN,MES,IDIA,IHR,MINS
         call ReadListBegin(3, NS)
         do ip=1, NS
            call CheckListItem(3, ip)
            read(3) NUMSEC(IP),NOMSEC(IP),LFLU(IP)
         enddo
         call CheckListEnd(3, NS)
         call CheckListBegin(3, NS)
         DO IP=1,NS
            call CheckListItem(3, ip)
            read(3) LFLAG ! repetido aqui para ObjTranus
            IF(.NOT.LFLU(IP))CYCLE
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8045,2)  ! COSTOS MONETARIOS POR SECTOR SOCIO-ECON
            WRITE(2,'(I3,2X,A6,4X,A3,2X,A3,I6,''-'',I2,''-'',' // &
                    'I4,I6,'':'',I2.2)')                          &
                    NUMSEC(IP),NOMSEC(IP),AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
             call CheckListBegin(3, NZN)
             DO I=1,NZN
                 call CheckListItem(3, I)
                 READ(3) (A, MAT(I,J), J=1,NZN)
             ENDDO
             call CheckListEnd(3, NZN)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(0,TTT,NZN)
            ELSE
               CALL ESCTAB(0,NZN,TTT,TAB)
            ENDIF
         ENDDO
         call CheckListEnd(3, NS)
         CLOSE(3)
      ENDIF


!  OPCION 13 - Viajes externos por categor¡a de transporte (de FLUJ)

      IF(IMP(13))THEN
         call OpenPolFile(3, IPOL, 'F1S', IO_BIN)
         READ(3)NZN,IAN,MES,IDIA,IHR,MINS

         call ReadListBegin(3, NPROP)
         do m=1, NPROP
           call CheckListItem(3, m)
           read(3) NUMCAT(M),NOMCAT(M)
         enddo
         call CheckListEnd(3, NPROP)

         call ReadListBegin(3, NTM)
         do k=1, NTM
           call CheckListItem(3, k)
           read(3) NUMMOD(K),NOMMOD(K)
           call CheckListBegin(3, NPROP)
           do m=1, NPROP
             call CheckListItem(3, m)
             read(3) MODO(M,K)
           enddo
           call CheckListEnd(3, NPROP)
         enddo
         call CheckListEnd(3, NTM)

         call CheckListBegin(3, NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip)
            MAT=0.
            MATAG=0.
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(8049,2)  ! VIAJES EXOGENOS POR CATEG DE TRANSPORTE
            WRITE(2,'(I3,1X,A8,6X,A3,2X,A3,I6,''-'','                // &
                    'I2,''-'',I4,I6,'':'',I2.2)')                       &
                    NUMCAT(IP),NOMCAT(IP),AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            call CheckListBegin(3, NZN)
            do i=1, NZN
               call CheckListItem(3, i)
               read(3) LFLAG           ! para ObjTranus
               IF(JER1(I).NE.JER2(I))CYCLE
               read(3) (DA, MAT(I,J), A, (A,k=1,NTM), J=1, NZN)
               DO J=1,NZN
                 IF(IAG(I).le.0.OR.IAG(J).le.0)CYCLE
                 MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+MAT(I,J)
               ENDDO
            ENDDO
            call CheckListEnd(3, NZN)
            CALL TOTAL(TTT,NZAG)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(1,TTT,NZAG)
            ELSE
               CALL ESCTAB(1,NZAG,TTT,TAB)
            ENDIF
         ENDDO
         call CheckListEnd(3, NPROP)
         CLOSE(3)
      ENDIF


!  OPCION 14 - Viajes externos por categor¡a y modo de transp (de FLUJ)

      IF(IMP(14))THEN
         call FindPolFile(3, IPOL, 'P0E', IO_FMT,POL_ANY)
         CALL LEEP0E
         CLOSE(3)
         call OpenPolFile(3, IPOL, 'F1S', IO_BIN)
         DO K=1,NTM
            READ(3)NZN,IAN,MES,IDIA,IHR,MINS
     
            call ReadListBegin(3, NPROP)
            do m=1, NPROP
              call CheckListItem(3, m)
              read(3) NUMCAT(M),NOMCAT(M)
            enddo
            call CheckListEnd(3, NPROP)

            call ReadListBegin(3, NTM)
            do k1=1, NTM
              call CheckListItem(3, k1)
              read(3) NUMMOD(K1),NOMMOD(K1)
              call CheckListBegin(3, NPROP)
              do m=1, NPROP
                call CheckListItem(3, m)
                read(3) MODO(M,K1)
              enddo
              call CheckListEnd(3, NPROP)
            enddo
            call CheckListEnd(3, NTM)

            call CheckListBegin(3, NPROP)
            DO IP=1,NPROP
               call CheckListItem(3, ip, 'cat i')
               MATAG=0
               IF(MODO(IP,K))THEN
                  IF(OPSAL)WRITE(2,'(1X,A)')SI
                  WRITE(2,'(/)')
                  CALL MENSA(8050,2)  ! VIAJES EXOGS POR CAT Y MODO DE TRANS
                  WRITE(2,'(I3,1X,A8,I3,1X,A,6X,A3,2X,A3,I6,''-'',I2,' &
                     // '''-'',I4,I6,'':'',I2.2)') &
                     NUMCAT(IP),NOMCAT(IP),NUMMOD(K), &
                     NOMMOD(K),AREA,POL,IDIA,MES,IAN,IHR,MINS
                  WRITE(2,'(/)')
               ENDIF
               call CheckListBegin(3, NZN, 'from zones')
               DO I=1,NZN
                  call CheckListItem(3, i, 'from zone i')
                  read(3) LFLAG           ! para ObjTranus
                  IF(JER1(I).NE.JER2(I))CYCLE
                  READ(3) (DA,A,A, (AUX1(J,k1), k1=1,NTM), J=1,NZN)
                  DO J=1,NZN
                     MAT(I,J)=AUX1(J,K)
                  ENDDO
                  DO J=1,NZN
                     IF(IAG(I).LE.0.OR.IAG(J).LE.0)CYCLE
                     MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+MAT(I,J)
                  ENDDO
               ENDDO      ! Fin do zonas I
               call CheckListEnd(3, NZN, 'from zones')
               IF(.NOT.MODO(IP,K))CYCLE
               CALL TOTAL(TTT,NZAG)
               IF(TAB.EQ.' ')THEN
                  CALL ESCRIBE(1,TTT,NZAG)
               ELSE
                  CALL ESCTAB(1,NZAG,TTT,TAB)
               ENDIF
            ENDDO         ! Fin do categor¡as IP
            call CheckListEnd(3, Nprop, 'CATEGORIES')
            REWIND(3)
         ENDDO            ! Fin do modos K
         CLOSE(3)
      ENDIF

!  OPCION 15 - Viajes de retorno categor¡a de transporte (de FLUJ)

      IF(IMP(15))THEN
         call OpenPolFile(3, IPOL, 'F1S', IO_BIN)
         READ(3)NZN,IAN,MES,IDIA,IHR,MINS

         call ReadListBegin(3, NPROP)
         do m=1, NPROP
           call CheckListItem(3, m)
           read(3) NUMCAT(M),NOMCAT(M)
         enddo
         call CheckListEnd(3, NPROP)

         call ReadListBegin(3, NTM)
         do k=1, NTM
           call CheckListItem(3, k)
           read(3) NUMMOD(K),NOMMOD(K)
           call CheckListBegin(3, NPROP)
           do m=1, NPROP
             call CheckListItem(3, m)
             read(3) MODO(M,K)
           enddo
           call CheckListEnd(3, NPROP)
         enddo
         call CheckListEnd(3, NTM)

         call CheckListBegin(3, NPROP)
         DO IP=1,NPROP
            call CheckListItem(3, ip)
            MAT=0.
            IF(OPSAL)WRITE(2,'(1X,A)')SI
            WRITE(2,'(/)')
            CALL MENSA(28049,2)  ! VIAJES EXOGENOS POR CATEG DE TRANSPORTE
            WRITE(2,'(I3,1X,A8,6X,A3,2X,A3,I6,''-'',I2,'// &
                    '''-'',I4,I6,'':'', I2.2)')            &
                    NUMCAT(IP),NOMCAT(IP),AREA,POL,IDIA,MES,IAN,IHR,MINS
            WRITE(2,'(/)')
            call CheckListBegin(3, NZN)
            do i=1, NZN
               call CheckListItem(3, i)
               read(3) LFLAG           ! para ObjTranus
               IF(JER1(I).NE.JER2(I))CYCLE
               read(3) (A, A, MAT(I,J), (A,k=1,NTM), J=1, NZN)
               DO J=1,NZN
                 IF(IAG(I).le.0.OR.IAG(J).le.0)CYCLE
                 MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+MAT(I,J)
               ENDDO
            ENDDO
            call CheckListEnd(3, NZN)
            CALL TOTAL(TTT,NZAG)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(0,TTT,NZAG)
            ELSE
               CALL ESCTAB(0,NZAG,TTT,TAB)
            ENDIF
         ENDDO
         call CheckListEnd(3, NPROP)
         CLOSE(3)
      ENDIF

      IF(OPSAL)THEN
         WRITE(2,'(1X,A)')SI
         SI=CHAR(18)
         WRITE(2,'(1X,A)')SI
      ENDIF

      if (docOutput) call DOCEnd(2)
      close(2)
      if (debugging >= dbg_Normal) then
         CALL MENSA(8,0)  ! FINAL NORMAL DE
         WRITE(*,*) 'M A T S'
      endif
      STOP 0


CONTAINS
      SUBROUTINE ESCRIBE(N0,TTT,N3)
      INTEGER N0, N3
      real(8) :: TTT

      CHARACTER*40 FORM,FORM1,FORM2,FORM3
      CHARACTER*1 RAYA
      CHARACTER*(2) :: ID(15)
      CHARACTER*1 SI
      DATA ID/'01','02','03','04','05','06','07','08','09','10','11','12','13','14','15'/

!  N0 = 0 matriz real con un decimal;  =1 matriz entera
      SI=CHAR(12)
      RAYA=CHAR(205)
            !         1         2         3 
            !123456789012345678901234567890
      FORM= '(1X,I3,1X,A8,12F12.2)'
      FORM1='(5H TOT ,8X, 12F12.2)'
      FORM2='(5H ZON ,8X, 12I12,9H    T O T)'
      FORM3='(16X,12A12)'
      N1=1
      N2=N3
      IF(N3.GT.12)N2=12

!  Escribe una pagina

2     FORM(14:15)=ID(N2-N1+2)
      FORM1(14:15)=ID(N2-N1+2)
      FORM2(14:15)=ID(N2-N1+2)
      IF(N0.EQ.0.OR.N2.NE.N3)THEN
         FORM2(26:30)='       '
         WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+15)
         WRITE(2,FORM2)(NUMAG(I),I=N1,N2)
         WRITE(2,FORM3)(NOMAG(I),I=N1,N2)
         WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+15)
      ELSE
         FORM2(26:30)='T O T'
         WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+25)
         WRITE(2,FORM2)(NUMAG(I),I=N1,N2)
         WRITE(2,FORM3)(NOMAG(I),I=N1,N2)
         WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+25)
      ENDIF
300   FORMAT(' ', 200A)
      WRITE(2,*)
      DO I=1,N3
         IF(N0.EQ.0)THEN
            WRITE(2,FORM)NUMAG(I),NOMAG(I),(MAT(I,J),J=N1,N2)
         ELSE
            IF(N2.NE.N3)THEN
                WRITE(2,FORM)NUMAG(I),NOMAG(I),(MATAG(I,J),J=N1,N2)
            ELSE
                WRITE(2,FORM)NUMAG(I),NOMAG(I),(MATAG(I,J),J=N1,N2),FF(I)
            ENDIF
         ENDIF
      ENDDO
      IF(N0.EQ.0.OR.N2.NE.N3)THEN
         WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+15)
      ELSE
         WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+25)
      ENDIF
      IF(N0.EQ.1)THEN
         IF(N2.NE.N3)THEN
            WRITE(2,FORM1)(CC(J),J=N1,N2)
         ELSE
            WRITE(2,FORM1)(CC(J),J=N1,N2),TTT
         ENDIF
      ENDIF
      IF(N2.EQ.N3)RETURN
      N1=N1+12
      N2=N2+12
      IF(N2.GT.N3)N2=N3
      WRITE(2,600)SI
600   FORMAT(' ', A)
      GO TO 2
      END SUBROUTINE


      subroutine EscDocTable(iun, N0,N3,TTT)
          integer iun
          INTEGER N0, N3
          CHARACTER*1 TAB
          real(8) :: TTT


!  N0 = 0 matriz real con dos decimales;  =1 matriz entera
!  N3 = N£mero de zonas a grabar

         call DocTableStart(iun)
         call DocTableRowStart(iun)
           call DocTableHeadChar(iun, ' ')
           call DocTableHeadChar(iun, ' ')
           do j = 1, N3
             call DocTableHeadInt(iun, NUMAG(j))
           enddo
           call DocTableHeadChar(iun, ' ')
         call DocTableRowEnd(iun)
         call DocTableRowStart(iun)
           call DocTableHeadChar(iun, 'N')
           call DocTableHeadChar(iun, 'Name')
           do j = 1, N3
             call DocTableHeadChar(iun, NOMAG(j))
           enddo
           call DocTableHeadChar(iun, ' ')
         call DocTableRowEnd(iun)

         do i= 1, N3
           call DocTableRowStart(iun)
           call DocTableCellInt( iun, NUMAG(i), .true.)
           call DocTableCellChar(iun, NOMAG(i), .true.)
           do j = 1, N3
             if (N0 == 0) then
               call DocTableCellReal(iun, MAT(i,j))
             else
               call DocTableCellDouble(iun, MATAG(i,j))
             endif
           enddo
           if (N0 /= 0) then
             call DocTableCellDouble(iun, FF(i))
           endif
           call DocTableRowEnd(iun)
         enddo

         if (N0 /= 0) then
           call DocTableRowStart(iun)
           call DocTableCellChar(iun, ' ',     .true.)
           call DocTableCellChar(iun, 'TOTAL', .true.)
           do j = 1, N3
             call DocTableCellDouble(iun, CC(j))
           enddo
           call DocTableCellDouble(iun, TTT)
           call DocTableRowEnd(iun)
         endif

         call DocTableEnd(iun)
      end subroutine

      SUBROUTINE ESCTAB(N0,N3,TTT,TAB)
          INTEGER N0, N3
          CHARACTER*1 TAB
          real(8) :: TTT


!  N0 = 0 matriz real con dos decimales;  =1 matriz entera
!  N3 = N£mero de zonas a grabar

      WRITE(2,'(5H ZON ,1000(A,I8))')(TAB,NUMAG(I),I=1,N3)
      WRITE(2,'(5H     ,1000(A))')(TAB,NOMAG(I),I=1,N3)
      DO I=1,N3
         IF(N0.EQ.0)THEN
           WRITE(2,'(1X,I3,1X,A,1000(A,F12.2))') &
             NUMAG(I),NOMAG(I),(TAB,MAT(I,J),J=1,N3)
         ELSE
           WRITE(2,'(1X,I3,1X,A,1000(A,F12.2))') &
             NUMAG(I),NOMAG(I),(TAB,MATAG(I,J),J=1,N3),TAB,FF(I)
         ENDIF
      ENDDO

      IF(N0.NE.0)THEN
         WRITE(2,'(5H TOT ,1000(A,F12.2))')(TAB,CC(J),J=1,N3),TAB,TTT
      ENDIF

      RETURN
      END SUBROUTINE




      SUBROUTINE FRECUEN(ESVER,OPSAL,AREA,POL,IDIA,MES,IAN,IHR,MINS,TAB,ITER)
      integer(2) :: IDIA,MES,IAN,IHR,MINS

      CHARACTER*1 SI,TAB
      CHARACTER*3 AREA,POL
      LOGICAL     OPSAL
      DIMENSION   FREQ(35)
      real(8) :: DESUT(MXZON),VIAJES(MXZON)

      SI=CHAR(15)
      ESHOR=0.

      call CheckListBegin(3, NPROP)
      call CheckListBegin(4, NPROP)
      DO IP=1,NPROP       !  Iteraciones r/categor¡a transporte
         call CheckListItem(3, ip)
         call CheckListItem(4, ip)
         TOT=0.
         PORC=0.
         FREQ=0.
         MAX=0
         MIN=9999

!  Escribe el encabezamiento para cada categor¡a
         IF(OPSAL.AND.IP.GT.1)WRITE(2,'(1X,A)')SI
         WRITE(2,'(/)')
         CALL MENSA(8051,2)  ! DISTRIBUCION FRECUENCIA X CATEGORIA
         WRITE(2,'(I3,1X,A8,6X,I4,6X,A3,2X,A3,I6,''-'',I2,' &
           // '''-'',I4,I6,'':'',I2.2)')NUMCAT(IP),NOMCAT(IP),ITER, &
           AREA,POL,IDIA,MES,IAN,IHR,MINS
         WRITE(2,'(/)')

!  Iteraciones r/origen
         call CheckListBegin(3, NZN)
         call CheckListBegin(4, NZN)
         DO I=1,NZN
            call CheckListItem(3, i)
            call CheckListItem(4, i)
            call CheckListBegin(3, NZN)
            call CheckListBegin(4, NZN)
            TRIPS=0.
            do j=1, NZN
              call CheckListItem(3, j)
              call CheckListItem(4, j)
               READ(4)DESUT(J),DA ! Lee desutilidades x cat de T1S
               call CheckListBegin(3, NTM)
               call CheckListBegin(4, NTM)
               DO K=1,NTM
                  call CheckListItem(3, k)
                  call CheckListItem(4, k)
                  READ(3)VIAJES(J)      ! Lee viajes x modo k de T4S
                  READ(4)DA
                  TRIPS(J)=TRIPS(J)+VIAJES(J) ! Acumula viajes tot cat IP
               ENDDO  ! Fin do modo k
               call CheckListEnd(3,NTM)
               call CheckListEnd(4,NTM)
            enddo
            call CheckListEnd(3, NZN)
            call CheckListEnd(4, NZN)
            DO J=1,NZN
               IF(I.EQ.J.OR.JER1(J).NE.JER2(J))CYCLE
               L=INT(DESUT(J)/ESVER+0.5)  ! Identifica el tramo de costo
               IF(L.LT.1)L=1
               IF(L.GT.35)L=35
               IF(L.GT.MAX)MAX=L
               IF(L.LT.MIN)MIN=L
               FREQ(L)=FREQ(L)+TRIPS(J)  ! Asigna los viajes al tramo
               TOT=TOT+TRIPS(J)          ! Total viajes contabilizados
            ENDDO  ! Fin zonas J
         ENDDO  ! Fin zonas I
         call CheckListEnd(3, NZN)
         call CheckListEnd(4, NZN)

! Recorre los tramos, calcula % e imprime

         WRITE(2,'(''       COST      %         No'')')
         DO I=MIN,MAX
            RI=I*ESVER
            PORC=FREQ(I)/TOT*100.
            WRITE(2,1001)RI,TAB,PORC,TAB,FREQ(I)
1001        FORMAT(' ',F10.2,A,F6.2,A,F10.0)
         ENDDO  ! Fin tramos I
         RI=100.
         WRITE(2,'(''      TOTAL'',A,F6.2,A,F10.0)')TAB,RI,TAB,TOT

      ENDDO  ! Fin categor¡a IP
      call CheckListEnd(3, NPROP)
      call CheckListEnd(4, NPROP)

      RETURN
      END SUBROUTINE


      SUBROUTINE TOTAL(TTT,N3)

      real(8) :: TTT

      TTT=0
      DO I=1,N3
         CC(I)=0
         FF(I)=0
      ENDDO
      DO I=1,N3
         DO J=1,N3
            CC(J)=CC(J)+MATAG(I,J)
            FF(I)=FF(I)+MATAG(I,J)
         ENDDO
         TTT=TTT+FF(I)
      ENDDO
      RETURN
      END SUBROUTINE



      SUBROUTINE LEEDAT(NZAG,IMP,ESVER)

      INTEGER IAUX(MXZON)
      logical(1) :: IMP(14)
      CHARACTER*(1) TIT
      CHARACTER*(32) NOM
     
      IMP=.FALSE.

!  SECCION 1.0 - Encabezamiento y las opciones 

      READ(3,'(///)',END=999)
      DO I=1,8
         READ(3,*,END=999,ERR=100,IOSTAT=IOS)TIT,I1
         IF(I1.EQ.1)IMP(I)=.TRUE.
      ENDDO
      READ(3,*,END=999,ERR=100,IOSTAT=IOS)TIT,ESVER
      DO I=9,15  ! una de m s para que caiga en CHECK
         READ(3,*,END=999,ERR=100,IOSTAT=IOS)TIT,I1
         IF(I1.EQ.1)IMP(I)=.TRUE.
      ENDDO
100   CALL CHECK(1.0,IOS,'MAT')

!  SECCION 2.0 - Agregaci¢n de zonas

      READ(3,'(//)',END=999)
      NZAG=0
      DO J=1,NZN
         IAUX(J)=0
         IAG(J)= 0
      ENDDO
201   READ(3,*,END=999,ERR=200,IOSTAT=IOS)NN,NOM,(IAUX(J),J=1,NZN)
      write(*,*) NN, NOM

!  Si la macrozona es cero, la lista de zonas IAUX ser  ignorada
      IF(NN.EQ.0)THEN
         DO J=1,NZN
            IF(IAUX(J).EQ.0)EXIT
            IAUX1=INTNUM(IAUX(J),NUMZON,NZN)
               IF(IAUX1.GT.NZN)THEN
                  WRITE(*,*)'MATS(2.0) - Zon:',IAUX(J)
                  CALL MENSA(6,-1)
               ENDIF
            IAG(IAUX1)=-1
            IAUX(J)=0 ! Porsi, blanquea
         ENDDO
         GO TO 201
      ENDIF
      NZAG=NZAG+1
      IF(NZAG.GT.MXZON)THEN
         WRITE(*,*)'MAT(2.0) - No Zon',NZAG,'  Max',MXZON
         CALL MENSA(9,-1)
      ENDIF
      NUMAG(NZAG)=NN
      NOMAG(NZAG)=NOM

      DO J=1,NZN
         IF(IAUX(J).EQ.0)EXIT ! del DO J
         IAUX1=INTNUM(IAUX(J),NUMZON,NZN)
            IF(IAUX1.GT.NZN)THEN
               WRITE(*,*)'MATS(2.0) - Zon:',IAUX(J)
               CALL MENSA(6,-1)
            ENDIF
         IAG(IAUX1)=NZAG
! OJO: Bea: la siguiente instrucci¢n porsi queda basura en vector IAUX(J)
         IAUX(J)=0
      ENDDO
      GOTO 201
200   CALL CHECK(2.0,IOS,'MAT')

!  Las zonas que no hayan sido incluidas en ninguna lista son conservadas
!  como si cada una fuera una macrozona de si misma

       DO J=1,NZN
          IF(JER1(J).NE.JER2(J))CYCLE
          IF(IAG(J).EQ.0)THEN ! J no pertenece a ning£n grupo
             NZAG=NZAG+1          ! J es un grupo ella misma
             IF(NZAG.GT.MXZON)THEN
                WRITE(*,*)'MAT(2.0) - No Zon',NZAG,'  Max',MXZON
                CALL MENSA(9,-1)
             ENDIF
             IAG(J)=NZAG
             NUMAG(NZAG)=NUMZON(J) ! No y nombre de la zona original J
             NOMAG(NZAG)=NOMZON(J) ! para el grupo individual que forma
          ENDIF  ! IAG(J)=0
       ENDDO ! Do J

      RETURN

999   WRITE(*,*)'MATS.DAT'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto
      END SUBROUTINE


subroutine GetOptions(SAL)
 character SAL*(*)
      useMatsDat = .false.
      docOutput  = .false.

      if (.not. hasopts()) then
          return
      endif
      IMP = .false.
      do i = 1, optc()
          select case(optv(i))
              case ('D')
                 IMP(1) = .true.
              case ('M')
                 IMP(2) = .true.
              case ('S')
                 IMP(3) = .true.
              case ('P')
                 IMP(4) = .true.
              case ('Q')
                 IMP(5) = .true.
              case ('R')
                 IMP(6) = .true.
              case ('T')
                 IMP(7) = .true.
              case ('O')
                 IMP(8) = .true.
              case ('E')
                 IMP(9) = .true.
              case ('F')
                 IMP(10) = .true.
              case ('C')
                 IMP(11) = .true.
              case ('K')
                 IMP(12) = .true.
              case ('X')
                 IMP(13) = .true.
              case ('Y')
                 IMP(14) = .true.
              case ('d')
                 useMatsDat = .true.
                 call verbose('using MATS.DAT ')
              case ('o')
                 SAL=trim(optarg(i))
                 if (SAL == '-') then
                    SAL = 'CON'
                 endif
              case ('x')
                 DocOutput = .true.
                 call verbose('producing Excel output')
              case ('g','q','v')
                ! nothing
              case default
                 call usage
          end select
      enddo
      SAL = trim(SAL)
 return
 end subroutine

END PROGRAM MATS


 subroutine Usage
 USE GETOPTM
 character*32 prog
    prog = argv(0)

    print *
    print '(A,'' - Transport Matrix reports for TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options]'')', trim(prog)
    print *
    print *, 'If no commands are given, the program enters interactive mode'


    print *
    print *, 'Commands are:'
    print *, '  -D        : Disut. by transport category'
    print *, '  -M        : Disut. by mode and transport category'
    print *, '  -S        : Disut. by socio-economic sector'
    print *, '  -P        : Trips by mode'
    print *, '  -Q        : Trips by transport category'
    print *, '  -R        : Trips by mode and transport category'
    print *, '  -T        : Total trips (sum of categories)'
    print *, '  -O        : Frequency distribution of trips by mode'
    print *, '  -E        : Flows by socio-economic sector'
    print *, '  -F        : Flows by transport category'
    print *, '  -C        : Costs by transport category'
    print *, '  -K        : Costs by socio-economic sector'
    print *, '  -X        : Exogenous trips by transport category'
    print *, '  -Y        : Exogenous trips by category and mode'

    print *
    print *, 'Options are:'
    print *, '  -d           : Read options from MATS.DAT'
    print *, '  -o <name>    : Write output to file <name>. Default is "AREAPOL.MTX"'
    print *, '  -x           : Write output in Excel format'
    call ExplainStdOptions

    STOP 02
 end subroutine

