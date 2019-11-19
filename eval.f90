! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2019 Modelistica, Caracas
! * Copyright (C) 1983-2019 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM EVAL
USE DEBUGM
USE GETOPTM
USE PARAM
USE GENER
USE CONTROL
USE IO_LIST
USE TPARC
USE ZCOMM
USE MENSAMOD

      character(80) :: EVAL_RCS_ID = & 
        "$Id$"

      CHARACTER    FILE1*11, SI*1
      CHARACTER(128) :: SALIDA
      CHARACTER(32)  :: CASBASE, CASALTER
      INTEGER      iTrash, status
      logical(1) :: OPSAL
      
      INTEGER    fT1SBas, fT1SAlt, fT4SBas,  fT4SAlt
      PARAMETER( fT1SBas = 10, fT1SAlt = 11, &
                 fT4SBas = 12, fT4SAlt =  13  &
                 )

      PARAMETER(MXANOS=40)

      INTEGER  NPER            ! Number of evaluation periods
      integer ::             &
        NUMPER(MXANOS),      & ! Year of each period
        iPolBase(MXANOS),    &
        iPolAlter(MXANOS)

      character(8) ::        &
        POLBASE(MXANOS),     & ! Code of the reference case of each period
        POLALTER(MXANOS)       ! Code of the alternative case of each period

      integer(4) :: MAT
      real(8) :: UTIBAS, UTIALT, A8, AUX1, AUX2
      DIMENSION &
        MAT(MXZON,MXZON),     & ! Matrix of surplus
        UTIBAS(MXZON),        & ! Disutilities reference case
        UTIALT(MXZON),        & ! Disutilities alternative case
        VIABASE(MXZON),       & ! Trips reference case
        VIAALTER(MXZON),      & ! Trips alternative case
        AUX1(MXZON),          & ! Auxiliar
        AUX2(MXZON),          & ! Auxiliar
        ZONEXE(MXZON,MXPROP)    ! Surplus by zone and category

        integer(2) :: I2
        integer :: IAN,MES,IDIA, IHR,MINS,ISEC,MILESM

        common/evalc/ MAT, UTIBAS, UTIALT, AUX1, AUX2, ZONEXE, &
                     VIABASE, VIAALTER

      external Usage

      call debug('start')

      if (getopts(STD_OPTIONS, Usage)) then
         call doStdOpts(Usage)
      endif

!  Asks for the base and alternative cases

      call Ident(8200) ! Identification of program eval
      call InitPol('   ',iTrash)
      WRITE(*,*)
      WRITE(*,*)
      ! CALL MENSA(8201,0)   ! Identify base and alternative cases
      ! WRITE(*,*)
      ! WRITE(*,'(''       BASE? ''$)')
      ! READ(*,'(A)')CASBASE
      ! WRITE(*,'(''      ALTER? ''$)')
      ! READ(*,'(A)')CASALTER  
      ! call Upper(CasBase)
      ! call Upper(CasAlter)

      CALL GetCurrentDate(IAN,MES,IDIA)
      CALL GetCurrentTime(IHR,MINS,ISEC,MILESM)

!  READS ZONES FILE Z1E

      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)


      CALL GetScenarios()



!  HEADING

      CALL MENSA(8200,2)   ! Identification of program EVAL
      WRITE(2,*)ESTUDIO

!  reads  E1E

!  CALCULATES THE SURPLUS IN EACH SCENARIO 

      DO IPER=1,NPER   ! Iterates over periods

      CASBASE = POLBASE(NPER)
      CASALTER = POLALTER(NPER)

     ! Specifies the output file
      OPSAL=.FALSE.
      SALIDA = trim(AREA) // trim(CASBASE) // '-'// trim(CASALTER) // '.EVL'
      SI=CHAR(15)
      CALL SALI(SALIDA)
      IF(SALIDA(1:3).EQ.'PRN')OPSAL=.TRUE.
      IF(OPSAL)WRITE(2,'('' '',A)')SI

      WRITE(2,*)
      CALL MENSA(8203,2)

      CALL MENSA(8202,2)   ! Area Base Alter Date...
      WRITE(2,'(1X,A,7X,A,9X,A,7X,I2.2,''-'',I2.2,'               // &
              '''-'',I4,1X,I2.2,'':'',I2.2)')                        &
              AREA,CASBASE,CASALTER,IDIA,MES,IAN,IHR,MINS

!  Abre los 4 archivos: costos base y alt (t1s) y viajes base y alt (t4s)
!  y luego lee los ecabezamientos de cada uno

         call OpenPolFile(fT1SBas, iPolBase(iPer),  'T1S', IO_BIN)
         CALL RDTPAR(fT1SBas, status,I,I2,I2,I2,I2,I2)

         call OpenPolFile(fT1SAlt, iPolAlter(iPer), 'T1S', IO_BIN)
         CALL RDTPAR(fT1SAlt, status,I,I2,I2,I2,I2,I2)

         call OpenPolFile(fT4SBas, iPolBase(iPer),  'T4S', IO_BIN)
         CALL RDTPAR(fT4SBas, status,I,I2,I2,I2,I2,I2)

         call OpenPolFile(fT4SAlt, iPolAlter(iPer), 'T4S', IO_BIN)
         CALL RDTPAR(fT4SAlt, status,I,I2,I2,I2,I2,I2)

         call CheckListBegin(fT1SBas, NPROP)
         call CheckListBegin(fT1SAlt, NPROP)
         call CheckListBegin(fT4SBas, NPROP)
         call CheckListBegin(fT4SAlt, NPROP)
         DO ip=1,NPROP
            call CheckListItem(fT1SBas, ip)
            call CheckListItem(fT1SAlt, ip)
            call CheckListItem(fT4SBas, ip)
            call CheckListItem(fT4SAlt, ip)
            EXED=RCERO                        ! Excedente total x categ
            call CheckListBegin(fT1SBas, NZN)
            call CheckListBegin(fT1SAlt, NZN)
            call CheckListBegin(fT4SBas, NZN)
            call CheckListBegin(fT4SAlt, NZN)
            DO I=1,NZN
               call CheckListItem(fT1SBas, i)
               call CheckListItem(fT1SAlt, i)
               call CheckListItem(fT4SBas, i)
               call CheckListItem(fT4SAlt, i)

               ZONEXE(I,ip)=RCERO          ! Exedente x zona y categor¡a
               call CheckListBegin(fT1SBas, NZN)
               call CheckListBegin(fT1SAlt, NZN)
               call CheckListBegin(fT4SBas, NZN)
               call CheckListBegin(fT4SAlt, NZN)

               ViaBase=rcero
               ViaAlter=rcero

               do j =1, NZN
                  call CheckListItem(fT1SBas, j)
                  call CheckListItem(fT1SAlt, j)
                  call CheckListItem(fT4SBas, j)
                  call CheckListItem(fT4SAlt, j)

                  READ(fT1SBas)UTIBAS(J),A8
                  READ(fT1SAlt)UTIALT(J),A8

                  call CheckListBegin(fT1SBas, NTM)
                  call CheckListBegin(fT1SAlt, NTM)
                  call CheckListBegin(fT4SBas, NTM)
                  call CheckListBegin(fT4SAlt, NTM)
                  DO K=1,NTM
                     call CheckListItem(fT1SBas, k)
                     call CheckListItem(fT1SAlt, k)
                     call CheckListItem(fT4SBas, k)
                     call CheckListItem(fT4SAlt, k)

                     READ(fT1SBas)A8      ! Salta utilids por modo base
                     READ(fT1SAlt)A8      ! Salta utilids por modo alter
                     READ(fT4SBas)AUX1(J) ! Lee los viajes por modo base
                     READ(fT4SAlt)AUX2(J) ! Lee los viajes por modo alter
                     ViaBase(J)=ViaBase(J)+AUX1(J)   ! Acumula viajes base
                     ViaAlter(J)=ViaAlter(J)+AUX2(J) ! Acumula viajes alter
                  ENDDO       ! Fin do modos k
                  call CheckListEnd(fT1SBas, NTM)
                  call CheckListEnd(fT1SAlt, NTM)
                  call CheckListEnd(fT4SBas, NTM)
                  call CheckListEnd(fT4SAlt, NTM)
               ENDDO
               call CheckListEnd(fT1SBas, NZN)
               call CheckListEnd(fT1SAlt, NZN)
               call CheckListEnd(fT4SBas, NZN)
               call CheckListEnd(fT4SAlt, NZN)

!  Calcula el excedente de cada par o-d y acumula variables
               DO J=1,NZN
                  IF(JER1(J).NE.JER2(J))CYCLE
                  A1=(UTIBAS(J)-UTIALT(J))*(ViaBase(J)+ViaAlter(J))*0.5
                  EXED=EXED+A1
                  ZONEXE(I,ip)=ZONEXE(I,ip)+A1
                  MAT(I,J)=A1+0.5
               ENDDO

            ENDDO   ! Fin do zona de origen I
            call CheckListEnd(fT1SBas, NZN)
            call CheckListEnd(fT1SAlt, NZN)
            call CheckListEnd(fT4SBas, NZN)
            call CheckListEnd(fT4SAlt, NZN)

! Manda a grabar cada matriz resultante por categoria ip
            WRITE(2,*)
            WRITE(2,'('' CAT'',I5,2X,A)')NUMCAT(ip),NOMCAT(ip)
            IF(TAB.EQ.' ')THEN
               CALL ESCRIBE(ip)
               WRITE(2,*)
               WRITE(2,'('' TOTAL='',F12.0)')EXED
            ELSE
               CALL ESCTAB(ip,TAB)
            ENDIF

         ENDDO      ! Fin do categor¡a ip
         call CheckListEnd(fT1SBas, NPROP)
         call CheckListEnd(fT1SAlt, NPROP)
         call CheckListEnd(fT4SBas, NPROP)
         call CheckListEnd(fT4SAlt, NPROP)

         close(fT1SBas)
         close(fT1SAlt)
         close(fT4SBas)
         close(fT4SAlt)

      ENDDO     ! Fin do per¡odos iper

      CALL MENSA(8,0)  ! FINAL NORMAL DE
      STOP' E V A L'
100   WRITE(*,*)'CONTROL(1)'
      CALL MENSA(3,-1)   ! E-G03: Problema de lectura en archivo entrada
999   WRITE(*,*)'CONTROL'
      CALL MENSA(10,-1)  ! E-G06: Archivo incompleto


CONTAINS

      SUBROUTINE GetScenarios()
!     =================
!     LEE EL ARCHIVO DE PARAMETROS DE EVALUACION E1E

      CHARACTER*32 NOM1,NOM2

      call debug('getting scenarios')

      if (paramc() < 2) then
         call debug('too few escenarios')
         call Usage
         call mensa(msg_TwoScenariosRequired, mensa_Aborta)
      endif

      NPER=1
      N1=1
      NOM1=paramv(1)
      NOM2=paramv(2) 

      NUMPER(NPER)=N1       ! N£mero de cada a¤o
      call FindPol(NOM1, iPolBase(nPer))
      if (iPolBase(nPer).le.0) then
         WRITE(*,111) NOM1
111      format(' Pol ="',A)         
         call mensa(msg_PoliticaNoDefinida, mensa_Aborta)
      endif
      call FindPol(NOM2, iPolAlter(nPer))
      if (iPolAlter(nPer).le.0) then
         WRITE(*,111) NOM2
         call mensa(msg_PoliticaNoDefinida, mensa_Aborta)
      endif
      POLBASE(NPER)=NOM1    ! C¢digos del caso base
      POLALTER(NPER)=NOM2   ! C¢digos del esc alternativo

      RETURN

999   WRITE(*,*)'E1E'
      CALL MENSA(10,-IUNO)
      END SUBROUTINE


      SUBROUTINE ESCRIBE(ip)
!     ==================

      CHARACTER FORM*23,RAYA*1
      CHARACTER*(2) ID(15)
      CHARACTER*1 SI
      INTEGER N3
      DATA ID/'01','02','03','04','05','06','07','08','09','10','11','12','13','14','15'/

      N3=NZN
      SI=CHAR(12)
      RAYA=CHAR(205)
      FORM='(1X,I3,1X,12I8  ,F10.0)'
      N1=1
      N2=N3
      IF(N3.GT.12)N2=12

!  Escribe una pagina

2     FORM(11:12)=ID(N2-N1+1)
      WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+17)
      WRITE(2,'('' ZON'',12I8)')(NUMZON(I),I=N1,N2)
      WRITE(2,300)(RAYA,I=1,(N2-N1+1)*8+17)
300   FORMAT(' ',120A)
      WRITE(2,*)
      DO I=1,N3
         IF(N2.NE.N3)THEN
             WRITE(2,FORM)NUMZON(I),(MAT(I,J),J=N1,N2)
         ELSE
             WRITE(2,FORM)NUMZON(I),(MAT(I,J),J=N1,N2),ZONEXE(I,ip)
         ENDIF
      ENDDO
      IF(N2.EQ.N3)RETURN
      N1=N1+12
      N2=N2+12
      IF(N2.GT.N3)N2=N3
      WRITE(2,600)SI
600   FORMAT(' ',A)
      GO TO 2
      END SUBROUTINE


      SUBROUTINE ESCTAB(ip,TAB)
!     ==================

      CHARACTER*1 TAB
      integer(4) :: TTT,CC(MXZON),GRANTOT

      CC=RCERO
      GRANTOT=RCERO
      WRITE(2,'(5H ZON ,1000(A,I8))')(TAB,NUMZON(I),I=IUNO,NZN)
      WRITE(2,'(5H     ,1000(A))')(TAB,NOMZON(I),I=IUNO,NZN)
      DO I=IUNO,NZN
           TTT=ZONEXE(I,ip)+0.5
           WRITE(2,'(1X,I3,1X,A8,1000(A,I8))') &
              NUMZON(I),NOMZON(I),(TAB,MAT(I,J),J=1,NZN),TAB,TTT
           DO J=1,NZN
              CC(J)=CC(J)+MAT(I,J)
           ENDDO
           GRANTOT=GRANTOT+TTT
      ENDDO
      WRITE(2,'(5H TOTL,1000(A,I8))')(TAB,CC(J),J=1,NZN),TAB,GRANTOT

      RETURN
      END SUBROUTINE

END PROGRAM EVAL

 subroutine usage
 USE GETOPTM
 character(32) prog
    prog = argv(0)

    print *
    print '(A,'' - TRANUS(r) Evaluation model'')', trim(prog)

    print *,'usage:'

    print '(4X, A,'' <scen1> <scen2> [options]'')', trim(prog)
    print *
    print *, 'If no arguments are given, the program exits.'

    print *
    print *, 'Options are:'
    call ExplainStdOptions

    STOP 2
 end subroutine

