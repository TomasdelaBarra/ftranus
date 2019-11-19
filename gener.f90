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
MODULE GENER
use PARAM
use MENSAMOD
use GETOPTM
!$if .not. defined(_DF_VERSION_)
!use MSIMSLSD
!$endif

    character(*), parameter :: GENER_RCS_ID = & 
      "$Id$" 

      character(*), parameter :: VAL_FMT = '(E24.16)'

CONTAINS

logical function SetCurrentDirectory(path)
   character(*) :: path
   call ChDir(path)
   SetCurrentDirectory = .true.
end function


!  GENER.FOR - RUTINAS EN LA LIBRERIA TRANUS 4.4
      SUBROUTINE ident(Men)
      INTEGER Men
      character Dir*256
      logical   FExists

        write(*,*)
        write(*,100) ReleaseDateStr, ReleaseStr,ReleaseName
        write(*,*)
        write(*,200) ReleaseYear
        write(*,201) 
        write(*,202) ReleaseYear
        write(*,203) 
        write(*,*)
        write(*,204) 
        write(*,205) 
        write(*,*) '      http://creativecommons.org/licenses/by-sa/2.0/'
        WRITE(*,*)'____________________________________________'
        write(*,*)
100     format(1x,' TRANUS ', A, ' v',A,1X,A)
200     format(1x,' Copyright (C) 1983-',I4,' Modelistica, Caracas')
201     format(1x,' Copyright (C) 1983-2003 Beatriz Perez, Caracas')
202     format(1x,' Copyright (C) 1985-',I4,' Juancarlo An~ez, Caracas')
203     format(1x,' Some rights reserved.')
204     format(1x,' (cc) This work is distrubuted under a Creative Commons')
205     format(1x,'      Attribution-ShareAlike 2.0 license')
        CALL MENSA(MEN,0)
        WRITE(*,*)'____________________________________________'
        write(*,*)

         inquire(FILE='TRANUS.CTL', EXIST=FExists)
         if (.not.FExists) then
            inquire(FILE='W_TRANUS.CTL', EXIST=FExists)
            if (.not.FExists) then
               call Mensa(msg_PathForCTL, 1)
               READ(*,'(A)') Dir
               if (.not.SetCurrentDirectory(Dir)) then
                    ! error, invalid directory
               endif        
            endif
         endif
      END SUBROUTINE

      SUBROUTINE INIT(POL,IIP,LP,MEN, Usage, options)
          CHARACTER POL*3,IIP*1
          LOGICAL   LP
          INTEGER   MEN
          integer(2) :: ISTAT
          optional :: Usage
          external Usage
          character(*), intent(in), optional :: options
          character(MXARGLEN) :: opts

      !  RUTINA INICIAL DE LOS PROGRAMAS DE TRANUS - PREGUNTA A#O/POLITICA
!  Si LP es .T., pregunta adem#s por la opci#n IIP
!  MEN es el n#mero del mensaje con el t#tulo del programa

      if (present(options)) then
         opts = options
      else
         opts = STD_OPTIONS
      endif

      if (.not. present(Usage)) then
         if (getopts(opts, StdUsage)) then
             call doStdOpts(StdUsage)
         endif
      elseif (getopts(opts, Usage)) then
         call doStdOpts(Usage)
      endif
!  Averigua cu#ntos argumentos hay en la l#nea de comando
      if (debugging >= dbg_normal) then
          call Ident(men)
          WRITE(*,*)
          WRITE(*,*)
      endif
      POL='???'
      if (paramc() >= 1) then
         POL = trim(argv(1))
      endif
      if (.not. HasOpts()) then
    !  Al menos deber#a haber 1 argumento 0=programa
    !  Si LP=T y hay 2 argumentos, los procesa
          IF(LP .and. paramc() >= 2)THEN
             IIP = trim(argv(2))
             CALL UPPER(IIP)
             IF(IIP.NE.'A'.AND.IIP.NE.'N'.AND.IIP.NE.'I')CALL MENSA(14,-1) ! Opc inv#lida
    !  Si LP=F y hay 2 argumentos, los procesa
          ELSEIF(paramc() < 1)THEN
             CALL MENSA(5,1)
             READ(*,'(A)')POL
             IF(LP)THEN
10             CALL MENSA(13,1)
               READ(*,'(A)')IIP
               CALL UPPER(IIP)
               IF(IIP.NE.'A'.AND.IIP.NE.'N'.AND.IIP.NE.'I')THEN
                  CALL MENSA(14,0) ! Opc inv#lida
                  GO TO 10
               ENDIF
             ENDIF
          ENDIF
      endif
      CALL UPPER(POL)

      RETURN
      END SUBROUTINE


      SUBROUTINE ABRE(iUn,FILE1,ITIP)
!     ===============
!  VERIFICA SI EXISTE UN ARCHIVO OLD Y LO ABRE. SI NO ERROR

      CHARACTER FILE1*(*)
      LOGICAL   NE

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!     MENSA (Subr)  Emite los mensajes

      INQUIRE(FILE=FILE1,EXIST=NE)
      IF(NE)THEN
         IF(ITIP.EQ.IO_FMT)THEN
            OPEN(iUn,FILE=FILE1)
         ELSE IF(ITIP.EQ.IO_UNF) THEN
            OPEN(iUn,FILE=FILE1,FORM='UNFORMATTED')
         ELSE IF(ITIP.EQ.IO_BIN) THEN
            OPEN(iUn,FILE=FILE1,ACCESS='STREAM')
         ELSE
            CALL MENSA(9797,-1)
         ENDIF
         RETURN
      ELSE
         WRITE(*,*)' FILE= ',FILE1
         CALL MENSA(1,-1)  !  E-G01: No existe este archivo
      ENDIF
      END SUBROUTINE

      SUBROUTINE creaArch(iUn,FILE1,ITIP)
!     ===============
!  ITIP=1  Archivo nuevo formatted
!  ITIP=0  Archivo nuevo unformatted
!  ITIP=2  Archivo nuevo binario

      CHARACTER FILE1*(*)

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!     MENSA (Subr)  Emite los mensajes

         IF(ITIP.EQ.IO_UNF) THEN
            OPEN(iUn,FILE=FILE1,STATUS='UNKNOWN',FORM='UNFORMATTED')
         ELSE IF(ITIP.EQ.IO_BIN) THEN
            OPEN(iUn,FILE=FILE1,STATUS='UNKNOWN', ACCESS='STREAM')
         ELSE
            OPEN(iUn,FILE=FILE1,STATUS='UNKNOWN',FORM='FORMATTED')
         ENDIF
      RETURN
      END SUBROUTINE

      SUBROUTINE CHECKIU(iUn, F,IOS,FIL)
      REAL F
      INTEGER iUn, IOS
      CHARACTER(3) :: FIL
!     ================
!  VERIFICA LA LECTURA DE LOS ARCHIVOS DE DATOS
!  F es un real = n#mero de la secci#n del archivo
!  IOS es el c#digo de error
!  FIL es el nombre del archivo

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!     MENSA (Subr)   Emite los mensajes

      CHARACTER   STAR*80
      BACKSPACE(iUn)
      READ(iUn,10)STAR
10    FORMAT(A)
      IF(STAR(3:8).NE.'------')THEN
         WRITE(*,*)
         WRITE(*,'(''Cod:'',I6,8H  File: ,A,''  Sec:'',F4.1)')IOS,FIL,F
         WRITE(*,*)
         WRITE(*,*)STAR
         CALL MENSA(3,-1)  !  EG03: Problema de lectura en archivo entrada
      ENDIF
      RETURN
        WRITE(*,100) FIL, F
100     FORMAT('+',A3,F3.1)
      END SUBROUTINE

      SUBROUTINE CHECK(F,IOS,FIL)
      REAL F
      INTEGER IOS
      CHARACTER   FIL*3
        CALL CHECKIU(3, F, IOS, FIL)
      RETURN
      END SUBROUTINE


      subroutine GetCurrentTime(IHR,MINS,ISEC,MILESM)
        integer, intent(out) :: IHR,MINS,ISEC,MILESM
        integer :: now(30)

        call itime(now)

        IHR = now(1)
        MINS = now(2)
        ISEC = now(3)
        MILESM = 0
      return
      end subroutine

      subroutine GetCurrentDate(IAN, IMES, IDIA)
        integer, intent(out) :: IAN, IMES, IDIA
        integer(8) :: now(8)

        call date_and_time(VALUEs=now)

        IAN  = now(1)
        IMES = now(2)
        IDIA = now(3)
      return
      end subroutine
      

      SUBROUTINE TIEJEC(IHR,HMIN,ISEC,IMILE)
!     =================
      integer, intent(in) :: IHR,HMIN,ISEC,IMILE
      real :: TIEMPO,IT0
      integer :: IIHR,IMIN,IISEC,MILESM

!  RUTINA PARA CALCULAR EL TIEMPO DE EJECUCION

      call GetCurrentTime(IIHR,IMIN,IISEC,MILESM)
!  Lleva todo a segundos
      IT0=HMIN*60.0+IHR*3600.0+ISEC+IMILE/1000.
      TIEMPO=IMIN*60.0+IIHR*3600.0+IISEC+MILESM/1000.
      TIEMPO=TIEMPO-IT0
      CALL MENSA(4,0)  ! run time
      WRITE(*,'(''   '',F8.3,5H mins)')TIEMPO/60.

      RETURN
      END SUBROUTINE



!====================================================
!  INTNUM es el n#mero interno de la categor#a externa j
!  Sirve para zonas, sectores, categor#as, modos, etc...
!  Lo busca en la lista NUMLIS dimensionada a MXZON, que es la mas grande
!  NT es la dimensi#n de la lista

      INTEGER FUNCTION INTNUM(J,NUMLIS,NT)
        INTEGER J, NumLis(*), NT
        INTEGER i


      INTNUM=INF
      DO I=1,NT
         IF(J.EQ.NUMLIS(I))INTNUM=I
      ENDDO

      END FUNCTION

      SUBROUTINE SALI(SALIDA, prompt)
        logical, intent(in), optional :: prompt
        logical :: doPrompt
      CHARACTER(1) ::  OP
      CHARACTER(*) :: SALIDA
      character(128) :: AUX
      LOGICAL      NE
      AUX=' '
      if (present(prompt)) then
         doPrompt = prompt
      else
         doPrompt = .true.
      endif
      if (doPrompt) then
547      WRITE(*,'(///)')
          WRITE(*,*)'___________________________________________'
          CALL MENSA(11,0)  ! Especifique el archivo salida...
          WRITE(*,*)'___________________________________________'
          WRITE(*,*)' Enter:----> ',trim(SALIDA)
          WRITE(*,'(''       ----->  ''$)')
          READ(*,'(A)')AUX
          CALL UPPER(AUX)
          IF(AUX(1:3).NE.'   ')SALIDA = trim(AUX)
          IF(SALIDA(1:3).NE.'PRN'.AND.SALIDA(1:3).NE.'CON')THEN
             INQUIRE(FILE=SALIDA,EXIST=NE)
             IF(NE)THEN
              print '(''File "'',A, ''" already exists. Overwrite it?'',$)', trim(SALIDA)
              !!CALL MENSA(12,1)
              READ(*,'(A)')OP
              IF(OP.EQ.'N'.OR.OP.EQ.'n')GO TO 547
             ENDIF
          ENDIF
      endif
      OPEN(2,FILE=SALIDA,STATUS='UNKNOWN')
      RETURN
      END SUBROUTINE

      SUBROUTINE UPPER(S)
        CHARACTER(*) S
        integer :: I
         DO I=1, LEN(S)
           N=ICHAR(S(I:I))
           IF (N.GE.ICHAR('a').AND.N.LE.ICHAR('z'))THEN
              S(I:I)=CHAR(N-ICHAR('a')+ICHAR('A'))
           ENDIF
         ENDDO
      RETURN
      END SUBROUTINE

      SUBROUTINE CheckStatus(status)
        integer   status

         if (status .gt. 0) then
            call mensa(status, -1)
         endif
      RETURN
      END SUBROUTINE

      REAL FUNCTION GetNextFileSection(iUn)
        integer iUn
        character Text*80, FileName*80
        real    Sect

        read(iUn, '(A)') Text
        backspace(iUn)
        inquire(UNIT=iUn, NAME= FileName)
        read(iUn,*, ERR=100, END=200) Sect
        GetNextFileSection = Sect 
      RETURN
       
100     write(*,*) Text
        write(*,'(1X,''[''A,F8.4,'']'')') trim(FileName), Sect
        call Mensa(msg_SectionStartExpected, mensa_Aborta)
      RETURN
200     GetNextFileSection = 0
      RETURN
      END FUNCTION
      
      
      SUBROUTINE CheckNextFileSection(iUn, Sect)
        integer iUn
        real    Sect
        real      S
        character Text*80, FileName*80
                                       
        inquire(UNIT=iUn, NAME= FileName)
        S = GetNextFileSection(iUn)
        if (S.ne.Sect) then
           backspace(iUn)
           read(iUn, '(A)') Text
           write(*,'(1X,A,F3.1)') FileName, Sect
           write(*,*) Text
           call Mensa(msg_SpecificSectionExpected, mensa_Aborta)
        endif
      END SUBROUTINE
      
      


      INTEGER FUNCTION iFindNum(J, NumLis, N)
          INTEGER J, NumLis(*), N
         integer i
           
         iFindNum = 0
         
         do i = 1, N 
            if (NumLis(i).eq.j) then
                iFindNum = i
                RETURN
            endif
        enddo
      END FUNCTION


     logical function IsInf(x)
          real :: x
          real :: inf
              inf = 1
              inf = inf / 0.0
              IsInf = x >= inf
          end function

          logical function IsInfD(x)
          double precision :: x
          double precision :: inf
              inf = 1
              inf = inf / 0.0
              IsInfD = x >= inf
     end function

character(20) function HoursToString(hours)
      real, intent(in) :: hours
      character(20) :: str
      integer :: h, m, s, z
      real :: d

      d = hours
      h = int(d)
      d = 60 * (d - h)
      m = int(d)
      d = 60 * (d - m)
      s = int(d)
      z = 1000*(d - s)

      write(str, 100) h, m, s!, z
100   format(I2,':',I2.2,':',I2.2)!,'.',I3.3)
      HoursToString = trim(str)
      return
end function   

! logical XOR
logical function lxor(a, b)
logical, intent(in) :: a, b
  lxor = (a .or. b) .and. .not. (a .and. b)
end function   

character(20) function IntegerToString(n)
    integer, intent(in) :: n

    character*20 a
    write(a, *) n
    a = adjustl(a)
    IntegerToString = trim(a)
end function

character(24) function fmtval(value)
    real(8), intent(in) :: value

    character*(24) :: sval

    write(sval, VAL_FMT) value
    fmtval = adjustl(sval)
end function

END MODULE GENER
