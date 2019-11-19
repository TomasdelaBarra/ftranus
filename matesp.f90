!$Id$
!   ------------------------------------------------------
!   T R A N U S  -  M A T E S P (c)MODELISTICA
!   ------------------------------------------------------
!                                       v4.6  Agosto 1995
!   ------------------------------------------------------
!   Desarrollado por T. de la Barra, B. Perez y J. Anez
!   ------------------------------------------------------
PROGRAM MATESP
USE DEBUGM
USE PARAM
USE GENER
USE CONTROL
USE IO_LIST
USE RCOMM
USE TCOMM
USE TPARC
USE ZCOMM
USE MECOMM
USE RTRANS
USE MPOWIT

      character(80) :: MATESP_RCS_ID = &
       "$Id$" 

      PARAMETER(MXIUN=9)
      CHARACTER*1 IUN(MXIUN), SI
      integer, parameter :: IUNCOSTS = 200
      integer     IUNPAS(MXMOD)
      real(8) :: TTT
      CHARACTER FILE1*11,IIP*1
      DATA IUN/'1','2','3','4','5','6','7','8','9'/

      character, parameter :: MATESP_OPTS*(*) = STD_OPTIONS // 'c:dl:m:n:o:p:?CDFLMOR:STY'
      external Usage


      integer :: i
      integer(2) :: IDIA,MES,IAN,IHR,MINS,I2
      integer :: status
      INTEGER IPOL
      logical TODO
      integer :: AuxNod(NLMAX)
      logical :: useMATESPDAT = .false.

!
!  DENOMINACION DE LOS ARCHIVOS
!
!  Unidad Nombre Tipo Formato   Contenido
!  ---------------------------------------------------------
!    !!!95.08.03 Esto hay que actualizarlo
!    1    MENSA.DAT     con     Mensajes
!    3     Z1E   Entra  con     Zonificacion
!    3     T1E   Entr.  con     Parametros
!    3     P0S   Entr.  sin     Red completa
!    4     F1S   Entr.  sin     Flujos por categoria de viaje
!    6,7.. PnS   Entr.  sin     Pasos del modo n
!
!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    !!!95.08.03 Eliminados por desactualizados
!

      VIAJOP = 0.
      OperOper = 0.
      LOPER=.FALSE.
      NOPAG=0
      LCAT=.FALSE.
      LMODE=.FALSE.
      status=0

      SALIDA=' '
      IMP=.FALSE.
      CALL INIT(POL,IIP,.FALSE.,8400,Usage,MATESP_OPTS)
      call InitPol(POL,IPOL)

!  LEE ARCHIVO DE ZONAS

      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)
!
!  LECTURA DE PARAMETROS
!

      if (.not. hasopts() ) then
      WRITE(*,*)'_________________________________________'
      CALL MENSA(1004,0)  ! LECTURA DE PARAMETROS Y DATOS
!  OPCION LECTURA DE OPCIONES (IP=1 de MATESP.DAT IP=0 de pantalla)

      WRITE(*,*)
      WRITE(*,*)'___________________________________________'
      CALL MENSA(8031,0)  ! Opciones para ingreso de datos:...
      WRITE(*,*)'___________________________________________'
      DO WHILE(.TRUE.)
           CALL MENSA(7,1)  !     Opci¢n --->
           IP = 0
           READ(*,'(I2)')IP
           IF(IP == 0) THEN
              EXIT
           ELSEIF(IP == 1) THEN
              INQUIRE(FILE='MATESP.DAT',EXIST=NE)
              IF(.NOT.NE)THEN
                 CALL MENSA(8032,0)
              ELSE
                 useMATESPDAT = .true.
                 EXIT
              ENDIF
         ENDIF
      ENDDO
      endif ! hasopts()

!  LECTURA DE LA RED DE T3S

      call OpenPolFile(3,IPOL,'T3S',IO_BIN)
      CALL RDTPAR(3,status,ITERAN,IDIA,MES,IAN,IHR,MINS)
      call CheckStatus(status)
      CALL RDREDANT(3)! ceros, no hay que excluir ningun enlace
      call CheckStatus(status)

! VERIFICACION DE OPCIONES EN LA LINEA DE COMANDOS
      SALIDA = trim(AREA) // trim(POL) // '.ESP'
      call GetOptions()

! ESPECIFICA EL ARCHIVO DE SALIDA

      SI=CHAR(15)
      if (HaveFileName) then
          OPEN(2,FILE=SALIDA,STATUS='UNKNOWN')
      else
          call SALI(SALIDA, .not. hasopts())
      endif
      SI=' '
      IF(SALIDA(1:3).EQ.'PRN')WRITE(2,'('' '',A)')SI
      call verbose('OUTPUT TO: "' // trim(SALIDA) // '"')

!  OPCIONES DE IMPRESION

      if (.not. hasopts()) then
      WRITE(*,'(///)')
      WRITE(*,*)'___________________________________________'
      CALL MENSA(8401,0)  ! Opci¢n de impresi¢n...
      WRITE(*,*)'___________________________________________'
40    CALL MENSA(7,1)  !      Opci¢n --->
      READ(*,'(I2)')IPP
      IF(IPP.LT.1.OR.IPP.GT.8)GO TO 40
      IMP(IPP)=.TRUE.
      ! if (IPP == 9) ReportNodeFlows = .true.
      endif ! hasopts()

      if (imp(8)) SuppressHeaders = .true.

      IopImpre=1
!  Opción 1 Viajes por Operador, pregunta por la lista de operadores
      IF(.not. hasopts() .and. IMP(1))THEN
14       WRITE(*,'(///)')
         CALL MENSA(8402,0)  ! Introduzca lista de operadores
         NOPAG=0  ! No de operadores a agregar
         AUXOPER=0
         DO I=1,NOPER
            WRITE(*,'(I6,2X,A)')NUMOP(I),NOMOP(I)
         ENDDO
         WRITE(*,*)
         CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,*)(AUXOPER(I),I=1,MXOPER)
         DO I=IUNO,MXOPER
            IF(AUXOPER(I).EQ.0)EXIT
            I2=INTNUM(AUXOPER(I),NUMOP,NOPER)
            IF(I2.GT.NOPER)THEN
               WRITE(*,*)'Oper:',AUXOPER(I)
               CALL MENSA(6,0)  ! G04 Definici¢n ilegal
               GO TO 14
            ENDIF
            LOPER(I2)=.TRUE.
            NOPAG=NOPAG+IUNO
         ENDDO
      ENDIF

!  Opción 2 Viajes que usan uno o más arcos, pregunta por la lista de categorias
      IF(.not. hasopts() .and. IMP(2))THEN
15       WRITE(*,'(///)')
         CALL MENSA(8403,0)  ! Introduzca lista de categorias
         NOCAG=0  ! No de categorias a agregar
         AUXPROP=0
         DO I=1,NPROP
            WRITE(*,'(I6,2X,A)')NUMCAT(I),NOMCAT(I)
         ENDDO
         WRITE(*,*)
         CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,*)(AUXPROP(I),I=1,MXPROP)
         DO I=IUNO,MXPROP
            IF(AUXPROP(I).EQ.0)EXIT
            I2=INTNUM(AUXPROP(I),NUMCAT,NPROP)
            IF(I2.GT.NPROP)THEN
               WRITE(*,*)'Categ:',AUXPROP(I)
               CALL MENSA(6,0)  ! G04 Definici¢n ilegal
               GO TO 15
            ENDIF
            LCAT(I2)=.TRUE.
            NOCAG=NOCAG+IUNO
         ENDDO
         if(ip.eq.0)then
           do while (.true.) 
              write(*,*)
              call mensa(8408,0) !  ingrese un enlace (Orig Dest)
              WRITE(*,*)
              CALL MENSA(7,1)  !      Opci¢n --->
              io = 0
              id = 0
              read(*,*) io,id
              if (io == 0) exit
              nl=0
              DO I=1,NLINK
                IF(IOR(I).EQ.IO.AND.IDES(I).EQ.ID)THEN
                   ESCOGE(I)=.TRUE.
                   nl=1
                ENDIF
              ENDDO
              if(nl.le.0)then
                call mensa(6,0) ! Definicion ilegal
              endif
           enddo
         endif
      ENDIF

!  Opciones 3 al 6, pregunta por el modo
       IF(.not. hasopts() .and. (IMP(3).or.imp(4).or.imp(5).or.imp(6)))THEN
34       WRITE(*,'(///)')
         CALL MENSA(8404,0)  ! Choose a mode
         DO I=1,NTM
            WRITE(*,'(I6,2X,A)')NUMMOD(I),NOMMOD(I)
         ENDDO
         WRITE(*,*)
         CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,*) IMO
         I2=INTNUM(IMO,NUMMOD,NTM)
         IF(I2.GT.NTM)THEN
            WRITE(*,*)'Mode:',IMO
            CALL MENSA(6,0)  ! G04 Definici¢n ilegal
            GO TO 34
         ENDIF
         LMODE(I2)=.TRUE.
      ENDIF

! Opción 3 Matriz de Transferencias, pide el No. minimo de transbordos
      if(.not. hasopts() .and. imp(3))then
         WRITE(*,'(///)')
         CALL MENSA(8405,0)  ! Introduzca el numero de transbordos
         WRITE(*,*)
         CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,*)ITR
      endif

!  Opciones 4, 5 y 6 Matrices de Distancia, Tiempo o Costo, pide  una categoria
      IF(.not. hasopts() .and. (IMP(4).or.imp(5).or.imp(6))) THEN
55       WRITE(*,'(///)')
         CALL MENSA(8407,0)  ! Introduzca una categoria
         DO I=1,NPROP
            WRITE(*,'(I6,2X,A)')NUMCAT(I),NOMCAT(I)
         ENDDO
         WRITE(*,*)
         CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,*)ipr
         I2=INTNUM(ipr,NUMCAT,NPROP)
         IF(I2.GT.NPROP)THEN
               WRITE(*,*)'Categ:',AUXPROP(I)
               CALL MENSA(6,0)  ! G04 Definici¢n ilegal
               GO TO 55
         ENDIF
         LCAT(I2)=.TRUE.
      ENDIF


!  Opcion 8, flujo a travez de nodos
      IF(.not. hasopts() .and. ReportNodeFlows) THEN
66       WRITE(*,'(///)')
         CALL MENSA(8409,0)  ! Introduzca la lista de nodos
         WRITE(*,*)
         CALL MENSA(7,1)  !      Opci¢n --->
         AuxNod = 0
         READ(*,*)(AuxNod(i), i=1, NLINK)
         do i=1, NLINK
             if (AuxNod(i) == 0) EXIT
             i2=iFindNum(AuxNod(i),IDES,NLINK)
             if (i2 == 0) then
                   write(*,*)'Node:', AuxNod(i)
                   call mensa(6,0)  ! G04 Definici¢n ilegal
                   goto 66
             endif
             !! call WantNodeFlow(AuxNod(i))
         enddo
      ENDIF

! SI IP=1 LECTURA DE MATESP.DAT POR SI HAY AGREG DE ZONAS
! Si Opcion 2, Matrices que usan uno o mas enlaces, deben ingresarse en
! MATESP.DAT

      DO I=1,NZN     ! Inicializa vector de agregaci¢n
         IAG(I)=I
      ENDDO
      NZAG=NZN
      DO J=1,NZN
         NUMAG(J)=NUMZON(J) ! a efectos de la rutina ESCRIBE
         NOMAG(J)=NOMZON(J)
      ENDDO
      IF(useMATESPDAT)THEN
         FILE1='MATESP.DAT   '
         OPEN(3,FILE=FILE1,STATUS='OLD')
         CALL LEEDAT(NZAG)
         CLOSE(3)
      ENDIF

!  VERIFICA SI HAY ALGO QUE HACER
   todo = .false.
   do i=1, NIMP
     todo = todo .or. imp(i)
     if (imp(i) .and. debugging >= dbg_verbose) then
       print *, 'Printing option ', i
     endif
   enddo
   call assert(todo, 'NOTHING TO DO!!')

!  ABRE LOS ARCHIVOS DE FLUJOS (F1S) Y DE PASOS (P_S)

      call OpenPolFile(IUNCOSTS, IPOL, 'T6S', IO_BIN)
      DO K=1,NTM
         ! Abre los archivos de pasos y calcula el número de modos públicos
         IUNPAS(K)=K+100
         call FindPolFile( IUNPAS(K), IPOL,'P'//IUN(K)//'S', IO_BIN, pol_Any)
         call SkipPathHeader(IUNPAS(K))
      ENDDO


      if (debugging >= dbg_normal) then
        CALL MENSA(8417,0)  ! Categoria   Origen
        call info('')
      endif

      !! if (ReportNodeFlows) call InitFlows

      VOL=0     ! Blanquea volumen por enlace

      if(imp(8)) then
          call CategoryOperatorSummaryHeader(2)
      endif
  
!  Iteraciones respecto a categorias IP
      DO IP=1,NPROP

        ! Precalcular los costos de los enlace-rutas
        ! para la categoria
        CALL CALCIPCOST(IP)

!  Iteraciones respecto a zonas de origen I

         DO I=1,NZN
           IF(JER1(I).ne.JER2(I)) CYCLE

           if (debugging >= dbg_normal) then
              if (MOD(I-1,10) == 0 .or. I == NZN) then
                 WRITE(*,'(A1,1X,I3,3X,''  ('',I2.2,''%)''$)') &
                         13, NUMCAT(IP),(100*I)/NZN
              endif
           endif


          ! Iteraciones respecto a zonas de destino J
   
           do J=1, NZN
             if (debugging >= dbg_debug) then
               print *, 'going to assign ', ip, i, j
             endif
             IF(I.EQ.J.OR.JER1(J).NE.JER2(J)) CYCLE
             call ASIGNA(IP, I, J, iuncosts)
             if (imp(8)) then
                call CategoryOperatorSummary(2, i, j)
             endif
             if (debugging >= dbg_debug) then
               print *, 'assigned ', ip, i, j
             endif
           enddo

        enddo         !  Iteraciones r/origen I
        call info('')

!  Rewind los archivos de pasos

        DO K=1,NTM
           REWIND IUNPAS(K)
           call SkipPathHeader(IUNPAS(K))
        ENDDO

      enddo
!  Fin de las categorias IP.

      call debug('End of iterations')

      if(.not. SuppressHeaders) WRITE(2,5)ESTUDIO,NOMBRE
5     FORMAT(' ',A30,3X,'POLITICA: ',A18)
      IF(IMP(1))THEN
         if(.not. SuppressHeaders) then
           CALL MENSA (8410,2)  ! Viajes por operador
           WRITE(2,'(2X,A3,2X,A3,2X,I3,I6,''-'',I2.2,''-'',' //  &
                   'I4,I6,'':'',I2)')                            &
                   AREA,POL,ITERAN,IDIA,MES,IAN,IHR,MINS
           WRITE(2,'(/)')
           AUXOPER = 0
           NOPAG = 0
           do I = 1, NOPER
               if (LOPER(I)) then
                   NOPAG = NOPAG+1
                   AUXOPER(NOPAG) = I
               endif
           enddo
           WRITE(2,'('' Opers:'',20(I4,1X,A))') &
             (AUXOPER(I),NOMOP(AUXOPER(I)),I=IUNO,NOPAG)
         endif
      ELSE
        IF(IMP(2)) THEN
          CALL MENSA (8411,2)  ! Viajes que usan uno o mas enlaces
        ELSEIF(IMP(3)) THEN
          CALL MENSA (8412,2)  ! Viajes que tienen ITR o mas trasbordos
        ELSEIF(IMP(4)) THEN
          CALL MENSA (8303,2)  ! Matriz de distancias
        ELSEIF(IMP(5)) THEN
          CALL MENSA (8304,2)  ! Matriz de tiempos de viaje
        ELSEIF(IMP(6)) THEN
          CALL MENSA (8306,2)  ! Matriz de costos de viaje
        ELSEIF(IMP(7)) THEN
          CALL MENSA (8317,2)  ! Matriz de trasnferencias entre oper.
        ENDIF

        if(.not. SuppressHeaders)  then
           WRITE(2,'(2X,A3,2X,A3,2X,I3,I6,''-'',I2.2,''-'',' // &
                   'I4,I6,'':'',I2)')                           &
                   AREA,POL,ITERAN,IDIA,MES,IAN,IHR,MINS
           WRITE(2,'(/)')
        endif
      ENDIF
     
      if (.not.imp(8).and..not. ReportNodeFlows) then
         DO I=IUNO,NZN
            DO J=IUNO,NZN 
               IF(IopImpre.eq.0)THEN
                  MAT(I,J)=VIAJOP(I,J)
               ELSE
                IF(IAG(I).LT.0.OR.IAG(J).LT.0)CYCLE
                MATAG(IAG(I),IAG(J))=MATAG(IAG(I),IAG(J))+VIAJOP(I,J)
               ENDIF
            ENDDO
         ENDDO
         IF(IopImpre.eq.1)CALL TOTAL(TTT,NZAG)
         IF (imp(7)) THEN
            CALL EscribeOperOper(2)
         ELSE
            CALL ESCTAB(IopImpre,NZAG,TTT,TAB)
         ENDIF
      endif

      if (ReportNodeFlows) then
        if (ReportNodeVehicles) then
          call AddRouteFlows
        endif
        !! write(dmsg,*) 'NUMBER OF FLOWS', CountFlows()
        call debug(dmsg)
        close(2)
        !! call SaveFlows(trim(SALIDA), .not. SuppressHeaders)
      endif

395    if (debugging >= dbg_normal) then
            CALL MENSA(8,0)  ! FINAL NORMAL DE
            call info(' M A T E S P')
       endif
CONTAINS

   SUBROUTINE ESCTAB(N0,N3,TTT,TAB)
      CHARACTER*1 TAB
      real(8) :: TTT

!  N0 = 0 matriz real con un decimal;  =1 matriz entera
!  N3 = N£mero de zonas a grabar

      WRITE(2,'(5H ZON ,1000(A,I8))')(TAB,NUMAG(I),I=IUNO,N3)
      WRITE(2,'(5H     ,1000(A,A8))')(TAB,NOMAG(I),I=IUNO,N3)
      DO I=IUNO,N3
         IF(N0.EQ.0)THEN
           WRITE(2,'(1X,I3,1X,A8,1000(A,F14.2))') &
              NUMAG(I),NOMAG(I),(TAB,MAT(I,J),J=1,N3)
         ELSE
           WRITE(2,'(1X,I3,1X,A8,1000(A,F14.2))')   &
              NUMAG(I),NOMAG(I),(TAB,MATAG(I,J),J=1,N3),TAB,FILAS(I)
         ENDIF
      ENDDO

      IF(N0.NE.0)THEN
         WRITE(2,'(5H TOT ,1000(A,F14.2))')(TAB,CC(J),J=1,N3),TAB,TTT
      ENDIF

      RETURN
   END SUBROUTINE


   SUBROUTINE TOTAL(TTT,N3)
      real(8) :: TTT

      TTT=0
      DO I=1,N3
         CC(I)=0
         FILAS(I)=0
      ENDDO
      DO I=1,N3
         DO J=1,N3
            CC(J)=CC(J)+MATAG(I,J)
            FILAS(I)=FILAS(I)+MATAG(I,J)
         ENDDO
         TTT=TTT+FILAS(I)
      ENDDO
      RETURN
   END SUBROUTINE

   SUBROUTINE LEEDAT(NZAG)

      INTEGER IAUX(MXZON)
      CHARACTER*1 TIT
      CHARACTER*32 NOM

!  READ SECTION 1.0 - Report options

      READ(3,'(///)',END=999)
      DO I=1,7
         READ(3,*,END=999,ERR=100,IOSTAT=IOS)TIT,I1
         IMP(I) = I1 /= 0
      ENDDO
      READ(3,*,END=999,ERR=100,IOSTAT=IOS)TIT,ESVER
      DO I=8,15  ! una de m s para que caiga en CHECK
         READ(3,*,END=999,ERR=100,IOSTAT=IOS)TIT,I1
      ENDDO
100   CALL CHECK(1.0,IOS,'MAT')

!  SECTION 2.0 - Zone aggregation

      READ(3,'(/)',END=999)
      NZAG=0
      DO J=1,NZN
         IAUX(J)=0
         IAG(J)= 0
      ENDDO
201   READ(3,*,END=999,ERR=200,IOSTAT=IOS)NN,NOM,(IAUX(J),J=1,NZN)

!  Si la macrozona es cero, la lista de zonas IAUX se  ignora
      IF(NN.EQ.0)THEN
         DO J=1,NZN
            IF(IAUX(J).EQ.0)EXIT
            IAUX1=INTNUM(IAUX(J),NUMZON,NZN)
               IF(IAUX1.GT.NZN)THEN
                  WRITE(*,*)'MATESP (2.0) - Zon:',IAUX(J)
                  CALL MENSA(6,-1)
               ENDIF
            IAG(IAUX1)=-1
            IAUX(J)=0 ! Porsi, blanquea 
         ENDDO
         GO TO 201
      ENDIF

      NZAG=NZAG+IUNO
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
               WRITE(*,*)'MATESP (2.0) - Zon:',IAUX(J)
               CALL MENSA(6,-1)
            ENDIF
         IAG(IAUX1)=NZAG
         IAUX(J)=0   !  Porsi, blanquea
      ENDDO
      GOTO 201
200   CALL CHECK(2.0,IOS,'MAT')

!  Las zonas que no hayan sido incluidas en ninguna lista se conservan
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

!  SECCION 3.0 - Enlaces seleccionados para matrices de volumen

      ESCOGE=.FALSE.
      READ(3,'(/)',END=999)
301   READ(3,*,END=999,ERR=300,IOSTAT=IOS)IO,ID
      DO I=1,NLINK
         IF(IOR(I).EQ.IO.AND.IDES(I).EQ.ID)ESCOGE(I)=.TRUE.
         IF(IDES(I).EQ.IO.AND.IOR(I).EQ.ID)ESCOGE(I)=.TRUE.
      ENDDO
      GO TO 301
300   CALL CHECK(3.0,IOS,'MAT')

      RETURN

999   WRITE(*,*)'MATESP.DAT'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto
   END SUBROUTINE

SUBROUTINE EscribeOperOper(iun)
  integer, intent(in) :: iun
  integer :: io, iode, ioa
  real :: TotalTotal, TotalFilas(0:NOPER), TotalColumnas(NOPER)

  TotalFilas    = 0
  TotalColumnas = 0
  TotalTotal    = 0
  do iode=0, NOPER
    do ioa=1, NOPER
       TotalFilas(iode)   = TotalFilas(iode) + OperOper(iode, ioa)
       TotalColumnas(ioa) = TotalColumnas(ioa) + OperOper(iode, ioa)
       TotalTotal = TotalTotal +  OperOper(iode, ioa)
    enddo
  enddo

  write(iun, 100) (NumOp(io), io=1, NOPER)
  write(iun, 110) 'OPERATORS       ', (TRIM(NomOp(io)), io=1, NOPER), 'TOTAL'

  write(iun, 120) 0, 'Board             ', (OperOper(0, ioa), ioa=1, NOPER), TotalFilas(0)
  do iode=1, NOPER
    write(iun, 120) NumOp(iode), NomOp(iode), (OperOper(iode, ioa), ioa=1, NOPER), TotalFilas(iode)
  enddo
  write(iun, 130) 'TOTAL', (TotalColumnas(io), io=1, NOPER), TotalTotal


100   format(1X,6X, '\t', 10X, 1000('\t', I12) )
110   format(1X,6X, '\t', A10, 1000('\t', A12))
120   format(1X,I6, '\t', A10, 1000('\t', F12.2))
130   format(1X,6X, '\t', A10, 1000('\t', F12.2))
END SUBROUTINE

subroutine CategoryOperatorSummaryHeader(iun)
    integer, intent(in) ::iun
    write(iun, 100) &
      'origin', 'destination', 'category', 'operator', &
      'vehicles', &
      'volume', &
      'veh-dist', &
      'vol-dist', &
      'vol-time', &
      'vol-cost' 
100 format(A12,9(', ',A12))
end subroutine


subroutine CategoryOperatorSummary(iun, i, j)
    integer, intent(in) ::iun

    integer :: i, j, ip, io
    type(TCatOpSummary) :: s

    do ip=1, NPROP
        do io=1, NOPER
            s = CatOpSummary(ip, io)
            if (s%volume > 0.0 .or. s%vehicles > 0.0) then
                write(iun, 100) &
                    numzon(i), numzon(j), numcat(ip), numop(io), &
                    s%vehicles, &
                    s%volume, &
                    s%vehdist, &
                    s%voldist, &
                    s%voltime, &
                    s%volcost
            endif
        enddo
    enddo
100 format(I12,3(', ',I12),6(', ', F12.2))
end subroutine
  

subroutine GetOptions()
    integer :: n, node, cat, oper, mode
    real :: mintr
    character*(MXARGLEN) :: buf

    ReportNodeFlows = .false.
    ReportNodeVehicles = .false.
    useMATESPDAT = .false.
    NOPAG=0

    if (.not. hasopts()) then
        return
    endif
    ESCOGE = .false.
    do i = 1, optc()
        select case(optv(i))
            case ('c')
                n = ioptarg(i)
                cat = iFindNum(n, NumCat, NPROP)
                if (n == 0) then
                    LCAT = .true.
                elseif (cat > 0) then
                    LCAT(cat) = .true.
                else
                    print '(''Category '', I4, '' not found '')', n
                    stop 01
                endif
            case ('d')
                useMATESPDAT = .true.
                call verbose('using MATESP.DAT ')
            case ('l')
                call optargirange(i, io, id)
                if (debugging >= dbg_Verbose) then
                    print '(''Reporting link ('',2I8,'')'')', io, id
                endif
                do l=1,NLINK
                    IF(IOR(l) == IO .and. IDES(l) == ID)ESCOGE(l)=.true.
                enddo
            case ('m')
                n = ioptarg(i)
                mode = iFindNum(n, NumMod, NTM)
                if (mode == 0) then
                    LMODE = .true.
                elseif (mode > 0) then
                    LMODE(mode) = .true.
                else
                    print '(''Mode '', I4, '' not found '')', n
                    stop 01
                endif
            case ('n')
                node = ioptarg(i)
                if (debugging > dbg_verbose) print '(''node flows for : '',I6)', node
                !! call WantNodeFlow(node)
            case ('o')
                SALIDA=trim(optarg(i))
                if (SALIDA == '-') then
                    SALIDA = 'CON'
                endif
                HaveFileName = .true.
            case ('p')
                n = ioptarg(i)
                oper = iFindNum(n, NumOp, NOPER)
                if (n == 0) then
                    LOPER = .true.
                elseif (oper > 0) then
                    LOPER(oper) = .true.
                else
                    print '(''Operator'', I4, '' not found '')', n
                    stop 01 
                endif
            case ('C')
                imp(6) = .true.
                IopImpre=0
            case ('D')
                imp(4) = .true.
                IopImpre=0
            case ('L')
                imp(2) = .true.
            case ('M')
                imp(5) = .true.
                IopImpre=0
            case ('O')
                imp(7) = .true.
            case ('R')
                ITR = doptarg(i)
                imp(3) = .true.
                if (debugging > dbg_verbose) print '(''node flows for : '',A)', trim(buf)
            case ('S')
                imp(8) = .true.
            case ('T')
                imp(1) = .true.
            case default
                call doStdOpts(Usage)
        end select
    enddo
    SALIDA = trim(SALIDA)
end subroutine

    SUBROUTINE ASIGNA(IP,I,J,iuncosts)
      INTEGER :: IP, I, J

      logical(1) :: PASAN
      integer:: L, LnR, LnRa, lnrant
      real(8) :: PROB, A
      real(8) :: vacios, consol
      integer(1) :: ioverl(MXPAS,MXARC)
      logical WantOper(MXOPER)
      type(TCatOpSummary) :: s
!  Asignaci¢n

     
      call LoadZoneCosts(iuncosts, ip, i, j, c(j), ff(j), retorno(j))
      s%vehicles = 0
      s%volume   = 0
      s%vehdist  = 0
      s%voldist  = 0
      s%voltime  = 0
      s%volcost  = 0
      CatOpSummary(:,:) = s
      DO K=1,NTM
         call LeePaso(iunpas(k), k, ioverl)
         call LoadModeCosts(iuncosts, ip, i, j, k, cost(j,k), trips(j,k), p(k))
         vacios=RETORNO(J)*p(k)               ! Retornos
         consol=MIN(TRIPS(J,K),vacios)        ! Demanda consolid
         IF(MODO(IP,K))THEN
            DO J2=1,nps(k)          ! Recorre los pasos
               PASAN=.FALSE.  !BEATRIZ
               WantOper = LOPER
               call LoadPathCosts(iuncosts, ip, i, j, k, j2, putil(j2,k), A, prop(j2,k))
               PROB=PROP(J2,K)
               VAC=VACIOS*PROB    ! Demanda vac¡a en el paso J2
               CON=CONSOL*PROB    ! Parte consolidable en el paso j2
               IRANT=0
               IOANT=0
               ioa=0 !operador anterior
               itrans=0
               tiemviaj=0.
               cosviaj=0.
               disviaj=0.
               lnrant = 0
               DO JJ=1,NCOL(J2,k)    ! Recorre los enlaces
                  LnR=MATPAS(J2,JJ,k) ! Identifica el enlace
                  LnRa=ABS(LnR)
                  L =ILINK(LnRa)
                  IR=IROUTE(LnRa)   ! Identifica la ruta
                  IO=IOPRUT(IR)

                  if (lnrant /= 0 .and. ReportNodeFlows) then
                     if (.not. ReportNodeVehicles) then
                       ! call AddFlow(ior(L), ior(ilink(lnrant)), numrut(iroute(lnrant)), ides(L), numrut(ir), REAL(A))
                     else if( ITIPOP(IO) /= 3 .and. ITIPOP(IO) /= 4 ) then
                       ! do not report walking or routes
                       ! route vehicles are computed elsewhere
                       if (ioant == io) then
                         ! do not report transfers
                         ! report hehicles per hour
                         VehHr=A/(TOC(IO)*FACTIEM(IO))
                         vstan = VehHr*VS(IO, ITIP(L))
                         ! call AddFlow(ior(L), ior(ilink(lnrant)), numop(io), ides(L), numop(ioant), REAL(VehHr))
                       endif
                     endif
                  endif
         IF((IMP(4).OR.IMP(5).OR.IMP(6)).AND.LCAT(IP).AND.LMODE(K))THEN
                  disviaj=disviaj+DIS(L)
                  cosviaj=cosviaj+TARIFA(LnRa)
                  tiemviaj=tiemviaj+TMV(LnRa)
         ENDIF
                  ! Marca el paso con PASAN si algún enlace fue seleccionado en opcion 2
                  IF(IMP(2).AND.ESCOGE(L))THEN  !BEATRIZ
                     PASAN=.TRUE.
                     EXIT
                  ENDIF             !BEATRIZ
                  if(ioa.eq.0)ioa=io

                  !  Verifica si hay trasbordo
                  IF(IR.NE.IRANT.OR.MATPAS(J2,JJ,k).LE.0)THEN

                  !  Cuenta el No. de trasbordos a operadores distintos de peaton en el paso
                     IF(ITIPOP(IO).NE.4)ITRANS=ITRANS+1

                     !  Cuenta el No. de transbordos entre operadores
                     OperOper(ioant, io) = A + OperOper(ioant, io)

                     !  Si opcion 1: agrega el viaje al operador monitoreado
                     IF(IMP(1) .and. WantOper(IO) ) THEN
                       VIAJOP(I,J)=VIAJOP(I,J)+A
                       WantOper(IO) = .false.
                     ENDIF

                     ! Si opcion 6: agrega la tarifa de abordaje al costo de viaje
                     IF(IMP(6).AND.LCAT(IP).AND.LMODE(K))then
                        cosviaj=cosviaj+cosmin(ioa,io)
                     ENDIF

                  ENDIF ! Transbordo

                  ! Summary by O, D, Category, and Operator
                  if (itipop(io) == 4) then
                      V = 0
                  else
                      V = A/(TOC(IO)*FACTIEM(IO))
                  endif
                  s = CatOpSummary(ip, io)
                  s%vehicles = s%vehicles + V
                  s%vehdist  = s%vehdist  + V * DIS(L)
                  s%volume   = s%volume   + A
                  s%voldist  = s%voldist  + A * DIS(L)
                  s%voltime  = s%voltime  + A * TMV(LnRa)
                  s%volcost  = s%volcost  + A * TARIFA(LnRa)
                  CatOpSummary(ip, io) = s

                  IRANT=IR
                  ioa=io
                  IOANT=io
                  lnrant = lnra
               ENDDO       !  Fin Do enlaces JJ

!  Si opcion 2 agrega el viaje si el paso usa algun enlace indicado
               IF(IMP(2).AND.PASAN.AND.LCAT(IP)) THEN
                  VIAJOP(I,J)=VIAJOP(I,J)+A
               ENDIF

!  Si opcion 3 agrega el viaje si el paso tiene ITR transfers o mas
!  Se descuenta el primer abordaje
            IF(IMP(3).and.LMODE(K))THEN
               IF(ITRANS-1.LE.0.AND.ITR.EQ.0)VIAJOP(I,J)=VIAJOP(I,J)+A
               IF(ITR.GT.0.AND.(ITRANS-1.GE.ITR)) THEN
                   VIAJOP(I,J)=VIAJOP(I,J)+A
               ENDIF
             ENDIF

            IF(IMP(4).OR.IMP(5).OR.IMP(6))THEN
               if(lmode(k).and.lcat(ip))then
                     if(imp(4))DTC(J2)=disviaj*PROB
                  if(imp(5))DTC(J2)=tiemviaj*PROB
                  if(imp(6))DTC(J2)=cosviaj*PROB
                  VIAJOP(I,J)=VIAJOP(I,J)+DTC(J2)
               endif
            ENDIF

            ENDDO          !  Paso J2
         ENDIF             !  Fin If modo
      ENDDO                !  Modo K

      RETURN
      END SUBROUTINE

SUBROUTINE AddRouteFlows
  integer :: lnr, ir, io, l, nfrom, nto
  integer :: lcan, lnrcan, ircan
  integer :: irant, ioant
  real(8) :: vstan

  do lnr =1, NLinkRut
    l     = ilink(lnr)
    nfrom = ior(l)
    nto   = ides(l)

    ir = iroute(lnr)
    io = ioprut(ir)

    vstan = FreqNew(ir)*VS(io, itip(l) )

    if (itipop(io) /= 3) cycle

    do lcan = 1, NLINK
      if (ior(lcan) /= nto)    cycle
      if (ides(lcan) == nfrom) cycle

      do lnrcan = RutPri(lcan), RutUlt(lcan)
        ircan = iroute(lnrcan)
        if (ircan /= ir) cycle
        !! call AddFlow(nto, nfrom, numop(io), ides(lcan), numop(ioprut(ircan)), REAL(vstan))
      enddo
    enddo

    irant = ir
    ioant = io
  enddo

END SUBROUTINE

END PROGRAM MATESP

 subroutine usage
 USE GETOPTM
 character(32) prog
    prog = argv(0)

    print *
    print '(A,'' - Transport Matrix reports for TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options]'')', trim(prog)
    print '(4X, A,''  <scen> -C -m <mod1> -m <mod2> ... -c <cat1> -c <cat2>...'')', trim(prog)
    print '(4X, A,''  <scen> -D -m <mod1> -m <mod2> ... -c <cat1> -c <cat2>...'')', trim(prog)
    print '(4X, A,''  <scen> -F -n <node1> -n <node2> ...'')', trim(prog)
    print '(4X, A,''  <scen> -Y -n <node1> -n <node2> ...'')', trim(prog)
    print '(4X, A,''  <scen> -L -c <cat1> -c <cat2>... -l <orig>,<dest>'')', trim(prog)
    print '(4X, A,''  <scen> -M -m <mod1> -m <mod2> ... -c <cat1> -c <cat2>...'')', trim(prog)
    print '(4X, A,''  <scen> -O'')', trim(prog)
    print '(4X, A,''  <scen> -R <min> -m <mod1> -m <mod2> ...'')', trim(prog)
    print '(4X, A,''  <scen> -T -p <op1> -p <op2> ...'')', trim(prog)
    print *
    print *, 'If no options are given, the program enters interactive mode'


    print *
    print *, 'Commands are:'
    print *, '  -C        : Matrix of Costs'
    print *, '  -D        : Matrix of Distance'
    !! print *, '  -F        : Volumes flowing through nodes'
    !! print *, '  -Y        : Equivalent vehicles flowing through nodes'
    print *, '  -L        : Trips that use one or more links'
    print *, '  -M        : Matrix of Time'
    print *, '  -O        : Matrix of Transfers Between Operators'
    print *, '  -R <min>  : Matrix of trips with <min> or more transfers.'
    print *, '  -S        : Comma-delimited summary by category and operator.'
    print *, '  -T        : Trips by operator.'

    print *
    print *, 'Options are:'
    print *, ' (an <id> of 0 means "include all)"'
    print *, '  -c <id>      : Include category <id> in report.'
    print *, '  -d           : Read options from MATESP.DAT'
    print *, '  -l <id>,<id> : Include link <id>:<id> in report.'
    print *, '  -m <id>      : Include mode <id> in report.'
    !!! print *, '  -n <id>      : Include node <id> in report.'
    print *, '  -o <name>    : Write output to file <name>. Default is "AREAPOL.ESP"'
    print *, '  -p <id>      : Include operator <id> in report.'
    call ExplainStdOptions

    STOP 02
 end subroutine

