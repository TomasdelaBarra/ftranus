! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo A�ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE CONTROL
USE PARAM
USE GENER
USE OPTIONSM
USE MENSAMOD

    character(80) :: CONTROL_RCS_ID = & 
        "$Id$" 
    
!  CONTROL.INC - DECLARACIONES DE CONTROL - TRANUS 4.0
      integer, parameter :: MXPOL = 150
      integer, parameter :: MXPOLNAME = 5

      character(5) :: NOMPOL(MXPOL)   ! Identificador de la politica
      character(32) :: DescPol(MXPOL)  ! Descripcion de la politica
      integer      iPrevPol(MXPOL) ! Numero interno de la politica anterior
      integer(1) ::     PrevPolType(MXPOL) ! Tipo de politica previa
      integer      NPOL            ! Numero de politicas


!  CONTROL.INC - DECLARACIONES DE CONTROL - TRANUS 4.0

      character(3) :: POL,AREA
      character(80) :: NOMBRE
      character(80) :: ESTUDIO


CONTAINS

! verificar que el numero de politica interna exista
      SUBROUTINE checkPol(ipol)
        integer   ipol

        if (ipol.le.0.or.ipol.gt.npol) then
          if (debugging >= dbg_debug) print *, ' NPOL=',ipol,'/',npol
          call mensa(msg_PoliticaNoDefinida,mensa_Aborta)
        endif
      END SUBROUTINE

! busca una politica y regresa su numero interno
      SUBROUTINE findPol(spol, ipol)
        character(3) :: spol
        integer      :: ipol

         call upper(spol)
         do ipol=1, npol
            if(spol.eq.nomPol(ipol)) return
         enddo
         ipol = 0
      END SUBROUTINE

! regresa el numero interno de la politica anterior
      SUBROUTINE prevPol(ipol, ipprev, type)
        integer   ipol
        integer   ipprev
        integer   type
          logical found
          integer ip

        ip = ipol
        call checkPol(ip)
        ipprev = iPrevPol(ip)
        if (type.eq.pol_OtherYear) then
           found = .FALSE.
           do while(ip.ne.0.and..not.found)
              if (PrevPolType(ip).eq.pol_OtherYear ) then
                 found = .TRUE.
              else
                 ip = iPrevPol(ip)
              endif
           enddo
           if(.not.found) then
              ipprev = 0
           else
              ipprev = iPrevPol(ip)
           endif
        else if (type.eq.pol_SameYear) then
             if (PrevPolType(ip).ne.pol_SameYear) then
                ipprev=0
             endif
        endif
      END SUBROUTINE

! registra una nueva politica, error si ya existe
      SUBROUTINE newPol(spol, spprev, desc)
        character(*) spol
        character(*) spprev
        character(*) desc
        integer   ipol
        integer   ipprev

         call upper(spol)
         call upper(spprev)

         if(spol(1:3) /= spol) then
           print *, 'POL= ', spol
           call mensa(msg_InvalidPolicyName,-1)
         endif

         call findPol(spol, ipol)
         if (ipol.ne.0) then
           print *, 'POL= ', spol
           call mensa(msg_PolicyRedefined,-1)
         endif

            ! buscar la politica anterior
         if (spprev.le.'   ') then
           ipprev = 0   ! no tiene politica anterior
         else
           call findPol(spprev, ipprev)
           if (ipprev.eq.0) then
              write(*,*) ' POL=', spprev
              call mensa(msg_PoliticaNoDefinida,mensa_Aborta)
           endif
         endif
         npol = npol+1
         nomPol(npol)   = spol
         iPrevPol(npol) = ipprev
         DescPol(npol)  = desc

         if (ipprev.eq.0) then
             PrevPolType(npol) = pol_Any
         elseif (spol(1:2).eq.spprev(1:2)) then
             PrevPolType(npol) = pol_SameYear
         else
             PrevPolType(npol) = pol_OtherYear
         endif
      END SUBROUTINE

! regresa los datos de una politica, error si no existe
      SUBROUTINE getPolData(ipol, spol, desc)
        integer :: ipol
        character(3), intent(out) ::  spol
        character(*), optional, intent(out) :: desc

        call checkPol(ipol)

        spol  = nomPol(ipol)
        if (present(desc)) then
           desc  = descPol(ipol)
        endif
      END SUBROUTINE

    ! construye el nombre de una archivo de politica
      SUBROUTINE polFileName(ipol,tipo,archivo)
        integer       ipol     !< politica
        character(*) tipo     !< tipo de archivo ('P0S')
        character(*) archivo  !> nombre del archivo ('xxxYYY.P0S')
        character(3) spol
        if (ipol.eq.0) then
          archivo = area(1:3) // '.' // tipo
        else
          call checkPol(ipol)
          archivo = area(1:3) // nomPol(ipol)(1:3) // '.' // tipo
        endif
        call upper(archivo)
      END SUBROUTINE

    ! verificar si una archivo existe
      logical FUNCTION polFileExists(ipol,tipo)
        integer      ipol
        character(3)  tipo
        logical   encontrado
        integer       i,p
        character(132) archivo, parchivo
        character(2)   prefix
        character(256) path
                                     
        archivo  = ' '
        parchivo = ' '
        prefix   = ' '
        path     = ' '

        do p=1,2
           do i=1,2
              call polFileName(ipol,tipo,archivo)
              parchivo = trim(path) // trim(prefix) // archivo
              inquire(FILE=parchivo, EXIST=encontrado)
              if (.not.encontrado.and.ipol.eq.0) then
                 call polFileName(1,tipo,archivo)
                 parchivo = trim(path) // trim(prefix) // archivo
                 inquire(FILE=parchivo, EXIST=encontrado)
              endif
              if (encontrado) exit
              prefix = 'W_ '
           enddo
           if (encontrado) exit
           if (ipol == 0)  exit
           path = trim(nomPol(ipol)) // dirsep()
        enddo
        polFileExists = encontrado
      END FUNCTION

! busca y abre un archivo entre una politica y sus anteriores
! error si no se encuentra
      SUBROUTINE findPolFile(iun, ipol, tipo, imodo, type, recurse)
        integer      iun
        integer      ipol
        character(3)  tipo
        integer      imodo
        integer      type
        logical, optional :: recurse
        integer      i, ip, p
        logical      encontrado
        character(132) archivo, parchivo
        character(2)   prefix
        character(256) path
        logical       donext
        
        if (present(recurse)) then
          donext = recurse .and. (ipol /= 0)
        else
          donext = .false.
        endif

        if (ipol /= 0) call checkPol(ipol)
        ip = ipol
        do while(.true.)
          path = ' '
          do p=1,2
            prefix = ' '
            do i=1,2
               call polFileName(ip,tipo,archivo)
               parchivo = trim(path) // trim(prefix) // archivo
               inquire(FILE=parchivo, EXIST=encontrado)
               if (encontrado) then
                 if (debugging >= dbg_debug) write(*,*) parchivo
                 call abre(iun, parchivo, imodo)
                 RETURN
               endif
               prefix = 'W_'
            enddo
            if (encontrado) RETURN
            if (ip /= 0) path = trim(nomPol(ip)) // dirsep()
          enddo
          if (donext) then
            call prevPol(ip, ip, type)
          else
            exit
          endif
        enddo

        ! error, no se encontro el archivo
        call polFileName(ipol, tipo, archivo)
        write(*,*)'  Pol = ', nomPol(ipol), '  ', tipo, ' ', parchivo
        call mensa(msg_FileNotFound,-1)
      END SUBROUTINE

! abre un archivo de una politica, error si no se encuentra
      SUBROUTINE openPolFile(iun, ipol, tipo, imodo)
        integer        iun
        integer        ipol
        character(3)   tipo
        integer        imodo
        character(100) archivo
        logical        encontrado

        call findPolFile(iun, ipol, tipo, imodo, pol_SameYear, .false.)
      END SUBROUTINE

! crea un archivo de una politica
      SUBROUTINE newPolFile(iun, ipol, tipo, imodo)
        integer        iun
        integer        ipol
        character(3)   tipo
        integer        imodo
        character(100) archivo

        call polFileName(ipol,tipo,archivo)
        if (ipol /= 0) archivo = trim(nomPol(ipol)) // dirsep() // trim(archivo)
        call creaArch(iun, archivo, imodo)
      END SUBROUTINE


! lee el archivo de politicas (control)
      SUBROUTINE initPol(sThisPol, iThisPol)
        character(3) sThisPol
        integer     iThisPol
        character(16) spol
        character(3)  sprev
        character(32) DESC
        character(132) ctlfile
        integer      IOS
        LOGICAL      encontrado
        integer      NoPowitTrans
        integer      NoPowitLoc

      nomPol   = ' '
      DescPol  = ' '
      iPrevPol = 0
      PrevPolType = pol_Any

      iThisPol = 0
      npol=0
      ctlfile = 'TRANUS.CTL'
      inquire(FILE=ctlfile, EXIST=encontrado)
      if(.not.encontrado)then
         inquire(FILE='W_TRANUS.CTL', EXIST=encontrado)
         if (encontrado) then
            ctlfile = 'W_TRANUS.CTL'
         endif
      endif
      if (debugging >= dbg_debug) write(*,*) ctlfile
      CALL ABRE(3,ctlfile,IO_FMT)
       !  Secci�n 1.0 - Identificaci�n del �rea de estudio
      READ(3,'(///)')
      READ(3,*,END=999,ERR=100)area,estudio
      CALL UPPER(area)
      if (debugging >= dbg_debug) WRITE(*,*)'Pol: ',sThisPol,'  AREA: ',area,'  ',estudio

!  Secci�n 2.0 - Descripci�n de los escenarios
      READ(3,'(//)')
      spol = '   '
      do while(.TRUE. .and. spol(1:3) .ne.'---' .and. spol(1:3) .ne. '*--')
         read(3,*,END=999,ERR=888,IOSTAT=IOS) &
               spol,desc,sprev
         spol = spol(1:3)
         call debug("read scenario: " // spol)
         if (trim(spol).ne.'---' .and. trim(spol).ne.'*--') then
            if (debugging >= dbg_debug) then
               write(*,'(''***DIAGNOSTIC CTL 2.0:'',3(A,1X))')spol,desc,sprev
            endif
            call newPol(spol,  sprev, desc)
         endif
      enddo
888   if (spol.ne.'---' .and. spol .ne. '*--') then 
        CALL CHECK(2.0,IOS,'CTL')
      endif

      NoPowitTrans = 1
      NoPowitLoc   = 1
      READ(3,*,END=55,ERR=50)
      !  Secci�n 3.0 - Tipo de logits
      call debug('Reading model options')
      READ(3,*,END=55,ERR=50)
      READ(3,*,ERR=100,END=999) PowitTrans
      call debug('Read TRANS model option')
      READ(3,*,END=55,ERR=50)
      READ(3,*,ERR=100,END=999) PowitLoc
      call debug('Read LOC model option')

55    use_powit_trans = (NoPowitTrans /= 1)
      use_powit_loc   = (NoPowitLoc   /= 1)

      READ(3,*,ERR=100,END=999)
      CALL CHECKIU(3,3.0,IOS,'CTL')
      call findPol(sThisPol, iThisPol)
      if(iThisPol.eq.0.and.sThisPol.ne.'   '.and.sThisPol.ne.' ')then
         WRITE(*,111) sThisPol,iThisPol
111      format(' Pol ="',A,'"',I3)         
         call mensa(msg_PoliticaNoDefinida, mensa_Aborta)
      endif
50    RETURN

        !  Final con errores

100   WRITE(*,*)'CONTROL(1)'
      CALL MENSA(3,-1)   ! E-G03: Problema de lectura en archivo entrada
999   WRITE(*,*)'CONTROL'
      CALL MENSA(10,-1)  ! E-G06: Archivo incompleto
      STOP 1
      END SUBROUTINE

      SUBROUTINE writePolInfo(iun)
        integer iun   !< unidad del archivo
        integer   i
        integer   ipol  ! politica actual

         write(iun) npol,-npol, &
                    ( i, &
                      iPrevPol(i), &
                      PrevPolType(i), &
                      nomPol(i), &
                      DescPol(i), &
                      i=1,npol &
                     ),npol
         call FindPol(POL, iPol)
         write(iun) iPol  !{!!!} change this to an external reference
      END SUBROUTINE

      SUBROUTINE readPolInfo(iun)
        integer iun   !< unidad del archivo
         integer   i, check, iPol
         read(iun) npol,check, &
                   (check, &
                   iPrevPol(i), &
                   PrevPolType(i), &
                   nomPol(i), &
                   DescPol(i), &
                   i=1,npol &
                   ), check
         read(iun) iPol
      END SUBROUTINE

      SUBROUTINE skipPolInfo(iun)
        integer iun   !< unidad del archivo
        integer     i, n, trash2, check, mcheck
        integer(1) :: trash1
        character(5)  nom
        character(32) desc

         read(iun) n,mcheck, &
                   ( &
                   check, &
                   trash2, &
                   trash1, &
                   nom, &
                   desc, &
                   i=1,n &
                   ), check
         read(iun) trash2
         if(-mcheck.ne.n.or.check.ne.n) STOP
      END SUBROUTINE

END MODULE CONTROL
