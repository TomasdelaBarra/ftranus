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
MODULE EXOGALLOC
USE PARAM
USE GENER
USE LPARC
USE LCOMM

    character(80) :: EXOGALLOC_RCS_ID = & 
      "$Id$" 

 integer, parameter ::     &
   MXVCHANGE  = 2*MXSEC,   &  ! max number of exog variable changes
   MXVATTRAC  = 4*MXVCHANGE   ! max number of function components for changes

 ! variables that get to play in in exogenous changes
 integer, parameter :: &
     V_XPRO =  1, &
     V_XDEM =  2, &
     V_RMIN =  3, &
     V_RMAX =  4, &
     V_TPRO =  5, &
     V_PRIC =  6, &
     V_CAPA =  7, &
     V_ACCE =  8, &
     V_LAST = V_ACCE

! names of the variables
 character(4) :: VARNAME(1:V_LAST)
 DATA VARNAME / &
                'XPRO', &
                'XDEM', &
                'RMIN', &
                'RMAX', &
                'TPRO', &
                'PRIC', &
                'CAPA', &
                'ACCE'  &
                /

! types of change functions
 character(1), parameter :: &
     VF_LINEAR = 'L', &
     VF_POWER  = 'P', &
     VF_EXP    = 'E'

! change record. one of these for
TYPE XVChange
  integer     :: nsector
  integer     :: nvar
  real(8) :: value
  character(1) :: functype
  real(8) :: constant
  integer     :: firstAtrac
  integer     :: nAtrac
END TYPE

TYPE XVAtractor
  integer :: changeid
  integer :: atracSector
  integer :: nvar
  real(8) :: param
END TYPE

TYPE (XVChange)   xvchanges(MXVCHANGE)    ! exogenous changes, one per sector
TYPE (XVAtractor) xvatractors(MXVATTRAC) ! components of each change function

integer :: nxvchanges   ! number of changes to apply
integer :: nxvatractors ! number of atractor records used

DATA nxvchanges/0/, nxvatractors/0/

real(8) :: &
     CHNG_XPRO(MXZON, MXSEC), &
     CHNG_XDEM(MXZON, MXSEC), &
     CHNG_RMIN(MXZON, MXSEC), &
     CHNG_RMAX(MXZON, MXSEC)

real(8) :: Accesibility(MXZON, MXSEC)
logical  :: AccesibilityCalculated = .FALSE.

CONTAINS

!***********************************************************************
!* apply the complete list of changes
!* this routine must be called AFTER having read the previous scenario
!* but BEFORE reading the detailed zone/sector activity increments
subroutine ACXV_ApplyChanges(ipol)
  integer :: ipol
  integer :: ichange, i, n, nvar, iun
  real(8) :: atractor(NZ1), atractorSUM, change

  if (.not. polFileExists(ipol, 'L3E')) then
    RETURN
  endif

  call openPolFile(3, ipol, 'L3E', IO_FMT)
  call readExogChanges(3)
  close(3)

  ! default changes to zero
  do i = 1, NZ1
     do n=1, NS
        CHNG_XPRO(i, n) = 0.
        CHNG_XDEM(i, n) = 0.
        CHNG_RMIN(i, n) = 0.
        CHNG_RMAX(i, n) = 0.
     enddo
  enddo

  !apply changes
  do ichange=1, nxvchanges
    n      = xvchanges(ichange)%nsector
    nvar   = xvchanges(ichange)%nvar
    change = xvchanges(ichange)%value

    ! calculate atractors
    atractor = 0
    atractorSUM = 0
    do i=1, NZ1
      atractor(i) = atrain(i,n) * evalChangeFunction(ichange, i)
      if (atractor(i) < 0 ) then
         write(*,100) ichange, &
             NUMSEC(xvchanges(ichange)%nsector), NOMSEC(xvchanges(ichange)%nsector), &
             NUMZON(i)
100      format('Function ', I2, ' Sector ', I6,1X, A, 1X, ' Zone', I6)
         call MENSA(19003, mensa_Aborta)
      endif
      atractorSUM = atractorSUM + atractor(i)
    enddo
    if (atractorSUM /= 0) then
      do i=1, NZ1
        atractor(i) = atractor(i) / atractorSUM
      enddo
    endif

    ! apply the change
    do i=1, NZ1
      call changeSectorVar(i, n, nvar, atractor(i)*change)
    enddo
  enddo

  ! finally, set working values to new values
  do i = 1, NZ1
     do n=1, NS
        XPRO(i, n) = max(0.d+0, CHNG_XPRO(i, n) + XPRO(i, n))
        XDEM(i, n) = max(0.d+0, CHNG_XDEM(i, n) + XDEM(i, n))
        RMIN(i, n) = max(0.d+0, CHNG_RMIN(i, n) + RMIN(i, n))
        RMAX(i, n) = max(0.d+0, CHNG_RMAX(i, n) + RMAX(i, n))
     enddo
  enddo
end subroutine

!***********************************************************************
!* Evaluate a change function for a given zone
real(8) function evalChangeFunction(ichange, i)
  integer, intent(in) :: ichange ! index of change function
  integer, intent(in) :: i       ! zone
  real(8) :: vars(MXSEC), params(MXSEC)
  integer :: NA, k, ia
  real(8) :: res

  NA = xvchanges(ichange)%nAtrac
  if (NA > MXSEC) STOP 'ERROR:evalChangeFunction:too many atractors'

  do k = 1, NA
     ia = (k-1) + xvchanges(ichange)%firstAtrac
     if (xvatractors(ia)%changeid /= ichange) then
        STOP 'EXOGALLOC:evalChangeFunction: Internal error'
     endif
     params(k) = xvatractors(ia)%param
     vars(k)   = getSectorVar(i, xvatractors(ia)%atracSector, xvatractors(ia)%nvar)
  enddo

  res = xvchanges(ichange)%constant
  select case (xvchanges(ichange)%functype)
    case (VF_LINEAR, VF_EXP)
      do k=1, NA
        res = res + vars(k)*params(k)
      enddo
    case (VF_POWER)
      do k=1, NA
        res = res * vars(k)**params(k)
      enddo
  end select
  if (xvchanges(ichange)%functype == VF_EXP) then
    res = EXP(res)
    if (IsInfD(res)) then
      write(*,100) ichange, NUMSEC(xvchanges(ichange)%nsector), NOMSEC(xvchanges(ichange)%nsector)
100   format('Function ', I2, ' Sector ', I6,1X, A)
      call MENSA(19002, mensa_Aborta)
    endif
  end if
  evalChangeFunction = res
end function

!***********************************************************************
!*
subroutine readExogChanges(iun)
  integer, intent(in) :: iun ! file to read from
  integer     :: n, nc, na, ia
  character(4) :: svar, savar(MXSEC)
  character(1) :: sfunc
  real(8) :: value, const
  integer     :: nasec(MXSEC)
  real(8) :: aparam(MXSEC)

  read(iun,'(////)',END=999)

  do while (.true.)

    n =0
    nasec  = 0
    savar  = '    '
    aparam = 0
    read(iun,*,END=999,ERR=1000) &
      n, svar, sfunc, value, const, &
         (nasec(i), savar(i), aparam(i), i=1, MXSEC)

    if (n == 0 .or. svar == '----') EXIT

    if (nxvchanges >= MXVCHANGE) then
      write(*,*) 'L3E - (1.0) '
      call MENSA(9,mensa_Aborta)
    endif

    nxvchanges = nxvchanges + 1
    nc = nxvchanges

    xvchanges(nc)%nsector = INTNUM(n, NUMSEC, NS)
    if (xvchanges(nc)%nsector > NS) then
      write(*,*) 'L3E - (1.0) Sec:', n
      call MENSA(6,mensa_Aborta)
    endif
    xvchanges(nc)%nvar = strToVar(svar)
    if (xvchanges(nc)%nvar <= 0) then
      write(*,*) 'L3E - (1.0) variable:', svar
      call MENSA(6,mensa_Aborta)
    endif

    xvchanges(nc)%functype = sfunc
    select case(xvchanges(nc)%functype)
       case(VF_LINEAR, VF_POWER, VF_EXP)
         !nothing
       case default
         write(*,*) 'L3E - (1.0) functype:', sfunc
         call MENSA(6,mensa_Aborta)
    end select
    if (sfunc == VF_POWER .and. const == 0) then
      write(*,*) 'L3E - (1.0) Sec:', nasec(i), '/', NOMSEC(xvchanges(nc)%nsector)
      call MENSA(19000, mensa_Aborta)
    endif

    xvchanges(nc)%functype = sfunc
    xvchanges(nc)%value    = value
    xvchanges(nc)%constant = const

    xvchanges(nc)%firstAtrac = nxvatractors
    na = 0
    do i=1, MXSEC
      if (nasec(i) <= 0) EXIT
      na = na+1
      ia = (na-1)+xvchanges(nc)%firstAtrac
      xvatractors(ia)%changeid = nc

      xvatractors(ia)%atracSector = INTNUM(nasec(i), NUMSEC, NS)
      if (xvatractors(ia)%atracSector > NS) then
        write(*,*) 'L3E - (1.0) Sec:', nasec(i),'/', NOMSEC(xvchanges(nc)%nsector)
        call MENSA(6,mensa_Aborta)
      endif

      xvatractors(ia)%nvar = strToVar(savar(i))
      if (xvatractors(ia)%nvar <= 0) then
        write(*,*) 'L3E - (1.0) variable:', savar(i)
        call MENSA(6,mensa_Aborta)
      endif

      xvatractors(ia)%param = aparam(i)
      if (aparam(i) == 0) then
        write(*,*) 'L3E - (1.0) Sec:', nasec(i), &
                '/', NOMSEC(xvchanges(nc)%nsector), ' Atrac:', savar(i)
        call MENSA(19001,mensa_Aborta)
      endif
    enddo
    xvchanges(nc)%nAtrac = na
    nxvatractors = nxvatractors + na
   enddo
999   continue
      return
1000  if (n /= 0) then
         call mensa(10, mensa_Aborta)  ! Archivo incompleto
      endif
end subroutine

!***********************************************************************
!*
real(8) function getSectorVar(i, n, nvar)
   integer, intent(in)  :: i     ! the zone
   integer, intent(in)  :: n     ! the sector
   integer, intent(in)  :: nvar  ! variable to get
    select case  (nvar)
       case (V_XPRO)
          getSectorVar = XPRO(i,n)
       case (V_XDEM)
          getSectorVar = XDEM(i,n)
       case (V_RMIN)
          getSectorVar = RMIN(i,n)
       case (V_RMAX)
          getSectorVar = RMAX(i,n)
       case (V_TPRO)
          getSectorVar = PRO(i,n) + XPRO(i,n)
       case (V_PRIC)
           getSectorVar = PRECIO(i,n) + AJUSTE(i,n)
       case (V_CAPA)
           getSectorVar = MAX(0.d+0, RMAX(i,n) - (PRO(i,n) + XPRO(i,n)))
       case (V_ACCE)
           call CalculateAccesibility
           getSectorVar = Accesibility(i,n)
       case default
          STOP 'can''t happen'
    end select
end function

!***********************************************************************
!*
subroutine changeSectorVar(i, n, nvar, value)
   integer, intent(in)  :: i     ! the zone
   integer, intent(in)  :: n     ! the sector
   integer, intent(in)  :: nvar  ! variable to get
   real(8), intent(in)  :: value ! new value
    select case  (nvar)
       case (V_XPRO)
          CHNG_XPRO(i,n) = CHNG_XPRO(i,n) + value
       case (V_XDEM)
          CHNG_XDEM(i,n) = CHNG_XDEM(i,n) + value
       case (V_RMIN)
          CHNG_RMIN(i,n) = CHNG_RMIN(i,n) + value
       case (V_RMAX)
          CHNG_RMAX(i,n) = CHNG_RMAX(i,n) + value
       case default
          STOP 'can''t happen'
    end select
end subroutine


!***********************************************************************
!*
integer function strToVar(svar)
  character(*), intent(in) :: svar
  integer :: i
  strToVar = 0
  do i=1, V_LAST
    if (VARNAME(i) == svar) then
      strToVar = i
      return
    endif
  enddo
end function

subroutine CalculateAccesibility
  integer :: i, j, n
  real(8) :: S, P, A

  if (AccesibilityCalculated) RETURN

  do i=1, MXZON
    do n=1, MXSEC
      S = 0
      A = 0
      do j=1, MXZON
        P = PRO(j,n) + XPRO(j,n) 
        S = S + P
        A = utra(i,j) * P 
      enddo
      if (S > 0) then
        Accesibility(i,n) = A/S
      else
        Accesibility(i,n) = 0
      endif
    enddo
  enddo
  
  AccesibilityCalculated = .TRUE.
end subroutine

END MODULE EXOGALLOC
