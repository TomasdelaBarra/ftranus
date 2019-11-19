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
MODULE IO_LIST
USE MENSAMOD
USE DEBUGM

    character(80) :: IO_LIST_RCS_ID = &
      "$Id$" 
    
CONTAINS
      SUBROUTINE WriteListBegin(iun, count)
        integer iun   ! < archivo a escribir
        integer count ! < numero de elementos en la lista
        write(iun) count,-count
      END SUBROUTINE

      SUBROUTINE WriteListEnd(iun, count)
        integer iun   ! < archivo a escribir
        integer count ! < numero de elementos en la lista
        write(iun) count
      END SUBROUTINE

      SUBROUTINE WriteListItem(iun, irec)
        integer iun   ! < archivo a leer
        integer irec  ! < numero del elemento en la lista
        write(iun) irec
      END SUBROUTINE

      SUBROUTINE IOAssert(iun, pos, fact, msg, info)
        integer   iun
        integer   pos
        logical   fact
        integer   msg
        character, optional :: info*(*)
          character fname*256

        if (.not.fact) then
            INQUIRE(UNIT=iun, NAME=fname)
            write(*,*)
            write(*,*) 'error',pos, ':',fname, ':', info
            call Mensa(msg, mensa_Aborta)
        endif
      END SUBROUTINE


      SUBROUTINE IOCheckEq(iun, expected, actual, swhere, info)
        integer, intent(in) ::   iun, expected, actual
        character, intent(in) :: swhere*(*)
        character, intent(in), optional :: info*(*)
          character fname*256, msg*256

     
        if (expected /= actual) then
            INQUIRE(UNIT=iun, NAME=fname)
            write(msg,*) trim(fname), &
                         ':', trim(swhere)
            if (present(info)) then
              msg = trim(msg) // ':' // trim(info)
            endif
            call assertIntEq(expected, actual, msg)
        endif
      END SUBROUTINE

      SUBROUTINE ReadListBegin(iun, count, info)
        integer, intent(in) :: iun   ! < archivo a leer
        integer :: count ! > numero de elementos en la lista
        character, intent(in), optional :: info*(*)
        integer c
        read(iun) count, c
        if (present(info)) then
          call IOCheckEq(iun, -count, c, 'begin', info)
        else
          call IOCheckEq(iun, -count, c, 'begin')
        endif
      END SUBROUTINE

      SUBROUTINE CheckListBegin(iun, count, info)
        integer, intent(in) :: iun   ! < archivo a leer
        integer, intent(in) :: count ! < numero de elementos en la lista
        character, intent(in), optional :: info*(*)
        integer c, mc
        read(iun) c, mc
        if (present(info)) then
          call IOCheckEq(iun, count, c, 'cbegin', info)
          call IOCheckEq(iun, -count, mc, 'cbegin', info)
        else
          call IOCheckEq(iun, count, c, 'cbegin')
          call IOCheckEq(iun, -count, mc, 'cbegin')
        endif
      END SUBROUTINE

      SUBROUTINE CheckListEnd(iun, count, info)
        integer, intent(in) :: iun   ! < archivo a leer
        integer, intent(in) :: count ! < numero de elementos en la lista
        character, optional, intent(in) :: info*(*)
        integer c
        read(iun) c
        if (present(info)) then
          call IOCheckEq(iun, count, c, 'end', info)
        else
          call IOCheckEq(iun, count, c, 'end')
        endif
      END SUBROUTINE

      SUBROUTINE CheckListItem(iun, irec, info)
        integer, intent(in) :: iun   ! < archivo a leer
        integer, intent(in) :: irec  ! < numero del elemento en la lista
        character, optional, intent(in) :: info*(*)
        integer i
        read(iun) i
        if (present(info)) then
          call IOCheckEq(iun, irec, i, 'item', info)
        else
          call IOCheckEq(iun, irec, i, 'item')
        endif
      END SUBROUTINE

      SUBROUTINE CheckRead(c1, mc, c2, msg)
        integer, intent(in) ::   c1, mc, c2
        integer, intent(in) ::   msg

        call IOCheckEq(iun, c1, c2, 'read')
        call IOCheckEq(iun, -c1, mc, 'read')
      END SUBROUTINE

END MODULE IO_LIST
