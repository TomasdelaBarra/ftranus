! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2011 Modelistica, Caracas
! * Copyright (C) 1983-2011 Tomas de la Barra
! * Copyright (C) 1985-2011 Juancarlo A�ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE DEBUGM
      integer, parameter :: &
         dbg_quiet   =   0, &
         dbg_normal  =  16, &
         dbg_verbose =  32, &
         dbg_debug   =  64
      integer :: debugging=dbg_normal

      character*256 dmsg

CONTAINS

      subroutine assert(fact, msg)
        logical :: fact
       character, optional, intent(in) :: msg*(*)
      !automatic
        if (.not. fact) then
          if (present(msg)) then
            call fail(msg)
          else
            call fail
          endif
        endif
      end subroutine

      subroutine assertIntEq(expected, actual, msg)
       integer, intent(in) :: expected, actual
       character, optional, intent(in) :: msg*(*)
      !automatic
        character*256 error_msg
        if (expected /= actual) then
          write(error_msg,*) 'expected <', expected, '>', & 
                             ' but was <', actual, '>' 
          if (present(msg)) then
            error_msg = trim(msg) // ': ' // trim(error_msg)
          endif
          call fail(error_msg)
        endif
      end subroutine

      subroutine assertCharEq(expected, actual, msg)
       character*(*), intent(in) :: expected, actual
       character, optional, intent(in) :: msg*(*)
      !automatic
        character*256 error_msg
        if (expected /= actual) then
          write(error_msg,*) 'expected <', trim(expected), '>', & 
                             ' but was <', trim(actual), '>' 
          if (present(msg)) then
            error_msg = trim(msg) // ': ' // trim(error_msg)
          endif
          call fail(error_msg)
        endif
      end subroutine

      subroutine fail(msg)
       character, optional, intent(in) :: msg*(*)
      !automatic
        print *
        if (present(msg)) then
           print 100, trim(msg)
        else
           print 100
        endif
100     format(' *** failure! : ', A)
        stop 1
      return
      end subroutine

      subroutine debug(msg, level)
        integer,   optional, intent(in) :: level
        character, intent(in) :: msg*(*)
        integer :: effective_level

        effective_level = dbg_debug
        if (present(level) ) then
          effective_level = level
        endif
        if (debugging >= effective_level) then
           print *, msg
        endif
      return
      end subroutine

      subroutine verbose(msg)
        character, intent(in) :: msg*(*)
      !automatic
        call debug(msg, dbg_verbose)
      return
      end subroutine

      subroutine info(msg)
        character, optional, intent(in) :: msg*(*)
      !automatic
        call debug(msg, dbg_normal)
      return
      end subroutine

    subroutine SetDebugLevel(level)
       integer(4) :: level
    !automatic
        debugging = level
    end subroutine


END MODULE DEBUGM
