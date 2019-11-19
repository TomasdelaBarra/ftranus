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
MODULE GETOPTM
USE DEBUGM
USE RELEASE
USE OPTIONSM

      integer, parameter :: MXARGS=1024, MXARGLEN=64
      character, parameter :: STD_OPTIONS*(*) = 'g:hkqvVw?'

      integer,   private :: getopt_flagc
      character, private :: getopt_flagv(MXARGS)*1
      logical,   private :: getopt_hasarg(MXARGS)
      integer,   private :: getopt_argc
      integer,   private :: getopt_optc
      integer,   private :: getopt_paramc
      character, private :: getopt_argv(0:MXARGS)*(MXARGLEN)
      integer,   private :: getopt_optv(MXARGS)
      integer,   private :: getopt_optpos(MXARGS)
      integer,   private :: getopt_optargv(MXARGS)
      integer,   private :: getopt_parpos(MXARGS)

      logical :: SuppressHeaders = .false.

CONTAINS

      logical function hasopts()
         hasopts = getopt_optc > 0
      return
      end function

      logical function hasOpt(flag)
        character(1) :: flag
        integer :: i
        do i=1, optc()
          if (optv(i) == flag) then
            hasOpt = .true.
            return
          endif
        enddo
        hasOpt = .false.
        return
      end function


      integer function argc()
         argc = getopt_argc
      return
      end function

      character(MXARGLEN) function argv(i)
        integer, intent(in) :: i
        argv = getopt_argv(i)
      return
      end function

      integer function paramc()
         paramc = getopt_paramc
      return
      end function

      character(MXARGLEN) function paramv(i)
        integer, intent(in) :: i
        paramv = getopt_argv(getopt_parpos(i))
      return
      end function

      integer function optc()
        optc = getopt_optc
      return
      end function

      character(1) function optv(i)
        integer, intent(in) :: i
        optv = getopt_flagv(getopt_optv(i))
      return
      end function

      character(MXARGLEN) function optarg(i)
        integer, intent(in) :: i
        integer :: n
          call assert(i <= getopt_optc, 'No option ' // char(i + ichar('0')) )
          call assert(getopt_hasarg(getopt_optv(i)), 'Option -' // optv(i) // ' does not carry arguments.')
          n = getopt_optargv(i)
          call assert(n > 0, 'No argument for flag -' // optv(i))
          optarg = getopt_argv(n) 
      return
      end function

      integer function ioptarg(i)
         integer, intent(in) :: i
         character(MXARGLEN) :: buf
         integer n
         buf = trim(optarg(i))
         read(buf,'(I12)') n
         ioptarg = n
      return
      end function

      double precision function doptarg(i)
         integer, intent(in) :: i
         character(MXARGLEN) :: buf
         double precision n
         buf = trim(optarg(i))
         read(buf,*) n
         doptarg = n
      return
      end function

      subroutine optargrange(i, a, b)
         integer, intent(in) :: i
         character(*) :: a, b
         character(MXARGLEN) :: buf
         buf = trim(optarg(i))
         read(buf,*) a, b
      return
      end subroutine

      subroutine optargirange(i, min, max)
         integer, intent(in) :: i
         integer :: min, max
         character(MXARGLEN) :: buf
         buf = trim(optarg(i))
         read(buf,*) min,max
      return
      end subroutine

      subroutine optargfrange(i, min, max)
         integer, intent(in) :: i
         real :: min, max
         character(MXARGLEN) :: buf
         buf = trim(optarg(i))
         read(buf,*) min,max
      return
      end subroutine

      logical function getopts(options, Usage)
        character(*), intent(in) :: options
        optional :: Usage
        external Usage
        integer :: i, f, N, L, st, pos, s, e
        character :: arg*(MXARGLEN), afile*(MXARGLEN)
        character :: line*256
        character :: flag*1, c*1

        getopts = .false.

        getopt_argc    = 0
        getopt_argv    = ' '
        getopt_optc    = 0
        getopt_paramc  = 0
        getopt_optv    = 0
        getopt_optpos  = 0
        getopt_optargv = 0
        getopt_parpos  = 0

        getopt_argv(0) = prognam()

        if (.not. parseopts(options)) return

        N = command_argument_count()
        i = 1
        do while (i <= N)
          call get_command_argument(i, arg)
          L = len(trim(arg))
          c = arg(1:1)
          if (c /= '@') then
            getopt_argc = getopt_argc+1
            getopt_argv(getopt_argc) = arg(1:L)
          else
            afile = arg(2:L)
            open(777,FILE=afile,STATUS='OLD')
            do while (.true.)
               read(777, '(A256)', END=777) line
               L = len(trim(line))
               s = 1
               do while (s <= L)
                 if (line(s:s) /= ' ') then
                   e = s+1
                   do while (e <= L .and. line(e:e) /= ' ')
                     e = e + 1
                   enddo
                   getopt_argc = getopt_argc+1
                   getopt_argv(getopt_argc) = line(s:e-1)
                   s = e
                 endif
                 s = s +1
               enddo
            enddo
777         close(777)            
          endif
          i=i+1
        enddo

        i = 1 
        do while (i <= getopt_argc)
          arg = getopt_argv(i)
          L = len(trim(arg))
          c = arg(1:1)
          if (c /= '-' .and. c /= '/') then
             getopt_paramc = getopt_paramc+1
             getopt_parpos(getopt_paramc) = i
          else
             ! we have an option
             pos=2
888          do while (pos <= L)
                flag = arg(pos:pos)
                pos = pos+1
                ! now find the option
                do f = 1, getopt_flagc
                   if (getopt_flagv(f) == flag) then
                      getopt_optc = getopt_optc+1
                      getopt_optv(getopt_optc)   = f
                      getopt_optpos(getopt_optc) = i
                      if (getopt_hasarg(f)) then
                         call assert(pos > L .and. i < getopt_argc,  &
                              'Expected argument for option -' // flag // &
                              ' missing')
                         i = i+1
                         c = getopt_argv(i)(1:1)
                         call assert(c /= '-' .and. c /= '/', &
                              'Expected argument for option -' // flag &
                               // ', but found ' // getopt_argv(i))
                         getopt_optargv(getopt_optc) = i
                         goto 999
                      endif
                      goto 888
                   endif
                enddo
                call assert(.false., 'Unknown option -' // flag)
                if ( present(Usage) ) then
                   call Usage
                else
                   call StdUsage
                endif
             enddo
          endif
999       i = i+1
        enddo
        getopts = .true.

        if (debugging >= 100) then
           print *
           print *, 'argc = ', getopt_argc, N
           print *, 'optc = ', getopt_optc
           do i = 1, getopt_optc
             print 100, i, optv(i), getopt_optpos(i), getopt_hasarg(getopt_optv(i)), getopt_optargv(i)
100          format('option:', I2, 1X,A2, 1X,I2, 1X,L4,1X,I4,1X,A)
           enddo
           do i = 0, getopt_argc
             print 200, i, argv(i)
200          format('argc:', I2,1X,A)
           enddo
        endif

      return
      end function

      logical function parseopts(options)
        character(*), intent(in) :: options
        integer :: i, L
        character :: c*1

        parseopts = .false.

        getopt_flagc   = 0
        getopt_flagv   = ' '
        getopt_hasarg  = .false.

        L = len(trim(options))
        i = 1
        do while (i <= L)
          c = options(i:i)
          if (c == ':') then
             stop 'Problems with option list'
          else
             getopt_flagc = getopt_flagc + 1
             getopt_flagv(getopt_flagc) = c
             if (i < L .and. options(i+1:i+1) == ':') then
               getopt_hasarg(getopt_flagc) = .true.
               i = i+1
             endif
          endif
          i = i+1
        enddo
        parseopts = .true.
        if (debugging >= 100) then
           do i = 1, getopt_flagc
             write(*, 100) i, getopt_flagv(i), getopt_hasarg(i)
100          format('option:', I2, A2, L4)             
           enddo
        endif
      end function

      character(256) function prognam()
        integer  ibeg, iend
        character(256) :: tprog

        call get_command_argument (0, tprog)
        iend = len (trim(tprog))
        ibeg = iend
        do while (tprog(ibeg:ibeg) /= '/' .and. &
                  tprog(ibeg:ibeg) /= '\' .and.  &
                  tprog(ibeg:ibeg) /= ':' .and.  &
                  ibeg .gt. 1)
          if (tprog(ibeg:ibeg) == '.') then
            iend = ibeg-1
          endif
          ibeg = ibeg - 1
        enddo
        lprog = iend - ibeg
        prognam = tprog(ibeg:iend)
      return
      end function

      subroutine doStdOpts(Usage)
        optional :: Usage
        external Usage
        integer :: i
        character(80) :: ccfmt = '('' Copyright (c) 1985-'',I4, X, A)'
        character(32) :: prog

        do i = 1, optc()
            select case(optv(i))
                case ('k')
                  SuppressHeaders = .true.
                case ('g')
                  call SetDebugLevel(ioptarg(i)) !! debug
                case ('V')
                  call SetDebugLevel(dbg_verbose) ! verbose output
                case ('q')
                  call SetDebugLevel(dbg_quiet) ! be quiet
                case ('w')
                  use_powit = .true.
                case ('h','?')
                  if ( present(Usage)) then
                    call Usage
                  else
                    call StdUsage
                  endif
                case ('v')
                  prog = prognam()
                  print *, ''
                  print '(''TRANUS '',A,'' v'',A,'' '',A)', trim(prog), trim(ReleaseStr), trim(ReleaseName)
                  print ccfmt, ReleaseYear, 'Modelistica, Caracas'
                  print ccfmt, ReleaseYear, 'Juancarlo A~nez'
                  print ccfmt, 2007, 'Tomas de la Barra'
                  print ccfmt, 2002, 'Beatriz Perez'
                  print *, ''
                  print *, '(cc) This work is distrubuted under a Creative Commons'
                  print *, '     Attribution-ShareAlike 2.0 license'
                  print *, '     http://creativecommons.org/licenses/by-sa/2.0/' 
                  STOP
            end select
        enddo
      end subroutine

 subroutine StdUsage
 character(32) :: prog
    prog = argv(0)

    print *
    print '(A,'' - TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> [options]'')', trim(prog)
    print *
    print *, 'If no options are given, the program enters interactive mode'

    print *
    print *, 'Options are:'
    call ExplainStdOptions

    STOP
 end subroutine

 subroutine ExplainStdOptions
    print *, '  -g <lev>     : Set debugging level to <lev> (try 1000).'
    print *, '  -h           : This help.'
    print *, '  -k           : Suppress headers from any output'
    print *, '  -q           : Be very quiet.'
    print *, '  -v           : Display program version information.'
    print *, '  -V           : Verbose output.'
    print *
    print *, ' NOTE: A "@filename" parameter will read options from the file "filename".'
 end subroutine

end module GETOPTM
