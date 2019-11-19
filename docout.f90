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
MODULE DOCOUT

     character(80) :: DOCOUT_RCS_ID = & 
       "$Id$" 

CONTAINS

  subroutine DocStart(iun, title)
    integer, intent(in) :: iun
    character*(*), intent(in) :: title

    write(iun, *) '<?xml version="1.0" encoding="ISO-8859-1"?>'
    write(iun, *) '<!DOCTYPE html' 
    write(iun, *) '     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"'
    write(iun, *) '     "DTD/xhtml1-strict.dtd" >'
    write(iun, *) '<html xmlns:o="urn:schemas-microsoft-com:office:office"'
    write(iun, *) '      xmlns:x="urn:schemas-microsoft-com:office:excel"'
    write(iun, *) '      xmlns="http://www.w3.org/TR/REC-html40">'
    write(iun, *)  '     xml:lang="en" lang="en" >'
    write(iun, *) 
    write(iun, *) '<head>'
    write(iun, *) '  <meta http-equiv=Content-Type content="text/html; charset=windows-1252">'
    write(iun, *) '  <meta name=ProgId content=Excel.Sheet>'
    write(iun, *) '  <meta name=Generator content="Microsoft Excel 9">'
    write(iun, *) '  <!-- <link rel="stylesheet" type="text/css"  href="///c:/tranus/style"> -->'

    write(iun, *) 
    write(iun, *) '<style>'
    write(iun, *) '.float { mso-number-format:"_\(* \#\,\#\#0\.00_\)"; }'
    write(iun, *) '.head  { font-weight:700; }'
    write(iun, *) '</style>'
    write(iun, *) 
    write(iun, *) '</head>'
    write(iun, *) '<body>'
    write(iun, *) '  <h1>' // trim(title) // '</h1>'
    write(iun, *) 
  end subroutine

  subroutine DocEnd(iun)
    integer, intent(in) :: iun

    write(iun, *) 
    write(iun, *) '</body>'
    write(iun, *) '</html>'
  end subroutine

  subroutine DocBreak(iun)
    integer, intent(in) :: iun

    write(iun, *) '<br />'
  end subroutine

  subroutine DocFormattedStart(iun)
    integer, intent(in) :: iun

    write(iun, *) '<pre>'
  end subroutine

  subroutine DocFormattedEnd(iun)
    integer, intent(in) :: iun

    write(iun, *) '</pre>'
  end subroutine

  subroutine DocTableStart(iun) 
    integer, intent(in) :: iun

    write(iun, *) '<table>'
    write(iun, *) '  <tbody>'
  end subroutine

  subroutine DocTableEnd(iun) 
    integer, intent(in) :: iun

    write(iun, *) '  </tbody>'
    write(iun, *) '</table>'
  end subroutine
  
  subroutine DocTableRowStart(iun) 
    integer, intent(in) :: iun

    write(iun, *) '    <tr>'
  end subroutine

  subroutine DocTableRowEnd(iun) 
    integer, intent(in) :: iun

    write(iun, *) '    </tr>'
  end subroutine

  subroutine DocTableHeadInt(iun, value)
    integer, intent(in) :: iun
    integer, intent(in) :: value
    write(iun, *) '      <th class="head" >', value, '</th>'
  end subroutine

  subroutine DocTableHeadChar(iun, value)
    integer, intent(in) :: iun
    character*(*), intent(in) :: value
    write(iun, *) '      <th class="head" >', value, '</th>'
  end subroutine


  subroutine DocTableCellInt(iun, value, head)
    integer, intent(in) :: iun
    integer, intent(in) :: value
    logical, intent(in), optional :: head
    logical :: ishead = .false.
    if (present(head)) then
      ishead = head
    endif
    if (ishead) then
      write(iun, *) '      <td class="head">', value, '</td>'
    else
      write(iun, *) '      <td>', value, '</td>'
    endif
  end subroutine

  subroutine DocTableCellReal(iun, value, head)
    integer, intent(in) :: iun
    real,    intent(in)    :: value
    logical, intent(in), optional :: head
    logical :: ishead = .false.
    if (present(head)) then
      ishead = head
    endif
    if (ishead) then
      write(iun, 101) value
    else
      write(iun, 100) value
    endif
100 format('       <td class="float" >',G18.9,'</td>')
101 format('       <td class="head"  >',G18.9,'</td>')
  end subroutine

  subroutine DocTableCellDouble(iun, value, head)
    integer, intent(in) :: iun
    real(8),  intent(in)    :: value
    logical, intent(in), optional :: head
    logical :: ishead = .false.
    if (present(head)) then
      ishead = head
    endif
    if (ishead) then
      write(iun, 101) value
    else
      write(iun, 100) value
    endif
100 format('       <td class="float" >',G18.9,'</td>')
101 format('       <td class="head"  >',G18.9,'</td>')
  end subroutine

  subroutine DocTableCellChar(iun, value, head)
    integer, intent(in) :: iun
    character*(*), intent(in) :: value
    logical, intent(in), optional :: head
    logical :: ishead = .false.
    if (present(head)) then
      ishead = head
    endif
    if (ishead) then
      write(iun, *) '      <td class="head">', value, '</td>'
    else
      write(iun, *) '      <td>', value, '</td>'
    endif
  end subroutine
  
END MODULE DOCOUT
