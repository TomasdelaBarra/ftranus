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
MODULE NODES
USE DEBUGM
USE PARAM
USE MENSAMOD

    character(80) :: NODES_RCS_ID = & 
      "$Id$" 

    integer,parameter :: MXNODTIPEXTRA=64
    integer,parameter :: NODES_VERSION=33619968

    integer(2) :: NNodes
    integer(4) :: NodeId(MXNODES)
    character(256) :: NodeName(MXNODES)

    real(8)           :: NodeX(MXNODES), NodeY(MXNODES)

    integer(1) :: NodeType(MXNODES), NodeFlag(MXNODES)
    integer(1) :: NodeExtra(MXNODES, MXNODTIPEXTRA)

    integer :: NNodeTypes
    integer(4) :: NodTipId(MXNODTIP)
    character(256) :: NodTipName(MXNODTIP)
    integer(1) :: NodTipExtra(MXNODTIP, MXNODTIPEXTRA)


    integer           :: NLinkNames
    integer(4) :: LinkNameId(NLMAX), LinkSrc(NLMAX), LinkDst(NLMAX)
    character(256) :: LinkName(NLMAX)
    

CONTAINS

character(256) function ReadString(iun)
  integer, intent(in) :: iun
  integer(1) :: l
  character(256) :: s

    read(iun) l
    if (l > 0) then
      read(iun) s(1:l)
    endif

    ReadString = s(1:l)
    return
end function

subroutine WriteString(iun, s)
  integer, intent(in) :: iun
  character(*), intent(in) :: s
  integer(1) :: l

  l = len(trim(s))

  write(iun) l
  if (l > 0) write(iun) s(1:l)
end subroutine
  
subroutine ReadNodeTypes(iun)
  integer, intent(in) :: iun
  integer :: i
  integer(1) :: l
  integer(1) :: n

  read(iun) n
  NNodeTypes = n

  if (NNodeTypes <= 0) then
    NNodeTypes = 7
    return
  endif

  do i = 1, NNodeTypes
    read(iun) NodTipId(i)


    NodTipName(i) = ReadString(iun)
    
    read(iun) NodTipExtra(i,:)

  enddo
end subroutine  

subroutine WriteNodeTypes(iun)
  integer, intent(in) :: iun
  integer :: i
  integer(1) :: n

  n = NNodeTypes
  write(iun) n
  do i = 1, NNodeTypes
    write(iun) NodTipId(i)

    call WriteString(iun, NodTipName(i))
    
    write(iun) NodTipExtra(i,:)
  enddo
end subroutine  

subroutine ReadNodes(iun)
  integer, intent(in) :: iun
  integer :: i

  read(iun) NNodes

  if (NNodes > MXNODES) then
    call mensa(03070,mensa_normal)
111 format(I6, A3, I6)
    write(*, 111) NNodes, " > ",MXNODES
    call mensa(03071,mensa_Aborta)
  endif

  do i = 1, NNodes
    read(iun) NodeId(i)


    NodeName(i) = ReadString(iun)
    
    read(iun) NodeType(i), NodeX(i), NodeY(i), NodeFlag(i)
    read(iun) NodeExtra(i,:)

  enddo
end subroutine  

subroutine WriteNodes(iun)
  integer, intent(in) :: iun
  integer :: i

  write(iun) NNodes
  do i = 1, NNodes
    write(iun) NodeId(i)

    call WriteString(iun, NodeName(i))
    
    write(iun) NodeType(i), NodeX(i), NodeY(i), NodeFlag(i)
    write(iun) NodeExtra(i,:)
  enddo
end subroutine  

subroutine ReadLinkNames(iun)
  integer, intent(in) :: iun
  integer :: i
  integer(4) :: id, src, dst
  integer(2) :: check

  i = 0
  read(iun) id, src, dst
  do while( src /= 0 .and. dst /= 0)
    i = i+1

    LinkNameId(i)  = id
    LinkSrc(i) = src
    LinkDst(i) = dst
    
    LinkName(i) = ReadString(iun)

    read(iun) check
    call assert(check == 0, 'no end of record reading link names')
    
    read(iun) id, src, dst
  enddo
  NLinkNames = i
end subroutine  

subroutine WriteLinkNames(iun)
  integer, intent(in) :: iun
  integer :: i
  integer(4) :: eof
  integer(2) :: check

  do i = 1, NLinkNames
    write(iun) LinkNameId(i), LinkSrc(i), LinkDst(i)
    
    call WriteString(iun, LinkName(i))

    check = 0
    write(iun) check
  enddo
  eof = 0
  write(iun) eof, eof, eof
end subroutine  

subroutine ReadNodesFile(iun)
  integer, intent(in) :: iun
  integer(4) :: version
  
  read(iun) version
  
  call ReadNodeTypes(iun)
  call ReadNodes(iun)
  call ReadLinkNames(iun)
end subroutine

subroutine WriteNodesFile(iun)
  integer, intent(in) :: iun
  integer(4) :: version

  version = NODES_VERSION
  
  write(iun) version
  
  call WriteNodeTypes(iun)
  call WriteNodes(iun)
  call WriteLinkNames(iun)
end subroutine

END MODULE NODES
