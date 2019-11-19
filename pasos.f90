! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2011 Modelistica, Caracas
! * Copyright (C) 1983-2011 Tomas de la Barra
! * Copyright (C) 1985-2011 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM PASOSNG
USE DEBUGM
USE GETOPTM
USE PARAM
USE RCOMM
USE PCOMM
USE ZCOMM
USE TPARC
USE CONTROL
USE GENER
USE IO_LIST
USE ASCII
USE HEAPQ
USE NODES

    character(80) :: PASOS_RCS_ID = &
      "$Id$" 

      LOGICAL(1) :: REPITE,REDANT
      CHARACTER(1) IIP
      integer   IPOL,IPANT,PARCHEIPOL
      integer   status
      integer :: date(3)
      integer :: IAN,MES,IDIA, I2
      integer :: hora, minuto, segundo, milesima
      integer :: IHR,MINS,ISEC,MILESM

      external Usage
      
      REPITE=.FALSE.
      CALL INIT(POL,IIP,.FALSE.,3001, Usage)

      call debug('inicio')

      call InitPol(POL,IPOL)
      ParcheIPOL = IPOL
      
      call debug('pol')
      
      call PrevPol(IPOL,IPANT, pol_Any)
      REDANT= IPANT.NE.0

      call debug('prev pol')

      call GetCurrentDate(ian, mes, idia)
      call GetCurrentTime(IHR, MINS, ISEC, MILESM)

      if (debugging >= dbg_normal) then
        CALL MENSA(1004,0)
        print *, '_________________________________________'
      endif


      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)

      call debug('read Z1E')
	  WRITE(*,*)'Prueba SIN GUS'
      !call OpenPolFile(3, 0, 'GUS', IO_BIN)
      !CALL ReadNodesFile(3)
      !CLOSE(3)

      call debug('read GUS')

      call FindPolFile(3,IPOL,'P0E',IO_FMT, pol_Any)
      call debug('found P0E')
      CALL LEEP0E
      CLOSE(3)
      call debug('read P0E')

      call FindPolFile(3, IPOL, 'T1E', IO_FMT,pol_Any)
      call LEET1E
      close(3)
      call debug('read T1E')

      call debug('read P1E')
      call OpenPolFile(11,IPOL,'P1E',IO_FMT)
      CALL LEERED(11)
      CLOSE(11)

      call newPolFile(4,IPOL,'P0S',IO_BIN)
      CALL WRTPAR(4, status, 0,IDIA,MES,IAN,IHR,MINS )
      call CheckStatus(status)
      CALL WRREDANT(4, status)
      call CheckStatus(status)
      CLOSE(4)
      call debug('create P0S')

      if (debugging >= dbg_normal) then
        print *, '_________________________________________'
        CALL MENSA(3011,0)  ! paths by mode
      endif

      call InitSearchStructures(NZN, NLINK, NLINKRUT)
      call PathSearch(ParcheIPOL, status)

      CLOSE(4)
      if (debugging >= dbg_normal) then
        print * 
        print *, '_________________________________________'
        CALL TIEJEC(IHR,MINS,ISEC,MILESM)
        CALL MENSA(8,0)
        STOP' P A S O S'
      endif
CONTAINS


      SUBROUTINE CalcOverlapping(nps, ncol, matpas, ioverl)
      integer(2) :: nps
      integer(2) :: ncol(MXPAS)
      INTEGER   matpas(MXPAS, MXARC)
      integer(1) ::  ioverl(MXPAS, MXARC)

      integer ::  INX, JNX, L, L1, NP, NPABS

         ioverl= 1
         DO L=1, nps-1
            DO INX= 1, ncol(l)
               NP=ABS(MATPAS(L,INX))
               DO L1=l+1, nps
                  DO JNX=1, NCOL(L1)
                     NPABS=ABS(MATPAS(L1, JNX))
                     IF(NPABS.EQ.NP)THEN
                        ioverl(L,INX)=ioverl(L,INX)+1
                        ioverl(L1,JNX)=ioverl(L1,JNX)+1
                        EXIT
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      RETURN
      END SUBROUTINE


      SUBROUTINE PASMIN(k, i, iunk, status)
      integer, intent(in) ::   k, i, iunk
      integer, intent(out) :: status
      integer :: ihmax = 0
      logical :: errors
      
      integer(1) ::  overlap(MXPAS, MXARC)


      ihmax = heapmax()
       
      if (debugging >= dbg_normal) then
            write(*, &
               '(A1,''  PAS   Mod'',I5, '' Z '',I7,''=('',I3,''%)'', '' h=('',I2,2H%),20X,'' '',$)') &
               13,nummod(k), &
               numzon(i),(100*i)/nzn, &
               (100*ihmax)/heapmax()
        endif
        nps = 0
        errors = .FALSE.
        do j=1,nzn
          if(ConSubZonas(j)) CYCLE
          if(j.eq.i) CYCLE
          call FindODPaths(k, i, j, gcmin(j,:), gpmin(j,:), status)
          if(status.ne.msg_OK) then
               write(*,9910) numzon(i),numzon(j),nummod(k)
9910           format(' Org:',I8,' Des:',I8, ' Mod:',I6)
               call mensa(status, mensa_Aborta)
               errors = .TRUE.
               status=msg_OK
          endif

          if (errors) then
            status = msg_ConectivityErrors
            return
          endif

          call CalcOverlapping(nps, ncol, matpas, overlap)
          write(iunk) nps, &
                     ( &
                        path_delay(ipord(l)), &
                        ncol(ipord(l)), &
                        ( &
                            matpas(ipord(l),ic), &
                            overlap(ipord(l),ic),  &
                        ic=1,ncol(ipord(l)) &
                        ), &
                     l=1,nps &
                     )
       enddo
      RETURN
      END SUBROUTINE

      SUBROUTINE PathInitMode(k)
       INTEGER k
         call calcKCost(k)
         do lnr=1, NLINKRUT
            lnrData(lnr)%cosOvr=ctmv(lnr)
            lnrData(lnr)%cosTot=ctmv(lnr)
         enddo
      RETURN
      END SUBROUTINE


      SUBROUTINE InitializeConnectivity
        INTEGER l

        call debug('InitializeConnectivity Enters')
        MATZ=0
        MAT =0
        iozon=0
        idzon=0
        numnodes = 0

        do l=1,nlink
          ii = intnum(ior(l), numzon, nzn)
          if (ii.le.nzn) iozon(l)=ii
          ii = intnum(ides(l), numzon, nzn)
          if (ii.le.nzn) idzon(l)=ii
        enddo
      call debug('InitializeConnectivity Exits')
      RETURN
      END SUBROUTINE

      SUBROUTINE CalcZoneConnectivity(k, i, inversa, status)
      INTEGER k,i
      LOGICAL inversa
      INTEGER status

      INTEGER lnr
      INTEGER l

        status = msg_OK
        numnodes = numnodes+1
        matz(i)=numnodes
        do l=1, NLINK
        if(.not.inversa.and.iozon(l).ne.i)     CYCLE
        if(inversa.and.idzon(l).ne.i)          CYCLE
        do lnr=RutPri(l),RutUlt(l)
           ir = iroute(lnr)
           io = ioprut(ir)
           ik = modoper(ioprut(iroute(lnr)))
           if (ik .ne.k) CYCLE
           if (.not.inversa) then
              cm = cosmin(io,io)+ ctme(lnr)
           else
              cm = 0
           endif
           if (cm < 0 .or. cm >= RINF) CYCLE
           graph(numnodes)%linkrut = lnr
           graph(numnodes)%cost    = cm
           graph(numnodes)%delay  = 0.0
           if(numnodes.ge.MXGNODES) then
               status = msg_DemasiadosEnlacesZona
               return
           endif
           numnodes=numnodes+1
        enddo
        enddo
        graph(numnodes)%linkrut=0
        numnodes=numnodes+1
        if(graph(matz(i))%linkrut == 0)then
           status = msg_ZonaNoConectada
           return
        endif

      RETURN
      END SUBROUTINE

      SUBROUTINE CalcLinkRouteConnectivity(k, lnr, inversa, status)
      INTEGER k
      INTEGER l
      INTEGER status
      LOGICAL inversa

      INTEGER lnr, lnrcan, ii, lcan
      INTEGER irfte, ircan, iofte, iocan
      INTEGER kcan, kfte
      INTEGER ldes
      integer(4) :: ia
      logical   lnrFound
      logical turnProhib

         status = msg_OK
         numnodes=numnodes+1
         lnrData(lnr)%connected=numnodes
         l = ilink(lnr)
         if(.not.inversa) then
           if(idzon(l).ne.0) return
         else
           if(iozon(l).ne.0) return
         endif
        irfte = iroute(lnr)
        iofte = ioprut(irfte)
        kfte  = modoper(iofte);
        if (kfte .ne.k) RETURN

        lnrFound = .FALSE.
        do ldes=1, NLINK
            if(.not.inversa) then
                  if(ides(l).ne.ior(ldes)) CYCLE
                  if(ides(ldes).eq.ior(l)) CYCLE
            else
                  if(ides(ldes).ne.ior(l)) CYCLE
                  if(ior(ldes).eq.ides(l)) CYCLE
            endif
            turnProhib = .FALSE.
            delay = 0.0
            do ii=1, MXGIR
                if (.not.inversa .and. nogir(l, ii) == ides(ldes)) then
                  delay = turn_delay(l, ii) 
                  turnProhib = delay < 0
                  exit
                elseif(inversa .and. nogir(ldes, ii) == ides(l)) then
                  delay =turn_delay(ldes, ii) 
                  turnProhib = delay < 0
                  exit
                endif
            enddo
            do lnrcan= RutPri(ldes), RutUlt(ldes)
              ircan = iroute(lnrcan)
              iocan = ioprut(ircan)
              kcan  = modoper(iocan)
              if (kcan .ne.k) CYCLE

               lcan=lnrcan
               if (turnProhib) lcan = -lcan
               if (lcan < 0) then
                 if (.not. inversa) then
                   if (itipop(iocan) == 1 .or. itipop(iocan) == 2) CYCLE
                 else
                   if (itipop(iofte) == 1 .or. itipop(iofte) == 2) CYCLE
                 endif
               endif

               cm = 0
               if(irfte.ne.ircan.or.lcan.lt.0) then
                   if(NoStops(lnr).or.NoStops(lnrcan)) CYCLE
                   if (.not.inversa) then
                      cm = cosmin(iofte, iocan)
                   else
                      cm = cosmin(iocan, iofte)
                   endif
                   if(cm.lt.0.or.cm.ge.RINF)CYCLE
                   if (.not.inversa) then
                     cm = cm + ctme(lnrcan)
                   else
                     cm = cm + ctme(lnr)
                   endif
                endif

               lnrFound = .TRUE.

              ! nuevo, guardarlo
               if(numnodes.ge.MXGNODES) then
                  status = msg_TooManyAdyacencies
                  return
                endif
                graph(numnodes)%linkrut = lcan
                graph(numnodes)%cost    = cm
                if (delay >= 0) then
                  graph(numnodes)%cost    = graph(numnodes)%cost + VOTMV(k) * delay
                endif
                graph(numnodes)%delay   = delay
                numnodes=numnodes+1
             enddo! Fin enlace-ruta candidato lnrcan
         enddo ! fin the enlace candidato ldes

         if(.not.lnrFound)then
             if(.not.inversa) then
                status = msg_NoHayEnlacesSalida
             else
                status = msg_NoHayEnlacesEntrada
             endif
             write(*,'(/,3H Or,I6,5H  Des,I6,3H  T,I6,3H  R,I6)') &
                ior(l),ides(l), numtip(itip(l)), numrut(irfte)
             print * 
         endif
         ! marcar el final de la lista
         graph(numnodes)%linkrut = 0

      RETURN
      END SUBROUTINE

      SUBROUTINE CalcConnectivity(k, inversa, status)
      INTEGER k
      LOGICAL inversa
      INTEGER status

      INTEGER i, io, iocan
      LOGICAL errors
      integer(4) :: l100 = 100

         call debug('CalcConnectivity Enters')
         errors = .FALSE.
         ! calcular una matriz de transferencia de modos
         ! solo permitimos transferir de este K a Ki, pero no de Ki, a Kj
         call debug('CalcConnectivity About to call InitializeConnectivity')
         call InitializeConnectivity
         call debug('CalcConnectivity InitializeConnectivity finished')
         do i=1, NZN
            if(ConSubZonas(i)) CYCLE
            call CalcZoneConnectivity(k, i, inversa, status)
            if (status.ne.0) then
              write(*,'('' Zon'',I6)')numzon(i)
              call mensa(status,0)  ! P10: zona no conectada por este modo
              print * 
              errors = .TRUE.
              if (status.eq.msg_DemasiadosEnlacesZona) exit
           endif
         enddo
         if(errors) then
            status = msg_ConectivityErrors
         else
            status = msg_OK
         end if

        call debug('CalcConnectivity done with Zones')
        if(status.eq.msg_OK) then
              do lnr= 1, NLINKRUT
                 if (debugging >= dbg_normal) then
                     if (mod(lnr,500) == 0 .or. lnr == NLINKRUT) then
                         write(*,'(A1,''  ADY   Mod '',I4,' &
                                // ''' ('',I3,''%)'',''  a=('',I3,''%)'',' &
                                // '5X,''  ''$)') &
                           13,nummod(k),(L100*lnr)/NLINKRUT, (L100*numnodes)/MXGNODES
                     endif
                 endif
                 call CalcLinkRouteConnectivity(k, lnr, inversa, status)
                 if (status.ne.0) then
                     call Mensa(status, 0)
                     errors = .TRUE.
                     if (status.eq.msg_TooManyAdyacencies) exit
                 endif
             enddo
        endif
        if (debugging >= dbg_normal) then
          if(.not.inversa)print * 
        endif
        if(errors) then
            call Mensa(msg_ConectivityErrors, 0)
            status = msg_OK
        end if
      END SUBROUTINE



      SUBROUTINE getcmins(k)
         integer, intent(in) :: k
         integer :: i, status

         do i=1, NZN
           if(.not.ConSubZonas(i)) then
               call cpasmin(i, gcmin(i,:), gpmin(i,:), status)
               if(status.ne.0) then
                  call mensa(status, mensa_Aborta)
               endif
               if (debugging >= dbg_normal) then
                 if (mod(i,16) == 0 .or. i == NZN) then
                    WRITE(*,'(A1,''  CMIN  Mod'',I5,'' ('',I3,''%)'',30X,'' '',$)') &
                        13,NUMMOD(K),(100*I)/NZN
                 endif
               endif
           endif
         enddo
       return
       END SUBROUTINE


       subroutine savePaso(path, pathSize, cpas, status)
         integer path(MXARC)
         integer pathSize
         real    cpas
         integer status

         integer(1) ::  ip, ipn
         integer l
         logical repetido

         status = msg_OK

         !Verificar que el paso no este repetido
         do ip=1, nps
           if(pathSize.eq.ncol(ip)) then
             l=1
             repetido=.TRUE.
             do while(l.le.ncol(ip).and.repetido)
                repetido=path(l).eq.matpas(ip,l)
                l = l+1
             enddo

             if(repetido) then
                status = msg_EndOfPathSearch
                RETURN ! Se repitio el paso, fin
             endif
           endif
         enddo
         status = msg_OK
         !Guardar el paso
         nps= nps+1
         ncol(nps) = pathSize
         do l=1,pathSize
           matpas(nps,l) = path(l)
         enddo
         cospas(nps)=cpas

         ! calculate total delay
         path_delay(nps) = 0.0
         do l=1,pathSize-1
            iln = ilink(abs(path(l)))
            ito = ides(ilink(abs(path(l+1)) ))
            do igir = 1, MXGIR
              if (NoGir(iln, igir) == 0) then
                 exit
              elseif (NoGir(iln,igir) == ito) then
                 if (turn_delay(iln,igir) > 0) then
                    path_delay(nps) =path_delay(nps) + turn_delay(iln,igir)
                 endif
              endif
            enddo
         enddo
         ! Insert-sort
         ipn=nps
         do while(ipn.gt.1)
           if(cpas.ge.cospas(ipord(ipn-1))) exit
           ipord(ipn)=ipord(ipn-1)
           ipn=ipn-1
         end do
         ipord(ipn)=nps
       RETURN
       END SUBROUTINE

       subroutine overlap(path, n)
         integer path(MXARC)
         integer n

         integer l, lnrAbs, io, it, iovr
         double precision ::  oz

       do l=1, n
          lnrAbs = abs(path(l))
          it = iTip(iLink(lnrAbs))
          io = iOpRut(iRoute(lnrAbs))

          iovr = 1 + lnrData(lnrAbs)%iover 
          lnrData(lnrAbs)%iover = iovr
          oz = (1+ iovr*OzLtOper(io,it))
          lnrData(lnrAbs)%cosOvr=lnrData(lnrAbs)%cosTot*oz
       enddo
       return
       END SUBROUTINE

    SUBROUTINE BuildMinPath(i,j,cmin,pmin,path,n,cpas,status)
       integer   i
       real    cmin(*)
       integer pmin(*)
       integer path(*)
       integer n
       real    cpas
       integer status

       real    MaxCostSeen, ct
       integer lnr, lnrAbs, nc, l,io
       integer(4) :: inode

        status = msg_OK

        inode = matz(i)
        lnr = 0
        MaxCostSeen  = rinf
        do while(graph(inode)%linkrut /= 0)
            nc = abs(graph(inode)%linkrut)
            io = ioprut(iroute(nc))
            ct=cmin(nc)+lnrData(nc)%cosTot+graph(inode)%cost
            if(ct.lt.MaxCostSeen) then
              lnr = nc
              MaxCostSeen = ct
            endif
            inode=inode+1
       enddo
          if (lnr.eq.0) then
             status = msg_NoPath
             return
          endif

          ! Colocar los enlaces en path
          l = lnr
          nc = 0
          do while(l.ne.0)
            lnrAbs=abs(l)
            nc=nc+1
            if(nc.gt.MXARC) then
               status = msg_TooManyLinksInPath
               return
            endif
            path(nc)=l
            l=pmin(lnrAbs)
          enddo
          n    = nc
          cpas = MaxCostSeen
       return
       END SUBROUTINE

    real function node_cost(inode)
        implicit none
        integer, intent(in) :: inode
        integer :: lnr

        lnr = abs(graph(inode)%linkrut)
        node_cost= graph(inode)%cost + lnrData(lnr)%cosTot
    end function

    real function node_ovr_cost(inode)
        implicit none
        integer, intent(in) :: inode
        integer :: lnr

        lnr = abs(graph(inode)%linkrut)
        node_ovr_cost = graph(inode)%cost + lnrData(lnr)%cosOvr
    end function


      SUBROUTINE BuildPath(pmin, lnr, path, n, status)
       integer pmin(*)
       integer lnr
       integer path(*)
       integer n
       integer status

       integer lnrAbs, l, i

         l = lnr
         n = 0
         do while(l.ne.0)
           n=n+1
           lnrAbs=abs(l)
           l=pmin(lnrAbs)
         enddo
         if(n.ge.MXARC) then
            status = msg_TooManyLinksInPath
            return
         endif

         i = n
         l = lnr
         do while(l.ne.0)
           path(i) = l
           i=i-1
           lnrAbs=abs(l)
           l=pmin(lnrAbs)
         enddo
      RETURN
      END SUBROUTINE

    subroutine InitPathSearchForZone(i)
        integer, intent(in) :: i
        real(8) :: cost

        call heapalloc(MXCON*NLINKRUT)
        inode = matz(i)
        do while(graph(inode)%linkrut /= 0)
            cost = node_ovr_cost(inode)
            call heappush(cost, inode)
            inode = inode+1
        end do
    end subroutine

      SUBROUTINE FindPath(i, j, astar, path, pathSize, cost, status)
      real      astar(*)
      integer   path(*)
      integer   pathSize
      integer   status

      integer(4) :: IHMAX = 0

      real    :: xcmin(NLINKRUT)
      integer :: xpmin(NLINKRUT)
      real    :: cosReal(NLINKRUT)

      integer   l, lnrCan, irfte, iofte,lrc
      double precision ::    ctransf,costocan, MaxCostSeen, pcost
      integer   nh, lnr, lnrAbs, ircan
      logical(1) :: mark(NLINKRUT)
      integer(4) :: inode, cnode

177   format(/,' N', I6,' Or',I6,'  Des',I6,'  T',I6,'  R',I6,'  C',2F12.5)

            xcmin = RINF
            xpmin = 0
            cosReal = RINF

            call heapalloc(MXCON*NLINKRUT)
            inode = matz(i)
            do while(graph(inode)%linkrut /= 0)
                pcost = node_ovr_cost(inode)
                lnr = abs(graph(inode)%linkrut)
                xcmin(lnr) = pcost
                cosReal(lnr) = node_cost(inode)
                call heappush(pcost + astar(lnr), inode)
                inode = inode+1
            end do

            status = msg_NoPath
            mark =.FALSE.
            MaxCostSeen=0
            nhmax   = 0
            do while(.not. heapempty())
              call heappop(pcost, inode)
              lnr = graph(inode)%linkrut
              lnrAbs=abs(lnr)
              l   = ilink(lnrAbs)
              if (mark(lnrAbs)) CYCLE
              mark(lnrAbs)=.TRUE.
              if(.false..and.MaxCostSeen /= 0 .and. (pcost/MaxCostSeen) < 0.999991) then
                print *,  'PASMIN ',pcost,MaxCostSeen, pcost/MaxCostSeen, heaplen
                print *,  '  from ',numzon(i), ' to', numzon(j)
                status = msg_PasosInconsistente
                call heapcheck
                return
              endif
              MaxCostSeen=pcost
              nf=idzon(l)
              if(nf /= 0) then
                if(nf == j)  then
                   status=msg_OK
                   exit ! end of search
                endif
                cycle  ! paths cannot go through zones
              endif
              ! search forcandidates
              cnode  = lnrData(lnrAbs)%connected
              do while(graph(cnode)%linkrut /= 0)
                lnrcan = abs(graph(cnode)%linkrut)
                if (.not. mark(lnrcan)) then
                    costocan = xcmin(lnrAbs) + node_ovr_cost(cnode)
                    if (costocan < xcmin(lnrcan)) then
                        xcmin(lnrcan) = costocan
                        xpmin(lnrcan) = lnr
                        cosReal(lnrcan) = cosReal(lnrabs) + node_cost(cnode)
                        costocan = costocan + astar(lnrcan)
                        call heappush(costocan, cnode)
                    endif
                endif
                cnode = cnode+1
              enddo
              ihmax=heaplen
           enddo
           if(status.eq.msg_OK) then
             cost = cosReal(lnrabs)
             call BuildPath(xpmin,lnr, path, pathSize, status)
           endif
      end subroutine

      SUBROUTINE FindODPaths(k, i, j, cmin, pmin, status)
      real      cmin(*)
      integer   pmin(*)
      integer   status

      integer   ip

      integer   path(MXARC)
      integer   pathSize
      real      local_cmin(NLINKRUT)

       local_cmin = cmin(1:NLINKRUT)

       nps = 0
       status = msg_OK
       call BuildMinPath(i,j,local_cmin,pmin, path,pathSize,cost,status)
       if (status.eq.msg_OK) then
         call savePaso(path, pathSize, cost, status)
       endif
       if(status.ne.msg_OK) then
          RETURN
       endif


       do lnr=1, NLINKRUT
          lnrData(lnr)%iover  = 0
          lnrData(lnr)%cosOvr = lnrData(lnr)%cosTot
       enddo
       do ip=2, npas(k)  ! desde 2, ya tengo el minimo
         call overlap(path, pathSize)
         call FindPath(i, j, local_cmin, path, pathSize, cost, status)
         if (status.eq.msg_OK) then
           call savePaso(path, pathSize, cost,status)
           if (status.eq.msg_EndOfPathSearch) then
!             Repeated path means end of search
              status = msg_Ok
              return
           endif
         else
              if (status.eq.msg_NoPath) then
!                There's only the minimum path
                 status = msg_Ok
              endif
             return
         endif
       enddo

      END SUBROUTINE



      SUBROUTINE CPASMIN(J, cmin, pmin, status)
      real ::     cmin(*)
      integer ::  pmin(*)
      integer ::  status



      integer(4) :: ihmax = 0

      logical(1) :: marK(NLINKRUT)
      integer L, lnrCan, IRFTE, IOFTE, lrc
      double precision ::  CCAN, COSTOCAN, CTE, costo,MaxCostSeen, cm
      integer NHMAX, NH, lnrAbs, lnr,IRCAN
      integer(4) :: inode

      status = msg_OK
      MaxCostSeen=0
      NHMAX=0
      cmin(1:NLINKRUT) = RINF
      pmin(1:NLINKRUT) = 0
      call heapalloc(MXCON*NLINKRUT)
      inode=matz(j)
      if (inode.eq.0) then
         status=msg_NoPath !!! ojo, esto es un parche
         return
      endif
      do while(graph(inode)%linkrut /= 0)
         lnr=graph(inode)%linkrut ! Enlace-ruta que sale de centroide, nunca negativo
         cmin(lnr)=0
         call heappush(0.0D0, lnr)
         inode=inode+1
      enddo

      mark=.FALSE.
      do while(.not. heapempty()) ! o sea, el heap no esta vacio
        ihmax=heapmax()
        call heappop(costo, lnr)
        lnrAbs = abs(lnr)
        if(mark(lnrAbs))CYCLE
        mark(lnrAbs)=.TRUE.
        !if (debugging >= dbg_Debug) then
        !   call ReportPath(lnr, costo, MaxCostSeen, cmin, pmin)
        !endif
        if(MaxCostSeen /= 0 .and. (costo/MaxCostSeen) < 0.999991) then
           call ReportPath(lnr, costo, MaxCostSeen, cmin, pmin)
           status = msg_PasosInconsistente
           call heapcheck
           return
        endif
        MaxCostSeen = costo
        l    = ilink(lnrAbs)
        if(iozon(l).ne.0) CYCLE ! No hay pasos que atraviecen un centroide

        ! busqueda de candidatos
        irfte = iroute(lnrAbs)
        iofte = ioprut(irfte)
        ccan  = costo+lnrData(lnrAbs)%cosTot
        inode = lnrData(lnrAbs)%connected
        do while(graph(inode)%linkrut /= 0)
          lrc = graph(inode)%linkrut
          lnrCan = abs(lrc)
          if(.not. mark(lnrCan))then
             costocan = ccan +graph(inode)%cost 
             if (costocan.lt.cmin(lnrCan)) then
                cmin(lnrCan)=costocan
                pmin(lnrCan)=lnr
                call heappush(costocan, lrc)
                if (costocan < MaxCostSeen) then
                   print *, 'ERROR: CANDIDATO MAS BARATO'
                   print *, lnrData(lnrAbs)%cosTot
                   print *, CTMV(lnrAbs)
                   print *, graph(inode)%cost
                   print *, TMV(lnrAbs)
                   print *, TARIFA(lnrAbs)
                   io = IOPRUT(IROUTE(LnRAbs))
                   print *, COPMIN(io)
                   print *, TARCOP(io)
                   print *, CONSTM(io)
                   it = ITIP(ILINK(lnrAbs))
                   print *, PENLACE(IO,it)
                   k = MODOPER(io)
                   print *, VOTMV(k)


                   call ReportPath(lnrCan, costocan, MaxCostSeen, cmin, pmin)
                   status = msg_PasosInconsistente
                   return
                endif
             endif
         endif
         inode=inode+1
        enddo
        if(nhmax.lt.nh)nhmax=nh
      enddo
     END SUBROUTINE

      SUBROUTINE PathSearch(IPOL, status)
        integer , intent(in) :: IPOL
        integer, intent(out) :: status

        logical errors
        CHARACTER(3) TIP

  ! Calulo de COSLINK, TMV, ETC
        CALL LINKCOSTS()
  !  Construye matriz de arcos conectados
        errors = .FALSE.
        DO K=1,NTM
           IF(NPAS(K).EQ.0)CYCLE
           call PathInitMode(k)
           if (status.ne.msg_OK) RETURN

  !  Abre el archivo PKS correspondiente (no hay primer registro)
           TIP= 'P' // trim(IntegerToString(k)) // 'S'
           call newPolFile(3,IPOL,TIP,IO_BIN)
           call writePathHeader(3, K)

  ! Construye los pasos por el modo k para todos los centroides
  ! matriz de conectividad inversa
           call debug('About to call CalcConnectivity TRUE')
           call CalcConnectivity(k, .TRUE., status)
           errors = errors .or. status.ne.msg_OK

           if(.not.errors) then
              call getcmins(k)
           endif
           call debug('Returned from CalcConnectivity TRUE')

   ! matriz de conectividad al derecho
   ! aunque haya habido errores, asi se reportan todos de una vez
           call CalcConnectivity(k, .FALSE., status)
           errors = errors .or. status.ne.msg_OK

           if (.not.errors) then
                do  i=1,NZN
                  if(.not.ConSubZonas(i)) then
                       call PASMIN(K, I, 3, status)
                       errors = errors .or. status.ne.msg_OK
                   endif
                enddo
           endif
           write(3)0
           CLOSE(3)
           if(errors) then
              call Mensa(msg_ConectivityErrors, mensa_Aborta)
           endif

      ENDDO         ! Fin DO modo k
      END SUBROUTINE


      subroutine ReportPath(lnr, cost, previousHighCost, cmin, pmin)
          integer, intent(in) :: lnr
          double precision, intent(in)  :: cost, previousHighCost
          real, intent(in)    :: cmin(*)
          integer, intent(in) :: pmin(*)

          integer :: p, l, ir

          print *, ' PATH REPORT'
          print 7701, cost, previousHighCost
          print *

          p = lnr
          do while(p.ne.0)
              lnra = abs(p)
              l = ilink(lnra)
              ir = iroute(lnra)
              print 7702, l, ior(l), ides(l), NumRut(ir), cmin(lnra)
              p = pmin(lnra)
          enddo

          print *


7701      format('path cost:', F12.4, ' previous high cost:', F12.4)
7702      format('pos:', I6,' link: (', I6,'/',I6, ') route:', I6, ' cost:', F12.4)
      end subroutine

END PROGRAM PASOSNG

 subroutine usage
 USE GETOPTM
 character(32) prog
    prog = argv(0)

    print *
    print '(A,'' - TRANUS(r) Path Search Model'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> [options]'')', trim(prog)
    print *
    print *, 'If no command is given, the program enters interactive mode'

    print *
    print *, 'Options are:'
    call ExplainStdOptions

    STOP 2
 end subroutine


