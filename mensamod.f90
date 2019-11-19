! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2010 Modelistica, Caracas
! * Copyright (C) 1983-2010 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo A�ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
module MENSAMOD
USE DEBUGM
USE OPTIONSM

PUBLIC
   integer, parameter :: TEXTLEN = 256
   private :: msg
   type :: msg
        integer :: id
        character(TEXTLEN) :: text
   end type

   integer, parameter ::mensa_Normal=0
   integer, parameter ::mensa_Aborta=-1
   integer, parameter ::msg_Ok = 0
   integer, parameter ::msg_FileNotFound = 1
   integer, parameter ::msg_PoliticaNoDefinida = 2
   integer, parameter ::msg_PolicyRedefined = 90003
   integer, parameter ::msg_InvalidPolicyName = 90004
   integer, parameter ::msg_DesutAgNegativa = 1027
   integer, parameter ::msg_NoHayPasos      = 3013
   integer, parameter ::msg_ConectivityErrors = 3035
   integer, parameter ::msg_PasosInconsistente = 3040
   integer, parameter ::msg_HeapDamaged        = 3050
   integer, parameter ::msg_ReadError=10001
   integer, parameter ::msg_WriteError=10002
   integer, parameter ::msg_TooManyLinksInPath = 3012
   integer, parameter ::msg_ZonaNoConectada = 3015
   integer, parameter ::msg_DemasiadosEnlacesZona = 3016
   integer, parameter ::msg_DemasiadosEnlacesConectados   = 3009
   integer, parameter ::msg_NoHayEnlacesSalida  =3032
   integer, parameter ::msg_NoHayEnlacesEntrada =3034
   integer, parameter ::msg_TooManyAdyacencies=3030
   integer, parameter :: msg_IncorrectFileVersion=0017
   integer, parameter :: msg_TwoScenariosRequired=08206
   integer, parameter :: msg_IncorrectFileFormat =10003
   integer, parameter :: msg_ErrorReadingZones=10203
   integer, parameter :: msg_ErrorReadingCategories=10204
   integer, parameter :: msg_ErrorReadingModes=10205
   integer, parameter :: msg_ErrorReadingOperators=10206
   integer, parameter :: msg_ErrorReadingRoutes=10207
   integer, parameter :: msg_ErrorReadingAdmins=10208
   integer, parameter :: msg_ErrorReadingLinkTypes=10209
   integer, parameter :: msg_EndOfPathSearch=10110
   integer, parameter :: msg_NoPath = 3013
   integer, parameter :: msg_DesAgNegativa = 1027
   integer, parameter :: msg_NoPreviousPol = 10004
   integer, parameter :: msg_PathForCTL    = 10005
   integer, parameter :: msg_SectionStartExpected =10008
   integer, parameter :: msg_SpecificSectionExpected =10009
   integer, parameter :: msg_LogitError =10010
   integer, parameter :: msg_LogitCostLtZero =10011
   integer, parameter :: msg_LogitElastTooHigh =10012
   integer, parameter :: msg_LogitNegativeCost =10013
   integer, parameter :: msg_LogitNoOptions =10006
   integer, parameter :: msg_SpeedIsZero=10014
   integer, parameter :: msg_InvalidZoneFactor=10015
   integer, parameter :: msg_InvalidZoneNumber=10016

  
   integer, parameter :: MXMSG = 408
   type(msg) :: messages(MXMSG) = (/ &
        msg(-0001, '____________________________________________'), &
        msg(00000, 'Missing Error Message'), &
        msg(00001, 'ERROR G01: This file does not exist'), &
        msg(00002, 'ERROR G02: Year/policy not defined'), &
        msg(00003, 'ERROR G03: Problem reading this file'), &
        msg(90003, 'ERROR G04: Year/Policy redefined'), &
        msg(90004, 'ERROR G05: Year/Policy name must be 3 alphanumeric characters'), &
        msg(00004, 'Run time:'), &
        msg(00005, 'IDENTIFY YEAR AND POLICY (3 characters) ------->'), &
        msg(00006, 'ERROR G04: Illegal definition'), &
        msg(00007, 'Option --->'), &
        msg(00008, 'NORMAL END OF'), &
        msg(00009, 'ERROR G05: Max dimension of this variable exceeded'), &
        msg(00010, 'ERROR G06: Incomplete file'), &
        msg(00011, 'Specify an output file:                  '), &
        msg(00011, '[PATH]name.ext (up to 32 characters)     '), &
        msg(00012, 'File already exists - overwrite ?   (Y/n)'), &
        msg(00013, 'New or additional run ? (N/A) ------->'), &
        msg(00014, 'Option not valid, try again'), &
        msg(00015, 'ERROR G07: This section cannot be empty'), &
        msg(00016, 'Specify the type of output:              '), &
        msg(00016, 'N normal                               '), &
        msg(00016, 'T tab (for spreadsheets)               '), &
        msg(00017, 'ERROR G17: Incorrect File Version'), &
        msg(01001, 'ERROR L01: zones or categories in C1S do not match those in L1E'), &
        msg(01002, 'L O C : ACTIVITY LOCATION'), &
        msg(01003, 'L C A L : ACTIVITY LOCATION CALIBRATION'), &
        msg(01004, 'READING PARAMETERS AND DATA'), &
        msg(01005, 'INCREMENT OF EXOGENOUS VARIABLES'), &
        msg(01006, 'ALLOCATION OF PRODUCTION'), &
        msg(01007, 'I M P L O C : DISPLAY LOC RESULTS'), &
        msg(01008, 'ERROR L02:  Negative attractor in this zone and sector'), &
        msg(01009, 'Convergence indicators'), &
        msg(01009, 'Sector     ConvPric   Zon   ConvProd   Zon   ExogProd   InducProd'), &
        msg(01010, 'RESULTS OF THE ACTIVITY LOCATION MODEL'), &
        msg(01011, 'Zone   TotProd   TotDem    ProdCost   Price     Supply    Stock Unstock  Adjust'), &
        msg(01012, 'Zone       TotProd         TotDem          ProdCost   Price         Supply          Stock Unstock Adjust'), &
        msg(01013, 'Objective  Prices  Zone Sect   Product Zone Sect'), &
        msg(01015, 'WARNING L07:  Last iteration without convergence'), &
        msg(01016, 'ERROR L03:  Increment in desaggregated zone, put in subzones'), &
        msg(01017, 'Zone   TotProd   TotDem    ProdCost   Price     MinRes    MaxRes    Adjust    ConCost   UtiCon     Atrac'), &
        msg(01018, 'Zone       TotProd         TotDem          ProdCost   Price         MinRes          MaxRes  &
                    Adjust    ConCost    UtiCon     Atrac'), &      
        msg(01019, 'ERROR L08:  Utility functions have become zero'), &
        msg(01020, 'ERROR L09:  Total minimum demand exceeds total production'), &
        msg(01021, 'ERROR L10:  Total maximum demand less that total production'), &
        msg(01022, 'ERROR L11:  Minimum demand exceeds supply (+substitutes) in this zone'), &
        msg(01023, 'ERROR L12:  Maximum demand (+substitutes) is less than supply in this zone'), &
        msg(01027, 'WARNING G08:  Aggregated utility has become negative'), &
        msg(01028, 'Production in this zone is not equal to its subzones'), &
        msg(01029, 'Aggregated utility has turned negative in this category'), &
        msg(01030, 'ERROR L04:  Minimum restriction is now greater than the maximum'), &
        msg(01031, 'ERROR: Distribution parameter is not zero but sector is not consumed'), &
        msg(01033, '_________________________________________________'), &
        msg(01033, 'Display options:                              '), &
        msg(01033, ''), &
        msg(01033, '[1] All information by sector and zone      '), &
        msg(01033, '[2] Total production by sector and zone     '), &
        msg(01033, '[3] Total production by year/policy         '), &
        msg(01033, '[4] Internal information by sector and zone '), &
        msg(01033, '[5] Consumption coeficients by sector       '), &
        msg(01033, '[6] Total consumption by sector and zone    '), &
        msg(01033, '[7] All information, comma-delimited        '), &
        msg(01033, '_________________________________________________'), &
        msg(01034, 'List of year/pol separated by blank space ending with /'), &
        msg(01035, 'List of sectors separated by blank space (0 for all) ending with /'), &
        msg(02001, 'F L U J : TRANSFORMATION OF FLOWS'), &
        msg(02003, 'ERROR F02:  File F1E has not been defined'), &
        msg(02005, 'ERROR F03:  No conexion from flows to transport categories'), &
        msg(02007, 'ERROR F05:  This sector does not generate flows'), &
        msg(02011, 'ERROR F09:  This sector generates flows but not trips'), &
        msg(03001, 'P A S O S :  PATH SEARCH BY MODE'), &
        msg(03005, 'ERROR P01:  Link with zero capacity'), &
        msg(03006, 'ERROR P02:  P1E contains changes without a previous network'), &
        msg(03007, 'Total number of links in the network:'), &
        msg(03009, 'ERROR P03:  Too many links connected to this link'), &
        msg(03010, 'ERROR'), &
        msg(03011, 'PATH SEARCH BY MODE'), &
        msg(03012, 'ERROR P05:  Too many links in a path'), &
        msg(03013, 'ERROR P06:  No path from this orig to this dest by this mode'), &
        msg(03014, 'ERROR P07:  Progam capacity exceeded in this mode'), &
        msg(03015, 'ERROR P08:  This zone is not connected by this mode'), &
        msg(03016, 'ERROR P09:  This zone has too many connected links'), &
        msg(03017, 'ERROR P10:  No operator can use this type of link'), &
        msg(03018, 'ERROR P11:  Too many link*operators in the system'), &
        msg(03019, 'ERROR P12:  Current and previous network are inconsistent'), &
        msg(03030, 'ERROR P30:  Too many connections in link adyacency graph'), &
        msg(03032, 'ERROR P04:  There is no out connection from this link/route'), &
        msg(03034, 'ERROR P05:  There is no in connection to this link/route'), &
        msg(03035, 'ERROR P13:  Connectivity errors in the network'), &
        msg(03040, 'ERROR P15:  Network is inconsistent'), &
        msg(03045, 'ERROR P16:  Empty Heap'), &
        msg(03050, 'ERROR P17:  Heap Damaged'), &
        msg(03055, 'ERROR P18:  Heap Full'), &
        msg(03060, 'ERROR P19:  Heap Inconsistent'), &
        msg(03070, 'Max number of nodes exceeded'), &
        msg(03071, 'Too many nodes'), &
        msg(04000, 'T R A N S : TRANSPORT MODEL'), &
        msg(04001, 'ERROR T05:  Min frequency of routes must be >= zero'), &
        msg(04003, 'ERROR T06:  Time factor for this operator must be > zero'), &
        msg(04004, 'ERROR T01:  Occupancy rates must be > zero'), &
        msg(04008, 'ERROR T03:  No category can use this mode'), &
        msg(04006, 'ERROR T04:  Categories in P0E and F1S do not coincide'), &
        msg(04007, 'ERROR T02:  Consolidation parameter must be between 0 and 1 or negative (-1)'), &
        msg(04009, 'EVALUATION INDICATORS'), &
        msg(04009, ''), &
        msg(04009, 'TOTAL TRIPS BY CATEGORY AND MODE'), &
        msg(04009, 'Category        Modes....'), &
        msg(04010, 'SUPPRESSED DEMAND BY CATEGORY AND MODE'), &
        msg(04010, 'Category        Trips       %'), &
        msg(04011, 'STATISTICS BY TRANSPORT CATEGORY'), &
        msg(04011, 'Category    TotDist   TotCost   TotTravTime TotWaitTime    TotDesut'), &
        msg(04012, 'STATISTICS BY TRANSPORT OPERATOR'), &
        msg(04012, 'Operator    Trips    Units-Dis    Energy      Costs    Income     Reven'), &
        msg(0, '412  STATISTICS BY TRANSPORT OPERATOR'), &
        msg(0, '412  Operator Trips   Units-Dis       Energy  Costs   Income          Reven'), &
        msg(04013, 'STATISTICS BY TRANSPORT ADMINISTRATOR'), &
        msg(04013, 'Administr  Length     Income       Costs     Reven'), &
        msg(0, '413  STATISTICS BY TRANSPORT ADMINISTRATOR'), &
        msg(0, '413  Administr        Length          Income          Costs   Reven'), &
        msg(04014, 'ERROR T07:  Modal split parameter must be > zero'), &
        msg(04015, 'ERROR T08:  Assignment parameter must be > zero'), &
        msg(04016, 'Iter Categ Origin ConvObj    ConvFlows        Worst  ConvSpeed          Worst'), &
        msg(04017, 'ConvObj  ConvFlows               ConvSpeed'), &
        msg(04018, 'AVERAGE STATISTICS BY TRANSPORT CATEGORY'), &
        msg(04018, 'Category    AvrgDist   AvrgCost  AvrTravTime AvrWaiTime    AvrgDisut'), &
        msg(0, '418  AVERAGE STATISTICS BE TRANSPORT CATEGORY'), &
        msg(0, '418  Category         AvtgDist        AvrgCost        AvrTravTime     AvrWaiTime      AvrgDisut'), &
        msg(04019, 'Oversaturated link'), &
        msg(05000, 'C A L C O : PRECALIBRACION DE TRANSPORTE'), &
        msg(05002, 'GRABACION FINAL DESUTILIDADES'), &
        msg(05004, 'PARAMETROS RESULTANTES DE CALCO'), &
        msg(05005, 'PARAMETROS SEPARACION MODAL POR CATEGORIA'), &
        msg(05006, 'PARAMETROS ASIGNACION POR MODO'), &
        msg(06000, 'C O S T : AJUSTMENT OF COSTS'), &
        msg(08000, 'I M P A S : PATH DISPLAY PROGRAM'), &
        msg(08001, 'Options to input data:                   '), &
        msg(08001, ''), &
        msg(08001, '[0] Manually, on screen                 '), &
        msg(08001, '[1] Read data from file IMPAS.DAT       '), &
        msg(08002, 'File IMPAS.DAT does not exist'), &
        msg(08003, 'Link not found'), &
        msg(08004, 'Area  Policy        Date/time simulation'), &
        msg(08005, 'PATHS BY MODE'), &
        msg(08005, 'Mode      Date/time simulation'), &
        msg(08006, 'PATHS THAT USE A LINK BY MODE'), &
        msg(08006, 'Mode          Link        Date/time simulation'), &
        msg(08008, 'Options to display paths:                '), &
        msg(08008, ''), &
        msg(08008, '[0] Some paths                        '), &
        msg(08008, '[1] All paths                         '), &
        msg(08008, '[2] Paths that use a link             '), &
        msg(08008, '[3] List of links in the network      '), &
        msg(08009, 'List of modes to display separated       '), &
        msg(08009, 'by blanks (/to finish):                  '), &
        msg(08009, ''), &
        msg(08009, '[0]  All modes                         '), &
        msg(08010, 'List of origins and destinations         '), &
        msg(08010, 'to display                               '), &
        msg(08011, 'Origin (0 to finish) ------->'), &
        msg(08012, 'List of destinations (/ finish 0=all)'), &
        msg(08013, 'List of origin and destination nodes     '), &
        msg(08014, 'Orig    Dest    Mode        Path    Dist  LnkTme  WaitTme MonCos  Chargs  UChrgs GenCost'), &
        msg(08015, 'Orig    Dest    Typ  Dist   Capac RefSpd  Oper/rout... -ProhTurns...'), &
        msg(08030, 'M A T S :  PROGRAM TO DISPLAY MATRICES'), &
        msg(08031, 'Options to input data:                   '), &
        msg(08031, ''), &
        msg(08031, '[0] Manually on-screen                  '), &
        msg(08031, '[1] Read data from file MATS.DAT        '), &
        msg(08032, 'File MATS.DAT does not exist'), &
        msg(08033, 'Options to display matrices:                  '), &
        msg(08033, ''), &
        msg(08033, '[1] Disut. by transport category            '), &
        msg(08033, '[2] Disut. by mode and transport category   '), &
        msg(08033, '[3] Disut. by socio-economic sector         '), &
        msg(08033, '[4] Trips by mode                           '), &
        msg(08033, '[5] Trips by transport category             '), &
        msg(08033, '[6] Trips by mode and transport category    '), &
        msg(08033, '[7] Total trips (sum of categories)         '), &
        msg(08033, '[8] Frequency distribution of trips by mode '), &
        msg(08033, '[9] Flows by socio-economic sector          '), &
        msg(08033, '[10] Flows by transport category             '), &
        msg(08033, '[11] Costs by transport category             '), &
        msg(08033, '[12] Costs by socio-economic sector          '), &
        msg(08033, '[13] Exogenous trips by transport category    '), &
        msg(08033, '[14] Exogenous trips by category and mode     '), &
        msg(08034, 'Intervals in the scale of costs ? :'), &
        msg(08035, 'DISUTILITIES BY TRANSPORT CATEGORY'), &
        msg(08035, 'Category       Iter    Area  Pol  Date/time simulation'), &
        msg(08036, 'DISUTILITIES BY MODE AND TRANSPORT CATEGORY'), &
        msg(08036, 'Mode      Category    Iter   Area  Pol  Date/time simulation'), &
        msg(08038, 'TRIPS BY TRANSPORT MODE'), &
        msg(08038, 'Mode      Area  Pol  Date/time simulation'), &
        msg(08039, 'TRIPS BY TRANSPORT CATEGORY'), &
        msg(08039, 'Category    Iter  Area  Pol  Date/time simulation'), &
        msg(08040, 'TRIPS BY MODE AND TRANSPORT CATEGORY'), &
        msg(08040, 'Mode      Category    Iter   Area  Pol  Date/time simulation'), &
        msg(08041, 'TOTAL TRIPS'), &
        msg(08041, 'Iteration  Area  Pol  Date/time simulation  TRANUS Version'), &
        msg(08043, 'MONETARY COSTS BY TRANSPORT CATEGORY'), &
        msg(08043, 'Category   Iter  Area  Pol  Date/time simulation'), &
        msg(08044, 'DISUTILITIES BY SOCIO-ECONOMIC SECTOR'), &
        msg(08044, 'Sector      Area  Pol  Date/time simulation'), &
        msg(08045, 'MONETARY COSTS BY SOCIO-ECONOMIC SECTOR'), &
        msg(08045, 'Sector      Area  Pol  Date/time simulation'), &
        msg(08046, 'FLOWS BY SOCIO-ECONOMIC SECTOR'), &
        msg(08046, 'Sector      Area  Pol  Date/time simulation'), &
        msg(08047, 'FLOWS BY TRANSPORT CATEGORY'), &
        msg(08047, 'Category     Area  Pol  Date/time simulation'), &
        msg(08048, 'FREQUENCY DISTRIBUTION OF TRIPS BY MODE'), &
        msg(08048, 'Mode        Area  Pol    Date/time simulation'), &
        msg(08049, 'EXOGENOUS TRIPS BY TRANSPORT CATEGORY'), &
        msg(08049, 'Category     Area  Pol  Date/time simulation'), &
        msg(08050, 'EXOGENOUS TRIPS BY MODE AND TRANSPORT CATEGORY'), &
        msg(08050, 'Category   Mode     Area  Pol  Date/time simulation'), &
        msg(08051, 'FREQUENCY DISTRIBUTION OF TRIPS BY CATEGORY'), &
        msg(08051, 'Category       Iter    Area  Pol  Date/time simulation'), &
        msg(08060, 'I M P T R A : DISPLAY TRANSPORT RESULTS'), &
        msg(08061, 'Options to input data:                   '), &
        msg(08061, ''), &
        msg(08061, '[0] Manually on-screen                  '), &
        msg(08061, '[1] Read from file IMPTRA.DAT           '), &
        msg(08062, 'Iteration   Area  Pol  Date/time simulation TRANUS Version'), &
        msg(08063, 'Number of links printed:'), &
        msg(08064, 'Options to display assignment results:   '), &
        msg(08064, ''), &
        msg(08064, '(1) All links                          '), &
        msg(08064, '(2) By link type                       '), &
        msg(08064, '(3) By Demand/Capacity range           '), &
        msg(08064, '(4) Specified on-screen                '), &
        msg(08064, '(5) Table of indicators                '), &
        msg(08064, '(6) Cordons (only with IMPTRA.DAT)     '), &
        msg(08064, '(7) Transit Routes profiles            '), &
        msg(08064, '(9) Link-Route & Category profile      '), &
        msg(08064, '(10) Route profile, comma-delimited     '), &
        msg(08064, '(11) Indicators, comma-delimited     '), &
        msg(08064, ''), &
        msg(08064, 'List of options ending with /          '), &
        msg(08065, 'List of link types (/to finish)'), &
        msg(08066, 'Specify demand/capacity range'), &
        msg(08067, 'Specify links to display (Origin=0 to finish)'), &
        msg(08068, 'Output format options:                   '), &
        msg(08068, ''), &
        msg(08068, '(1) Minimum                            '), &
        msg(08068, '(2) Medium                             '), &
        msg(08068, '(3) Maximum                            '), &
        msg(08069, 'IMPTRA.DAT(1): Too many links'), &
        msg(08070, 'RESULTS IN CORDONS'), &
        msg(08070, 'Iteration   Area  Pol  Date/time simulation TRANUS Version'), &
        msg(08071, 'Cordon    Direction'), &
        msg(08072, 'TRANSIT ROUTES PROFILES'), &
        msg(08072, 'Iteration   Area  Pol  Date/time simulation TRANUS Version'), &
        msg(08073, 'List of routes ending with / (0 for all)'), &
        msg(08079, 'D I M E N :  PRESENTA LAS DIMENSIONES MAXIMAS'), &
        msg(08079, ''), &
        msg(08079, 'Esta es la versi¢n 4.0 del sistema TRANUS.'), &
        msg(08079, 'Todos los programas de TRANUS est n sujetos a ciertas'), &
        msg(08079, 'dimensiones m ximas.  En las pantallas a continuaci¢n'), &
        msg(08079, 'se presentan los valores m ximos de las variables.'), &
        msg(08079, 'Una explicaci¢n m s detallada de cada valor se encuentra'), &
        msg(08079, 'en el Manual de Referencia del Sistema TRANUS.'), &
        msg(08080, 'MAXIMUM DIMENSION OF TRANUS VARIABLES'), &
        msg(08080, ''), &
        msg(08080, 'LOCALIZATION'), &
        msg(08080, ''), &
        msg(08080, 'Zones   Sectors    Substitutes'), &
        msg(08081, 'TRANSPORT'), &
        msg(08081, ''), &
        msg(08081, 'Modes  Categs  Opers  Routes  Adms  LinkTip  Paths'), &
        msg(08082, 'Links Link-Routes TurnProhibitions LiksInAPath'), &
        msg(08084, 'EVALUACION'), &
        msg(08084, ''), &
        msg(08084, 'Escenarios'), &
        msg(08085, 'Or   Des     Cap      Dem   Dem/Cap   Veloc    Board   Down    Seats   Espera'), &
        msg(08100, 'G T D A T :  GRAPHICS INTERFACE'), &
        msg(08101, 'Network file:'), &
        msg(08102, 'WARNING GT02:  This node is not in the network'), &
        msg(08103, 'ERROR GT01: This node does not have coordinates'), &
        msg(08150, 'T D A T : VERIFY TRANSPORT DATA'), &
        msg(08151, 'Do you want to generate output file P0S(y/n)? '), &
        msg(08152, 'READING AND VERIFYING NETWORK DATA...'), &
        msg(08153, 'VERIFYING CONSISTENCY OF PROHIBITED TURNS...'), &
        msg(08154, 'Turn prohibition to an unconnected node'), &
        msg(08155, 'TESTING FOR DUPLICATE LINKS...'), &
        msg(08156, 'This is a repeated link'), &
        msg(08157, 'Do you want to verify symmetry (y/n)          '), &
        msg(08158, 'Asymmetric distances in these links'), &
        msg(08159, 'Asymmetric capacities in these links'), &
        msg(08160, 'This link has no reverse'), &
        msg(08161, 'VERIFYING SYMMETRY OF PROHIBITED TURNS...'), &
        msg(08162, 'This prohibited turn has no equivalent'), &
        msg(08163, 'VERIFYING SYMMETRY OF LINKS...'), &
        msg(08164, 'This transit route is repeated in this link'), &
        msg(08165, 'READING AND VERIFYING PARAMETERS...'), &
        msg(08166, 'The number of paths in this mode is negative or > maximum'), &
        msg(08167, 'Dispertion for this modes must be > 1.0'), &
        msg(08168, 'Car availablity for this category must be between 0. and 1.'), &
        msg(08169, 'Overlapping parameter must be > 0'), &
        msg(08200, 'E V A L - EVALUATION PROCEDURE'), &
        msg(08201, 'IDENTIFY BASE AND ALTERNATIVE CASES (1 character)'), &
        msg(08202, 'Area  BaseCase Alternative  Date/time evaluation'), &
        msg(08203, 'TRANSPORT USERS''S SURPLUS BY ZONE AND CATEGORY'), &
        msg(08203, '(In transport time units)'), &
        msg(08204, 'EVALUATION SUMMARY TABLE - CONSTANT VALUES'), &
        msg(08204, ''), &
        msg(08204, 'Year  CaptlCosts   UserBenf   OperBalnc   AdmBalanc   ShadowEner'), &
        msg(08205, 'EVALUATION SUMMARY TABLE - DISCOUNTED VALUES'), &
        msg(08205, ''), &
        msg(08205, 'Year  CaptlCosts   UserBenf   OperBalnc   AdmBalanc   ShadowEner'), &
        msg(08206, 'Two scenarios required for evaluation.'), &
        msg(08300, 'Options to display matrices              '), &
        msg(08300, ''), &
        msg(08300, '[1] Distances                         '), &
        msg(08300, '[2] Travel times                      '), &
        msg(08300, '[3] Waiting times                     '), &
        msg(08300, '[4] Monetary costs                    '), &
        msg(08300, '[5] Generalized costs                 '), &
        msg(08301, 'D T C - DISTANCE, TIME AND COST MATRICES'), &
        msg(08302, 'Specify the path number to display       '), &
        msg(08303, 'DISTANCE MATRICES'), &
        msg(08304, 'TRAVEL TIME MATRICES'), &
        msg(08305, 'WAITING TIME MATRICES'), &
        msg(08306, 'MONEY COSTS MATRICES'), &
        msg(08307, 'GENERALIZED COSTS MATRICES'), &
        msg(08308, 'SYMMETRY CHECK (in % differences)'), &
        msg(08309, 'There are additional paths with the same cost'), &
        msg(08310, '(TM) MODELISTICA  v4.6               1995'), &
        msg(08317, 'TRANSFERS BY OPERATOR MATRIX'), &
        msg(08400, 'M A T E S P - SPECIAL TRANSPORT MATRICES'), &
        msg(08401, 'Options to display matrices:                  '), &
        msg(08401, ''), &
        msg(08401, '[1] Trips by operator                       '), &
        msg(08401, '[2] Trips that use one or more links        '), &
        msg(08401, '[3] Matrix of transfers                      '), &
        msg(08401, '[4] Matrix of Distance                      '), &
        msg(08401, '[5] Matrix of Time                          '), &
        msg(08401, '[6] Matrix of Costs                         '), &
        msg(08401, '[7] Matrix of Transfers Between Operators   '), &
        msg(08401, '[8] Summary by category and operator in CSV format'), &
        msg(08402, 'List of operators ending with /'), &
        msg(08403, 'List of categories ending with /'), &
        msg(08404, 'Choose a mode'), &
        msg(08405, 'Minium number of transfers'), &
        msg(08406, 'Turning Movements '), &
        msg(08407, 'Select a category'), &
        msg(08408, 'Identify a link                         (Origin  Destination)'), &
        msg(08409, 'List of nodes ending with /'), &
        msg(08410, 'TRIP MATRICES BY OPERATOR'), &
        msg(08411, 'TRIPS MATRICES THAT USE ONE OR MORE LINKS'), &
        msg(08412, 'MATRIX OF TRANSFERS'), &
        msg(08416, 'Iter  Categ   Origin'), &
        msg(08417, 'Categ'), &
        msg(10000, 'Status messages'), &
        msg(10001, 'Read Error'), &
        msg(10002, 'Write Error'), &
        msg(10003, 'Incorrect File Format'), &
        msg(10004, 'No previous scenario'), &
        msg(10005, 'Path for TRANUS.CTL? ------------------------->'), &
        msg(10006, 'Logit : No options'), &
        msg(10007, 'Logit : Elasticity less than zero'), &
        msg(10008, 'Section start expected'), &
        msg(10009, 'Wrong section found'), &
        msg(10010, 'Logit Error'), &
        msg(10011, 'Generalized cost less than zero'), &
        msg(10012, 'Numerical error in logit:'), &
        msg(10012, 'Exp(-param * cost) became zero for all options.'), &
        msg(10012, 'Elasticities are too high, or scaling is too low.'), &
        msg(10012, 'A scaling of 1 (one) avoids this problem.'), &
        msg(10013, 'Numerical error in logit. Generalized cost less than zero.'), &
        msg(10014, 'Speed is zero'), &
        msg(10015, 'Target occupancy rate must be between 0 and 1.'), &
        msg(10016, 'Zone not found.'), &
        msg(10017, 'Internal cost factors must be between 0 and 1.'), &
        msg(10203, 'Error reading Zones section'), &
        msg(10209, 'Error reeading Link Types'), &
        msg(18080, ''), &
        msg(18080, 'MAXIMUM DIMENSION OF TRANUS MODEL VARIABLES'), &
        msg(18080, ''), &
        msg(18080, 'Max  number of'), &
        msg(18080, '-------  ----------------------------------------------------'), &
        msg(18081, 'Zones'), &
        msg(18082, 'Sectors'), &
        msg(18083, 'Sector substitutes'), &
        msg(18084, 'Modes'), &
        msg(18085, 'Categories'), &
        msg(18086, 'Operators'), &
        msg(18087, 'Routes'), &
        msg(18088, 'Administrators'), &
        msg(18089, 'Link Types'), &
        msg(18090, 'Links'), &
        msg(18091, 'Link-Routes'), &
        msg(18092, 'Turn restrictions'), &
        msg(18093, 'Paths'), &
        msg(18094, 'Links in a path'), &
        msg(18095, 'Operators per Link'), &
        msg(18100, 'Scenarios in evaluation'), &
        msg(18101, 'Connections per link'), &
        msg(18103, 'Nodes'), &
        msg(19000, 'Found a power function with constant equal to zero. '), &
        msg(19000, 'Result is undetermined.'), &
        msg(19001, 'A parameter equal to zero has no effect.'), &
        msg(19002, 'Numerical overflow while evaluating exponential (logit) increment.'), &
        msg(19002, 'Please check your attractor parameters.'), &
        msg(19003, 'Computed attractor is negative.'), &
        msg(19003, 'Please check function parameters.'), &
        msg(28049, 'RETURN TRIPS BY TRANSPORT CATEGORY'), &
        msg(28049, 'Category     Area  Pol  Date/time simulation'), &
        msg(29999, 'LAST MESSAGE****************************************************'), &
        msg(0, '')  &
    /)

CONTAINS

      SUBROUTINE MENSA(N,M)
!     ================
       integer N,M
       Logical FExists

!  ESCRIBE LOS MENSAJES N CON LA OPCION M
!  m<0   error: escribe el mensaje y stop
!  m=0   mensaje de advertencia o mera informaci�n (regresa y contin�a)
!  m=1   pregunta interactiva:  regresa dejando el cursor al final
!  m>1   escribe mensaje en la unidad m y regresa

      character(256) :: TEXTO
      integer :: L
      integer :: i

      L = -1
      do i = 1, MXMSG
          if (messages(i)%id.eq.N) then
              L = i
              exit
          endif
      enddo  

      do while(l.le.MXMSG.and.messages(l)%id.eq.n)
         TEXTO = messages(l)%text
         IF(M.LT.0) THEN
             WRITE(*,*)
             WRITE(*,100)N,TRIM(TEXTO)
100          FORMAT(' ERROR#',I5,': ',A,' ')
         ENDIF 
         IF(M.EQ.0)WRITE(*,102)TRIM(TEXTO)
102      FORMAT(' ',A)
         IF(M.EQ.1)WRITE(*,'('' '',A,'' ''$)')TRIM(TEXTO)
         IF(M.GE.2)WRITE(M,102)TRIM(TEXTO)
         L = L + 1
      enddo
      IF(M.LT.0)STOP 1
200   RETURN
      END SUBROUTINE

      SUBROUTINE LOGMSG(N)
         CALL MENSA(N, 0)
      END SUBROUTINE

SUBROUTINE MENSAS(Msg,Kind)
!     ================
   character*(*), intent(in) :: Msg
   integer,intent(in),optional :: Kind
   integer  M
      if ( present(Kind) ) then
        M = Kind
      else
        M = 0
      endif
               

500   FORMAT(I5,2X,A)
      IF(M.LT.0) THEN
          WRITE(*,*)
          WRITE(*,100)N,TRIM(Msg)
          IF(M.LT.0)STOP 1
100       FORMAT(' ERROR#',I5,': ',A,' ')
      ENDIF 
      IF(M.EQ.0)WRITE(*,102)TRIM(Msg)
102   FORMAT(' ',A)
      IF(M.EQ.1)WRITE(*,'('' '',A,'' ''$)')TRIM(Msg)
      IF(M.GE.2)WRITE(M,102)TRIM(Msg)
200   RETURN

END SUBROUTINE

    function fimd_msg_pos(id)
        implicit none
        integer :: fimd_msg_pos
        integer :: id
        integer i

        fimd_msg_pos = -1
        do i = 1, MXMSG
            if (messages(i)%id.eq.id) then
                fimd_msg_pos = -1
                exit
            endif
        enddo
    end function

    function msgtext(id)
        implicit none
        character*(TEXTLEN) :: msgtext
        integer, intent(in) :: id
        integer :: pos

        pos = fimd_msg_pos(id)
        if (pos >= 1) then
            msgtext = messages(pos)%text
        else
            msgtext = ''
        endif
    end function

end module MENSAMOD
