!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,GRAV,ROEAU,PRIVE,ATMFILEA,ATMFILEB,FILES,LISTIN,
     & PATMOS_VALUE,AWATER_QUALITY,PLUIE,AOPTWIND,AWIND_SPD)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!+               (IN GENERAL FROM INPUT DATA FILES).
!
!warning  CAN BE ADAPTED BY USER
!
!history  J-M HERVOUET (LNHE)
!+        02/01/2004
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/01/2013
!+        V6P3
!+   Now 2 options with an example for reading a file. Extra arguments.
!
!history  C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Reading a file of meteo data for exchange with atmosphere
!+   Only the wind is used here
!
!history R.ATA (LNHE)
!+        09/11/2014
!+        V7P0
!+  introducion of water quality option + pluie is introduced as
!+   an optional parameter + remove of my_option which is replaced
!+   by a new keyword + value of patmos managed also with a new keyword
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        07/01/2015
!+        V7P0
!+  Adding optional arguments to remove USE DECLARATIONS_TELEMAC2D.
!
!history R.ATA (LNHE)
!+        16/11/2015
!+        V7P0
!+  Adding USE WAQTEL...
!
!history A. LEROY (LNHE)
!+        25/11/2015
!+        V7P1
!+  INTERPMETEO now writes directly in variables of WAQTEL which
!+  can be used by the other modules. This makes it possible to
!+  remove subsequent calls to INTERPMETEO in TELEMAC3D
!
!history J.-M. HERVOUET (RETIRED)
!+        01/07/2017
!+        V7P2
!+  Setting of UL moved outside the test IF(LT.EQ.0)... After a post by
!+  Qilong Bi (thanks Qilong...).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| ATMFILEA       |-->| LOGICAL UNIT OF THE ASCII ATMOSPHERIC FILE
!| ATMFILEB       |-->| LOGICAL UNIT OF THE BINARY ATMOSPHERIC FILE
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FUAIR          |<->| VELOCITY OF WIND ALONG X, IF CONSTANT
!| FVAIR          |<->| VELOCITY OF WIND ALONG Y, IF CONSTANT
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| DEPTH
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| LT             |-->| ITERATION NUMBER
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PATMOS         |<--| ATMOSPHERIC PRESSURE
!| PATMOS_VALUE   |-->| VALUE OF ATMOSPHERIC PRESSURE IS CONSTANT
!| PRIVE          |-->| USER WORKING ARRAYS (BIEF_OBJ BLOCK)
!| ROEAU          |-->| WATER DENSITY
!| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
!| WINDX          |<--| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |<--| SECOND COMPONENT OF WIND VELOCITY
!| X              |-->| ABSCISSAE OF POINTS
!| Y              |-->| ORDINATES OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: PVAP,RAY3,NWIND,NEBU,TAIR,
     &                              TAIR_VALUE,HREL,RAINFALL,
     &                              EVAPORATION,ATMOSEXCH
      USE DECLARATIONS_TELEMAC2D, ONLY : AT1_METEO,AT2_METEO,
     &                                   FUAIR1_METEO,FUAIR2_METEO,
     &                                   FVAIR1_METEO,FVAIR2_METEO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LT,NPOIN,ATMFILEA,ATMFILEB
      LOGICAL, INTENT(IN)             :: ATMOS,VENT,LISTIN
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(*)
      DOUBLE PRECISION, INTENT(IN)    :: AT,GRAV,ROEAU,PATMOS_VALUE
      DOUBLE PRECISION, INTENT(INOUT) :: FUAIR,FVAIR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!     OPTIONAL
      LOGICAL, INTENT(IN)          ,OPTIONAL :: AWATER_QUALITY
      TYPE(BIEF_OBJ), INTENT(INOUT),OPTIONAL :: PLUIE
      INTEGER, INTENT(IN)          ,OPTIONAL :: AOPTWIND
      DOUBLE PRECISION, INTENT(IN) ,OPTIONAL :: AWIND_SPD(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL WATER_QUALITY
      INTEGER UL,OPTWIND
      DOUBLE PRECISION COEF
      DOUBLE PRECISION UAIR,VAIR,WIND_SPD(2)
!     EXCHANGE WITH ATMOSPHERE
      DOUBLE PRECISION PATM,WW,TA
	  
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_	  
!	  open(unit=50,file="../wind_myfile_01.txt",access = 'append')
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_	
!
!
!-----------------------------------------------------------------------
!
!     DEFAULT VALUES OF PARAMETERS WHEN THEY ARE NOT GIVEN
!
      WATER_QUALITY=.FALSE.
      IF(PRESENT(AWATER_QUALITY)) WATER_QUALITY=AWATER_QUALITY
      OPTWIND=1
      IF(PRESENT(AOPTWIND)) OPTWIND=AOPTWIND
      WIND_SPD(1)=0.D0
      WIND_SPD(2)=0.D0
      IF(PRESENT(AWIND_SPD)) THEN
        WIND_SPD(1)=AWIND_SPD(1)
        WIND_SPD(2)=AWIND_SPD(2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      UL = FILES(ATMFILEA)%LU

!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!			  write(50,*)'LT: ', LT
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_

!
!
!     AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
!       ATMOSPHERIC PRESSURE AND AIR TEMPERATURE
!
        IF(ATMOS.OR.WATER_QUALITY) THEN
          CALL OV('X=C     ', X=PATMOS, C=PATMOS_VALUE, DIM1=NPOIN)
        ENDIF
        IF(WATER_QUALITY) THEN
          CALL OV('X=C     ', X=TAIR%R, C=TAIR_VALUE, DIM1=NPOIN)
        ENDIF
!
!       WIND :
!
        IF(VENT.OR.WATER_QUALITY) THEN
          IF(OPTWIND.EQ.1)THEN
!           IN THIS CASE THE WIND IS CONSTANT, VALUE GIVEN IN STEERING FILE.
            CALL OV( 'X=C     ' , X=WINDX, C=FUAIR, DIM1=NPOIN)
            CALL OV( 'X=C     ' , X=WINDY, C=FVAIR, DIM1=NPOIN)
          ELSEIF(OPTWIND.EQ.2) THEN
            IF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
!             JUMPING TWO LINES OF COMMENTS
              READ(UL,*)
              READ(UL,*)
!             READING THE FIRST TWO LINES OF DATA
              READ(UL,*) AT1_METEO,FUAIR1_METEO,FVAIR1_METEO
              IF(AT.LT.AT1_METEO) THEN
                WRITE(LU,*) ' '
                WRITE(LU,*) 'METEO'
                WRITE(LU,*) 'LATE BEGINNING OF THE WIND FILE'
                CALL PLANTE(1)
                STOP
              ENDIF
!             FIRST TIME, THE VARIABLES 2 TO BE INITIALISED
              AT2_METEO = AT1_METEO
              FUAIR2_METEO = FUAIR1_METEO
              FVAIR2_METEO = FVAIR1_METEO
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR THE REMAINING TIME STEPS
!
      IF(VENT.OR.WATER_QUALITY) THEN
!
!       WATER QUALITY
!
        IF(FILES(ATMFILEA)%NAME(1:1).NE.' ')THEN

          IF(WATER_QUALITY) THEN
!         TIME VARYING WATER QUALITY
            IF(ATMOSEXCH.EQ.0)THEN
              CALL INTERPMETEO2(NWIND,UAIR,VAIR,TA,PATM,NEBU,RAINFALL,
     &                          PVAP,RAY3,AT,UL)
!
              CALL OV('X=C     ', X=WINDX, C=UAIR, DIM1=NPOIN)
              CALL OV('X=C     ', X=WINDY, C=VAIR, DIM1=NPOIN)
              CALL OV('X=C     ', X=PATMOS, C=PATM, DIM1=NPOIN)
              CALL OV('X=C     ', X=TAIR%R, C=TA, DIM1=NPOIN)
              IF(PRESENT(PLUIE))THEN
                CALL OS('X=C     ',X = PLUIE, C=RAINFALL) ! MM/S
              ENDIF
!
!         TIME VARYING WATER QUALITY WITH HEAT EXCHANGE WITH ATMOSPHERE
            ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
              CALL INTERPMETEO(WW,UAIR,VAIR,TA,PATM,
     &                         HREL,NEBU,RAINFALL,EVAPORATION,AT,UL)
              CALL OV('X=C     ',X=WINDX, C=UAIR, DIM1=NPOIN)
              CALL OV('X=C     ',X=WINDY, C=VAIR, DIM1=NPOIN)
              CALL OV('X=C     ',X=PATMOS, C=PATM, DIM1=NPOIN)
              CALL OV('X=C     ',X=TAIR%R, C=TA, DIM1=NPOIN)
            ENDIF
!
           ELSEIF (VENT) THEN
!
!           WIND VARYING IN TIME CONSTANT IN SPACE
            IF(OPTWIND.EQ.2)THEN
10            CONTINUE
              IF(AT.GE.AT1_METEO.AND.AT.LT.AT2_METEO) THEN
                IF(AT2_METEO-AT1_METEO.GT.1.D-6) THEN
                  COEF=(AT-AT1_METEO)/(AT2_METEO-AT1_METEO)
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R
!				  write(50,*)'COEF: ', COEF
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R				  

                ELSE
                  COEF=0.D0
                ENDIF
                UAIR=FUAIR1_METEO+COEF*(FUAIR2_METEO-FUAIR1_METEO)
                VAIR=FVAIR1_METEO+COEF*(FVAIR2_METEO-FVAIR1_METEO)
				
                IF(LISTIN) THEN
!                  WRITE(LU,*) 'WIND AT T=',AT,' UAIR=',
!     &                                     UAIR,' VAIR=',VAIR
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R
!                  WRITE(50,*) 'WIND AT T=',AT,' UAIR=',
!    &                                     UAIR,' VAIR=',VAIR
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R

                ENDIF
              ELSE
                AT1_METEO=AT2_METEO
                FUAIR1_METEO=FUAIR2_METEO
                FVAIR1_METEO=FVAIR2_METEO
                READ(UL,*,ERR=100,END=200) AT2_METEO,FUAIR2_METEO,
     &                                     FVAIR2_METEO
                GO TO 10
!
!-----------------------------------------------------------------------
!
100             CONTINUE
                WRITE(LU,*) ' '
                WRITE(LU,*) 'METEO'
                WRITE(LU,*) 'ERROR IN THE WIND FILE'
                CALL PLANTE(1)
                STOP
200             CONTINUE
                WRITE(LU,*) ' '
                WRITE(LU,*) 'METEO'
                WRITE(LU,*) 'WIND FILE TOO SHORT'
                CALL PLANTE(1)
                STOP
              ENDIF
              CALL OV('X=C     ', X=WINDX, C=UAIR, DIM1=NPOIN)
              CALL OV('X=C     ', X=WINDY, C=VAIR, DIM1=NPOIN)
!
              FUAIR = UAIR
              FVAIR = VAIR
!
!             WIND VARYING IN TIME AND SPACE
!
            ELSEIF(OPTWIND.EQ.3)THEN
              WRITE(LU,*) 'THIS OPTION IS NOT IMPLEMENTED YET'
              WRITE(LU,*) 'SEE VALIDATION CASE WIND_TXY '
              WRITE(LU,*) 'LOCATED AT THE FOLDER EXAMPLES/TELEMAC2D'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
!
!       WIND AND/OR WATER QUALITY VARIABLES
!       VARYING IN SPACE AND TIME, FROM A BINARY FILE
!
        IF(FILES(ATMFILEB)%NAME(1:1).NE.' ') THEN
          IF(FILES(ATMFILEA)%NAME(1:1).NE.' ')THEN
            WRITE(LU,*) 'METEO: THE DATA FROM THE ASCII METEO'
            WRITE(LU,*) 'FILE WILL BE OVERWRITTEN BY THE'
            WRITE(LU,*) 'CORRESPONDING BINARY FILE DATA'
          ENDIF
          CALL METEO_FROM_BINARY_FILE(PATMOS,WINDX,WINDY,AT,NPOIN,VENT,
     &                                ATMOS,ATMFILEB,FILES,LISTIN,
     &                                WATER_QUALITY,PLUIE,OPTWIND,
     &                                WIND_SPD)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!

! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!      close(50)
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_


      RETURN
      END
