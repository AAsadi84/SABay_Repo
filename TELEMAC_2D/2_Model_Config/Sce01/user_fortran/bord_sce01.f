!                    ***************
                     SUBROUTINE BORD
!                    ***************
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)

      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY: STA_DIS_CURVES,PTS_CURVES,QZ,
     &                                  FLUX_BOUNDARIES,MAXFRO,FRTYPE,
     &                                  TIDALTYPE,BOUNDARY_COLOUR,
     &                                  T2D_FILES,T2DFO1
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_IMAX,P_DMIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=144), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
!

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,MSK8,IFRLIQ,YADEB(MAXFRO),IERR,ITRAC,IFR,N,IELEB,KP1
!
      DOUBLE PRECISION Z,QIMP,ZMIN(MAXFRO)
!
      LOGICAL YAZMIN
!
      INTRINSIC MAX
!
!     PROVISOIRE
!     PROVISIONAL
!
      DOUBLE PRECISION DIS_STA_CUR
      EXTERNAL         DIS_STA_CUR
!
!-----------------------------------------------------------------------

      DOUBLE PRECISION ALF ,PI,Y100,AJUL4,NLUN, HT
      DOUBLE PRECISION  DD,JD,TUNIV,HSUN,SLUN,PLUN,TLUN,PSUN
      DOUBLE PRECISION TONDES (120)
      DOUBLE PRECISION UONDES(120),FONDES(120),VONDES(120)
      DOUBLE PRECISION AHN(50,120) , PHN(50,120)
      INTEGER NPTFRL,IPTFRL,NPTFRLM,IONDES,NONDES,NSPECTR,NFO1
      INTEGER YY,MM,DAY,HOUR,MINU,SEC,AJUL,BJUL
      DOUBLE PRECISION NIVM(50)
      DOUBLE PRECISION PROF(NPOIN)
      SAVE AHN, PHN
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
	  REAL, DIMENSION(1,4) :: TIDE
	  INTEGER cnt, bc_cnt
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
      DOUBLE PRECISION, POINTER :: X(:),Y(:)
      X=>MESH%X%R
      Y=>MESH%Y%R
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_	  
!	  open(unit=36,file="../Sab_myfile_02.txt", access = 'append')
!	  rewind(36)
	  open(unit=37,file="../CSV_files/tide_value.csv")
	  READ(37, *) TIDE
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_	
!
!
	  NFO1=T2D_FILES(T2DFO1)%LU
!
!
!
!

      YAZMIN=.FALSE.
      DO IFR=1,NFRLIQ
        ZMIN(IFR)=1.D99
        IF(PROVEL(IFR).EQ.5.OR.STA_DIS_CURVES(IFR).EQ.2) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR
          IFR=NUMLIQ(K)
		  ! asgin the value of the elevation using the .cli file
          IF(IFR.GT.0) ZMIN(IFR)=MIN(ZMIN(IFR),ZF(NBOR(K))+H%R(NBOR(K)))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFR=1,NFRLIQ
            ZMIN(IFR)=P_DMIN(ZMIN(IFR))
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      MSK8 = 8
!
!  INITIALISATION OF YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
	  
      PI=ACOS(-1.D0)
!
!
! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
! The number of the open boundaries changed from 28 to 289
! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      NPTFRLM = 272
! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
!
!
      REWIND NFO1
      READ(NFO1,*) NSPECTR
      READ(NFO1,*) NONDES
	  
      DO IONDES =1,NONDES
        READ (NFO1,*) TONDES(IONDES)
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!		write(36,*) 'TONDES(IONDES)', TONDES(IONDES)
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
      ENDDO

      DO IONDES =NONDES+1,NSPECTR
        READ (NFO1,*) TONDES(IONDES)
      ENDDO
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
	  cnt = 0
	  bc_cnt = 0
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
      IPTFRL = 1
	  
      DO K=1,NPTFR
!
        IF(LIHBOR(K).EQ.KENT) THEN
!
!		
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
			IF(NUMLIQ(K).EQ.1) THEN
				HBOR(K) = -ZF(NBOR(K)) + TIDE(1,1) * 0.3048
				cnt = cnt+1
			ENDIF
			IF(NUMLIQ(K).EQ.2) THEN
				HBOR(K) = -ZF(NBOR(K)) + TIDE(1,2) * 0.3048
				cnt = cnt+1
			ENDIF
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
!GB
            IPTFRL=IPTFRL+1
        ENDIF

!  DISCHARGE IMPOSED: VARIOUS OPTIONS ACCORDING TO PROVEL
!                 ONE USES THE VALUES PROVIDED BY THE USER
!                 AS VELOCITY PROFILE.
!                 UBOR(K,2) AND VBOR(K,2) ARE THE VALUES OF
!                 THE CONLIM FILE, AND ARE CONSERVED.
!
      IF(LIUBOR(K).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        IFR=NUMLIQ(K)
        IF(PROVEL(IFR).EQ.1) THEN
!         CONSTANT NORMAL PROFILE
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(IFR).EQ.2) THEN
!         PROFILE PROVIDED BY THE USER
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.3) THEN
!         NORMAL VELOCITY PROVIDED IN UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.4) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H

! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!		  write(36, *) 'NORMAL PROFILE IN SQUARE ROOT OF H'
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_

          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
		  bc_cnt = bc_cnt + 1
!		  write(36,*)'H%R(NBOR(K))', H%R(NBOR(K))
!		  write(36,*)'1.UBOR(K,1) = ', UBOR(K,1)
!		  write(36,*)'1.VBOR(K,1) = ', VBOR(K,1)
!		  write(36,*)'K =', K
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
        ELSEIF(PROVEL(IFR).EQ.5) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!         DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
          UBOR(K,1)=-XNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
          VBOR(K,1)=-YNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
        ELSE
          WRITE(LU,*) 'BOUNDARY ',IFR
          WRITE(LU,*) 'VELOCITY PROFILE ',PROVEL(IFR),
     &                ' NOT IMPLEMENTED YET'
          WRITE(LU,*) 'PLEASE GIVE A VALUE BETWEEN 1 AND 5'
          CALL PLANTE(1)
          STOP
        ENDIF
!       ONE DOES NOT SET VELOCITY IF THERE IS NO WATER.
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!		  write(36,*)'H%R(NBOR(K))', H%R(NBOR(K))
!		  write(36,*)'water depth is equal to zero or less at:',TEMPS
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
!       U AND V INITIALISED WITH THE IMPOSED VALUES
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU.AND.
     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!		write(36,*)'hello Velocity imposed!'
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
        IF(NUMLIQ(K).GT.0) THEN
          IFR=NUMLIQ(K)
          IF(PROVEL(IFR).EQ.1) THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),N)
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),N)
          ELSEIF(PROVEL(IFR).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(IFR).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ELSE
            WRITE(LU,*) 'BOUNDARY ',IFR
            WRITE(LU,*) 'PROFILE ',PROVEL(IFR),
     &                  ' ASKED'
            WRITE(LU,*) 'IMPOSSIBLE COMBINATION'
            CALL PLANTE(1)
            STOP
          ENDIF
!         U AND V INITIALISED WITH THE IMPOSED VALUES
!         IF NOT IN THOMPSON MODE
          IF(FRTYPE(IFR).NE.2) THEN
            U%R(NBOR(K)) = UBOR(K,1)
            V%R(NBOR(K)) = VBOR(K,1)
          ENDIF
        ENDIF
      ENDIF
!
!  IMPOSED TRACER
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
        IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.
     &    (NTRACE.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
!         THE CASE NUMLIQ(K)=0 CORRESPONDS TO A SINGULARITY INITIALLY
!         DECLARED AS A SOLID BOUNDARY AND FOR WHICH
!         TBOR IS FILLED IN CLHUVT
          IF(NUMLIQ(K).GT.0) THEN
            N=NBOR(K)

            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
				IF(NUMLIQ(K).EQ.1) THEN
					Z = TIDE(1,3)
!				write(36,*) 'Z1 = ', Z
				ENDIF
				IF(NUMLIQ(K).EQ.2) THEN
					Z = TIDE(1,4)
!				write(36,*) 'Z2 = ', Z
				ENDIF
!            Z = TR(NUMLIQ(K),ITRAC,N,IERR)
!			write(36,*) 'Z =', Z
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_

            IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
          ENDIF
        ENDIF
        ENDDO
      ENDIF
!
      ENDDO ! K

!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!	  write(36,*)'bc_cnt = ',bc_cnt	  
	  bc_cnt = 0	  
!	  write(36,*)'UBOR: ', UBOR(:,1)	
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_	
!
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T2D()

! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!	  write(36,*)'TIDALTYPE = ', TIDALTYPE
!	  write(36,*)'UBOR: ', UBOR	
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_	  

!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!	  write(36,*) 'U%ELM = ', U%ELM
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
      IF(U%ELM .EQ.13)THEN
        DO IELEB=1,MESH%NELEB
          K  =MESH%IKLBOR%I(IELEB)
          KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &      (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
            U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(KP1,1))*0.5D0
            V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(KP1,1))*0.5D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CASE OF DISCHARGE IMPOSED:
!
!  LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
        DO IFRLIQ = 1 , NFRLIQ
!
          IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
!
!           ONE TAKES THE MASK OF LIQUID BOUNDARIES MSK8, WHICH IS
!           EQUAL TO THE MASK OF THE DISCHARGE IMPOSED ON A DISCHARGE
!           IMPOSED BOUNDARY. THIS MAKES IT POSSIBLE TO CHANGE A FREE
!           VELOCITY BOUNDARY TO A DISCHARGE IMPOSED TO A LEVEL IMPOSED
!           BOUNDARY, IN SPITE OF THE FACT THAT THE MASKS ARE MADE IN
!           PROPIN BEFORE THE CALL TO BORD
!
            IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
            IF(YADEB(IFRLIQ).EQ.1) THEN
              IF(STA_DIS_CURVES(IFRLIQ).EQ.2) THEN
                QIMP=DIS_STA_CUR(IFRLIQ,PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                           ZMIN(IFRLIQ))
              ELSE
                QIMP=Q(IFRLIQ)
              ENDIF
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!			  write(36,*)'QIMP: ', QIMP
!			  write(36,*)'VBOR: ', VBOR	
!			  write(36,*)'H%R', H%R
			  bc_cnt = bc_cnt + 1
!			  write(36,*)'bc_cnt =', bc_cnt
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
              CALL DEBIMP(QIMP,UBOR,VBOR,U,V,H,NUMLIQ,
     &                    IFRLIQ,TRA05,TRA06,
     &                    NPTFR,MASK%ADR(MSK8)%P%R,MESH,MESH%KP1BOR%I,
     &                    EQUA)
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!			  write(36,*)'QIMP: ', QIMP
!			  write(36,*)'H%R(NBOR): ', H%R(NBOR)
!			  Write(36,*)'HBOR = ', HBOR
!			  write(36,*)'UBOR: ', UBOR
!			  write(36,*)'VBOR: ', VBOR
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!			  
            ENDIF
!
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM.EQ.13) THEN
        DO IELEB=1,MESH%NELEB
          K  =MESH%IKLBOR%I(IELEB)
          KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(KP1,1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(KP1,1))*0.5D0
        ENDDO
      ENDIF

      IF(NCSIZE.GT.1) CALL PARCOM_BORD(HBOR,1,MESH)
!
!-----------------------------------------------------------------------
!
!
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!      close(36)
! A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_A_R_
!
!
      RETURN
      END
