      PROGRAM TRUNCATE
C	USE MSFLIB
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     Program to modify the output files of program RDVEL2 to permit
C     simulation of limited segments of annular regions on an accretion disk.
C     This part of the program selects the desired region by setting visibility
C     keys of remaining individual segments to 0.
C     The program also permits setting temperature profiles on individual
C     annuli so that the profile is not axisymmetric.
C     This capability permits simulation of a hot spot region on the face of
C     the accretion disk.
C
C     *******************
C     Version TRUNCATE
C     *******************
C
      INCLUDE 'ACOMA.FOR'
	INCLUDE 'COMA.FOR'
	INCLUDE 'COMM.FOR'
	INCLUDE 'COMR.FOR'
      COMMON /NUNIT/ NU, NUOUT, IFOUT
C
      DIMENSION VLA(MTW,MTHM,MPHM)
      DIMENSION STVS(MTW,MTHM,MPHM)
C
      DIMENSION CSGR(MRM,MSG),CSGF(MRING,MSG),
     $PRFH(MRING,MSG),PRRH(MRM,MSG)
C
      DIMENSION PJARM(MRM,MSG),VRRM(MRM,MSG)
      DIMENSION PJAFC(MRING,MSG),VFACE(MRING,MSG),PHA(MSG)
      DIMENSION TMPRF(MRING,MSG),TMPRR(MRM,MSG),GBFFC(MRING,MSG)
	DIMENSION GBFR(MSG)
      DIMENSION SMV(MRING,MSG),RMIFLX(MRM,MSG),VC(MRM)
	DIMENSION STARTANG(MRING),ENDANG(MRING)
	DIMENSION NSTART(MRING),NEND(MRING)
	DIMENSION VINITANG(MRING),VINENDANG(MRING),VNDITANG(MRING)
	DIMENSION VNDENDANG(MRING),TRTMP(MRING)
	DIMENSION KINSTART(MRING),KINEND(MRING),KNDSTART(MRING)
	DIMENSION KNDEND(MRING)
	DIMENSION FL1(14)
      CHARACTER * 7 LABEL
	CHARACTER * 36 FL1
	DATA IN1,IN2,IN3,IN4,IN5/1,2,3,4,5/
	DATA NOUT1,NOUT2/6,7/
	DATA IN6,IN7/8,9/
	DATA IN8,IN9,IN10,IN11,IN12/10,11,12,13,14/
	INTEGER(4) RESULT
	INTEGER * 2 KEYGOOD, KEYBAD
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
  100 FORMAT (A36)
  101 FORMAT (14F5.0)
  102 FORMAT(8F10.0)
  106 FORMAT (10I5)
  120 FORMAT (I2)
  122 FORMAT (4E16.8)
  206 FORMAT (3I5)
C***********************************************************
C      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=15, FILE='C:\LGTSYN\TRUNCATE\TRUNCATE.ILS', 
     $STATUS='OLD')
      READ (15, 100) (FL1(J),J=1,14)
	OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')
      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')
      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')
      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')
      OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS='OLD')
      OPEN (UNIT=NOUT1,   FILE=FL1(6),  STATUS='REPLACE')
      OPEN (UNIT=NOUT2,   FILE=FL1(7),  STATUS='REPLACE')
	OPEN (UNIT=IN6,   FILE=FL1(8),  STATUS='OLD')
	OPEN (UNIT=IN7,   FILE=FL1(9),  STATUS='OLD')
	OPEN (UNIT=IN8,   FILE=FL1(10),  STATUS='OLD')
	OPEN (UNIT=IN9,   FILE=FL1(11),  STATUS='OLD')
	OPEN (UNIT=IN10,   FILE=FL1(12),  STATUS='OLD')
	OPEN (UNIT=IN11,   FILE=FL1(13),  STATUS='OLD')
	OPEN (UNIT=IN12,   FILE=FL1(14),  STATUS='OLD')
C***********************************************************
C     Set test for successful completion of program
      WRITE (IN1,120) KEYBAD
	CLOSE (UNIT=IN1, STATUS = 'KEEP')
C     Read input data
      LBLERR=0
      LABEL = 'TRUNCAT'
      NU=IN2
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Input keys to decide whether to isolate annular regions
C     and whether to impose a new temperature profile.
C     Key to isolate annular regions
      CALL IREAD (KEY1)
	CALL DUMMY(1)
C     Key to impose new temperature profile
	CALL IREAD (KEY2)
	CALL DUMMY(1)
C     Key to hide all of accretion disk rim
      CALL IREAD (KEY3)
	CALL DUMMY(1)
C     Key to hide all of secondary star
	CALL IREAD (KEY4)
	CALL DUMMY(1)
C     Key to hide all of primary star
      CALL IREAD (KEY5)
C     Input data from program ACPGA
      CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,AVL,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN3)
C     Read the first line of the accretion disk face data
	READ (IN4,106) NTH,NRM,NSG,NRING
C     If KEY1=1, read in the starting angles of the visibility
C     region.
      IF (KEY1.EQ.1) THEN
		READ (IN6,101) (STARTANG(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment starting visibility
C     regions.
      DO K=1,NRING
		NSTART(K)=STARTANG(K)/360.*NSG
	END DO
C     If KEY1=1, read in the ending angles of the visibility
C     regions.
      IF (KEY1.EQ.1) THEN
		READ (IN7,101) (ENDANG(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment ending visibility
C     regions.
      DO K=1,NRING
		NEND(K)=ENDANG(K)/360.*NSG
	END DO
C     If KEY2=1, read in the starting angles of the initial
C     temperature ramp region.
      IF (KEY2.EQ.1) THEN
		READ (IN8,101) (VINITANG(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment starting the initial
C     temperature ramp regions.
      DO K=1,NRING
		KINSTART(K)=VINITANG(K)/360.*NSG
	END DO
C     If KEY2=1, read in the ending angles of the initial
C     temperature ramp region.
      IF (KEY2.EQ.1) THEN
		READ (IN9,101) (VINENDANG(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment ending the initial
C     temperature ramp regions.
      DO K=1,NRING
		KINEND(K)=VINENDANG(K)/360.*NSG
	END DO
C     If KEY2=1, read in the starting angle ot the terminating
C     temperature ramp region.
C     The hot spot region for this annulus is included between
C     this terminating angle and the immediately preceeding
C     angle.
      IF (KEY2.EQ.1) THEN
		READ (IN10,101) (VNDITANG(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment starting the terminating
C     temperature ramp regions.
      DO K=1,NRING
		KNDSTART(K)=VNDITANG(K)/360.*NSG
	END DO
C     If KEY2=1, read in the ending angle for the terminating
C     temperature ramp region.
      IF (KEY2.EQ.1) THEN
		READ (IN11,101) (VNDENDANG(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment ending the terminating
C     temperature ramp regions.
      DO K=1,NRING
		KNDEND(K)=VNDENDANG(K)/360.*NSG
	END DO
C     If KEY2=1, read in the temperature values for the hot spot
C     regions.
      IF (KEY2.EQ.1) THEN
		READ (IN12,102) (TRTMP(K),K=1,NRING)
	END IF
C     Calculate ordinal number of segment ending visibility
C     regions.
      DO K=1,NRING
		NEND(K)=ENDANG(K)/360.*NSG
	END DO
C     Write the same data on the first output file
      WRITE (NOUT1, 106) NTH,NRM,NSG,NRING
C     Read the second line
	READ (IN4,206) IOB,K,NSG
	BACKSPACE IN4
C     If the data point to an accretion disk, proceed
	IF (IOB.EQ.1) THEN
	 DO K=1,NRING
	    READ (IN4,206) IOB,KR,NSG
	    READ (IN4,122) (PJAFC(K,J),J=1,NSG)
	    READ (IN4,122) (PRFH(K,J),J=1,NSG)
	    READ (IN4,122) (CSGF(K,J),J=1,NSG)
	    READ (IN4,122) (VFACE(K,J),J=1,NSG)
	    READ (IN4,122) (PHA(J),J=1,NSG)
	 END DO
	 DO K=1,NRING
	    READ (IN5,206) IOB,KR,NSG
	    READ (IN5,122) (GBFFC(K,J),J=1,NSG)
		READ (IN5,122) (TMPRF(K,J),J=1,NSG)
		READ (IN5,122) (SMV(K,J),J=1,NSG)
	 END DO
C     Set the visibility keys according to the prescribed
C     starting and ending angles.
C     For a segment to be designated as hidden, the visibility
C     key must be negative.
      IF (KEY1.EQ.1) THEN
      DO K=1,NRING
		DO J=1,NSG
			IF (J.LT.NSTART(K)) PRFH(K,J)=-1.0
			IF (J.GE.NEND(K))   PRFH(K,J)=-1.0
		END DO
	END DO
	END IF
	 DO K=1,NRING
	    WRITE (NOUT1,206) IOB,K,NSG
	    WRITE (NOUT1,122) (PJAFC(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (PRFH(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (CSGF(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (VFACE(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (PHA(J),J=1,NSG)
	 END DO
C     Set the temperatures within the hot spot area
      IF (KEY2.EQ.1) THEN
	DO K=1, NRING
				AA=KINEND(K)-KINSTART(K)
				IF (AA.LT.0.D0) CALL PMDSTOP
				IF (AA.EQ.0.D0) AA=1.D0
				BB=KNDEND(K)-KNDSTART(K)
				IF (BB.LT.0.D0) CALL PMDSTOP
				IF (BB.EQ.0.D0) BB=1.D0
		DO J=1,NSG
			IF ((J.GE.KINSTART(K)).AND.(J.LT.KINEND(K))) THEN
				TMPRF(K,J)=(J-KINSTART(K))/AA*(TRTMP(K)-TMPRF(K,J))
     $				+TMPRF(K,J)
			END IF
			IF ((J.GE.KINEND(K)).AND.(J.LE.KNDSTART(K))) THEN
				TMPRF(K,J)=TRTMP(K)
			END IF
			IF ((J.GT.KNDSTART(K)).AND.(J.LE.KNDEND(K))) THEN
				TMPRF(K,J)=(KNDEND(K)-J)/BB*(TRTMP(K)-TMPRF(K,J))
     $				+TMPRF(K,J)
			END IF
		END DO
	END DO
	END IF
	 DO K=1,NRING
	    WRITE (NOUT2,206) IOB,K,NSG
	    WRITE (NOUT2,122) (GBFFC(K,J),J=1,NSG)
		WRITE (NOUT2,122) (TMPRF(K,J),J=1,NSG)
		WRITE (NOUT2,122) (SMV(K,J),J=1,NSG)
	 END DO
	ELSE
		STOP
	END IF
	IOB=IOB+1
C     Visibility keys are not changed on the rim unless set
C     by KEY3
	 DO K=1,NRM
	    READ (IN4,206) IOB,KR,NSG
	    READ (IN4,122) (PJARM(K,J),J=1,NSG)
	    READ (IN4,122) (PRRH(K,J),J=1,NSG)
	    READ (IN4,122) (CSGR(K,J),J=1,NSG)
	    READ (IN4,122) (VRRM(K,J),J=1,NSG)
	    READ (IN4,122) (PHA(J),J=1,NSG)
	 END DO
	IF (KEY3.EQ.1) THEN
		DO K=1,NRM
			DO J=1,NSG
				PRRH(K,J)=-1.D0
			END DO
		END DO
	END IF
	 DO K=1,NRM
	    WRITE (NOUT1,206) IOB,K,NSG
	    WRITE (NOUT1,122) (PJARM(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (PRRH(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (CSGR(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (VRRM(K,J),J=1,NSG)
	    WRITE (NOUT1,122) (PHA(J),J=1,NSG)
	 END DO
	 DO K=1,NRM
	    READ (IN5,206) IOB,KR,NSG
	    READ (IN5,122) (GBFR(J),J=1,NSG)
	    READ (IN5,122) (TMPRR(K,J),J=1,NSG)
		READ (IN5,122) (RMIFLX(K,J),J=1,NSG)
	 END DO 
	 DO K=1,NRM
	    WRITE (NOUT2,206) IOB,K,NSG
	    WRITE (NOUT2,122) (GBFR(J),J=1,NSG)
	    WRITE (NOUT2,122) (TMPRR(K,J),J=1,NSG)
		WRITE (NOUT2,122) (RMIFLX(K,J),J=1,NSG)
	 END DO
	IOB=3
      DO ITL=1,NTH
	    READ (IN4,206) II,ITH,N
	    READ (IN4,122) (VL1(1,ITH,KN),KN=1,N)
	    READ (IN4,122) (STVS(1,ITH,KN),KN=1,N)
	    READ (IN4,122) (CSG(1,ITH,KN),KN=1,N)
	    READ (IN4,122) (VLA(1,ITH,KN),KN=1,N)
	    READ (IN4,122) (PH(1,ITH,KN),KN=1,N)
	    READ (IN5,206) II,ITH,N
	    READ (IN5,122) (G(1,ITH,KN),KN=1,N)
	    READ (IN5,122) (TMPR(1,ITH,KN),KN=1,N)
		READ (IN5,122) (FIN(1,ITH,KN),KN=1,N)
C     If KEY5=1, hide the primary component
		IF (KEY5.EQ.1) THEN
			DO KN=1,N
				STVS(1,ITH,KN)=-1.D0
			END DO
		END IF
		WRITE (NOUT1,206) IOB,ITH,N
		WRITE (NOUT1,122) (VL1(1,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (STVS(1,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (CSG(1,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (VLA(1,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (PH(1,ITH,KN),KN=1,N)
		WRITE (NOUT2,206) IOB,ITH,N
		WRITE (NOUT2,122) (G(1,ITH,KN),KN=1,N)
		WRITE (NOUT2,122) (TMPR(1,ITH,KN),KN=1,N)
		WRITE (NOUT2,122) (FIN(1,ITH,KN),KN=1,N)
	END DO
C     Now do other star      
	IOB=4
      DO ITL=1,NTH
	    READ (IN4,206) II,ITH,N
		WRITE (NOUT1,206) IOB,ITH,N
	    READ (IN4,122) (VL1(2,ITH,KN),KN=1,N)
	    READ (IN4,122) (STVS(2,ITH,KN),KN=1,N)
	    READ (IN4,122) (CSG(2,ITH,KN),KN=1,N)
	    READ (IN4,122) (VLA(2,ITH,KN),KN=1,N)
	    READ (IN4,122) (PH(2,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (VL1(2,ITH,KN),KN=1,N)
C     If KEY4=1, hide the secondary component
		IF (KEY4.EQ.1) THEN
			DO KN=1,N
				STVS(2,ITH,KN)=-1.D0
			END DO
		END IF
		WRITE (NOUT1,122) (STVS(2,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (CSG(2,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (VLA(2,ITH,KN),KN=1,N)
		WRITE (NOUT1,122) (PH(2,ITH,KN),KN=1,N)
	    READ (IN5,206) II,ITH,N
	    READ (IN5,122) (G(2,ITH,KN),KN=1,N)
	    READ (IN5,122) (TMPR(2,ITH,KN),KN=1,N)
		READ (IN5,122) (FIN(2,ITH,KN),KN=1,N)
		WRITE (NOUT2,206) IOB,ITH,N
		WRITE (NOUT2,122) (G(2,ITH,KN),KN=1,N)
		WRITE (NOUT2,122) (TMPR(2,ITH,KN),KN=1,N)
		WRITE (NOUT2,122) (FIN(2,ITH,KN),KN=1,N)
      END DO
C
C     Write key to indicate successful completion of program
   99	OPEN (UNIT=IN1,  FILE=FL1(1),  STATUS='OLD')
	WRITE (IN1,120) KEYGOOD
	CLOSE (UNIT=IN1, STATUS = 'KEEP')
	CLOSE (UNIT=IN2, STATUS = 'KEEP')
	CLOSE (UNIT=IN3, STATUS = 'KEEP')
	CLOSE (UNIT=IN4, STATUS = 'KEEP')
	CLOSE (UNIT=IN5, STATUS = 'KEEP')
	CLOSE (UNIT=NOUT1, STATUS = 'KEEP')
	CLOSE (UNIT=NOUT2, STATUS = 'KEEP')
	STOP
	END
