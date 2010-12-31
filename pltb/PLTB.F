      PROGRAM PLTB													  
C     PROGRAM TO PRODUCE PLOTS OF PROJECTED OUTLINES OF TWO STARS,ON
C     THE PLANE OF THE SKY
C     FOR A SELECTION OF ORBITAL PHASES
C     USES CONTROL FUNCTION OF ARRAY LCT   
C     LCT(1)=1,PLOT GRID OF VISIBLE POINTS,ECLIPSED STAR
C     LCT(2)=1,PLOT LARGE THETA HORIZON,ECLIPSED STAR
C     LCT(3)=1,PLOT SMALL THETA HORIZON,ECLIPSED STAR
C     LCT(4)=1,PLOT OUTLINE,ECLIPSED STAR
C     LCT(5)=1,PLOT ALL POINTS,ECLIPSED STAR
C     LCT(6)=1,PLOT XS,XN POINTS,ECLIPSED STAR
C     LCT(7)=1,PLOT SHADOW BOUNDARY POINTS,ECLIPSED STAR
C     LCT(8)=1,INCLUDE PLOT OF POINTS ON ECLIPSED STAR INSIDE
C      BOUNDARY OF ECLIPSING STAR     
C     LCT(9)=1,PLOT GRID,ECLIPSING STAR
C     LCT(10)=1,PLOT LARGE THETA HORIZON,ECLIPSING STAR
C     LCT(11)=1,PLOT SMALL THETA HORIZON,ECLIPSING STAR
C     LCT(12)=1,PLOT OUTLINE,ECLIPSING STAR
C     LCT(13)=1,PLOT ALL POINTS,ECLIPSING STAR
C     LCT(14)=1,PLOT XS,XN POINTS,ECLIPSING STAR
C     LCT(15)=1,DO NOT ROTATE THE COORDINATE SYSTEM
C     LCT(16)=1,PLOT SPOTS
C
C     Program does eclipsing star first, for ICOUNT=1, then eclipsed
C     star
C     for ICOUNT=2
C     IEC designates the eclipsing star
C     ISV designates the star whose center is at the origin of
C     coordinates. NOTE!!! This is also the star eclipsed at orbital
C     phase 0.0!
C
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
	  INCLUDE 'COMV.FOR'
      CHARACTER * 28 FL1
      CHARACTER * 10 DATE,TIME,ZONE
	  INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DIMENSION FL1(36)
      DIMENSION TARY(MTR,MTR),VLPSI(MPHS),LV(13)
	  DIMENSION XSV(MTHM,MPHS),YSV(MTHM,MPHS),ZSV(MTHM,MPHS)
      DIMENSION LCT(16)
C
C     *****************************************************************
C     	 
      DIMENSION AGLV(MPHM),VLMDA(MPHM),VGMDA(5,MPHM),VBTDA(MPHM)
      DIMENSION VLMDB(MPHM),VGMDB(5,MPHM),VBTDB(MPHM)
C     *****************************************************************
C
C     *****************************************************************
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 20 OBJ
	  INTEGER * 2 KEYGOOD,KEYBAD
	  DATA KEYGOOD/0/
	  DATA KEYBAD/-1/
      DATA PIO2/1.5707963267948D0/
      DATA PI,TWPI/3.1415926535898D0,6.2831853071796D0/
      DATA TPIO2/4.7123889803847D0/
      DATA ANG/6.29D0/
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,5,6,7/
      DATA IN8,IN9/8,9/
      DATA IN10,IN11,IN12,IN13/10,11,12,13/
      DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5/20,21,22,23,24/
	  DATA NOUT6,NOUT7,NOUT8,NOUT9/25,26,27,28/
	  DATA NOUT10,NOUT11,NOUT12,NOUT13/29,30,31,32/
	  DATA NOUT14,NOUT15/33,34/
	  DATA NOUT16,NOUT17,NOUT18,NOUT19/35,36,37,38/
	  DATA NOUT20,NOUT21/39,40/
	  DATA NOUT22,NOUT23/41,42/
      DATA NPT/200/
      DATA CF/57.295779513082320876798D0/
	  DATA ERLT/1.D-13/
  401 FORMAT (I2)
  193 FORMAT (3F12.5,I8)
  194 FORMAT (' ')
  195 FORMAT (3F10.5)
  201 FORMAT (A28)
  104 FORMAT (1H1)      
  105 FORMAT (80I1)     
  106 FORMAT (10I5)     
  107 FORMAT (F6.3)     
  111 FORMAT (1X,'VALUE OF NPND.GT.NPSI')    
  112 FORMAT (8F10.5)
  113 FORMAT (1X,'End of PLTB')
  227 FORMAT (7F18.13)
  254 FORMAT (1X,'PROGRAM CALPT',26X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/,13X,'**OUT**',/,1X,'*')
  255 FORMAT (1X,'Program running time=',F12.4,2X,'seconds')
  416 FORMAT (A20)
  417 FORMAT (A9,3X,A8)
  418 FORMAT (1X,'Ordinal frame no.',I2)
  419 FORMAT (1X,'Orbital longitude=',F10.3)
  420 FORMAT (5X,'ICOUNT=',I2)
  421 FORMAT (1X,'Orbital inclination=',F8.3)
C********************************************************
C
      OPEN (UNIT= 43,  FILE= 'PLTB.ILS',STATUS= 'OLD')
      READ (43, 201)   (FL1(J),J=1,36)
      OPEN (UNIT=  IN1, FILE= FL1(1), STATUS='OLD')
      OPEN (UNIT=  IN2, FILE= FL1(2), STATUS='OLD')
      OPEN (UNIT=  IN3, FILE= FL1(3), STATUS='OLD')
      OPEN (UNIT=  IN4, FILE= FL1(4), STATUS='OLD')
      OPEN (UNIT=  IN5, FILE= FL1(5), STATUS='OLD')
      OPEN (UNIT=  IN6, FILE= FL1(6), STATUS='OLD')
      OPEN (UNIT=  IN7, FILE= FL1(7), STATUS='OLD')
      OPEN (UNIT=  IN8, FILE= FL1(8), STATUS='OLD')
      OPEN (UNIT=  IN9, FILE= FL1(9), STATUS='OLD')
      OPEN (UNIT= IN10, FILE=FL1(10), STATUS='OLD')
	  OPEN (UNIT= IN11, FILE=FL1(11), STATUS='OLD')
	  OPEN (UNIT= IN12, FILE=FL1(12), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(13), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(15), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(16), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(17), STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(18), STATUS='REPLACE')
      OPEN (UNIT=NOUT7, FILE=FL1(19), STATUS='REPLACE')
      OPEN (UNIT=NOUT8, FILE=FL1(20), STATUS='REPLACE')
	  OPEN (UNIT=NOUT9, FILE=FL1(21), STATUS='REPLACE')
      OPEN (UNIT=NOUT10,FILE=FL1(22), STATUS='REPLACE')
	  OPEN (UNIT=NOUT11,FILE=FL1(23), STATUS='REPLACE')
	  OPEN (UNIT=NOUT12,FILE=FL1(24), STATUS='REPLACE')
	  OPEN (UNIT=NOUT13,FILE=FL1(25), STATUS='REPLACE')
	  OPEN (UNIT=NOUT14,FILE=FL1(26), STATUS='REPLACE')
      OPEN (UNIT=NOUT15,FILE=FL1(27), STATUS='REPLACE')
	  OPEN (UNIT=NOUT16,FILE=FL1(28), STATUS='REPLACE')
	  OPEN (UNIT=NOUT17,FILE=FL1(29), STATUS='REPLACE')
	  OPEN (UNIT=NOUT18,FILE=FL1(30), STATUS='REPLACE')
	  OPEN (UNIT=NOUT19,FILE=FL1(31), STATUS='REPLACE')
	  OPEN (UNIT=NOUT20,FILE=FL1(32), STATUS='REPLACE')
	  OPEN (UNIT=NOUT21,FILE=FL1(33), STATUS='REPLACE')
	  OPEN (UNIT=NOUT22,FILE=FL1(34), STATUS='REPLACE')
	  OPEN (UNIT=NOUT23,FILE=FL1(35), STATUS='REPLACE')
	  OPEN (UNIT= IN13, FILE=FL1(36), STATUS='OLD')
C********************************************************
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (IN13,401) KEYBAD
      WRITE (NOUT1,254) IMON,IDAY,IYR,IHR,IMIN,ISEC
      NKP=0
	  CLOSE (UNIT=IN13, STATUS = 'KEEP')
      LABEL= '***PLTB'
      NU=IN1
      LBLERR=0
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL CREAD ( OBJ)
      CALL DUMMY (1)
      CALL IREAD ( NPST)
      CALL DUMMY (1)
      CALL IREAD ( NPND)
      CALL DUMMY (1)
      CALL IREAD (INDX)
      CALL DUMMY (1)
      CALL IREAD ( LCT(1))
      CALL DUMMY (1)
      CALL IREAD ( LCT(2))
      CALL DUMMY (1)
      CALL IREAD ( LCT(3))
      CALL DUMMY (1)
      CALL IREAD ( LCT(4))
      CALL DUMMY (1)
      CALL IREAD ( LCT(5))
      CALL DUMMY (1)
      CALL IREAD ( LCT(6))
      CALL DUMMY (1)
      CALL IREAD ( LCT(7))
      CALL DUMMY (1)
      CALL IREAD ( LCT(8))
      CALL DUMMY (2)
      CALL IREAD ( LCT (9))
      CALL DUMMY (1)
      CALL IREAD ( LCT(10))
      CALL DUMMY (1)
      CALL IREAD ( LCT(11))
      CALL DUMMY (1)
      CALL IREAD ( LCT(12))
      CALL DUMMY (1)
      CALL IREAD ( LCT(13))
      CALL DUMMY (1)
      CALL IREAD ( LCT(14))
      CALL DUMMY (1)
      CALL IREAD ( LCT(15))
      CALL DUMMY (1)
      CALL IREAD ( LCT(16))
      CALL DUMMY (1)
C*********************************************************************
      READ (IN4,106) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN4,227) (VLPSI(K),K=1,NPSI)
	  READ (IN4,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
	  READ (IN4,106) (LV(K),K=1,13)
      READ (IN4,227) OMRAD,ECC,SMA,TH0,VCONJ,VMN0,PHZERO,THFCP,THSCP,
     $THTCP,THLCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,THTCS,
     $THLCS,THFOUR,PHFCP,PHSCP,PHTCP,PHLCP,PHTHREE,PHFCS,
     $PHSCS,PHTCS,PHLCS,PHFOUR
      PHSEC=THTWO
C     Set test for max. elongations
      IF ((DABS(THTHREE).LE.ERLT).AND.(DABS(THFOUR).LE.ERLT)) THEN
         THTS1=PIO2
         THTS2=TPIO2
      ELSE
         THTS1=THTHREE
         THTS2=THFOUR
      END IF
      IF (DABS(PHSEC).LE.ERLT) THEN
         THTS3=PI
         THTS4=0.D0
      ELSE
         THTS3=PHSEC
         THTS4=PHZERO
      END IF
      NU= IN5
	  CALL IREAD ( NSPA)
	  CALL DUMMY (1)
	  CALL IREAD ( NSPB)
      NU=IN12
      READ (IN12,106,END=399,ERR=299) NWL
      NU=IN6
      CALL DUMMY(2)
C     ENTER DATA ON SPOTS
C
      NSP=NSPA
      READ (IN6,112,END=399,ERR=299) (VLMDA(K),K=1,NSP)
C
         DO K=1,NSP
         VLMDA(K)=VLMDA(K)/CF
         END DO
      NU=IN7
      CALL DUMMY(2)
      NLT=NWL
      IF (NLT.GT.5) NLT=5
      DO J=1,NLT
	  IF (NSP.GT.0) THEN
		READ (IN7,112,END=399,ERR=299) (VGMDA(J,K),K=1,NSP)
	  END IF
       DO K=1,NSP
       VGMDA(J,K)=VGMDA(J,K)/CF
       END DO
      END DO
      NU=IN8
      CALL DUMMY(2)
      READ (IN8,112,END=399,ERR=299) (VBTDA(K),K=1,NSP)
       DO K=1,NSP
       VBTDA(K)=VBTDA(K)/CF
       END DO 
C     End of spot data for component I
      NU=IN9
	  CALL DUMMY (2)
      NSP=NSPB
      READ (IN9,112,END=399,ERR=299) (VLMDB(K),K=1,NSP)
C
         DO K=1,NSP
         VLMDB(K)=VLMDB(K)/CF
         END DO
      NU=IN10
      CALL DUMMY(2)
      NLT=NWL
      IF (NLT.GT.5) NLT=5
      DO J=1,NLT
	  IF (NSP.GT.0) THEN
		READ (IN10,112,END=399,ERR=299) (VGMDB(J,K),K=1,NSP)
	  END IF
       DO K=1,NSP
       VGMDB(J,K)=VGMDB(J,K)/CF
       END DO
      END DO
      NU=IN11
      CALL DUMMY(2)
      READ (IN11,112,END=399,ERR=299) (VBTDB(K),K=1,NSP)
       DO K=1,NSP
       VBTDB(K)=VBTDB(K)/CF
       END DO
C     End of spot data for component 3-I	  
C	       
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN2)
      IF ((NPST.EQ.0).AND.(NPND.EQ.0)) NPND=NPSI  
      IF (NPND.GT.NPSI) GO TO 199     
C*******************************************************************
C     START OF PRINCIPAL LOOP IN ORBITAL LONGITUDE
C*******************************************************************
      KSRF=0
      DO 100 KS=1,NPND
 	  K2CT=0
      REWIND IN4
C     Initialize arrays
	  DO J=1,MTHM
		XHZE(J)=0.D0
		YHZE(J)=0.D0
		XHZB(J)=0.D0
		YHZB(J)=0.D0
		DO JJ=1,3
			RS(J,JJ)=0.D0
			ALP(J,JJ)=0.D0
		END DO
	  END DO
C  
C  
      CALL RDWT2(NT,NU,NTH,ISV,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN3)
C     ISV ALWAYS DESIGNATES THE COMPONENT AT THE ORIGIN OF COORDINATES   
C     SET IEC TO THE DESIGNATION OF THE ECLIPSING STAR
      IF ((PSI.LE.THTS1).OR.(PSI.GE.THTS2)) IEC=3-ISV
      IF ((PSI.GT.THTS1).AND.(PSI.LT.THTS2)) IEC=ISV
C     Set control for quadrant in which PSI is located
      IF ((PSI.GE.THTS4).AND.(PSI.LE.THTS1)) ISEQ=1
      IF ((PSI.GT.THTS1).AND.(PSI.LE.THTS3)) ISEQ=2
      IF ((PSI.GT.THTS3).AND.(PSI.LE.THTS2)) ISEQ=3
      IF ((PSI.GT.THTS2).AND.(PSI.LT.THTS4+TWPI)) ISEQ=4
      IF ((ISEQ.LT.1).OR.(ISEQ.GT.4)) CALL PMDSTOP
C  
  
      CM=TWPI-PSI
      IF (DABS(PSI).LE.ERLT) CM=0.D0
      IF (KS.LT.NPST) GO TO 100
      IF (KS.LT.KSRF) GO TO 100
      IF (KSRF.EQ.0) KSRF=1
      KSRF=KSRF+INDX
      CSI=-DCOS(VI)
      SNI=DSIN(VI)
C     SET TO LOOK AT OPPOSED HEMISPHERE OF ECLIPSING STAR  
      AG=0.D0
      IF (ISV.EQ.IEC) AG=PI
C
C     Calculate XG,YG,ZG
C
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
      CALL PROJ(TARY,VRD,0.0D0,0.0D0,XG,YG,ZG)
      XC=QS/(1.D0+QS)
      IF (IEC.EQ.ISV) XC=QN/(1.D0+QN)
C
C     Calculate XCMS,YCMS,ZCMS
C
      CALL PROJ(TARY,XC*VRD,0.D0,0.D0,XCMS,YCMS,ZCM)
      IF (IEC.EQ.ISV) XCMS=XCMS-XG
      IF (IEC.EQ.ISV) YCMS=YCMS-YG
      XCM=YCMS
      YCM=XCMS
C     The names of the axes are reversed later in the program
C*******************************************************
C     XG,YG ZG are the coods.of the center of the eclipsing star
C     relative to the center of the eclipsed star.
C     XG and YG are the projections of the center on the plane of the
C     sky.
C
C     XCM,YCM,ZCM are the coordinates of the center of mass of the
C     system
C     and XCM,YCM are the coods. projected on the plane of the sky
C*******************************************************
      ANGLO=0.D0
      CSPJ=DCOS(ANGLO)
      SNPJ=DSIN(ANGLO)
C
C
C  
C      STAR ISV DESIGNATES THE STAR BEING ECLIPSED AT PHASE 0.0   
C     STAR ISV ALWAYS DESIGNATES THE STAR AT THE ORIGIN OF COODS. 
C     SET TO PLOT ECLIPSING STAR FIRST
C  
C     SET TO LOOK AT VISIBLE HEMISPHERE OF ECLIPSED STAR   
      AG=PI      
      IF (ISV.EQ.IEC) AG=0.0D0
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
C     Calculate XG,YG
C      CALL PROJ(TARY,VRD,0.D0,0.D0,XG,YG,ZG)
      IF ((PSI.GT.THTS1).AND.(PSI.LT.THTS2)) GO TO 1
C  
C
      I=3-ISV    
      Q=QN
      GO TO 2    
C  
C  
    1 I=ISV      
      Q=QS
	  K3CT=0
C     ICOUNT=1 ALWAYS GIVES ECLIPSING STAR   
C     ICOUNT=2 ALWAYS GIVES ECLIPSED STAR    
C  
C  
C*********************************************************************
C     Start of repeated loop for consecutive treatment of two
C     components
C*********************************************************************
C
    2 DO 101 ICOUNT=1,2
      DO 6 J=1,MTHM
      DO 5 K=1,3 
      XS(J,K)=ANG
      YS(J,K)=ANG
      XN(J,K)=ANG
      YN(J,K)=ANG
    5 CONTINUE   
    6 CONTINUE   
      DO J=1,MNKP
      XNK(J)=ANG
      YNK(J)=ANG
      END DO
	  IF (ICOUNT.EQ.1)NTS=NTL(I,NTH)
	  IF (ICOUNT.EQ.2)NTS=NTL(3-I,NTH)
      IF (ICOUNT.EQ.1) NTSA=NTS
      IF (ICOUNT.EQ.2) NTSB=NTS
C  
C  
C********************************************************************  
C     This section produces projections for all points on both
C     components
C
      DO ITH=1,NTH
	  NN=NPH(I,ITH)
	  DO K=1,NN
	  X=VR(I,ITH,K)*ALM(I,ITH,K)
	  Y=VR(I,ITH,K)*AMU(I,ITH,K)
	  Z=VR(I,ITH,K)*ANU(I,ITH,K)
	  IF (I.NE.ISV) THEN
C		X=X-VRD
		XCMV=XC*VRD
	  ELSE
		XCMV=-(1.D0-XC)*VRD
	  END IF
	  XSV(ITH,K)=X-XCMV
	  YSV(ITH,K)=Y
	  ZSV(ITH,K)=Z
	  IF ((ITH.EQ.1).OR.(ITH.EQ.NTH)) THEN
		KN=NPH(I,NTH/2+1)
		DO KK=1,KN
			XSV(ITH,KK)=X-XCMV
			YSV(ITH,KK)=Y
			ZSV(ITH,KK)=Z
		END DO
	  END IF
	  END DO
	  END DO
C     
C
      IF (KS.LE.1) THEN
	  IF (ICOUNT.EQ.1) THEN
	  NN=NPH(I,NTH/2+1)
	  WRITE (NOUT22,106) NTH,NN
	  DO ITH=1,NTH
		NN=NPH(I,ITH)
		DO K=1,NN
			IF (CSG(I,ITH,K).GE.0.D0) THEN
			WRITE (NOUT22,195) XSV(ITH,K),YSV(ITH,K),ZSV(ITH,K)
			END IF
C     Expand out to common number of longitude points for each latitude 
C     circle
			IF ((ITH.EQ.1).OR.(ITH.EQ.NTH)) THEN
				KN=NPH(I,NTH/2+1)
				DO KL=1,KN-1
				  WRITE (NOUT22,195) XSV(ITH,K),YSV(ITH,K),ZSV(ITH,K)
				END DO
			END IF
		END DO
	  END DO
	  END IF
C
	  IF (ICOUNT.EQ.2) THEN
	  NN=NPH(I,NTH/2+1)
	  WRITE (NOUT23,106) NTH,NN
	  DO ITH=1,NTH
		NN=NPH(I,ITH)
		DO K=1,NN
			WRITE (NOUT23,195) XSV(ITH,K),YSV(ITH,K),ZSV(ITH,K)
			IF ((ITH.EQ.1).OR.(ITH.EQ.NTH)) THEN
				KN=NPH(I,NTH/2+1)
				DO KL=1,KN-1
				  WRITE (NOUT23,195) XSV(ITH,K),YSV(ITH,K),ZSV(ITH,K)
				END DO
			END IF
		END DO
	  END DO
	  END IF
	  END IF
C
C
C     Set counter for total number of points on a component
      KK=0
C     Set counter for number of visible points. This is used for debug.
      KKV=0
C     Set counter for number of hidden points. This is for debug.
      KKH=0
C
      DO 50 ITH=1,NTH
      NN=NPH(I,ITH)
      DO K=1,NN
	  VPHS=PH(I,ITH,K)
      IF ((ICOUNT.EQ.2).AND.(LCT(5).EQ.1)) GO TO 39 
      IF ((ICOUNT.EQ.1).AND.(LCT(13).EQ.1)) GO TO 39
      N=1
      IF ((VPHS.GE.VST(I,ITH,N)).AND.(VPHS.LE.VND(I,ITH,N))) GO TO   
     $31  
      N=2
      IF ((VPHS.GE.VST(I,ITH,N)).AND.(VPHS.LE.VND(I,ITH,N))) GO TO   
     $31  
      N=3
      IF ((VPHS.GE.VST(I,ITH,N)).AND.(VPHS.LE.VND(I,ITH,N))) GO TO   
     $31  
C     IF (ICOUNT.EQ.1),IT IS THE ECLIPSING STAR     
      IF (ICOUNT.EQ.1) GO TO 32
   34 QX(KK)=1000.0D0
      QY(KK)=1000.0D0
	  KKH=KKH+1
      GO TO 40   
   31 IF (ICOUNT.EQ.1) GO TO 390
C     IF WE ARRIVE HERE,WE HAVE THE ECLIPSED STAR   
      IF (LCT( 8).EQ.1) GO TO 39      
C     Plot the visible points,so include only the points not in shadow						
C     We have already eliminated the points below the horizon.
      IF (ITH.NE.NTH) GO TO 33
      IF ((SHS(ITH-1,1).NE.ANG).AND.(DABS(SHS(ITH,1)-ANG).LE.ERLT)) 
     $GO TO 40
   33 IF (VPHS.LT.SHS(ITH,1)) GO TO 39
      IF ((VPHS.GT.SHN(ITH,1)).AND.(VPHS.LT.SHS(ITH,2))) GO TO 39
      IF ((VPHS.GT.SHN(ITH,2)).AND.(VPHS.LT.SHS(ITH,3))) GO TO 39
      IF (VPHS.GT.SHN(ITH,3)) GO TO 39
      GO TO 34
   32 PX(KK)=1000.0D0
      PY(KK)=1000.0D0
	  KKH=KKH+1
      GO TO 40   
C     Check for neck region.If not in neck,include.
  390 IF ((DABS(VPHS).LE.ERLT).AND.(DABS(VST(I,ITH,3)-ANG).LT.1.D-9))
     $GO TO 39
      RG=VND(I,ITH,N)-VST(I,ITH,N)
      IF ((RG.LT.PIO2).AND.(N.EQ.1)) GO TO 32
   39 X=VR(I,ITH,K)*ALM(I,ITH,K)    
      Y=VR(I,ITH,K)*AMU(I,ITH,K)   
      Z=VR(I,ITH,K)*ANU(I,ITH,K)
      IF (I.EQ.ISV) GO TO 66
C     Displace points for case I.NE.ISV
      X=X-VRD
C                                     !VRD=separ. of centers
   66 IF (ICOUNT.EQ.2) GO TO 38
      CALL PROJ(TARY,X,Y,Z,PX(KK),PY(KK),PZ(KK))     !eclipsing star
      KKV=KKV+1
      GO TO 40   
   38 CALL PROJ(TARY,X,Y,Z,QX(KK),QY(KK),QZ(KK))     !eclipsed star
      KKV=KKV+1
   40 KK=KK+1
      END DO
   50 CONTINUE
C**********************************************************************
C
      IF (ICOUNT.EQ.2) GO TO 300               !ICOUNT=2,eclipsed star
C  
C     The following two sections do the actual plotting
C     THE FOLLOWING SECTION IS FOR THE ECLIPSING STAR      
C  
      DO 65 K=1,NTSA    
      IF ((LCT(9).NE.1).AND.(LCT(13).NE.1)) GO TO 65
      XP=PX(K)   
      YP=PY(K)
	  ZP=PZ(K)
C     NOTE THAT THE POSITIVE Y AXIS ON THE PLOTTER CORRESPONDS WITH      
C     THE NEGATIVE X AXIS FOR PHASE ANGLE ZERO(IN THE ORBIT)      
      X=XP
      Y=YP
C     ELIMINATE POINTS THAT HAVE BEEN FLAGGED NOT TO PLOT  
      IF (DABS(X-1000.0D0).LE.ERLT) GO TO 65
      IF (LCT(15).EQ.1) GO TO 71      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
	  Z=ZP
      GO TO 67   
   71 CONTINUE   
      X=XP
      Y=YP
	  Z=ZP
   67 CONTINUE
      WRITE (NOUT18, 193) X-XCM,-Y+YCM,Z-ZCM,K
	  K2CT=K2CT+1
   65 CONTINUE
      GO TO 301  
C  
C     THE FOLLOWING SECTION IS FOR THE ECLIPSED STAR
C  
  300 DO 64 K=1,NTSB    
      IF ((LCT(1).NE.1).AND.(LCT(5).NE.1)) GO TO 64 
      XP=QX(K)   
      YP=QY(K)
	  ZP=QZ(K)
      X=XP
      Y=YP
	  Z=ZP
C     ELIMINATE POINTS THAT HAVE BEEN FLAGGED NOT TO PLOT  
      IF (DABS(X-1000.0D0)-ERLT.LE.0.D0) GO TO 64
      IF (LCT(15).EQ.1) GO TO 72      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
	  Z=ZP
      GO TO 68   
   72 CONTINUE   
      X=XP
      Y=YP
	  Z=ZP
   68 CONTINUE
      WRITE (NOUT18, 193) X-XCM, -Y+YCM, Z-ZCM, K
	  K2CT=K2CT+1
   64 CONTINUE
  301 IF (LCT(16).NE.1) GO TO 501
      CALL PLSPT(LCT,NSPA,NSPB,I,VBTDA,VLMDA,VGMDA,VBTDB,VLMDB,
     $VGMDB,ISV,TARY,NTH,
     $CSPJ,SNPJ,VRD,ICOUNT,XCM,YCM,NOUT6,NOUT7,NOUT8,NOUT9)
C     Write out all points for eclipsing component, for use by ACPGCRG
C     Note that the output is the actual spatial coods., not their
C     projections on the plane of the sky.
	  IF (ICOUNT.EQ.1) THEN
		KKK=0
		DO ITH=1,NTH
			N=NPH(I,ITH)
			DO K=1,N
				X=VR(I,ITH,K)*ALM(I,ITH,K)
				Y=VR(I,ITH,K)*AMU(I,ITH,K)
				Z=VR(I,ITH,K)*ANU(I,ITH,K)
				IF (I.NE.ISV) THEN
					X=X-VRD
					XCMV=-(1.D0-XC)*VRD
				ELSE
					XCMV=XC*VRD
				END IF
				KKK=KKK+1
C     Note that the output coods. are the center of mass coods.of the
C     photospheric points on the eclipsed star.
C     The cood. system is the true coods. in the system 
C     rotating with the components, with the X-axis joining the centers
C     of the components.
				WRITE (NOUT17,193) X-XCMV,Y,Z,KKK
			END DO
		END DO
		REWIND NOUT17
C     Save the total number of points written to unit 17
		KLM=KKK
C     Write the number of output points as the first record on unit 16
		WRITE (NOUT16,106) KLM
C     Copy the output points from unit 17 to unit 16.
		DO JKL=1,KLM
			READ (NOUT17,193) AA,BB,CC,KKK
			WRITE (NOUT16,193) AA,BB,CC,KKK
		END DO
	  END IF
C     Write out all points for eclipsed component, for use by ACPGCRG
	  IF (ICOUNT.EQ.2) THEN
		KKK=0
		DO ITH=1,NTH
			N=NPH(I,ITH)
			DO K=1,N
				X=VR(I,ITH,K)*ALM(I,ITH,K)
				Y=VR(I,ITH,K)*AMU(I,ITH,K)
				Z=VR(I,ITH,K)*ANU(I,ITH,K)
				IF (I.NE.ISV) THEN
					X=X-VRD
					XCMV=-(1.D0-XC)*VRD
				ELSE
					XCMV=XC*VRD
				END IF
C     Only write points to output that are visible to the observer.
				IF (CSG(I,ITH,K).GE.0.D0) THEN
					KKK=KKK+1
C     Note that the output coods. are the center of mass coods.of the
C     photospheric points on the eclipsed star that are visible to the
C     observer. The cood. system is the true coods. in the system 
C     rotating with the components, with the X-axis joining the centers
C     of the components.
					WRITE (NOUT20,193) X-XCMV,Y,Z,K
				END IF
			END DO
		END DO
		REWIND NOUT20
C     Save the total number of visible points written to unit 20.
		KLM=KKK
C     Write the number of output points as the first record on unit 19.
		WRITE (NOUT19,106) KLM
C     Copy the output points from unit 20 to unit 19.
		DO JKL=1,KLM
			READ (NOUT20,193) AA,BB,CC,KKK
			WRITE (NOUT19,193) AA,BB,CC,KKK
		END DO
	  END IF
C*********************************************************************
C
C     PLOT BOUNDARY PROFILE,AS PROVIDED BY XS,YS AND XN,YN POINTS
C     THIS INCLUDES THE CENTRAL MERIDIAN POINTS
C*********************************************************************
C
  501 CALL SHAD1(I,NTH,  
     $TARY,XBGEQ,YBGEQ,XNDEQ,YNDEQ,ISEQ,
     $NPB,NPE,NKP,XG,YG,KYA,KYB,ISV,IEC,IRD,ICOUNT,VRD,NOUT1)
C
      DO 240 J=1,NTH
C     ICOUNT=1, eclipsing star
C     ICOUNT=2, eclipsed star
      IF ((ICOUNT.EQ.1).AND.(LCT(14).NE.1)) GO TO 240
      IF ((ICOUNT.EQ.2).AND.(LCT(6).NE.1)) GO TO 240
      DO 239 K=1,3      
      IF (DABS(XS(J,K)-ANG).LE.ERLT) GO TO 239
      XP=XS(J,K) 
      YP=YS(J,K) 
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 73      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 237  
   73 CONTINUE   
      X=XP
      Y=YP
  237 CONTINUE
      WRITE (NOUT21, 193) X-XCM, -Y+YCM ,ZCM, 0
C	K2CT=K2CT+1
      XP=XN(J,K) 
      YP=YN(J,K) 
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 74      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 238  
   74 CONTINUE   
      X=XP
      Y=YP
  238 CONTINUE
      WRITE (NOUT21, 193) X-XCM, -Y+YCM  ,ZCM, 0
C	  K2CT=K2CT+1
  239 CONTINUE   
  240 CONTINUE
	IF (ICOUNT.EQ.1) THEN
	OPEN (UNIT=35,FILE='SCRATCHX.DAT',STATUS='REPLACE')
	DO ITH=1,NTH
	WRITE(35,1000) ITH,XHZB(ITH),YHZB(ITH)
	END DO
	WRITE (35,1001)
	DO ITH=1,NTH
	WRITE(35,1000) ITH,XHZE(ITH),YHZE(ITH)	
	END DO
	WRITE (35,1001)
	WRITE (35,1000) 0,XBGEQ,YBGEQ,XNDEQ,YNDEQ
	CLOSE (35,STATUS='KEEP')
	END IF
	IF (ICOUNT.EQ.2) THEN
	OPEN (UNIT=36,FILE='SCRATCHY.DAT',STATUS='REPLACE')
	DO ITH=1,NTH
	WRITE(35,1000) ITH,XHZB(ITH),YHZB(ITH)
	END DO
	WRITE (35,1001)
	DO ITH=1,NTH
	WRITE(35,1000) ITH,XHZE(ITH),YHZE(ITH)	
	END DO
	WRITE (35,1001)
	WRITE (35,1000) 0,XBGEQ,YBGEQ,XNDEQ,YNDEQ
	CLOSE (36,STATUS='KEEP')
	END IF
 1000 FORMAT (I5,6F12.8)
 1001 FORMAT (' ')
C
C**********************************************************************
C     Prepare to locate intersections of boundaries
C
      CALL HORZ(ISEQ,IRC,IFC,NPB,NPE,NKP,
     $I,XBGEQ,YBGEQ,
     $XNDEQ,YNDEQ,IEC,XG,YG,ISV,IRD,ICOUNT,KCOR,NOUT1)
C
C  
C     ARYB GOES WITH THE SMALL THETA BRANCH,ARRAYS RS(2),ALP(2),
C     AND INDEX IRC     
C     ARYA GOES WITH THE LARGE THETA BRANCH,ARRAYS RS(1),ALP(1),
C     AND INDEX IFC
C  
C  
C  
C     LOCATE INTERSECTIONS OF COMPONENT BOUNDARIES,IF THERE ARE ANY      
C  
C     Initialize limiting angles for boundaries of visible horizon
      AGLMT1=ANG 
      AGLMT2=ANG
      IF (I.NE.IEC) GO TO 43           !IEC=the eclipsing star
C     SAVE DATA FOR ECLIPSING STAR    
C     these data are saved the first time through the long loop
C     starting with statement 2,for ICOUNT=1
      IRCS=IRC
      IFCS=IFC
      DO 41 J=1,MTHM
      DO 42 JJ=1,3     
      ARYBS(J,JJ)=ARYB(J,JJ)  
      ARYAS(J,JJ)=ARYA(J,JJ)
   42 CONTINUE
      DO 22 JJ=1,3
      ALPS(J,JJ)=ALP(J,JJ)
      RSS(J,JJ)=RS(J,JJ)
   22 CONTINUE
   41 CONTINUE
      JLM=NKP-1
      DO 58 J=1,JLM
      DO 57 JJ=1,3
      ARYCS(J,JJ)=ARYC(J,JJ)
   57 CONTINUE
   58 CONTINUE
      IFCS=IFC
      IRCS=IRC
      NKPS=NKP
      GO TO 46
C
C     Arrive here for ICOUNT=2.I=eclipsed star.
C     ALPS(K,1) GOES WITH THE LARGE THETA BRANCH OF THE ECLIPSING STAR
C     ALPS(K,2) GOES WITH THE SMALL THETA BRANCH OF THE ECLIPSING STAR
C     The large theta branch is on the (downward) positive direction of
C     the X axis.
C     The array ALP(K,1) measures angles from the projected equatorial
C     point (on the eclipsed star) that is most remote from the
C     companion.
C     The positive direction for angle measurement is CCW.
C     DO LARGE THETA BRANCH FIRST,FOR THE ECLIPSED STAR 
   43 IF ((VI.LT.0.8D0).AND.(NKP.EQ.0)) GO TO 46  
      AGL1=ALP(1,1)
      AGL2=ALP(IFC,1)
      RG=(AGL2-AGL1)/(NPT-1)
C     FILL ARRAY ERR,PREPARATORY TO SEARCH FOR INTERSECTIONS OF
C     BOUNDARIES
      DO 45 J=1,NPT 
C     ANGLE always is measured on the eclipsed star
      ANGLE=AGL1+(J-1)*RG      
      AGLV(J)=ANGLE     
C     IF ERR(J) IS NEGATIVE,THE POINT IN QUESTION LIES INSIDE THE 
C     BOUNDARY OF THE STAR AT THE ORIGIN     
C     NOTE THAT ANGLE IS ALWAYS MEASURED ON THE STAR WHOSE CENTER
C     IS AT YG,XG
   44 CALL RANGE(ANGLE,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERV(J),IRD)     
   45 CONTINUE
C     Set reference indices which eventually will mark the angle
C     pairs that bracket the intersection points.
      J1=0
      J2=0
      J3=0
      J4=0
C     Set counter for number of large theta roots
      ICTLG=0
C     SEARCH FOR FIRST ROOT ON THE LARGE THETA BRANCH OF THE STAR BEING
C     ECLIPSED     
      DO 47 J=2,NPT     
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 48
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 48
   47 CONTINUE   
C     NO ROOT ON THE LARGE THETA BRANCH      
      AGRTS=AGLV(NPT)
      GO TO 90   
C     ON ARRIVING HERE WE HAVE BRACKETED THE FIRST ROOT    
   48 J1=J-1     
      J2=J
      ICTLG=ICTLG+1
      X1=AGLV(J1)
      X2=AGLV(J2)
      AGRTS=X2
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      Y1=ERV(J1) 
      Y2=ERV(J2)
C     LOCATE FIRST ROOT ON THE LARGE THETA BRANCH   
      DO 120 KCT=1,4    
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  127 CALL RANGE(AGRT,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD)
  128 IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 123
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 124
      CALL PMDSTOP      
  123 IF (ERVL.GE.0.0D0) GO TO 119
      GO TO 118  
  124 IF (ERVL.LT.0.0D0) GO TO 119
  118 X2=AGRT    
      Y2=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 117
      GO TO 120  
  119 X1=AGRT    
      Y1=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 117
  120 CONTINUE   
C     AGLMT1 IS THE FIRST ROOT OF ERV,AGLMT2 IS THE SECOND 
C     Set AGLMT1
  117 AGLMT1=AGRT
      AGLRTA=AGRT+1.0D-9
C     ON ARRIVING HERE WE HAVE LOCATED EITHER ONE OR NO ROOTS ON THE      
C     LARGE THETA BRANCH
C     IF WE HAVE LOCATED ONLY ONE ROOT,IT IS AGLMT1
C     SEARCH FOR ROOT ON THE SMALL THETA BRANCH    
   90 AGL1=ALP(1,2)  
      AGL2=ALP(IRC,2) 
      RG=(AGL2-AGL1)/(NPT-1)
C     Fill array ERV,preparatory to root search.
      DO 91 J=1,NPT     
      ANGLE=AGL1+(J-1)*RG      
      AGLV(J)=ANGLE     
   95 CALL RANGE(ANGLE,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERV(J),IRD)     
   91 CONTINUE
C  
C  
C     SET UP TO SEARCH FOR POSSIBLE ROOTS ON THE SMALL THETA BRANCH      
C  
      J1=0
      J2=0
C     Set counter for number of small theta roots
      ICTSM=0
C     SEARCH FOR FIRST ROOT ON THE SMALL THETA BRANCH      
      DO 130 J=2,NPT    
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 131
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 131
  130 CONTINUE   
C     NO ROOT ON THE SMALL THETA BRANCH,AND EITHER ONE OR NO ROOTS
C     ON THE LARGE THETA BRANCH
      AGRTSA=AGLV(NPT)
C     If no root on the large theta branch, AGLMT1.EQ.ANG
      IF (DABS(AGLMT1-ANG).LE.ERLT) GO TO 140
      GO TO 149  
  131 J1=J-1     
      J2=J
      ICTSM=ICTSM+1
      X1=AGLV(J1)
      X2=AGLV(J2)
      AGRTSA=X2
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      Y1=ERV(J1) 
      Y2=ERV(J2)
C     LOCATE FIRST ROOT ON THE SMALL THETA BRANCH   
      DO 133 KCT=1,4    
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  151 CALL RANGE(AGRT,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  152 CONTINUE
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 141
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 142
      CALL PMDSTOP      
  141 IF (ERVL.GE.0.0D0) GO TO 138
      GO TO 132  
  142 IF (ERVL.LT.0.0D0) GO TO 138
      GO TO 132  
  138 X2=AGRT    
      Y2=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 139
      GO TO 133  
  132 X1=AGRT    
      Y1=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 139
  133 CONTINUE   
C     May have one root already located.
C     If so,AGLMT1.NE.ANG
C
  139 CONTINUE
      AGLMT2=AGRT
      AGRTSA=AGRT-1.0D-9
      IF ((AGLMT1.NE.ANG).AND.(AGLMT2.NE.ANG)) GO TO 150
C     SEARCH FOR SECOND ROOT ON THE LARGE THETA BRANCH     
  149 AGL1=AGRTS
      AGL2=ALP(IFC,1)
      RG=(AGL2-AGL1)/(NPT-1)
      IF (DABS(RG).LE.ERLT) GO TO 187
      DO 158 J=1,NPT
      ANGLE=AGL1+(J-1)*RG
      AGLV(J)=ANGLE
  159 CALL RANGE(ANGLE,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERV(J),IRD)
  158 CONTINUE
      DO 49 J=2,NPT
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 56
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 56
   49 CONTINUE   
C     NO SECOND ROOT ON THE LARGE THETA BRANCH
      GO TO 187
C     ON ARRIVING HERE,WE HAVE BRACKETED THE SECOND ROOT   
C     ON THE LARGE THETA BRANCH
   56 J3=J-1     
      J4=J
      ICTLG=ICTLG+1
      X1=AGLV(J3)
      X2=AGLV(J4)
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      Y1=ERV(J3) 
      Y2=ERV(J4)
C     LOCATE SECOND ROOT ON THE LARGE THETA BRANCH  
      DO 122 KCT=1,3    
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  147 CALL RANGE(AGRT,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  148 CONTINUE
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 125
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 126
      CALL PMDSTOP      
  125 IF (ERVL.GE.0.0D0) GO TO 121
      GO TO 116  
  126 IF (ERVL.LT.0.0D0) GO TO 121
  116 X2=AGRT    
      Y2=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 115
      GO TO 122  
  121 X1=AGRT    
      Y1=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 115
  122 CONTINUE   
C     Set  second root.
  115 CONTINUE
      AGLMT2=AGRT
C     Have located both roots.
      GO TO 150
C     Seek second root on the small theta branch
  187 AGL1=AGRTSA
      AGL2=ALP(IRC,2)
      RG=(AGL2-AGL1)/(NPT-1)
      IF (DABS(RG).LE.ERLT) GO TO 160
      DO 189 J=1,NPT
      ANGLE=AGL1+(J-1)*RG
      AGLV(J)=ANGLE
  188 CALL RANGE(ANGLE,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERV(J),IRD)
  189 CONTINUE
C     SEARCH FOR SECOND ROOT ON THE SMALL THETA BRANCH     
      DO 134 J=2,NPT   
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 135
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 135
  134 CONTINUE   
C     NO SECOND ROOT ON SMALL THETA BRANCH   
C     THERE MAY BE A ROOT ON THE NECK REGION
      GO TO 160
C     LOCATE SECOND ROOT ON SMALL THETA BRANCH      
  135 J3=J-1     
      J4=J
      ICTSM=ICTSM+1
      X1=AGLV(J3)
      X2=AGLV(J4)
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      Y1=ERV(J3) 
      Y2=ERV(J4)
C     LOCATE SECOND ROOT ON THE SMALL THETA BRANCH  
      DO 137 KCT=1,3
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  153 CALL RANGE(AGRT,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  154 CONTINUE
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 144
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 145
      CALL PMDSTOP      
  144 IF (ERVL.GE.0.0D0) GO TO 146
      GO TO 136  
  145 IF (ERVL.LT.0.0D0) GO TO 146
      GO TO 136  
  146 X2=AGRT    
      Y2=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 143
      GO TO 137  
  136 X1=AGRT    
      Y1=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 143
  137 CONTINUE   
  143 IF (DABS(PI-PSI).GT.0.05D0) GO TO 110
      IF (DABS(AGLMT2-ANG).LE.ERLT) GO TO 110
      IF (AGLMT2.LE.0.0D0) GO TO 110
      IF (AGRT.LT.0.0D0) AGRT=AGRT+TWPI
  110 IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(AGLMT2.NE.ANG)) AGLMT1=AGRT
      IF ((AGLMT1.NE.ANG).AND.(DABS(AGLMT2-ANG).LE.ERLT)) AGLMT2=AGRT
C     Both roots have been found
      GO TO 150  
C     On arriving here,either one or no root has been found on the
C     large theta branch,and no root on the small theta branch.
C  
C     Check the neck region for possible roots
C     Make check only if there is a possible intersection
  160 IF ((ICTLG.NE.0).OR.(ICTSM.NE.0)) GO TO 185
      GO  TO 140
  185 AGL1=ALP(1,3)
      AGL2=ALP(NKP,3)
      RG=(AGL2-AGL1)/MNKP-1
C     Fill array ERV
      DO 162 J=1,MNKP
      ANGLE=AGL1+(J-1)*RG
      AGLV(J)=ANGLE
  161 CALL RANGE(ANGLE,3,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERV(J),IRD)
  162 CONTINUE
      J1=0
      J2=0
      J3=0
      J4=0
C     Search for possible roots on the neck
      DO 163 J=2,MNKP
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 164
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 164
  163 CONTINUE
C     No root on the neck
      GO TO 140
  164 J1=J-1
      J2=J
      X1=AGLV(J1)
      X2=AGLV(J2)
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      Y1=ERV(J1)
      Y2=ERV(J2)
C     Locate first root on neck
      DO  171 KCT=1,3
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  165 CALL RANGE(AGRT,3,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD)
  166 CONTINUE
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 167
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 168
  167 IF (ERVL.GE.0.0D0) GO TO 169
      GO TO 170
  168 IF (ERVL.LT.0.0D0) GO TO 169
      GO TO 170
  169 X2=AGRT
      Y2=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 172
      GO TO 171
  170 X1=AGRT
      Y1=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 172
  171 CONTINUE
C     May have one root already located
C     If so,AGLMT1.NE.ANG
C
  172 IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(AGLMT2.NE.ANG)) AGLMT1=AGRT
      IF ((AGLMT1.NE.ANG).AND.(DABS(AGLMT2-ANG).LE.ERLT)) AGLMT2=AGRT
C     Test whether both roots have been found
      IF ((AGLMT1.NE.ANG).AND.(AGLMT2.NE.ANG)) GO TO 150
      AGLMT1=AGRT
C     Search for second root on neck
      J2=J2+1
      DO 175 J=J2,MNKP
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 176
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 176
  175 CONTINUE
C     No second root on neck
      GO TO 150
C     Locate second root on neck
  176 J3=J-1
      J4=J
      X1=AGLV(J3)
      X2=AGLV(J4)
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      Y1=ERV(J3)
      Y2=ERV(J4)
C     Locate second root
      DO 183 KCT=1,3
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  177 CALL RANGE(AGRT,3,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD)
  178 CONTINUE
      IF ((ERV(J).GE.0.0D0).AND.(ERV(J-1).LT.0.0D0)) GO TO 179
      IF ((ERV(J).LT.0.0D0).AND.(ERV(J-1).GE.0.0D0)) GO TO 180
      CALL PMDSTOP
  179 IF (ERVL.GE.0.0D0) GO TO 181
      GO TO 182
  180 IF (ERVL.LT.0.0D0) GO TO 181
      GO TO 182
  181 X2=AGRT
      Y2=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 184
      GO TO 183
  182 X1=AGRT
      Y1=ERVL
      IF (DABS(ERVL).LE.ERLT) GO TO 184
  183 CONTINUE
  184 AGLMT2=AGRT
      GO TO 150
C     If we arrive here,there is no overlap
C     Check for no eclipse vs. total or annular eclipse
C     IF THE VALUES IN ARRAY ERV ARE POSITIVE,THERE IS NO OVERLAP OF
C     THE TWO COMPONENT BOUNDARIES
C     Separate the two cases by comparing the separation of centers
C     with the smaller of the two component radii
  140 DST=DSQRT(YG**2+XG**2)
      RDC=VR(IEC,1,1)
      RDCA=VR(ISV,1,1)
      RDCTS=RDC
      IF (RDC.LT.RDCA) RDCTS=RDCA
      IF (DST.GT.RDCTS) GO TO 150
C     THE FOLLOWING IS ANNULAR OR TOTAL PHASE
C     If annular,plot horizon points
      RDTS=VR(I,1,1)
      IF (RDTS.GT.RDC) GO TO 150
      AGLMT1=-ANG
      AGLMT2=-ANG
C
C     Both roots have been found
C
  150 CONTINUE
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $GO TO 157
      IF ((AGLMT1.NE.ANG).AND.(AGLMT2.NE.ANG)) GO TO 174
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(AGLMT2.NE.ANG)) go to 173
      IF ((AGLMT1.NE.ANG).AND.(DABS(AGLMT2-ANG).LE.ERLT)) CALL PMDSTOP
  173 AGLMT1=ALP(IFC,1)
C     SET ROOT SEQUENCE FOR PLOTTING
  174 IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 155
      IF ((AGLMT1.LE.0.0D0).AND.(AGLMT2.LE.0.0D0)) GO TO 156
  155 IF (AGLMT2.GE.AGLMT1) GO TO 157 
      SV=AGLMT2  
      AGLMT2=AGLMT1     
      AGLMT1=SV
      GO TO 157  
  156 IF (AGLMT2.LE.AGLMT1) GO TO 157 
      SV=AGLMT2  
      AGLMT2=AGLMT1     
      AGLMT1=SV
  157 CONTINUE   
C     Save the position  angles of intersections
      AGLSV1=AGLMT1
      AGLSV2=AGLMT2
   46 CONTINUE
      CALL PSMH(IRC,ICOUNT,K3CT,LCT,XP,YP,AGLMT1,AGLMT2,
     $IEC,IRD,ISV,XG,YG,CSPJ,SNPJ,XCM,YCM,XNKSV1,YNKSV1,NOUT1,NOUT21)
C
      CALL PLGH(IFC,ICOUNT,K3CT,LCT,XP,YP,CSPJ,SNPJ,AGLMT1,
     $AGLMT2,IRD,XG,YG,XCM,YCM,XNKSV2,YNKSV2,IEC,ISV,NOUT1,NOUT21)
C
      CALL PNK(NKP,ICOUNT,K3CT,LCT,XP,YP,CSPJ,SNPJ,XCM,YCM,
     $NOUT1,NOUT21)
C
      CALL PSHB(ICOUNT,K3CT,LCT,NTH,TARY,XP,YP,ZP,CSPJ,SNPJ,XCM,
     $YCM,NOUT1,NOUT21)
C
C*********************************************************************
C     PLOT PROJECTED CENTER OF STAR I.NE.ISV
C*********************************************************************
C
C     NOTE THAT DISPLACEMENT OF BOUNDARY OF STAR HAS BEEN TAKEN CARE OF  
C     IN SRT SHAD1
      XP=XG      
      YP=YG      
      IF (LCT(15).EQ.1) GO TO 87      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
C     SET COODS.SO THAT X-AXIS DIRECTION IS CORRECT FOR STAR 3-ISV
      IF (ISV.NE.IEC) GO TO 88 
      X=-X
      Y=-Y
      GO TO 88   
   87 Y=YP
      X=XP
      IF (ISV.NE.IEC) GO TO 88 
      X=-X
      Y=-Y
   88 CONTINUE
      WRITE (NOUT5, 193) X-XCM, -Y+YCM
C*********************************************************************
C     PLOT PROJECTED CENTER OF STAR I.EQ.ISV
C*********************************************************************
      X=0.D0
      Y=0.D0
      WRITE (NOUT5, 193) X-XCM, -Y+YCM
C
C
      CALL DOUT1(AGLMT1,I,IEC,IFC,ISV,AGLMT2,
     $IRC,NKP,XG,YG,ICOUNT,LCT,XCM,YCM,
     $CSPJ,ISEQ,SNPJ,KS,NOUT1,NOUT4,NOUT12)

C
      CALL DOUT2(AGLMT1,I,IEC,IFC,ISV,AGLMT2,
     $IRC,NKP,XG,YG,ICOUNT,LCT,XCM,YCM,XNKSV1,YNKSV1,
     $XNKSV2,YNKSV2,CSPJ,ISEQ,SNPJ,AGL2,KS,NOUT1,NOUT10,NOUT13)
C
      CALL DOUT3(AGLMT1,I,IEC,IFC,ISV,AGLMT2,
     $IRC,NKP,XG,YG,ICOUNT,LCT,XCM,YCM,
     $AGLSV2,CSPJ,NPT,ISEQ,SNPJ,KS,NOUT1,NOUT11,NOUT14)
C
      CALL DOUT4(AGLMT1,I,IEC,IFC,ISV,AGLMT2,
     $IRC,NKP,XG,YG,ICOUNT,LCT,XCM,YCM,
     $CSPJ,ISEQ,SNPJ,KS,NOUT1,NOUT15)
C
C            
   98 PSI=PSI+PI
      IF (PSI.GT.TWPI) PSI=PSI-TWPI   
      CM=TWPI-PSI
      IF (CM.LT.0.0D0) CM=CM+TWPI
      IF (CM.GE.TWPI) CM=0.0D0
      IF (PSI.GE.TWPI) PSI=0.0D0
      I=3-I      
      AG=0.D0
      IF (ISV.EQ.IEC) AG=PI
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)  
      IF ((PSI.GE.THTS1).AND.(PSI.LE.THTS2)) GO TO 102
      Q=QN
      GO TO 103  
  102 Q=QS
  103 IF (ICOUNT.EQ.2) THEN
	  IF (I.EQ.ISV) THEN
C     0If I.EQ.ISV, the current object star is the star eclipsed at
C     orbital phase 0.0, and XC measures the distance from it to the
C     center of mass, along the line of centers.
		DLX=XC*VRD
	  ELSE
		DLX=1.D0-XC*VRD
	  END IF
	  N=NPH(3-I,NTH/2+1)
      WRITE (NOUT21,106) NTH, N
	  DO ITH=1,NTH
		N=NPH(3-I,ITH)
		DO K=1,N
			X=VR(3-I,ITH,K)*ALM(3-I,ITH,K)
			Y=VR(3-I,ITH,K)*AMU(3-I,ITH,K)
			Z=VR(3-I,ITH,K)*ANU(3-I,ITH,K)
			CALL PROJ(TARY,DLX-X,-Y,Z,XP,YP,ZP)
			WRITE (NOUT21,112) YP,XP,ZP
		END DO
	  END DO
	  END IF
C
  101 CONTINUE
      REWIND NOUT18
	  WRITE (NOUT2,106) K2CT
	  DO KK=1,K2CT
		READ (NOUT18,193) AA,BB,CC,KPT
		WRITE (NOUT2,193) AA,BB,CC,KPT
	  END DO
	  REWIND NOUT18
      CALL WTOUT8(KS,IEC,ISV,IRC,IFC,NKP,IRCS,IFCS,NKPS,
     $AGLMT1,AGLMT2,XG,YG,XCM,
     $YCM,ISEQ,NOUT3)
C
C
  100 CONTINUE
C*********************************************************************
      GO TO 99
  199 CONTINUE
      GO TO 99   
  299 CALL PMDSTOP
  399 CALL PMDSTOP
   99 CONTINUE
  499 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
C      WRITE (NOUT1, 255) TM
      CLOSE (UNIT=  IN1, STATUS='KEEP')
      CLOSE (UNIT=  IN2, STATUS='KEEP')
      CLOSE (UNIT=  IN3, STATUS='KEEP')
      CLOSE (UNIT=  IN4, STATUS='KEEP')
      CLOSE (UNIT=  IN5, STATUS='KEEP')
      CLOSE (UNIT=  IN6, STATUS='KEEP')
      CLOSE (UNIT=  IN7, STATUS='KEEP')
      CLOSE (UNIT=  IN8, STATUS='KEEP')
      CLOSE (UNIT=  IN9, STATUS='KEEP')
      CLOSE (UNIT= IN10, STATUS='KEEP')
	  CLOSE (UNIT= IN11, STATUS='KEEP')
	  CLOSE (UNIT= IN12, STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=   43, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
	  CLOSE (UNIT=NOUT6, STATUS='KEEP')
	  CLOSE (UNIT=NOUT7, STATUS='KEEP')
	  CLOSE (UNIT=NOUT8, STATUS='KEEP')
	  CLOSE (UNIT=NOUT9, STATUS='KEEP')
	  CLOSE (UNIT=NOUT10, STATUS='KEEP')
	  CLOSE (UNIT=NOUT11, STATUS='KEEP')
	  CLOSE (UNIT=NOUT12, STATUS='KEEP')
	  CLOSE (UNIT=NOUT13,STATUS='KEEP')
	  CLOSE (UNIT=NOUT14,STATUS='KEEP')
	  CLOSE (UNIT=NOUT15,STATUS='KEEP')
	  CLOSE (UNIT=NOUT16,STATUS='KEEP')
	  CLOSE (UNIT=NOUT17,STATUS='KEEP')
	  CLOSE (UNIT=NOUT18,STATUS='KEEP')
	  CLOSE (UNIT=NOUT19,STATUS='KEEP')
	  CLOSE (UNIT=NOUT20,STATUS='KEEP')
	  CLOSE (UNIT=NOUT21,STATUS='KEEP')
	  CLOSE (UNIT=NOUT22,STATUS='KEEP')
	  CLOSE (UNIT=NOUT23,STATUS='KEEP')
	  OPEN (UNIT=IN13, FILE=FL1(36), STATUS = 'OLD')
	  WRITE (IN13,401) KEYGOOD
C**********************************************************************
      STOP 'PLTB: Completed successfully.'
      END PROGRAM PLTB
