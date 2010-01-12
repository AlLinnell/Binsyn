      PROGRAM PLTDD
C     PROGRAM TO PRODUCE PLOTS OF PROJECTED OUTLINES OF TWO STARS,ON THE 
C     PLANE OF THE SKY, PLUS AN ACCRETION DISK AROUND ONE COMPONENT
C      FOR A SELECTION OF ORBITAL PHASES
C***********************************************************************
C     PLOT FUNCTIONS FOR TWO STARS ALONE
C     CONTROL FUNCTION OF ARRAY LCT   
C LCT(1)=1,PLOT GRID OF VISIBLE POINTS,ECLIPSED STAR
C LCT(2)=1,PLOT LARGE THETA HORIZON,ECLIPSED STAR 
C LCT(3)=1,PLOT SMALL THETA HORIZON,ECLIPSED STAR 
C LCT(4)=1,PLOT OUTLINE,ECLIPSED STAR 
C LCT(5)=1,PLOT ALL POINTS,ECLIPSED STAR     
C LCT(6)=1,PLOT XS,XN POINTS,ECLIPSED STAR   
C LCT(7)=1,PLOT SHADOW BOUNDARY POINTS,ECLIPSED STAR
C LCT(8)=1,INCLUDE PLOT OF POINTS ON ECLIPSED STAR INSIDE  
C      BOUNDARY OF ECLIPSING STAR     
C LCT(9)=1,PLOT GRID,ECLIPSING STAR   
C LCT(10)=1,PLOT LARGE THETA HORIZON,ECLIPSING STAR      
C LCT(11)=1,PLOT SMALL THETA HORIZON,ECLIPSING STAR      
C LCT(12)=1,PLOT OUTLINE,ECLIPSING STAR      
C LCT(13)=1,PLOT ALL POINTS,ECLIPSING STAR   
C LCT(14)=1,PLOT XS,XN POINTS,ECLIPSING STAR 
C LCT(15)=1,DO NOT ROTATE THE COORDINATE SYSTEM     
C LCT(16)=1,PLOT SPOTS
C
C     Program does eclipsing star first, for ICOUNT=1, then eclipsed star
C     for ICOUNT=2
C     IEC designates the eclipsing star
C     ISV designates the star whose center is at the origin of coordinates
C
C***************************************************************************
C     PLOT FUNCTIONS FOR ACCRETION DISK ALONE
C LCW(1)=1, PLOT GRID OF VISIBLE POINTS, ACCRETION DISK
C LCW(2)=1, PLOT ALL POINTS, ACCRETION DISK
C LCW(3)=1, DRAW VISIBLE BOUNDARY, ACCRETION DISK
C
C
C***************************************************************************
C
C     The boundaries of the two stars, the accretion disk, and the rim, are
C     stored in output file NOUT4. The various segments of the boundaries
C     are delimited by numerical keys. These keys are used by program
C     HORZPLT to segregate the segments and write separate output files
C     for use by a separate plot routine.
C     The delimiters are as follows:
C     eclipsing star, large theta, first section			ANGA
C     eclipsed star, large theta, first section			ANG
C     one rim vert. section, first segment				SP2
C     other rim vert. section, first segment				SP3
C     eclipsed star, large theta, second section			SP14
C     bottom rim, 3rd segment								SP15
C     eclipsing star, small theta, first section			SP1
C     eclipsing star, small theta, second section			SP6
C     eclipsed star, small theta, first section			SP7
C     eclipsed star, small theta, second section			SP8
C     top rim, second section								SP9
C     bottom rim, second section							SP10
C     top rim, first section								SP4
C     bottom rim, first section							SP5
C     first rim vert. section, second segment				SP11
C     other rim vert. section, second segment				SP12
C     eclipsing star, large theta, second section			SP13
C     bottom edge of rim, 3rd section						SP16
C
C     The rim bright spot is output on NOUT8
C     The spots on the star photospheres are output by program PLTB
C****************************************************************************
      INCLUDE 'COMA.FOR'
	INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
      CHARACTER * 28 FL1
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER * 2 IHR,IMIN,ISEC,I100TH
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DIMENSION FL1(32)
      DIMENSION TARY(MTR,MTR)      
      DIMENSION LCT(16),WL(MWL) 
      DIMENSION AGLV(MSG),VLMDA(MTHM),VGMDA(MFV,MTHM),VBTDA(MTHM),
     $ERR(MTHM),
     $VLMDB(MTHM),VGMDB(MFV,MTHM),VBTDB(MTHM)
      DIMENSION DLTA(MFV,MTHM)
      DIMENSION LCW(MTR)
      DIMENSION CSGR(MRM,MSG),CSGF(MRING,MSG),PXR(MRM,MSG),
     $PYR(MRM,MSG),
     $PZR(MRM,MSG),PXF(MRING,MSG),PYF(MRING,MSG),PZF(MRING,MSG),
     $PRRH(MRM,MSG),
     $PRFH(MRING,MSG),PRSH(MTW,MTHM,MTW)
      DIMENSION VXP(MTW,MTHM,MTW),VYP(MTW,MTHM,MTW)
      DIMENSION RYHZG(MRM),RXHZG(MRM),RYHZL(MRM),RXHZL(MRM),
     $PRVFG(MRM),PRVFL(MRM),RVLP(MSG),ANGV(MSG),RD(MSG),AGLW(MSG)
      DIMENSION STX(MTW,MTHM,MPHM),STY(MTW,MTHM,MPHM),
     $STZ(MTW,MTHM,MPHM),STVS(MTW,MTHM,MPHM)
	DIMENSION NHSP(MSG),BFX(MSG),BFY(MSG)
	DIMENSION AZ(MTLM)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 20 OBJ
      CHARACTER * 20 OBJA
	INTEGER * 2 KEYGOOD, KEYBAD
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
      DATA PIO2/1.5707963267949D0/
      DATA PI,TWPI/3.1415926535898D0,6.2831853071796D0/
      DATA TPIO2/4.7123889803847D0/
      DATA ANG,ANGA/6.29D0,6.39D0/
      DATA SP1,SP2,SP3,SP4,SP5,SP6/7.D0,7.2D0,7.4D0,7.6D0,7.8D0,8.D0/
	DATA SP7,SP8,SP9,SP10,SP11,SP12/8.2D0,8.4D0,8.6D0,8.8D0,9.D0,
     $9.2D0/
	DATA SP13,SP14,SP15,SP16/9.4D0,9.6D0,9.8D0,10.8D0/
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
      DATA IN10,IN11,IN12,IN13,IN14,IN15,IN16/10,11,12,13,14,15,16/
	DATA IN17,IN18/17,18/
	DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5/19,20,21,22,23/
	DATA NSCR,NOUT6,NOUT7,NOUT8,NOUT9,NOUT10/24,25,26,27,28,29/
	DATA NOUT11/30/
	DATA IN19/31/
      DATA ER/1.0D-8/
      DATA NPT/201/
      DATA CF/57.29577951D0/
	DATA ERLT/1.0D-13/
  601 FORMAT (I2)
  193 FORMAT (3F12.5,I8)
  194 FORMAT (4F12.5)
  201 FORMAT (A28)
  104 FORMAT (1H1)      
  105 FORMAT (80I1)     
  106 FORMAT (10I5)     
  107 FORMAT (F6.3)     
  111 FORMAT (1X,'VALUE OF NPND.GT.NPSI')    
  112 FORMAT (8F10.5)
  113 FORMAT (2F12.5)
  114 FORMAT (' ')
  416 FORMAT (A20)
  417 FORMAT (A9,3X,A8)
  418 FORMAT (1X,'Ordinal frame no.',I2)
  419 FORMAT (1X,'Orbital longitude=',F10.3)
  420 FORMAT (5X,'ICOUNT=',I2)
  421 FORMAT (1X,'Orbital inclination=',F8.3)
C********************************************************
      OPEN (UNIT= 32,FILE= 'PLTDD.ILS',STATUS='OLD')
      READ (32, 201)     (FL1(J),J=1,31)
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
      OPEN (UNIT= IN13, FILE=FL1(13), STATUS='OLD')
	OPEN (UNIT= IN14, FILE=FL1(14), STATUS='OLD')
	OPEN (UNIT= IN15, FILE=FL1(15), STATUS='OLD')
	OPEN (UNIT= IN16, FILE=FL1(16), STATUS='OLD')
	OPEN (UNIT= IN17, FILE=FL1(17), STATUS='OLD')
	OPEN (UNIT= IN18, FILE=FL1(18), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(19), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(20), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(21), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(22), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(23), STATUS='REPLACE')
      OPEN (UNIT=NSCR,  FILE=FL1(24), STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(25), STATUS='REPLACE')
      OPEN (UNIT=NOUT7, FILE=FL1(26), STATUS='REPLACE')
	OPEN (UNIT=NOUT8, FILE=FL1(27), STATUS='REPLACE')
      OPEN (UNIT=NOUT9, FILE=FL1(28), STATUS='REPLACE')
      OPEN (UNIT=NOUT10,FILE=FL1(29), STATUS='REPLACE')
	OPEN (UNIT=NOUT11,FILE=FL1(30), STATUS='REPLACE')
	OPEN (UNIT=IN19,  FILE=FL1(31), STATUS='OLD')
C********************************************************
      WRITE (IN19, 601) KEYBAD
	CLOSE (UNIT=IN19, STATUS = 'KEEP')
      LABEL= '**PLOTD'
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
      CALL IREAD( NSP)
      CALL DUMMY(1)
      CALL IREAD( KSP)
C*********************************************************************
      LABEL= '*PLOTDA'
      NU=IN2
      LBLERR=0
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL CREAD ( OBJA)
      IF (OBJA.NE.OBJ) CALL PMDSTOP
      CALL DUMMY(1)
      CALL IREAD ( NPSTA)
      CALL DUMMY (1)
      CALL IREAD (NPNDA)
      CALL DUMMY (1)
      CALL IREAD (INDXA)
      CALL DUMMY (1)
      CALL IREAD (LCW(1))
      CALL DUMMY (1)
      CALL IREAD (LCW(2))
      CALL DUMMY (1)
      CALL IREAD (LCW(3))
      CALL DUMMY (1)
C*************************************************************************
      WRITE (NOUT1,104)
C     Read NPSI	     
      READ (IN5,106) KCTRL,NPSI
      NU= IN6
	CALL IREAD (NSPA)
	CALL DUMMY (1)
	CALL IREAD (NSPB)
      NU=IN14
      READ (IN14,106,END=399,ERR=299) NWL
C     ENTER DATA ON SPOTS
C
      NSP=NSPA
	NU=IN7
	CALL DUMMY (2)
	READ (IN7,112,END=399,ERR=299) (VLMDA(K),K=1,NSP)
	DO K=1,NSP
		VLMDA(K)=VLMDA(K)/CF
	END DO
	NU=IN8
	CALL DUMMY(2)
	NLT=NWL
	IF (NLT.GT.5) NLT=5
	DO J=1,NLT
	READ (IN8,112,END=399,ERR=299) (VGMDA(J,K),K=1,NSP)
		DO K=1,NSP
			VGMDA(J,K)=VGMDA(J,K)/CF
		END DO
	END DO
	NU=IN9
	CALL DUMMY(2)
	READ (IN9,112,END=399,ERR=299) (VBTDA(K),K=1,NSP)
	DO K=1,NSP
		VBTDA(K)=VBTDA(K)/CF
	END DO
      NSP=NSPB
      NU=IN10
      CALL DUMMY(2)
      READ (IN10,112,END=399,ERR=299) (VLMDB(K),K=1,NSP)
C
         DO K=1,NSP
         VLMDB(K)=VLMDB(K)/CF
         END DO
      NU=IN11
      CALL DUMMY(2)
      NLT=NWL
      IF (NLT.GT.5) NLT=5
      DO J=1,NLT
      READ (IN11,112,END=399,ERR=299) (VGMDB(J,K),K=1,NSP)
      DO K=1,NSP
      VGMDB(J,K)=VGMDB(J,K)/CF
      END DO
      END DO
      NU=IN12
      CALL DUMMY(2)
      READ (IN12,112,END=399,ERR=299) (VBTDB(K),K=1,NSP)
      DO K=1,NSP
      VBTDB(K)=VBTDB(K)/CF
      END DO       
      NU=IN13
      CALL DUMMY(2)
      NLT=NWL
      IF (NLT.GT.5) NLT=5
      DO J=1,NLT
      READ (IN13,112,END=399,ERR=299) (DLTA(J,K),K=1,NSP)
      END DO
C     Input data from PGA (PGAX2.DAT)
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN3)

C     Input ACPGC data on hot spot, if any
      LABEL =' *ACPGC'
	NU=IN18
	CALL GTLBL(LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL IREAD (NWL)
	CALL IREAD (MDAT)
	CALL DUMMY (2)
	CALL IREAD (IRCT)
	CALL DUMMY (2)
	CALL IREAD (NRF)
	CALL IREAD (NGR)
	CALL IREAD (NTMP)
	DO I=1,NWL
	CALL DREAD(WL(I),'F10.8')
	END DO
	CALL DUMMY(3)
	CALL DREAD (VMDOT,'F10.8')
	CALL DUMMY(1)
	CALL DREAD (TECS,'F10.7')
	CALL DUMMY (1)
	CALL DREAD (VMU,'F10.8')
	CALL DUMMY (1)
	CALL DREAD (BALF,'F10.8')
	CALL DUMMY (1)
	CALL DREAD (VMUF,'F10.8')
	CALL DUMMY (1)
	CALL DREAD (BALB,'F10.8')
	CALL DUMMY (1)
	CALL DREAD (VMUD,'F10.8')
	CALL DUMMY (1)
	CALL IREAD (KEY)
	CALL DUMMY (7)
	CALL IREAD (KYRM)
	CALL DUMMY (1)
	CALL IREAD (IHSPT)
	CALL DREAD (HSAZ,'F8.1')
	HSAZ=HSAZ/180.D0*PI
	CALL DUMMY (1)
	CALL DREAD (HSWD,'F8.1')
	HSWD=HSWD/180.D0*PI
	HSLL=HSAZ
	HSUL=HSAZ+HSWD
C     Input data from ACPGA (ACPGA2.DAT)
      CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,A,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN16)
      IF (NPND.GT.NPSI) GO TO 199     
C*******************************************************************
C     START OF PRINCIPAL LOOP IN ORBITAL LONGITUDE
C*******************************************************************
      KSRF=0
      DO 100 KS=1,NPND
      REWIND IN5
C  
C  	Input data from PGB (PGBX2.DAT)
      CALL RDWT2(NT,NU,NTH,ISV,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN4)
C     ISV ALWAYS DESIGNATES THE COMPONENT AT THE ORIGIN OF COORDINATES   
C     SET IEC TO THE DESIGNATION OF THE ECLIPSING STAR
      IF ((PSI.LE.PIO2).OR.(PSI.GE.TPIO2)) IEC=3-ISV
      IF ((PSI.GT.PIO2).AND.(PSI.LT.TPIO2)) IEC=ISV 
C     Set control for quadrant in which PSI is located
      IF ((PSI.GE.0.D0).AND.(PSI.LE.PIO2)) ISEQ=1
      IF ((PSI.GT.PIO2).AND.(PSI.LE.PI)) ISEQ=2
	IF ((PSI.GT.PI).AND.(PSI.LE.TPIO2)) ISEQ=3
	IF ((PSI.GT.TPIO2).AND.(PSI.LE.TWPI)) ISEQ=4
	IF ((PSI.LT.0.D0).OR.(PSI.GT.TWPI)) CALL PMDSTOP
C
C     Input data from ACPGB (ACPGB2.DAT)
      CALL ACRD3(NSG,NRM,NRING,CSGR,CSGF,PXR,PYR,PZR,PXF,PYF,PZF,
     $XHZ,YHZ,PRFH,PRSH,PRRH,XRMC,YRMC,RBP,STX,STY,STZ,STVS,
     $RXHZL,RYHZL,RXHZG,RYHZG,PRVFG,PRVFL,XUL,XLL,YUL,YLL,RVLP,
     $ANGV,RD,AGLW,VXP,VYP,IFCP,IRCP,IFCSP,IRCSP,IGAIN,I,IEC,IFLAG,
     $NTH,IBDY,IN17)
C     Read input files until the program reaches the starting index
      IF (KS.LT.NPST) GO TO 100
C     Set data for rim hot spot plot
	HSLLR=HSLL
C     Allow for case where hot spot goes past phase zero.
	IF (HSLLR.LT.0.D0) HSLLR=HSLLR+TWPI
	IF (HSLLR.GT.TWPI) HSLLR=HSLLR-TWPI
	DLHS=HSUL-HSLL
	HSULR=HSLLR+DLHS
      IF (IHSPT.NE.0) THEN
		IHSPCT=0
		DO J=1,NSG
			NHSP(J)=0
			AZIM=PHIR(J)
			IF ((AZIM.GE.HSLLR).AND.(AZIM.LE.HSULR)) THEN
				NHSP(J)=1
				IHSPCT=IHSPCT+1
			END IF
C     Make sure the hot spot is at least one segment wide
			IF ((IHSPCT.EQ.1).AND.(NHSP(J).EQ.0)) THEN
				NHSP(J)=1
				IHSPCT=IHSPCT+1
			END IF
		END DO
	END IF
            
C
      CM=TWPI-PSI
      IF (DABS(PSI).LE.ERLT) CM=0.D0
      IF (KS.LT.NPST) GO TO 100
      IF (KS.LT.KSRF) GO TO 100
      IF (KSRF.EQ.0) KSRF=1
      KSRF=KSRF+INDX
      WRITE (NOUT1,418) KS
      WRITE (NOUT1,419) PSI
      CSI=-DCOS(VI)
      SNI=DSIN(VI)
C     SET TO LOOK AT OPPOSED HEMISPHERE OF ECLIPSING STAR  
      AG=0.D0
      IF (ISV.EQ.IEC) AG=PI
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)      !calculate XG,YG
C
C     Calculate XG,YG,ZG
C
      CALL PROJ(TARY,1.0D0,0.0D0,0.0D0,XG,YG,ZG)
      XC=QS/(1.D0+QS)
      IF (IEC.EQ.ISV) XC=QN/(1.D0+QN)
C
C     Calculate XCMS,YCMS,ZCMS
C
      CALL PROJ(TARY,XC,0.D0,0.D0,XCMS,YCMS,ZCM)
      IF (IEC.EQ.ISV) XCMS=XCMS-XG
      IF (IEC.EQ.ISV) YCMS=YCMS-YG
      XCM=YCMS
      YCM=XCMS
C     The names of the axes are reversed later in the program
C*******************************************************
C     XG,YG ZG are the coods.of the center of the eclipsing star
C     relative to the center of the eclipsed star.
C     XG and YG are the projections of the center on the plane of the sky.
C
C     XCM,YCM,ZCM are the coordinates of the center of mass of the system
C     and XCM,YCM are the coods. projected on the plane of the sky
C*******************************************************
      IF (DABS(XG).GT.0.000000001D0) GO TO 3
      IF (YG.LT.0.0D0) ANGLO=-PIO2
      IF (YG.GT.0.0D0) ANGLO= PIO2
      GO TO 4    
    3 ANGLO=DATAN(YG/XG)
C     SET FOR ZERO ROTATION
      ANGLO=0.0D0
      GO TO 9
C  
   4  ANGLO=0.0D0
C  
   9  CSPJ=DCOS( ANGLO)
      SNPJ=DSIN( ANGLO)
      N=NPH(IEC,NTH/2+1)
      RGT=VR(ISV,NTH/2+1,N/2+1)
      RLS=VR(3-ISV,NTH/2+1,N/2+1)
      IF (RGT.GT.RLS) GO TO 205
      RGT=VR(3-ISV,NTH/2+1,N/2+1)
      RLS=VR(ISV,NTH/2+1,N/2+1)
C     At this point, RGT is back equatorial radius of larger star, RLS is
C     back equatorial radius of smaller star
  205 IF ((0.D0.LE.PSI).AND.(PSI.LT.PI)) GO TO 206
C     For PI.LE.PSI and PSI.LT.TWPI,star 3-ISV is on the right
      IF (RGT.EQ.VR(ISV,NTH/2+1,N/2+1)) RLF=RGT     !If star at origin is larger,RLF=RGT
      IF (RGT.EQ.VR(3-ISV,NTH/2+1,N/2+1)) RLF=RLS   !If star not at orig. is>,RLF=RLS
      IF (RLF.EQ.VR(ISV,NTH/2+1,N/2+1)) RRT=VR(3-ISV,NTH/2+1,N/2+1)
      IF (RLF.EQ.VR(3-ISV,NTH/2+1,N/2+1)) RRT=VR(ISV,NTH/2+1,N/2+1)
      GO TO 207
C     For 0.0.LE.PSI,and PSI.LT.PI,star ISV is on the right
  206 IF (RGT.EQ.VR(3-ISV,NTH/2+1,N/2+1)) RLF=RGT
      IF (RGT.EQ.VR(ISV,NTH/2+1,N/2+1)) RLF=RLS
      IF (RLF.EQ.VR(ISV,NTH/2+1,N/2+1)) RRT=VR(3-ISV,NTH/2+1,N/2+1)
      IF (RLF.EQ.VR(3-ISV,NTH/2+1,N/2+1)) RRT=VR(ISV,NTH/2+1,N/2+1)
  207 IF (RLF.EQ.VR(ISV,NTH/2+1,N/2+1)) RPLF=VR(ISV,1,1)    !RPLF=polar rad.of star,RLF=back
      IF (RLF.EQ.VR(3-ISV,NTH/2+1,N/2+1)) RPLF=VR(3-ISV,1,1)
      IF (RRT.EQ.VR(ISV,NTH/2+1,N/2+1)) RPRT=VR(ISV,1,1)
      IF (RRT.EQ.VR(3-ISV,NTH/2+1,N/2+1)) RPRT=VR(3-ISV,1,1)
C     Y2,X2 are coods. of leftmost pt.,rel.to center of star on left
      Y2=-RLF
      X2=0.0D0
C     Y3,X3 are coods. of upper polar pt.of star on left,rel.to its center, 1st
C     and 2nd quadrants. Upper polar point of star on right, rel. to its center,
C     3rd and 4th quadrants
      Y3=0.0D0
      IF ((0.D0.LE.PSI).AND.(PSI.LT.PI)) X3=RPLF
      IF ((PI.LE.PSI).AND.(PSI.LE.TWPI)) X3=RPRT
C     Y4,X4 are coods.of rightmost pt.,rel.to center of star on right
      Y4=RRT
      X4=0.0D0
C     Y5,X5 are coods.of upper pole of star on right,rel.to its center, 1st and
C     2nd quadrants. Upper pole of star on left, 3rd and 4th quadrants
      Y5=0.0D0
      IF ((0.D0.LE.PSI).AND.(PSI.LT.PI)) X5=RPRT
      IF ((PI.LE.PSI).AND.(PSI.LE.TWPI)) X5=RPLF
  103 IF ((0.D0.LE.PSI).AND.(PSI.LT.PI)) GO TO 108
C     Star I=ISV is on the left
      IF (PSI.LT.TPIO2) AD=-YG
      IF (PSI.GE.TPIO2) AD=YG
      TS=DABS(Y2)-AD
      YRT=Y4
      IF (TS.GT.YRT) YRT=-Y2
      XRT=X4
      IF (YRT.EQ.-Y2) XRT=X2
C     YRT,XRT now are coods of rightmost point
      TS=-(DABS(Y2)+AD)
      YLF=-Y4
      IF (TS.LT.YLF) YLF=Y2
      XLF=X4
      IF (YLF.EQ.Y2) XLF=X2
C     YLF,XLF now are coods of leftmost point
      GO TO 109
C     Star I=ISV is on the right
  108 IF (PSI.LE.PIO2) AD=-YG
      IF (PSI.GT.PIO2) AD=YG
      TS=Y4+AD
      YRT=-Y2
      IF (TS.GT.YRT) YRT=Y4
      IF (YRT.EQ.Y4) XRT=X4
      TS=-Y4+AD
      YLF=Y2
      IF (TS.LT.YLF) YLF=-Y4
      XLF=X2
      IF (YLF.EQ.-Y4) XLF=X4
C     Now determine top and bottom points
  109 AD=XG
      IF ((PIO2.LE.PSI).AND.(PSI.LT.TPIO2)) AD=-XG
C     AD is (-) in 1st and 4th quadrants, (+) in 2nd and 3rd
      TS=X3+AD
      XTP=X5
      IF (TS.GT.XTP) XTP=X3+AD
      YTP=Y5
      IF (XTP.EQ.(X3+AD)) YTP=Y3
      TS=-X3+AD
      XBT=-X5
      IF (TS.LT.XBT) XBT=-X3+AD
      YBT=Y5
      IF (XBT.EQ.(-X3+AD)) YBT=Y3
  411 QMXX=XTP
      QMNX=XBT
      QMXY=YLF
      IF (YRT.GT.YLF) QMXY=YRT
      QMNY=YLF
      IF (YRT.LT.YLF) QMNY=YRT
      IF ((0.0D0.LE.PSI).AND.(PSI.LE.PIO2)) GO TO 350
      IF ((PIO2.LT.PSI).AND.(PSI.LE.PI)) GO TO 352
      IF ((PI.LT.PSI).AND.(PSI.LT.TPIO2)) GO TO 354
      IF ((TPIO2.LE.PSI).AND.(PSI.LE.TWPI)) GO TO 356
      CALL PMDSTOP
  350 QMNY=QMNY+YG
      GO TO 8
  352 QMNY=QMNY-YG
      GO TO 8
  354 QMXY=QMXY-YG
      GO TO 8
  356 QMXY=QMXY+YG
    8 CONTINUE   
  401 XMNV=QMNX
      XMX=QMXX
      YMNV=QMNY
      YMX=QMXY
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
      IF ((PSI.GT.PIO2).AND.(PSI.LT.TPIO2)) GO TO 1 
C  
C  
      I=3-ISV    
      Q=QN
      GO TO 2    
C  
C  
    1 I=ISV      
      Q=QS
C     ICOUNT=1 ALWAYS GIVES ECLIPSING STAR   
C     ICOUNT=2 ALWAYS GIVES ECLIPSED STAR    
C  
C  
C***********************************************************************
C     Start of repeated loop for consecutive treatment of two components
C***********************************************************************
C
    2 DO 101 ICOUNT=1,2
C     For ICOUNT=1, I designates the star not eclipsed at orbital
C     phase 0.0.
  900 WRITE (NOUT1,420) ICOUNT
      DO 6 J=1,MPHM
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
      IF (ICOUNT.EQ.1) NTS=NTL(I,NTH)
      IF (ICOUNT.EQ.2) NTS=NTL(3-I,NTH)
	IF (ICOUNT.EQ.1) NTSA=NTS
	IF (ICOUNT.EQ.2) NTSB=NTS
C  
C  
C********************************************************************  
C     This section produces projections for all points on both components
C
C     Set counter for total number of points on a component
      KK=1
C
      DO 50 ITH=1,NTH
      NN=NPH(I,ITH)      
      DO K=1,NN
	VPHS=PH(I,ITH,K)
	IF ((ICOUNT.EQ.2).AND.(LCT(5).EQ.1)) GO TO 39
	IF ((ICOUNT.EQ.1).AND.(LCT(13).EQ.1)) GO TO 39
	N=1
	IF ((VPHS.GE.VST(I,ITH,N)).AND.(VPHS.LE.VND(I,ITH,N))) 
     $	GO TO 31
      N=2
	IF ((VPHS.GE.VST(I,ITH,N)).AND.(VPHS.LE.VND(I,ITH,N)))
     $	GO TO 31
      N=3
	IF ((VPHS.GE.VST(I,ITH,N)).AND.(VPHS.LE.VND(I,ITH,N)))
     $	GO TO 31
C     IF (ICOUNT.EQ.1), IT IS THE ECLIPSING STAR
      IF (ICOUNT.EQ.1) GO TO 32
   34 QX(KK)=1000.D0
      QY(KK)=1000.D0
	GO TO 40
   31 IF (ICOUNT.EQ.1) GO TO 390
C     IF WE ARRIVE HERE, WE HAVE THE ECLIPSED STAR
      IF (LCT(8).EQ.1) GO TO 39
C     We have already eliminated the points below the horizon.
      IF (ITH.NE.NTH) GO TO 33
	IF ((SHS(ITH,1).NE.ANG).AND.(DABS(SHS(ITH,1)-ANG).LE.ERLT))
     $ GO TO 40
   33 IF (VPHS.LT.SHS(ITH,1)) GO TO 39
      IF ((VPHS.GT.SHN(ITH,1)).AND.(VPHS.LT.SHS(ITH,2))) GO TO 39
	IF ((VPHS.GT.SHN(ITH,2)).AND.(VPHS.LT.SHS(ITH,3))) GO TO 39
	IF (VPHS.GT.SHN(ITH,3)) GO TO 39
	GO TO 34
   32 PX(KK)=1000.D0
      PY(KK)=1000.D0
	GO TO 40
C     Check for neck region . If not in neck, include.
  390 IF ((DABS(VPHS).LE.ERLT).AND.(DABS(VST(I,ITH,3)-ANG).LT.1.D-9))
     $GO TO 39
      RG=VND(I,ITH,N)-VST(I,ITH,N)
	IF ((RG.LT.PIO2).AND.(N.EQ.1)) GO TO 32			  	   		        
   39 X=VR(I,ITH,K)*ALM(I,ITH,K)    
      Y=VR(I,ITH,K)*AMU(I,ITH,K)
      Z=VR(I,ITH,K)*ANU(I,ITH,K)
      IF (I.EQ.ISV) GO TO 66
C     Displace points for case I.NE.ISV	   
      X=X-1.0D0
   66 IF (ICOUNT.EQ.2) GO TO 38
      IF ((STVS(I,ITH,K).LT.0.D0).AND.(IBDY.EQ.1)) GO TO 32
      CALL PROJ(TARY,X,Y,Z,PX(KK),PY(KK),PZ(KK))     !eclipsing star
      GO TO 40
   38 IF (I.EQ.ISV) THEN
		IF ((STVS(I,ITH,K).LT.0.D0).AND.(IBDY.EQ.1)) GO TO 34	   
		CALL PROJ(TARY,X,Y,Z,QX(KK),QY(KK),QZ(KK))     !eclipsed star
		GO TO 40
	ELSE
		IF (STVS(I,ITH,K).LT.0.D0) GO TO 34	   
		CALL PROJ(TARY,X,Y,Z,QX(KK),QY(KK),QZ(KK))     !eclipsed star
		GO TO 40
	END IF
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
      IF (DABS(X-1000.0D0).LT.ERLT) GO TO 65
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
      WRITE (NOUT2, 193) X-XCM,-Y+YCM,Z-ZCM,K
   65 CONTINUE
      GO TO 301  
C  
C     THE FOLLOWING SECTION IS FOR THE ECLIPSED STAR
C
  300 CONTINUE
      DO 64 K=1,NTSB
      IF ((LCT(1).NE.1).AND.(LCT(5).NE.1)) GO TO 64 
      XP=QX(K)   
      YP=QY(K)
	ZP=QZ(K)   
      X=XP
      Y=YP
C     ELIMINATE POINTS THAT HAVE BEEN FLAGGED NOT TO PLOT
      IF (DABS(X-1000.D0).LE.ERLT) GO TO 64 
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
      WRITE (NOUT2, 193) X-XCM, -Y+YCM, Z-ZCM, K
   64 CONTINUE
C
C  
C************************************************************************  
C  
C     following section for both eclipsed and eclipsing star  
C  
C     Do spots
C***********************************************************************
C
  301 CONTINUE
      IF (LCT(16).NE.1) GO TO 501
	CALL PLSPT(LCT,NSPA,NSPB,I,VBTDA,VLMDA,VGMDA,VBTDB,VLMDB,
     $VGMDB,ISV,TARY,NTH,
     $CSPJ,SNPJ,VRD,ICOUNT,XCM,YCM,NOUT6,NOUT9,NOUT10,NOUT11)
C******************************************************************************
C
C     PLOT BOUNDARY PROFILE,AS PROVIDED BY XS,YS AND XN,YN POINTS
C     THIS INCLUDES THE CENTRAL MERIDIAN POINTS
C*******************************************************************************
C
  501 CONTINUE
      CALL SHAD1(I,NTH,
     $TARY,XBGEQ,YBGEQ,XNDEQ,YNDEQ,ISEQ,
     $NPB,NPE,NKP,KK,LJ,KYA,KYB,ISV,IEC,IRD,ICOUNT,VRD,NOUT4)
C     Save value of IRD for use in locating intersections of projected
C     boundaries
C
      DO 240 J=1,NTH
      IF ((ICOUNT.EQ.1).AND.(LCT(14).NE.1)) GO TO 240   !ICOUNT=1,eclipsing star
      IF ((ICOUNT.EQ.2).AND.(LCT(6).NE.1)) GO TO 240    !ICOUNT=2,eclipsed star
      DO 239 K=1,3      
      IF (XS(J,K).EQ.ANG) GO TO 239
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
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
  233 IF (DABS(XN(J,K)-XNK(J)).LT.0.0000000001D0) GO TO 239
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
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
  239 CONTINUE   
  240 CONTINUE   
C
C**********************************************************************
C     Prepare to locate intersections of boundaries
C
      CALL HORZ(ISEQ,IRC,IFC,NPB,NPE,NKP,
     $I,XBGEQ,YBGEQ,
     $XNDEQ,YNDEQ,IEC,XG,YG,ISV,IRD,ICOUNT,KCOR,NOUT7)
C  
C  
C  
C      ARYB GOES WITH THE SMALL THETA BRANCH,ARRAYS RS(2),CSA(2),ALP(2), 
C     AND INDEX IRC     
C     ARYA GOES WITH THE LARGE THETA BRANCH,ARRAYS RS(1),CSA(1),ALP(1),  
C    AND INDEX IFC      
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
C     these data are saved the first time through the long loop starting
C     with statement 2,for ICOUNT=1
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
C    DO LARGE THETA BRANCH FIRST,FOR THE ECLIPSED STAR   
   43 AGL1=ALP(1,1)
      AGL2=ALP(IFC,1)
      RG=(AGL2-AGL1)/(NPT-1)   
C     FILL ARRAY ERR,PREPARATORY TO SEARCH FOR INTERSECTIONS OF BOUNDARIES      
      DO 45 J=1,NPT     
C     ANGLE always is measured on the eclipsed star
      ANGLE=AGL1+(J-1)*RG      
      AGLV(J)=ANGLE
	ERR(J)=0.D0
C	IF (VI.LT.0.8D0) GO TO 45     
C     IF ERR(J) IS NEGATIVE,THE POINT IN QUESTION LIES INSIDE THE 
C     BOUNDARY OF THE STAR AT THE ORIGIN     
C     NOTE THAT ANGLE IS ALWAYS MEASURED ON THE STAR WHOSE CENTER  
C     IS AT YG,XG
   44 CALL RANGE(ANGLE,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERR(J),IRD)     
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
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 48
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 48
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
      Y1=ERR(J1) 
      Y2=ERR(J2) 
C     LOCATE FIRST ROOT ON THE LARGE THETA BRANCH   
      DO 120 KCT=1,4    
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  127 CALL RANGE(AGRT,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  128 IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 123
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 124
      CALL PMDSTOP      
  123 IF (ERVL.GE.0.0D0) GO TO 119
      GO TO 118  
  124 IF (ERVL.LT.0.0D0) GO TO 119
  118 X2=AGRT    
      Y2=ERVL     
      IF (ERVL.EQ.0.0D0) GO TO 117
      GO TO 120  
  119 X1=AGRT    
      Y1=ERVL     
      IF (DABS(ERVL).LE.ERLT) GO TO 117
  120 CONTINUE   
C     AGLMT1 IS THE FIRST ROOT OF ERR,AGLMT2 IS THE SECOND 
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
C     Fill array ERR,preparatory to root search.
      DO 91 J=1,NPT     
      ANGLE=AGL1+(J-1)*RG      
      AGLV(J)=ANGLE 
	ERR(J)=0.D0
	IF (VI.LT.0.8D0) GO TO 91    
   95 CALL RANGE(ANGLE,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERR(J),IRD)     
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
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 131
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 131
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
      Y1=ERR(J1) 
      Y2=ERR(J2) 
C     LOCATE FIRST ROOT ON THE SMALL THETA BRANCH   
      DO 133 KCT=1,4    
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  151 CALL RANGE(AGRT,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  152 CONTINUE   
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 141
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 142
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
     $AGLRT,ERR(J),IRD)
  158 CONTINUE
      DO 49 J=2,NPT    
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 56
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 56
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
      Y1=ERR(J3) 
      Y2=ERR(J4) 
C     LOCATE SECOND ROOT ON THE LARGE THETA BRANCH  
      DO 122 KCT=1,3    
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  147 CALL RANGE(AGRT,1,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  148 CONTINUE   
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 125
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 126
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
     $AGLRT,ERR(J),IRD)
  189 CONTINUE
C     SEARCH FOR SECOND ROOT ON THE SMALL THETA BRANCH     
      DO 134 J=2,NPT   
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 135
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 135
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
      Y1=ERR(J3) 
      Y2=ERR(J4) 
C     LOCATE SECOND ROOT ON THE SMALL THETA BRANCH  
      DO 137 KCT=1,3
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  153 CALL RANGE(AGRT,2,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD) 
  154 CONTINUE   
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 144
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 145
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
  143 IF (DABS(PI-PSI).GT.0.05D0) GO TO 536
      IF (DABS(AGLMT2-ANG).LE.ERLT) GO TO 536
      IF (AGLMT2.LE.0.0D0) GO TO 536
      IF (AGRT.LT.0.0D0) AGRT=AGRT+TWPI
  536 IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(AGLMT2.NE.ANG)) AGLMT1=AGRT
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
      RG=(AGL2-AGL1)/(MNKP-1)
C     Fill array ERR
      DO 162 J=1,MNKP
      ANGLE=AGL1+(J-1)*RG
      AGLV(J)=ANGLE
  161 CALL RANGE(ANGLE,3,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERR(J),IRD)
  162 CONTINUE
      J1=0
      J2=0
      J3=0
      J4=0
C     Search for possible roots on the neck
      DO 163 J=2,MNKP
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 164
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 164
  163 CONTINUE
C     No root on the neck
      GO TO 140
  164 J1=J-1
      J2=J
      X1=AGLV(J1)
      X2=AGLV(J2)
      IF ((DABS(X2-X1).GT.PI).AND.(X1.GT.0.0D0)) X1=X1-TWPI
      IF ((DABS(X2-X1).GT.PI).AND.(X2.GT.0.0D0)) X2=X2-TWPI
      Y1=ERR(J1)
      Y2=ERR(J2)
C     Locate first root on neck
      DO  171 KCT=1,3
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  165 CALL RANGE(AGRT,3,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD)
  166 CONTINUE
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 167
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 168
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
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 176
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 176
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
      Y1=ERR(J3)
      Y2=ERR(J4)
C     Locate second root
      DO 183 KCT=1,3
      CALL LNTER(Y1,X1,Y2,X2,0.0D0,AGRT)
  177 CALL RANGE(AGRT,3,IRC,IFC,NKP,YG,XG,
     $IRCS,IFCS,NKPS,
     $AGLRT,ERVL,IRD)
  178 CONTINUE
      IF ((ERR(J).GE.0.0D0).AND.(ERR(J-1).LT.0.0D0)) GO TO 179
      IF ((ERR(J).LT.0.0D0).AND.(ERR(J-1).GE.0.0D0)) GO TO 180
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
C     IF THE VALUES IN ARRAY ERR ARE POSITIVE,THERE IS NO OVERLAP OF THE 
C     TWO COMPONENT BOUNDARIES 
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
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(AGLMT2.EQ.ANG)) GO TO 157
      IF ((AGLMT1.NE.ANG).AND.(AGLMT2.NE.ANG)) GO TO 174
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(AGLMT2.NE.ANG)) GO TO 173
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
C
C     The intersection points have now been located
C***************************************************************
C     PLOT THE SMALL THETA HORIZON POINTS, under control of LCT(11)
C     If, =1, plot remote horizon, eclipsing star.
C     This control is nominally set = 0.
C***************************************************************
C
C
      DO 320 J=1,IRC
      IF (LCT(11).NE.1) GO TO 320     
C     ECLIPSING STAR    
      YP=YR(J)   
      XP=XR(J)   
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 75      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 306  
   75 CONTINUE   
      X=XP
      Y=YP
      GO TO 306  
C     ECLIPSED STAR     
  209 IF (LCT(3).NE.1) GO TO 320      
C     If AGLMT1 and AGLMT2.EQ.(-ANG),the eclipsed star is
C     not visible,in toto.
      IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(DABS(AGLMT2+ANG).LE.ERLT)) 
     $GO TO 320   
      YP=YR(J)   
      XP=XR(J)   
      X=XP
      Y=YP
      IF (LCT(5).EQ.1) GO TO 307      
      IF (ISV.EQ.IEC) GO TO 270
C  Following section follows procedure in SRT RANGE
      RDS=DSQRT(X**2+Y**2)      
      ANGLE=0.0D0
      IF ((X.GE.0.0D0).AND.(DABS(Y).LE.ERLT)) ANGLE=PIO2
      IF (DABS(ANGLE-PIO2).LE.ERLT) GO TO 1036
      IF ((X.LT.0.0D0).AND.(DABS(Y).LE.ERLT)) ANGLE=-PIO2
      IF (DABS(ANGLE+PIO2).LE.ERLT) GO TO 1036
 1017 ANGLE=DACOS(Y/RDS)
      IF (IRD.EQ.-1) ANGLE=PI-ANGLE
      IF (X.LT.0.0D0) ANGLE=-ANGLE
 1036 CONTINUE
C     IF AGLMT1 AND AGLMT2 ARE ON THE SMALL THETA BRANCH,PLOT ALL POINTS ON     
C     THE LARGE THETA BRANCH   
C     NOTE THAT ANGLE MOVES TOWARD MORE POSITIVE ANGLES ON THE LARGE     
C     THETA BRANCH      
C     If AGLMT1.EQ.ANG,plot all points
      IF (DABS(AGLMT1-ANG).LE.ERLT) GO TO 307
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 304
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 305
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 309
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 312
      CALL PMDSTOP      
  304 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 307      
      GO TO 320  
C     IF THE POINT FALLS BETWEEN AGLMT1 AND AGLMT2,PLOT IT 
  305 IF ((ANGLE.GT.AGLMT2).AND.(ANGLE.LT.AGLMT1)) GO TO 320      
      GO TO 307  
  309 IF (DABS(AGLMT2).LT.PIO2) GO TO 311
      IF ((ANGLE.LE.AGLMT1).AND.(ANGLE.GE.AGLMT2)) GO TO 307      
      GO TO 320  
  311 IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 307
      GO TO 320
  312 IF (DABS(AGLMT1).GT.PIO2) GO TO 313
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 307
      GO TO 320
  313 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 307
      GO TO 320
C     IF WE ARRIVE HERE,THE ECLIPSED STAR IS CENTERED AT YG,XG    
  270 A=X+XG     
      B=Y+YG     
      RDS=DSQRT(A**2+B**2)      
      ANGLE=0.0D0
      IF ((A.GE.0.0D0).AND.(DABS(B).LE.ERLT)) ANGLE=PIO2
      IF (DABS(ANGLE-PIO2).LE.ERLT) GO TO 1336
      IF ((A.LT.0.0D0).AND.(DABS(B).LE.ERLT)) ANGLE=-PIO2
      IF (DABS(ANGLE+PIO2).LE.ERLT) GO TO 1336
 1317 ANGLE=DACOS(B/RDS)
      IF (IRD.EQ.-1) ANGLE=PI-ANGLE
      IF (A.LT.0.0D0) ANGLE=-ANGLE
 1336 IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $	GO TO 307   
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 272
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 273
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 274
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 275
      CALL PMDSTOP      
  272 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 307      
      GO TO 320  
  273 IF ((ANGLE.LT.AGLMT1).AND.(ANGLE.GT.AGLMT2)) GO TO 320      
      GO TO 307  
  274 IF (AGLMT2.GT.-PIO2) GO TO 276  
      IF ((ANGLE.LE.AGLMT1).AND.(ANGLE.GE.AGLMT2)) GO TO 307
      GO TO 320
  276 IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 307 
      GO TO 320  
  275 IF (AGLMT2.GT.PIO2) GO TO 278   
      IF ((ANGLE.LT.AGLMT2).AND.(ANGLE.GT.AGLMT1)) GO TO 320      
      GO TO 307  
  278 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 307      
      GO TO 320  
  307 IF (LCT(15).EQ.1) GO TO 76      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 306  
   76 CONTINUE   
      X=XP
      Y=YP
  306 CONTINUE
      WRITE (NOUT3, 193) X-XCM, -Y+YCM
  370 IF (ICOUNT.EQ.2) GO TO 320
      IF (J.NE.(IRC-1)) GO TO 320
      XNKSV1=X
      YNKSV1=Y
  320 CONTINUE
C  
C************************************************************************
C     PLOT THE LARGE THETA HORIZON POINTS, under control of LCT(1).
C     If set =1, plot facing horizon, eclipsing star.
C     This control is nominally set=0.
C************************************************************************
C
      DO 321 J=1,IFC
C     ECLIPSING STAR    
  400 IF (LCT(10).NE.1) GO TO 321     
      YP=YF(J)   
      XP=XF(J)   
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 77      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 310  
   77 CONTINUE   
      X=XP
      Y=YP
      GO TO 310  
C     ECLIPSED STAR     
  210 IF (LCT(2).NE.1) GO TO 321      
      IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(DABS(AGLMT2+ANG).LE.ERLT)) 
     $	GO TO 321
      YP=YF(J)   
      XP=XF(J)   
      X=XP
      Y=YP
      IF (LCT( 5).EQ.1) GO TO 308     
      IF (ISV.EQ.IEC) GO TO 280
      RDS=DSQRT(X**2+Y**2)      
C  Following section follows procedure in SRT RANGE
      ANGLE=0.0D0
      IF ((X.GE.0.0D0).AND.(DABS(Y).LE.ERLT)) ANGLE=PIO2
      IF (DABS(ANGLE-PIO2).LE.ERLT) GO TO 1136
      IF ((X.LT.0.0D0).AND.(DABS(Y).LE.ERLT)) ANGLE=-PIO2
      IF (DABS(ANGLE+PIO2).LE.ERLT) GO TO 1136
 1117 ANGLE=DACOS(Y/RDS)
      IF (IRD.EQ.-1) ANGLE=PI-ANGLE
      IF (X.LT.0.0D0) ANGLE=-ANGLE
 1136 CONTINUE
C     IF AGLMT1 AND AGLMT2 ARE ON THE LARGE THETA BRANCH,DO NOT PLOT SMALL THETA
C     BRANCH POINTS     
C     NOTE THAT ANGLE MOVES THROUGH MORE NEGATIVE VALUES ON THE SMALL THETA     
C     BRANCH     
C     If AGLMT1.EQ.ANG,plot all points
      IF (DABS(AGLMT1-ANG).LE.ERLT) GO TO 308
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 214
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 215
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 216
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 218
      CALL PMDSTOP      
  214 IF ((ANGLE.LT.AGLMT1).AND.(ANGLE.GT.AGLMT2)) GO TO 321      
      GO TO 308  
C     IF THE POINT FALLS BETWEEN AGLMT1 AND AGLMT2,OMIT IT 
  215 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 308      
      GO TO 321  
  216 IF (DABS(AGLMT2).LT.PIO2) GO TO 217
      IF ((ANGLE.LT.0.0D0).AND.(J.GT.IFC/2)) GO TO 321
      IF ((ANGLE.GT.AGLMT1).AND.(ANGLE.LT.(AGLMT2+TWPI))) GO TO 321      
      GO TO 308  
  217 IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 308
      GO TO 321
  218 IF (AGLMT1.GT.-PIO2) GO TO 219  
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 308      
      GO TO 321  
  219 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 308      
      GO TO 321  
C     IF WE ARRIVE HERE,THE ECLIPSED STAR IS CENTERED AT YG,XG    
  280 A=X+XG     
      B=Y+YG     
      RDS=DSQRT(A**2+B**2)      
C     A,B are coods.of point rel. to center of star at YG,XG
      ANGLE=DACOS(B/RDS)
      IF (IRD.EQ.-1) ANGLE=PI-ANGLE
      IF (A.LT.0.0D0) ANGLE=-ANGLE
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $	GO TO 308   
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 282
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 283
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 284
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 285
      CALL PMDSTOP      
  282 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 308      
      GO TO 321  
  283 IF ((ANGLE.GT.AGLMT1).AND.(ANGLE.LT.AGLMT2)) GO TO 321      
      GO TO 308  
  284 IF (AGLMT1.LT. PIO2) GO TO 386  
      IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) 
     $GO TO 308
      GO TO 321
  386 IF ((ANGLE.GT.AGLMT2).AND.(ANGLE.LT.AGLMT1)) 
     $GO TO 308      
      GO TO 321  
  285 IF  (AGLMT2.GT.PIO2) GO TO 288  
      IF ((ANGLE.LT.AGLMT2).AND.(ANGLE.GT.AGLMT1)) GO TO 321      
      GO TO 308  
  288 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 308      
      GO TO 321  
  308 IF (LCT(15).EQ.1) GO TO 78      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 310  
   78 CONTINUE   
      X=XP
      Y=YP
  310 CONTINUE
      WRITE (NOUT3, 193) X-XCM, -Y+YCM
  371 IF (ICOUNT.EQ.2) GO TO 321
      IF (J.NE.(IFC-1)) GO TO 321
      XNKSV2=X
      YNKSV2=Y
  321 CONTINUE
C  
C************************************************************************
C     PLOT POINTS OUTSIDE HORIZON BOUNDARY,ON CONNECTING NECK   
C************************************************************************
C
      DO 322 J=1,NKP     
      IF (DABS(XNK(J)-ANG).LE.ERLT) GO TO 322     
      IF (ICOUNT.EQ.2) GO TO 211      
C     ECLIPSING STAR    
      IF (LCT(12).NE.1) GO TO 322    
      YP=YNK(J)
      XP=XNK(J)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 79      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 303  
   79 CONTINUE   
      X=XP
      Y=YP
      GO TO 303  
C     ECLIPSED STAR     
  211 IF (LCT(8).NE.1) GO TO 322      
      YP=YNK(J)
      XP=XNK(J)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 80      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 303  
   80 X=XP
      Y=YP
  303 CONTINUE
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
  322 CONTINUE
C
C*************************************************************************
C     PLOT PROJECTIONS OF SHADOW BOUNDARIES  
C*************************************************************************
C  
      IF (ICOUNT.NE.2) GO TO 55
      IF (LCT(7).NE.1) GO TO 55
C
      DO 60 ITH=1,NTH   
      SNTV=SNT(I,ITH)   
      ANUV=ANU(I,ITH,1)   
   52 DO 59 J=1,3
      VGL=SHS(ITH,J)    
      IF (DABS(VGL-ANG).LE.ERLT) GO TO 59 
      CALL SGLTE(I,ITH,VGL,0.0000000001D0,SN,CS,RV)
      X=RV*SNTV*CS      
      Y=RV*SNTV*SN      
      Z=RV*ANUV  
      CALL PROJ(TARY,X,Y,Z,XP,YP,ZP)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 83      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 53   
   83 CONTINUE   
      X=XP
      Y=YP
   53 CONTINUE
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
  372 VGL=SHN(ITH,J)
      CALL SGLTE(I,ITH,VGL,0.0000000001D0,SN,CS,RV)
      X=RV*SNTV*CS      
      Y=RV*SNTV*SN      
      Z=RV*ANUV  
      CALL PROJ(TARY,X,Y,Z,XP,YP,ZP)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 84      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 54   
   84 CONTINUE   
      X=XP
      Y=YP
   54 CONTINUE
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
   59 CONTINUE   
   60 CONTINUE
   55 CONTINUE   
C*************************************************************************  
C     PLOT PROJECTED CENTER OF STAR 3-ISV    
C*************************************************************************
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
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
C**************************************************************************
C     PLOT PROJECTED CENTER OF STAR I 
C**************************************************************************
      WRITE (NOUT2, 193) X-XCM, -Y+YCM
C
C*************************************************************************** 
C     DRAW OUTLINES OF BOTH STARS
C     This program section assigns separators to the output file on
C     unit NOUT4. All of the outline data for both stars is produced in this
C     program section.
C     The outline data for any accretion disk are added in the accretion disk
C     section.      
C***************************************************************************
C  
C    IF I=ISV,WE HAVE THE STAR AT THE ORIGIN OF COODS.     
C    If ICOUNT=1, we have the eclipsing star.
C    If ICOUNT=2, we have the eclipsed star.
C    IGAIN designates the mass gainer, the star at the center of the 
C    accretion disk. ISV designates the star assigned to the origin
C    of coordinates. IEC is the eclipsing star.
C  
C    DO LARGE THETA BRANCH     
C
  373 IF (IGAIN.EQ.ISV) THEN
         IF (I.EQ.ISV) THEN
            IF (I.EQ.IEC) THEN
               LMA=IFCSP
               LMB=IRCSP
            ELSE
               LMA=IFCP
               LMB=IRCP
            END IF
         ELSE
            IF (I.EQ.IEC) THEN
               LMA=IFCSP
               LMB=IRCSP
            ELSE
               LMA=IFCP
               LMB=IRCP
            END IF
         END IF
      ELSE
         IF (I.NE.ISV) THEN
            IF (I.EQ.IEC) THEN
               LMA=IFCSP
               LMB=IRCSP
            ELSE
               LMA=IFCP
               LMB=IRCP
            END IF
         ELSE
            IF (I.EQ.IEC) THEN
               LMA=IFCSP
               LMB=IRCSP
            ELSE
               LMA=IFCP
               LMB=IRCP
            END IF
         END IF
      END IF
      K=0
C     Start with delimiter for this output section
C     ANGA=6.39. ANG=6.29.
      IF ((ICOUNT.EQ.1).AND.(I.EQ.IEC)) SPR=ANGA
      IF ((ICOUNT.NE.1).OR.(I.NE.IEC))  SPR=ANG
      WRITE (NOUT4,193) SPR,SPR
      WRITE (NOUT5,106) I,IEC,ISV,IGAIN,LMA,LMB,IFCP,IRCP,IFCSP,IRCSP
	WRITE (NOUT5,106) ICOUNT
      WRITE (NOUT5,193) SPR,SPR
C
C
	K=0
      IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(I.NE.IEC)) GO TO 221
	DO 220 J=1,IFC-1
      AGL1=ALP(J,1)
      RG=(ALP(J+1,1)-ALP(J,1))/16.D0
	JLM=16
	JKL=0
	IF (K.LE.0) THEN
	JLM=17
	JKL=1
	END IF
	IF ((AGLMT1.GT.ALP(J,1)).AND.(AGLMT1.LT.ALP(J+1,1))) GO TO 244
	GO TO 246
  244 JLM=16
      JKL=0
  246	ISKIPA=0
      IF (PRSH(I,J,1).LT.0.D0) GO TO 220
      DO 422 JJ=1,JLM
	CFF=JJ-JKL
      ANGLE=AGL1+CFF*RG
C     IF I.EQ.IEC WE HAVE THE ECLIPSING STAR.PLOT ALL OF ITS POINTS.
      IF (I.EQ.IEC) GO TO 222    
C    DO LARGE THETA BRANCH CASE WHEN THE LIMITING ANGLES ARE ON THE      
C     SMALL THETA BRANCH
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $	GO TO 222
      IF (ISV.EQ.IEC) GO TO 261
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 230   !both rts on lg theta
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 231   !both rts on sm theta
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 250
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 242
      CALL PMDSTOP      
  230 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222
      GO TO 220
  231 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.(AGLMT2+TWPI)))GO TO 222
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.GE.AGLMT2)) GO TO 222
      GO TO 220  
  250 IF (AGLMT2.LT.-PIO2) GO TO 260  
      IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 220
      GO TO 222
  260 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.(AGLMT2+TWPI)))GO TO 220
      GO TO 222  
  242 IF (AGLMT1.GT.-PIO2) GO TO 249  
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  249 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  261 IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 262   !both rts on lg theta
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 263   !both roots on sm theta
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 264
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 265
      CALL PMDSTOP      
  262 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  263 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.(AGLMT2+TWPI))) GO TO 222      
      GO TO 220  
  264 IF (AGLMT2.LT.-PIO2) GO TO 266  
      IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 222
      GO TO 220
  266 IF ((ANGLE.LE.AGLMT1).AND.(ANGLE.GE.AGLMT2)) GO TO 222
      GO TO 220
  265 IF (AGLMT1.LT.-PIO2) GO TO 267  
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  267 IF ((ANGLE.LT.AGLMT1).OR.(ANGLE.GT.AGLMT2)) GO TO 220      
      GO TO 222
  222 CALL RHOV(ANGLE,ARYA,ARYB,ARYC,ALP,RS,1,IRC,IFC,NKP,VRHO)   
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
      IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
      XP=B       
      IF (IGAIN.EQ.ISV) THEN
         IF(I.EQ.ISV) THEN
            IF (PRSH(I,J,1).LT.0.D0) THEN
		  GO TO 422
		  END IF
C            YP=VYP(I,J,1)
C            XP=VXP(I,J,1)
		  GO TO 540
         ELSE
C     Note that we reach here on ICOUNT=2, so I points at the companion
            IF (PRSH(I,J,1).LT.0.D0) THEN
		  GO TO 422
		  END IF
            GO TO 540
         END IF
      ELSE
         IF (I.NE.ISV) THEN
            IF (PRSH(I,J,1).LT.0.D0) THEN 
		  GO TO 422
		  END IF
		  GO TO 540
         ELSE
            IF (PRSH(I,J,1).LT.0.D0) THEN
		  GO TO 422
		  END IF
            GO TO 540
         END IF
      END IF
  540 IF (I.EQ.ISV) GO TO 224
      IF (ISV.NE.IEC) GO TO 243
      YP=YP-YG  
      XP=XP-XG   
      GO TO 224  
  243 YP=YP+YG   
      XP=XP+XG 
  224 CONTINUE
      K=K+1
      AZ(K)=ANGLE
      XV(K)=XP   
      YV(K)=YP   
   89 IF (LCT(15).EQ.1) GO TO 220
      YV(K)=XP*CSPJ-YP*SNPJ    
      XV(K)=XP*SNPJ+YP*CSPJ
	GO TO 422
  425 IF (ISKIPA.GT.0) GO TO 422
      K=K+1
      XV(K)=SPR
	YV(K)=SPR
	ISKIPA=ISKIPA+1
  422 CONTINUE
  220 CONTINUE
C     Entry point to terminate first segment of large theta branch
  286 KST=K
      AGLSV2=ALP(J+1,1)
      KLT=K
      IF ((ICOUNT.EQ.2).AND.(LCT(4).NE.1)) GO TO 221
      IF ((ICOUNT.EQ.1).AND.(LCT(12).NE.1)) GO TO 221
C     Draw outline, first segment of large theta branch
      DO L=1,K
      X=XV(L)
      Y=YV(L)
      Z=AZ(L)
	IF ((DABS(XV(L)-SPR).LE.ERLT).AND.(DABS(YV(L)-SPR).LE.ERLT)) THEN
		WRITE (NOUT4, 193) SPR,SPR
		GO TO 374
	END IF
      WRITE (NOUT4, 193) X-XCM, -Y+YCM
  374 END DO
      WRITE (NOUT4, 193) SPR,SPR
      WRITE (NOUT5, 193) SPR,SPR
  221 CONTINUE
C
C      Now do second segment of large theta branch, if necessary
      IF ((ICOUNT.EQ.1).AND.(I.EQ.IEC)) SPR=SP13
	IF ((ICOUNT.EQ.2).OR.(I.NE.IEC))  SPR=SP14
	IF (ICOUNT.EQ.ISV) GO TO 4240
	IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(J.GE.LMB)) GO TO 4240
	IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(I.NE.IEC)) GO TO 4240
	IF ((AGLMT2.LE.0.D0).AND.(AGLMT1.GE.0.D0)) THEN
		IF (NRING.EQ.0) GO TO 4240
	END IF
	WRITE (NOUT4,193) SPR,SPR
	WRITE (NOUT5,193) SPR,SPR
C     Reinitialize counter for plotted points
	K=0
C     Set AGL2 for last possible angle
      AGL2=ALP(IFC,1)
C     Set starting angle from value saved
      AGL1=AGLSV2
	RG=(AGL2-AGL1)/(IFC-1)
	WRITE (NOUT1,106) I,IEC,ISV,J+1,IFC
	WRITE (NOUT1,112) AGL1,AGL2,RG,AGLMT1,AGLMT2
	IF (DABS(RG).LT.1.D-3) GO TO 3770
C     Skip if there is no eclipse
C     Set starting value of index
      JST=J+1
	DO 2280 J=JST,IFC
	IF (PRSH(I,J,1).LT.0.D0) GO TO 2280
	ANGLE=AGL1+(J-1)*RG
	WRITE (NOUT1,106) J
	WRITE (NOUT1,112) ANGLE,PRSH(I,J,1)
      CALL RHOV(ANGLE,ARYA,ARYB,ARYC,ALP,RS,1,IRC,IFC,NKP,VRHO)
      A=VRHO*DCOS(ANGLE)
	B=VRHO*DSIN(ANGLE)
	YP=A
	IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
	IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
	XP=B
	IF (I.EQ.ISV) GO TO 2270
	IF (ISV.NE.IEC) GO TO 2470
	YP=YP-YG
	XP=XP-XG
	GO TO 2270
 2470 YP=YP+YG
      XP=XP+XG
 2270 CONTINUE
      K=K+1
	AZ(K)=ANGLE
	XV(K)=XP
	YV(K)=YP
	IF (I.NE.ISV) GO TO 9400
 9400 IF (LCT(15).EQ.1) GO TO 2280
      YV(K)=XP*CSPJ-YP*SNPJ
	XV(K)=XP*SNPJ+YP*CSPJ
 2280 CONTINUE
 2281 KLT=K
	IF ((ICOUNT.EQ.2).AND.(LCT(4).NE.1)) GO TO 2290
	IF ((ICOUNT.EQ.1).AND.(LCT(12).NE.1)) GO TO 2290
C     Draw outline
      DO L=1,K
	X=XV(L)
	Y=YV(L)
	Z=AZ(L)
	WRITE (NOUT4,193) X-XCM,-Y+YCM
      END DO
 3770 WRITE (NOUT4,193) SPR,SPR
      WRITE (NOUT5,193) SPR,SPR
 4240 CONTINUE
 2290 CONTINUE 												      
C  
C    DO SMALL THETA BRANCH
C
C
C     If orbital inclination is small enough,two segments of small
C     theta branch may show.
C     DIVIDE SMALL THETA BRANCH INTO TWO SEGMENTS IF NECESSARY    
C     THE PROGRAM LOOP THROUGH STATEMENT 235 IS THE INITIAL SEGMENT      
C  
C     The following section parallels DOUT2 of PLTB
C
      K=0
      KJPZZ=0
	IF ((ICOUNT.EQ.1).AND.(I.EQ.IEC))SPR=SP1
	IF ((ICOUNT.EQ.2).OR.(I.NE.IEC)) SPR=SP7
	WRITE (NOUT4,193) SPR,SPR
	K=0
	IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(I.NE.IEC)) GO TO 316
	DO 235 J=1,IRC-1
      AGL1=ALP(J,2)
      RG=(ALP(J+1,2)-ALP(J,2))/16.D0
	JLM=16
	JKL=0
	IF (K.GT.0) GO TO 234
	JLM=17
	JKL=1
  234 IF ((AGLMT2.LT.ALP(J,2)).AND.(AGLMT2.GT.ALP(J+1,2))) GO TO 236
      GO TO 241
  236 JLM=16
      JKL=0
  241	ISKIPA=0
      IF (PRSH(I,J,2).LT.0.D0) GO TO 235
      IF (PRSH(I,J,2).GE.0.D0) THEN
		IF (PRSH(I,J+1,2).LT.0.D0) GO TO 226
	END IF
      DO 423 JJ=1,JLM
      CFF=JJ-JKL
      ANGLE=AGL1+CFF*RG
      JRF=2
      IF (NKP.LE.1) GO TO 257
      IF ((ANGLE.LT.ALP(1,3)).AND.(ANGLE.GE.AGL2)) JRF=3
      IF (DABS(ANGLE-AGL2).LT.ERLT) JRF=3
  257 IF (ICOUNT.EQ.ISV) GO TO 223    
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $	GO TO 223
      IF (ISV.EQ.IEC) GO TO 290
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 251
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 252
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 253
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 255
      CALL PMDSTOP      
  251 IF (ANGLE.GE.AGLMT1) GO TO 223
      GO TO 226
C     If the following test is met, terminate the initial branch
  252 IF ((ANGLE.LT.AGLMT1).AND.(ANGLE.GT.AGLMT2)) GO TO 226
      GO TO 223
C  252 IF (ANGLE.GE.AGLMT1) GO TO 223  
C      GO TO 226  
  253 IF (DABS(AGLMT2).LT.PIO2) GO TO 254
      IF (ANGLE.GE.AGLMT2) GO TO 223
      GO TO 423  
  254 IF (ANGLE.GE.AGLMT2) GO TO 423
      GO TO 223
  255 IF (AGLMT1.GT.-PIO2) GO TO 256  
      IF (ANGLE.GE.AGLMT1) GO TO 223  
      GO TO 423  
  256 IF (ANGLE.GE.AGLMT1) GO TO 223  
      GO TO 423  
  290 IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 292
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 293
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 294
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 295
      CALL PMDSTOP      
  292 IF (ANGLE.GE.AGLMT1) GO TO 223
      GO TO 423
  293 IF (ANGLE.GE.AGLMT1) GO TO 223  
      GO TO 423  
  294 IF (AGLMT2.GT.-PIO2) GO TO 296  
      IF ((ANGLE.LE.AGLMT1).AND.(ANGLE.GE.AGLMT2)) GO TO 223
      GO TO 423 
  296 IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 223  
      GO TO 423  
  295 IF (AGLMT1.GT.-PIO2) GO TO 297  
      IF ((ANGLE.LT.AGLMT1).OR.(ANGLE.GT.AGLMT2)) GO TO 423  
      GO TO 223  
  297 IF ((ANGLE.LT.AGLMT1).OR.(ANGLE.GT.AGLMT2)) GO TO 423  
      GO TO 223
  223 CALL RHOV(ANGLE,ARYA,ARYB,ARYC,ALP,RS,JRF,IRC,IFC,NKP,VRHO)
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
      IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
      XP=B
  541 IF (I.EQ.ISV) GO TO 225
      IF (ISV.NE.IEC) GO TO 245
      YP=YP-YG  
      XP=XP-XG  
      GO TO 225
  245 YP=YP+YG  
      XP=XP+XG
  225 CONTINUE
   15 K=K+1
      AZ(K)=ANGLE
      XV(K)=XP   
      YV(K)=YP   
   93 IF (LCT(15).EQ.1) GO TO 423
      YV(K)=XP*CSPJ-YP*SNPJ    
      XV(K)=XP*SNPJ+YP*CSPJ
  423 CONTINUE
  235 CONTINUE
C     Entry point to terminate first segment of small theta branch.
  226 KST=K
      AGLSV2=ALP(J+1,2)
C     Draw outline, first segment of small theta branch.
      DO L=1,K
      X=XV(L)
      Y=YV(L)
      Z=AZ(L)
	IF ((XV(L).EQ.SPR).AND.(YV(L).EQ.SPR)) THEN
		WRITE (NOUT4, 193) SPR,SPR
		GO TO 375
	END IF
      WRITE (NOUT4, 193) X-XCM, -Y+YCM
  375 END DO
      WRITE (NOUT4,193) SPR,SPR
      WRITE (NOUT5,193) SPR,SPR
  316 CONTINUE
      WRITE (NOUT5,106) IRC
      WRITE (NOUT5,112) XCM,YCM
      WRITE (NOUT5,114)
      WRITE (NOUT5,112) (VXP(I,K,2),K=1,IRC)
      WRITE (NOUT5,114)
      WRITE (NOUT5,112) (VYP(I,K,2),K=1,IRC)
      WRITE (NOUT5,114)
      WRITE (NOUT5,112) (PRSH(I,K,2),K=1,IRC)

C  
C  
C     NOW DO SECOND SEGMENT IF NECESSARY     
C
C     The following section parallels DOUT3 of PLTB
C
      IF ((ICOUNT.EQ.1).AND.(I.EQ.IEC)) SPR=SP6
	IF ((ICOUNT.EQ.2).OR.(I.NE.IEC)) SPR=SP8
      IF (ICOUNT.EQ.ISV) GO TO 424
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(J.GE.LMB)) GO TO 424
      IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(I.NE.IEC)) GO TO 424
      IF ((AGLMT2.LE.0.0D0).AND.(AGLMT1.GE.0.0D0)) THEN
		IF (NRING.EQ.0) GO TO 424
	END IF
      WRITE (NOUT4,193) SPR,SPR
	WRITE (NOUT5,193) SPR,SPR
C     Reinitialize counter for plotted points.
      K=0
C     Set starting value of index.
      JST=J+1
C     Set upper limit of J to avoid equatorial point.
  314 DO 228 J=JST,IRC-1
      ANGLE=ALP(J,2)  
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $	GO TO 213
      IF (ISV.EQ.IEC) GO TO 330
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 340
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 341
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 342
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 343
      CALL PMDSTOP      
  340 IF (ANGLE.LT.(AGLMT2-TWPI)) GO TO 213  
      GO TO 228  
  341 IF (ANGLE.LT.AGLMT2) GO TO 213  
      GO TO 228  
  342 IF (AGLMT2.GT.-PIO2) GO TO 344  
      IF (ANGLE.LT.AGLMT2) GO TO 228  
      GO TO 213  
  344 IF (ANGLE.LT.AGLMT2) GO TO 228  
      GO TO 213  
  343 IF (AGLMT2.LT.PIO2) GO TO 345   
      IF (ANGLE.LT.(AGLMT2-TWPI)) GO TO 213  
      GO TO 228  
  345 IF (ANGLE.GT.AGLMT1) GO TO 213  
      GO TO 228  
  330 IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 332
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 333
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 334
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 335
      CALL PMDSTOP      
  332 IF (ANGLE.GT.(AGLMT2-TWPI)) GO TO 228  
      GO TO 213  
  333 IF (ANGLE.GT.AGLMT2) GO TO 228  
      GO TO 213  
  334 IF (AGLMT2.GT.-PIO2) GO TO 336  
      IF (ANGLE.GT.AGLMT2) GO TO 228  
      GO TO 213  
  336 IF (ANGLE.LT.AGLMT2) GO TO 213  
      GO TO 228  
  335 IF (AGLMT1.GT.-PIO2) GO TO 337  
      IF (ANGLE.GT.AGLMT1) GO TO 228  
      GO TO 213  
  337 IF (ANGLE.GT.(AGLMT2-TWPI)) GO TO 228  
      GO TO 213
  213 CALL RHOV(ANGLE,ARYA,ARYB,ARYC,ALP,RS,2,IRC,IFC,NKP,VRHO)   
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
	IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
      XP=B
      IF (ICOUNT.EQ.2) THEN
         IF (PRSH(I,J,2).LT.0.D0) GO TO 228
      END IF
      IF (I.EQ.ISV) GO TO 227  
      IF (ISV.NE.IEC) GO TO 247
      YP=YP-YG   
      XP=XP-XG   
      GO TO 227  
  247 YP=YP+YG   
      XP=XP+XG   
  227 CONTINUE   
      K=K+1
      AZ(K)=ANGLE
      XV(K)=XP   
      YV(K)=YP   
      IF (I.NE.ISV) GO TO 94   
   94 IF (LCT(15).EQ.1) GO TO 228
      YV(K)=XP*CSPJ-YP*SNPJ    
      XV(K)=XP*SNPJ+YP*CSPJ
  228 CONTINUE
      KTT=K
      IF ((ICOUNT.EQ.2).AND.(LCT(4).NE.1)) GO TO 229
      IF ((ICOUNT.EQ.1).AND.(LCT(12).NE.1)) GO TO 229
C     Draw outline
      DO L=1,K
      X=XV(L)
      Y=YV(L)
      Z=AZ(L)
      WRITE (NOUT4, 193) X-XCM,-Y+YCM
  376 END DO
  377 WRITE (NOUT4,193) SPR,SPR
      WRITE (NOUT5,193) SPR,SPR
  424 CONTINUE
  229 CONTINUE
      KLTSV=KLT
      KSTSV=KST
      KTTSV=KTT
      PSI=PSI+PI 
      IF (PSI.GT.TWPI) PSI=PSI-TWPI   
      CM=TWPI-PSI
      IF (CM.LT.0.0D0) CM=CM+TWPI
      IF (CM.GE.TWPI) CM=0.0D0
      IF (PSI.GE.TWPI) PSI=0.0D0
      I=3-I      
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)  
      IF ((PSI.GE.PIO2).AND.(PSI.LE.TPIO2)) GO TO 102      
      Q=QN
      GO TO 101  
  102 Q=QS
  101 CONTINUE
*******************************************************************
C     Accretion disk section
C********************************************************************
  510 REWIND NSCR
  402 DPX=0.D0
      DPY=0.D0
      DLX=0.D0
      DLY=0.D0
C
      ANGLO=0.0D0
      CSPJ=COS(ANGLO)
      SNPJ=SIN(ANGLO)
C     Start of plot section for accretion disk
C
C
      IFLAG=0
	ITST=0
	DO K=1,NRM
		IF (IFLAG.EQ.0) THEN
			DO J=1,NSG
				IF (CSGR(K,J).GE.0) ITST=ITST+1
			END DO
			IF (ITST.GT.0) IFLAG=K
		END IF
	END DO
      DO 520 K=1,NRM
         KY=0
         DO 519 J=1,NSG
C     Omit points whose normal makes angle of more than PIO2, if
C     specified
         IF ((CSGR(K,J).LT.0.0).AND.(LCW(2).NE.1)) GO TO 534
C     Omit points flagged not to plot.
         IF (PRRH(K,J).LT.0.D0) GO TO 534
         XP=PXR(K,J)
         YP=PYR(K,J)
         IF (LCT(15).EQ.1) GO TO 516
         Y=XP*CSPJ-YP*SNPJ+DPX
         X=XP*SNPJ+YP*CSPJ+DPY
         GO TO 515
  516    X=XP+DPX
         Y=YP+DPY
  515 WRITE (NOUT2,113) X,-Y
         KY=0
         GO TO 519
  534    KY=KY+1
  519    CONTINUE
  520 CONTINUE
C
C     List bright spot points in order of distance along rim as inner
C     loop and distance vertically on rim as outer loop. This is for 
C     ease in subsequent plotting.
	KZ=0
	KY=0
      DO 820 J=1,NSG
         KY=0
         DO 819 K=1,NRM
C     Omit points whose normal makes angle of more than PIO2, if
C     specified
         IF ((CSGR(K,J).LT.0.0).AND.(LCW(2).NE.1)) GO TO 819
C     Omit points flagged not to plot.
         IF (PRRH(K,J).LT.0.D0) GO TO 819
         XP=PXR(K,J)
         YP=PYR(K,J)
         IF (LCT(15).EQ.1) GO TO 816
         Y=XP*CSPJ-YP*SNPJ+DPX
         X=XP*SNPJ+YP*CSPJ+DPY
         GO TO 815
  816    X=XP+DPX
         Y=YP+DPY
  815     IF (NHSP(J).EQ.1) THEN
			KY=KY+1
			KZ=KZ+1
			BFX(KZ)=X
			BFY(KZ)=-Y
	    END IF
  819    CONTINUE
	   IF (KY.NE.0) KYSV=KY
  820 CONTINUE
         KY=KYSV
		DO KK=1,KY
			WRITE (NOUT8,113) BFX(KK),BFY(KK)
		END DO
		DO KK=1,KY
			WRITE (NOUT8,113) BFX(KZ-KK+1),BFY(KZ-KK+1)
		END DO
C
C     *****************************************************************
C     Write out data for top rim for outline plot
      SPR=SP4
      WRITE (NOUT4,113) SPR,SPR
C     Use IFLAG to choose the top visible rim circle
      DO 560 J=1,NSG
      XP=PXR(IFLAG,J)
      YP=PYR(IFLAG,J)
C     Restrict plot to visible part of rim circle.
c      IF (CSGR(IFLAG,J).LT.0.D0) GO TO 560
c      IF (CSGR(IFLAG,J).GE.0.D0) THEN
		IF (J.EQ.1) THEN
			IF (PRRH(IFLAG,J).LT.0.D0) GO TO 560
			GO TO 571
		END IF
C     Locate horizon crossing from hidden to visible.
c		IF ((CSGR(IFLAG,J-1).LT.0.D0).AND.(J.GT.1)) THEN
C     Terminate this segment of top rim plot and switch to second 
C     segment.
c				WRITE (NOUT4,113) SPR,SPR
c				SPR=SP9
c				WRITE (NOUT4,113) SPR,SPR
c				IF (PRRH(IFLAG,J).LT.0.D0) GO TO 560
c				GO TO 571
c		END IF
C     Special case for orbital inclination of 90 degrees.
C     The values of CSGR(IFLAG,J) are all equal and positive.
C     Must locate end of first plot section by sign change of
C     PRRH(IFLAG,J).
		IF ((PRRH(IFLAG,J-1).GE.0.D0).AND.(PRRH(IFLAG,J).LT.0.D0))THEN
			IF (SPR.EQ.SP4)	THEN
				WRITE (NOUT4,113) SPR,SPR
				SPR=SP9
				WRITE (NOUT4,113) SPR,SPR
				GO TO 560
			END IF
		END IF
c	END IF
c	IF ((CSGR(IFLAG,J-1).GE.0.D0).AND.(J.GT.1)) THEN
		IF (PRRH(IFLAG,J).LT.0.D0) GO TO 560
c		GO TO 571
c	END IF
  571 IF (LCT(15).EQ.1) GO TO 555
      Y=XP*CSPJ-YP*SNPJ+DPX
      X=XP*SNPJ+YP*CSPJ+DPY
      GO TO 556
  555 X=XP+DPX
      Y=YP+DPY
  556 WRITE (NOUT4,113) X,-Y
C      WRITE (NOUT1,113) X,-Y
  560 CONTINUE
C     Write terminator for top rim plot.
      WRITE (NOUT4,113) SPR,SPR
C     Write out points on lower rim for outline plot
C     Note that we plot the bottom rim whether or not
C     the value of CSGR is negative.
C     Write key to designate bottom rim plot.
      SPR=SP5
      WRITE (NOUT4,113) SPR,SPR
      DO J=1,NSG
      XP=PXR(NRM,J)
      YP=PYR(NRM,J)
      IF ((PRRH(NRM,J).GE.0.D0).AND.(J.EQ.1)) GO TO 572
	IF ((PRRH(NRM,J).LT.0.D0).AND.(J.EQ.1)) GO TO 570
	IF ((PRRH(NRM,J).GE.0.D0).AND.(PRRH(NRM,J-1).LT.0.D0)) GO TO 572
	IF ((PRRH(NRM,J).GE.0.D0).AND.(PRRH(NRM,J-1).GE.0.D0)) GO TO 572
	IF ((PRRH(NRM,J).LT.0.D0).AND.(PRRH(NRM,J-1).GE.0.D0)) THEN
C     Write terminator for this rim segment and switch to next 
C     segment.
		WRITE (NOUT4,113) SPR,SPR
		IF (SPR.EQ.SP15) SPR=SP16
		IF (SPR.EQ.SP10) SPR=SP15
		IF (SPR.EQ.SP5) SPR=SP10
		WRITE (NOUT4,113) SPR,SPR
		GO TO 570
	END IF
	GO TO 570
  572 IF (LCT(15).EQ.1) GO TO 569
      Y=XP*CSPJ-YP*SNPJ+DPX
      X=XP*SNPJ+YP*CSPJ+DPY
      GO TO 568
  569 X=XP+DPX
      Y=YP+DPY
  568 WRITE (NOUT4,113) X,-Y
  570 END DO
C     Write terminator for bottom rim plot.
      WRITE (NOUT4,113) SPR,SPR
C
C     Plot rim horizon (vertical face)
C
      NRMHZ=NRM-IFLAG+1
	 SPR=SP2
      WRITE (NOUT4,113) SPR,SPR
      IF (NRING.NE.0) THEN
		DO J=1,NRMHZ
		XP=RXHZG(J)
		YP=RYHZG(J)
		IF (LCT(15).EQ.1) GO TO 528
		Y=XP*CSPJ-YP*SNPJ+DPX
		X=XP*SNPJ+YP*CSPJ+DPY
		GO TO 518
  528		X=XP+DPX
		Y=YP+DPY
  518		IF ((PRVFG(J).LT.0.D0).AND.(J.EQ.1)) GO TO 550
		IF ((PRVFG(J).GE.0.D0).AND.(J.EQ.1)) THEN
			WRITE (NOUT4,113) X,-Y
			GO TO 550
		END IF
		IF ((PRVFG(J).GE.0.D0).AND.(PRVFG(J-1).GE.0.D0)) THEN
			WRITE (NOUT4,113) X,-Y
			GO TO 550
		END IF
		IF ((PRVFG(J).GE.0.D0).AND.(PRVFG(J-1).LT.0.D0)) THEN
			WRITE (NOUT4,113) SPR,SPR
			SPR=SP11
			WRITE (NOUT4,113) SPR,SPR
			WRITE (NOUT4,113) X,-Y
		END IF
  550		CONTINUE
		END DO
	END IF
	WRITE (NOUT4,113) SPR,SPR
C
C     Now do other horizon (vertical face)
C
      SPR=SP3
      WRITE (NOUT4,113) SPR,SPR
      IF (NRING.NE.0) THEN
		DO J=1,NRMHZ
		XP=RXHZL(J)
		YP=RYHZL(J)
		IF (LCT(15).EQ.1) GO TO 530
		Y=XP*CSPJ-YP*SNPJ+DPX
		X=XP*SNPJ+YP*CSPJ+DPY
		GO TO 521
  530		X=XP+DPX
		Y=YP+DPY
  521		IF ((PRVFL(J).LT.0.D0).AND.(J.EQ.1)) GO TO 551
		IF ((PRVFL(J).GE.0.D0).AND.(J.EQ.1)) THEN
			WRITE (NOUT4,113) X,-Y
			GO TO 551
		END IF
		IF ((PRVFL(J).GE.0.D0).AND.(PRVFL(J-1).GE.0.D0)) THEN
			WRITE (NOUT4,113) X,-Y
			GO TO 551
		END IF
		IF ((PRVFL(J).GE.0.D0).AND.(PRVFL(J-1).LT.0.D0)) THEN
			WRITE (NOUT4,113) SPR,SPR
			SPR=SP12
			WRITE (NOUT4,113) SPR,SPR
			WRITE (NOUT4,113) X,-Y
		END IF
  551		CONTINUE
		END DO
	END IF
      WRITE (NOUT4,113) SPR,SPR
C     Plot central star
      READ (NSCR,194, END=526) XP,YP,ANGLE
      IF (XP.LT.5.D0) CALL PMDSTOP
         WRITE (NOUT5,193) XP,YP
         SPR=XP
C     Omit hidden points
      DO J=1,KLTSV
      READ (NSCR,194, END=526) XP,YP,ANGLE
  503 IF (J.EQ.1) GO TO 440
      IF (J.EQ.KLTSV) GO TO 440
      IF ((PRSH(I,J,1).LT.0.0).AND.(PRSH(I,J-1,1).LT.0.0)) GO TO 439
      IF ((PRSH(I,J-1,1).LT.0.D0).AND.(PRSH(I,J,1).GE.0.D0)) WRITE
     $(NOUT4,113)SP1,SP1
      IF ((PRSH(I,J-1,1).GE.0.D0).AND.(PRSH(I,J,1).LT.0.D0)) WRITE
     $(NOUT4,113)SP1,SP1
  440 IF (PRSH(I,J,1).LT.0.0) GO TO 439
      IF (LCT(15).EQ.1) GO TO 436
      Y=XP*CSPJ-YP*SNPJ+DPX
      X=XP*SNPJ+YP*CSPJ+DPY
      GO TO 438
  436 X=XP+DPX
      Y=YP+DPY
  438 RDVL=DSQRT((Y-YG)**2+(X-XG)**2)
      IF ((X-XG).GE.0.D0) THEN
         ANGLE=PI-DASIN((X-XG)/RDVL)
      ELSE
         ANGLE=-PI-DASIN((X-XG)/RDVL)
      END IF
      IF (X.GT.XG) THEN
	  CALL RHOV(ANGLE,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,
     $IFCS,NKPS,RDS)
	 END IF
      IF (X.LE.XG) THEN
	  CALL RHOV(ANGLE,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,
     $IFCS,NKPS,RDS)
	 END IF
      ER=RDVL-RDS
      IF (ER.LT.0.D0) GO TO 439
      WRITE (NOUT5,113) X,-Y
  439 END DO
      WRITE (NOUT5, 193) SPR,SPR
C     Plot face points
C     Starting point is on side nearer observer, in middle
C     Go around circles in CCW direction
  526 REWIND NSCR
      WRITE (NSCR,106) NRING
      DO 525 K=1,NRING
	   WRITE (NSCR,106) K,NSG
         KY=0
         DO 524 J=1,NSG
C     Omit points not to be plotted
         IF ((LCW(1).NE.1).AND.(LCW(2).NE.1)) GO TO 535
C     Omit points that are hidden
         IF ((PRFH(K,J).LT.0.0).AND.(LCW(2).NE.1)) GO TO 535
         XP=PXF(K,J)
         YP=PYF(K,J)
         IF (LCT(15).EQ.1) GO TO 522
         Y=XP*CSPJ-YP*SNPJ+DPX
         X=XP*SNPJ+YP*CSPJ+DPY
         GO TO 532
  522    X=XP+DPX
         Y=YP+DPY
  532 WRITE (NOUT2,113) X,-Y
	WRITE (NSCR,113) X,-Y
         KY=0
C     Include top face if all points are plotted
         IF (LCW(2).NE.1) GO TO 524
         IF (J.EQ.1) GO TO 533
         IF (KY.GE.1) GO TO 524
         IF (LCW(3).NE.1) GO TO 533
  533 WRITE (NOUT2,113) X,-Y
      WRITE (NSCR,113) X,-Y
         KY=0
         GO TO 524
  535    KY=KY+1
  524    CONTINUE
  525 CONTINUE
C**********************************************************************
  100 CONTINUE
C**********************************************************************
   99 CLOSE (UNIT=  IN1, STATUS='KEEP')
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
      CLOSE (UNIT= IN13, STATUS='KEEP')
	CLOSE (UNIT= IN14, STATUS='KEEP')
	CLOSE (UNIT= IN15, STATUS='KEEP')
	CLOSE (UNIT= IN16, STATUS='KEEP')
	CLOSE (UNIT= IN17, STATUS='KEEP')
	CLOSE (UNIT= IN18, STATUS='KEEP')
	CLOSE (UNIT= IN19, STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      CLOSE (UNIT=NSCR,  STATUS='KEEP')
      CLOSE (UNIT=NOUT6, STATUS='KEEP')
      CLOSE (UNIT=NOUT7, STATUS='KEEP')
	CLOSE (UNIT=NOUT8, STATUS='KEEP')
      CLOSE (UNIT=NOUT9, STATUS='KEEP')
      CLOSE (UNIT=NOUT10,STATUS='KEEP')
	CLOSE (UNIT=NOUT11,STATUS='KEEP')
      CLOSE (UNIT=32,    STATUS='KEEP')
	  OPEN (UNIT=IN19, FILE= FL1(31), STATUS = 'OLD')
	  WRITE (IN19,601) KEYGOOD
	  CLOSE (UNIT=IN19, STATUS = 'KEEP')
C**********************************************************************
      STOP
  199 WRITE (NOUT1,111)     
      GO TO 99   
  299 CALL PMDSTOP
  399 CALL PMDSTOP
      END  PROGRAM PLTDD
