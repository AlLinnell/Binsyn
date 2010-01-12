      PROGRAM ACPGB

C     Program to calculate projection of accretion disk on plane of sky,
C     and produce visibility key data.
C
C
C     ***************
C     Version ACPGB1
C     ***************
C  
C
C     Important internal variables in ACPGB
C		Rim variables
C     -----------------------------------------------------------
C     PHIR(J)		Values of azimuth of grid point (Same for all
C				rings on rim). Zero point in direction of center
C				of companion. Direction of measurement: positive
C				in right hand system, viewed from below acc. disk.
C     CSPH(J)		cos(phir(j))
C     SNPH(J)		sin(phir(j))
C     CSGR(K,J)		cos(gamma(point k,j))
C     PRRH(K,J)   Visibility keys for rim grid points
C     VXAR(K,J),VYAR(K,J),VZAR(K,J)
c				Spatial coods. of rim grid points relative to center
C				of central star.
C     PXR(K,J),PYR(K,J),PZR(K,J)
C				Plane of sky and perpendicular direction coods. of 
C				rim	grid points. Origin initially at center of
C				central star. Subsequently transferred to projected
C				center of mass as origin.
C     XA,YA		Plane of sky coods. of bottom edge of rim point at
C				first max. elongation on projected ellipse. Origin
C				at center of central star.
C     XB,YB		Plane of sky coods., similar to (XA,YA), but second
C				max. elongation.
C     XRMC,YRMC	Mean of (XA,YA),(XB,YB). Locates center of ellipse 
C				which is projection on plane of sky of bottom edge
C				of rim. Origin at center of central star.
C     RD(J)       Plane of sky radii from central star center to points 
C				on bottom edge of rim.
C     AGLD(J)		Position angles of RD(J). AGLD(J)=0.0 for topmost
C				point on ellipse.
C
C		Face of accretion disk variables
C     -----------------------------------------------------------------
C     CSGF(K,J)	Values of cos(gamma(point k,j))
C     VXAF(K,J),VYAF(K,J),VZAF(K,J)
C				Spatial coods. of grid points defining accretion
C				disk face.
C     PXF(K,J),PYF(K,J),PZF(K,J)
C				Plane of sky and perpendicular direction coods. of
C				face grid points. Origin initially at center of
C				central star, then transferred to projected center
C				of mass coods.
C     PRFH(H,J)   Visibility keys for face of accretion disk
C
C		Star variables
C     -----------------------------------------------------------------
C     VR(I,ITH,K)		Radii of photosphere points, current star 
C				
C     ALM(I,ITH,K),AMU(I,ITH,K),ANU(I,ITH,K)
C				Direction cosines of radii to photosphere points,
C				all points.
C     STX(K,J),STY(K,J),STZ(K,J)
C				Plane of sky and perpendicular direction coods.
C				of all points on star photospheres. Initially
C				relative to center of star in question, then
C				transformed to projected center of mass of system.
C     STVS(I,ITH,K)	Visibility keys, all points on both stars,
C	
C     PRSH(I,K,L) Visibility keys for star boundaries
C     VXP(I,K,L), VYP(I,K,L)  Projected coordinates of star boundaries.
C
C     -----------------------------------------------------------------
C
      INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
      DIMENSION TARY(3,3)      
      DIMENSION AGLD(MSG),RD(MSG),XHZ(MSG),YHZ(MSG)
      DIMENSION STX(2,MTHM,MPHM),STY(2,MTHM,MPHM),STZ(2,MTHM,MPHM),
     $STVS(2,MTHM,MPHM)
      DIMENSION RVLD(MSG),ANG(MSG),RMLX(MRM),RMLY(MRM),
     $RMGX(MRM),RMGY(MRM),VC(MRM)
      DIMENSION FL1(16)
      DIMENSION STXA(2,MTHM,MPHM),STYA(2,MTHM,MPHM)
C****************************************************
      DIMENSION CSPH(MSG),SNPH(MSG),CSGR(MRM,MSG),CSGF(MRING,MSG),
     $PXR(MRM,MSG),
     $PYR(MRM,MSG),PZR(MRM,MSG),PXF(MRING,MSG),PYF(MRING,MSG),
     $PZF(MRING,MSG),
     $PRFH(MRING,MSG),PRSH(2,MPHM,2),PRRH(MRM,MSG),RXHZL(MRING),
     $RYHZL(MRING),
     $RXHZG(MRING),RYHZG(MRING),PRVFG(MRING),PRVFL(MRING),
     $VXP(2,MPHM,2),VYP(2,MPHM,2)
C****************************************************
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 28 FL1
      CHARACTER * 7 LABEL
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD, KEYBAD
      CHARACTER * 10 DATE,TIME,ZONE^M
      DIMENSION TIMEVALUES(8)^M
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES^M
      DATA IN2,IN3,IN4,IN5,IN6,IN7,NOUT1,NOUT2,NOUT3/2,3,4,5,6,7,8,9,10/
      DATA NOUT4,NOUT5,NOUT6,NOUT7/11,12,13,14/
	DATA NOUT8/16/
	DATA IN8/15/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
	DATA TWPI/6.2831853071795864769D0/
	DATA PI/3.141592653589793238463D0/
  100 FORMAT (A28)
  101 FORMAT (I2)
  103 FORMAT (1H1)      
  104 FORMAT (1X,'PROGRAM ACPGB',28X,'DATE=',I2,':',I2,':',I4,
     $2X,'TIME=',I2,':',I2,':',I2)
  105 FORMAT (1X,'ORDINAL VALUE OF ORBITAL LONGITUDE=',I5) 
  106 FORMAT (10I5)     
  107 FORMAT (1H )      
  108 FORMAT (4F18.13)  
  109 FORMAT (1X,'CONTROL INTEGER=',I5,/,' IF THE CONTROL INTEGER=0,
     $CALCULATE THE ORBITAL PHASE VALUES')
  110 FORMAT (1X,'NUMBER OF ORBITAL PHASE VALUES INPUT=',4X,I5)
  112 FORMAT (1X,'NUMERICAL VALUE OF ORBITAL INCLINATION=',F20.12)    
  113 FORMAT (1X,'NUMERICAL VALUES OF ORBITAL PHASE=')     
  114 FORMAT (1X,'CALCULATED VALUES OF ORBITAL PHASE',/,1X,
     $'NUMBER OF POINTS BETWEEN ORBITAL PHASE 0.0 AND INTERNAL CONTACT='
     $,I5,/,1X,
     $'NUMBER OF POINTS BETWEEN INTERNAL AND EXTERNAL CONTACT='
     $,I5,/,1X,
     $'NUMBER OF POINTS BETWEEN EXTERNAL CONTACT AND MAXIMUM ELONGATION=
     $',I5,/,1X,
     $'TOTAL NUMBER OF POINTS BETWEEN PHASE 0.0 AND PHASE OF MID-SECONDA
     $RY=',I5,/,1X,'ORDINAL VALUE OF STARTING ORBITAL LONGITUDE=',I5,5X,
     $'ORDINAL VALUE OF FINAL ORBITAL LONGITUDE=',I5)      
  115 FORMAT (1X,'PHASE OF MID-SECONDARY ECLIPSE=',7X,F20.12)
  116 FORMAT (1X,'REVISED NUMBERS OF POINTS IN CRITICAL PHASE RANGES=',
     $4I5,/,1X,'REVISED ORDINAL STARTING LONGITUDE=',6X,I5,/,1X,
     $'REVISED ORDINAL FINAL LONGITUDE=',9X,I5)
  117 FORMAT (1X,'ORBITAL LONGITUDE=',F15.10,5X,'ORBITAL INCLINATION=',
     $F15.10)
  121 FORMAT (1X,'DEPTH OF ECLIPSE=',F12.5)  
  125 FORMAT (5X,9F14.6)      
  122 FORMAT (1X,'TOO LARGE ORBITAL INCLINATION, VALUE=', F12.6)
  127 FORMAT (7F18.13)
  139 FORMAT (3F10.5)
  305 FORMAT (1X, 'Program rumming time=',F12.4,2X,'seconds')
C*********************************************************
      OPEN (UNIT=17,    FILE='ACPGB.ILS',STATUS='OLD')
      READ (17, 100)    (FL1(J),J=1,16)
      OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')
      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')
      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')
      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')
      OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS='OLD')
      OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS='OLD')
      OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(8),  STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(9),  STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(10), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(11), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(12), STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(13), STATUS='REPLACE')
      OPEN (UNIT=NOUT7, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT8, FILE=FL1(15), STATUS='REPLACE')
      OPEN (UNIT=IN8,   FILE=FL1(16), STATUS='OLD')
C********************************************************
      WRITE (IN8,101) KEYBAD
      CLOSE (UNIT=IN8, STATUS = 'KEEP')
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      LABEL = ' *ACPGB'
      LBLERR=0
      NU=IN1
      CALL GTLBL(LABEL,LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (IBDY)   !Key to decide on optical thick., inner disk
  908 FORMAT ('ACPGB')
C      WRITE (NOUT7,908)
C
      WRITE (NOUT1,103)
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC
C     Input NPSI, number of orbital longitudes
      READ (IN7,106) KCTRL,NPSI
C     Input description of type boundary for accretion disk.
      LABEL = ' *ACPGA'
	LBLERR=0
	NU=IN2
	LBLERR=0
	CALL GTLBL (LABEL, LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL DUMMY(18)
	CALL IREAD(NTYPE)  !Key for outer accretion disk boundary
C     INPUT PERMANENT FILE DATA FROM ACPGA
   30 CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,AD,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN3)
C     I=star eclipsed at pri. min.
C     ISTAR=component with accretion disk.
C     Input data on stellar components from PGA, including radius data
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN5)
C     Initial approx. to radius of central star, equatorial radius
      RBP=VR(ISTAR,NTH/2+1,1)
C
C  
C     PRINCIPAL LOOP FOR SUCCESSIVE VALUES OF ORBITAL LONGITUDE   
C  
C  
      DO 20 KS=1,NPSI
	if (ks.ge.36) then
		zzzz=0.d0
	end if
C     Initialize visibility keys.
      DO J=1,MRING
         DO K=1,MSG
C     Visibility keys for face of accretion disk
            PRFH(J,K)=0.D0
         END DO
      END DO
      DO J=1,2
         DO K=1,MPHM
            DO LL=1,2
C	Visibility keys for boundaries of stars
               PRSH(J,K,LL)=0.D0
            END DO
         END DO
      END DO
      DO J=1,MRM
         DO K=1,MSG
C     Visibility keys for rim of accretion disk
            PRRH(J,K)=0.D0
         END DO
      END DO
      DO J=1,MRM
C     Visibility keys for horizon points, rim of accretion disk
         RXHZL(J)=0.D0
         RYHZL(J)=0.D0
         RXHZG(J)=0.D0
         RYHZG(J)=0.D0
      END DO
      DO J=1,2
		DO ITH=1,NTH
		N=NPH(I,ITH)
			DO K=1,N
C     Visibility keys for photosphere points, stellar components
				STVS(J,ITH,K)=0.D0
			END DO
		END DO
      END DO                                    
      NTS=0
C     Input data from PLTB, including parabolic fits to projected
C     boundaries of both stellar components. (Note that PLTB must be
C     run, rather than just PGB, because boundary fits for both
C     components are needed. PGB produced a fit only for the eclipsing
C     component.)
      CALL RDWT8(KSPP,IEC,ISV,IRC,IFC,NKP,IRCS,IFCS,NKPS,
     $AGLMT1,AGLMT2,XG,YG,XCM,YCM,ISEQ,IN4)
C     Input data from PGB, including projection data for both stellar
C     components.
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN6)
C     Set X axis to point toward companion
C     Save actual orbital longitude in PSISV
      PSISV=PSI
C
C     ISTAR is star with the accretion disk, ISV is the star eclipsed
C     at primary minimum.
C     ISEQ designates the quadrant containing the orbital longitude.
	IF (ISTAR.EQ.ISV) THEN
		IF ((ISEQ.EQ.1).OR.(ISEQ.EQ.4)) PSI=PSI+PI
	ELSE
	    IF ((ISEQ.EQ.2).OR.(ISEQ.EQ.3)) PSI=PSI+PI
	END IF
C
	IF (PSI.GT.TWPI) PSI=PSI-TWPI
C     CM is a variable that causes the projection of the eclipsing star
C     to move from right to left across the eclipsed star
      CM=TWPI-PSI
      IF (CM.LT.0.0) CM=CM+TWPI
      IF (PSI.EQ.0.D0) CM=0.D0
	PSI=PSISV
      AG=0.D0
      CSI=-DCOS(VI)
      SNI=DSIN(VI)
      IF (ISEQ.EQ.1) THEN
         IF (I.EQ.IEC) THEN
            IRD=-1
         ELSE
            IRD=1
         END IF      
      END IF
      IF (ISEQ.EQ.2) THEN
         IF (I.EQ.IEC) THEN
            IRD=1
         ELSE
            IRD=-1
         END IF
      END IF
      IF (ISEQ.EQ.3) THEN
         IF (I.EQ.IEC) THEN
            IRD=-1
         ELSE
            IRD=1
         END IF
      END IF
      IF (ISEQ.EQ.4) THEN
         IF (I.EQ.IEC) THEN
            IRD=1
         ELSE
            IRD=-1
         END IF
      END IF
C     ISV is the star eclipsed at pri. min.,IEC is the eclipsing star.	
      IF (ISV.EQ.IEC) AG=PI
C     The origin of coods. is at the center of the eclipsing star.
C     SET TARY, the projection matrix.
C     Calculate VL0,VM0,VN0, the direction cosines of the line
C     of sight toward the observer.
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
C     Calculate XG,YG,ZG, the coordinates of the center of the star 
C     being eclipsed.
C     Note that VRD, from PGB, is the current separation of centers
C     of the stellar components. (Allow for eccentric orbit.)
      CALL PROJ(TARY,VRD,0.D0,0.D0,XG,YG,ZG)
      XC=QS/(1.D0+QS)
      IF (IEC.EQ.ISV) XC=QN/(1.D0+QN)
C     Calculate XCMS,YCMS,ZCMS, the coods. of the center of mass
C     relative to the center of the eclipsing star,
C     projected on the plane of sky and perpendicular to it.
      CALL PROJ(TARY,XC*VRD,0.D0,0.D0,XCMS,YCMS,ZCMS)
      IF (IEC.EQ.ISV) THEN
		XCMS=XCMS-XG
		YCMS=YCMS-YG
		ZCMS=ZCMS-ZG
	END IF
C     Note that XCM,YCM refer to plane of sky coods., as distinct from
C        the cood. system for representing photosphere points
C     Calculate displacements for center of mass system.
C     Set to look at visible hemisphere of eclipsed star
      AG=PI
      IF (ISV.EQ.IEC) AG=0.D0
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
      IF ((ISEQ.EQ.2).OR.(ISEQ.EQ.3)) GO TO 1
C
      I=3-ISV
      Q=QN
      GO TO 2
C
    1 I=ISV
      Q=QS
C
C     Do the section referring to the accretion disk.
C     Note that the accretion disk coods. are handled separately from
C     the stellar components.
C
C     Fill cos(gamma) for rim, where rim is perpendicular to XY plane,
C     and the normal is directed outward. Set visibility keys in PRRH.
    2 CONTINUE
	IF (ISEQ.NE.1) GO TO 3
      IF (ISTAR.EQ.ISV) THEN
         DX=-XCMS
         DY=-YCMS
		DZ=-ZCMS
         DDX=XCMS-XG
         DDY=YCMS-YG
		DDZ=ZCMS-ZG
	ELSE
         DX=XCMS
         DY=YCMS
		DZ=ZCMS
         DDX=XCMS-XG
         DDY=YCMS-YG
		DDZ=ZCMS-ZG
	END IF
    3	IF (ISEQ.NE.2) GO TO 4
      IF (ISTAR.EQ.ISV) THEN
	   DX=XCMS
	   DY=YCMS
		DZ=ZCMS
         DDX=XCMS+XG
         DDY=YCMS+YG
		DDZ=ZCMS+ZG
	ELSE
	   DX=XCMS
	   DY=YCMS
		DZ=ZCMS
         DDX=-(XCMS+XG)
         DDY=-(YCMS+YG)
		DDZ=-(ZCMS+ZG)
      END IF
    4	IF (ISEQ.NE.3) GO TO 5
      IF (ISTAR.EQ.ISV) THEN
	   DX=XCMS
	   DY=YCMS
		DZ=ZCMS
	   DDX=XCMS+XG
	   DDY=YCMS+YG
		DDZ=ZCMS+ZG
	ELSE
	   DX=XCMS
	   DY=YCMS
		DZ=ZCMS
	   DDX=-(XCMS+XG)
	   DDY=-(YCMS+YG)
		DDZ=-(ZCMS+ZG)
	END IF
    5	IF (ISEQ.NE.4) GO TO 6
      IF (ISTAR.EQ.ISV) THEN
	   DX=-XCMS
	   DY=-YCMS
		DZ=-ZCMS
	   DDX=XCMS-XG
	   DDY=YCMS-YG
		DDZ=ZCMS-ZG
	ELSE
	   DX=XCMS
	   DY=YCMS
		DZ=ZCMS
	   DDX=XCMS-XG
	   DDY=YCMS-YG
		DDZ=ZCMS-ZG
	END IF
    6 CONTINUE
C
C     *********************
C     RIM OF ACCRETION DISK
C     *********************
      IF (NRM.EQ.0) GO TO 10
	DO K=1,NRM
		DO J=1,NSG
			VLR=DCOS(PHIR(J))
			VM=DSIN(PHIR(J))
			CSPH(J)=VLR
			SNPH(J)=VM
			IF (NTYPE.EQ.1) THEN
				CSGR(K,J)=VLR*VL0+VM*VM0
			END IF
			IF (NTYPE.EQ.2)THEN
				CSGR(K,J)=ZLAR(K,J)*VL0+ZMAR(K,J)*VM0+ZNAR(K,J)*VN0
			END IF 
C     Set visibility key negative for points below horizon.
			IF (CSGR(K,J).LT.0.D0) THEN
				PRRH(K,J)=CSGR(K,J)
			END IF
         END DO
      END DO
C      IF (KS.EQ.NPSI) THEN
C      DO JJ=1,NRM
C         WRITE (NOUT7,903) JJ
C         WRITE (NOUT7,901) (PRRH(JJ,KK),KK=1,NSG)
C		WRITE (NOUT7,902)
C		WRITE (NOUT7,901) (CSGR(JJ,KK),KK=1,NSG)
C      END DO
C	END IF
C     Reset bottom rim to complete visibility.
      DO J=1,NSG
      PRRH(NRM,J)=0.D0
      END DO
C     Project corners of rim pixels on plane of sky
C     Note that VXAR,VYAR,VZAR are spatial coods. from ACPGA.
C     PXR,PYR,PZR are projections on plane of sky and perpendicular
C     direction, with origin at center of central star.
      DO K=1,NRM
         DO J=1,NSG
         X=VXAR(K,J)
         Y=VYAR(K,J)
         Z=VZAR(K,J)
         CALL PROJ(TARY,X,Y,Z,PXR(K,J),PYR(K,J),PZR(K,J))
         END DO
      END DO
C	DO K=1,NSG
C	WRITE (NOUT7,1000) PXR(NRM,K),PYR(NRM,K),PZR(NRM,K)
C	END DO
C	IF (I.LT.1000) STOP
 1000 FORMAT (6F12.8)
      DO K=1,NRM
         DO 8 J=1,NSG
C     Place origin at center of mass
C     Note that positive X direction is downward.
         IF (ISTAR.EQ.ISV) THEN
         PXR(K,J)=PXR(K,J)-DX
         PYR(K,J)=PYR(K,J)-DY
		PZR(K,J)=PZR(K,J)-DZ
         ELSE
         PXR(K,J)=PXR(K,J)-DDX
         PYR(K,J)=PYR(K,J)-DDY
		PZR(K,J)=PZR(K,J)-DDZ
         END IF
   8     CONTINUE
      END DO
C     Write coods of rim points.
      WRITE (NOUT4,106) NRM,NSG
	DO K=1,NRM
		DO J=1,NSG
			WRITE (NOUT4,139) PYR(K,J),PXR(K,J),PZR(K,J)
		END DO
	END DO
C
	XA=-5.D0
	XB=5.D0
	DO J=1,NSG
		IF (XA.LT.PXR(NRM,J))XA=PXR(NRM,J)
		IF (XB.GT.PXR(NRM,J))XB=PXR(NRM,J)
	END DO
C     XA and XB are in center of mass coods.
C     Momentarily restore them to center of gainer coods.
	IF (ISTAR.EQ.ISV) THEN
		XA=XA+DX
		XB=XB+DX
	ELSE
		XA=XA+DDX
		XB=XB+DDX
	END IF
C     XRMC is the ---DISTANCE--- from the projected center of the
C     gainer to projected center of the bottom rim.
C     Note that XRMC   IS NOT   the center of mass coordinate of
C     the projected center of the bottom rim. 	      
      XRMC=0.5*(XA+XB)
	YA=-5.D0
	YB=5.D0
	DO J=1,NSG
		IF (YA.LT.PYR(NRM,J))YA=PYR(NRM,J)
		IF (YB.GT.PYR(NRM,J))YB=PYR(NRM,J)
	END DO
	IF (ISTAR.EQ.ISV) THEN
		YA=YA+DY
		YB=YB+DY
	ELSE
		YA=YA+DDY
		YB=YB+DDY
	END IF
      YRMC=0.5*(YA+YB)
C     (XA,YA) and (XB,YB) are plane of sky coods. of points of
C     max. elongation, bottom edge of rim.
C
C     Set radii and position angles on plane of sky for outer rim
	RMX=0.D0
	RMN=1.D0
      DO J=1,NSG
	IF (ISTAR.EQ.ISV) THEN
	X=(PXR(NRM,J)+DX)-XRMC
	Y=(PYR(NRM,J)+DY)-YRMC
	ELSE
      X=(PXR(NRM,J)+DDX)-XRMC
      Y=(PYR(NRM,J)+DDY)-YRMC
	END IF
C     RD(J) are plane of sky radii from central star center to points 
C     on bottom edge of rim.
      RD(J)=DSQRT(X*X+Y*Y)
	IF (RD(J).GT.RMX) RMX=RD(J)
	IF (RD(J).LT.RMN) RMN=RD(J)
      IF (DABS(Y).GT.1.0D-12) GO TO 16
      IF ((DABS(Y).LT.1.0D-12).AND.(J.EQ.NSG)) GO TO 16
      IF (X.LT.0.0) AGLD(J)=0.0
      IF (X.GE.0.0) AGLD(J)=PI
      GO TO 17
   16 AGLD(J)=DASIN(DABS(Y)/RD(J))
      IF ((Y.LT.0.0).AND.(X.LT.0.0)) GO TO 17
      IF ((Y.LT.0.0).AND.(X.GE.0.0)) AGLD(J)=PI-AGLD(J)
      IF ((Y.GE.0.0).AND.(X.GE.0.0)) AGLD(J)=AGLD(J)+PI
      IF ((Y.GE.0.0).AND.(X.LT.0.0)) AGLD(J)=TWPI-AGLD(J)
   17 CONTINUE
      WRITE (NOUT8,2000) PXR(NRM,J),PYR(NRM,J),X,Y,RD(J),AGLD(J)
      END DO
C	WRITE (NOUT8,2001) NSG
C	WRITE (NOUT8,2000) (RD(J),J=1,NSG)
C	WRITE (NOUT8,2002)
C	WRITE (NOUT8,2000) (AGLD(J),J=1,NSG)
C	IF (NSG.NE.0) STOP
 2001 FORMAT (I5)
 2000 FORMAT (10F8.5)
 2002 FORMAT (' ')
C     Handle case of orbital inclination very close to PIO2
C     Make breakover at 89.9 degrees
      IF (VI.GT.1.569051D0) THEN
		DO J=1,NSG
			AGLD(J)=PHIR(J)
		END DO
		DO J=1,NSG
			IF (RD(J).EQ.RMN) KSTP=J
		END DO
		DO J=1,KSTP-1
			AGLSV=AGLD(1)
			RDSV=RD(1)
			DO K=2,NSG
				AGLD(K-1)=AGLD(K)
			END DO
			AGLD(NSG)=AGLSV
			RD(NSG)=RDSV
		END DO
	END IF
   50 CONTINUE
C
C     AGLD(J) are position angles of RD(J). AGLD(J)=0.0 for topmost
C     point on projection ellipse.
C
C     **********************
C     FACE OF ACCRETION DISK
C     **********************
C
C     Fill cos(gamma) for face of disk
C     Note that we are working with bottom face, by using NRM
C
C     ------------------------------------------------------------
C     The following procedure would be appropriate for the case of
C     a sloping face for the accretion disk.
C      ALPHA=DATAN(HV/RA)
C     Note!! In PGB, VN0  has been set to -cos(i), so it is necessary to
C     set VLN=-cos(alpha)      
C      VLN=-DCOS(ALPHA)
C      SNAL=DSIN(ALPHA)
C
C     Calculate projection factors
      DO J=1,NSG
C      (sloping face)
C
C     ----------------------------------------------------------
C     The following expression is appropriate for a stepped face
C     for the accretion disk
      CSGF(1,J)=-VN0  
         DO K=2,NRING
         CSGF(K,J)=CSGF(1,J)
         END DO
      END DO
C     -----------------------------------------------------------	 
C     Write values of cos(gamma) to output for a face ring
      WRITE (NOUT1,123)
      WRITE (NOUT1,126) (CSGF(1,J),J=1,NSG) 
  123 FORMAT(1X,'Values of cos(gamma) for one of face rings')
      WRITE (NOUT1,107)
C     Write values of cos(gamma) to output for a rim ring
      WRITE (NOUT1,124)
      WRITE (NOUT1,126) (CSGR(1,J),J=1,NSG)
      WRITE (NOUT1,107)
  124 FORMAT(1X,'Values of cos(gamma) for one of rim rings')
  126 FORMAT (1X,9F10.5)                      
C     Project corners of face pixels on plane of sky
      DO K=1, NRING
         DO 15 J=1,NSG
         X=VXAF(K,J)
         Y=VYAF(K,J)
         Z=VZAF(K,J)
C     Note that VXAF,VYAF,VZAF are spatial coods. of grid points,
C     relative to center of central star.
C     Initially, PXF,PYF,PZF are plane of sky and perpendicular 
C     direction coods., relative to center of central star.
         CALL PROJ(TARY,X,Y,Z,PXF(K,J),PYF(K,J),PZF(K,J))
C     Now transform coods. to origin at projected center of mass.
         IF (ISTAR.EQ.ISV) THEN
         PXF(K,J)=PXF(K,J)-DX
         PYF(K,J)=PYF(K,J)-DY
	   PZF(K,J)=PZF(K,J)-DZ
         ELSE
         PXF(K,J)=PXF(K,J)-DDX
         PYF(K,J)=PYF(K,J)-DDY
	   PZF(K,J)=PZF(K,J)-DDZ
         END IF
   15    CONTINUE
      END DO
C     Write coods. of face points to output.
      WRITE (NOUT4,106) NRING, NSG
	DO K=1,NRING
		DO J=1,NSG
			WRITE (NOUT4,139) PYF(K,J),PXF(K,J),PZF(K,J)
		END DO
	END DO
C
C
C     End of exclusively accretion disk section.
C
C     *****************************************************************
C
C     Now do the projections for the stars.
C
C     ********************
C     Component I
C     ********************
C
C     Fill arrays for coods. of surface points on eclipsing component
C     Component I is the eclipsing component
C     Project eclipsing component (I) on plane of sky
   10 DO ITH=1,NTH
	   N=NPH(I,ITH)
         DO K=1,N
         X=VR(I,ITH,K)*ALM(I,ITH,K)
         Y=VR(I,ITH,K)*AMU(I,ITH,K)
         Z=VR(I,ITH,K)*ANU(I,ITH,K)
         CALL PROJ(TARY,X,Y,Z,STX(I,ITH,K),STY(I,ITH,K),STZ(I,ITH,K))
         STXA(I,ITH,K)=STX(I,ITH,K)
         STYA(I,ITH,K)=STY(I,ITH,K)         
C     Displace coods. to their value relative to the center of mass.
         IF (ISTAR.EQ.ISV) THEN
		IF (I.EQ.ISV) THEN
			STX(I,ITH,K)=STX(I,ITH,K)-DX
			STY(I,ITH,K)=STY(I,ITH,K)-DY
          ELSE
			STX(I,ITH,K)=STX(I,ITH,K)-DDX
			STY(I,ITH,K)=STY(I,ITH,K)-DDY
		END IF
	   ELSE
		IF (I.EQ.ISV) THEN
			STX(I,ITH,K)=STX(I,ITH,K)-DX
			STY(I,ITH,K)=STY(I,ITH,K)-DY
          ELSE
			STX(I,ITH,K)=STX(I,ITH,K)-DDX
			STY(I,ITH,K)=STY(I,ITH,K)-DDY
		END IF
         END IF 	    		         
C     Set visibility keys for eclipsing component.
C     First set keys negative for points below horizon.
         IF (CSG(I,ITH,K).LT.0.D0) STVS(I,ITH,K)=CSG(I,ITH,K)
	   VPHV=PH(I,ITH,K)
C     Initialize all points between start and end of visible
C     limits to visibility true condition. 
         IF ((VPHV.GE.VST(I,ITH,1)).AND.(VPHV.LE.VND(I,ITH,1)))
     $   STVS(I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(I,ITH,2)).AND.(VPHV.LE.VND(I,ITH,2)))
     $   STVS(I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(I,ITH,3)).AND.(VPHV.LE.VND(I,ITH,3)))
     $   STVS(I,ITH,K)=1.D0
         END DO
  907 FORMAT (10F8.4)              
      END DO
C     End of section for eclipsing component
C
C     ************************
C     Eclipsed component
C     ************************
C
C     Must reset directions of axes so line of centers points from
C        eclipsed component toward eclipsing component.
      PSISV=PSI
      CMSV=CM
      PSI=PSI+PI
      IF (PSI.GT.TWPI) PSI=PSI-TWPI
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
      IF (CM.GE.TWPI) CM=0.D0
      IF (PSI.GE.TWPI) PSI=0.D0
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
      NTS=0
C     First set arrays for points relative to center of eclipsed
C         component
	ITHR=0
	NN=0
      DO ITH=1,NTH
	DPH=0.D0
      N=NPH(3-I,ITH)
         DO K=1,N
         X=VR(3-I,ITH,K)*ALM(3-I,ITH,K)
         Y=VR(3-I,ITH,K)*AMU(3-I,ITH,K)
         Z=VR(3-I,ITH,K)*ANU(3-I,ITH,K)
C     Calculate plane of sky and perpendicular coods.
C     Origin initially is center of eclipsed component.
         CALL PROJ(TARY,X,Y,Z,STX(3-I,ITH,K),STY(3-I,ITH,K),
     $   STZ(3-I,ITH,K))
         STXA(3-I,ITH,K)=STX(3-I,ITH,K)
         STYA(3-I,ITH,K)=STY(3-I,ITH,K)
C     Displace coods. to position relative to center of mass.
C     Note: Because the axes directions were reversed in the above
C        code, the displacement corrections to the center of mass also
C        change.
         IF (ISTAR.EQ.ISV) THEN
		IF (I.EQ.ISV) THEN
			STX(3-I,ITH,K)=STX(3-I,ITH,K)-DDX
			STY(3-I,ITH,K)=STY(3-I,ITH,K)-DDY
          ELSE
			STX(3-I,ITH,K)=STX(3-I,ITH,K)-DX
			STY(3-I,ITH,K)=STY(3-I,ITH,K)-DY
		END IF
	   ELSE
		IF (I.EQ.ISV) THEN
			STX(3-I,ITH,K)=STX(3-I,ITH,K)-DDX
			STY(3-I,ITH,K)=STY(3-I,ITH,K)-DDY
          ELSE
			STX(3-I,ITH,K)=STX(3-I,ITH,K)-DX
			STY(3-I,ITH,K)=STY(3-I,ITH,K)-DY
		END IF
         END IF
C     Set visibility keys for points below horizon	    	    		         
         IF (CSG(3-I,ITH,K).LT.0.D0) STVS(3-I,ITH,K)=CSG(3-I,ITH,K)
C     Initialize points between start and end of visible
C     limits to condition of visible.
	   VPHV=PH(3-I,ITH,K)
		IF ((K.EQ.1).AND.(N.GT.1)) THEN
	        VPHA=PH(3-I,ITH,K+1)
			DPH=VPHA-VPHV
		END IF
         IF ((VPHV.GE.VST(3-I,ITH,1)).AND.(VPHV.LE.VND(3-I,ITH,1)))
     $   STVS(3-I,ITH,K)=1.0D0
         IF ((VPHV.GE.VST(3-I,ITH,2)).AND.(VPHV.LE.VND(3-I,ITH,2)))
     $   STVS(3-I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(3-I,ITH,3)).AND.(VPHV.LE.VND(3-I,ITH,3)))
     $   STVS(3-I,ITH,K)=1.D0
C     Set visibility keys to allow for eclipse effects.
C     Note that these are eclipses due to companion star.
         IF ((VPHV.GE.SHS(ITH,1)).AND.(VPHV.LE.SHN(ITH,1)))
     $   THEN
		IF (DABS(SHS(ITH,1)-SHN(ITH,1)).GT.1.0E-12)
     $    STVS(3-I,ITH,K)=-1.D0
		END IF
	   IF ((VPHV.GE.SHS(ITH,2)).AND.(VPHV.LE.SHN(ITH,2)))
     $   THEN
		IF (DABS(SHS(ITH,2)-SHN(ITH,2)).GT.1.0E-12)
     $    STVS(3-I,ITH,K)=-1.D0
		END IF
	   IF ((VPHV.GE.SHS(ITH,3)).AND.(VPHV.LE.SHN(ITH,3)))
     $   THEN
		IF (DABS(SHS(ITH,3)-SHN(ITH,3)).GT.1.0E-12)
     $    STVS(3-I,ITH,K)=-1.0
		END IF
	END DO
  901 FORMAT (10F8.3)
  902 FORMAT (' ')
  903 FORMAT (5I5)
C      WRITE (NOUT7,903) 3-I,ITH
C	WRITE (NOUT7,901) (STVS(3-I,ITH,K),K=1,N)
      END DO
C	IF (I.LT.1000) GO TO 99
C     If there is no accretion disk, go directly to write output.
      IF (NRM.EQ.0) GO TO 19
C
C     Now have projections and nominal visibility keys set for both
C        stellar components. The below horizon points have been tagged
C        as not visible.
C
C     *****************************************************************
C
C     *********************
C     Set visibility keys
C     *********************
C
C     Reset parameters to initial values.
C      IF (KS.EQ.NPSI) THEN
C      DO JJ=1,NRM
C         WRITE (NOUT7,903) JJ
C         WRITE (NOUT7,901) (PRRH(JJ,KK),KK=1,NSG)
C		WRITE (NOUT7,902)
C		WRITE (NOUT7,901) (CSGR(JJ,KK),KK=1,NSG)
C      END DO
C	END IF
  998 FORMAT (1X,'ACPGB',10I5)
C      IF (I.LT.1000) STOP
C
C
C     ******************************************
C     Determine visibility of given pixel on face of accretion disk,
C     based on blocking by central star.
C     ******************************************
C
C     Visibility keys for face of accretion disk are in array PRFH.
   21 IF ((ISEQ.EQ.1).OR.(ISEQ.EQ.4)) THEN
		IF (ISTAR.EQ.ISV) THEN
			CALL VSBSA(DX,DY,DDX,DDY,PXF,PYF,PRFH,IFC,ALP,RS,XRMC,
     $		ISTAR,ISV,NRING,NSG,MRING,MSG,MPHM,NOUT7)
		ELSE
			CALL VSBSA(DX,DY,DDX,DDY,PXF,PYF,PRFH,IFC,ALPS,RSS,XRMC,
     $		ISTAR,ISV,NRING,NSG,MRING,MSG,MPHM,NOUT7)
		END IF
	ELSE
		IF (ISTAR.EQ.ISV) THEN
			CALL VSBSA(DX,DY,DDX,DDY,PXF,PYF,PRFH,IFCS,ALPS,RSS,XRMC,
     $		ISTAR,ISV,NRING,NSG,MRING,MSG,MPHM,NOUT7)
		ELSE
			CALL VSBSA(DX,DY,DDX,DDY,PXF,PYF,PRFH,IFC,ALP,RS,XRMC,
     $		ISTAR,ISV,NRING,NSG,MRING,MSG,MPHM,NOUT7)
		END IF
	END IF
C
C
C     *********************************************
C     Determine visibility of central star boundary
C     *********************************************
C
C     XHZ, and YHZ contains coods. of points on central star boundary,
C     starting at topmost point and moving around boundary in CCW
C     direction.
C     Visibility keys for central star boundary are in PRSH.
   22 CALL VSBSB(ISTAR,PXR,PYR,PZR,AGLD,RD,PRSH,CSGR,
     $PRRH,CSI,
     $RMLX,RMLY,RMGX,RMGY,NRM,NTH,
     $IEC,IFC,IRC,IFCS,IRCS,NSG,VXP,VYP,XRMC,YRMC,XG,YG,DX,DY,
     $DDX,DDY,ISTAR,ISV,STX,ISEQ,IBDY,NTYPE,MRM,MSG,IRD,NOUT7)
c	 WRITE (NOUT7,1001) (PRSH(3-I,K,1),K=1,IFC)
c	WRITE (NOUT7,1002)
c	 WRITE (NOUT7,1001) (PRSH(3-I,K,2),K=1,IRC)
c	WRITE (NOUT7,1002)
c	IF (I.LT.1000) STOP
c      IF (KS.EQ.NPSI) THEN
c      DO JJ=1,NRM
c         WRITE (NOUT7,903) JJ
c         WRITE (NOUT7,901) (PRRH(JJ,KK),KK=1,NSG)
c		WRITE (NOUT7,902)
c		WRITE (NOUT7,901) (CSGR(JJ,KK),KK=1,NSG)
c      END DO
c	if (i.lt.1000) stop
c	END IF
C
C     **************************************************
C     Modify STVS to allow for eclipse by accretion disk
C     **************************************************
C
   23 IF (IBDY.NE.0) THEN
		IF ((ISEQ.EQ.1).OR.(ISEQ.EQ.4)) THEN
		CALL VSBSC(ISTAR,DX,DY,STX,STY,PXF,PYF,PXR,PYR,AGLD,RD,STVS,
     $	XRMC,PRFH,CSGR,NTH,NSG,NRM,DDX,DDY,
     $	ISTAR,ISV,NOUT7)
		ELSE
		CALL VSBSC(ISTAR,DX,DY,STX,STY,PXF,PYF,PXR,PYR,AGLD,RD,STVS,
     $	XRMC,PRFH,CSGR,NTH,NSG,NRM,DDX,DDY,
     $	ISTAR,ISV,NOUT7)
		END IF
      END IF
C
C     **************************************************************
C     Determine visibility of given pixel on face of accretion disk,
C     based on blocking by disk itself.
C     **************************************************************
C
C     The visibility keys for the accretion disk face are in array PRFH.
C     Note that this SRT does not set data in PRFH for the outermost
C     rim.
   18 CALL VSB(PXF,PYF,PRFH,XRMC,YRMC,AGLD,RD,
     $NRING,NSG,DX,DY,DDX,DDY,XLSL,ISTAR,ISV,VI,NOUT7)
c	DO ITH=1,NTH
c		N=NPH(ISTAR,ITH)
c		WRITE (NOUT7,903) ISTAR,ITH
c		WRITE (NOUT7,901) ((ISTAR,ITH,K),K=1,N)
c	END DO
C	IF (I.LT.1000) STOP
 1001 FORMAT (10F8.4)
 1002 FORMAT (' ')
C
C     *******************************************************
C     Modify PRFH to allow for eclipse effects
C     due to the companion to the star with the accretion disk.
C     *******************************************************
C
   24 CALL VSBSD(ISTAR,STX,STY,PXF,PYF,PXR,PYR,AGLD,RD,STVS,
     $XRMC,YRMC,PRFH,CSGR,NTH,NSG,NRING,NRM,PRRH,IEC,
     $IRCS,IFCS,NKPS,
     $XG,YG,RXHZL,RYHZL,RXHZG,RYHZG,
     $PRVFG,PRVFL,RVLD,ANG,XUL,XLL,YUL,YLL,DX,DY,DDX,DDY,ISTAR,ISV,ISEQ,
     $NTYPE,IFLAG,MRM,MSG,MRING,NOUT7)
	 WRITE (NOUT7,1001) (PRSH(3-I,K,1),K=1,IFC)
	WRITE (NOUT7,1002)
	 WRITE (NOUT7,1001) (PRSH(3-I,K,2),K=1,IRC)
	WRITE (NOUT7,1002)
C
   19 CALL BWTO(NSG,NRM,NRING,CSGR,CSGF,PXR,PYR,PZR,PXF,PYF,PZF,
     $XHZ,YHZ,PRFH,PRSH,PRRH,XRMC,YRMC,RBP,STX,STY,STZ,STVS,
     $RXHZL,RYHZL,RXHZG,RYHZG,PRVFG,PRVFL,XUL,XLL,YUL,YLL,RVLD,ANG,
     $RD,AGLD,VXP,VYP,IFC,IRC,IFCS,IRCS,ISTAR,I,IEC,IFLAG,NTH,IBDY,
     $NOUT2)
C
      WRITE (NOUT3,108) DX,DY,DZ,DDX,DDY,DDZ,XG,YG,ZG,XLSL
	WRITE (NOUT3,106) ISV,ISEQ
C     List visibility keys on face of accretion disk
      WRITE (NOUT1,137)
  137 FORMAT (1X,'Visibility keys, face of accretion disk')    
      DO K=1,NRING
         WRITE (NOUT1,128) K
  128 FORMAT (1X,'Ring number', I5)
         WRITE (NOUT1,129) (PRFH(K,L),L=1,NSG)
      END DO
  129 FORMAT (1X,10F8.3)
C     List visibility keys on components
  140 FORMAT (1X,'Visibility keys component',I5)
  141 FORMAT (10F7.3)
      WRITE (NOUT1,140) ISV
	DO ITH=1,NTH
		N=NPH(ISV,ITH)
		WRITE (NOUT1,106) ITH,N
		WRITE (NOUT1,141) (STVS(ISV,ITH,K),K=1,N)
	END DO                    
      WRITE (NOUT1,140) 3-ISV
	DO ITH=1,NTH
		N=NPH(3-ISV,ITH)
		WRITE (NOUT1,106) ITH,N
		WRITE (NOUT1,141) (STVS(3-ISV,ITH,K),K=1,N)
	END DO                    
   20 CONTINUE
   99 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
      WRITE(NOUT1, 305) TM
      CLOSE (UNIT=14,    STATUS='KEEP')
	CLOSE (UNIT=IN1,   STATUS='KEEP')
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=IN3,   STATUS='KEEP')
      CLOSE (UNIT=IN4,   STATUS='KEEP')
      CLOSE (UNIT=IN5,   STATUS='KEEP')
      CLOSE (UNIT=IN6,   STATUS='KEEP')
	CLOSE (UNIT=IN7,   STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      CLOSE (UNIT=NOUT6, STATUS='KEEP')
      CLOSE (UNIT=NOUT7, STATUS='KEEP')
	  OPEN (UNIT=IN8, FILE = FL1(16), STATUS = 'OLD')
	  WRITE (IN8,101) KEYGOOD
	  CLOSE (UNIT=IN8, STATUS = 'KEEP')
      STOP
   60 WRITE (NOUT1,122) VI  
      GO TO 99   
      END PROGRAM ACPGB
