      PROGRAM ACPGA										 
C     Use Microsoft Power Station Fortran for following statement
C	USE MSFLIB
C     Program to calculate geometric properties of accretion disk
C     
C     **************
C     Version ACPGA2
C     **************
C
C
C     Quantities output by ACPGA, via SRT AWTO
C     NRM		Number of rim divisions producing rings
C     NSG		Number of divisions on a given ring
C     NRING	Number of ring divisions on face
C     ITHU	Theta index for the boundary of the central star whose
C			theta value is less than the thetra value for the upper
C			rim
C     ITHL	Theta index for the boundary of the central star whose
C			theta value is greater than the theta for the lower rim
C     ISTAR	Designation (1 or 2) of the star with the accretion disk
C     RA		Radius of outer edge of accretion disk (solar radii)
C     RB		Radius of inner edge of accretion disk (solar radii)
C     HV		Semithickness of outer rim of accretion disk (solar rad)
C     A		Semi-major axis of relative orbit in solar radii
C     THR(J)	Array of polar angles of rim division boundaries
C     SNTR(J) Array of values of sin(thr(J))
C     GR(J)   Array of gravity values, toward central plane, rim
C     PHIR(K) Array of azimuthal division angles, all rings
C     VXAR(K,J),VYAR(K,J),VZAR(J,K)
C			X,Y,Z coods. of rim division pts.,in units of component
C			separation, origin at center of central star, X axis
C			directed toward companion
C     ALMAR(K,J),AMUAR(K,J),ANUAR(K,J)
C			Direction cosines of rim radius vector, at division points
C     ZLAR(K,J),ZMAR(K,J),ZNAR(K,J)
C			Direction cosines of outer normal to rim, at division pts
C     CSBR(K,J) cos(beta) at rim division points
C     RGRD(K)	Radii of face rings, solar radii
C     FVF(K)	Angular vel. of annular rings, in units of Keplerian.
C			If exactly Keplerian, value=0.0
C			If 110% of Keplerian, value=0.1
C			If  90% of Keplerian, value=-0.1
C     GF(K)	Gravity towards central plane on face of annular ring
C     GPER(K)	
C     SNTF(K)	sin(theta) for polar angle of bottom face of disk,
C			repeated for all rings
C     ZLAF(K),ZMAF(K),ZNAF(K)
C			Direction cosines of normal to central plane
C			(same for all rings)
C     ALMAF(J),AMUAF(J),ANUAF(J)
C			Direction cosines of radius vector, in central plane, to
C			ring division points. Varies with ring division pts., but
C			the same for all rings.)
C     VXAF(K,J),VYAF(K,J),VZAF(K,J)
C			X,Y,Z coods. of bottom face points, relative to center of
C			central star. X axis directed toward center of companion.
C     CSBF(K,J) cos(beta) for face division points
C
C    
      INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
      DIMENSION VC(MRM),FL1(6)
      COMMON /NUNIT/ NU,NUOUT,IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 28 FL1
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD,KEYBAD
	INTEGER(4) RESULT
      DATA IN1,IN2,IN3,IN4,NOUT1,NOUT2/1,2,3,4,5,6/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
	DATA GRAV/6.6704D-8/
	DATA SNRAD/6.9599D10/
	DATA PIO2/1.5707963267948966192D0/
	DATA TWPI/6.2831853071795864769D0/
	DATA PI/3.141592653589793238463D0/
C  
C     THE FOLLOWING ARE INSTALLATION PARAMETERS     
C  
C  
      DATA HFER/0.000000001/   
      DATA KL/20/
      DATA I/1/
C  
C  
C
C
  100 FORMAT (A28)
  101 FORMAT (10F7.4)
  102 FORMAT (I2)
  103 FORMAT (1H1)      
  104 FORMAT (1X,'Program ACPGA',28X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/,13X,
     $'**OUT**',/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'*')
  107 FORMAT (1X,'Number of rim subdivision boundaries',18X,'NRM',6X,
     $':',I3,/,1X,
     $'Number of segment boundaries on each rim subdivision',
     $2X,'NSG',6X,':',I3,/)
  108 FORMAT (1X,'Outer radius of disk(solar radii)',21X,'RA',7X,':',
     $F9.4,/,
     $1X,'Inner radius of disk(solar radii)',21X,'RB',7X,':',F9.4,/,
     $1X,'Outer rim semithickness(solar radii)',18X,'HV',7X,':',F9.4)
  109 FORMAT (1X,'Equatorial radius of central star from PGA',10X,
     $'  RBSTAR',
     $3X,':',F9.4,/,1X,'Inner radius of disk from .INF file',
     $19X,'RBSV',5X,':',F9.4)
  110 FORMAT (1X,'Separation of components(solar radii)',18X,'A',7X,
     $':',F9.4)
  111 FORMAT (1X,'Number of face rings',34X,'NRING',4X,':',I3,/,1X,
     $'Number of segments on each face ring',18X,'NSG',6X,':',I3)
  112 FORMAT (1X,'Half height, boundary layer',27X,'HVBL',5X,':',F9.4)
  113 FORMAT (1X,'Thickness, boundary layer',29X,'THBL',5X,':',F10.6)
  114 FORMAT ('NRM too large')
  115 FORMAT ('NSG too large')
  116 FORMAT ('NRING too large')                
  205 FORMAT (1X,'Program running time=',F12.4,2X,'minutes')
C
C     RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)  
C  
C  
C  
C  
C***************************************************
C
      OPEN (UNIT=7,     FILE='C:\LGTSYN\ACPGA\ACPGA.ILS',STATUS='OLD')
      READ (7, 100)     (FL1(J),J=1,6)
	OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')  !LINKV.DAT
      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')  !Information file ACPGA.INF
      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')  !File PGAX2.DAT
      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')  !File ACPGAR.DAT
      OPEN (UNIT=NOUT1, FILE=FL1(5),  STATUS='REPLACE') !Output file ACPGA1.DAT
      OPEN (UNIT=NOUT2, FILE=FL1(6),  STATUS='REPLACE') !Output file ACPGA2.DAT
C***************************************************
      WRITE (IN1,102) KEYBAD
	CLOSE (UNIT=IN1, STATUS = 'KEEP')
C      CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,FVL1)
      WRITE (NOUT1,103)
      WRITE (NOUT1, 104) IMON,IDAY,IYR,IHR,IMIN,ISEC
C
      LBLERR=0
C     Find label and point to first data line
      LABEL = ' *ACPGA'
      NU=IN2
      CALL GTLBL(LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (ISTAR)	 !Star with accretion disk
      CALL DUMMY (1)
      CALL IREAD (NRM)	 !Number of rim ring divisions
	IF (NRM.GT.MRM) THEN
		WRITE (NOUT1,114)
		GO TO 99
	END IF
      CALL DUMMY (1)
      CALL IREAD (NSG)	 !Number of divisions of a given rim ring
	IF (NSG.GT.MSG) THEN
		WRITE (NOUT1,115)
		GO TO 99
	END IF
      CALL DUMMY (1)
      CALL IREAD (NRING)	 !Number of ring divisions on face of disk
	IF (NRING.GT.MRING) THEN
		WRITE (NOUT1,116)
		GO TO 99
	END IF
      CALL DUMMY (1)
      CALL DREAD (RA,'F12.6')	!Outer disk radius (solar radii)
      CALL DUMMY (1)
      CALL DREAD (RB,'F12.6')	!Inner disk radius (solar radii)
      CALL DUMMY(1)
      CALL DREAD (HV,'F12.6')	!Outer rim semithickness (solar radii)
      CALL DUMMY (1)
	CALL DREAD (THBL,'F12.6') !Radial width of boundary layer
	CALL DUMMY (1)
      CALL DREAD (FRC,'F12.6')!Fraction of rings inside circle of max. face Teff
      CALL DUMMY (1)
      CALL IREAD (NTYPE)		!Key for outer accretion disk boundary
C     If NTYPE=1, boundary is vertical cylinder
C     If NTYPE=2, boundary is half torus
C     If NTYPE.EQ.1, produce X,Y,Z coods of rim as on a cylinder.
C     If NTYPE.NE.1, produce X,Y,Z coods of rim as on a half toroid.
C     Check consistency of NTYPE, RA, RB, and HV.
      IF (NTYPE.EQ.2) THEN
		IF ((RA-RB).LT.HV) NTYPE=1
	END IF
      CALL DUMMY(2)
C     Read input from PGA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN3)
C******************************************************
      IF (NRM.EQ.0) GO TO 10
C     Calculate component separation in solar radii
C     RI=semi-major axis of relative orbit, in cgs units
C     
      A=RI/SNRAD
C     Calculate test value for radius of central star, in solar radii,
C     and mass of central star
C     ISTAR=star with accretion disk
      IF (ISTAR.EQ.I) THEN
C     Note that VR(I,KST) is the radius on the equator, and at the
C     point closest to the companion.
         RBSTAR=VR(I,NTH/2+1,1)*A
         VMS=VMI
      ELSE
         RBSTAR=VR(3-I,NTH/2+1,1)*A
         VMS=QS*VMI
      END IF
C     RBSTAR now is in solar radii
C     VMS is in gm.
C     If the specified radius of the innermost ring is less than RBSTAR,
C     reset RB. In any case write output that gives informative data.
      RBSV=RB
      IF (RB.LT.RBSTAR) RB=RBSTAR
C     RBSTAR is the radius of the central star
C     RB is the radius of the innermost ring
C
C     Calculate dimensions in units of component separation
      RAP=RA/A
      RBP=RB/A
      HVP=HV/A
      RSTR=RBSTAR*SNRAD
C     RSTR is in cm. It is the radius of the central star.
      GVSTR=GRAV*VMS/RSTR**2
C     GVSTR is the surface gravity of the central star, in cgs units.
C     Read coefficients for angular velocities of rings
C     Note that the angular vel. of the rim is the same as FVF(NRING)
C     The angular velocities are given as fractional differences from
C     Keplerian angular velocities. Thus, if the ring in question has
C     Keplerian angular velocity, the value of FVF(LRING) is 0.0. If
C     the angular velocity is 110% of Keplerian, FVF(LRING)=0.10. If
C     the angular velocity is 90% of Keplerian, FVF(LRING)=-0.10.
      READ (IN4, 101) (FVF(K),K=1,NRING)
C
C     ******************************
C     Rim of accretion disk
C     ******************************
C
C     Set nominal rim radii for cylindrical model.
      DO K=1,NRM
      RMR(K)=RAP
      END DO
C     Calculate central opening angle of accretion disk
C     This sets the small theta boundary of visibility on the star disk
C     This section is valid for either the cylindrical or toroidal 
C     model.
C     HVP is half thickness of rim, RAP is outer radius of rim, both in
C     units of component separation.
      THETA=DATAN(HVP/RAP)
C     THETAD is polar angle of top rim.
      THETD=PIO2-THETA
C     Calculate the corresponding large theta boundary
C     THETB is the polar angle of bottom rim.
      THETB=PIO2+THETA
C     Calculate component separation in cm
      ACM=A*SNRAD
      IF (NRM.GT.MRM) CALL PMDSTOP
      IF (NSG.GT.MSG) CALL PMDSTOP
C     NSG must be odd number
      KTS=MOD(NSG,2)
      IF (KTS.NE.1) NSG=NSG-1
C     NRM must be odd number
      KTS=MOD(NRM,2)
      IF (KTS.NE.1) NRM=NRM-1
      KLM=(NRM+1)/2
      DIV=KLM-1
C     Calculate array of theta values bounding rim divisions
C     Start at the top of the rim, for the smallest theta value.
C     K=1 designates the top of the rim. NRM designates the bottom.
C     The theta values are evenly spaced.
C     Remember that theta is measured from the polar axis.
		NLM=(NRM+1)/2
      IF (NTYPE.EQ.1) THEN
		DO K=1,NRM
			ZVL=(KLM-K)*HVP/DIV      
			THR(K)=PIO2-DATAN(ZVL/RAP)
C     Calculate values of sin(theta)
			SNTR(K)=DSIN(THR(K))
		END DO
C     Calculate array of gravity values
C     The rim is cylindrical in this approximation
		DO K=1,NRM
			ZVL=(KLM-K)*HVP/DIV
			DIST=DSQRT((RA*SNRAD)**2+(ZVL*SNRAD)**2)
			GVRM=GRAV*VMS/(DIST)**3*DABS(ZVL)*SNRAD
			IF (GVRM.LT.1.D-4) GVRM=1.D-4
			GR(K)=GVRM
		END DO
C     (RMR values are already set)
      ELSE
C     Note that the rim is a half toroid at this approximation
C      NTYPE=2
C     The distance RMR(k) is measured from the center of the central star
C     to the point on the half toroid, NOT in the orbital plane.
C     This differs from the distances to the face rings, which are measured
C     in the orbital plane.
		DO K=1,NRM
			ZVL=(KLM-K)*HVP/DIV      
			THR(K)=PIO2-DATAN(ZVL/(RAP-HVP))
C     Calculate values of sin(theta)
			SNTR(K)=DSIN(THR(K))
		END DO
		DO K=1,NLM-1
			ZVL=(RAP-HVP)/DTAN(THR(K))
			IF (K.EQ.1) ZVL1=ZVL
			ARG=ZVL/HVP
			IF (DABS(ARG).GT.1.D0) ARG=ARG/DABS(ARG)
			VC(K)=PI-DASIN(ARG)
			RMR(K)=DSQRT(HVP**2+(RAP-HVP)**2
     $		  -2.D0*HVP*(RAP-HVP)*DCOS(VC(K)))
			DIST=RMR(K)*A*SNRAD
C     The gravity values are in cm/sec/sec
			IF (K.EQ.1) GR(K)=GRAV*VMS/DIST**3*ZVL*SNRAD
			IF (K.GT.1) GR(K)=GR(1)
			RMR(NRM-K+1)=RMR(K)
			GR(NRM-K+1)=GR(K)
			VC(NRM-K+1)=TWPI-VC(K)
		END DO
		RMR(NLM)=RAP
		VC(NLM)=PI
		DIST=RMR(NLM)*A*SNRAD
		GR(NLM)=GR(NLM-1)
      END IF
C     Calculate array of phi values for rim segments
      DIV=NSG-1
      DO K=1,NSG
      PHIR(K)=(K-1)*TWPI/DIV
      END DO
C
C	**********************************
C
      DO K=1,NRM
C     Calculate other geometric parameters for rim
         VTH=THR(K)
         SNTH=DSIN(VTH)
         COSTH=DCOS(VTH)
C     Choose the type rim
C     If NTYPE.EQ.1, set rim parameters for a cylindrical section rim.
         IF (NTYPE.NE.1) GO TO 27
         COT=COSTH/SNTH
C
         DO J=1,NSG
            VPHV=PHIR(J)
            SNPH=DSIN(VPHV)
            COSPH=DCOS(VPHV)
C     Calculate direction cosines of normal to rim
            ZLAR(K,J)=COSPH
            ZMAR(K,J)=SNPH
            ZNAR(K,J)=0.0D0
C     Calculate direction cosines of radius vector, rim
            ALMAR(K,J)=SNTH*COSPH
            AMUAR(K,J)=SNTH*SNPH
            ANUAR(K,J)=COSTH
C     Calculate cos(beta) for rim
            CSBR(K,J)=ZLAR(K,J)*ALMAR(K,J)+ZMAR(K,J)*AMUAR(K,J)
     $                +ZNAR(K,J)*ANUAR(K,J)
C     Calculate X,Y,Z coordinates of rim points
            VXAR(K,J)=RAP*COSPH
            VYAR(K,J)=RAP*SNPH
            VZAR(K,J)=RAP*COT
         END DO
         GO TO 28
C     Calculate other geometric parameters for rim.
C     NTYPE=2
C     This approximation assumes a half toroid for the outer rim.
C
   27		  ZVL=(RAP-HVP)/DTAN(THR(K))
		  XI=PIO2-DASIN(ZVL/RMR(K))
		  SNA=DSIN(XI)
		  CSA=DCOS(XI)
            DO J=1,NSG
            VPHV=PHIR(J)
            SNPH=DSIN(VPHV)
            COSPH=DCOS(VPHV)
C     Calculate direction cosines of normal to rim
C     In the polar coordinate diagram, with center at the point
C     in the orbital plane which is the center of the circle
C     describing the half torus, the angle VC(K) is measured from
C     the radius to the torus backwards behind the Z axis. The
C     supplement of this angle is the angle between the radius
C     and the orbital plane, or the angle PI-VC(K). Then the
C     angle "theta" is PIO2 minus this or PIO2-(PI-VC(K)),
C     or VC(K)-PIO2. 
C     
            CS=DCOS(VC(K)-PIO2)
            SN=DSIN(VC(K)-PIO2)
            ZLAR(K,J)=SN*COSPH
            ZMAR(K,J)=SN*SNPH
            ZNAR(K,J)=CS
C     Calculate direction cosines of radius vectors to rim
            ALMAR(K,J)=SNA*COSPH
            AMUAR(K,J)=SNA*SNPH
		  ANUAR(K,J)=CSA
C     Calculate X,Y,Z coordinates of rim points
            VXAR(K,J)=RMR(K)*ALMAR(K,J)
            VYAR(K,J)=RMR(K)*AMUAR(K,J)
            VZAR(K,J)=RMR(K)*ANUAR(K,J)
            END DO
   28 END DO
C
C     ********************************
C     Face of accretion disk
C     ********************************
C     
C     Set outer radius for face of disk
      IF (NTYPE.EQ.1) RFACE=RAP
      IF (NTYPE.EQ.2) RFACE=RMR(1)*DSIN(THR(1))
C     Note that THETA is measured from the polar axis 
C     The limiting anges, measured from the polar axis,
C     are THETD and THETB.      
      COT=DCOS(THETD)/DSIN(THETD)
	SINEN=DSIN(THETD)
      AA=DCOS(THETD)/(1.D0+COT**2)
C     Now do face of disk      
      IF (FRC.GT.0.D0) THEN
         HLF=((100.D0*NRING)*FRC)/100.D0
         NHLF=HLF+1
C     NHLF must be an odd number      
         NFTST=MOD(NHLF,2)
         IF (NFTST.EQ.0) NHLF=NHLF+1
      ELSE
         NHLF=1
      END IF      
      IF (NHLF.LT.3) NHLF=3
      DIV=NHLF-1
      VMAX=1.36111111111*RBSTAR         != RBSTAR*(7/6)**2
	IF (VMAX.GT.RA/2.D0) VMAX=RA/2.D0
C     VMAX-RB is the distance from the inner radius of the accretion disk to the
C     point of maximum Teff. in units of solar radius.
C     RGWD is the width of each face annulus, within the distance VMAX,
C     in units of component separation.
      IF (VMAX.GT.RB) THEN
		RGWD=(VMAX-RB)/DIV             !increment in ring radii, inner pt.
	ELSE
		DIV=NRING-1
		RGWD=(RA-RB)/DIV
	END IF
C     Set limit on width of boundary layer
      IF (THBL.GT.RGWD) THBL=RGWD
C     Do first group of face rings
      RGRD(1)=RB
	DIST=RGRD(1)*SNRAD/SINEN
	GF(1)=GRAV*VMS*AA/DIST**2
	RGRD(2)=RB+THBL
	DIST=RGRD(2)*SNRAD/SINEN
	GF(2)=GRAV*VMS*AA/DIST**2
      DO K=3,NHLF
      RGRD(K)=RGRD(K-1)+RGWD                !In solar radii
      DIST=RGRD(K)*SNRAD/SINEN					 !In cm.
      GF(K)=GRAV*VMS*AA/DIST**2			 !Gravity, vertical component
      END DO
   29 DIV=NRING-NHLF
      IF (VMAX.GT.RB) THEN  
		RGWD=(RFACE*A-VMAX)/DIV            !solar radii
	END IF		
	IF (RGWD.LE.0.D0) THEN
		RGWD=(RA-RB)/DIV
		IF (RGWD.LE.0.D0) CALL PMDSTOP
	END IF
C     Do second group of face rings
      DO K=NHLF+1,NRING
	IF (VMAX.GT.RB) THEN
		RGRD(K)=VMAX+(K-NHLF)*RGWD
	ELSE
		RGRD(K)=RGRD(NHLF)+(K-NHLF)*RGWD
	END IF
      DIST=RGRD(K)*SNRAD/SINEN
      GF(K)=GRAV*VMS*AA/DIST**2
      END DO
C     Convert ring radii to units of component separation
      DO K=1,NRING
         RGRD(K)=RGRD(K)/A
      END DO
C     Specify bottom face
      VTH=THR(NRM)
      SNTH=DSIN(VTH)
C     Save values of sin(theta)
      DO K=1,NRING
      SNTF(K)=SNTH
      END DO
      COSTH=DCOS(VTH)
      COT=COSTH/SNTH
      DO K=1,NRING
         DO J=1,NSG
         VPHV=PHIR(J)
         SNPH=DSIN(VPHV)
         COSPH=DCOS(VPHV)
C     Calculate direction cosines of central plane normal
         ZLAF(J)=0.0                        ! otherwise (-COSTH*COSPH)
         ZMAF(J)=0.0                        ! otherwise (-COSTH*SNPH)
         ZNAF(J)=1.0                        ! otherwise (SNTH)
C     Set direction cosines of radius vector
         ALMAF(J)=SNTH*COSPH
         AMUAF(J)=SNTH*SNPH
         ANUAF(J)=COSTH
C      Calculate cos(beta) for bottom face. Note that there is no
C      allowance for tilt of bottom face, or for non-synchronous
C      rotation
          CSBF(K,J)=ZLAF(J)*ALMAF(J)+ZMAF(J)*AMUAF(J)+ZNAF(J)*ANUAF(J)
C      Calculate X,Y,Z coods. of bottom face points
C      The bottom face will always be seen, not the top face
         VXAF(K,J)=RGRD(K)*COSPH
         VYAF(K,J)=RGRD(K)*SNPH
         VZAF(K,J)=RGRD(K)*COT
         END DO
       END DO
      WRITE (NOUT1,107) NRM,NSG
	WRITE (NOUT1,111) NRING,NSG
      WRITE (NOUT1,108) RA,RB,HV
      WRITE (NOUT1,109) RBSTAR,RBSV
      WRITE (NOUT1,110) A
C     Calculate height of boundary layer
      HVBL=DSQRT(THBL*RBSTAR)
	WRITE (NOUT1,112) HVBL
	WRITE (NOUT1,113) THBL
C     Reset the outer boundary of the innermost face ring
C     to equal the value for radiating area the same as the
C     boundary layer.
C      RSAVE=RGRD(2)
C      RGRD(2)=DSQRT(2.D0*RBP*HVBL+RBP**2)
C     Override the previous setting if the face differs appreciably
C     from a standard accretion disk face.
C      IF (RGRD(2).GT.RGRD(3)) RGRD(2)=RSAVE
C
C
C     ***********************
C          Central Star
C     ***********************
C
      IF (ISTAR.EQ.1) L=I
      IF (ISTAR.EQ.2) L=3-I
C     Locate the theta index for the portion of the star above the
C     accretion disk, and the theta index for the portion below
      ITHU=NTH
      ITHL=NTH
      DO ITH=1,NTH
	  VTH=TH(L,ITH)
      IF (VTH.LT.THETD) ITHU=ITH
      IF (VTH.LT.THETB) ITHL=ITH
      END DO
      ITHL=ITHL+1
   10 CALL AWTO(NRM,NSG,NRING,RAP,RBP,HVP,ACM,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,NOUT2)
   99 CONTINUE
C   99 CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,FVL2)
      TM=(FVL2-FVL1)*1440.D0
      WRITE (NOUT1, 205) TM
      CLOSE ( UNIT=7,     STATUS='KEEP')
      CLOSE ( UNIT=IN2,   STATUS='KEEP')
      CLOSE ( UNIT=IN3,   STATUS='KEEP')
	CLOSE ( UNIT=IN4,   STATUS='KEEP')
      CLOSE ( UNIT=NOUT1, STATUS='KEEP')
      CLOSE ( UNIT=NOUT2, STATUS='KEEP')
	OPEN (UNIT=IN1, FILE = FL1(1), STATUS = 'OLD')
	WRITE (IN1,102) KEYGOOD
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
      STOP
      END PROGRAM ACPGA
