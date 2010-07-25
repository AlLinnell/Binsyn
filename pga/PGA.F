      PROGRAM PGA
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
      DIMENSION ER(MFV),TARY(MTR,MTR),VV(MTW),AR3(MTHM)
      COMMON /NUNIT/ NU,NUOUT,IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1, BFR
      CHARACTER * 10 DATE,TIME,ZONE

	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)

      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES

      DIMENSION FL1(7)
C
      DATA PIO2/1.5707963267948966192D0/
      DATA PI/3.141592653589793238463D0/
      DATA TWPI/6.2831853071795864769D0/
	DATA SNRAD/6.9599D10/     !Sun radius, cgs
	DATA SOLMS/1.9891D33/     !Sun mass, cgs
	DATA GRAV/6.6704D-8/      !Gravity constant
      DATA IN1,IN2/1,2/
      DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5/3,4,5,66,7/
C  
C     THE FOLLOWING ARE INSTALLATION PARAMETERS     
C  
      DATA HFER/0.000000001D0/   
      DATA KL/20/
      DATA I/1/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C  
C  
C  
  100 FORMAT (A36)
  101 FORMAT (I2)
  102 FORMAT (10F8.5)
  103 FORMAT (1H1)
  104 FORMAT (1X,'Program PGA',28X,'DATE=',I2,'/',I2,'/',I4,2X,'TIME=',I
     $2,':'I2,':'I2,'.',I2,/,13X,'**OUT**',/,1X,'*',/,1X,'*',/,1X,'*',/,
     $1X,'*')
  106 FORMAT (10I5)     
  107 FORMAT (1X,'Number of latitude values',28X,'NTH',6X,':',I3,/,1X,'T
     $otal number of points on each component',13X,'NP',7X,':',I5
     $,/,1X,'Limit on number of iterations in root location',7X,'KL',7X,
     $':',I3,/,1X,'Designation of component eclipsed at phase 0.0',7X,'I
     $',8X,':',I3,/,1X,'Number of points,non-polar latitude contour',
     $10X,'NPTH',5X,':',I3)
  109 FORMAT (6E20.12)  
  110 FORMAT (1X,'Mass ratio,in the sense(mass of nearer star at phase 0
     $.0)/(mass',/,10X,'of star being eclipsed at phase 0.0)',8X,'Q',8X,
     $':',F11.8)
  111 FORMAT (1X,'Rotation rate,component I,units of synchronous',7X,'FV
     $(I)',4X,':',F10.7,/,1X,'Rotation rate,component 3-I,units of synch
     $ronous',5X,'FV(3-I)',2X,':',F10.7)
  114 FORMAT (1X,'Potential of first component photosphere,cgs units',3X
     $,'VV(I)',4X,':',E15.8,/,1X,'Potential of second component photosph
     $ere,cgs units',2X,'VV(3-I)',2X,':',E15.8)
  116 FORMAT (1X,'Roche potential,inner critical surface',15X,'OMC(I,1)'
     $,1X,':',F16.13)
  117 FORMAT (1X,'Distance,center of component 1 to L1 point',11X,'VL(I,
     $1)',2X,':',F11.8,/,1X,'Distance,L1 point to center of component 2'
     $,11X,'VL(3-I,1):',F11.8,/,1X,'Distance,center of component 1 to L2
     $ point',11X,'VL(I,2)',2X,':',F11.8)
  118 FORMAT (1X,'Roche potential,photosphere of component 1',11X,'OM(I)
     $',4X,':',F18.13,/,1X,'Roche potential,photosphere of component 2',
     $11X,'OM(3-I)',2X,':',F18.13)
  119 FORMAT (1X,'*',/,1X,'Distance,center of component 1 to inner X-axi
     $s',6X,'XMN(I)',4X,':',F11.8,/,10X,'intercept',/,1X,'Distance,cente
     $r of component 2 to inner X-axis',6X,'XMN(3-I)',2X,':',F11.8,/,10X
     $,'intercept',/,1X,'Value of Y-axis photosphere intercept at interf
     $ace',2X,'YMN',7X,':',F11.8,/,1X,'*',8X,'Differs from 0,0 only if c
     $ontact system',/,1X,'Value of Z-axis photosphere intercept at inte
     $rface',2X,'ZMN',7X,':',f11.8,/,1X,'*',8X,'Differs from 0.0 only if
     $ contact system')
  120 FORMAT (1X,'Separation of centers,in cm.',25X,'RI',7X,':',E15.8)
  121 FORMAT (1X,'First component',14X,'Polar radius',11X,'VR(I,1)',3X,'
     $:',F11.8,/,1X,'Units of',21X,'Point radius',
     $11X,'VR(I,KST)',1X,':',F11.8,/,1X,'component separation',9X,'Side
     $ radius',11X,'VR(I,KQT)',1X,':',F11.8,/,30X,'Back radius',12X,
     $'VR(I,KHF)',1X,':',F11.8)
  122 FORMAT (30X,'Mean radius',22X,':',F11.8,/,30X,'Projected area,equi
     $v.sphere',6X,':',F11.8)
  123 FORMAT (30X,'Polar gravity (cm/sec^2)',9X,':',E15.8,/,30X,'Point 
     $gravity',19X,':',E15.8,/,30X,'Side gravity',21X,':',E15.8,/,30X,'B
     $ack gravity',21X,':',E15.8)
  124 FORMAT (1X,'Second component',13X,'Polar radius',9X,'VR(3-I,1)',3X
     $,':',F11.8,/,1X,'Units of',21X,'Point radius',9X,'VR(3-I,KST)',1X,
     $':',F11.8,/,1X,'component separation',9X,'Side radius',10X,'VR(3-I
     $,KQT)',1X,':',F11.8,/,30X,'Back radius',10X,'VR(3-I,KHF)',1X,':',
     $F11.8)
  125 FORMAT (1X,'Roche potential,outer critical surface',15X,'OMC(I,2)'
     $,1X,':',F11.8)
  127 FORMAT (1X,'Filling factor',39X,'FL',7X,':',1PF11.8)   
  128 FORMAT (30X,'Volume',27X,':',F11.8,/,30X,'Volume,cgs units',17X,':
     $',E15.8)
  129 FORMAT (30X,'Mass(gm.)',24X,':',E11.4,/,30X,'Mean density(gm/cm^3)
     $',12X,':',E11.4)
  130 FORMAT (30X,'Mass(solar masses)',15X,':',E11.4)
  131 FORMAT (1X,'Separation of centers in solar radii',26X,':',F9.6)
  134 FORMAT (1X,'NUMBER OF LATITUDE GRID VALUES BETWEEN POLE AND EQUATO
     $R=',I5,5X,'AND THIS VALUE EXCEEDS THE LIMIT OF 101')  
  135 FORMAT (1X,'NUMBER OF LATITUDE GRID VALUES BETWEEN POLE AND EQUATO 
     $R=',I5,5X,'AND THIS VALUE IS TOO SMALL')
  136 FORMAT (1X,'NUMBER OF DIVISON POINTS ON HALF THETA CIRCLE=',I5,
     $5X,'AND THIS VALUE EXCEEDS LIMIT OF 101')         
  139 FORMAT (1X,'DIVISION POINT TOTAL EXCEEDS LIMIT OF 10101')     
  140 FORMAT (1X,'Potential of inner critical surface,cgs units',8X,'OML
     $11',4X,':',E15.8,/,1X,'Potential of outer critical surface,cgs uni
     $ts',8X,'OML21',4X,':',E15.8)
  143 FORMAT (1X,'Value of the contact parameter',23X,'KC',7X,':',I3,/,1
     $X,'*',7X,'If KC=0,this is a contact system',/,1X,'*',7X,'If KC=1,t
     $his is a detached or semidetached system')
  146 FORMAT (1H )      
  147 FORMAT (1X,'ORDINAL THETA VALUE=',I5,5X,'OF A TOTAL OF',I5,5X,'THE 
     $TA VALUES')
  148 FORMAT (1X,'LISTING OF INTERPOLATION APPROXIMATION ERRORS FOR PROJ
     $ECTED AREAS,ASSUMING ',/,1X,'ORBITAL INCLINATION VI=PIO2 AND ORBIT
     $AL LONGITUDE PSI=',F13.10,/,1X,'COMPONENT=',I5)
  149 FORMAT (1X,'VALUES OF PHI COORDINATE') 
  150 FORMAT (10F13.10) 
  151 FORMAT (1X,'VALUES OF PROJECTED AREA ELEMENTS',/,1X,'(NEGATIVE VAL 
     $UES REPRESENT REGIONS BELOW THE HORIZON)')    
  152 FORMAT (1X,'VALUES OF APPROXIMATION ERROR,AT FIVE POINTS BRIDGING  
     $SUCCESSIVE PHI VALUES')  
  153 FORMAT (5X,I5,5F20.10)   
  154 FORMAT (3F11.9,2F12.9,F11.9,E12.6,4F11.8,/,F12.9)      
  156 FORMAT (5E20.12)   
  158 FORMAT (1X,'VALUE OF THETA=',F13.10)   
  159 FORMAT (1X,'**POTENTIAL RESET TO OUTER CRITICAL SURFACE**')
  160 FORMAT (1X,'Notes:')
  161 FORMAT (10X,'Component I overflowed outer critical surface.Reset p
     $hotosphere at outer critical surface.')
  162 FORMAT (10X,'Component 3-I overflowed outer critical surface.Reset
     $ photosphere at outer critical surface.')
  163 FORMAT (10X,'Overcontact system.Set VV(3-I)=VV(I).')
  164 FORMAT (10X,'VV(I) overcontact,VV(3-I) undercontact.Set VV(I) at i
     $nner critical surface.')
  165 FORMAT (10X,'VV(3-I) overcontact,VV(I) undercontact.Set VV(3-I) at
     $ inner critical surface.')
  166 FORMAT (1X,'First component (R_sun)',6X,'Polar radius',11X,'VR(I,1
     $)',3X,':',F11.8,/,30X,'Point radius',11X,'VR(I,KST)',1X,':',F11.8,
     $/,30X,'Side radius',12X,'VR(I,KQT)',1X,':',F11.8,/,30X,'Back radiu
     $s',12X,'VR(I,KHF)',1X,':',F11.8)
  167 FORMAT (1X,'Second component (R_sun)',5X,'Polar radius',11X,'VR(I,
     $1)',3X,':',F11.8,/,30X,'Point radius',11X,'VR(I,KST)',1X,':',F11.8
     $,/,30X,'Side radius',12X,'VR(I,KQT)',1X,':',F11.8,/,30X,'Back radi
     $us',12X,'VR(I,KHF)',1X,':',F11.8)
  205 FORMAT (1X,'Program running time=',F12.4,2X,'seconds')
C
C     THE FOLLOWING IS AN INSTALLATION PARAMETER    
      EL=1.0D-10
C
      SUNMASS=SOLMS
      SUNRAD=SNRAD
C  
C  
C  
C***************************************************
      OPEN (UNIT=9,     FILE='PGAA.ILS', STATUS='OLD')
      READ (9, 100) BFR
      OPEN (UNIT=8,     FILE= BFR, STATUS='OLD')
      READ (8, 100) (FL1(J),J=1,7)
      OPEN (UNIT=IN1,   FILE=FL1(1), STATUS='OLD')
      WRITE (IN1,101) KEYBAD
      REWIND IN1
      OPEN (UNIT=IN2,   FILE=FL1(2), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(3), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(4), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(5), STATUS='REPLACE')



      OPEN (UNIT=NOUT4, FILE=FL1(6), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(7), STATUS='REPLACE')
c***************************************************
C
      LBLERR=0
C     Find label and point to first data line
      LABEL = ' ***PGA'
      NU=IN2
      CALL GTLBL(LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NTH)	  !Number of colatitudes ,pole to pole, inclusive
      CALL IREAD (NPTH)	  !Number of points on each theta circle
      CALL IREAD (NR)		  !Number of radial increments, volume calculation
      CALL IREAD (KDB)	  !Key for debug printout
      CALL DUMMY(2)
      CALL IREAD (LRT)	  !Key for type stellar model. Roche=1
      CALL DUMMY(8)
      CALL DREAD (FV(I),'F14.7')	  !Rotation parameter, component I
      CALL DREAD (FV(3-I),'F14.7')  !Rotation parameter, component 3-I
      CALL DREAD (RA,'F11.8')		  !Spherical model radius, comp. I
      CALL DREAD (RB,'F11.8')		  !Spherical model radius, comp. 3-I
      CALL DUMMY(1)
      CALL DREAD (VMI,'D19.11')	  !Mass, component I, cgs units
      CALL DREAD (RI,'D20.13')	  !Semi-major axis, cgs units
      CALL DUMMY(1)
      CALL DREAD (QS,'F14.8')		  !Mass ratio
      CALL DUMMY(1)
      CALL DREAD (OM(I),'D19.11')	  !Photospheric potential, comp. I
      CALL DREAD (OM(3-I),'D19.11') !Photospheric pot., comp. 3-I
C******************************************************
C     Transform OM(3-I)
C
      OMA=OM(3-I)
      OM(3-I)=OMA/QS+(QS-1.D0)/QS/2.D0
C
C     Calculate VV(I), VV(3-I)
C
      A=QS*QS/(1.D0+QS)/2.D0
      VV(I)=GRAV*VMI/RI*(OM(I)+A)
	QP=1.D0/QS
	A=QP*QP/(1.D0+QP)/2.D0
	VV(3-I)=GRAV*QS*VMI/RI*(OM(3-I)+A)
C
      IF (NTH.GT.MTHM) THEN
		WRITE (NOUT1,206)
		STOP 'PGA: NTH TOO LARGE'
	END IF
  206 FORMAT (1X,'NTH TOO LARGE')
	IF (NPTH.GT.MPHM) THEN
		WRITE (NOUT1,207)
		STOP 'PGA: NPTH TOO LARGE'
	END IF
  207 FORMAT (1X,'NPTH TOO LARGE')
	IF (NR.GT.100) THEN
		WRITE (NOUT1,208)
		STOP 'PGA: NR TOO LARGE'
	END IF
  208 FORMAT (1X,'NR TOO LARGE')
      KEYA=0
      KEYB=0
      KEYC=0
      KEYD=0
      KEYE=0
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (NOUT1,103)
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
C     SET CALL KEY IN A COMMON ARRAY
      M(2)=LRT   
      IF (M(2).NE.2) GO TO 17
      IF ((RA+RB).GT.1.D0) CALL PMDSTOP
   17 NT=NPTH*(NTH-2)+2
      NP=NT
C     Make sure there is an odd number of colatitudes
      KK=MOD(NTH,2)     
      IF (KK.EQ.0) NTH=NTH+1   
      WRITE (NOUT1,107) NTH,NP,KL,I,NPTH    
      IF (NTH.GT.MTHM) GO TO 13                !Error
      IF (NPTH.GT.MPHM) GO TO 14                 !Error
C     PASS RA,RB FOR SPHERICAL CASE
      VR(I,1,1)=RA
      VR(3-I,1,1)=RB
      WRITE (NOUT1,110) QS  
      KRP=0
   31 WRITE (NOUT1,114) VV(I),VV(3-I)     
      GVI=GRAV*VMI/RI/RI
      GVJ=GVI*QS 
      Q=QS
      VL(I,1)=0.D0
      VL(I,2)=0.D0
      VL(3-I,1)=0.D0     
      VL(3-I,2)=0.D0    
      CALL STUP(I,KL,EL,FL,QS,VMI,RI,VV(I),VV(3-I),QN,KC,NOUT1)     
      CALL IVPT(I,QS,VMI,RI,OML11,OML21)     
      WRITE (NOUT1,140) OML11,OML21
	IF (M(2).EQ.2) GO TO 20
C     TEST FOR COMPONENT I OVERFLOWING OUTER CRITICAL SURFACE
      IF (VV(I).GT.OML21) GO TO 19
	KEYA=1
	VV(I)=OML21
	VV(3-I)=OML21
	WRITE (NOUT1,159)
C     TEST FOR COMPONENT 3-I OVERFLOWING OUTER CRITICAL SURFACE
   19 IF (VV(3-I).GT.OML21) GO TO 20
      KEYB=1
	VV(3-I)=OML21
	VV(I)=OML21
	WRITE (NOUT1,159)
   20 WRITE (NOUT1,143) KC	   
   21 WRITE (NOUT1,127) FL  
      WRITE (NOUT1,111) FV(I),FV(3-I)
      WRITE (NOUT1,118) OM(I),OMA
      WRITE (NOUT1,116) OMC(I,1)
      WRITE (NOUT1,125) OMC(I,2)
      RISNU=RI/SUNRAD
      WRITE (NOUT1,131) RISNU
      WRITE (NOUT1,120) RI
      WRITE (NOUT1,117) VL(I,1),VL(3-I,1),VL(I,2)    
      WRITE (NOUT4,156) OM(I),OMA,QS,VMI,RI
   10 WRITE (NOUT1,119) XMN(I),XMN(3-I),YMN,ZMN  
      CALL DVPT(I,NTH,NPTH)
      NT=NTL(I,NTH)
      IF (NT.LE.MTLM) GO TO 18  
      WRITE (NOUT1,139)     
      GO TO 99   
   18 CALL PHIGRID(I,NTH)
      Q=QN
      CALL PHIGRID(3-I,NTH)
      Q=QS
      CALL FILL(I,NTH,EL,QN)
	AA=ZL(I,NTH/2+1,1)
	AB=ZL(3-I,NTH/2+1,1)

C     If either component rotates non-synchronously,
C     calculate revised values of radii.
C     Choose alternative processing for synchronous or non-synch. rot.
C
C     The theory for this section is from Limber, 1963, ApJ, 138, 1112
C     See also Collins, 1963, ApJ, 138, 1134
C
      IF (FV(I).GT.1.D0) THEN
C     Save the polar radius
	    VREF=VR(I,1,1)
		DO ITH=2,NTH-1
			N=NPH(I,ITH)
C     Correct the component radii on colatitude ITH
			DO K=1,N
C     Save the tidally distorted radius
				RVTD=VR(I,ITH,K)
C     Reset the radius to the polar radius, to calculate rotational
C     distortion
				IF (RVTD-VREF.GT.0.001*VREF) THEN
					VR(I,ITH,K)=VREF
				ELSE
					VR(I,ITH,K)=RVTD*0.999D0
				END IF
				AVL=0.5D0*(1.D0+Q)*FV(I)**2*(1.D0-ANU(I,ITH,1)**2)
C     Calculate the rotational distortion.
				CALL ROTDIST(I,AVL,Q,ALM(I,ITH,K),SV,1.D-9)
C     Calculate the amount of rotational distortion
				SVDF=SV-VREF
C     Calculate the amount of tidal distortion
				RDDIFF=RVTD-VREF
C     Only allow tidal distortion if FV(I)=1.0 !!!!!!
				IF (FV(I).GT.1.0) THEN
					RDCORR=SVDF
				ELSE
					RDCORR=RDDIFF
				END IF
C     Reset the radius at ITH,K to include combined distortion.
				IF (RDCORR.GT.0.D0) THEN
					VR(I,ITH,K)=VREF+RDCORR
				ELSE
					VR(I,ITH,K)=RVTD
				END IF
C     Reset values of gravity and ZL, ZM, ZN, CSB.
				CALL RCHERD(I,ITH,K)
			END DO
		END DO
	END IF
	Q=QN
      IF (FV(3-I).GT.1.D0) THEN
		VREF=VR(3-I,1,1)
		DO ITH=2,NTH-1
			N=NPH(3-I,ITH)
			DO K=1,N
				RVTD=VR(3-I,ITH,K)
				IF (RVTD-VREF.GT.0.001*VREF) THEN
					VR(3-I,ITH,K)=VREF
				ELSE
					VR(3-I,ITH,K)=RVTD*0.999D0
				END IF
				AVL=0.5D0*(1.D0+Q)*FV(3-I)**2*(1.D0-ANU(3-I,ITH,1)**2)
				CALL ROTDIST(3-I,AVL,Q,ALM(3-I,ITH,K),SV,1.D-9)
				SVDF=SV-VREF
				RDDIFF=RVTD-VREF
				RDCORR=RDDIFF+SVDF
				IF (RDCORR.GT.0.D0) THEN
					VR(3-I,ITH,K)=VREF+RDCORR
				ELSE
					VR(I,ITH,K)=RVTD
				END IF
C     Reset values of gravity and ZL, ZM, ZN, CSB.
				CALL RCHERD(3-I,ITH,K)
			END DO
		END DO
	END IF
	Q=QS
C     Gravity values, grid points, component I
      DO ITH=1,NTH
	N=NPH(I,ITH)
      DO 11 K=1,N
      G(I,ITH,K)=GVI*G(I,ITH,K) 
   11 CONTINUE
      END DO
C     Gravity values, grid points, component 3-I
      DO ITH=1,NTH
	N=NPH(3-I,ITH)      
      DO 12 K=1,N      
      G(3-I,ITH,K)=GVJ*G(3-I,ITH,K)    
   12 CONTINUE
      END DO   
      N=NPH(I,NTH/2+1)      
      KHF=N/2+1
      KQT=N/4+1   
      A=VR(I,1,1)  
      B=VR(I,NTH/2+1,1)
      C=VR(I,NTH/2+1,KQT)
      D=VR(I,NTH/2+1,KHF)
      WRITE (NOUT1,121) A,B,C,D    
      CALL VOLUME(I,A,B,NR,NTH,VOL1)
	E3=0.75D0/PI*VOL1
	E=E3**0.3333333333333D0
	F=PI*E*E
      WRITE (NOUT1,122) E,F 
      VLCGS1=RI*VOL1*RI*RI
      VMSNU=VMI/SUNMASS
      DENS1=VMI/VLCGS1
      VM2=VMI*QS
      WRITE (NOUT1,128) VOL1,VLCGS1
      WRITE (NOUT1,130) VMSNU
      WRITE (NOUT1,129) VMI,DENS1
      A=A*RISNU
      B=B*RISNU
      C=C*RISNU
      D=D*RISNU
      WRITE (NOUT1,166) A,B,C,D
      A=G(I,1,1)   
      B=G(I,NTH/2+1,1) 
      C=G(I,NTH/2+1,KQT) 
      D=G(I,NTH/2+1,KHF) 
      WRITE (NOUT1,123) A,B,C,D    
      N=NPH(3-I,NTH/2+1)    
      KHF=N/2+1
      KQT=N/4+1  
      A=VR(3-I,1,1)
      B=VR(3-I,NTH/2+1,1)     
      C=VR(3-I,NTH/2+1,KQT)     
      D=VR(3-I,NTH/2+1,KHF)     
      WRITE (NOUT1,124) A,B,C,D    
      CALL VOLUME(3-I,A,B,NR,NTH,VOL2)
	E3=0.75D0/PI*VOL2
	E=E3**0.3333333333333D0
      F=PI*E*E   
      WRITE (NOUT1,122) E,F 
      VLCGS2=RI*VOL2*RI*RI
      WRITE (NOUT1,128) VOL2,VLCGS2
      DENS2=VM2/VLCGS2
      VMSNU=VM2/SUNMASS
      WRITE (NOUT1,130) VMSNU
      WRITE (NOUT1,129) VM2,DENS2
      A=A*RISNU
      B=B*RISNU
      C=C*RISNU
      D=D*RISNU
      WRITE (NOUT1,167)A,B,C,D
      A=G(3-I,1,1) 
      B=G(3-I,NTH/2+1,1)      
      C=G(3-I,NTH/2+1,KQT)      
      D=G(3-I,NTH/2+1,KHF)      
      WRITE (NOUT1,123) A,B,C,D    
      WRITE (NOUT1,160)
      IF (KEYA.EQ.1) WRITE (NOUT1,161)
      IF (KEYB.EQ.1) WRITE (NOUT1,162)
      IF (KEYC.EQ.1) WRITE (NOUT1,163)
      IF (KEYD.EQ.1) WRITE (NOUT1,164)
      IF (KEYE.EQ.1) WRITE (NOUT1,165)
      CALL WTOUT1(FL,HFER,I,KC,KL,NT,NTH,NPTH,QS,QN,RI,VMI,VV(I),
     $VV(3-I),NOUT2)
C  
C  
   33 IF (KDB.EQ.0) GO TO 99
      WRITE (NOUT1,103)     
      VI=PIO2
      PSI=TWPI
      CSI=0.D0
      SNI=1.D0
      CM=0.D0
      DO 70 KCT=1,2     
      Q=QS
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SNP,CSP,TARY)
      WRITE (NOUT1,148) PSI,I      
      WRITE (NOUT3,106) I,NTH      
      WRITE (NOUT3,106) (NPH(I,ITH),ITH=1,NTH)   
      WRITE (NOUT3,106) (NTL(I,ITH),ITH=1,NTH)   
      DO 30 ITH=1,NTH   
      SNTHV=SNT(I,ITH)  
      N=NPH(I,ITH)      
      DO 29 K=1,N   
      CALL SGLTP(TH(I,ITH),PH(I,ITH,K),1.D-9,RV,GV,VLV,VMV,
     $VNV,VCSB,VCSG,I,NTH)      		 
      CSPH=DCOS(PH(I,ITH,K)) 
      SNPH=DSIN(PH(I,ITH,K)) 
      VL1(I,ITH,K)=RV*RV/VCSB*SNTHV*VCSG  
      AR3(K)=PH(I,ITH,K)     
      WRITE (NOUT3,154) TH(I,ITH),SNTHV,PH(I,ITH,K),SNPH,CSPH,RV,GV,VLV,
     $VMV,VNV,VCSB,VCSG  
      WRITE (NOUT3,109) VL1(I,ITH,K)   
   29 CONTINUE   
      KLM=KND-1  
      WRITE (NOUT1,146)     
      WRITE (NOUT1,147) ITH,NTH    
      WRITE (NOUT1,158) TH(I,ITH)  
      IF (ITH.EQ.1) GO TO 30   
      WRITE (NOUT1,149)     
      WRITE (NOUT1,150) (PH(I,ITH,K),K=1,N)
      WRITE (NOUT1,146)     
      WRITE (NOUT1,151)     
      WRITE (NOUT1,150) (VL1(I,ITH,K),K=1,N)     
      WRITE (NOUT1,152)     
      DO 28 K=1,N   
      PHBG=PH(I,ITH,K)      
      DPH=(PH(I,ITH,K+1)-PH(I,ITH,K))/4.D0
      DO 27 J=1,5
      PHVL=PHBG+(J-1)*DPH      
      CALL TPL(I,ITH,2,N,AA,PHVL,AR3,MTHM,N)
      CALL SGLTP(TH(I,ITH),PHVL,0.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,
     $VCSG,I,NTH)    
      ZZ=RV*RV/VCSB*SNTHV*VCSG 
      ER(J)=AA-ZZ
   27 CONTINUE   
      WRITE (NOUT1,153) K,(ER(J),J=1,5)  
   28 CONTINUE   
      WRITE (NOUT1,146)     
   30 CONTINUE   
      I=3-I      
      PSI=PSI+PI 
      IF (PSI.GE.TWPI) PSI=PSI-TWPI   
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SNP,CSP,TARY)
      Q=QN
      WRITE (NOUT1,148) PSI,I      
      WRITE (NOUT3,106) I,NTH      
      WRITE (NOUT3,106) (NPH(I,ITH),ITH=1,NTH)   
      WRITE (NOUT3,106) (NTL(I,ITH),ITH=1,NTH)   
      DO 60 ITH=1,NTH   
      SNTHV=SNT(I,ITH)  
      N=NPH(I,ITH)      
      DO 59 K=1,N   
      CALL SGLTP(TH(I,ITH),PH(I,ITH,K),0.000000001D0,RV,GV,VLV,VMV,
     $VNV,VCSB,VCSG,I,NTH)      
      CSPH=DCOS(PH(I,ITH,K)) 
      SNPH=DSIN(PH(I,ITH,K)) 
      VL1(I,ITH,K)=RV*RV/VCSB*SNTHV*VCSG  
      AR3(K)=PH(I,ITH,K)     
      WRITE (NOUT3,154) TH(I,ITH),SNTHV,PH(I,ITH,K),SNPH,CSPH,RV,GV,VLV,
     $VMV,VNV,VCSB,VCSG  
      WRITE (NOUT3,109) VL1(I,ITH,K)   
   59 CONTINUE   
      KLM=KND-1  
      WRITE (NOUT1,146)     
      WRITE (NOUT1,147) ITH,NTH    
      WRITE (NOUT1,158) TH(I,ITH)  
      IF (ITH.EQ.1) GO TO 60   
      WRITE (NOUT1,149)     
      WRITE (NOUT1,150) (PH(I,ITH,K),K=1,N)      
      WRITE (NOUT1,146)     
      WRITE (NOUT1,151)     
      WRITE (NOUT1,150) (VL1(I,ITH,K),K=1,N)     
      WRITE (NOUT1,152)     
      DO 58 K=1,N   
      PHBG=PH(I,ITH,K)      
      DPH=(PH(I,ITH,K+1)-PH(I,ITH,K))/4.D0
      DO 57 J=1,5
      PHVL=PHBG+(J-1)*DPH      
      CALL TPL(I,ITH,2,KND,AA,PHVL,AR3,MTHM,N)
      CALL SGLTP(TH(I,ITH),PHVL,0.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,
     $VCSG,I,NTH)    
      ZZ=RV*RV/VCSB*SNTHV*VCSG 
      ER(J)=AA-ZZ
   57 CONTINUE   
      WRITE (NOUT1,153) K,(ER(J),J=1,5)  
   58 CONTINUE   
      WRITE (NOUT1,146)     
   60 CONTINUE   
      I=3-I      
      PSI=PIO2   
      CM=TWPI-PSI
   70 CONTINUE
   99 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
      WRITE (NOUT1, 205) TM
      CLOSE ( UNIT=IN2,   STATUS='KEEP')
	CLOSE ( UNIT=IN3,   STATUS='KEEP')
	CLOSE ( UNIT=IN4,   STATUS='KEEP')
	CLOSE ( UNIT=IN5,   STATUS='KEEP')
	CLOSE ( UNIT=IN6,   STATUS='KEEP')
	CLOSE ( UNIT=IN7,   STATUS='KEEP')
	CLOSE ( UNIT=IN8,   STATUS='KEEP')
      CLOSE ( UNIT=NOUT1, STATUS='KEEP')
      CLOSE ( UNIT=NOUT2, STATUS='KEEP')
      CLOSE ( UNIT=NOUT3, STATUS='KEEP')
      CLOSE ( UNIT=NOUT4, STATUS='KEEP')
      CLOSE ( UNIT=NOUT5, STATUS='KEEP')
	CLOSE ( UNIT=15,    STATUS='KEEP')
      WRITE (IN1, 101) KEYGOOD
      CLOSE ( UNIT=IN1,   STATUS='KEEP')
      STOP 'PGA: Completed successfully.'
   13 WRITE (NOUT1,134) NTH
      GO TO 99
   14 WRITE (NOUT1,135) NPTH 
      GO TO 99
      END PROGRAM PGA
