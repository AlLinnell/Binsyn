      PROGRAM PGC
C	
C     PROGRAM TO CALCULATE TEMPERATURE DISTRIBUTION AND NORMAL INTENSITY 
C     DISTRIBUTION OVER THE SURFACES OF THE TWO COMPONENTS OF A BINARY   
C     STAR SYSTEM
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION WL(MWL),T0(MTW),A(MTW),TE(MTW)
      DIMENSION BCF(MTW),GLOG(MPHM)  
      DIMENSION UA(MTW)   
      DIMENSION UC(MTW),C1(MTW),C2(MTW),C3(MTW),C4(MTW)
      DIMENSION FILNAM(MTW,MFV,MSX)
	DIMENSION FL1(24)
      DIMENSION KGR(MTW),KTMP(MTW),GRPH(MTW,MFV),TMPH(MTW,MSX)
	DIMENSION VLMDA(10),VGMDA(10),VBTDA(10),DLTAA(10)
	DIMENSION VLMDB(10),VGMDB(10),VBTDB(10),DLTAB(10)
	DIMENSION RFL(2,MTHM,MPHM)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1,FILNAM,BFR
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA SGMA/5.66956D-5/              !Stefan-Boltzmann const.    
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
      DATA IN10,IN11,IN12,IN13,IN14,IN15,IN16/10,11,12,13,14,15,16/
	DATA IN17,IN18,IN19/17,18,19/
      DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5/20,21,22,23,24/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
	DATA CVTRAD/57.2957795131D0/
	DATA PIO2/1.5707963267948966192D0/
	DATA PI/3.141592653589793238463D0/
	DATA TWPI/6.2831853071795864769D0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  102 FORMAT (1X,7F10.5)
  103 FORMAT (' ')
  104 FORMAT (1X,'PROGRAM PGC',30X,'DATE=',I2,'/',I2,'/',I4,2X,'TIME=',I
     $2,':',I2,':',I2,'.',I2)
  105 FORMAT (6(1PD11.3))
  106 FORMAT (10I5)     
  107 FORMAT (1X,'Number of wavelengths of observation=',I5,/,1X,
     $'Model atmosphere control=',12X,I5,/,1X,
     $'If this integer=0,use black body approximation,otherwise use mode
     $l atmospheres',/,1X,
     $'Number of reference wavelengths for determination of limb darkeni
     $ng coeffs.',/,20X,' and model atmosphere=',I5,/,1X,
     $'Number of values of surface gravity associated with model atmosph
     $ere=',I5,/,1X,
     $'Number of values of surface temperature associated with model atm
     $osphere=',I5,/,1X,'Key to impose specified polar temperatures=',I5
     $,/,1X,'If key.NE.0,impose specified polar temperatures')
  108 FORMAT (1X,'Limb darkening coefficient, component',I4,1X,
     $'for irradiation effect=',F6.2,/,1X,
     $'Corresponding coefficient,  component',I4,1X,
     $'for irradiation effect=',F6.2)
  109 FORMAT (8F10.5)
  110 FORMAT (1X,'Wavelengths of observation, in microns,=',/,6F12.5)
  111 FORMAT (1X,'Polar temperatures of two components=',2F10.5,1X,
     $'in kilokelvins',/,1X,
     $'Bolometric albedos of two components=',
     $2F10.5,/,1X,'Gravity brightening exponents of two components='
     $,2F10.5)
  112 FORMAT (1X,'Polar effective temperatures of two components, in kil
     $okelvins=',2F7.3,/,1X,'after application of gravity brightening')
  113 FORMAT (1X,'Point temperature, component',I10,5X,'=',F10.5,/,28X,
     $'Side temperature=',F10.5,/,28X,'Back temperature=',F10.5)
  114 FORMAT (1X,'Following temperatures are without irradiation effect'
     $)   
  115 FORMAT (1X,'Following temperatures include irradiation effect')
  116 FORMAT (1X,'Photospheric temperature array, component 1')
  117 FORMAT (1X,'Intensities, B(lambda), normal to surface of component
     $',I10,5X,
     $/,10X,'in ergs/cm.**2/sec/micron/sr ',/,43X,'at wavelength=',
     $F12.5,5X,'microns',/,43X,'Polar intensity=',1PE15.5,/,43X,
     $'Point intensity=',1PE15.5,/,43X,'Side intensity =',1PE15.5,/,43X,
     $'Back intensity =',1PE15.5)
  118 FORMAT (1X,'Contact system.Recalculated polar temperature of compo
     $nent',I5,2X,'is',F12.4)
  119 FORMAT(6F12.7)
  120 FORMAT (1X,'Limb darkening scaling coefficient, component',I5,'='
     $,F8.3,/,37X,'component',I5,'=',F8.3)
  121 FORMAT (1X,'Limb darkening coefficients',/,1X,'Pole,first order=',
     $F10.3,2X,'Second order=',F10.3,/,1X,'Point,first order=',F9.3,2X,
     $'Second order=',F10.3,/,1X,'Side,first order=',F10.3,2X,
     $'Second order=',F10.3,/,1X,'Back,first order=',F10.3,2X,
     $'Second order=',F10.3)
  122 FORMAT (1X,'Photospheric temperature array, component 2')
  123 FORMAT (1H )      
  124 FORMAT (1X,'Empirical temperature coefficients, component',I5,1X,
     $4F8.5)
  125 FORMAT (1X,'Array of normal intensity values, B(lambda), component
     $',I5,/,
     $3X,'in ergs/cm**2/sec/micron/sr, at wavelength=',3X,F12.5)
  126 FORMAT (1X,'Array of first and second order limb darkening coeffic
     $ients for component',I5,/,3X,'at wavelength=',F12.5,5X,'microns')
  127 FORMAT (1X,'Array of log(10) surface gravity values for component'
     $,I5)
  128 FORMAT (1X,'Array of incident flux values for component',I5,/,
     $3X,'in ergs/cm**2/sec')
  130 FORMAT (1X,'ITH=',I5)
  131 FORMAT (6(1PD11.4))
  132 FORMAT (1X,'Array of reflection coefficients for component',I5)
  205 FORMAT (1X,'Program running time=',F15.4,2X,'seconds')
C*****************************************************
      OPEN (UNIT=26,     FILE='PGCA.ILS',  STATUS='OLD')
      READ (26, 100) BFR
      OPEN (UNIT=25,     FILE= BFR,  STATUS='OLD')
      READ (25, 100)     (FL1(J),J=1,24)
      OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')
      WRITE (IN1,101)   KEYBAD
      REWIND IN1
      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')
      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')
      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')
      OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS='OLD')
      OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS='OLD')
      OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS='OLD')
      OPEN (UNIT=IN8,   FILE=FL1(8),  STATUS='OLD')
      OPEN (UNIT=IN9,   FILE=FL1(9),  STATUS='OLD')
      OPEN (UNIT=IN10,  FILE=FL1(10), STATUS='OLD')
	OPEN (UNIT=IN11,  FILE=FL1(11), STATUS='OLD')
      OPEN (UNIT=IN12,  FILE=FL1(12), STATUS='OLD')
	OPEN (UNIT=IN13,  FILE=FL1(13), STATUS='OLD')
      OPEN (UNIT=IN14,  FILE=FL1(14), STATUS='OLD')
	OPEN (UNIT=IN15,  FILE=FL1(15), STATUS='OLD')
      OPEN (UNIT=IN16,  FILE=FL1(16), STATUS='OLD')
	OPEN (UNIT=IN17,  FILE=FL1(17), STATUS='OLD')
      OPEN (UNIT=IN18,  FILE=FL1(18), STATUS='OLD')
	OPEN (UNIT=IN19,  FILE=FL1(19), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(20), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(21), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(22), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(23), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(24), STATUS='REPLACE')
C*****************************************************
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)

C     Read input data
      LABEL = ' ***PGC'
      NU=IN2
      CALL GTLBL ( LABEL,LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD ( NWL)
	IF (NWL.GT.MWL) THEN
		WRITE (NOUT1,206)
		STOP
	END IF
  206 FORMAT (1X,'NWL TOO LARGE')
      CALL IREAD ( MDAT)
      CALL DUMMY (2)
	CALL IREAD ( IRCT)
	CALL DUMMY (2)
      CALL IREAD ( NRF)
      CALL IREAD ( NGR)
      CALL IREAD ( NTMP)
      CALL IREAD ( KY)
      CALL DUMMY (6)
      DO I=1,NWL
      CALL DREAD ( WL(I),'F11.8')
      END DO
      CALL DUMMY (3)
      CALL DREAD (C1(1),'F11.8')
      CALL DREAD (C2(1),'F11.8')
      CALL DREAD (C3(1),'F11.8')
      CALL DREAD (C4(1),'F11.8')
      CALL DREAD (C1(2),'F11.8')
      CALL DREAD (C2(2),'F11.8')
      CALL DREAD (C3(2),'F11.8')
      CALL DREAD (C4(2),'F11.8')
      CALL DUMMY (1)
      CALL DREAD (UC(1),'F11.8')
      CALL DREAD (UC(2),'F11.8')
      CALL DUMMY (1)
      CALL DREAD (TE(1),'F11.8')
      CALL DREAD (TE(2),'F11.8')
      CALL DUMMY(1)
      CALL DREAD (A(1),'F11.8')
      CALL DREAD (A(2),'F11.8')
      CALL DUMMY(1)
      CALL DREAD (BCF(1),'F11.8')
      CALL DREAD (BCF(2),'F11.8')
      CALL DUMMY(1)
      CALL DREAD (UA(1),'F11.8')
      CALL DREAD (UA(2),'F11.8')
	CALL DUMMY(1)
	CALL IREAD ( NITER)
C     Input data on spots
      NU=IN11
	CALL IREAD (NSPA)   !Number of spots on first component
	CALL DUMMY(1)
	CALL IREAD (NSPB)   !Number of spots on second component
	NU=IN12
	CALL DUMMY (2)
	READ (IN12,109) (VLMDA(K),K=1,NSPA)
	DO K=1,NSPA
	VLMDA(K)=VLMDA(K)/CVTRAD
	END DO
	NU=IN13
	CALL DUMMY (2)
	READ (IN13,109) (VGMDA(K),K=1,NSPA)
	DO K=1,NSPA
	VGMDA(K)=VGMDA(K)/CVTRAD
	END DO
	NU=IN14
	CALL DUMMY (2)
	READ (IN14,109) (VBTDA(K),K=1,NSPA)
	DO K=1,NSPA
	VBTDA(K)=VBTDA(K)/CVTRAD
	END DO
	NU=IN15
	CALL DUMMY (2)
	READ (IN15,109) (DLTAA(K),K=1,NSPA)
	NU=IN16
	CALL DUMMY(2)
	READ (IN16,109) (VLMDB(K),K=1,NSPB)
	DO K=1,NSPB
	VLMDB(K)=VLMDB(K)/CVTRAD
	END DO
	NU=IN17
	CALL DUMMY (2)
	READ (IN17,109) (VGMDB(K),K=1,NSPB)
	DO K=1,NSPB
	VGMDB(K)=VGMDB(K)/CVTRAD
	END DO
	NU=IN18
	CALL DUMMY (2)
	READ (IN18,109) (VBTDB(K),K=1,NSPB)
	DO K=1,NSPB
	VBTDB(K)=VBTDB(K)/CVTRAD
	END DO
	NU=IN19
	CALL DUMMY(2)
	READ (IN19,109) (DLTAB(K),K=1,NSPB)
C*********************************************************************
C
C     Prepare data file for calculation of photospheric fluxes
C     Read number of files and file names, separately by binary 
C     component
C     Initially, read range of wavelengths to accept
	IF (MDAT.EQ.0) GO TO 30
      READ (IN10,*) WLST,WLND
      DO ICOMP=1,2
         READ (IN10,106) KGR(ICOMP),KTMP(ICOMP)
      END DO
      DO ICOMP=1,2
         DO J=1,KGR(ICOMP)
            READ (IN10,*) GRPH(ICOMP,J)
         END DO   
         DO K=1,KTMP(ICOMP)
            READ (IN10,*) TMPH(ICOMP,K)
            TMPH(ICOMP,K)=TMPH(ICOMP,K)/1000.D0
         END DO
      END DO         
      DO ICOMP=1,2
         DO J=1,KGR(ICOMP)
            DO K=1,KTMP(ICOMP)
               READ (IN10,*) FILNAM(ICOMP,J,K)
            END DO
         END DO
      END DO
C     Read in data
      NUNIT = 20
      DO ICOMP=1,2
         DO J=1,KGR(ICOMP)
            DO K=1,KTMP(ICOMP)
               OPEN (UNIT=NUNIT,FILE = FILNAM(ICOMP,J,K),STATUS='OLD')
               NWLV=0
               DO 40 LWL=1,10000
                  READ (NUNIT, 129, END=41) WLRD,FLXVL
                  IF ((WLRD.LT.WLST).OR.(WLRD.GT.WLND)) GO TO 40
                  NWLV=NWLV+1
                  RFLX(ICOMP,J,K,NWLV)=FLXVL
                  WLVRY(NWLV)=WLRD
   40          CONTINUE
   41          CLOSE (UNIT=NUNIT, STATUS ='KEEP')
            END DO
         END DO
      END DO
  129 FORMAT (F11.4, D15.5)
   30 CONTINUE                     
C                        
      CALL RDWT4(NGR,NTMP,NRF,U1R,U2R,IN4)
      IF (NRF.GT.13) GO TO 31  
      LM1=NGR-1
	IF (LM1.LT.1) LM1=1  
      LM2=NTMP-1
	IF (LM2.LT.1) LM2=1 
      DO 23 J=1,LM1     
      DO 22 K=1,LM2     
      DO 21 L=NRF,13    
      U1R(J,K,L+1)=U1R(J,K,L)  
      U2R(J,K,L+1)=U2R(J,K,L)  
   21 CONTINUE   
   22 CONTINUE   
   23 CONTINUE   
   31 IF (NTMP.GT.21) GO TO 32 
      LM1=NGR-1
	IF (LM1.LT.1) LM1=1
      LM2=NTMP-1
	IF (LM2.LT.1) LM2=1
      DO 26 J=1,LM1     
      DO 25 K=LM2 ,21   
      DO 24 L=1,14      
      U1R(J,K+1,L)=U1R(J,K,L)  
      U2R(J,K+1,L)=U2R(J,K,L)  
   24 CONTINUE   
   25 CONTINUE   
   26 CONTINUE   
   32 IF (NGR.GT.9) GO TO 13   
      LM1=NGR-1
	IF (LM1.LT.1) LM1=1
      DO 29 J=LM1,9     
      DO 28 K=1,22      
      DO 27 L=1,14      
      U1R(J+1,K,L)=U1R(J,K,L)  
      U2R(J+1,K,L)=U2R(J,K,L)  
   27 CONTINUE   
   28 CONTINUE   
   29 CONTINUE
C***********************************************
   13 T0(1)=TE(1)
	T0(2)=TE(2)
      WRITE (NOUT1,103)     
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
      WRITE (NOUT1,107) NWL,MDAT,NRF,NGR,NTMP,KY 
      WRITE (NOUT1,110) (WL(K),K=1,NWL)   
C     INPUT PERMANENT FILE DATA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,
     $IN3)
	SEPCM2=RI*RI
C     Note! RDWT2 is required to provide the value of VRD, the 
C     component separation. If the orbital eccentricity differs from 
C     0.0, this quantity varies with orbital longitude.
      CALL RDWT2(NT,NUV,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN9)
      ICOMP=I
      NU=IN6
      CALL DUMMY(2)
      READ (IN6,109,END=399,ERR=299) (RWL(K),K=1,NRF)
      NU=IN7
      CALL DUMMY(2)
      READ (IN7,109,END=399,ERR=299) (RTMP(K),K=1,NTMP)
      NU=IN8
      CALL DUMMY(2)
      READ (IN8,109,END=399,ERR=299) (RGRAV(K),K=1,NGR)
C  
      WRITE (NOUT1,124) I,C1(I),C2(I),C3(I),C4(I)      
      WRITE (NOUT1,124) 3-I,C1(3-I),C2(3-I),C3(3-I),C4(3-I)     
      WRITE (NOUT1,120) I,UC(I),3-I,UC(3-I)      
C
C     Initialize some arrays 
      DO ITH=1,NTH
	N=NPH(I,ITH) 
      DO 14 K=1,N     
      S(I,ITH,K)=0.D0
      S(3-I,ITH,K)=0.D0
	RFL(I,ITH,K)=1.D0
	RFL(3-I,ITH,K)=1.D0      
   14 CONTINUE
      END DO   
      WRITE (NOUT1,111) T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I)     
      WRITE (NOUT1,108) I,UA(I),3-I,UA(3-I)      
      WRITE (NOUT4,119) A(I),A(3-I),BCF(I),BCF(3-I),TE(I),TE(3-I),UC(I),    
     $UC(3-I)
C     Test for case in which the polar temperature of secondary component
C     is set from the polar temperature of the primary component (Case of
C     over-contact system).         
      IF (KY.NE.0) GO TO 17    
      IF (FL.LE.0.0) GO TO 17  
      IF (QS.GT.1.0) GO TO 18  
      T0(3-I)=T0(I)*(G(3-I,1,1)/G(I,1,1))**BCF(I)
      WRITE (NOUT1,118) 3-I,T0(3-I)
      GO TO 17   
   18 T0(I)=T0(3-I)*(G(I,1,1)/G(3-I,1,1))**BCF(3-I)     
      WRITE (NOUT1,118) I,T0(I)    
C     CALCULATE POLAR EFFECTIVE TEMPERATURES OF BOTH COMPONENTS   
   17 DO 1 J=1,2 
		TE(J)=T0(J)      
    1 CONTINUE   
      WRITE (NOUT1,112) TE(I),TE(3-I)     
C     CALCULATE LOCAL TEMPERATURE GRID FOR BOTH COMPONENTS,WITHOUT
C     IRRADIATION EFFECT
      DO ITH=1,NTH 
      N=NPH(I,ITH)     
      DO 2 J=1,N
      TMPR(I,ITH,J)=(T0(I)*(G(I,ITH,J)/G(I,1,1))**BCF(I))     
      TMPR(3-I,ITH,J)=(T0(3-I)*(G(3-I,ITH,J)/G(3-I,1,1))**BCF(3-I))  
    2 CONTINUE
      END DO   
      ITH=NTH/2+1    
      N=NPH(I,ITH)      
      KHF=N/2+1
      KQT=N/4+1   
      V1=TMPR(I,ITH,1)
	V2=TMPR(I,ITH,KQT)
	V3=TMPR(I,ITH,KHF)
      WRITE (NOUT1,114)     
      WRITE (NOUT1,113) I,V1,V2,V3 
      N=NPH(3-I,ITH)    
      KHF=N/2+1
      KQT=N/4+1   
      V1=TMPR(3-I,ITH,1)
	V2=TMPR(3-I,ITH,KQT)
	V3=TMPR(3-I,ITH,KHF)
      WRITE (NOUT1,113) 3-I,V1,V2,V3
	ISV=I      
C     ALLOW FOR IRRADIATION EFFECT
C     For discussion of reflection term, RFL(I<J<K), see
C     Wilson, 1990, ApJ, 356,613.
C***********************************************************************
C     Save current (unirradiated) temperature values
      DO J=1,NTH
		NP=NPH(I,J)
		DO K=1,NP
			TMPRA(I,J,K)=TMPR(I,J,K)
			TMPRA(3-I,J,K)=TMPR(3-I,J,K)
		END DO
	END DO
C     Set designation of irradiating star to L
C     Assume initial designation of star I denotes star
C     at origin.
      IF (I.EQ.ISV) THEN
		L=I
	ELSE
		L=3-I
	END IF
	I=3-L
C     Set proper orientation for irradiated star.
      CALL SHIFT(I,NTH)
	RD1=VR(L,1,1)
C     
	NA=NPH(L,NTH/2+1)
C     RD2 is the radius of the "side" point on the equator.
C     RSV is the mean radius of the irradiating star, component L.
	RD2=VR(L,NTH/2+1,KQT)
	RSV=(RD1+RD2)/2.D0
	WLV=WL(1)
C     Now do iterated irradiation calculation
C
      DO JITER=1,NITER    
      DO 4 J=1,NTH
      NP=NPH(I,J)
      DO 3 K=1,NP
	X=VR(I,J,K)*ALM(I,J,K)
	X=VRD+X
	Y=VR(I,J,K)*AMU(I,J,K)
	Z=VR(I,J,K)*ANU(I,J,K)
C     Calculate element of area at X,Y,Z
      IF ((J.GT.1).AND.(J.LT.NTH)) THEN
		VNTH=NTH-1
		DTH=PI/VNTH
		NA=NPH(I,ITH)
		VN=NA-1
		DOMG=TWPI/VN
		DA=VR(I,ITH,K)*VR(I,ITH,K)*SNT(I,ITH)*SEPCM2*DTH*DOMG
	ELSE
		IF (J.EQ.1) THEN
			DA=VR(I,1,1)*VR(I,1,1)*PI*(1.0D0-ANU(I,NTH-1,1))*SEPCM2
		END IF
		IF (J.EQ.NTH) THEN
			DA=VR(I,NTH,1)*VR(I,NTH,1)*(1.0D0-ANU(I,NTH-1,1))*SEPCM2
		END IF
	END IF
C     CALCULATE FLUX FROM COMPANION AT THAT MESH POINT     
      CALL FIFR(I,J,NTH,K,L,X,Y,Z,FNRM,FIN(I,J,K),RFL,SEPCM2,NOUT5)
C     CORRECT LOCAL TEMPERATURE FOR IRRADIATION
		TMPR(I,J,K)=(((TMPRA(I,J,K)*1.0D3)**4+A(I)*FIN(I,J,K)/
     $	SGMA)**0.25D0)/1.0D3
C     Set reflection term for next iteration
		FLXI=SGMA*(TMPR(I,J,K)*1.D3)**4
		RFL(I,J,K)=1.D0+A(I)*FIN(I,J,K)/FLXI
    3 CONTINUE
    4 CONTINUE
C
	RD1=VR(I,1,1)
C     
	NA=NPH(I,NTH/2+1)
C     RD2 is the radius of the "side" point on the equator.
C     RSV is the mean radius of the irradiating star, component L.
	RD2=VR(I,NTH/2+1,KQT)
	RSV=(RD1+RD2)/2.D0
	DO 6 J=1,NTH
	NP=NPH(L,J)
	DO 5 K=1,NP
C     CALCULATE FLUX FROM COMPANION AT THAT MESH POINT           
	X=VR(L,J,K)*ALM(L,J,K)
	X=X-VRD
	Y=VR(L,J,K)*AMU(L,J,K)
	Z=VR(L,J,K)*ANU(L,J,K)
C     Calculate element of area at X,Y,Z
      IF ((J.GT.1).AND.(J.LT.NTH)) THEN
		VNTH=NTH-1
		DTH=PI/VNTH
		NA=NPH(L,J)
		VN=NA-1
		DOMG=TWPI/VN
		DA=VR(L,J,K)*VR(L,J,K)*SNT(L,J)*SEPCM2*DTH*DOMG
	  ELSE
		IF (J.EQ.1) THEN
			DA=VR(L,1,1)*VR(L,1,1)*PI*(1.0D0-ANU(L,NTH-1,1))*SEPCM2
		END IF
		IF (J.EQ.NTH) THEN
			DA=VR(L,NTH,1)*VR(L,NTH,1)*(1.0D0-ANU(L,NTH-1,1))*SEPCM2
		END IF
	  END IF
      CALL FIFR(L,J,NTH,K,I,X,Y,Z,FNRM,FX,RFL,SEPCM2,NOUT5)
	FIN(L,J,K)=FX
C     CORRECT LOCAL TEMPERATURE FOR IRRADIATION
		TMPR(L,J,K)=(((TMPRA(L,J,K)*1.0D3)**4+A(L)*FIN(L,J,K)/
     $	SGMA)**0.25D0)/1.0D3
C     Set reflection term for this component, for next iteration
		FLXI=SGMA*(TMPR(L,J,K)*1.D3)**4
		RFL(L,J,K)=1.D0+A(L)*FIN(L,J,K)/FLXI
    5 CONTINUE
    6 CONTINUE
      END DO
C     Restore original component identifications
      IF (L.EQ.ISV) THEN
		I=ISV
	ELSE
		I=3-ISV
	END IF
	L=3-I
C     Restore original orientation of irradiated star
      CALL SHIFT(L,NTH)
C***********************************************************************
      I=ISV
      DO J=1,NTH
		NP=NPH(I,J)
		DO K=1,NP
C			WRITE (NOUT5,150) (TMPR(3-I,J,K),K=1,NP)
C			WRITE (NOUT5,151)
		END DO
	END DO   
      DO J=1,NTH
		NP=NPH(I,J)
		DO K=1,NP
C			WRITE (NOUT5,150) (TMPR(I,J,K),K=1,NP)
C			WRITE (NOUT5,151)
		END DO
	END DO   
  150 FORMAT (6F11.7)
  151 FORMAT (1H )
C  
C     IMPOSE AN EMPIRICAL TEMPERATURE VARIATION     
C  
      DO 200 J=2,NTH    
		NP=NPH(I,J)
		C2LM1=C2(I)/180.D0*PI
		C2LM2=C2LM1+PIO2
		IF ((TH(I,J).GE.C2LM1).AND.(TH(I,J).LE.C2LM2)) THEN     
			V1=1.D0
		ELSE 
			V1=0.D0
		END IF
		D=C1(I)*V1
		DO 199 K=1,NP
			E=0.D0
C     Note that ALM(I,K) is sin(theta)*cos(phi).
C     Set term to affect the inner face only.
			IF (ALM(I,J,K).GT.0.D0) THEN
				E=C3(I)*ALM(I,J,K)
			END IF			 
   34			TMPR(I,J,K)=TMPR(I,J,K)+D*T0(I)+E*T0(I)     
  199		CONTINUE
  200 CONTINUE   
      DO 210 J=2,NTH    
		NP=NPH(3-I,J)     
		C2LM1=C2(3-I)/180.D0*PI
		C2LM2=C2LM1+PIO2
		IF ((TH(3-I,J).GE.C2LM1).AND.(TH(I,J).LE.C2LM2)) THEN     
			V1=1.D0
		ELSE 
			V1=0.D0
		END IF
		D=C1(3-I)*V1
		DO 209 K=1,NP
			E=0.D0
			IF (ALM(3-I,J,K).GT.0.D0) THEN
				E=C3(3-I)*ALM(3-I,J,K)
			END IF
   35			TMPR(3-I,J,K)=TMPR(3-I,J,K)+D*T0(3-I)+E*T0(3-I)      
  209		CONTINUE
  210 CONTINUE   
      WRITE (NOUT1,115)     
      V1=TMPR(I,NTH/2+1,1)
      V2=TMPR(I,NTH/2+1,KQT)
      V3=TMPR(I,NTH/2+1,KHF)
      WRITE (NOUT1,113) I,V1,V2,V3 
      V1=TMPR(3-I,NTH/2+1,1)
      V2=TMPR(3-I,NTH/2+1,KQT)
      V3=TMPR(3-I,NTH/2+1,KHF)
      WRITE (NOUT1,113) 3-I,V1,V2,V3      
      WRITE (NOUT1,103)     
      WRITE (NOUT1,116)
C     Locate photospheric points within spot areas and reset local 
C     temperatures to the spot values.
C     ***********
C     Component I
C     ***********
      DO KSPT=1,NSPA
C     Set test value of spot radius
      TEST=DCOS(VGMDA(KSPT))
C     The value of VBTDA(KSPT) is a latitude. We need a colatitude.
      COLAT=PIO2-VBTDA(KSPT)
C     Test whether pole of star is within the spot
      IF (COLAT.LT.VGMDA(KSPT)) GO TO 51
C     *************************************************************
	DO ITH=1,NTH
	THVL=TH(I,ITH)
C     If the photospheric point is outside the upper bound of the spot,
C     skip it
      DIST1=COLAT-VGMDA(KSPT)
      IF (DIST1.GT.0.D0) THEN
		IF (DIST1.GT.THVL) GO TO 91 
	END IF
C     If the photospheric point is below the lower bound of the spot,
C     skip it.
	DIST2=VGMDA(KSPT)+COLAT
	IF (DIST2.GT.0.D0) THEN
		IF (THVL.GT.DIST2) GO TO 91
	END IF
	N=NPH(I,ITH)
	DO K=1,N
	VPHV=PH(I,ITH,K)
C     Calculate cosine OF the angular distance from the spot center to the point
C
C     For geometry and equations, see research journal 5
C
C
      CSDLTA=DCOS(THVL)*DCOS(COLAT)+DSIN(THVL)*DSIN(COLAT)
     $*DCOS(VLMDA(KSPT)-VPHV)
	IF (CSDLTA.LT.TEST) GO TO 90
C     If the angular dist. is less than or equal to the spot angular
C     radius, reset the spot temperature.
	TMPR(I,ITH,K)=DLTAA(KSPT)
   90	END DO
   91 CONTINUE
	END DO
C     ****************************************************************
C     If the program arrives here the pole of the star lies within the spot
C     Determine radius of inscribed circle with pole at center.
   51 RADIN=VGMDA(KSPT)-COLAT
C     Determine radius of circumscribed circle with pole at center.
 	RADOUT=VGMDA(KSPT)+COLAT
      DO ITH=1,NTH
      THVL=TH(I,ITH)
	N=NPH(I,ITH)
C     If the colatitude circle lies within the spot, reset Teff values of
C     all points on that circle.
	IF (THVL.LE.RADIN) THEN
		DO K=1,N
			TMPR(I,ITH,K)=DLTAA(KSPT)
		END DO
	END IF
C     If the colatitude circle lies between RADIN and RADOUT, test spot
C     boundaries to decide whether to reset point Teff
	IF ((THVL.GT.RADIN).AND.(THVL.LE.RADOUT)) THEN
		DO K=1,N
			VPHV=PH(I,ITH,K)
C     Calculate cosine OF the angular distance from the spot center to the point
      CSDLTA=DCOS(THVL)*DCOS(COLAT)+DSIN(THVL)*DSIN(COLAT)
     $*DCOS(VLMDA(KSPT)-VPHV)
C     If the angular distance is less than the spot radius, reset Teff
	        IF (CSDLTA.LT.TEST) GO TO 89
		TMPR(I,ITH,K)=DLTAA(KSPT)
   89 CONTINUE
		END DO
	END IF
      END DO
   	END DO
C		     
C     Locate photospheric points within spot areas and reset local 
C     temperatures to the spot values.
C     *************
C     Component 3-I
C     *************
      DO KSPT=1,NSPB
C     Set test value of spot radius.
      TEST=DCOS(VGMDB(KSPT))
C     The value of VBTDB(KSPT) is a latitude. We need a colatitude.
      COLAT=PIO2-VBTDB(KSPT)
C     Test whether pole of star is within the spot
      IF (COLAT.LT.VGMDB(KSPT)) GO TO 81
C     ***********************************************************
	DO ITH=1,NTH
	THVL=TH(3-I,ITH)
C     If the photospheric pointis outside the upper bound of the spot,
C     skip it.
      DIST1=COLAT-VGMDB(KSPT)
	IF (DIST1.GT.0.D0) THEN
		IF (DIST1.GT.THVL) GO TO 93
	END IF
	DIST2=VGMDB(KSPT)-COLAT
	IF (DIST2.GT.0.D0) THEN
		IF (THVL.GT.DIST2) GO TO 93
	END IF
	N=NPH(3-I,ITH)
	DO K=1,N
	VPHV=PH(3-I,ITH,K)
C     Calculate angular radius of spot along a parallel of latitude.
      CSDLTA=DCOS(THVL)*DCOS(COLAT)+DSIN(THVL)*DSIN(COLAT)
     $*DCOS(VLMDB(KSPT)-VPHV)
      IF (CSDLTA.LT.TEST) GO TO 92
C     If the angular dist. is less than or equal to the spot angular
C     radius, reset the spot temperature.
      TMPR(3-I,ITH,K)=DLTAB(KSPT)
   92	END DO
   93	END DO
C	END DO
C     **************************************************************
C     If the program arrives here the pole of the star lies within the spot
C     Determine radius of inscribed circle with pole at center
   81 RADIN=VGMDB(KSPT)-COLAT
C     Determine radius of circumscribed circle with pole at center.
      RADOUT=VGMDB(KSPT)+COLAT
	DO ITH=1,NTH
	THVL=TH(3-I,ITH)
	N=NPH(3-I,ITH)
C     If the colatitude circle lies within the spot, reset Teff values of
C     all points on that circle.
      IF (THVL.LE.RADIN) THEN
		DO K=1,N
			TMPR(3-I,ITH,K)=DLTAB(KSPT)
		END DO
	END IF
C     If the colatitude circle lies between RADIN and RADOUT, test spot
C     boundaries to decide whether to reset point Teff
      IF ((THVL.GT.RADIN).AND.(THVL.LE.RADOUT)) THEN
		DO K=1,N
			VPHV=PH(3-I,ITH,K)
C     Calculate cosine of the angular distance from the spot center to the point
      CSDLTA=DCOS(THVL)*DCOS(COLAT)+DSIN(THVL)*DSIN(COLAT)
     $*DCOS(VLMDB(KSPT)-VPHV)
C     If the angular distance is less than the spot radius, reset Teff
			IF (CSDLTA.LT.TEST) GO TO 88
			TMPR(3-I,ITH,K)=DLTAB(KSPT)
   88 CONTINUE
		END DO
		END IF
		END DO
	END DO      
C		     
C
      DO 55 J=1,NTH
	WRITE (NOUT1,130) J     
      NP=NPH(I,J)
      WRITE (NOUT1,102) (TMPR(I,J,K),K=1,NP)    
      WRITE (NOUT1,123)     
   55 CONTINUE   
      WRITE(NOUT1,122)      
      DO 56 J=1,NTH     
	WRITE (NOUT1,130) J     
      NP=NPH(I,J)
      WRITE (NOUT1,102) (TMPR(3-I,J,K),K=1,NP)  
      WRITE (NOUT1,123)     
   56 CONTINUE   
C  
C     PRINCIPAL LOOP    
C  
      DO 50 IWL=1,NWL   
      WLV=WL(IWL)
      DO 8 J=1,NTH      
      NP=NPH(I,J)
      DO 7 K=1,NP    
C     DETERMINE LIMB DARKENING COEFFICIENTS  
      TMP=TMPR(I,J,K)
      CALL DKCF(WLV,NGR,NTMP,NRF,TMP,G(I,J,K),U1(I,J,K),U2(I,J,K))
      U1(I,J,K)=UC(I)*U1(I,J,K)    
      U2(I,J,K)=UC(I)*U2(I,J,K)    
C     NOTE THAT THIS CALCULATION USES EFFECTIVE TEMPERATURE 
      ICOMP=I
      IF (MDAT.NE.0) GO TO 15  
      CALL BBDY(TMP,WLV,BLMK)  
      GO TO 16   
   15 CALL MDLAT(WLV,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TMP,G(I,J,K),BLMK)  
C     CALCULATE FLUX AT SPECIFIED WAVELENGTH,IN WAVELENGTH UNITS OF      
C     MICRONS,FOR THE EFFECTIVE TEMPERATURE  
C     DETERMINE OUTWARD INTENSITY,NORMAL TO SURFACE,AT BOUNDARY OF STAR, 
C     IN WAVELENGTH UNITS OF MICRONS
C     Note: BLMK is in intensity units, and is for a wavelength interval
C     of one micron. For a wavelength interval of one Angstrom, 
C     multiply BLMK by 1.E-4. Note in particular that BLMK is the
C     quantity B(lambda) for the black body case.  
   16 BDI(I,J,K)=BLMK
      BDJ(I,J,K)=BLMK
    7 CONTINUE   
    8 CONTINUE
      J=NTH/2+1   
      WRITE (NOUT1,117) I,WLV,BDI(I,1,1),BDI(I,J,1),
     $BDI(I,J,KQT),BDI(I,J,KHF)     
      WRITE (NOUT1,121) U1(I,1,1),U2(I,1,1),U1(I,J,1),U2(I,J,1),
     $U1(I,J,KQT),U2(I,J,KQT),U1(I,J,KHF),U2(I,J,KHF)   
C     CHANGE REFERENCE TO COMPONENT 3-I      
      I=3-I      
      DO 10 J=1,NTH     
      NP=NPH(I,J)
      DO 9 K=1,NP    
C     DETERMINE LIMB DARKENING COEFFICIENTS  
      TMP=TMPR(I,J,K)
      CALL DKCF(WLV,NGR,NTMP,NRF,TMP,G(I,J,K),U1(I,J,K),U2(I,J,K))
      U1(I,J,K)=UC(I)*U1(I,J,K)    
      U2(I,J,K)=UC(I)*U2(I,J,K)
      ICOMP=I    
      IF (MDAT.NE.0) GO TO 19  
      CALL BBDY(TMP,WLV,BLMK)  
      GO TO 20   
   19 CALL MDLAT(WLV,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TMP,G(I,J,K),BLMK)
C     BLMK is an intensity value.     
   20 BDI(I,J,K)=BLMK
      BDJ(I,J,K)=BLMK
    9 CONTINUE   
   10 CONTINUE
      J=NTH/2+1   
      WRITE (NOUT1,117) I,WLV,BDI(I,1,1),BDI(I,J,1),
     $BDI(I,J,KQT),BDI(I,J,KHF)      
      WRITE (NOUT1,121) U1(I,1,1),U2(I,1,1),U1(I,J,1),U2(I,J,1),
     $U1(I,J,KQT),U2(I,J,KQT),U1(I,J,KHF),U2(I,J,KHF)   
C     RESTORE REFERENCE TO COMPONENT I
      I=3-I      
      NT=NTL(I,NTH)     
      NUV=NTL(3-I,NTH)   
      CALL WTOUT7(I,IWL,NTH,NOUT3)
C
C	     
C     LIST DATA FOR EACH COMPONENT
      DO 63 IX=1,2
C     LIST EMERGENT NORMAL INTENSITY VALUES
      WRITE (NOUT1,103)
      WRITE (NOUT1,125) IX,WLV
      DO 59 J=1,NTH
	WRITE (NOUT1,130) J     
      NP=NPH(IX,J)
      WRITE (NOUT1,105) (BDI(IX,J,K),K=1,NP)
      WRITE (NOUT1,123)
   59 CONTINUE
C     LIST LIMB DARKENING VALUES
      WRITE (NOUT1,103)
      WRITE (NOUT1,126) IX,WLV
      DO 60 J=1,NTH
	WRITE (NOUT1,130) J     
      NP=NPH(IX,J)
      WRITE (NOUT1,102) (U1(IX,J,K),K=1,NP)
      WRITE (NOUT1,123)
      WRITE (NOUT1,102) (U2(IX,J,K),K=1,NP)
      WRITE (NOUT1,123)
   60 CONTINUE
C     LIST GRAVITY VALUES
      WRITE (NOUT1,103)
      WRITE (NOUT1,127) IX
      DO 61 J=1,NTH
	WRITE (NOUT1,130) J     
      NP=NPH(IX,J)
	DO K=1,NP
		GLOG(K)=DLOG10(DABS(G(IX,J,K)))
	END DO
      WRITE (NOUT1,131) (GLOG(K),K=1,NP)
      WRITE (NOUT1,123)
   61 CONTINUE
C     LIST INCIDENT FLUX VALUES
      WRITE (NOUT1,103)
      WRITE (NOUT1,128) IX
      DO 62 J=1,NTH
	WRITE (NOUT1,130) J     
      NP=NPH(IX,J)
      DO 610 K=1,NP
      FARY(K)=FIN(IX,J,K)
  610 CONTINUE
      WRITE (NOUT1,105) (FARY(K),K=1,NP)
      WRITE (NOUT1,123)
   62 CONTINUE
C     List reflection term
	WRITE (NOUT1,132) IX
      DO J=1,NTH
	WRITE (NOUT1,130) J     
	NP=NPH(IX,J)
	WRITE (NOUT1,102) (RFL(IX,J,K),K=1,NP)
	WRITE (NOUT1,103)
	END DO
   63 CONTINUE
C
C
   50 CONTINUE
      IF (IRCT.EQ.0) THEN
		DO J=1,2
			DO ITH=1,NTH
				N=NPH(J,ITH)
				DO K=1,N
					FIN(J,ITH,K)=0.D0
				END DO
			END DO
		END DO
	END IF   
      CALL WTOUT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),   
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),   
     $UA(3-I),NOUT2)
   99 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
      WRITE (NOUT1, 205) TM
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=IN3,   STATUS='KEEP')
      CLOSE (UNIT=IN4,   STATUS='KEEP')
      CLOSE (UNIT=IN5,   STATUS='KEEP')
      CLOSE (UNIT=IN6,   STATUS='KEEP')
      CLOSE (UNIT=IN7,   STATUS='KEEP')
      CLOSE (UNIT=IN8,   STATUS='KEEP')
      CLOSE (UNIT=IN9,   STATUS='KEEP')
      CLOSE (UNIT=IN10,  STATUS='KEEP')
      CLOSE (UNIT=IN11,  STATUS='KEEP')
      CLOSE (UNIT=IN12,  STATUS='KEEP')
      CLOSE (UNIT=IN13,  STATUS='KEEP')
      CLOSE (UNIT=IN14,  STATUS='KEEP')
      CLOSE (UNIT=IN15,  STATUS='KEEP')
      CLOSE (UNIT=IN16,  STATUS='KEEP')
      CLOSE (UNIT=IN17,  STATUS='KEEP')
      CLOSE (UNIT=IN18,  STATUS='KEEP')
      CLOSE (UNIT=IN19,  STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      WRITE (IN1, 101) KEYGOOD
      CLOSE (UNIT=IN1,   STATUS='KEEP')
      STOP
  399 CALL PMDSTOP
  299 CALL PMDSTOP
      END  PROGRAM PGC
