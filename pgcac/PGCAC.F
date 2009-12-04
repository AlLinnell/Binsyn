      PROGRAM PGCAC											  
C	USE MSFLIB												 
C     Program for calculating irradiation of a binary star mass donor
C     by an accretion disk, including the irradiation of the donor by
C     the central star.
C
C     GENERAL NOTE: In this calculation, the star at the center of the 
C     accretion disk, and the	accretion disk, have their geometric
C     origins at the center of the star. The irradiated star has its
C     center at X,Y,Z coordinates VRD,0,0. It is unnecessary to
C     consider center-of-mass coordinates because the effects of
C     irradiation depend only on the separation between the components.
C     The coordinates selected here make it easy to treat the mass
C     gainer and the accretion disk.
C     For a circular orbit, VRD=1.0, but the program, at least formally,
C     can treat the case of an eccentric orbit. The value of VRD is
C     input from program PGB.
C
C     NOTE: The output files PGCAC2.DAT and PGCAC3.DAT must be copied
C     to PGCX2.DAT and PGCX3.DAT after this program has completed.
C
	INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
	INCLUDE 'COMM.FOR'
	INCLUDE 'COMR.FOR'
	INCLUDE 'COMN.FOR'
	INCLUDE 'COMPH.FOR'
	DIMENSION FL1(19)
	DIMENSION WL(MWL),
     $UC(MTW),C1(MTW),C2(MTW),T0(MTW),A(MTW),BCF(MTW),UA(MTW)
	DIMENSION TMPRFA(MRING,MSG),SMV(MRING,MSG),RMIFLX(MRM,MSG),
     $TMPRF(MRING,MSG),TMPRR(MRM,MSG),VC(MRM)
	DIMENSION SMB(MPHM),VXI(MPHM),AV(MTW)
C
C
	COMMON /NUNIT/ NU,NUOUT,IFOUT
	CHARACTER * 7 LABEL
	CHARACTER * 36 FL1
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 4 RESULT
C
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
	DATA IN10,IN11,IN12,IN13,IN14,IN15/10,11,12,13,14,15/
	DATA NOUT1,NOUT2,NOUT3/16,17,18/
  100 FORMAT (A36)
  103 FORMAT (1H1)
  104 FORMAT (1X,'Program PGCAC',25X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/,13X,
     $'**OUT**',/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'*')
  105 FORMAT (8F10.7)
  106 FORMAT (10I5)
  107 FORMAT (' ')
  108 FORMAT (1X,'Control keys:',/,1X,'Key1=',I5,5X,'Key2=',I5,5X,
     $'Key3=',I5,5X,'MDAT=',I5)
  109 FORMAT (1X,'Pole effective temperature, comp.      ',I1,5X,
     $'=',F10.5,/,29X,
     $'point temperature=',F10.5,/,29X,
     $'side temperature =',F10.5,/,29X,'back temperature =',F10.5)
  110 FORMAT (1X,'Photospheric temperature array, component',I5)
  111 FORMAT (10F8.4)
  112 FORMAT (10X,'ITH=',I5)
  113 FORMAT (1X,'Wavelength number',40X,I5)
  205 FORMAT (1X,'Program running time=',F12.4,2X,'minutes')
C
C      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C     Avoid warnings on assembly
      SGTMZ=SGTM
	AMUMSZ=AMUMS
	VLIGHTZ=VLIGHT
	SGMAZ=SGMA
	COEFZ=COEF
	YRSECZ=YRSEC
	SOLZ=SOL
	RADZ=RAD
	TPIO2Z=TPIO2
	PIO2Z=PIO2
	GRAVZ=GRAV
	SOLMSZ=SOLMS
	BOLTZZ=BOLTZ

C**********************************************************************
      OPEN (UNIT=20,  FILE='C:\LGTSYN\PGCAC\PGCAC.ILS',STATUS='OLD')
	READ (20, 100)     (FL1(J),J=1,19)
	OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS = 'OLD')
	OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS = 'OLD')
	OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS = 'OLD')
	OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS = 'OLD')
	OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS = 'OLD')
	OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS = 'OLD')
	OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS = 'OLD')
	OPEN (UNIT=IN8,   FILE=FL1(8),  STATUS = 'OLD')
	OPEN (UNIT=IN9,   FILE=FL1(9),  STATUS = 'OLD')
	OPEN (UNIT=IN10,  FILE=FL1(10), STATUS = 'OLD')
	OPEN (UNIT=IN11,  FILE=FL1(11), STATUS = 'OLD')
	OPEN (UNIT=IN12,  FILE=FL1(12), STATUS = 'OLD')
	OPEN (UNIT=IN13,  FILE=FL1(13), STATUS = 'OLD')
	OPEN (UNIT=IN14,  FILE=FL1(14), STATUS = 'OLD')
	OPEN (UNIT=IN15,  FILE=FL1(15), STATUS = 'OLD')
	OPEN (UNIT=NOUT1, FILE=FL1(16), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT2, FILE=FL1(17), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT3, FILE=FL1(18), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT4, FILE=FL1(19), STATUS = 'REPLACE')
C**********************************************************************
C      CALL GETDAT(IYR,IMON,IDAY)
C	CALL GETTIM(IHR,IMIN,ISEC,I100TH)
	TM=3600*IHR+60*IMIN+ISEC
	AD=I100TH
	TM=(100.D0*TM+AD)/100.D0
	AD=IDAY
	DA=AD+TM/86400.D0
	CALL CVJD(DA,IMON,IYR,FVL1)
	WRITE (NOUT1,103)
	WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC
C     Input control information.
      LBLERR=0
	LABEL = '**PGCAC'
	NU=IN1
	CALL GTLBL(LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL DREAD (AV(1),'F12.6')
	CALL DREAD (AV(2),'F12.6')
	CALL DUMMY(1)
	CALL IREAD (MDAT)  ! Key to control use of model atmospheres.
	CALL DUMMY(1)
	CALL IREAD (KEY1)  ! Key to determine application of rim irrad.
	CALL DUMMY(1)
	CALL IREAD (KEY2)  ! Key to determine applic. of face irrad.
	CALL DUMMY(1)
	CALL IREAD (KEY3)  ! Key to determine applic. of irrad. by gainer
C
C     Input data from PGA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN10)
C     Input data from PGB to get value of VRD, the separation of 
C     components.	
C     NOTE! This procedure will need to be expanded if there is orbital
C     eccentricity in an actual system!!!
C
C     Input data from PGB
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN5)
C     Input data from ACPGA.
C     This includes full data concerning the accretion disk itself.
C     The coordinates of the rim and face points are relative to the
C     center of the accretion disk.
      CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,AD,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN2)
C     We wish to work with the upper accretion disk face. By symmetry,
C     the irradiative effects produced by the lower accretion disk face
C     will be the same on the lower hemisphere of the donor.
C     VZAF() is the Z cood on the accretion disk face, relative to the
C     orbital plane.
C
C     If there is no accretion disk, exit
C
      IF ((NRM.EQ.0).OR.(NRING.EQ.0)) GO TO 99
C
      DO KK=1,NRING
		DO J=1,NSG
			VZAF(KK,J)=-VZAF(KK,J)
		END DO
	END DO
C

C     Get value of ISV.
C     ISV is the designation of the star eclipsed at pri. min.
C     IEC is the designation of the star currently closer to
C     the observer than the companion.
      CALL RDWT8(KSPP,IEC,ISV,IRC,IFC,NKP,IRCS,IFCS,NKPS,
     $AGLMT1,AGLMT2,XG,YG,XCM,YCM,ISEQ,IN7)
C
C     Read ACPGA.INF to determine the star with the accretion disk, and 
C     rim height in solar radii
      LBLERR=0
	LABEL = ' *ACPGA'
	NU=IN11
	CALL GTLBL(LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     ISTAR designates the star with the accretion disk.
	CALL IREAD (ISTAR)
	CALL DUMMY(11)
C     HVP is the outer rim thickness in solar radii
	CALL DREAD (HVP,'F12.6')
	CALL DUMMY(3)
C     NTYPE is the type rim boundary.
C     If NTYPE=1, it is cylindrical.
C     If NTYPE=2, it is toroidal
	CALL IREAD (NTYPE)
C
C     Determine scale factor, component separation in cm.
      SEPCM=HVP/HV*SNRAD
	SEPCM2=SEPCM**2
	SEPCM3=SEPCM**3
C     Extract the theta coordinate of the top of the accretion disk 
C     rim, in THR(1).
      VTH=THR(1)
	SNTH=DSIN(VTH)
	COSTH=DCOS(VTH)
C     Calculate the ratio of the vertical height of an accretion disk
C     annulus to its inner radius. 
	COT=COSTH/SNTH
C
C     Initialize SMB()
      DO ITH=1,NTH
		SMB(ITH)=0.D0
	END DO
C
C     Input data from PGC
      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),
     $UA(3-I),IN8)     
C     Input intensity data for both stars.
	CALL RDWT6(I,IWL,NTH,IN9)
C***********************
C     Input temperature, flux, and limb darkening data for accretion 
C     disk face.
      CALL ACRD2(NWL,NSG,NRM,NRING,IRCT,WLV,
     $TMPRF,TMPRFA,SMV,TMPRR,RMIFLX,IN13)
C     We must expand out the arrays for the star at the center of the 
C     accretion disk to the full arrays covering the complete 
C     photosphere.
C     Identify the proper designation for that star.
C     Irrespective of whether L=I or L=3-I, the expanded arrays set
C     the meridian which passes through the point on the gainer on the
C     X=1.0 side of the center of the star. Similarly, the starting
C     point for measuring the azimuth of any bright spot on the rim is
C     on the half line directed from the origin to X=1.0.
      IF (I.EQ.ISTAR) THEN
		L=I
	ELSE
		L=3-I
	END IF
C
      NTS=0
C     ********************************************************
C
C     Now prepare to do the irradiation calculation.
C     If I.eq.ISTAR at this stage, then the value of I must be replaced
C     by 3-I to select the donor.
      ITST=I
      IF (I.EQ.ISTAR) I=3-I
C     Must reverse the direction for the positive X axis, since we are 
C     displacing the center of the donor to +VRD, and the positive
C     X axis at the center of the gainer points toward x=1.0.
C     Do this with SRT SHIFT
      CALL SHIFT(I,NTH)
C     Now proceed with the setup of points on the secondary.
C     Note: X,Y,Z designates a point on the donor, which is the irradiated 
C     star.
      DO ITH=1,NTH
		N=NPH(I,ITH)
		DO K=1,N
			X=VR(I,ITH,K)*ALM(I,ITH,K)
			Y=VR(I,ITH,K)*AMU(I,ITH,K)
			Z=VR(I,ITH,K)*ANU(I,ITH,K)
C     Calculate element of photospheric area associated with the point
C     in question. Note that, for the cap region at the pole, we 
C     integrate over the angular radius of the cap.
			IF (ITH.EQ.1) THEN
				DLA=TWPI*VR(I,ITH,K)**2*(1.D0-ANU(I,ITH+1,K))
			END IF
			IF (ITH.EQ.NTH) THEN
				DLA=TWPI*VR(I,ITH,K)**2*(1.D0-ANU(I,ITH-1,K))
			END IF
			IF ((ITH.NE.1).AND.(ITH.NE.NTH)) THEN
				VTH1=TH(I,ITH)
				VTH2=TH(I,ITH+1)
				IF (K.LT.N) THEN
					DPHL=PH(I,ITH,K+1)-PH(I,ITH,K)
				END IF
				IF (K.EQ.N) THEN
					DPHL=PH(I,ITH,K)-PH(I,ITH,K-1)
				END IF
				DLA=VR(I,ITH,K)**2*SNT(I,ITH)*DPHL*(VTH2-VTH1)
			END IF
C     Express element of area in cm^2
			DLA=DLA*SEPCM2
C     The mass gainer and the accretion disk have their centers at the 
C     origin of coordinates. For calculation of irradiation, displace
C     the mass loser to VRD.
			X=VRD+X
C     Initialize flux sums from rim, face, and central star.
			RMFX=0.D0
			FCFX=0.D0
			CSTFX=0.D0
C     Initialize total irradiative flux.
			TOTFX=0.D0
C     **********************************************
C 	Beginning of irradiation calculation.
C     **********************************************
C     
C     Determine whether to apply irradiation by accretion disk rim.
      IF (KEY1.NE.1) GO TO 30
C
C     Given X,Y,Z, a point on the photosphere of the loser, calculate 
C     flux at that point from points on accretion disk rim.
C
			DO KK=1,NRM-1
				DO J=1,NSG-1
C     Calculate separation of X,Y,Z and the current point on the rim.
					VRV=DSQRT((X-VXAR(KK,J))**2+(Y-VYAR(KK,J))**2+
     $					(Z-VZAR(KK,J))**2)
C     Calculate direction cosines of vector toward X,Y,Z.
					VLDC=(X-VXAR(KK,J))/VRV
					VMDC=(Y-VYAR(KK,J))/VRV
					VNDC=(Z-VZAR(KK,J))/VRV
C     Calculate cosine of angle between vector and local normal to
C     photosphere point, at the point X,Y,Z. Note that the direction 
C     cosines of VLDC must be reversed so that we have a vector
C     pointed toward the rim segment.
					CSNR=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-
     $							VNDC*ZN(I,ITH,K)
C     Only consider illumination if X,Y,Z is visible from rim point.
					IF (CSNR.GE.0.D0) THEN
C     Calculate cosine of angle between the vector and the local normal
C     to the rim.
					CSVEC=VLDC*ZLAR(KK,J)+VMDC*ZMAR(KK,J)+
     $						VNDC*ZNAR(KK,J)
C     Initialize incremental flux contributions from rim segment.
					DRMFX=0.D0

C     Only consider a point if CSVEC.GE.0.D0
					IF (CSVEC.GE.0.D0) THEN
C     Calculate projected area of rim area segment, projected on plane
C     perpendicular to vector just produced.
						DPH=PHIR(J+1)-PHIR(J)
C     If NTYPE=1, the rim boundary is cylindrical.
C     If NTYPE=2, the rim boundary is toroidal.
C     For a toroidal rim boundary,
C     RMR(KK) is the radial distance from the origin to the rim point.
C     SNTR(KK) is the sine of an angle nearly equal to the polar angle
C     of the line RMR(KK). See the development in the notebook 
C     "Accretion Disk and Related Plots".
						IF (NTYPE.EQ.2) THEN
							DPHL=DPH*RMR(KK)*SNTR(KK)
						END IF
						IF (NTYPE.EQ.1) THEN
							DPHL=DPH*RA
						END IF
						DTH=THR(KK+1)-THR(KK)
C     HV is the rim height.
						IF (NTYPE.EQ.1) THEN 
							DTHL=DTH*HV
						END IF
						IF (NTYPE.EQ.2) THEN
							DTHL=RA*(1.D0/DTAN(THR(KK))-
     $								1.D0/DTAN(THR(KK+1)))
						END IF
						DA=DPHL*DTHL*SEPCM2
						DAPJ=DA*CSVEC
C     Determine normal intensity for isotropic radiation.
C     The temperatures stored for the rim are Teff values.
						FM=SGMA*TMPRR(KK,J)**4
						F=FM*CF/(6.D0-2.D0*U1RM(KK,J)-3.D0*U2RM(KK,J))
C     Calculate light intensity toward X,Y,Z. (Specific intensity times
C     projected area), in erg/s/steradian
						DI=F*(1.D0-U1RM(KK,J)*(1.D0-CSVEC)
     $						-U2RM(KK,J)*(1.D0-CSVEC)**2)*DAPJ
C     Calculate flux contribution for that rim element.
C     Note that radiation from rim element is into only a hemisphere,
C     so division is by two pi, not 4*pi.
							DRMFX=DI/VRV**2/SEPCM2*CSNR
						END IF
					END IF
C     Accumulate rim flux contributions.
					RMFX=RMFX+DRMFX
				END DO
			END DO
C     End of flux calculation for illumination of photosphere segment 
C     at X,Y,Z by accretion disk rim.
C     *******************************************
C
C     Determine whether to apply irradiation by accretion disk face.
   30 IF (KEY2.NE.1) GO TO 40
C     Now calculate illumination of photosphere segment by the accretion
C     disk face.
			DO KK=1,NRING-1
				DO J=1,NSG-1
C     Calculate the separation of the points.
					IF (ITH.LE.NTH/2+1) THEN
					VRV=DSQRT((X-VXAF(KK,J))**2+(Y-VYAF(KK,J))**2
     $					+(Z-VZAF(KK,J))**2)
					ELSE
					VRV=DSQRT((X-VXAF(KK,J))**2+(Y-VYAF(KK,J))**2
     $					+(Z+VZAF(KK,J))**2)
					END IF
C     Calculate the direction cosines of the vector toward X,Y,Z.
					VLDC=(X-VXAF(KK,J))/VRV
					VMDC=(Y-VYAF(KK,J))/VRV
					IF (ITH.LE.NTH/2+1) THEN
					VNDC=(Z-VZAF(KK,J))/VRV
					ELSE
					VNDC=(Z+VZAF(KK,J))/VRV
					END IF
C     Calculate the cosine of the angle between the vector and the 
C     local normal to the photosphere at X,Y,Z.
					CSNR=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-
     $						VNDC*ZN(I,ITH,K)
C     Only consider illumination if X,Y,Z is visible from the face 
C     point.
					IF (CSNR.GE.0.D0) THEN
C     Calculate the cosine of the angle between the vector and the local
C     normal to the accretion disk face.
					IF (ITH.LE.NTH/2+1) THEN
					CSVEC=VLDC*ZLAF(KK)+VMDC*ZMAF(KK)+VNDC*ZNAF(KK)
					ELSE
					CSVEC=VLDC*ZLAF(KK)+VMDC*ZMAF(KK)-VNDC*ZNAF(KK)
					END IF
C     Note that this calculation is for one face of the accretion disk 
C     only. By symmetry, the irradiation is the same for the region below the
C     orbital plane and the other face of the accretion disk.
C     Initialize incremental flux contribution from face segment.
					DFCFX=0.D0
C     Only consider the point being examined if CSVEC.GE.0.D0.
					IF (CSVEC.GE.0.D0) THEN
C     Determine whether the rim blocks this vector.
C     Accomplish this by comparing the Z cood. of the point on the 
C     vector which is at a distance from the origin equal to the rim
C     radius and the rim height itself. If the Z cood. is less than the
C     rim height, the face point is blocked by the rim, as seen from
C     point X,Y,Z.
C
					IF (ITH.LE.NTH/2+1) THEN
C     Initialize starting point on vector.
						XCURR=VXAF(KK,J)
						YCURR=VYAF(KK,J)
						ZCURR=VZAF(KK,J)
C     Choose an initial step length along vector.
						DISTC=DSQRT((XCURR-X)**2+(YCURR-Y)**2+
     $							(ZCURR-Z)**2)
						DS=DISTC/16.D0
						IF (DS.LT.0.0001D0) DS=0.0001D0
C     Take a step along vector.
    1						XCURR=XCURR+DS*VLDC
						YCURR=YCURR+DS*VMDC
						ZCURR=ZCURR+DS*VNDC
C     Determine distance of current point from center of accretion disk,
C     in the orbital plane.
						DISTC=DSQRT(XCURR**2+YCURR**2)
C     If distance is greater than the rim radius, step back, divide the
C     step length, and start stepping outward again.
						IF (DISTC-RGRD(NRING).GT.0.D0) THEN
							XCURR=XCURR-DS*VLDC
							YCURR=YCURR-DS*VMDC
							ZCURR=ZCURR-DS*VNDC
							DS=DS/2.D0
							IF (DS.GT.0.0001D0) GO TO 1
							GO TO 2
						ELSE
							IF (DS.GT.0.0001D0) GO TO 1
							GO TO 2
						END IF
C     Test whether the vector misses the rim.
    2						DELZ=HV-ZCURR
						GO TO 203
					ELSE
C     Initialize starting point on vector.
						XCURR=VXAF(KK,J)
						YCURR=VYAF(KK,J)
						ZCURR=-VZAF(KK,J)
C     Choose an initial step length along vector.
						DISTC=DSQRT((XCURR-X)**2+(YCURR-Y)**2+
     $							(ZCURR-Z)**2)
						DS=DISTC/16.D0
						IF (DS.LT.0.0001D0) DS=0.0001D0
C     Take a step along vector.
  101						XCURR=XCURR+DS*VLDC
						YCURR=YCURR+DS*VMDC
						ZCURR=ZCURR+DS*VNDC
C     Determine distance of current point from center of accretion disk,
C     in the orbital plane.
						DISTC=DSQRT(XCURR**2+YCURR**2)
C     If distance is greater than the rim radius, step back, divide the
C     step length, and start stepping outward again.
						IF (DISTC-RGRD(NRING).GT.0.D0) THEN
							XCURR=XCURR-DS*VLDC
							YCURR=YCURR-DS*VMDC
							ZCURR=ZCURR-DS*VNDC
							DS=DS/2.D0
							IF (DS.GT.0.0001D0) GO TO 101
							GO TO 102
						ELSE
							IF (DS.GT.0.0001D0) GO TO 101
							GO TO 102
						END IF
C     Test whether the vector misses the rim.
  102						DELZ=HV+ZCURR
					END IF
C     If DELZ is negative, the vector clears the rim.
C     In this case, continue with the calculation of irradiation.
  203						IF (DELZ.GE.0.D0) GO TO 3     
C     Calculate the projected area for the face segment.
						DPH=PHIR(J+1)-PHIR(J)
C     Use an average radius for the face segment.
						DPHL=DPH*(RGRD(KK+1)+RGRD(KK))/2.D0
						DRL=RGRD(KK+1)-RGRD(KK)
						DA=DPHL*DRL*SEPCM2
						DAPJ=DA*CSVEC
C     Calculate the normal intensity for isotropic radiation.
						FM=SGMA*TMPRF(KK,J)**4
						F=FM*CF/(6.D0-2.D0*U1F(KK,J)-3.D0*U2F(KK,J))
C     Calculate the light intensity toward X,Y,Z.
						DI=F*(1.D0-U1F(KK,J)*(1.D0-CSVEC)
     $					-U2F(KK,J)*(1.D0-CSVEC)**2)*DAPJ
C     Calculate face flux contributions.
C     See the discussion of the rim flux for further details.
							DFCFX=DI/VRV**2/SEPCM2*CSNR
    3						CONTINUE
					END IF
					END IF
C     Accumulate face flux contributions.
					FCFX=FCFX+DFCFX
				END DO
			END DO
C     End of flux calculation for illumination of photosphere segment
C     at X,Y,Z by accretion disk face.
C     ****************************************
C
C     Determine whether to apply irradiation by mass gainer.
   40 IF (KEY3.NE.1) GO TO 51
C     Now calculate illumination of photosphere segment by the component
C     at the center of the accretion disk. This component is L.
C     Follow the procedure in program PGC, SRT FIFR.
C     The star at the center of the accretion disk may be rotationally 
C     distorted. 
C     Calculate the total irradiative flux received at the photospheric
C     segment centered at point X,Y,Z on the irradiated star.
C     Loop over the irradiated photosphere.Note that we consider the
C     complete photosphere of the irradiating star, component L.
C
C     *****************************************************************
C     Start with the upper polar cap.
C	
C     Note in the next section that we
C     have integrated over sine(theta). That is, we consider the polar
C     cap to be a small irradiating segment. The area of a polar cap
C     segment is 	TWPI*r^2*(1-cos(theta))*SEPCM2, where theta is the
C     polar angle of the cap edge.
C     XA, YA, ZA are the coordinates of a point on the photosphere of
C     the irradiating star.
C     Initialize SMB(1)
		SMB(1)=0.D0
		XA=VR(L,1,1)*ALM(L,1,1)
		YA=VR(L,1,1)*AMU(L,1,1)
		ZA=VR(L,1,1)*ANU(L,1,1)
C     The coordinates of the point on the irradiating star are XA,YA,ZA
C     RCP is the distance between the irradiating segment and the
C     irradiated segment. VLDC, VMDC, VNDC are the direction cosines of
C     a unit vector at the irradiating segment, directed toward the
C     irradiated segment.
		RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
		VLDC=(X-XA)/RCP
		VMDC=(Y-YA)/RCP
		VNDC=(Z-ZA)/RCP
C     Calculate the cosine of the angle
C     between the vector toward XA,YA,ZA and the local normal at X,Y,Z.
		CSX=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-VNDC*ZN(I,ITH,K)
		IF (CSX.LT.0.D0) GO TO 48
C     Calculate the cosine of the angle between this vector and the
C     local normal at the polar cap of the irradiating star.
		CSNR=VLDC*ZL(L,1,1)+VMDC*ZM(L,1,1)+VNDC*ZN(L,1,1)
C     Only continue if CSNR is not negative.
		IF (CSNR.LT.0.D0) GO TO 48
C     Determine whether the rim blocks this vector.
C     The procedure is the same as for the accretion disk face.
C     Initialize starting point on vector.
				XCURR=XA
				YCURR=YA
				ZCURR=ZA
C     Choose an initial length along vector.
				DISTC=DSQRT((XCURR-X)**2+(YCURR-Y)**2+(ZCURR-Z)**2)
				DS=DISTC/16.D0
				IF (DS.LT.0.0001D0) DS=0.0001D0
C     Take a step along vector.
   21				XCURR=XCURR+DS*VLDC
				YCURR=YCURR+DS*VMDC
				ZCURR=ZCURR+DS*VNDC
C     Determine distance of current point from center of accretion disk,
C     in the orbital plane.
				DIST=DSQRT(XCURR**2+YCURR**2)
C     If distance is greater than the rim radius, step back, divide the
C     step length, and start stepping out again.
				IF (DIST-RGRD(NRING).GT.0.D0) THEN
					XCURR=XCURR-DS*VLDC
					YCURR=YCURR-DS*VMDC
					ZCURR=ZCURR-DS*VNDC
					DS=DS/2.D0
					IF (DS.GT.0.0001D0) GO TO 21
					GO TO 22
				ELSE
					IF (DS.GT.0.0001D0) GO TO 21
					GO TO 22
				END IF
C     Test whether the vector misses the rim.
   22				DELZ=HV-ZCURR
C     If DELZ is negative, the vector clears the rim.
				IF (DELZ.GE.0.D0) GO TO 48
C     The vector clears the rim. 
C     There is an irradiation contribution only if CSX is not negative.
			CSXA=VLDC*ZL(L,1,1)+VMDC*ZM(L,1,1)+VNDC*ZN(L,1,1)
C     DAP is the area of the cap, projected normal to the connecting 
C     vector, in cm^2. 
			DAP=VR(L,1,1)*VR(L,1,1)*TWPI*(1.D0-ANU(L,2,1))*CSXA*SEPCM2
			IF (DAP.LT.0.D0) DAP=0.D0
			F1=SGMA*(TMPR(L,1,1)*1.0D3)**4
			F=F1*CF/(6.D0-2.D0*U1(L,1,1)-3.D0*U2(L,1,1))
			SMB(1)=F*DAP*(1.D0-U1(L,1,1)*(1.D0-CSXA)-
     $			U2(L,1,1)*(1.D0-CSXA)**2)/RCP**2/SEPCM2*CSX
C
C     *****************************************************************
C     Now do lower polar cap.
C     Initialize SMB(MTH)
   48		SMB(NTH)=0.D0
		XA=VR(L,NTH,1)*ALM(L,NTH,1)
		YA=VR(L,NTH,1)*AMU(L,NTH,1)
		ZA=VR(L,NTH,1)*ANU(L,NTH,1)
		RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
		VLDC=(X-XA)/RCP
		VMDC=(Y-YA)/RCP
		VNDC=(Z-ZA)/RCP
C     Calculate cosine of angle between vector toward XA, YA, ZA at
C     X,Y,Z and local normal at X,Y,Z.
		CSX=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-VNDC*ZN(I,ITH,K)
		IF (CSX.LT.0.D0) GO TO 49
C     Calculate the cosine of the angle between the vector toward X,Y,Z
C     and the local normal.
		CSNR=VLDC*ZL(L,NTH,1)+VMDC*ZM(L,NTH,1)+VNDC*ZN(L,NTH,1)
C     Only continue if CSNR is not negative.
		IF (CSNR.LT.0.D0) GO TO 49
C     Determine whether the rim blocks this vector.
C     The procedure is the same as for the accretion disk face.
C     Initialize starting point on vector.
				XCURR=XA
				YCURR=YA
				ZCURR=ZA
				ZCURRL=ZA
C     Choose an initial length along vector.
				DISTC=DSQRT((XCURR-X)**2+(YCURR-Y)**2+(ZCURR-Z)**2)
				DS=DISTC/16.D0
				IF (DS.LT.0.0001D0) DS=0.0001D0
C     Take a step along vector.
   31				XCURR=XCURR+DS*VLDC
				YCURR=YCURR+DS*VMDC
				ZCURR=ZCURR+DS*VNDC
C     Test whether the vector has crossed the orbital plane.
C     If it has, and the distance from the center is within the
C     opaque part of the accretion disk, exit.
				IF ((ZCURR.GE.0.D0).AND.(ZCURRL.LT.0.D0)) THEN
					DISTC=DSQRT(XCURR**2+YCURR**2)
					IF (DISTC.GT.RGRD(1))GO TO 49
				ELSE
					ZCURRL=ZCURR
				END IF
C     Determine distance of current point from center of accretion disk,
C     in the orbital plane.
				DIST=DSQRT(XCURR**2+YCURR**2)
C     If distance is greater than the rim radius, step back, divide the
C     step length, and start stepping out again.
				IF (DIST-RGRD(NRING).GT.0.D0) THEN
					XCURR=XCURR-DS*VLDC
					YCURR=YCURR-DS*VMDC
					ZCURR=ZCURR-DS*VNDC
					DS=DS/2.D0
					IF (DS.GT.0.0001D0) GO TO 31
					GO TO 32
				ELSE
					IF (DS.GT.0.0001D0) GO TO 31
					GO TO 32
				END IF
C     Test whether the vector misses the rim.
   32				DELZ=HV+ZCURR
C     If DELZ is negative, the vector clears the rim.
				IF (DELZ.GE.0.D0) GO TO 49
			CSXA=VLDC*ZL(L,NTH,1)+VMDC*ZM(L,NTH,1)+VNDC*ZN(L,NTH,1)
			DAP=VR(L,NTH,1)*VR(L,NTH,1)*TWPI*(1.D0-ANU(L,NTH,1))
     $			*CSXA*SEPCM2
			IF (DAP.LT.0.D0) DAP=0.D0
			F1=SGMA*(TMPR(L,NTH,1)*1.0D3)**4
			F=F1*CF/(6.D0-2.D0*U1(L,NTH,1)-3.D0*U2(L,NTH,1))
			SMB(NTH)=F*DAP*(1.D0-U1(L,NTH,1)-U2(L,NTH,1)+U1(L,NTH,1)
     $			*CSXA+U2(L,NTH,1)*CSXA**2)/RCP**2/SEPCM2*CSX
C
C     *****************************************************************
C     Do the remainder of star L.
C
   49		VNTH=NTH-1
		DTH=PI/VNTH
C     Initialize SMB
		DO ITHA=2,NTH-1
			SMB(ITHA)=0.D0
		END DO
C     Loop over remainder of star L for the point X,Y,Z
		DO ITHA=2,NTH-1
			NA=NPH(L,ITHA)
			VN=NA-1
C     Calculate the phi increment, DOMG, in radians.
			DOMG=TWPI/VN
C     Initialize VXI
			DO KA=1,NA
				VXI(KA)=0.D0
			END DO
			DO KA=1,NA
				VPHP=PH(L,ITHA,KA)
C     Determine the coordinates of the photospheric segment on star L.
				XA=VR(L,ITHA,KA)*ALM(L,ITHA,KA)
				YA=VR(L,ITHA,KA)*AMU(L,ITHA,KA)
				ZA=VR(L,ITHA,KA)*ANU(L,ITHA,KA)
C     Calculate the separation of the irradiated point and the 
C     irradiating segment.
				RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
C     Calculate direction cosines of vector directed from (XA,YA,ZA)
C     to (X,Y,Z).
				VLDC=(X-XA)/RCP
				VMDC=(Y-YA)/RCP
				VNDC=(Z-ZA)/RCP
C     Calculate the cosine of the angle between the vector, directed
C     toward XA,YA,ZA, and the surface normal at (X,Y,Z).
			   CSX=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-VNDC*ZN(I,ITH,K)
C     If CSX.LT 0.D0, then no irradiation of X,Y,Z is possible for that
C     XA,YA,ZA.
			   IF (CSX.LT.0.D0) GO TO 50 
C     Calculate cosine of angle between vector toward X,Y,Z and the
C     local normal to point XA,YA,ZA.
				CSNR=VLDC*ZL(L,ITHA,KA)+VMDC*ZM(L,ITHA,KA)+VNDC*
     $				ZN(L,ITHA,KA)
				IF (CSNR.LT.0.D0) GO TO 50
C     Determine whether the rim blocks this vector.
C     In this process, determine whether the vector passes through the
C     orbital plane inside the innermost ring of the accretion disk.
C     If yes, permit the vector to proceed.
C     The procedure is similar to that of the accretion disk face.
C     Initialize starting point on vector.
				XCURR=XA
				YCURR=YA
				ZCURR=ZA
C     Note: ZCURRL will be the Z value on the vector at the last previous
C     step.
				ZCURRL=ZA
C     Choose an initial length along vector equal to 1/16 of the distance
C     from the current point on the central star to the irradiated point.
				DISTC=DSQRT((XCURR-X)**2+(YCURR-Y)**2+(ZCURR-Z)**2)
				DS=DISTC/16.D0
				IF (DS.LT.0.0001D0) DS=0.0001D0
C     Take a step along vector.
   11				XCURR=XCURR+DS*VLDC
				YCURR=YCURR+DS*VMDC
				ZCURR=ZCURR+DS*VNDC
C     Test whether the vector has crossed the orbital plane.
C     If it has, and the distance from the center is within the
C     opaque part of the accretion disk, exit.
C     Note that DISTC is the distance of the curent point from the center
C     of the irradiating star, measured in the orbital plane.
C     The following sequence is for the case where the vector starts at a
C     point below the orbital plane on star L and crosses the orbital
C     plane.
				IF ((ZCURR.GE.1.0D-12).AND.(ZCURRL.LT.-1.0D-12)) THEN
					DISTC=DSQRT(XCURR**2+YCURR**2)
C     If the vector crosses the orbital plane inside RGRD(1), continue
C     normally
					IF ((DISTC-RGRD(1)).LT.0.D0) THEN
						DELZ=-1.D0
						GO TO 13
					END IF
C     If the vector crosses the orbital plane between RGRD(1) and RGRD(NRING),
C     irradiation is blocked for this vector.
					IF (((DISTC-RGRD(1)).GT.0.D0).AND.
     $					((DISTC-RGRD(NRING)).LT.0.D0))
     $				GO TO 50
				END IF
C     The following sequence is for the case where the vector starts at a
C     point above the orbital plane on star L and crosses the orbital plane.
				IF ((ZCURR.LT.-1.0D-12).AND.(ZCURRL.GT.1.0D-12)) THEN
					DISTC=DSQRT(XCURR**2+YCURR**2)
					IF ((DISTC-RGRD(1)).LT.0.D0) THEN
						DELZ=-1.0
						GO TO 13
					END IF
					IF (((DISTC-RGRD(1)).GT.0.D0).AND.
     $					((DISTC-RGRD(NRING)).LE.0.D0))
     $				GO TO 50
				END IF
C     Set ZCURRL equal to ZCURR for next step.
				ZCURRL=ZCURR
C     Determine distance of current point from center of accretion disk,
C     in the orbital plane.
				DIST=DSQRT(XCURR**2+YCURR**2)
C     If distance is greater than the rim radius, step back, divide the
C     step length, and start stepping out again.
				IF (DIST-RGRD(NRING).GT.0.D0) THEN
					XCURR=XCURR-DS*VLDC
					YCURR=YCURR-DS*VMDC
					ZCURR=ZCURR-DS*VNDC
					DS=DS/2.D0
					IF (DS.GT.0.0001D0) GO TO 11
					GO TO 12
				ELSE
					IF (DS.GT.0.0001D0) GO TO 11
					GO TO 12
				END IF
C     Test whether the vector misses the rim.
   12				DELZ=HV-DABS(ZCURR)
C     If DELZ is negative, the vector clears the rim.
   13 CONTINUE
   				IF (DELZ.LT.0.D0) THEN
C     If we arrive here, continue with calculation.				   																				
C     Only calculate irradiation for points visible at (XA,YA,ZA).
C     Calculate the cosine of the angle between the vector and the
C     surface normal at (XA,YA,ZA).
					CSXA=VLDC*ZL(L,ITHA,KA)+VMDC*ZM(L,ITHA,KA)+VNDC*
     $					ZN(L,ITHA,KA)
C     Calculate irradiative contribution.
C     The integrand, for constant phi increment of DOMG, is VXI().
					DAP=VR(L,ITHA,KA)*VR(L,ITHA,KA)*SNT(L,ITHA)*
     $					CSXA*SEPCM2
					IF (DAP.LT.0.D0) DAP=0.D0
					F1=SGMA*(TMPR(L,ITHA,KA)*1.0D3)**4
					F=F1*CF/(6.D0-2.D0*U1(L,ITHA,KA)-3.D0*U2(L,ITHA,KA))
					VXI(KA)=F*DAP*(1.D0-U1(L,ITHA,KA)*(1.D0-CSXA)
     $						-U2(L,ITHA,KA)*(1.D0-CSXA)**2)/
     $						RCP**2/SEPCM2*CSX
				END IF
   50				CONTINUE
C     End of loop for phi-wise stepping on irradiating star.
   			END DO
C     Perform phi-wise integration for one strip on irradiating star.
C     Store integrated results, as a function of the index ITHA, in
C     SMB(ITHA).
			CALL ITF(VXI,1,NA,DOMG,0.D0,0.D0,0,ER,SMB(ITHA),
     $					  NSG,KRT)
C     End of loop for theta-wise stepping on irradiating star.
		END DO
C
C     *****************************************************************
C     Perform theta-wise integration with theta increment DTH.
C     Store integrated result for photospheric element at X,Y,Z in CSTFX
		CALL ITF(SMB,1,NTH,DTH,0.D0,0.D0,0,ER,CSTFX,NTH,KRT)
		IF (CSTFX.LT.0.D0) CSTFX=0.D0
C     Calculate total irradiating flux from rim, face and mass gainer
C     in TOTFX. The flux is in erg/s on area DLA.
   51		TOTFX=RMFX+FCFX+CSTFX
C     Correct local Teff for irradiation.
C     Note that we use the bolometric albedo read in from PGCAC.INF.
C     Be sure that the correct values of A(I), A(3-I)	have been used
C     in PGC run. Should use A(L)=0.D0 for that PGC run. 
		TMPR(I,ITH,K)=(((TMPR(I,ITH,K)*1.0D3)**4+AV(I)*TOTFX/
     $				SGMA)**0.25D0)/1.0D3
    4		END DO
	END DO
C     End of loop over irradiated component
C
C     *****************************************************************
C     List some of the calculated quantities.
      WRITE (NOUT1,108) KEY1,KEY2,KEY3,MDAT
	N=NPH(L,NTH/2+1)
	KQT=1+N/4
	KHF=1+N/2
	VP=TMPR(L,1,1)
	V1=TMPR(L,NTH/2+1,1)
	V2=TMPR(L,NTH/2+1,KQT)
	V3=TMPR(L,NTH/2+1,KHF)
	WRITE (NOUT1,109) L,VP,V1,V2,V3
	WRITE (NOUT1,107)
	N=NPH(3-L,NTH/2+1)
	KQT=1+N/4
	KHF=1+N/2
	VP=TMPR(3-L,1,1)
	V1=TMPR(3-L,NTH/2+1,1)
	V2=TMPR(3-L,NTH/2+1,KQT)
	V3=TMPR(3-L,NTH/2+1,KHF)
	WRITE (NOUT1,109) 3-L,VP,V1,V2,V3
	WRITE (NOUT1,107)
	WRITE (NOUT1,110) L
	DO ITH=1,NTH
		N=NPH(L,ITH)
		WRITE (NOUT1,112) ITH
		WRITE (NOUT1,111) (TMPR(L,ITH,K),K=1,N)
		WRITE (NOUT1,107)
	END DO
	WRITE (NOUT1,107)
	WRITE (NOUT1,110) 3-L
	DO ITH=1,NTH
		N=NPH(3-L,ITH)
		WRITE (NOUT1,112) ITH
		WRITE (NOUT1,111) (TMPR(3-L,ITH,K),K=1,N)
		WRITE (NOUT1,107)
	END DO
C     ********************************************
C     Loop for successive wavelengths
C     ********************************************
C
      DO IWL=1,NWL
		WLV=WL(IWL)
	DO J=1,NTH
		NP=NPH(L,J)
		DO K=1,N
			TMP=TMPR(L,J,K)
C			CALL DKCF(WLV,NGR,NTMP,NRF,TMP,G(L,ITH,K),U1(L,ITH,K),U2(L,ITH,K))
			ICOMP=L
			IF (MDAT.NE.0) GO TO 15
			CALL BBDY(TMP,WLV,BLMK)
			GO TO 16
   15			CALL MDLAT(WLV,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TMP,G(L,ITH,K),
     $				BLMK)
   16			BDI(L,J,K)=CF*BLMK/(6.D0-2.D0*U1(L,J,K)-3.D0*U2(L,J,K))
		END DO
	END DO
C     Change reference to other component
	DO J=1,NTH
		N=NPH(I,J)
		DO K=1,N
			TMP=TMPR(I,J,K)
C			CALL DKCF(WLV,NGR,NTMP,NRF,TMP,G(I,ITH,K),U1(I,ITH,K),U2(I,ITH,K))
			ICOMP=I
			IF (MDAT.NE.0) GO TO 19
			CALL BBDY(TMP,WLV,BLMK)
			GO TO 20
   19			CALL MDLAT(WLV,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TMP,G(I,ITH,K),
     $				BLMK)
   20			BDI(I,J,K)=CF*BLMK/(6.D0-2.D0*U1(I,J,K)-3.D0*U2(I,J,K))
		END DO
	END DO
	NT=NTL(L,NTH)
	NUV=NTL(3-L,NTH)
	CALL WTOUT9(L,3-L,NTH,NOUT4)
C
	CALL WTOUT7(I,IWL,NTH,NOUT3)
C
C     ***********************************************************
C
C
      REWIND IN4
	REWIND IN5									
	REWIND IN6
	REWIND IN7
	REWIND IN8
	REWIND IN15
	END DO
	CALL WTOUT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),
     $UA(3-I),NOUT2)
C
   99 CONTINUE
C   99 CALL GETDAT(IYR,IMON,IDAY)
C	CALL GETTIM(IHR,IMIN,ISEC,I100TH)
	TM=3600*IHR+60*IMIN+ISEC
	AD=I100TH
	TM=(100.D0*TM+AD)/100.D0
	AD=IDAY
	DA=AD+TM/86400.D0
	CALL CVJD(DA,IMON,IYR,FVL2)
	TM=(FVL2-FVL1)*1440.D0
	WRITE (NOUT1, 205) TM
C**********************************************************************
      CLOSE (UNIT = 19,    STATUS = 'KEEP')
	CLOSE (UNIT = IN1,   STATUS = 'KEEP')
	CLOSE (UNIT = IN2,   STATUS = 'KEEP')
	CLOSE (UNIT = IN3,   STATUS = 'KEEP')
	CLOSE (UNIT = IN4,   STATUS = 'KEEP')
	CLOSE (UNIT = IN5,   STATUS = 'KEEP')
	CLOSE (UNIT = IN6,   STATUS = 'KEEP')
	CLOSE (UNIT = IN7,   STATUS = 'KEEP')
	CLOSE (UNIT = IN8,   STATUS = 'KEEP')
	CLOSE (UNIT = IN9,   STATUS = 'KEEP')
	CLOSE (UNIT = IN10,  STATUS = 'KEEP')
	CLOSE (UNIT = IN11,  STATUS = 'KEEP')
	CLOSE (UNIT = IN12,  STATUS = 'KEEP')
	CLOSE (UNIT = IN13,  STATUS = 'KEEP')
	CLOSE (UNIT = IN14,  STATUS = 'KEEP')
	CLOSE (UNIT = IN15,  STATUS = 'KEEP')
	CLOSE (UNIT = NOUT1, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT2, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT3, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT4, STATUS = 'KEEP')
	CLOSE (UNIT = 25, STATUS = 'KEEP')
	CLOSE (UNIT = 26, STATUS = 'KEEP')
	CLOSE (UNIT = 40, STATUS = 'DELETE')
	STOP
	END PROGRAM PGCAC
