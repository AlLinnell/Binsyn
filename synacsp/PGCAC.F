      PROGRAM PGCAC											  
	USE MSFLIB												 
C     Program for calculating irradiation of a binary star mass donor
C     by an accretion disk, including the irradiation of the donor by
C     the central star.
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
	DIMENSION AGLP(MPHM),BDIZ(MPHM),U1Z(MPHM),U2Z(MPHM),
     $Z1(MPHM),Z2(MPHM),Z3(MPHM),RVLP(MTLM),GLS(MTLM),
     $ZLAL(MTLM),ZLAM(MTLM),ZLAN(MTLM),SNTP(MTW,MPHM),GZ(MPHM)
	DIMENSION SMB(MPHM),VXI(MPHM),AV(MTW)
C
      DIMENSION VPHLS(MTLM),RVLPLS(MTLM),ALLS(MTLM),AMLS(MTLM),
     $ANLS(MTLM),ZLALLS(MTLM),ZLAMLS(MTLM),ZLANLS(MTLM),
     $TMPRLS(MTLM),TMPRSV(MTLM),TMPRL(MTLM),GL(MTLM),
     $U1L(MTLM),U2L(MTLM),BDIL(MTLM),U1LS(MTLM),U2LS(MTLM),
     $BDILS(MTLM)
C
	COMMON /NUNIT/ NU,NUOUT,IFOUT
	CHARACTER * 7 LABEL
	CHARACTER * 24 FL1
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 4 RESULT
C
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
	DATA IN10,IN11,IN12,IN13,IN14,IN15/10,11,12,13,14,15/
	DATA NOUT1,NOUT2,NOUT3/16,17,18/
C     Number of division points in integration over irradiating star.
	DATA NTHETA/11/
  100 FORMAT (A24)
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
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
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
      OPEN (UNIT=20,  FILE='C:\LGTSYN\ACDISK\PGCAC.ILS',STATUS='OLD')
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
	open (unit=99,    file='c:\oa2\pgcacscr.dat',status='replace')
C**********************************************************************
      CALL GETDAT(IYR,IMON,IDAY)
	CALL GETTIM(IHR,IMIN,ISEC,I100TH)
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
      CALL RDWT2(NT,NU,NTH,I,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,PSIP,
     $KCTRL,KFORM,IN5)
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
      MTH=2*NTH-1
C     Initialize SMB()
      DO ITH=1,MTH
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
c	write (99,1001) i,l,istar
C
      NTS=0
      DO ITH=1,NTH
		IF (ITH.LE.NTH) SNTP(L,ITH)=SNT(L,ITH)
		IF (ITH.GT.NTH) SNTP(L,ITH)=SNT(L,2*NTH-ITH)
	END DO
	DO ITH=1,NTH
		CALL IDX(L,ITH,KST)
		N=NPH(L,ITH)
		KND=KST+N-1
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1001) ith
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1001) kst,knd,n
		DO K=KST,KND
			AGLP(K-KST+1)=PH(L,K)
			RVLT(K-KST+1)=VR(L,K)
			ALT(K-KST+1)=ALM(L,K)
			AMT(K-KST+1)=AMU(L,K)
			ANT(K-KST+1)=ANU(L,ITH)
			Z1(K-KST+1)=ZL(L,K)
			Z2(K-KST+1)=ZM(L,K)
			Z3(K-KST+1)=ZN(L,K)
			TMPRSV(K-KST+1)=TMPR(L,K)
			GZ(K-KST+1)=G(L,K)
			BDIZ(K-KST+1)=BDI(L,K)
			U1Z(K-KST+1)=U1(L,K)
			U2Z(K-KST+1)=U2(L,K)
		END DO
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmpr(l,k),k=kst,knd)
C     Complete data for points above equator.
		LM=N-1
		IF (LM.NE.0) THEN
			DO K=1,LM
				AGLP(N+K)=TWPI-AGLP(N-K)
				RVLT(N+K)=RVLT(N-K)
				ALT(N+K)=ALT(N-K)
				AMT(N+K)=-AMT(N-K)
				ANT(N+K)=ANT(N-K)
				Z1(N+K)=Z1(N-K)
				Z2(N+K)=-Z2(N-K)
				Z3(N+K)=Z3(N-K)
				TMPRSV(N+K)=TMPRSV(N-K)
				GZ(N+K)=GZ(N-K)
				BDIZ(N+K)=BDIZ(N-K)
				U1Z(N+K)=U1Z(N-K)
				U2Z(N+K)=U2Z(N-K)
			END DO
		END IF
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1001) n,lm
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmprsv(k),k=1,2*n-1)
C     Assemble data in main storage, upper four octants.
		LM=2*N-1
		DO K=1,LM
			VPH(NTS+K)=AGLP(K)
			RVLP(NTS+K)=RVLT(K)
			AL(NTS+K)=ALT(K)
			AM(NTS+K)=AMT(K)
			AN(NTS+K)=ANT(K)
			ZLAL(NTS+K)=Z1(K)
			ZLAM(NTS+K)=Z2(K)
			ZLAN(NTS+K)=Z3(K)
			TMPRL(NTS+K)=TMPRSV(K)
			GL(NTS+K)=GZ(K)
			BDIL(NTS+K)=BDIZ(K)
			U1L(NTS+K)=U1Z(K)
			U2L(NTS+K)=U2Z(K)
		END DO
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1003)
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1001) nts,lm
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002)
c     $(tmprl(nts+k),k=1,lm)
		NTS=NTS+LM
	END DO
C     Set up to do lower four octants.
      NTR=NTS-LM
	NTLM=NTH-1
c	write (99,1001) nts,lm,ntr,nth,ntlm
	DO ITH=1,NTLM
		N=NPH(L,NTH-ITH)
		LM=2*N-1
		DO K=1,LM
			VPH(NTS+K)=VPH(NTR-LM+K)
			RVLP(NTS+K)=RVLP(NTR-LM+K)
			AL(NTS+K)=AL(NTR-LM+K)
			AM(NTS+K)=-AM(NTR-LM+K)
			AN(NTS+K)=-AN(NTR-LM+K)
			ZLAL(NTS+K)=ZLAL(NTR-LM+K)
			ZLAM(NTS+K)=ZLAM(NTR-LM+K)
			ZLAN(NTS+K)=-ZLAN(NTR-LM+K)
			TMPRL(NTS+K)=TMPRL(NTR-LM+K)
			GL(NTS+K)=GL(NTR-LM+K)
			BDIL(NTS+K)=BDIL(NTR-LM+K)
			U1L(NTS+K)=U1L(NTR-LM+K)
			U2L(NTS+K)=U2L(NTR-LM+K)
		END DO
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write(99,1001) 
c     $ith,n,lm,nts,ntr,ntr-lm
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmprl(nts+k),k=1,lm)
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1003)
		NTS=NTS+LM
		NTR=NTR-LM
	END DO
c	do ith=1,mth
c	n=mph(i,ith)
c	call idw(i,ith,kst)
c	knd=kst+n-1
c	write (99,1001) ith,n,kst,knd
c	write (99,1002) (tmprl(k),k=kst,knd)
c	write (99,1003)
c	end do
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
C     Do this with SRT TRSPOS
      CALL TRSPOS(NTH,I)
C     Because the loser may be irradiated by a bright spot that is off
C     the line of centers of the two stars, we must expand out the 
C     arrays for the loser and consider all eight octants.
      NTS=1
	DO ITH=1,MTH
		IF (ITH.LE.NTH) SNTP(I,ITH)=SNT(I,ITH)
		IF (ITH.GT.NTH) SNTP(I,ITH)=SNT(I,2*NTH-ITH)
	END DO
	DO ITH=1,NTH
		CALL IDX(I,ITH,KST)
		N=NPH(I,ITH)
		KND=KST+N-1
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1001) 
c     $ith,kst,knd,n,i
		DO K=KST,KND
C     Fill temporary arrays, starting at index 1.
			AGLP(K-KST+1)=PH(I,K)
			RVLT(K-KST+1)=VR(I,K)
			ALT(K-KST+1)=ALM(I,K)
			AMT(K-KST+1)=AMU(I,K)
			ANT(K-KST+1)=ANU(I,ITH)
			Z1(K-KST+1)=ZL(I,K)
			Z2(K-KST+1)=ZM(I,K)
			Z3(K-KST+1)=ZN(I,K)
			TMPRSV(K-KST+1)=TMPR(I,K)
			GZ(K-KST+1)=G(I,K)
			BDIZ(K-KST+1)=BDI(I,K)
			U1Z(K-KST+1)=U1(I,K)
			U2Z(K-KST+1)=U2(I,K)
		END DO
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmprsv(k),k=1,n)
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1003)
C     Complete data for points above equator
C     Each temporary array will have data for the full azimuthal range
C     0 to two pi.
		LM=N-1
			DO K=1,LM
				AGLP(N+K)=TWPI-AGLP(N-K)
				RVLT(N+K)=RVLT(N-K)
				ALT(N+K)=ALT(N-K)
				AMT(N+K)=-AMT(N-K)
				ANT(N+K)=ANT(N-K)
				Z1(N+K)=Z1(N-K)
				Z2(N+K)=-Z2(N-K)
				Z3(N+K)=Z3(N-K)
				TMPRSV(N+K)=TMPRSV(N-K)
				GZ(N+K)=GZ(N-K)
				BDIZ(N+K)=BDIZ(N-K)
				U1Z(N+K)=U1Z(N-K)
				U2Z(N+K)=U2Z(N-K)
			END DO
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmprsv(k),k=1,2*n-1)
C     Assemble data in main storage, upper four octants.
		IF (ITH.EQ.1) THEN
			VPHLS(1)=PH(I,1)
			RVLPLS(1)=VR(I,1)
			ALLS(1)=ALM(I,1)
			AMLS(1)=AMU(I,1)
			ANLS(1)=ANU(I,1)
			ZLALLS(1)=ZL(I,1)
			ZLAMLS(1)=ZM(I,1)
			ZLANLS(1)=ZN(I,1)
			TMPRLS(1)=TMPR(I,1)
			GLS(1)=G(I,1)
			BDILS(1)=BDI(I,1)
			U1LS(1)=U1(I,1)
			U2LS(1)=U2(I,1)
		END IF
		IF (ITH.GT.1) THEN
		LM=2*N-1
		DO K=1,LM
			VPHLS(NTS+K)=AGLP(K)
			RVLPLS(NTS+K)=RVLT(K)
			ALLS(NTS+K)=ALT(K)
			AMLS(NTS+K)=AMT(K)
			ANLS(NTS+K)=ANT(K)
			ZLALLS(NTS+K)=Z1(K)
			ZLAMLS(NTS+K)=Z2(K)
			ZLANLS(NTS+K)=Z3(K)
			TMPRLS(NTS+K)=TMPRSV(K)
			GLS(NTS+K)=GZ(K)
			BDILS(NTS+K)=BDIZ(K)
			U1LS(NTS+K)=U1Z(K)
			U2LS(NTS+K)=U2Z(K)
		END DO
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1001) 
c     $ith,n,lm,nts
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmprsv(k),k=1,lm)
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1003)
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1002) 
c     $(tmprls(nts+k),k=1,lm)
c	if ((ith.ge.nth-1).and.(ith.le.nth+1)) write (99,1003) 
		NTS=NTS+LM
		END IF
	END DO
C     Set up to do lower four octants.
	NTLM=NTH-1
	NTR=NTS-LM
	DO ITH=1,NTLM
		N=NPH(I,NTH-ITH)
		LM=2*N-1
		DO K=1,LM
			VPHLS(NTS+K)=VPHLS(NTR-LM+K)
			RVLPLS(NTS+K)=RVLPLS(NTR-LM+K)
			ALLS(NTS+K)=ALLS(NTR-LM+K)
			AMLS(NTS+K)=AMLS(NTR-LM+K)
			ANLS(NTS+K)=-ANLS(NTR-LM+K)
			ZLALLS(NTS+K)=ZLALLS(NTR-LM+K)
			ZLAMLS(NTS+K)=ZLAMLS(NTR-LM+K)
			ZLANLS(NTS+K)=-ZLANLS(NTR-LM+K)
			TMPRLS(NTS+K)=TMPRLS(NTR-LM+K)
			GLS(NTS+K)=GLS(NTR-LM+K)
			BDILS(NTS+K)=BDILS(NTR-LM+K)
			U1LS(NTS+K)=U1LS(NTR-LM+K)
			U2LS(NTS+K)=U2LS(NTR-LM+K)
		END DO
		NTS=NTS+LM
		NTR=NTR-LM
	END DO
c	do ith=1,mth
c	n=mph(i,ith)
c	call idw(i,ith,kst)
c	knd=kst+n-1
c	write (99,1001) ith,n,kst,knd
c	write (99,1002) (tmprls(k),k=kst,knd)
c	write (99,1003)
c	end do
c	if (i.lt.1000) stop
C     Now proceed with the setup of points on the loser.
      DO ITH=1,MTH
		CALL IDW(I,ITH,KST)
		N=MPH(I,ITH)
		KND=KST+N-1
c	write (99,1001) i,iec,isv,istar
 1001 format (10i5)
c	write (99,1001) ith
c	write (99,1001)kst,knd,n
c	write (99,1003)
c	write (99,1002) (tmprls(k),k=1,n)
c	write (99,1003)
c	write (99,1002) (tmprl(k),k=1,n)
c	write (99,1003)
 1002 format (5e15.8)
 1003 format (' ')
		DO K=KST,KND
			X=RVLPLS(K)*ALLS(K)
			Y=RVLPLS(K)*AMLS(K)
			Z=RVLPLS(K)*ANLS(K)
C     Calculate element of photospheric area associated with the point
C     in question. Note that, for the cap region at the pole, we 
C     integrate over the angular radius of the cap.
			IF (ITH.EQ.1) THEN
				DLA=TWPI*RVLPLS(K)**2*(1.D0-ANLS(K+1))
			END IF
			IF (ITH.EQ.MTH) THEN
				DLA=TWPI*RVLPLS(K)**2*(1.D0-ANLS(K-1))
			END IF
			IF ((ITH.NE.1).AND.(ITH.NE.MTH)) THEN
				CALL SYP(I,ITH,NTH,VTH1)
				CALL SYP(I,ITH+1,NTH,VTH2)
				DPHL=VPHLS(KST+1)-VPHLS(KST)
				DLA=RVLPLS(K)**2*SNTP(I,ITH)*DPHL*(VTH2-VTH1)
			END IF
C     Express element of area in cm^2
			DLA=DLA*SEPCM2
C     The mass gainer and the accretion disk have their centers at the 
C     origin of coordinates. For calculation of irradiation, displace
C     the mass loser to VRD.
			X=X+VRD
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
						DI=F*(1.D0-U1RM(KK,J)-U2RM(KK,J)
     $						+U1RM(KK,J)*CSVEC+U2RM(KK,J)*CSVEC**2)*
     $						DAPJ
C     Calculate cosine of angle between vector and local normal to
C     photosphere point, at the point X,Y,Z. Note that the direction 
C     cosines of VLDC must be reversed so that we have a vector
C     pointed toward the rim segment.
						CSNR=-VLDC*ZLALLS(K)-VMDC*ZLAMLS(K)-
     $							VNDC*ZLANLS(K)
C     Only consider illumination if X,Y,Z is visible from rim point.
						IF (CSNR.GE.0.D0) THEN
C     Calculate flux contribution for that rim element.
C     Note that radiation from rim element is into only a hemisphere,
C     so division is by two pi, not 4*pi.
C     The terms  DLA/TWPI/VRV**2/SEPCM2  represent the solid angle that
C     the element of area on the donor intercepts at the location on the
C     rim under consideration.
							DRMFX=DI*CSNR*DLA/TWPI/VRV**2/SEPCM2
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
					IF (ITH.LE.NTH) THEN
					VRV=DSQRT((X-VXAF(KK,J))**2+(Y-VYAF(KK,J))**2
     $					+(Z-VZAF(KK,J))**2)
					ELSE
					VRV=DSQRT((X-VXAF(KK,J))**2+(Y-VYAF(KK,J))**2
     $					+(Z+VZAF(KK,J))**2)
					END IF
C     Calculate the direction cosines of the vector toward X,Y,Z.
					VLDC=(X-VXAF(KK,J))/VRV
					VMDC=(Y-VYAF(KK,J))/VRV
					IF (ITH.LE.NTH) THEN
					VNDC=(Z-VZAF(KK,J))/VRV
					ELSE
					VNDC=(Z+VZAF(KK,J))/VRV
					END IF
C     Calculate the cosine of the angle between the vector and the local
C     normal to the accretion disk face.
					IF (ITH.LE.NTH) THEN
					CSVEC=VLDC*ZLAF(KK)+VMDC*ZMAF(KK)+VNDC*ZNAF(KK)
					ELSE
					CSVEC=VLDC*ZLAF(KK)+VMDC*ZMAF(KK)-VNDC*ZNAF(KK)
					END IF
C     Note that this calculation is for one face of the accretion disk 
C     only. By symmetry, since points X,Y,Z cover only 1/4 of the entire
C     photosphere, the irradiation is the same for the region below the
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
					IF (ITH.LE.NTH) THEN
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
						DI=F*(1.D0-U1F(KK,J)-U2F(KK,J)
     $					+U1F(KK,J)*CSVEC+U2F(KK,J)*CSVEC**2)
     $					*DAPJ
C     Calculate the cosine of the angle between the vector and the 
C     local normal to the photosphere at X,Y,Z.
						CSNR=-VLDC*ZLALLS(K)-VMDC*ZLAMLS(K)-
     $							VNDC*ZLANLS(K)
C     Only consider illumination if X,Y,Z is visible from the face 
C     point.
						IF (CSNR.GE.0.D0) THEN
C     Calculate face flux contributions.
C     See the discussion of the rim flux for further details.
							DFCFX=DI*CSNR*DLA/TWPI/VRV**2/SEPCM2
						END IF
    3						CONTINUE
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
C     Note that we consider illumination by only half of component L.
C     Follow the procedure in program PGC, SRT FIFR.
C     The star at the center of the accretion disk may be rotationally 
C     distorted. Need to find a mean radius. RD1 is the polar radius.
			RD1=VR(L,1)
C     KSTA is the starting index for points on the equator.
			CALL IDX(L,NTH,KSTA)
			NA=NPH(L,NTH)
			KQT=KSTA+NA/2
C     RD2 is the radius of the "side" point on the equator.
			RD2=VR(L,KQT)
			RSV=(RD1+RD2)/2.D0
C     Determine mean intensity of radiation and mean limb darkening..
			F1=SGMA*(TMPR(L,1)*1.23244D3)**4
			F2=SGMA*(TMPR(L,KQT)*1.23244D3)**4
			FM=(F1+F2)/2.D0
			U1M=(U1(L,1)+U1(L,KQT))/2.D0
			U2M=(U2(L,1)+U2(L,KQT))/2.D0
C     F is normal intensity in erg/cm^2(cross-section)/s/sr
			F=FM*CF/(6.D0-2.D0*U1M-3.D0*U2M)
C     Calculate the total irradiative flux.
C     Loop over the irradiated photosphere.Note that we consider the
C     complete photosphere of the irradiating star, component L.
C     Start with the upper polar cap.	Note in the following that we
C     have integrated over sine(theta). That is, we consider the polar
C     cap to be a small irradiating segment. The area of a polar cap
C     segment is 	TWPI*r^2*(1-cos(theta))*SEPCM2, where theta is the
C     polar angle of the cap edge.
C     XA, YA, ZA are the coordinates of a point on the photosphere of
C     the irradiating star.
C     Initialize SMB(1)
		SMB(1)=0.D0
		XA=RVLP(1)*AL(1)
		YA=RVLP(1)*AM(1)
		ZA=RVLP(1)*AN(1)
C     The coordinates of the point on the irradiating star are XA,YA,ZA
C     RCP is the distance between the irradiating segment and the
C     irradiated segment. VLDC, VMDC, VNDC are the direction cosines of
C     a unit vector at the irradiating segment, directed toward the
C     irradiated segment.
		RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
		VLDC=(X-XA)/RCP
		VMDC=(Y-YA)/RCP
		VNDC=(Z-ZA)/RCP
C     Calculate the cosine of the angle between this vector and the
C     local normal at the polar cap.
		CSNR=VLDC*ZLAL(1)+VMDC*ZLAM(1)+VNDC*ZLAN(1)
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
C     The vector clears the rim. Calculate the cosine of the angle
C     between the vector toward XA,YA,ZA and the local normal at X,Y,Z.
		CSX=-VLDC*ZLALLS(K)-VMDC*ZLAMLS(K)-VNDC*ZLANLS(K)
C     There is an irradiation contribution only if CSX is not negative.
		IF (CSX.GE.0.D0) THEN
			CSXA=VLDC*ZLAL(1)+VMDC*ZLAM(1)+VNDC*ZLAN(1)
C     DAP is the area of the cap, projected normal to the connecting 
C     vector, in cm^2. 
C     DLA*CSX/TWPI/RCP^2/SEPCM2 is the solid angle which the element of area
C     on the loser intercepts at XA,YA,ZA.
			DAP=RVLP(1)*RVLP(1)*TWPI*(1.D0-ANU(L,2))*CSXA*SEPCM2
			IF (DAP.LT.0.D0) DAP=0.D0
			SMB(1)=F*DAP*(1.D0-U1(L,1)-U2(L,1)+U1(L,1)*CSXA+U2(L,1)*
     $		CSXA**2)*DLA*CSX/TWPI/RCP**2/SEPCM2
		END IF
C     Now do lower polar cap.
   48		ITHA=MTH
C     Initialize SMB(MTH)
		SMB(MTH)=0.D0
		CALL IDW(L,ITHA,KSTA)
		N=MPH(L,ITHA)
		KNDA=KSTA+N-1
		XA=RVLP(KNDA)*AL(KNDA)
		YA=RVLP(KNDA)*AM(KNDA)
		ZA=RVLP(KNDA)*AN(KNDA)
		RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
		VLDC=(X-XA)/RCP
		VMDC=(Y-YA)/RCP
		VNDC=(Z-ZA)/RCP
C     Calculate the cosine of the angle between the vector toward X,Y,Z
C     and the local normal.
		CSNR=VLDC*ZLAL(KNDA)+VMDC*ZLAM(KNDA)+VNDC*ZLAN(KNDA)
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
C     Calculate cosine of angle between vector toward XA, YA, ZA at
C     X,Y,Z and local normal at X,Y,Z.
		CSX=-VLDC*ZLALLS(K)-VMDC*ZLAMLS(K)-VNDC*ZLANLS(K)
C     Only proceed if CSX is not negative.
		IF (CSX.GE.0.D0) THEN
			CSXA=VLDC*ZLAL(KNDA)+VMDC*ZLAM(KNDA)+VNDC*ZLAN(KNDA)
			DAP=RVLP(KNDA)*RVLP(KNDA)*TWPI*(1.D0-ANU(L,2))*CSXA*SEPCM2
			IF (DAP.LT.0.D0) DAP=0.D0
			SMB(MTH)=F*DAP*(1.D0-U1(L,KNDA)-U2(L,KNDA)+U1(L,KNDA)*CSXA
     $		+U2(L,KNDA)*CSXA**2)*DLA*CSX/TWPI/RCP**2/SEPCM2
		END IF
C
C     Do the remainder of star L.
C
   49		VMTH=MTH-1
		DTH=PI/VMTH
C     Initialize SMB
		DO ITHA=2,MTH-1
			SMB(ITHA)=0.D0
		END DO
C     Loop over remainder of star L for the point X,Y,Z
		DO ITHA=2,MTH-1
			CALL IDW(L,ITHA,KSTA)
			N=MPH(L,ITHA)
			KNDA=KSTA+N-1
			VN=N-1
C     Calculate the phi increment, DOMG, in radians.
			DOMG=TWPI/VN
C     Initialize VXI
			DO KA=1,201
				VXI(KA)=0.D0
			END DO
			DO KA=KSTA,KNDA
				CALL SYO (L,ITHA,KA,NTH,KAP,ITHP,ISG,VPHP)
C     Determine the coordinates of the photospheric segment on star L.
				XA=RVLP(KA)*AL(KA)
				YA=RVLP(KA)*AM(KA)
				ZA=RVLP(KA)*AN(KA)
C     Calculate the separation of the irradiated point and the 
C     irradiating segment.
				RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
C     Calculate direction cosines of vector directed from (XA,YA,ZA)
C     to (X,Y,Z).
				VLDC=(X-XA)/RCP
				VMDC=(Y-YA)/RCP
				VNDC=(Z-ZA)/RCP
C     Calculate cosine of angle between vector toward X,Y,Z and the
C     local normal to point XA,YA,ZA.
				CSNR=VLDC*ZLAL(KA)+VMDC*ZLAM(KA)+VNDC*ZLAN(KA)
C     Only proceed if CSNR is not negative.
				IF (CSNR.LE.0.D0) GO TO 50
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
   11				XCURR=XCURR+DS*VLDC
				YCURR=YCURR+DS*VMDC
				ZCURR=ZCURR+DS*VNDC
C     Test whether the vector has crossed the orbital plane.
C     If it has, and the distance from the center is within the
C     opaque part of the accretion disk, exit.
				IF (ITH.LE.NTH) THEN
				IF ((ZCURR.GE.0.D0).AND.(ZCURRL.LT.0.D0)) THEN
					DISTC=DSQRT(XCURR**2+YCURR**2)
					IF (DISTC.GT.RGRD(1))GO TO 50
				ELSE
					IF ((ZCURR.LT.0.D0).AND.(ZCURRL.LT.ZCURR))
     $					ZCURRL=ZCURR
				END IF
				ELSE
				IF ((ZCURR.LT.0.D0).AND.(ZCURRL.GT.0.D0)) THEN
					DISTC=DSQRT(XCURR**2+YCURR**2)
					IF (DISTC.GT.RGRD(1))GO TO 50
				ELSE
					IF ((ZCURR.GE.0.D0).AND.(ZCURRL.GT.ZCURR))
     $					ZCURRL=ZCURR
				END IF
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
					IF (DS.GT.0.0001D0) GO TO 11
					GO TO 12
				ELSE
					IF (DS.GT.0.0001D0) GO TO 11
					GO TO 12
				END IF
C     Test whether the vector misses the rim.
   12				IF (ITH.LE.NTH) THEN
   					DELZ=HV-ZCURR
				ELSE
					DELZ=HV+ZCURR
				END IF
C     If DELZ is negative, the vector clears the rim.
				IF (DELZ.LT.0.D0) THEN
C     If we arrive here, continue with calculation.				   																				
C     Calculate the cosine of the angle between the vector, directed
C     toward XA,YA,ZA, and the surface normal at (X,Y,Z).
				CSX=-VLDC*ZLALLS(K)-VMDC*ZLAMLS(K)-VNDC*ZLANLS(K)
C     Only calculate irradiation for points visible at (XA,YA,ZA).
				IF (CSX.GE.0.D0) THEN
C     Calculate the cosine of the angle between the vector and the
C     surface normal at (XA,YA,ZA).
					CSXA=VLDC*ZLAL(KA)+VMDC*ZLAM(KA)+VNDC*ZLAN(KA)
C     Calculate irradiative contribution.
C     The integrand, for constant phi increment of DOMG, is VXI().
C     Note that DAP/TWPI/RCP^2 is the solid angle which the element of
C     area at X,Y,Z intercepts at XA,YA,ZA.
					DAP=RVLP(KA)*RVLP(KA)*SNT(L,ITHP)*CSXA*SEPCM2
					IF (DAP.LT.0.D0) DAP=0.D0
					VXI(KA-KSTA+1)=F*DAP*(1.D0-U1(L,KAP)-U2(L,KAP)+
     $								U1(L,KAP)*CSXA
     $								+U2(L,KAP)*CSXA**2)*DLA*CSX/
     $								TWPI/RCP**2/SEPCM2
				END IF
				END IF
   50				CONTINUE
C     End of loop for phi-wise stepping on irradiating star.
   			END DO
C     Perform phi-wise integration for one strip on irradiating star.
C     Store integrated results, as a function of the index ITHA, in
C     SMB(ITHA).
			CALL ITF(VXI,1,N,DOMG,0.D0,0.D0,0,ER,SMB(ITHA),
     $					  201,KRT)
C     End of loop for theta-wise stepping on irradiating star.
		END DO
C     Perform theta-wise integration with theta increment DTH.
C     Store integrated result for photospheric element at X,Y,Z in CSTFX
		CALL ITF(SMB,1,MTH,DTH,0.D0,0.D0,0,ER,CSTFX,201,KRT)
		IF (CSTFX.LT.0.D0) CSTFX=0.D0
C     Calculate total irradiating flux from rim, face and mass gainer
C     in TOTFX. The flux is in erg/s on area DLA.
   51		TOTFX=RMFX+FCFX+CSTFX
C     Convert to flux per unit area, if a value of TOTFX has been 
C     calculated.
		IF (DLA.GT.0.D0) TOTFX=TOTFX/DLA
C     Correct local Teff for irradiation.
C     Note that we use the bolometric albedo read in from PGCAC.INF.
C     Be sure that the correct values of A(I), A(3-I)	have been used
C     in PGC run. Should use A(L)=0.D0 for that PGC run. 
		TMPRLS(K)=(((TMPRLS(K)*1.23244D3)**4+AV(I)*TOTFX/
     $				SGMA)**0.25D0)/1.23244D3
    4		END DO
	END DO
c	if (i.lt.1000) stop
C     List some of the calculated quantities.
      WRITE (NOUT1,108) KEY1,KEY2,KEY3,MDAT
	CALL IDW(L,NTH,KST)
	N=MPH(L,NTH)
	KQT=KST+(N+1)/4
	KHF=KST+(N+1)/2
	VP=TMPRL(1)*1.23244D0
	V1=TMPRL(KST)*1.23244D0
	V2=TMPRL(KQT)*1.23244D0
	V3=TMPRL(KHF)*1.23244D0
	WRITE (NOUT1,109) L,VP,V1,V2,V3
	WRITE (NOUT1,107)
	CALL IDW(3-L,NTH,KST)
	N=MPH(3-L,NTH)
	KQT=KST+(N+1)/4
	KHF=KST+(N+1)/2
	VP=TMPRLS(1)*1.23244D0
	V1=TMPRLS(KST)*1.23244D0
	V2=TMPRLS(KQT)*1.23244D0
	V3=TMPRLS(KHF)*1.23244D0
	WRITE (NOUT1,109) 3-L,VP,V1,V2,V3
	WRITE (NOUT1,107)
	WRITE (NOUT1,110) L
	DO ITH=1,MTH
		CALL IDW(L,ITH,KST)
		N=MPH(L,ITH)
		KND=KST+N-1
		WRITE (NOUT1,112) ITH
		WRITE (NOUT1,111) (TMPRL(K),K=KST,KND)
		WRITE (NOUT1,107)
	END DO
	WRITE (NOUT1,107)
	WRITE (NOUT1,110) 3-L
	DO ITH=1,MTH
		CALL IDW(3-L,ITH,KST)
		N=MPH(3-L,ITH)
		KND=KST+N-1
		WRITE (NOUT1,112) ITH
		WRITE (NOUT1,111) (TMPRLS(K),K=KST,KND)
		WRITE (NOUT1,107)
	END DO
C     ********************************************
C     Loop for successive wavelengths
C     ********************************************
C
      DO IWL=1,NWL
		WLV=WL(IWL)
	DO J=1,MTH
		CALL IDW(L,J,KST)
		NP=MPH(I,J)
		KND=KST+NP-1
		DO K=KST,KND
			TMP=TMPRL(K)*1.23244D0
C			CALL DKCF(WLV,NGR,NTMP,NRF,TMP,GL(K),U1L(K),U2L(K))
			ICOMP=L
			IF (MDAT.NE.0) GO TO 15
			CALL BBDY(TMP,WLV,BLMK)
			GO TO 16
   15			CALL MDLAT(WLV,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TMP,GL(K),
     $				BLMK)
   16			BDIL(K)=CF*BLMK/(6.D0-2.D0*U1L(K)-3.D0*U2L(K))
		END DO
	END DO
C     Change reference to other component.
	DO J=1,NTH
		CALL IDW(I,J,KST)
		NP=MPH(I,J)
		KND=KST+NP-1
		DO K=KST,KND
			TMP=TMPRLS(K)*1.23244D0
C			CALL DKCF(WLV,NGR,NTMP,NRF,TMP,GLS(K),U1LS(K),U2LS(K))
			ICOMP=I
			IF (MDAT.NE.0) GO TO 19
			CALL BBDY(TMP,WLV,BLMK)
			GO TO 20
   19			CALL MDLAT(WLV,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TMP,GLS(K),
     $				BLMK)
   20			BDILS(K)=CF*BLMK/(6.D0-2.D0*U1LS(K)-3.D0*U2LS(K))
		END DO
	END DO
	NT=MTL(L,NTH)
	NUV=MTL(3-L,NTH)
	CALL WTOUT9(L,3-L,MTH,RVLP,ZLAL,ZLAM,ZLAN,TMPRL,
     $GL,BDIL,U1L,U2L,VPHLS,RVLPLS,ALLS,AMLS,ANLS,ZLALLS,ZLAMLS,ZLANLS,
     $TMPRLS,GLS,BDILS,U1LS,U2LS,NOUT4)
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
C     Now copy corrected arrays to proper storage.
C     Determine correct initial value of pointer to star eclipsed at
C     orbital phase 0.0
C 
      I=L
	IF (I.NE.ISV) I=3-I
C
      IF (I.EQ.L) THEN
C     Note range of ITH
		DO ITH=1,NTH
			NA=MPH(I,ITH)
			N=NPH(I,ITH)
			CALL IDX(I,ITH,KST)
			KND=KST+N-1
			CALL IDW(I,ITH,KSTA)
			KNDA=KSTA+NA-1
			KK=KSTA-1
			DO K=KST,KND
				KK=KK+1
				TMPR(I,K)=TMPRL(KK)
				TMPR(3-I,K)=TMPRLS(KK)
C				FIN(I,K)=FINL(KK)
C				FIN(3-I,K)=FINLS(K)
				BDI(I,K)=BDIL(KK)
				BDI(3-I,K)=BDILS(KK)
C				BDJ(I,K)=BDJL(KK)
C				BDJ(3-I,K)=BDJLS(KK)
			END DO
		END DO
	ELSE
C     Note range of ITH
		DO ITH=1,NTH
			NA=MPH(3-I,ITH)
			N=NPH(3-I,ITH)
			CALL IDX(3-I,ITH,KST)
			KND=KST+N-1
			CALL IDW(3-I,ITH,KSTA)
			KNDA=KSTA+NA-1
			KK=KSTA-1
			DO K=KST,KND
				KK=KK+1
				TMPR(3-I,K)=TMPRL(KK)
				TMPR(I,K)=TMPRLS(KK)
C				FIN(3-I,K)=FINL(KK)
C				FIN(I,K)=FINLS(KK)
				BDI(3-I,K)=BDIL(KK)
				BDI(I,K)=BDILS(KK)
C				BDJ(3-I,K)=BDJL(KK)
C				BDJ(I,K)=BDJLS(KK)
			END DO
		END DO
	END IF
	CALL WTOUT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),
     $UA(3-I),NOUT2)
C
	CALL WTOUT7(I,IWL,NTH,NOUT3)
C
   99 CALL GETDAT(IYR,IMON,IDAY)
	CALL GETTIM(IHR,IMIN,ISEC,I100TH)
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
	END
