      SUBROUTINE PLSPT(LCT,NSPA,NSPB,I,VBTDA,VLMDA,VGMDA,VBTDB,
     $VLMDB,VGMDB,ISV,TARY,NTH,
     $CSPJ,SNPJ,VRD,ICOUNT,XCM,YCM,NOUT1,NOUT2,NOUT3,NOUT4)
C     Subroutine to plot starspots projected on star disks
	INCLUDE 'COMA.FOR'
      DIMENSION TARY(MTR,MTR),VBTDA(MTHM),VLMDA(MTHM),
     $VGMDA(MFV,MTHM),
     $VBTDB(MTHM),VLMDB(MTHM),VGMDB(MFV,MTHM)
      DIMENSION LCT(16) 
      DIMENSION PXA(MTHM),PYA(MTHM),QXA(MTHM),QYA(MTHM)
      DATA PIO2/1.5707963267949D0/
      DATA TWPI/6.2831853071796D0/
      DATA ER/1.0D-8/
	DATA ERLT/1.D-13/
C*********************************************************************
C     The following section is for both the eclipsing and eclipsed 
C     star
C
C     Do spots
C*********************************************************************
C
  193 FORMAT (2F12.5)
C     For ICOUNT=1, star I designates the eclipsing star.
C     NSPA specifies the number of spots on the first component,
C     i.e., the component eclipsed at primary minimum.
	IF (I.EQ.ISV) NSP=NSPA
	IF (I.NE.ISV) NSP=NSPB
      DO 500 K=1,NSP
C     Set position of center of that spot
	IF (I.EQ.ISV) THEN
		THVC=PIO2-VBTDA(K)
		PHVC=VLMDA(K)
	ELSE
		THVC=PIO2-VBTDB(K)
		PHVC=VLMDB(K)
	END IF
C     Check whether any of spot is above horizon
      IF (I.EQ.ISV) DLGM=VGMDA(1,K)
	IF (I.NE.ISV) DLGM=VGMDB(1,K)
	DLPHV=DLGM/20.D0
C     SRT SGLTP returns cos(gamma) for assigned values of THVC, PHVC
C
	CALL SGLTP(THVC,PHVC-DLGM,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG1,I,NTH)
	CALL SGLTP(THVC,PHVC+DLGM,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG2,I,NTH)
	CALL SGLTP(THVC-DLGM,PHVC,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG3,I,NTH)
	CALL SGLTP(THVC+DLGM,PHVC,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG4,I,NTH)
C     Test for allof spot below horizon.
C     If yes,skip plot of spot.
	IF (VCSG1.LT.0.D0.AND.VCSG2.LT.0.D0.AND.VCSG3.LT.0.D0.AND.VCSG4.
     $LT.0.D0) GO TO 500
C     Set value of AGL, fixed for a given spot.
      AGL=VGMDA(1,K)/DSIN(PIO2-VBTDA(K))
C     Set minimum theta, corresponding to point at top of spot
C     Use angular radius of first color
C     (Program allows for spot radius as a fn. of color.)
      THETAMIN=PIO2-VBTDA(K)-VGMDA(1,K)
C     Set maximum theta, corresponding to point at bottom of spot.
      THETAMAX=PIO2-VBTDA(K)+VGMDA(1,K)
C     Set theta range
      THETARG=2.0D0*VGMDA(1,K)
C     Outline each spot with MPTS-1 points
      MPTS=201
      DVVL=MPTS/2
      DO 400 J=1,MPTS
C     Set value of theta
      IF (J.LE.MPTS/2+1) THEN
	THV=THETAMIN+(J-1)*THETARG/DVVL
	IF (I.EQ.ISV) THEN
		THPRIME=PIO2*(THV-PIO2+VBTDA(K))/VGMDA(1,K)
		DISPL=AGL*DCOS(THPRIME)
		PHV=VLMDA(K)-DISPL
C		THV=THVC+VGMDA(1,K)*DSIN(ALPH)
C		PHV=PHVC-VGMDA(1,K)*DCOS(ALPH)
	ELSE
		THPRIME=PIO2*(THV-PIO2+VBTDB(K))/VGMDB(1,K)
		DISPL=AGL*DCOS(THPRIME)
		PHV=VLMDB(K)-DISPL
C		THV=THVC+VGMDB(1,K)*DSIN(ALPH)
C		PHV=PHVC-VGMDB(1,K)*DCOS(ALPH)
	END IF
	END IF	
      IF (J.GT.MPTS/2+1) THEN
	THV=THETAMAX-(J-MPTS/2-1)*THETARG/DVVL
C
	IF (I.EQ.ISV) THEN
		THPRIME=PIO2*(THV-PIO2+VBTDA(K))/VGMDA(1,K)
		DISPL=AGL*DCOS(THPRIME)
		PHV=VLMDA(K)+DISPL
C		THV=THVC+VGMDA(1,K)*DSIN(ALPH)
C		PHV=PHVC-VGMDA(1,K)*DCOS(ALPH)
	ELSE
		THPRIME=PIO2*(THV-PIO2+VBTDB(K))/VGMDB(1,K)
		DISPL=AGL*DCOS(THPRIME)
		PHV=VLMDB(K)+DISPL
C		THV=THVC+VGMDB(1,K)*DSIN(ALPH)
C		PHV=PHVC-VGMDB(1,K)*DCOS(ALPH)
	END IF
	END IF	
      SNP=DSIN(PHV)
      CSP=DCOS(PHV)
      ANUV=DCOS(THV)
      SNTHV=DSIN(THV)
      ALUV=SNTHV*CSP
      AMUV=SNTHV*SNP
      PXA(J)=1000.0D0
      PYA(J)=1000.0D0
      QXA(J)=1000.0D0
      QYA(J)=1000.0D0
C     Calculate cos(gamma) for that particular point on spot boundary
      CALL SGLTP(THV,PHV,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG,I,NTH)
C     Is that point above horizon?
      IF (VCSG.GE.0.0D0) GO TO 659
C     If arrive here, the point is below horizon.
C     Find the horizon point and plot it
      LPCT=0
	CF=1.D0
	PHVSV=PHV
      VCSGSV=VCSG
  201 PHV=PHV-DLPHV*CF
      LPCT=LPCT+1
	CALL SGLTP(THV,PHV,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG,I,NTH)
	IF (VCSG.GE.0.D0) THEN
	SNP=DSIN(PHV)
	CSP=DCOS(PHV)
	ALUV=SNTHV*CSP
	AMUV=SNTHV*SNP
	GO TO 659
	END IF
	IF (VCSG.GT.VCSGSV) GO TO 201
	PHV=PHVSV
	CF=-1.D0
	IF (LPCT.LT.100) GO TO 201
	GO TO 400
C	CALL PMDSTOP
  659 X=RV*ALUV
      Y=RV*AMUV
      Z=RV*ANUV
      IF (I.EQ.ISV) GO TO 660
      X=X-VRD
  660 IF (ICOUNT.EQ.2) GO TO 380
      CALL PROJ(TARY,X,Y,Z,PXA(J),PYA(J),PZ)
      GO TO 425
  380 CALL PROJ(TARY,X,Y,Z,QXA(J),QYA(J),PZ)
  425 DO ITH=1,NTH-1
	VTH1=TH(I,ITH)
	VTH2=TH(I,ITH+1)
      IF ((THV.GE.VTH1).AND.(THV.LT.VTH2)) GO TO 430
      END DO
      CALL PMDSTOP
  430 CALL LNTER(VTH1,SHS(ITH,1),VTH2,SHS(ITH+1,1),THV,
     $SHDB1)
      CALL LNTER(VTH1,SHN(ITH,1),VTH2,SHN(ITH+1,1),THV,
     $SHDN1)
      CALL LNTER(VTH1,SHS(ITH,2),VTH2,SHS(ITH+1,2),THV,
     $SHDB2)
      CALL LNTER(VTH1,SHN(ITH,2),VTH2,SHN(ITH+1,2),THV,
     $SHDN2)
      CALL LNTER(VTH1,SHS(ITH,3),VTH2,SHS(ITH+1,3),THV,
     $SHDB3)
      CALL LNTER(VTH1,SHN(ITH,3),VTH2,SHN(ITH+1,3),THV,
     $SHDN3)
      IF ((PHV.GT.SHDB1).AND.(PHV.LT.SHDN1)) GO TO 431
      IF ((PHV.GT.SHDB2).AND.(PHV.LT.SHDN2)) GO TO 431
      IF ((PHV.GT.SHDB3).AND.(PHV.LT.SHDN3)) GO TO 431
      GO TO 400
  431 IF (ICOUNT.EQ.2) GO TO 432
      IF (J.LE.1) GO TO 433
      GO TO 400
  433 PXA(J)=1000.0D0
      PYA(J)=1000.0D0
      GO TO 400
  432 IF (J.LE.1) GO TO 434
      GO TO 400
  434 QXA(J)=1000.0D0
      QYA(J)=1000.0D0
  400 CONTINUE
      DO 410 J=1,MTHM
      IF (ICOUNT.EQ.2) GO TO 404
      XP=PXA(J)
      YP=PYA(J)
      GO TO 405
  404 XP=QXA(J)
      YP=QYA(J)
  405 IF (DABS(XP-1000.0D0).LE.ERLT) GO TO 410
      IF (LCT(15).EQ.1) GO TO 406
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 407
  406 X=XP
      Y=YP
  407 CONTINUE
C
      IF (ICOUNT.EQ.1) THEN
	IF (K.EQ.1) THEN
		WRITE (NOUT1,193) X-XCM,-Y+YCM
	END IF
	IF (K.EQ.2) THEN
		WRITE (NOUT2,193) X-XCM,-Y+YCM
	END IF
	END IF
C
      IF (ICOUNT.EQ.2) THEN
	IF (K.EQ.1) THEN
		WRITE (NOUT3,193) X-XCM,-Y+YCM
	END IF
	IF (K.EQ.2) THEN
		WRITE (NOUT4,193) X-XCM,-Y+YCM
	END IF
	END IF
  410 CONTINUE
  500 CONTINUE
      RETURN
      END
