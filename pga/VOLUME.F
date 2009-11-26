      SUBROUTINE VOLUME(I,A,B,NR,NTH,VOL)
C     Subroutine to calculate the volume of component I
      INCLUDE 'COMA.FOR'
      DIMENSION ARB(MTHM),SMA(MTHM),SMB(MTHM),AR3(MTHM)
      DATA PI/3.1415926535898D0/
C     A = polar radius of component I
C     B = point radius of component I
C     Approximate by calculating volume of sphere of radius A
C     and adding to that an integration over the volume of the 
C     component between A and B. Divide that volume into NR
C     shells.
C     DR is the thickness of each shell.
      DR=(B-A)/(NR-1)
      RVLD=A-DR
C     RVLD is the inner radius of a given shell, starting at 
C     RVLD = A.
      DO 51 IR=1,NR
C     SMB(IR) accumulates the volume contribution for each shell.
      SMB(IR)=0.D0
      RVLD=RVLD+DR
      DO 50 ITH=1,NTH
C     SMA(ITH) accumulates the volume contribution for each ITH,
C     on a given shell.
      SMA(ITH)=0.D0
C     There are N azimuthal points on a given shell at a given ITH.
      N=NPH(I,ITH)
      DO 49 K=1,N
C     AR3(k) contains the values of phi.
	AR3(K)=PH(I,ITH,K)
      CF=0.D0
C     If the following test is true, the radial distance of the
C     point from the origin is greater than the current value
C     of RVLD. Calculate a volume increment by setting CF=1.D0.
      IF ((VR(I,ITH,K)+DR).GE.RVLD) CF=1.D0
C     ARB(K) contains a volume increment.
      ARB(K)=CF*RVLD*RVLD*SNT(I,ITH)*DR
   49 CONTINUE
      KST=1
      LND=N
      KLM=LND-1
      K=KST
      KBG=KST
      KND=LND
  201 H=AR3(KBG+1)-AR3(KBG)
C     H is the increment of phi
      DO 202 K=KBG,KLM
      IF (DABS(AR3(K+2)-AR3(K+1)-H).GT.0.000000001D0) GO TO 203
  202 CONTINUE
C     Integrate over one value of ITH, on a given shell.
  205 CALL ITF(ARB,KBG,KND,H,0.D0,0.D0,0,ER,SM,MTHM,KR)
C     Accumulate the integral.
      SMA(ITH)=SMA(ITH)+SM
      GO TO 204
  203 KND=K+1
      IF (KBG.NE.KND) GO TO 205
      IF ((K.EQ.KBG).OR.(KBG.EQ.KND)) CALL PMDSTOP
  204 KBG=KND
      KND=LND
      IF (KBG.LT.LND) GO TO 201
   50 CONTINUE
C     Now prepare to integrate over the theta values for a given
C     shell.
      KST=1
      LND=NTH
C     Place theta values in AR3(K)
      DO 48 K=1,NTH
      AR3(K)=TH(I,K)
   48 CONTINUE
      KLM=LND-1
      K=KST
      KBG=KST
      KND=LND
C     Calculate the theta increment.
  301 H=AR3(KBG+1)-AR3(KBG)
      DO 302 K=KBG,KLM
      IF (DABS(AR3(K+2)-AR3(K+1)-H).GT.0.000000001D0) GO TO 303
  302 CONTINUE
C     Perform integration over theta.
  305 CALL ITF(SMA,KBG,KND,H,0.D0,0.D0,0,ER,SM,MTHM,KR)
      SMB(IR)=SMB(IR)+SM
      GO TO 304
  303 KND=K+1
      IF (KBG.EQ.KND) GO TO 304
      GO TO 305
  304 KBG=KND
      KND=LND
      IF (KBG.LT.LND) GO TO 301
   51 CONTINUE
C     Now sum over the shells for the volume outside the
C     sphere of radius A
      SM=0.D0
	DO K=1,NR
		SM=SM+SMB(K)
	END DO
C     Calculate the total volume by adding the volume of the sphere
C     of radius A.
      VOL=4.D0/3.D0*PI*A**3
	VOL=VOL+SM
      RETURN
      END
