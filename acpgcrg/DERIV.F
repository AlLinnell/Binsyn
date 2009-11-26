      SUBROUTINE DERIV(VKL,VNE,T,WVL,AVI,SMT,VLI,DVIDL)
C     Subroutine for calculating derivatives of light intensity along
C     set of specified directions for application to
C     bremsstrahlung equation of transfer.
C     The directions are for Gauss integration from -pi/2 to pi/2.
C     The COSV  array contains values of cos(theta) for the gauss
C     division point directions. The SNV array contains values of 
C     sin(theta) for the same set of directions. The WT array contains
C     the weights for gauss integration.
C     AVI contains the mean intensity. Note!! The integration interval is
C     from -pi/2 to pi/2. But the theta we have defined runs from
C     -pi to 0. So the sinusoidal functions to be integrated exchange from
C     the normal ones.
C     SMT contains (1/2) the integral of sin^2(theta)*cos(theta)*I(theta)
C     over the interval -pi/2 to pi/2.
C     Note that, by using BBDY, we assume LTE.
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION VLI(8),DVIDL(8),COSV(8),SNV(8),ABSTERM(8),WT(8)
	DATA COSV/0.623361059512D-1,0.313992738203D0,0.678186388771D0,
     $0.958774525644D0,0.958774525644D0,0.678186388771D0,
     $0.313992738203D0,0.623361059512D-1/
	DATA SNV/-0.998055213851D0,-0.949425384301D0,-0.734889938757D0,
     $-0.284167923904D0,0.284167923904D0,0.734889938757D0,
     $0.949425384301D0,0.998055213851D0/
	DATA WT/0.101228536290D0,0.222381034453D0,0.3137066458779D0,
     $0.3626837833784D0,0.3626837833784D0,0.3137066458779D0,
     $0.222381034453D0,0.101228536290D0/
	DATA H/0.7853981634D0/
	DATA SGE/6.65D-25/
	DATA THRET/0.375D0/
	DATA THRFTH/0.75D0/
C     Calculate mean intensity and second term of scattering function.
      AVI=H*(WT(1)*VLI(1)*COSV(1)+WT(2)*VLI(2)*COSV(2)+
     $WT(3)*VLI(3)*COSV(3)+WT(4)*VLI(4)*COSV(4)+WT(5)*VLI(5)*COSV(5)+
     $WT(6)*VLI(6)*COSV(6)+WT(7)*VLI(7)*COSV(7)+WT(8)*VLI(8)*COSV(8))
	SMT=H*(WT(1)*VLI(1)*COSV(1)*SNV(1)**2+
     $WT(2)*VLI(2)*COSV(2)*SNV(2)**2+
     $WT(3)*VLI(3)*COSV(3)*SNV(3)**2+
     $WT(4)*VLI(4)*COSV(4)*SNV(4)**2+
     $WT(5)*VLI(5)*COSV(5)*SNV(5)**2+
     $WT(6)*VLI(6)*COSV(6)*SNV(6)**2+
     $WT(7)*VLI(7)*COSV(7)*SNV(7)**2+
     $WT(8)*VLI(8)*COSV(8)*SNV(8)**2)
C     The input temperature to BBDY must be in thousands of degrees.
C     The value of T in the call to DERIV is in degrees.
C     The wavelength at input to BBDY must be in microns. The output
C     value BVL is for a spectral interval of one micron.
	CALL BBDY(T/1.D3,WVL,BVL)
C     Must express BVL for a spectral interval of one cm.
	BVL=BVL*1.D4
C     Calculate emission term for bremsstrahlung.
	EMISSA=VKL*BVL
C     Calculate first term for Thomson scattering.
	EMISSB=THRFTH*SGE*VNE*AVI
C     Calculate second term for Thomson scattering
      EMISSC=THRET*SGE*VNE*SMT
C     Calculate total emission term
      EMISS=EMISSA+EMISSB+EMISSC
C     Calculate absorption terms.
	DO J=1,4
		ABSTERM(J)=(VKL+VNE*SGE)*VLI(J)
	END DO
C     Calculate derivatives of intensity.
	DO J=1,4
		DVIDL(J)=(EMISS-ABSTERM(J))/COSV(J)
	END DO
	RETURN
	END