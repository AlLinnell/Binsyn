      SUBROUTINE POTFN(ISTAR,ICON,VMU,VMOMU,PHI,PTRF,PTERR,R)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate the difference between the potential
C     for a given azimuth and equatorial radius on component ISTAR
C     and reference potential PTRF.
C     The potentials are in center of mass coordinates.
C     ICON is the designation of the component in contact with the
C     Roche lobe.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU=1.d0-VMU.
C     XL1 is the X cood. of the L1 point.
C     PHI is the azimuth of the point relative to the center of the
C     component in question, measured from the line of centers.
C     PTRF is the reference potential.
C     PTERR is the difference between the potential and the reference 
C     potential.
C     R is the radial distance from the center of the component ISTAR.
C     NOTE! The parameter -VMU supplied to COODS is the X coordinate
C     of the center of the contact component!
	CALL COODS(ISTAR,ICON,R,PHI,-VMU,VMOMU,X,Y)
	CALL POTCM(X,Y,0.D0,VMU,VMOMU,POT)
	PTERR=POT-PTRF
	RETURN
	END