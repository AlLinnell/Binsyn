      SUBROUTINE COODS(ISTAR,ICON,R,PHI,XVMU,XVMOMU,X,Y)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DATA PI/3.14159265359D0/
C     Subroutine to calculate center of mass coordinates, given
C     the component designation and the the radial distance from
C     the center of that component.
C     ISTAR is the component designation.
C     ICON is the designation of the component in contact with the
C     Roche lobe.
C     R is the radial distance from the component center.
C     PHI is the azimuth, measured from the line of centers.
C     VMU is the X cood. of the contact component.
C     VMOMU is the X cood. of the detached component.
C     XL1 is the X cood. of the L1 point.
	IF (ISTAR.EQ.ICON) THEN
		XSM=R*DCOS(PHI)
		YSM=R*DSIN(PHI)
		X=XVMU+XSM
		Y=YSM
	ELSE
		XSM=R*DCOS(PHI-PI)
		YSM=R*DSIN(PHI-PI)
		X=XVMOMU-XSM
		Y=YSM
	END IF
      RETURN
	END