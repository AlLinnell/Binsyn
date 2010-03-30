      SUBROUTINE CVJD(DA,NM,NY,FN)
C     SRT to convert a date to corresponding Julian Date
C     DA=day of month as two digit number with decimal
C     NM=ordinal value of month. Jan=1, Dec=12
C     NY=year. Works for any date since 1 Jan. 4713 B.C.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      NY1=NY
	IF (NY.LT.0) NY1=NY1-1
	IF (NM.GE.3) THEN
		MN1=NM
	ELSE
		MN1=NM+12
		NY1=NY1-1
	END IF
	IB=0
	IF (NY.GT.1582) THEN
		IA=INT(NY1/100)
		IB=2-IA+INT(IA/4)
	ELSE
		IF (NY.EQ.1582) THEN
			IF (NM.GT.10) THEN
				IA=INT(NY1/100)
				IB=2-IA+INT(IA/4)
			ELSE
				IF (NM.EQ.10) THEN
					IF (DA.GE.15.D0) THEN
						IA=INT(NY1/100)
						IB=2-IA+INT(IA/4)
					END IF
				END IF
			END IF
		END IF
	END IF
	IF (NY1.LT.0) THEN
		IC=INT(365.25D0*NY1-0.75D0)-694025
	ELSE
		IC=INT(365.25D0*NY1)-694025
	END IF
	ID=INT(30.6001D0*(MN1+1))
	DJD=IB+IC+ID+DA-0.5D0
	FN=DJD+2415020.D0		
      RETURN
      END
