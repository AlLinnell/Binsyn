      SUBROUTINE CR(Y0,H,F1,DF1,DF2,DF3,DF4,DF5,DF6,Y1)
C     SRT to calculate a corrected value for a predicted ordinate
C     The quantities DF1,DF2,...come from differences using the
C     original differential equation for DF1.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DATA CF1/-0.5D0/
	DATA CF2/-8.33333333333333333D-2/
	DATA CF3/-4.16666666666666667D-2/
	DATA CF4/-2.63888888888888889D-2/
	DATA CF5/-1.875D-2/
	DATA CF6/1.42691798942D-2/
	Y1=Y0+H*(F1+CF1*DF1+CF2*DF2+CF3*DF3+CF4*DF4+CF5*DF5+CF6*DF6)
	RETURN
	END