      SUBROUTINE SHIFT(I,NTH)
C     Subroutine to reverse values of ALM, AMU, ZL, and ZM
C     
      INCLUDE 'COMA.FOR'
	DO ITH=1,NTH
		N=NPH(I,ITH)
		DO K=1,N
			ALM(I,ITH,K)=-ALM(I,ITH,K)
			AMU(I,ITH,K)=-AMU(I,ITH,K)
			ZL(I,ITH,K)=-ZL(I,ITH,K)
			ZM(I,ITH,K)=-ZM(I,ITH,K)
		END DO
	END DO
	RETURN
	END