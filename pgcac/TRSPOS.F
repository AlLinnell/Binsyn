      SUBROUTINE TRSPOS(NTH,I)
C     SRT to shift the arrays associated with star I so that the 
C     starting value for a given latitude profile corresponds with
C     the value stored at longitude pi, and is interchanged with the
C     value stored at longitude 0.0. Corresponding interchanges are
C     made down to the end of the arrays.
	INCLUDE 'COMA.FOR'
	CALL SHF(I,NTH)
	NSHF=NTH/2+1
	DO ITH=2,NSHF
		CALL SHIFT(I,NTH)
	END DO
	RETURN
	END 