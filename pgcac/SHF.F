      SUBROUTINE SHF(I,NTH)
C     Subroutine to move second half of call arrays downward by one
C     index value and duplicate entry NTH/2+1 at NTH/2+2.
      INCLUDE 'COMA.FOR'
	DO ITH=1,NTH
		N=NPH(I,ITH)
		NS=N/2+1
		DO K=1,NS-1
			ALM(I,ITH,N-K+1)=ALM(I,ITH,N-K)
			AMU(I,ITH,N-K+1)=AMU(I,ITH,N-K)
			ZL(I,ITH,N-K+1)=ZL(I,ITH,N-K)
			ZM(I,ITH,N-K+1)=ZM(I,ITH,N-K)
			ZN(I,ITH,N-K+1)=ZN(I,ITH,N-K)
			POPL(I,ITH,N-K+1)=POPL(I,ITH,N-K)
			POPN(I,ITH,N-K+1)=POPN(I,ITH,N-K)
			CSB(I,ITH,N-K+1)=CSB(I,ITH,N-K)
		END DO
		IF (N.GT.1) THEN
			ALM(I,ITH,N/2+2)=ALM(I,ITH,N/2+1)
			AMU(I,ITH,N/2+2)=AMU(I,ITH,N/2+1)
			ZL(I,ITH,N/2+2)=ZL(I,ITH,N/2+1)
			ZM(I,ITH,N/2+2)=ZM(I,ITH,N/2+1)
			ZN(I,ITH,N/2+2)=ZN(I,ITH,N/2+1)
			POPL(I,ITH,N/2+2)=POPL(I,ITH,N/2+1)
			POPN(I,ITH,N/2+2)=POPN(I,ITH,N/2+1)
			CSB(I,ITH,N/2+2)=CSB(I,ITH,N/2+1)
		END IF
	END DO
	RETURN
	END