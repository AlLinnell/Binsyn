      SUBROUTINE ROTFN(I,AVL,R,QVL,VLAMV,ER)
	INCLUDE 'COMA.FOR'
C     SRT to calculate function whose root gives radius to photosphere
C     for non-synchronously rotating stellar component.
C     This SRT uses the Roche model.
      BKT1V=DSQRT(1.D0-2.D0*VLAMV*R+R**2)
      ER=1.D0/R+QVL*(1.D0/BKT1V-VLAMV*R)+AVL*R**2-OM(I)
	RETURN
	END
