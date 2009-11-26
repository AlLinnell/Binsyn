      SUBROUTINE ROOTFN(X,X1,X2,A,B,V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Condition for double points as given by Moulton,p.291,Eq.19A
      C=X-X1
      D=X-X2
      V=X-A*C/DABS(C**3)-B*D/DABS(D**3)
      RETURN
      END

