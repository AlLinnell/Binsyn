      FUNCTION GASDEV(IDUM)
      DATA ISET/0/
      IF (ISET.EQ.0) THEN
C       V1=2.*RAN1(IDUM)-1.
C       V2=2.*RAN1(IDUM)-1.
   1    call random_number(v1)
        V1=2*V1-1
        call random_number(v2)
        V2=2*V2-1
        R=V1**2+V2**2
        IF(R.GE.1.)GO TO 1
        FAC=SQRT(-2.*LOG(R)/R)
        GSET=V1*FAC
        GASDEV=V2*FAC
        ISET=1
      ELSE
        GASDEV=GSET
        ISET=0
      ENDIF
      RETURN
      END
