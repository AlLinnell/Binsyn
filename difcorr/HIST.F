      SUBROUTINE HIST(NWL,LWT,SIGMA,NTO,NHIST)
C     WITH KOLMOGOROV-SMIRNOV TEST OF GOODNESS OF FIT (TO NORMAL DISTRIBUTION)  
C     REFERENCE - STATISTICS IN RESEARCH - OSTLE - PAGE 471
C     TCDF IS THEORETICAL CUMULATIVE DISTRIBUTION FUNCTION 
C     SCDF IS SAMPLE CUMULATIVE DISTRIBUTION FUNCTION      
C     HIST ASSUMES NBIN IS FILLED SUCH THAT NBIN(26) CONTAINS THE NUMBER OF     
C     OBSERVATIONS WITHIN + OR - .05 SIGMA OF MEAN  
C     NBIN(59) CONTAINS NO. OF OBSERVATIONS LESS THAN -2.55 SIGMA FROM MEAN     
C     NBIN(60) CONTAINS NO. OF OBSERVATIONS GREATER THAN 2.55 SIGMA 
C     FROM MEAN     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LWT(5)
      DIMENSION NBIN(60)
      DIMENSION RSDV(100000,5)
      COMMON /G/ RSDV
      SAVE /G/
      DO K=1,60
         NBIN(K)=0
      END DO
      NTOT=0
      DO IWL=1,NWL
         NTOT=NTOT+LWT(IWL)
      END DO     
C     Produce frequency count of residual sizes, in units of std. dev.
      DO IWL=1,NWL
         LUL=LWT(IWL)
         DO 10 II=1,LUL
         RSD=RSDV(II,IWL)
         RSDN=RSD/SIGMA
            IF (RSDN.LT.-2.55D0) THEN
                NBIN(59)=NBIN(59)+1
                GO TO 10
            END IF
            IF ((-2.55D0.LE.RSDN).AND.(RSDN.LT.-2.45D0)) THEN
             NBIN(51)=NBIN(51)+1
             GO TO 10
            END IF
            IF ((-2.45D0.LE.RSDN).AND.(RSDN.LT.-2.35D0)) THEN
             NBIN(50)=NBIN(50)+1
             GO TO 10
            END IF
            IF ((-2.35D0.LE.RSDN).AND.(RSDN.LT.-2.25D0)) THEN
             NBIN(49)=NBIN(49)+1
             GO TO 10
            END IF 
            IF ((-2.25D0.LE.RSDN).AND.(RSDN.LT.-2.15D0)) THEN
             NBIN(48)=NBIN(48)+1
             GO TO 10
            END IF 
            IF ((-2.15D0.LE.RSDN).AND.(RSDN.LT.-2.05D0)) THEN
             NBIN(47)=NBIN(47)+1
             GO TO 10
            END IF 
            IF ((-2.05D0.LE.RSDN).AND.(RSDN.LT.-1.95D0)) THEN
             NBIN(46)=NBIN(46)+1
             GO TO 10
            END IF 
            IF ((-1.95D0.LE.RSDN).AND.(RSDN.LT.-1.85D0)) THEN
             NBIN(45)=NBIN(45)+1
             GO TO 10
            END IF 
            IF ((-1.85D0.LE.RSDN).AND.(RSDN.LT.-1.75D0)) THEN
             NBIN(44)=NBIN(44)+1
             GO TO 10
            END IF 
            IF ((-1.75D0.LE.RSDN).AND.(RSDN.LT.-1.65D0)) THEN
             NBIN(43)=NBIN(43)+1
             GO TO 10
            END IF 
            IF ((-1.65D0.LE.RSDN).AND.(RSDN.LT.-1.55D0)) THEN
             NBIN(42)=NBIN(42)+1
             GO TO 10
            END IF 
            IF ((-1.55D0.LE.RSDN).AND.(RSDN.LT.-1.45D0)) THEN
             NBIN(41)=NBIN(41)+1
             GO TO 10
            END IF 
            IF ((-1.45D0.LE.RSDN).AND.(RSDN.LT.-1.35D0)) THEN
             NBIN(40)=NBIN(40)+1
             GO TO 10
            END IF 
            IF ((-1.35D0.LE.RSDN).AND.(RSDN.LT.-1.25D0)) THEN
             NBIN(39)=NBIN(39)+1
             GO TO 10
            END IF 
            IF ((-1.25D0.LE.RSDN).AND.(RSDN.LT.-1.15D0)) THEN
             NBIN(38)=NBIN(38)+1
             GO TO 10
            END IF 
            IF ((-1.15D0.LE.RSDN).AND.(RSDN.LT.-1.05D0)) THEN
             NBIN(37)=NBIN(37)+1
             GO TO 10
            END IF 
            IF ((-1.05D0.LE.RSDN).AND.(RSDN.LT.-0.95D0)) THEN
             NBIN(36)=NBIN(36)+1
             GO TO 10
            END IF 
            IF ((-0.95D0.LE.RSDN).AND.(RSDN.LT.-0.85D0)) THEN
             NBIN(35)=NBIN(35)+1
             GO TO 10
            END IF 
            IF ((-0.85D0.LE.RSDN).AND.(RSDN.LT.-0.75D0)) THEN
             NBIN(34)=NBIN(34)+1
             GO TO 10
            END IF 
            IF ((-0.75D0.LE.RSDN).AND.(RSDN.LT.-0.65D0)) THEN
             NBIN(33)=NBIN(33)+1
             GO TO 10
            END IF 
            IF ((-0.65D0.LE.RSDN).AND.(RSDN.LT.-0.55D0)) THEN
             NBIN(32)=NBIN(32)+1
             GO TO 10
            END IF 
            IF ((-0.55D0.LE.RSDN).AND.(RSDN.LT.-0.45D0)) THEN
             NBIN(31)=NBIN(31)+1
             GO TO 10
            END IF 
            IF ((-0.45D0.LE.RSDN).AND.(RSDN.LT.-0.35D0)) THEN
             NBIN(30)=NBIN(30)+1
             GO TO 10
            END IF 
            IF ((-0.35D0.LE.RSDN).AND.(RSDN.LT.-0.25D0)) THEN
             NBIN(29)=NBIN(29)+1
             GO TO 10
            END IF 
            IF ((-0.25D0.LE.RSDN).AND.(RSDN.LT.-0.15D0)) THEN
             NBIN(28)=NBIN(28)+1
             GO TO 10
            END IF 
            IF ((-0.15D0.LE.RSDN).AND.(RSDN.LT.-0.05D0)) THEN
             NBIN(27)=NBIN(27)+1
             GO TO 10
            END IF 
            IF ((-0.05D0.LE.RSDN).AND.(RSDN.LE.0.05D0)) THEN
             NBIN(26)=NBIN(26)+1
             GO TO 10
            END IF 
            IF ((0.05D0.LE.RSDN).AND.(RSDN.LT.0.15D0)) THEN
             NBIN(25)=NBIN(25)+1
             GO TO 10
            END IF 
            IF ((0.15D0.LE.RSDN).AND.(RSDN.LT.0.25D0)) THEN
             NBIN(24)=NBIN(24)+1
             GO TO 10
            END IF 
            IF ((0.25D0.LE.RSDN).AND.(RSDN.LT.0.35D0)) THEN
             NBIN(23)=NBIN(23)+1
             GO TO 10
            END IF 
            IF ((0.35D0.LE.RSDN).AND.(RSDN.LT.0.45D0)) THEN
             NBIN(22)=NBIN(22)+1
             GO TO 10
            END IF
            IF ((0.45D0.LE.RSDN).AND.(RSDN.LT.0.55D0)) THEN
             NBIN(21)=NBIN(21)+1
             GO TO 10
            END IF
            IF ((0.55D0.LE.RSDN).AND.(RSDN.LT.0.65D0)) THEN
             NBIN(20)=NBIN(20)+1
             GO TO 10
            END IF
            IF ((0.65D0.LE.RSDN).AND.(RSDN.LT.0.75D0)) THEN
             NBIN(19)=NBIN(19)+1
             GO TO 10
            END IF
            IF ((0.75D0.LE.RSDN).AND.(RSDN.LT.0.85D0)) THEN
             NBIN(18)=NBIN(18)+1
             GO TO 10
            END IF
            IF ((0.85D0.LE.RSDN).AND.(RSDN.LT.0.95D0)) THEN
             NBIN(17)=NBIN(17)+1
             GO TO 10
            END IF
            IF ((0.95D0.LE.RSDN).AND.(RSDN.LT.1.05D0)) THEN
             NBIN(16)=NBIN(16)+1
             GO TO 10
            END IF
            IF ((1.05D0.LE.RSDN).AND.(RSDN.LT.1.15D0)) THEN
             NBIN(15)=NBIN(15)+1
             GO TO 10
            END IF
            IF ((1.15D0.LE.RSDN).AND.(RSDN.LT.1.25D0)) THEN
             NBIN(14)=NBIN(14)+1
             GO TO 10
            END IF
            IF ((1.25D0.LE.RSDN).AND.(RSDN.LT.1.35D0)) THEN
             NBIN(13)=NBIN(13)+1
             GO TO 10
            END IF
            IF ((1.35D0.LE.RSDN).AND.(RSDN.LT.1.45D0)) THEN
             NBIN(12)=NBIN(12)+1
             GO TO 10
            END IF
            IF ((1.45D0.LE.RSDN).AND.(RSDN.LT.1.55D0)) THEN
             NBIN(11)=NBIN(11)+1
             GO TO 10
            END IF
            IF ((1.55D0.LE.RSDN).AND.(RSDN.LT.1.65D0)) THEN
             NBIN(10)=NBIN(10)+1
             GO TO 10
            END IF
            IF ((1.65D0.LE.RSDN).AND.(RSDN.LT.1.75D0)) THEN
             NBIN(9)=NBIN(9)+1
             GO TO 10
            END IF
            IF ((1.75D0.LE.RSDN).AND.(RSDN.LT.1.85D0)) THEN
             NBIN(8)=NBIN(8)+1
             GO TO 10
            END IF
            IF ((1.85D0.LE.RSDN).AND.(RSDN.LT.1.95D0)) THEN
             NBIN(7)=NBIN(7)+1
             GO TO 10
            END IF
            IF ((1.95D0.LE.RSDN).AND.(RSDN.LT.2.05D0)) THEN
             NBIN(6)=NBIN(6)+1
             GO TO 10
            END IF
            IF ((2.05D0.LE.RSDN).AND.(RSDN.LT.2.15D0)) THEN
             NBIN(5)=NBIN(5)+1
             GO TO 10
            END IF
            IF ((2.15D0.LE.RSDN).AND.(RSDN.LT.2.25D0)) THEN
             NBIN(4)=NBIN(4)+1
             GO TO 10
            END IF
            IF ((2.25D0.LE.RSDN).AND.(RSDN.LT.2.35D0)) THEN
             NBIN(3)=NBIN(3)+1
             GO TO 10
            END IF
            IF ((2.35D0.LE.RSDN).AND.(RSDN.LT.2.45D0)) THEN
             NBIN(2)=NBIN(2)+1
             GO TO 10
            END IF
            IF ((2.45D0.LE.RSDN).AND.(RSDN.LT.2.55D0)) THEN
             NBIN(1)=NBIN(1)+1
             GO TO 10
            END IF
            IF (2.55D0.LE.RSDN) THEN
             NBIN(60)=NBIN(60)+1
            END IF                      
   10    CONTINUE
      END  DO                                                                                                                                                                                                        
      DO K=1,51
         WRITE (NHIST,70) K,NBIN(K)
      END DO
   70 FORMAT (I5,I15)     
   11 WRITE (NTO,51) SIGMA,NTOT
   51 FORMAT(1H ,9X,'STANDARD DEVIATION=1.0 SCALE UNIT=',E10.3,   
     2' (O-C) RESIDUALS',/,10X,'THERE WERE ',I5,' OBSERVATIONS (TOTAL)') 
      WRITE (NTO,53) NBIN(59),NBIN(60)
   53 FORMAT(1H ,9X,'THERE WERE ',I5,' OBSERVATIONS LESS THAN -2.5 SCALE 
     1 UNITS',/,1H ,9X,'THERE WERE ',I5,' OBSERVATIONS GREATER THAN +2.5 
     3 SCALE UNITS',/,1H ,'IN THE FOLLOWING TABLE, TCDF IS THE THEORETIC
     4AL CUMULATIVE DISTRIBUTION FUNCTION',/,
     51H ,'SCDF IS THE SAMPLE CUMULATIVE DISTRIBUTION FUNCTION',/,
     61H ,'D IS THE DIFFERENCE')    
      TNTOT=NTOT 
      WRITE (NTO,1200)  
 1200 FORMAT('      D               TCDF             SCDF            BIN
     2',/)    
      SCDF=NBIN(59)/TNTOT      
      TCDF=.00539D0
      D=DABS(SCDF-TCDF)
      I=59 
      WRITE (NTO,1000) D,TCDF,SCDF,I    
      DO 80 I=1,51      
      SCDF=SCDF+NBIN(I)/TNTOT  
      UNIT=(I-26.D0)/10.D0  
      TCDF=TCDF+.03989423D0*DEXP (-UNIT**2/2.D0)  
      DI=DABS(SCDF-TCDF)
      IF(DI.GT.D) D=DI  
   80 WRITE (NTO,1000) DI,TCDF,SCDF,I
      SCDF=SCDF+NBIN(60)/TNTOT
      TCDF=TCDF+.0053494D0
      DI=DABS(SCDF-TCDF)
      I=60
      WRITE (NTO,1000)DI,TCDF,SCDF,I  
 1000 FORMAT(1H ,3(E14.7,2X),6X,I3)   
      CRITVAL=1.36D0/DSQRT(TNTOT)
      WRITE (NTO,60) SIGMA,D,CRITVAL  
   60 FORMAT('HYPOTHESIS - THE DISTRIBUTION OF RESIDUALS IS NORMAL WITH 
     2MEAN OF ZERO',/,'AND STANDARD DEVIATION SIGMA=',E10.3,/,' THE VALU 
     3E OF D IS ',E10.3, 'AND AT THE .05 LEVEL OF SIGNIFICANCE',/,
     4' THE CRITICAL VALUE OF D IS ',E10.3)      
      IF(D.GT.CRITVAL)GO TO 64 
      GO TO 68   
   64 WRITE (NTO,65)    
   65 FORMAT(1H ,'THE HYPOTHESIS IS REJECTED',/)      
      GO TO 100  
   68 WRITE (NTO,69)    
   69 FORMAT(1H ,'THE HYPOTHESIS IS ACCEPTED',/) 

  100 RETURN     
      END 
