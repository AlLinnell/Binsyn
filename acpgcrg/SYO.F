      SUBROUTINE SYO(I,KTH,KPH,NTH,K,ITH,ISG,VPHV)
C     SRT to determine the phi (longitude) on component I
C     The input value of the latitude index is KTH, using the input
C     number of latitude indices between the upper pole and the equator
C     equal to NTH. The value of the longitude index is KPH.
C     Associated phi values range between 0.0 and pi.
C     The SRT requires the array of phi values to be in the array PH,
C     located in a COMMON block included in COMA.FOR. The phi values
C     are sequential, extending from 0 to 2*pi on a given latitude
C     circle, and with phi values for a given latitude circle following
C     sequentially after the end of the phi values for the preceeding
C     latitude circle, and starting at the upper pole. There is a single
C     phi value for eack pole.
C     The SRT also returns the latitude index, based on the number of
C     latitude indices between the upper and lower poles, in ITH and the
C     longitude index in K. The segment of the equivalent sphere in which
C     the index K is located is returned in ISG. The value of phi is
C     returned in VPHV.   
      INCLUDE 'COMA.FOR'
      DATA TWPI/6.2831853071796D0/
      IF (KTH.GT.NTH) GO TO 30 
C     IF WE ARRIVE HERE,WE ARE IN THE FIRST OR SECOND SEGMENTS    
      CALL IDX(I,KTH,KST)      
      CALL IDW(I,KTH,MST)      
      KK=KPH-MST 
      IF (KTH.EQ.1) GO TO 10   
      IF ((KK+2).GT.NPH(I,KTH)) GO TO 20     
C     IF WE ARRIVE HERE,WE ARE IN THE FIRST SEGMENT 
   10 K=KST+KK   
      VPHV=PH(I,K)
      ISG=1      
      ITH=KTH    
      GO TO 99   
C     IF WE ARRIVE HERE,WE ARE IN THE SECOND SEGMENT
   20 KL=2*NPH(I,KTH)-1 
      K=KST+KL-KK-1     
      VPHV=TWPI-PH(I,K)  
      ISG=2      
      ITH=KTH    
      GO TO 99   
C     IF WE ARRIVE HERE,WE ARE IN THE THIRD OR FOURTH SEGMENTS    
   30 ITH=2*NTH-KTH     
      CALL IDX(I,ITH,KST)      
      CALL IDW(I,KTH,MST)      
      KK=KPH-MST 
      IF (ITH.EQ.1) GO TO 35   
      IF ((KK+2).GT.NPH(I,ITH)) GO TO 40     
C     IF WE ARRIVE HERE,WE ARE IN THE THIRD SEGMENT 
   35 K=KST+KK   
      VPHV=PH(I,K)
      ISG=3      
      GO TO 99   
C     IF WE ARRIVE HERE,WE ARE IN THE FOURTH SEGMENT
   40 KL=2*NPH(I,ITH)-1 
      K=KST+KL-KK-1     
      VPHV=TWPI-PH(I,K)  
      ISG=4      
   99 RETURN     
      END 
