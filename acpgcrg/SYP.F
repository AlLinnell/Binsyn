      SUBROUTINE SYP(I,ITH,NTH,VTH)   
      INCLUDE 'COMA.FOR'
      DATA PI/3.1415926535898D0/ 
      IF (ITH.GT.NTH) GO TO 1  
      VTH=TH(I,ITH)     
      GO TO 10   
    1 VTH=PI-TH(I,2*NTH-ITH)   
   10 RETURN     
      END 
