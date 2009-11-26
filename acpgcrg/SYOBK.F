      SUBROUTINE SYOBK(I,KTH,KPH,MTH,K,ITH)
C     Subroutine for converting indices from MTH basis to NTH basis.
C     The input quantities are I,KTH,KPH,MTH.
C     I is the designation of the stellar component.
C     KTH is the polar angle index for the angle theta.
C     KPH is the azimuthal index for the angle phi.
C     The output quantities are K,ITH.
C     K and ITH are the indices on the NTH basis,covering the first
C     quarter of the entire star.
      INCLUDE 'COMA.FOR'
	NTH=(MTH+1)/2
      IF (KTH.GT.NTH) GO TO 30 
C     IF WE ARRIVE HERE,WE ARE IN THE FIRST OR SECOND SEGMENTS    
      CALL IDX(I,KTH,KST)      
      CALL IDW(I,KTH,MST)      
      N1=MPH(I,KTH)
	N2=NPH(I,KTH)
C     Determine location of crossover from first segment to second 
C     segment.
	KND=MST+N2-1
	IF (KTH.EQ.1) GO TO 10
      IF (KPH.LE.KND) GO TO 10   
      GO TO 20     
C     IF WE ARRIVE HERE,WE ARE IN THE FIRST SEGMENT 
   10 K=KST+KPH-MST   
      ITH=KTH    
      GO TO 99   
C     IF WE ARRIVE HERE,WE ARE IN THE SECOND SEGMENT
   20 K=KST+N2-1+KND-KPH
      ITH=KTH    
      GO TO 99   
C     IF WE ARRIVE HERE,WE ARE IN THE THIRD OR FOURTH SEGMENTS    
   30 ITH=MTH-KTH+1     
      CALL IDX(I,ITH,KST)      
      CALL IDW(I,KTH,MST)      
      N1=MPH(I,KTH)
	N2=NPH(I,ITH)
	KND=MST+N2-1
	IF (KPH.LE.KND) GO TO 35 
      GO TO 40     
C     IF WE ARRIVE HERE,WE ARE IN THE THIRD SEGMENT 
   35 K=KST+KPH-MST   
      GO TO 99   
C     IF WE ARRIVE HERE,WE ARE IN THE FOURTH SEGMENT
   40 K=KST+N2-1+KND-KPH 
   99 RETURN     
      END 
