      SUBROUTINE S2GLTP(I,THV,PHB,ER,SN,CS,RVS)
C     Subroutine to calculate radial distance to photosphere, as well
C     as sine and cosine of PHB
C     On return, the radius is in RVS, the sine of PHB is in SN, and
C     the cosine of PHB is in CS.
      INCLUDE 'COMA.FOR'
	INCLUDE 'COMM.FOR'
		CALL SGLTP(THV,PHB,ER,RVS,GV,VLV,VMV,VNV,VCSB,VCSG,I,NTH)
		SN=DSIN(PHB)
		CS=DCOS(PHB)
      RETURN
	END
