      SUBROUTINE CNVRT(NDM,NVRR,NCON2,RTHETA2,NVRTR,RXVT,IFA)
C
C  PROGRAM CONVRT COMPUTES THE EXTREME VERTICES OF ANY
C  CONSTRAINT REGION THAT CAN BE DESCRIBED INITIALLY
C  BY A SET OF VERTICES AND A SET OF LINEAR CONSTRAINTS
C  ON THE VARIABLES.  CONVRT THEN CONSIDERS EACH OF
C  SEVERAL ADDITIONAL NEW CONSTRAINTS IN TURN, AND
C  DETERMINES ITS EFFECT ON THE CURRENT VERTICES.
C  CONVRT HANDLES BOTH MIXTURE AND NON-MIXTURE
C  VARIABLES.
C
C  CONSTRAINTS ARE INPUT TO CONVRT BY LISTING THEIR
C  COEFFICIENTS, WHERE IT IS ASSUMED THE CONSTRAINTS
C  ARE ONE-SIDED AND ARE WRITTEN IN THE FORM
C
C     A1*X1 + A2*X2 + ... + AQ*XQ + A0  .GE. 0
C
C  X1,X2, ..., XQ ARE THE VARIABLES AND A1, A2, ... ,
C  AQ, A0 ARE THE COEFFICIENTS.  CONVRT CHECKS FOR
C  INCONSISTENT OR UNNECESSARY CONSTRAINTS.
C
C  CONVRT PROMPTS THE USER FOR A PROBLEM BASE-NAME
C  THAT IS THEN USED AS THE FIRST PART OF ALL INPUT
C  AND OUTPUT FILE NAMES.  THE FOLLOWING EXTENSIONS
C  ARE USED TO COMPLETE THE FILE NAMES:
C     .DAT    CONVRT INPUT FILE.
C     .FUL    OUTPUT FILE WITH FULL OUTPUT.
C     .VRT    OUTPUT FILE WITH EXTREME VERTICES ONLY.
C     .INP    OPTIONAL OUTPUT FILE FOR USE AS INPUT
C             TO CONAEV.
C  THE "FULL" OUTPUT FILE (*.FUL) CONTAINS THE NUMBER
C  OF VARIABLES, THE NUMBER OF CONSTRAINTS, THE
C  INITIAL CONSTRAINT MATRIX, THE FINAL CONSTRAINT
C  MATRIX, THE FINAL DK MATRIX, AND THE EXTREME
C  VERTICES.
C
C  LIMITS:  12 VARIABLES
C           45 TOTAL ORIG. & NEW ONE-SIDED CONSTRAINTS
C         1000 EXTREME VERTICES
C
C  TO MODIFY THE LIMITS, CHANGE THE PARAMETER
C  STATEMENT VALUES BELOW AND IN THE SUBROUTINES.  THE
C  FORMAT STATEMENTS FOR WRITE(1,.), WRITE(2,.), AND
C  WRITE(4,.) MAY ALSO HAVE TO BE CHANGED.
C
C  SUBROUTINES CALLED: DKSUB,CONCHK,EDGE,SEARCH,FPRINT
C
C  CONVRT VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C  SUBROUTINES CALLED: DKSUB,CONCHK,EDGE,SEARCH,FPRINT
C
C  CONVRT VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
C  MODIFIED TO A SUBROUTINE BY JOHN LAWSON FEB 2011
C
C INPUT PARAMETERS
C    NDM -     ORDER OF THE CENTROIDS TO BE CALCULATED
C    NVRR -    NUMBER OF MIXTURE VARIABLES
C    NCON2 -   NUMBER OF CONSTRAINTS
C    RTHETA2 - CONSTRAINT MATRIX STORED AS A VECTOR OF COLUMNS
C 
C OUTPUT RESULTS
C    NVRTR  -  NUMBER OF ROWS IN OUT PUT MATRIX
C    RXVT   -  OUTPUT MATRIX OF EXTREME VERTICIES, CENTROIDS AND 
C              DIMENSION INDICATORS, STORED AS A VECTOR OF COLUMNS
C    IFA    -  VECTOR OF ERROR INDICATOR FLAGS 

      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,NCON,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
      CHARACTER*20 FIN
	  integer ynr(n3)
      DIMENSION THETA2(N3,N1),RTHETA2(N3*N1),RXVT(N2*N1)
	  DIMENSION IVRT(N2),IFA(4)
	  DIMENSION CNTRD(N1),CENT(N2,N1),CNTM(N2,N1)
	  TOL1=0.000001
      TOL2=0.00001
C SET ERROR FLAGS TO ZERO
      DO 10 IJ=1,4
	  IFA(IJ)=0
  10  CONTINUE
C DEFINES NVAR, AND NCON
      NVAR=NVRR
C  **** I will make NCON = NVAR ****
      NCON=NVAR
C  **** I will make NVERT = NVAR ****
      NVERT=NVAR
C  ***** I will always assume mixture variables *****
      IMIX=1
C initialize RXVT
      do 11 IJ=1, 12000
	  RXVT(IJ)=0.0
  11  continue
C Initialize RTHETA2 vector
C
C  **** NVERT is created in SUBConvrt ****
C CREATES INITIAL MATRIX OF CONSTRAINTS (SIMPLEX)
  65  DO 70 I=1,NCON
         DO 69 J=1,NVAR+1
		 THETA(I,J)=0.0
  69	 IF(I.EQ.J)THETA(I,J)=1.0
  70     CONTINUE
C CREATES INTIAL SET OF VERTICES OF SIMPLEX
      DO 75 I=1,NVERT
	     DO 74 J=1,NVERT
       XVT(I,J)=0.0
  74   IF(I.EQ.J)XVT(I,J)=1.0
  75  CONTINUE
C CREATES MATRIX OF CONSTRAINTS
      DO 51 I=1,NVAR+1
	     DO 31 J=1,NCON2
		  KPOS=(I-1)*NCON2+J
		  THETA2(J,I)=RTHETA2(KPOS)
  31    CONTINUE
  51  CONTINUE
C  CALL SUBROUTINE DKSUB TO COMPUTE THE DK MATRIX FOR
C  THE INITIAL EXTREME VERTICES AND INITIAL CONSTRAINTS.
C
      CALL DKSUB
C
C  CHECK INITIAL CONSTRAINTS FOR UNNECESSARY OR
C  INCONSISTENT CONSTRAINTS.
C
      CALL CONCHK(ICORR)
C	  WRITE(*,*)ICORR
      IF (ICORR .GE. 0) GO TO 200
	  IFA(1)=-1
C      STOP

C
C  CONSIDER EACH CONSTRAINT IN TURN, ADDING IT TO
C  THE LIST OF WORKING CONSTRAINTS.
C
C  THIS IS THE MAIN LOOP OF THE PROGRAM--AS EACH NEW
C  CONSTRAINT IS CONSIDERED, IT IS DETERMINED EITHER
C  TO BE UNNECESSARY OR INCONSISTENT, OR THE NEW
C  VERTICES IT CREATES ARE DETERMINED.
C

 200  DO 2000 NTH2=1,NCON2
      NCON=NCON+1
         DO 215 J=1,NVAR+1
 215     THETA(NCON,J)=THETA2(NTH2,J)
C
C  COMPUTE DK VALUES FOR THE NEW CONSTRAINT
C
      DO 218 I=1,NVERT
         DKA=0.0
         DO 217 K=1,NVAR
 217     DKA=DKA+THETA(NCON,K)*XVT(I,K)
         DK(I,NCON)=DKA+THETA(NCON,NVAR+1)
 218  CONTINUE
C
C  NEW CONSTRAINT UNNECESSARY IF ALL DK'S >= ZERO.
C  NEW CONSTRAINT INCONSISTENT IF ALL DK'S <= ZERO.
C
      IFN=0
      IFP=0
      DO 220 I=1,NVERT
         IF(DK(I,NCON) .LE. -TOL1) IFN=1
         IF(DK(I,NCON) .GE. TOL1) IFP=1
 220  CONTINUE
      IF (IFN .EQ. 1 .AND. IFP .EQ. 1) GO TO 235
      IF (IFN .EQ. 0) GO TO 230
C
C  NEW CONSTRAINT INCONSISTENT--WRITE TO OUTPUT FILE
C  AND DELETE THE CONSTRAINT FROM THETA (BY REDUCING
C  THE COUNTER NCON).  THEN EXIT CONVRT.
C
C      WRITE(2,225) (THETA(NCON,K),K=1,NVAR+1)
C 225  FORMAT('0 NEW CONSTRAINT INCONSISTENT'/
C     +' ',10E12.4,:/' ',10E12.4)
      IFA(1)=-1
      NCON=NCON-1
C      STOP
C
C  NEW CONSTRAINT UNNECESSARY--WRITE TO OUTPUT FILE,
C  AND DELETE THE CONSTRAINT FROM THETA
C
 230  CONTINUE
C      WRITE(2,232) (THETA(NCON,K),K=1,NVAR+1)
C 232  FORMAT('0 NEW CONSTRAINT UNNECESSARY'/
C     +' ',10E12.4,:/' ',10E12.4)
      NCON=NCON-1
      GO TO 2000
C
C  FOR EACH NEW CONSTRAINT, CERTAIN CURRENT EXTREME
C  VERTICES HAVE NEGATIVE DK'S ASSOCIATED WITH THEM.
C  FOR EACH OF THESE, FIND OTHER EXTREME VERTICES
C  HAVING POSITIVE DK VALUES, SUCH THAT THE TWO
C  POINTS LIE ON THE SAME EDGE.
C
  235 DO 250 I=1,NVERT
         IF(DK(I,NCON) .GE. -TOL1) GO TO 250
         DO 240 J=1,NVERT
            IF(J .EQ. I) GO TO 240
            IF(DK(J,NCON) .LE. TOL1) GO TO 240
C
C  DO POINTS I AND J LIE ON THE SAME EDGE?  IF SO,
C  COMPUTE NEW VERTEX AND ADD TO THE LIST
C
            CALL EDGE(I,J)
            IF (NVERT .LE. N2) GO TO 240
			IFA(2)=-1
C            WRITE(*,239)
C            WRITE(2,239)
C 239        FORMAT('0 NUMBER OF VERTICES EXCEEDS',
C     +      ' ROW DIMENSION OF MATRIX XVT')
C            STOP
 240     CONTINUE
 250  CONTINUE
C
C  DELETE POINTS FROM XVT THAT ARE NO LONGER
C  EXTREME VERTICES, AND REMOVE CORRESPONDING
C  ROWS OF DK.
C
      J=0
      DO 300 I=1,NVERT
         IF(DK(I,NCON) .LT. -TOL1) GO TO 300
         J=J+1
         DO 275 K=1,NVAR
 275        XVT(J,K)=XVT(I,K)
         DO 280 K=1,NCON
 280        DK(J,K)=DK(I,K)
 300  CONTINUE
C
C  RESET NUMBER OF VERTICES
C
      NVERT=J
C
C  SUBROUTINE CONCHK CHECKS FOR INCONSISTENT OR
C  UNNECESSARY CONSTRAINTS
C
      CALL CONCHK(ICORR)
      IF (ICORR .GE. 0) GO TO 2000
C      WRITE(2,310)
C      WRITE(*,310)
C  310 FORMAT('0 INCONSISTENT CONSTRAINT--FATAL ERROR')
      IFA(1)=-1
C      STOP
 2000 CONTINUE
C
C  PRINT FINAL CONSTRAINT AND DK MATRICES TO
C  OUTPUT FILE
C
C      CALL FPRINT
C
C  WRITE FINAL EXTREME VERTICES TO OUTPUT FILES
C
C      WRITE(2,400)
C 400  FORMAT('0'/' EXTREME VERTICES'/)
C
C SETS DIMENSION INDICATOR = 0 FOR VERTICES
C
      DO 600 I=1,NVERT
	  IVRT(I)=0
C        WRITE(2,520) I,(XVT(I,J),J=1,NVAR)
 520     FORMAT(' ',I4,2X,10E12.4,:/' ',6X,10E12.4)
C         WRITE(1,530) (XVT(I,J),J=1,NVAR)
 530     FORMAT(' ',10E12.4)
 600  CONTINUE
C
C
 	  KNT=0
	  KNVRT=NVERT 
	  IF(NDM.EQ.0)GO TO 400
      DO 360 IDIM = 1, NDM
 290  NCM=NVAR-IDIM-IMIX
C
C  WRITE TITLE FOR OUTPUT FILE
C
C      WRITE(2,305) IDIM
C 305  FORMAT('0'/,I3,' DIMENSIONAL CENTROIDS'/' ',2X,
C     +'# VERTICES AVERAGED,',
C     +' CONSTRAINT INDICES FOR CENTROID'/)
C
C  IF NCM EQUALS ZERO, SKIP SUBROUTINE ALLNR
C
      IF (NCM .EQ. 0) GO TO 400
C
C  ZERO OUT COUNTER VARIABLE FOR NUMBER OF CENTROIDS
C
      NCENT=0
C
C  APPLIED STATISTICS ROUTINE "ALLNR" (AS 88) IS
C  USED TO PRODUCE SETS OF NCM "COMMON" CONSTRAINTS.
C  ALLNR CALLS PCESS TO COMPUTE AND STORE THE
C  CENTROIDS.
C
C	  WRITE(*,87)IDIM,KNT
C  87  FORMAT(1X,'FROM SUBCONVRT BEFORE ALLNRC IDEM=',I3,' KNT=',I3)
      CALL ALLNR(NCM,YNR,KNT,IFAULT,NCENT,CENT,CNTM)
C	  WRITE(*,89)IDIM,KNT
C  89  FORMAT(1X,'FROM SUBCONVRT AFTER ALLNRC IDEM=',I3,' KNT=',I3)
C      WRITE(2,*)' OUTPUT FROM SUBROUTINE CONVRT'
C
C SETS DIMENSION INDICATOR FOR CENTROIDS OF ORDER IDIM
C
      DO 766 IK=1,KNT
	  IVRT(IK+KNVRT)=IDIM
 766  CONTINUE
      KNVRT=KNVRT+KNT
      KNTO=KNT
C	  DO 604 KK=1,KNT
C      WRITE(2,603)IDIM,KNT,(CNTM(KNT,JJ),JJ=1,NVAR)
C 603  FORMAT(' ',2I3,2X,10E12.4,:/' ',5X,10E12.4)
C 604  CONTINUE
C
C  CHECK FOR ERRORS
C
      IF (IFAULT .EQ. 0) GO TO 350
      IFA(3)=-1
C      WRITE(2,325) NCON
C      WRITE(*,325) NCON
C 325  FORMAT('0 IN CALLING ALLNR, NCM MUST BE',
C     +' BETWEEN 1 AND NCON =',I3)
C      STOP
 350  IF (NCENT .LE. N2) GO TO 360
      IFA(4)=-1
C      WRITE(2,355)
C      WRITE(*,355)
C 355  FORMAT('0 NUMBER OF CENTROIDS EXCEEDS ROW',
C     +' DIMENSION OF MATRIX CENT')
C      STOP
 360  CONTINUE
C 
C  COMBINE COMPUTED CENTROIDS WITH VERTICES
C
      DO 586 II=1,KNT
         DO 587 KK=1, NVRR	
         XVT(II+NVERT,KK)=CNTM(II,KK)
 587  CONTINUE	
 586  CONTINUE 
      
C    
C  COMPUTE OVERALL CENTROID BY AVERAGING ALL
C  NVERT VERTICES
C
 400  DO 450 J=1,NVAR
 450     CNTRD(J)=0.0
      DO 500 I=1,NVERT
         DO 475 J=1,NVAR
 475        CNTRD(J)=CNTRD(J)+XVT(I,J)/FLOAT(NVERT)
 500  CONTINUE
      NRUN=NVERT+1+KNT
	  IVRT(NRUN)=NVAR-1
	  DO 526 I=1,NVAR
	  XVT(NRUN,I)=CNTRD(I)
 526  CONTINUE
C ROLLS THE EXTREME VERTICIES INTO A VECTOR FOR RETURN
      NVRTR=NRUN
C	  GO TO 602
C 601  NVRTR=NVERT
 602     DO 3001 II=1,NVAR
	     DO 3002 JJ=1,NVRTR
		 KPOS2=(II-1)*NVRTR+JJ
		 RXVT(KPOS2)=XVT(JJ,II)
3002     CONTINUE
3001  CONTINUE
C 
C ADDS DIMENSION INDICATOR TO RXVT
C
       DO 3003 II=1,NVRTR
	   JJ=NVAR*NVRTR+II
	   INTEG=IVRT(II)
	   FICON=FLOAT(INTEG)
C	   WRITE(*,3004)II,FICON
C3004   FORMAT(1X,' I = ',I3, ' FLOAT(IVRT(I)) = ',E10.2)
	   RXVT(JJ)=FICON
3003   CONTINUE
C  WRITE CONAEV INPUT FILE IF REQUESTED EARLIER
C
C      IF (INP .EQ. 0)GO TO 555
C      WRITE(4,610) NVAR,NCON
C 610  FORMAT(' ',I3/' ',I3)
C      DO 620 I=1,NCON
C 620     WRITE(4,530) (THETA(I,J),J=1,NVAR+1)
C      WRITE(4,630) NVERT
C 630  FORMAT(' ',I4)
C      DO 640 I=1,NVERT
C 640     WRITE(4,530) (XVT(I,J),J=1,NVAR)
 555  RETURN
      END
C  ************************************************
      SUBROUTINE DKSUB
C
C  DKSUB COMPUTES THE DK MATRIX FOR THE EXTREME
C  VERTICES IN XVT AND THE CONSTRAINTS IN THETA.
C
C  DKSUB VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,NCON,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
      DO 50 I=1,NVERT
         DO 45 J=1,NCON
            DKA=0.0
            DO 40 K=1,NVAR
               DKA=DKA+THETA(J,K)*XVT(I,K)
  40        CONTINUE
            DK(I,J)=DKA+THETA(J,NVAR+1)
  45     CONTINUE
  50  CONTINUE
      RETURN
      END
C  **************************************************
      SUBROUTINE CONCHK(ICORR)
C
C  CONCHK CHECKS FOR INCONSISTENT OR UNNECESSARY
C  CONSTRAINTS.  CONSTRAINT J IS UNNECESSARY IF
C  DK(I,J) > 0 FOR ALL VERTICES I, OR IF FOR SOME
C  OTHER CONSTRAINT K, DK(I,J)=0 IMPLIES DK(I,K)=0
C  FOR ALL VERTICES I.  CONSTRAINT J IS INCONSISTENT
C  IF DK(I,J) < 0 FOR ANY I.
C
C  CONCHK VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,NCON,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
      DIMENSION NCORR(N3)
      ICORR=0
      DO 10 K=1,NCON
  10     NCORR(K)=0
C
C  COLUMNS OF DK CONTAINING NEGATIVE VALUES
C  CORRESPOND TO INCONSISTENT CONSTRAINTS.
C  COLUMNS NOT CONTAINING A ZERO CORRESPOND
C  TO UNNECESSARY CONSTRAINTS.
C
      DO 25 J=1,NCON
         IZERO=0
         DO 15 I=1,NVERT
            IF (DK(I,J) .LT. -TOL1) GO TO 18
            IF(ABS(DK(I,J)) .LT. TOL1) IZERO=1
  15     CONTINUE
         IF (IZERO .EQ. 1) GO TO 25
C
C  FOR UNNECESSARY CONSTRAINT
C
         NCORR(J)=1
         ICORR=1
C         WRITE(2,16) (THETA(J,KI),KI=1,NVAR+1)
  16     FORMAT('0 UNNECESSARY CONSTRAINT'/
     +   ' ',10E12.4,:/' ',10E12.4)
         GO TO 25
C
C  FOR INCONSISTENT CONSTRAINT
C
  18     ICORR = -1
C         WRITE(2,20) (THETA(J,KI),KI=1,NVAR+1)
  20     FORMAT('0 INCONSISTENT CONSTRAINT'/
     +   ' ',10E12.4,:/' ',10E12.4)
         RETURN
  25  CONTINUE
C
C  CHECK TO SEE IF CONSTRAINT I IMPLIES CONSTRAINT J
C
      DO 50 I=1,NCON
         IF(NCORR(I) .EQ. 1) GO TO 50
         DO 40 J=1,NCON
            IF (I .EQ. J) GO TO 40
            DO 30 K=1,NVERT
               IF (ABS(DK(K,I)) .GT. TOL1) GO TO 30
               IF (ABS(DK(K,J)) .GT. TOL1) GO TO 40
  30        CONTINUE
C
C  CONSTRAINT I IMPLIES CONSTRAINT J.
C  FLAG CONSTRAINT I
C
            NCORR(I)=1
            ICORR=1
C            WRITE(2,35) (THETA(I,KI),KI=1,NVAR+1)
  35        FORMAT('0 CONSTRAINT IMPLIES ANOTHER,',
     +      ' AND HENCE IS UNNECESSARY'/' ',
     +      10E12.4,:/' ',10E12.4)
            GO TO 50
  40     CONTINUE
  50  CONTINUE
      IF (ICORR .EQ. 0) RETURN
C
C  CONSTRAINTS WITH NCORR(K)=1 ARE UNNECESSARY.
C  CREATE NEW DK AND THETA MATRICES BY WRITING OVER
C  COLUMNS OF DK OR ROWS OF THETA THAT CONTAIN
C  UNNECESSARY CONSTRAINTS.
C
      IC=0
      DO 100 K=1,NCON
         IF (NCORR(K) .EQ. 1) GO TO 100
         IC=IC+1
C
C  CREATE NEW DK MATRIX
C
         DO 60 I=1,NVERT
  60        DK(I,IC)=DK(I,K)
C
C  CREATE NEW THETA MATRIX
C
         DO 70 J=1,NVAR+1
  70        THETA(IC,J)=THETA(K,J)
 100  CONTINUE
C
C  SET NCON TO NEW VALUE
C
      NCON=IC
      RETURN
      END
C  *************************************************
      SUBROUTINE EDGE(IB,IA)
C
C  EDGE CHECKS TO SEE IF POINT IA (HAVING A POSITIVE
C  DK) AND POINT IB (HAVING A NEGATIVE DK) LIE ON
C  THE SAME EDGE.  IF SO, THE NEW VERTEX IS COMPUTED
C  AND ADDED TO THE LIST IN XVT, AND THE DK VALUES
C  FOR THE NEW VERTEX ARE COMPUTED.
C
C  EDGE VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,NCON,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
      DIMENSION X(N1)
C
C  POINTS IA AND IB ARE ON THE SAME EDGE IF THEY HAVE
C  AT LEAST NVAR-1-IMIX CONSTRAINT PLANES IN COMMON.
C  IF MIXTURE VARIABLES ARE INVOLVED, IMIX=1;
C  OTHERWISE IMIX=0.
C
      NCM=0
      DO 100 J=1,NCON-1
         IF(ABS(DK(IA,J)) .LT. TOL1 .AND. ABS(DK(IB,J))
     +   .LT. TOL1) NCM=NCM+1
 100  CONTINUE
      IF(NCM .LT. NVAR-1-IMIX) RETURN
C
C  POINTS ARE ON THE SAME EDGE.  CREATE NEW EXTREME
C  VERTEX WHICH IS THE INTERSECTION OF THE NEW
C  CONSTRAINT PLANE AND THIS EDGE.
C
      ALPHA=DK(IB,NCON)/(DK(IB,NCON)-DK(IA,NCON))
      DO 150 K=1,NVAR
 150     X(K)=ALPHA*XVT(IA,K) + (1.0-ALPHA)*XVT(IB,K)
C
C  ADD NEW VERTEX X TO MATRIX XVT OF EXTREME
C  VERTICES IF IT IS NOT ALREADY ON THE LIST
C
      IDUP=0
      CALL SEARCH(X,XVT,NVERT,NVAR,TOL2,IDUP)
      IF(IDUP .EQ. 1) RETURN
      NVERT=NVERT+1
      IF (NVERT .GT. N2) RETURN
      DO 200 K=1,NVAR
 200     XVT(NVERT,K)=X(K)
C
C  COMPUTE DK VALUES FOR THE NEW VERTEX
C
      DO 300 J=1,NCON
         DKA=0.0
         DO 250 K=1,NVAR
 250        DKA=DKA+THETA(J,K)*XVT(NVERT,K)
         DK(NVERT,J)=DKA+THETA(J,NVAR+1)
 300  CONTINUE
      RETURN
      END
C  *************************************************
      SUBROUTINE SEARCH(X,Y,NPT,NVAR,TOL2,IDUP)
C
C  SEARCH TAKES THE CURRENT POINT IN VECTOR X AND
C  CHECKS THROUGH THE CURRENT LIST OF POINTS IN
C  MATRIX Y TO SEE IF IT HAS ALREADY BEEN OBTAINED.
C  IF SO, IDUP IS SET TO 1.
C
C  SEARCH VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      DIMENSION X(N1),Y(N2,N1)
C
C  SKIP REST OF ROUTINE IF FIRST POINT
C
      IF (NPT .EQ. 0) RETURN
C
C  CHECK EACH OF THE ELEMENTS OF X AGAINST THOSE
C  FOR EACH POINT IN Y
C
      DO 50 I=1,NPT
         DO 40 J=1,NVAR
            IF (ABS(Y(I,J)-X(J)) .GT. TOL2) GO TO 50
  40     CONTINUE
         IDUP=1
         RETURN
  50  CONTINUE
      RETURN
      END
C  ************************************************
      SUBROUTINE FPRINT
C
C  WRITE FINAL CONSTRAINT AND DK MATRICES TO
C  OUTPUT FILE
C
C  FPRINT VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,NCON,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
C     WRITE(2,10)
  10  FORMAT('0'/' FINAL CONSTRAINT MATRIX--EACH ROW',
     +' IS A CONSTRAINT'/)
      DO 30 I=1,NCON
C        WRITE(2,20) I,(THETA(I,J),J=1,NVAR+1)
  20     FORMAT(' ',I4,2X,10E12.4,:/' ',6X,10E12.4)
  30  CONTINUE
C      WRITE(2,40)
  40  FORMAT('0'/' FINAL DK MATRIX (# VERTICES BY',
     +' # CONSTRAINTS)'/'   DK(I,J) = 0 INDICATES',
     +' THAT VERTEX I IS ON CONSTRAINT PLANE J'/)
      DO 60 I=1,NVERT
C        WRITE(2,50) I,(DK(I,J),J=1,NCON)
  50     FORMAT(' ',I4,10E12.4,:/' ',
     +   4(4X,10E12.4,:/' '))
  60  CONTINUE
      RETURN
      END
C  **************************************************
      SUBROUTINE ALLNR(R,J,KOUNT,IFAULT,NCENT,CENT,CNTRDM)
C
C  ADAPTED FROM ALGORITHM AS#88, GENTLEMAN,J.F.(1975),
C  APPLIED STATISTICS,24,374-376.
C
C  WHEN CALLED ONCE, ALLNR GENERATES ALL COMBINATIONS
C  OF R ITEMS FROM A GROUP OF N ITEMS.  EACH
C  COMBINATION (REPRESENTED AS R ORDERED INTEGERS
C  BETWEEN 1 AND N) IS STORED IN VECTOR J AND MUST BE
C  PROCESSED FOR USE IN THE CALLING ROUTINE.  KOUNT
C  IS THE NUMBER OF N--R COMBINATIONS.  IFAULT IS 1
C  IF R IS NOT BETWEEN 1 AND N; ZERO OTHERWISE.
C
C  ALLNR VERSION 1.01, JULY 1989
C  ADAPTED BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,N,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
      DIMENSION CNTRD(N1),CENT(N2,N1),CNTRDM(N2,N1)
      INTEGER R,J(N3)
      IFAULT=1
      IF(R .LT. 1 .OR. R .GT. N) RETURN
      IFAULT=0
C      KOUNT=0
      NMR=N-R
C	  WRITE(*,89)KOUNT
C  89  FORMAT(1X,' FROM ALLNRC AT CALL KOUNT=',I3)
C
C  INITIALIZE J(1) TO LOWER LIMIT SEPARATELY, SINCE
C  LOWER LIMIT FOR EACH INDEX DEPENDS ON LOWER LIMIT
C  FOR PREVIOUS INDEX
C
      I=1
      J(1)=1
C
C  INITIALIZE INDICES FOR LOOPS I+1,....,R TO
C  LOWER LIMITS
C
  1   IF(I .EQ. R) GO TO 3
      IP1=I+1
      DO 2 L=IP1,R
  2      J(L)=J(L-1)+1
  3   KOUNT=KOUNT+1
C
C  UPDATE THE COUNT (KOUNT) OF COMBINATIONS AND
C  PROCESS THE CURRENT COMBINATION
C
      CALL PCESS(R,J,NCENT,CENT,CNTRD)
	  izero1=0
	  do 551 Jk=1,nvar
 551  if(cntrd(Jk) .gt. tol1) izero1=izero1+1
      if(izero1 .gt. 0) go to 98
      KOUNT=KOUNT-1
      go to 97
  98  continue	  
	  DO 99 JJ=1,NVAR
  99  CNTRDM(KOUNT,JJ)=CNTRD(JJ)
  97  continue
C	  WRITE(2,600)NCENT,KOUNT,(CNTRDM(KOUNT,JJ),JJ=1,NVAR)
C 600  FORMAT(' ',2I3,2X,10E12.4,:/' ',5X,10E12.4)
      IF (NCENT .GT. N2) RETURN
C
C  INCREMENT THE FIRST POSSIBLE INDEX (OF LOOP 1)
C  AMONG INDICES OF LOOPS R,R-1,....,1
C
      I=R
  5   IF(J(I) .LT. NMR+I) GO TO 6
      I=I-1
C
C  RETURN AFTER ALL INDICES HAVE ACHIEVED THEIR
C  UPPER LIMITS
C
      IF(I .LE. 0) RETURN
      GO TO 5
  6   J(I)=J(I)+1
      GO TO 1
      END
C  **************************************************
      SUBROUTINE PCESS(NCM,YNR,NCENT,CENT,CNTRD)
C
C  PCESS "PROCESSES" EACH VECTOR YNR OUTPUT FROM
C  SUBROUTINE ALLNR.  EACH VECTOR YNR IS ONE POSSIBLE
C  WAY TO CHOOSE NCM COMMON CONSTRAINT PLANES, AND 
C  PCESS COMPUTES THE AEV CENTROID (IF THERE IS
C  ONE) FOR THE SET OF COMMON CONSTRAINT PLANES
C  SPECIFIED BY YNR.
C
C  PCESS VERSION 1.01, JULY 1989
C  WRITTEN BY G.F. PIEPEL
C
      PARAMETER (N1=12,N2=1000,N3=45)
      COMMON NVAR,NVERT,NCON,THETA(N3,N1),XVT(N2,N1),
     +DK(N2,N3),IMIX,TOL1,TOL2
      INTEGER YNR(N3)
      DIMENSION CNTRD(N1),CENT(N2,N1)
      ICNT=0
      DO 50 J=1,NVAR
 50      CNTRD(J)=0.0
C
C  CHECK TO SEE IF EACH EXTREME VERTEX HAS DK VALUES
C  EQUAL TO ZERO FOR THE SET OF COMMON CONSTRAINT
C  PLANES SPECIFIED BY VECTOR YNR.
C
      DO 500 IA=1,NVERT
         DO 200 J=1,NCM
            JA=YNR(J)
            ABSDK=ABS(DK(IA,JA))
            IF (ABSDK .GT. TOL1) GO TO 500
 200     CONTINUE
         ICNT=ICNT+1
         DO 250 J=1,NVAR
 250        CNTRD(J)=CNTRD(J)+XVT(IA,J)
 500  CONTINUE
C
C  IF THE NUMBER OF EXTREME VERTICES SATISFYING THE
C  NCM COMMON CONSTRAINT PLANES IS NOT AT LEAST
C  (NDIM+1), DO NOT COMPUTE A CENTROID, SINCE THE
C  DIMENSION IS NOT CORRECT.
C
      NDIM=NVAR-NCM-IMIX
      IF (ICNT .LT. NDIM+1) RETURN
      DO 550 J=1,NVAR
 550     CNTRD(J)=CNTRD(J)/FLOAT(ICNT)
C
C  CHECK TO SEE IF THIS CENTROID HAS BEEN FOUND
C  ALREADY--IF SO, RETURN.  IF NOT, ADD ONE TO
C  CENTROID COUNTER AND PUT CENTROID IN MATRIX CENT.
C
      IDUP=0
      CALL SEARCH(CNTRD,CENT,NCENT,NVAR,TOL2,IDUP)
      IF (IDUP .EQ. 1) RETURN
      NCENT=NCENT+1
      IF (NCENT .GT. N2) RETURN
      DO 575 J=1,NVAR
 575     CENT(NCENT,J)=CNTRD(J)
C
C  WRITE OUT COMPUTED AEV CENTROID, THE NUMBER OF
C  POINTS ICNT THAT WERE AVERAGED, AND THE INDICES
C  OF THE COMMON CONSTRAINTS.
C
C      WRITE(2,600) NCENT,(CNTRD(J),J=1,NVAR)
C 600  FORMAT(' ',I3,2X,10E12.4,:/' ',5X,10E12.4)
C      WRITE(2,601) ICNT,(YNR(K),K=1,NCM)
C 601  FORMAT(' ',7X,I4,3X,38I3)
C      WRITE(1,610) (CNTRD(J),J=1,NVAR)
C 610  FORMAT(' ',10E12.4)
      RETURN
      END