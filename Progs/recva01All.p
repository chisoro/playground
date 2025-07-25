DEF SHARED STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
/*DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.*/
DEF VAR X         AS INT.
DEF VAR j         AS INT.
DEF  SHARED VAR wsDate      LIKE dbRecH.RecDate.
DEF SHARED VAR wsOp        LIKE dbRecH.OpCode.
DEF  SHARED VAR wsDate1      LIKE dbRecH.RecDate.
DEF  SHARED VAR wsOp1        LIKE dbRecH.OpCode.
DEF  SHARED VAR wsValid     AS LOGICAL INITIAL YES.
DEF  SHARED VAR wsStatus   AS CHAR FORM "X(20)".
DEF  SHARED VAR wsVar       AS CHAR FORM "X(20)".
DEF  SHARED VAR wsamt      AS DEC  FORM "ZZZZZZZZZZZZZ9.99-".
DEF  SHARED VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZZZZ9.99-".
DEF VAR wsCancel LIKE wsAmt.
DEF VAR wsValAmt LIKE wsAmt.
DEF SHARED VAR wsTitle AS CHAR FORM "x(80)".
DEF TEMP-TABLE bfdbRecH LIKE  dbRecH.
FORM /*bfdbRecH.RecNo      LABEL "RECEIPT"*/
     bfdbRecH.SeqNo      LABEL "INCOME CODE" FORM "ZZZZZZZ"
     /*bfdbRecH.Account    LABEL "ACCOUNT"*/
     bfdbRecH.Descrip      LABEL "DESCRIPTION" FORM "X(20)"
    /* bfdbRecH.Paytype    LABEL "PCODE"
     bfdbRecH.rCode      LABEL "ICODE"
     BFdbRecH.Amount     LABEL "AMOUNT" FORM "ZZZZZZZZZZZZZ9.99-"*/
     wsTotal            LABEL "AMOUNT"
     /*wsStatus           LABEL "COMMENT"*/
    HEADER wsTitle AT 20
       skip(1) "             RECEIPT COLLECTION REPORT FOR RECEIPTING DATES STARTING ON: " wsDate "ENDING ON:" wsDate1
    "Page: " AT 90 PAGE-NUMBER(a) SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.


FOR each dbRecH WHERE dbRecH.OpCode >= wsOp AND  dbRecH.OpCode <= wsOp1 AND dbRecH.RecDate >= wsDate AND dbRecH.RecDate <= wsDate1:
    
    CREATE bfdbRecH.
     ASSIGN bfdbRecH.Account = dbRecH.Account 
         bfdbRecH.Amount =   dbRecH.Amount
         bfdbRecH.contype = dbRecH.contype
         bfdbRecH.decRate =  dbRecH.decRate
         bfdbRecH.Descrip = dbRecH.Descrip
         bfdbRecH.OpCode = dbRecH.OpCode
         bfdbRecH.Paytype = dbRecH.Paytype
         bfdbRecH.rCode = dbRecH.rCode 
         bfdbRecH.RecDate =  dbRecH.RecDate
         bfdbRecH.RecNo =  dbRecH.RecNo 
         bfdbRecH.RecStat = dbRecH.RecStat
         bfdbRecH.Ref  =  dbRecH.Ref
         bfdbRecH.SeqNo =  dbRecH.SeqNo 
         bfdbRecH.txtCur = dbRecH.txtCur.
END.
FOR each dbRec WHERE dbRec.OpCode >= wsOp AND  dbRec.OpCode <= wsOp1 AND dbRec.RecDate >= wsDate AND dbRec.RecDate <= wsDate1:
   
    CREATE bfdbRecH.
     ASSIGN bfdbRecH.Account = dbRec.Account 
         bfdbRecH.Amount =   dbRec.Amount
         bfdbRecH.contype = dbRec.contype
         bfdbRecH.decRate =  dbRec.decRate
         bfdbRecH.Descrip = dbRec.Descrip
         bfdbRecH.OpCode = dbRec.OpCode
         bfdbRecH.Paytype = dbRec.Paytype
         bfdbRecH.rCode = dbRec.rCode 
         bfdbRecH.RecDate =  dbRec.RecDate
         bfdbRecH.RecNo =  dbRec.RecNo 
         bfdbRecH.RecStat = dbRec.RecStat
         bfdbRecH.Ref  =  dbRec.Ref
         bfdbRecH.SeqNo =  dbRec.SeqNo 
         bfdbRecH.txtCur = dbRec.txtCur.
END.


FIND FIRST bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND  bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1  EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE bfdbRecH  THEN DO:
       {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
END.             
ELSE IF NOT AVAILABLE bfdbRecH THEN
        MESSAGE "No receipts for the given date range and operators" VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.
RETURN.

PROCEDURE Report.ip.
  /* FOR EACH bfdbRecH where bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate  AND bfdbRecH.RecDate <= wsDate1 NO-LOCK BREAK BY RecNo :
       wsStatus = "".
       wsVar = bfdbRecH.rcode.
       IF   bfdbRecH.Recstat = "C" THEN
           wsStatus = "Cancelled".
       ELSE DO:
           IF bfdbRecH.contype = "C" THEN DO:
                FIND FIRST dbcmf WHERE dbcmf.dbAcc = bfdbRecH.Account NO-LOCK NO-ERROR.
                IF NOT AVAILABLE dbcmf THEN
                    ASSIGN wsStatus = "Invalid Account"
                           wsVar    = ""
                           wsValid = NO.
                ELSE wsVar = dbcmf.Name.
           END.
           ELSE IF bfdbRecH.contype = "L" THEN DO:
                FIND FIRST hsecmf WHERE hsecmf.dbAcc = bfdbRecH.Account NO-LOCK NO-ERROR.
                IF NOT AVAILABLE hsecmf THEN
                    ASSIGN wsStatus = "Invalid Account"
                           wsVar    = ""
                           wsValid = NO.
                ELSE wsVar = hsecmf.Name.
           END.
           ELSE IF bfdbRecH.contype = "V" THEN DO:
               FIND FIRST glmf WHERE glmf.acct = bfdbRecH.Account NO-LOCK NO-ERROR.
                IF NOT AVAILABLE glmf THEN
                    ASSIGN wsStatus = "Invalid Account"
                          /* wsVar    = "" */
                           wsValid = NO.
                /*ELSE wsVar = glmf.DESCRIPTION. */
           END.
           FIND FIRST dbRCod WHERE dbRCod.rCode = bfdbRecH.rCode NO-LOCK NO-ERROR.
             IF NOT AVAILABLE dbRCod THEN
                    ASSIGN wsStatus = wsStatus + "/Income" WHEN wsStatus <> ""
                           wsStatus = "Invalid Income"   WHEN wsStatus = ""
                           wsValid = NO.
          FIND FIRST dbPay WHERE dbPay.Paytype = bfdbRecH.Paytype NO-LOCK NO-ERROR.
             IF NOT AVAILABLE dbPay THEN
                    ASSIGN wsStatus = wsStatus + "/Pay Method" WHEN wsStatus <> ""
                           wsStatus = "Invalid Pay Method"   WHEN wsStatus = ""
                           wsValid = NO.
       END.
           /* print transactions */
       IF FIRST-OF(bfdbRecH.recno) THEN DO:
             wsTotal = 0.
             DISPLAY STREAM a bfdbRecH.recno WITH FRAME frm-rpt.
       END.
       ASSIGN wsTotal = wsTotal + bfdbRecH.Amount
              wsValAmt = wsValAmt + (bfdbRecH.Amount * bfdbRecH.decRate) WHEN bfdbRecH.Recstat <> "C"
               wsCancel = wsCancel + (bfdbRecH.Amount * bfdbRecH.decRate) WHEN bfdbRecH.Recstat = "C".
       DISPLAY STREAM a bfdbRecH.Descrip bfdbRecH.Paytype /*wsVar @ */ bfdbRecH.rCode bfdbRecH.SeqNo bfdbRecH.Account 
               bfdbRecH.Amount wsStatus WITH FRAME frm-rpt.
       /* DOWN STREAM a WITH FRAME frm-rpt. */
       IF LAST-OF(bfdbRecH.recno) THEN
           DISPLAY STREAM a wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
   END.
   UNDERLINE STREAM a wsTotal WITH FRAME frm-rpt.
   DOWN  5 STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "VALID RECEIPTS" @  bfdbRecH.Descrip wsValAmt @ wsTotal WITH FRAME frm-rpt.
   DOWN  STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "CANCELLED RECEIPTS" @  bfdbRecH.Descrip wsCancel @ wsTotal WITH FRAME frm-rpt.
   PAGE STREAM a. */
   DISPLAY STREAM a "COLLECTION BY INCOME CODES REPORTING CURRENCY" @ bfdbRecH.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt.
   FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 AND bfdbRecH.Recstat <> "C"
        NO-LOCK BREAK BY bfdbRecH.rCode:
       IF FIRST-OF(bfdbRecH.rCode) THEN
          ASSIGN wsTotal = 0.
           wsTotal = wsTotal +(bfdbRecH.Amount * bfdbRecH.decRate).
       IF LAST-OF(bfdbRecH.rCode) THEN DO:
           wsVar = bfdbRecH.rCode. 
           FIND FIRST dbRcod WHERE dbRCod.rCode =  bfdbRecH.rCode NO-LOCK NO-ERROR.
           DISPLAY STREAM a wsVar @  bfdbRecH.SeqNo dbRCod.descrip @ bfdbRecH.descrip wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.      
   END.
   UNDERLINE STREAM a wsTotal WITH FRAME frm-rpt.
   DOWN  5 STREAM a WITH FRAME frm-rpt.
  DISPLAY STREAM a "COLLECTION BY INCOME CODES BY CURRENCY" @ bfdbRecH.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt.
   FOR EACH tblForex:
       DISPLAY STREAM a tblForex.txtCur @ bfdbRecH.Descrip WITH FRAME frm-rpt.
       UNDERLINE STREAM a bfdbRecH.Descrip WITH FRAME frm-rpt.
       FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 AND bfdbRecH.Recstat <> "C" AND bfdbRecH.txtCur = tblForex.txtCur 
        NO-LOCK BREAK BY bfdbRecH.rCode:
       IF FIRST-OF(bfdbRecH.rCode) THEN
          ASSIGN wsTotal = 0.
           wsTotal = wsTotal +(bfdbRecH.Amount ).
       IF LAST-OF(bfdbRecH.rCode) THEN DO:
           wsVar = bfdbRecH.rCode. 
           FIND FIRST dbRcod WHERE dbRCod.rCode =  bfdbRecH.rCode NO-LOCK NO-ERROR.
           DISPLAY STREAM a  wsVar @  bfdbRecH.SeqNo dbRCod.descrip @ bfdbRecH.descrip wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.      
    END.
    DOWN 2 STREAM a WITH FRAME frm-rpt.
   END.
   /*UNDERLINE STREAM a wsTotal WITH FRAME frm-rpt.*/
   DOWN  5 STREAM a WITH FRAME frm-rpt.


   DISPLAY STREAM a "COLLECTION BY PAYMENT METHODS" @ bfdbRecH.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt.
   FOR EACH bfdbRecH where bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 and bfdbRecH.Recstat <> "C"
        NO-LOCK BREAK BY bfdbRecH.Paytype:
       IF FIRST-OF(bfdbRecH.Paytype) THEN
           WStOTAL = 0.
       wsTotal = wsTotal + bfdbRecH.Amount.
       IF LAST-OF(bfdbRecH.Paytype) THEN DO:
           FIND FIRST dbPay WHERE dbPay.Paytype = bfdbRecH.Paytype NO-LOCK NO-ERROR.
           FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
           DISPLAY STREAM a dbPay.descrip @ bfdbRecH.Descrip wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.  
   END.
   DOWN  5 STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "COLLECTION BY CURRENCY" @ bfdbRecH.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt.
   FOR EACH tblforex:
        IF NOT CAN-FIND(FIRST bfdbRecH WHERE  bfdbRecH.txtCur = tblForex.txtCur) THEN NEXT.
        ASSIGN wsTotal = 0.
        FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND  bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1
            AND bfdbRecH.txtCur = tblForex.txtCur AND bfdbRecH.Recstat <> "C":
            wsTotal = wsTotal + bfdbRecH.Amount.
        END.
        IF wsTotal <> 0 THEN DO:
            DISPLAY STREAM a tblForex.txtCur @ bfdbRecH.Descrip wsTotal WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
   END.
END.

