DEF  SHARED STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
/*DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.*/
DEF VAR X         AS INT.
DEF VAR j         AS INT.
DEF  SHARED VAR wsDate      LIKE dbRec.RecDate.
DEF  SHARED VAR wsOp        LIKE dbRec.OpCode.
DEF  SHARED VAR wsValid     AS LOGICAL INITIAL YES.
DEF  SHARED VAR wsStatus   AS CHAR FORM "X(20)".
DEF  SHARED VAR wsVar       AS CHAR FORM "X(20)".
DEF  SHARED VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF  SHARED VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsCancel LIKE wsAmt.
DEF VAR wsValAmt LIKE wsAmt.
DEF SHARED VAR wsTitle AS CHAR FORM "x(80)".
DEF SHARED BUFFER bfdbRec FOR dbRec.
DEF VAR wsFile  AS CHAR FORM "X(20)".
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST dbRecCtr WHERE dbRecCtr.usercode = wsOp AND dbRecCtr.RecDate = wsDate EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE dbRecCtr AND dbRecCtr.EOD = YES THEN DO:
     wsFile = TRIM(simctr.repDir) + "Rec" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + ".csv".
     OUTPUT STREAM a TO VALUE(wsFile).
     RUN Report.ip.
END.             
ELSE IF AVAILABLE dbRecCtr AND dbRecCtr.EOD = NO THEN 
       MESSAGE "End-of-Day not done for these receipts" VIEW-AS ALERT-BOX.
     ELSE IF NOT AVAILABLE dbRecCtr THEN
        MESSAGE "No receipts for this Cashier on the given date" VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.
RETURN.

PROCEDURE Report.ip.
    EXPORT STREAM a DELIMITER ','   "RECEIPT" "SEQ" "ACCOUNT" "DESCRIPTION" "PCODE" "ICODE"
                 "REFERENCE" "AMOUNT" "RECPT TOTAL" "CUR" "RATE" "STATUS". 
   FOR EACH bfdbRec where bfdbRec.OpCode = wsOp AND bfdbRec.RecDate = wsDate NO-LOCK BREAK BY RecNo BY seq:
       wsStatus = "".
       wsVar = bfdbRec.rcode.
       IF   bfdbRec.Recstat = "C" THEN
           wsStatus = "Cancelled".
       ELSE DO:
           IF bfdbRec.contype = "C" THEN DO:
                FIND FIRST dbcmf WHERE dbcmf.dbAcc = bfdbRec.Account NO-LOCK NO-ERROR.
                IF NOT AVAILABLE dbcmf THEN
                    ASSIGN wsStatus = "Invalid Account"
                           wsVar    = ""
                           wsValid = NO.
                ELSE wsVar = dbcmf.Name.
           END.
           ELSE IF bfdbRec.contype = "L" THEN DO:
                FIND FIRST hsecmf WHERE hsecmf.dbAcc = bfdbRec.Account NO-LOCK NO-ERROR.
                IF NOT AVAILABLE hsecmf THEN
                    ASSIGN wsStatus = "Invalid Account"
                           wsVar    = ""
                           wsValid = NO.
                ELSE wsVar = hsecmf.Name.
           END.
           ELSE IF bfdbRec.contype = "V" THEN DO:
               FIND FIRST glmf WHERE glmf.acct = bfdbRec.Account NO-LOCK NO-ERROR.
                IF NOT AVAILABLE glmf THEN
                    ASSIGN wsStatus = "Invalid Account"
                          /* wsVar    = "" */
                           wsValid = NO.
                /*ELSE wsVar = glmf.DESCRIPTION. */
           END.
           FIND FIRST dbRCod WHERE dbRCod.rCode = bfdbRec.rCode NO-LOCK NO-ERROR.
             IF NOT AVAILABLE dbRCod THEN
                    ASSIGN wsStatus = wsStatus + "/Income" WHEN wsStatus <> ""
                           wsStatus = "Invalid Income"   WHEN wsStatus = ""
                           wsValid = NO.
          FIND FIRST dbPay WHERE dbPay.Paytype = bfdbRec.Paytype NO-LOCK NO-ERROR.
             IF NOT AVAILABLE dbPay THEN
                    ASSIGN wsStatus = wsStatus + "/Pay Method" WHEN wsStatus <> ""
                           wsStatus = "Invalid Pay Method"   WHEN wsStatus = ""
                           wsValid = NO.
       END.
           /* print transactions */
       IF FIRST-OF(bfdbrec.recno) THEN DO:
             wsTotal = 0.
            /* DISPLAY STREAM a bfdbrec.recno WITH FRAME frm-rpt. */
       END.
       ASSIGN wsTotal = wsTotal + bfdbRec.Amount
              wsValAmt = wsValAmt + (bfdbRec.Amount * bfdbRec.decRate) WHEN bfdbRec.Recstat <> "C"
               wsCancel = wsCancel + (bfdbRec.Amount * bfdbRec.decRate) WHEN bfdbRec.Recstat = "C".
       IF FIRST-OF(bfdbrec.recno) THEN
          EXPORT STREAM a DELIMITER ','  bfdbRec.RecNo bfdbRec.SeqNo bfdbRec.Account bfdbRec.Descrip 
             bfdbRec.Paytype bfdbRec.rCode bfdbRec.Ref BFdbRec.Amount "" bfdbRec.txtCur bfdbRec.decRate wsStatus.
       ELSE IF LAST-OF(bfdbrec.recno) THEN
           EXPORT STREAM a DELIMITER ',' "" bfdbRec.SeqNo bfdbRec.Account bfdbRec.Descrip 
             bfdbRec.Paytype bfdbRec.rCode bfdbRec.Ref BFdbRec.Amount wsTotal bfdbRec.txtCur bfdbRec.decRate  wsStatus.
       ELSE      
            EXPORT STREAM a  DELIMITER ',' " " bfdbRec.SeqNo bfdbRec.Account bfdbRec.Descrip 
             bfdbRec.Paytype bfdbRec.rCode bfdbRec.Ref BFdbRec.Amount " " bfdbRec.txtCur bfdbRec.decRate wsStatus.
   END.
   OUTPUT STREAM a CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile). 
   /* UNDERLINE STREAM a wsTotal WITH FRAME frm-rpt.
   DOWN  5 STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "VALID RECEIPTS" @  bfdbRec.Descrip wsValAmt @ wsTotal WITH FRAME frm-rpt.
   DOWN  STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "CANCELLED RECEIPTS" @  bfdbRec.Descrip wsCancel @ wsTotal WITH FRAME frm-rpt.
   PAGE STREAM a.
   DISPLAY STREAM a "COLLECTION BY INCOME CODES" @ bfdbRec.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt. */
   /*FOR EACH bfdbRec WHERE bfdbRec.OpCode = wsOp AND bfdbRec.RecDate = wsDate AND bfdbRec.Recstat <> "C"
        NO-LOCK BREAK BY bfdbRec.rCode:
       IF FIRST-OF(bfdbRec.rCode) THEN
          ASSIGN wsTotal = 0.
           wsTotal = wsTotal +(bfdbRec.Amount * bfdbRec.decRate).
       IF LAST-OF(bfdbRec.rCode) THEN DO:
           wsVar = bfdbRec.rCode. 
           DISPLAY STREAM a wsVar @ bfdbRec.descrip wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.      
   END.
   UNDERLINE STREAM a wsTotal WITH FRAME frm-rpt.
   DOWN  5 STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "COLLECTION BY PAYMENT METHODS" @ bfdbRec.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt.
   FOR EACH bfdbRec where bfdbRec.OpCode = wsOp AND bfdbRec.RecDate = wsDate and bfdbRec.Recstat <> "C"
        NO-LOCK BREAK BY bfdbRec.Paytype:
       IF FIRST-OF(bfdbRec.Paytype) THEN
           WStOTAL = 0.
       wsTotal = wsTotal + bfdbRec.Amount.
       IF LAST-OF(bfdbRec.Paytype) THEN DO:
           FIND FIRST dbPay WHERE dbPay.Paytype = bfdbRec.Paytype NO-LOCK NO-ERROR.
           FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
           DISPLAY STREAM a dbPay.descrip @ bfdbRec.Descrip wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.  
   END.
   DOWN  5 STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "COLLECTION BY CURRENCY" @ bfdbRec.descrip WITH FRAME frm-rpt.
   DOWN 2 STREAM a WITH FRAME frm-rpt.
   FOR EACH tblforex:
        IF NOT CAN-FIND(FIRST bfDbRec WHERE  bfDbRec.txtCur = tblForex.txtCur) THEN NEXT.
        ASSIGN wsTotal = 0.
        FOR EACH bfDbRec WHERE bfDbRec.OpCode = wsOp AND bfDbRec.RecDate = wsDate
            AND bfDbRec.txtCur = tblForex.txtCur AND bfdbRec.Recstat <> "C":
            wsTotal = wsTotal + bfdbRec.Amount.
        END.
        IF wsTotal <> 0 THEN DO:
            DISPLAY STREAM a tblForex.txtCur @ bfdbRec.Descrip wsTotal WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
   END.
   ASSIGN dbRecCtr.VALIDATE = YES WHEN wsValid = YES.*/
END PROCEDURE. 

