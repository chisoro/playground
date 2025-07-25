DEF SHARED STREAM a.
DEF SHARED STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
/*DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.*/
DEF VAR X         AS INT.
DEF VAR j         AS INT.
DEF VAR wsFile AS CHAR.
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
DEF SHARED BUFFER bfdbRecH FOR dbRecH.
FIND FIRST SIMCTR NO-LOCK.
FIND FIRST bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND  bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1  EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE bfdbRecH  THEN DO:
    wsFile = simctr.repDir + "recrep" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv". 
    wsTitle = "RECEIPT COLLECTION REPORT FOR RECEIPTING DATES STARTING ON: " + string(wsDate) + "ENDING ON:" + string(wsDate1).
    OUTPUT STREAM b TO VALUE(wsFile).  
    RUN Report.ip.
    wsfile =  wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.             
ELSE IF NOT AVAILABLE bfdbRecH THEN
        MESSAGE "No receipts for the given date range and operators" VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.
RETURN.

PROCEDURE Report.ip.
   EXPORT STREAM b DELIMITER ',' wsTitle.
   EXPORT STREAM b DELIMITER ',' "COLLECTION BY INCOME CODES".
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ','  "INCOME CODE"  "DESCRIPTION"  "AMOUNT".
   FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 AND bfdbRecH.Recstat <> "C"
        NO-LOCK BREAK BY bfdbRecH.rCode:
       IF FIRST-OF(bfdbRecH.rCode) THEN
          ASSIGN wsTotal = 0.
           wsTotal = wsTotal +(bfdbRecH.Amount * bfdbRecH.decRate).
       IF LAST-OF(bfdbRecH.rCode) THEN DO:
           wsVar = bfdbRecH.rCode. 
           FIND FIRST dbRcod WHERE dbRCod.rCode =  bfdbRecH.rCode NO-LOCK NO-ERROR.
            EXPORT STREAM b DELIMITER ','  wsVar  dbRCod.descrip  wsTotal.
         END.      
   END.
  EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ',' "COLLECTION BY PAYMENT METHODS".
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ','  "METHOD"  "AMOUNT".
   FOR EACH bfdbRecH where bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 and bfdbRecH.Recstat <> "C"
        NO-LOCK BREAK BY bfdbRecH.Paytype:
       IF FIRST-OF(bfdbRecH.Paytype) THEN
           WStOTAL = 0.
       wsTotal = wsTotal + bfdbRecH.Amount.
       IF LAST-OF(bfdbRecH.Paytype) THEN DO:
           FIND FIRST dbPay WHERE dbPay.Paytype = bfdbRecH.Paytype NO-LOCK NO-ERROR.
           FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
            EXPORT STREAM b DELIMITER ',' dbPay.descrip  wsTotal.
           
       END.  
   END.
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ',' "COLLECTION BY CURRENCY".
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ','  "CURRENCY" "AMOUNT".
   FOR EACH tblforex:
        IF NOT CAN-FIND(FIRST bfdbRecH WHERE  bfdbRecH.txtCur = tblForex.txtCur) THEN NEXT.
        ASSIGN wsTotal = 0.
        FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND  bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1
            AND bfdbRecH.txtCur = tblForex.txtCur AND bfdbRecH.Recstat <> "C":
            wsTotal = wsTotal + bfdbRecH.Amount.
        END.
        IF wsTotal <> 0 THEN DO:
             EXPORT STREAM b DELIMITER ','  tblForex.txtCur  wsTotal.
            
        END.
   END.
END.

