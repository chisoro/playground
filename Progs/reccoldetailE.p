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
DEF VAR wsWard LIKE dbWrd.ward.
DEF VAR wsName LIKE dbcmf.NAME.
DEF SHARED VAR wsTitle AS CHAR FORM "x(80)".
DEF TEMP-TABLE bfdbRecH LIKE  dbRecH.
DEF TEMP-TABLE bfdbRecH1 NO-UNDO
    FIELD ward LIKE wsWard
    FIELD    wDescrip LIKE dbwrd.descrip
    FIELD      NAME LIKE wsName
    FIELD      cDescrip LIKE  dbRCod.descrip
    FIELD      Account LIKE dbRecH.Account 
    FIELD      Amount LIKE   dbRecH.Amount
    FIELD      contype LIKE dbRecH.contype
    FIELD     decRate LIKE dbRecH.decRate
    FIELD      Descrip LIKE dbRecH.Descrip
    FIELD      OpCode LIKE dbRecH.OpCode
    FIELD      Paytype LIKE dbRecH.Paytype
    FIELD      rCode LIKE dbRecH.rCode 
    FIELD      RecDate LIKE  dbRecH.RecDate
    FIELD      RecNo LIKE  dbRecH.RecNo 
    FIELD      RecStat LIKE dbRecH.RecStat
    FIELD      Ref  LIKE  dbRecH.Ref
    FIELD      SeqNo LIKE  dbRecH.SeqNo 
    FIELD      txtCur LIKE dbRecH.txtCur.
    

FOR each dbRecH WHERE dbRecH.OpCode >= wsOp AND  dbRecH.OpCode <= wsOp1 AND dbRecH.RecDate >= wsDate AND dbRecH.RecDate <= wsDate1 AND dbRecH.contype <> "V":
    
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
       
     IF  dbRecH.contype = "C" THEN DO:
           FIND FIRST dbcmf WHERE dbRecH.account = dbcmf.dbAcc NO-ERROR.
           IF AVAILABLE dbcmf THEN DO:
               wsWard = dbcmf.ward.
               wsName = dbcmf.NAME.
           END.
       END.
       ELSE IF dbRecH.contype = "L" THEN DO:
             FIND FIRST hsecmf WHERE dbRecH.account = hsecmf.dbAcc NO-ERROR.
                 IF AVAILABLE hsecmf THEN DO:
                     wsWard = hsecmf.ward.
                     wsName = hsecmf.NAME.
                 END.
       END.
       wsVar = dbRecH.rCode.
        FIND FIRST dbwrd WHERE dbwrd.ward = wsWard NO-ERROR.
        FIND FIRST dbRCod WHERE dbRCod.rCode = dbRecH.rCode  NO-ERROR. 
     
      CREATE bfdbRecH1.
     ASSIGN 
         bfdbRecH1.ward =  wsWard
         bfdbRecH1.wDescrip = dbwrd.descrip
         bfdbRecH1.NAME = wsName
         bfdbRecH1.cDescrip =  dbRCod.descrip
         bfdbRecH1.Account = dbRecH.Account 
         bfdbRecH1.Amount =   dbRecH.Amount
         bfdbRecH1.contype = dbRecH.contype
         bfdbRecH1.decRate =  dbRecH.decRate
         bfdbRecH1.Descrip = dbRecH.Descrip
         bfdbRecH1.OpCode = dbRecH.OpCode
         bfdbRecH1.Paytype = dbRecH.Paytype
         bfdbRecH1.rCode = dbRecH.rCode 
         bfdbRecH1.RecDate =  dbRecH.RecDate
         bfdbRecH1.RecNo =  dbRecH.RecNo 
         bfdbRecH1.RecStat = dbRecH.RecStat
         bfdbRecH1.Ref  =  dbRecH.Ref
         bfdbRecH1.SeqNo =  dbRecH.SeqNo 
         bfdbRecH1.txtCur = dbRecH.txtCur.

END.
FOR each dbRec WHERE dbRec.OpCode >= wsOp AND  dbRec.OpCode <= wsOp1 AND dbRec.RecDate >= wsDate AND dbRec.RecDate <= wsDate1 AND dbRecH.contype <> "V":
   
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

         IF  dbRec.contype = "C" THEN DO:
           FIND FIRST dbcmf WHERE dbRec.account = dbcmf.dbAcc NO-ERROR.
           IF AVAILABLE dbcmf THEN DO:
               wsWard = dbcmf.ward.
               wsName = dbcmf.NAME.
           END.
       END.
       ELSE IF dbRec.contype = "L" THEN DO:
             FIND FIRST hsecmf WHERE dbRec.account = hsecmf.dbAcc NO-ERROR.
                 IF AVAILABLE hsecmf THEN DO:
                     wsWard = hsecmf.ward.
                     wsName = hsecmf.NAME.
                 END.
       END.
       wsVar = dbRecH.rCode.
        FIND FIRST dbwrd WHERE dbwrd.ward = wsWard NO-ERROR.
        FIND FIRST dbRCod WHERE dbRCod.rCode = dbRec.rCode  NO-ERROR. 
     
      CREATE bfdbRecH1.
     ASSIGN 
         bfdbRecH1.ward =  wsWard
         bfdbRecH1.wDescrip = dbwrd.descrip
         bfdbRecH1.NAME = wsName
         bfdbRecH1.cDescrip =  dbRCod.descrip
         bfdbRecH1.Account = dbRec.Account 
         bfdbRecH1.Amount =   dbRec.Amount
         bfdbRecH1.contype = dbRec.contype
         bfdbRecH1.decRate =  dbRec.decRate
         bfdbRecH1.Descrip = dbRec.Descrip
         bfdbRecH1.OpCode = dbRec.OpCode
         bfdbRecH1.Paytype = dbRec.Paytype
         bfdbRecH1.rCode = dbRec.rCode 
         bfdbRecH1.RecDate =  dbRec.RecDate
         bfdbRecH1.RecNo =  dbRec.RecNo 
         bfdbRecH1.RecStat = dbRec.RecStat
         bfdbRecH1.Ref  =  dbRec.Ref
         bfdbRecH1.SeqNo =  dbRec.SeqNo 
         bfdbRecH1.txtCur = dbRec.txtCur.
END.

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
   EXPORT STREAM b DELIMITER ',' "COLLECTION BY INCOME CODES REPORTING CURRENCY". 
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ','  "INCOME CODE"  "DESCRIPTION"  "AMOUNT".
   FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 AND bfdbRecH.Recstat <> "C" AND bfdbRecH.contype <> "V"
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
  EXPORT STREAM b DELIMITER ',' "COLLECTION BY INCOME CODES BY CURRENCY".
 EXPORT STREAM b DELIMITER ',' "".
   FOR EACH tblForex:
       EXPORT STREAM b DELIMITER ','   tblForex.txtCur.
       EXPORT STREAM b DELIMITER ','  "METHOD"  "DESCRIPTION" "AMOUNT".
       FOR EACH bfdbRecH WHERE bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 AND bfdbRecH.Recstat <> "C" AND bfdbRecH.txtCur = tblForex.txtCur AND bfdbRecH.contype <> "V"
        NO-LOCK BREAK BY bfdbRecH.rCode:
       IF FIRST-OF(bfdbRecH.rCode) THEN
          ASSIGN wsTotal = 0.
           wsTotal = wsTotal +(bfdbRecH.Amount ).
       IF LAST-OF(bfdbRecH.rCode) THEN DO:
           wsVar = bfdbRecH.rCode. 
           FIND FIRST dbRcod WHERE dbRCod.rCode =  bfdbRecH.rCode NO-LOCK NO-ERROR.
          EXPORT STREAM b DELIMITER ','  wsVar  dbRCod.descrip  wsTotal.
        END.      
    END.
     EXPORT STREAM b DELIMITER ',' "".
   END.


  EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ',' "COLLECTION BY PAYMENT METHODS".
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ','  "METHOD"  "AMOUNT".
   FOR EACH bfdbRecH where bfdbRecH.OpCode >= wsOp AND bfdbRecH.OpCode <= wsOp1 AND bfdbRecH.RecDate >= wsDate AND bfdbRecH.RecDate <= wsDate1 and bfdbRecH.Recstat <> "C" AND bfdbRecH.contype <> "V"
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
            AND bfdbRecH.txtCur = tblForex.txtCur AND bfdbRecH.Recstat <> "C" AND bfdbRecH.contype <> "V":
            wsTotal = wsTotal + bfdbRecH.Amount.
        END.
        IF wsTotal <> 0 THEN DO:
             EXPORT STREAM b DELIMITER ','  tblForex.txtCur  wsTotal.
            
        END.
   END.

   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ',' "DETAILED COLLECTION".
   EXPORT STREAM b DELIMITER ',' "".
   EXPORT STREAM b DELIMITER ','   "WARD NUMBER" "WARD NAME" "ACCOUNT" "NAME" "TARRIF" "AMOUNT" "CURRENCY".
FOR EACH bfdbRecH1 WHERE bfdbRecH1.OpCode >= wsOp AND bfdbRecH1.OpCode <= wsOp1 AND bfdbRecH1.RecDate >= wsDate AND bfdbRecH1.RecDate <= wsDate1 AND bfdbRecH1.Recstat <> "C" AND bfdbRecH1.contype <> "V"
        BREAK BY bfdbRecH1.ward  BY bfdbRecH1.txtCur:
     EXPORT STREAM b DELIMITER ',' bfdbRecH1.ward bfdbRecH1.wDescrip  bfdbRecH1.Account  bfdbRecH1.NAME bfdbRecH1.cDescrip  bfdbRecH1.Amount  BFdbRecH1.txtCur.
END.
FOR EACH bfdbRecH:
    DELETE bfdbRecH.
END.
FOR EACH bfdbRecH1:
    DELETE bfdbRecH1.
END.
END.

