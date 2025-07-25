/* Program.................dbrecbnk.p
   Notes:................. Receipt Banking Report
   Author:.................S. Mawire
   Modified:..............S. Chisoro
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF STREAM a.
DEF VAR wsDate LIKE dbrecH.recDate EXTENT 2.
DEF VAR wsop   LIKE dbrecH.opcode EXTENT 2.
DEF VAR wsRef LIKE dbrecH.recNo.
DEF VAR wsBank LIKE dbPay.descrip.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR wsAmt AS DEC FORM "zzzzzzzzzzz9.99-".
DEF VAR wsFile AS CHAR FORM "X(40)".
DEF VAR wsFile1 AS CHAR FORM "X(40)".
DEF VAR wsFile2 AS CHAR FORM "X(40)".
DEF BUTTON  btn-Ok LABEL "OK".
DEF BUTTON  btnClose LABEL "CLOSE".
DEF BUTTON btnExp    LABEL "EXPORT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 99 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 99 by 17.

DEF BUFFER bfBank FOR cbkmf.

DEFINE TEMP-TABLE tmpRec
   FIELD opcode  LIKE dbrec.opcode
   FIELD recDate LIKE dbrec.RecDate
   FIELD bank    LIKE cbkmf.descrip
   FIELD acb     LIKE cbkmf.descrip
   FIELD Paytype LIKE dbPay.descrip
   FIELD Amt     LIKE dbRec.Amount.

DEFINE FRAME frmMain
    skip(0.5)
    wsDate[1]       colon 30 LABEL "RECEIPTS DATE: FROM" 
    wsDate[2]       COLON 60  LABEL "TO:" SKIP(0.5)
    wsop[1]         COLON 30 LABEL "OPERATOR FROM"
    wsOp[2]         COLON 60 LABEL "TO:" SKIP(0.5)
    skip(1.5)
    dbRec.RecNo     COLON 40 LABEL "Processing....." VIEW-AS TEXT SKIP(1.5)
     btnexp  COLON 10 SPACE(40) btnclose SPACE(10)
    SKIP(1)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "RECEIPT BANKING REPORT".


/******** Triggers ***********/

ON CHOOSE OF btnexp IN FRAME frmMain 
DO:
   wsFile = simctr.repDir + "bank" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
   wsFile1 = simctr.repDir + "Static.csv".
   wsFile2 =  substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)).
   OUTPUT STREAM a TO VALUE(wsFile1).
   EXPORT STREAM a DELIMITER ','  wsFile.
    ASSIGN wsOp wsDate.
    IF CAN-FIND (FIRST dbrecctr WHERE dbRecCtr.usercode >= wsOp[1] AND dbRecCtr.usercode <= wsOp[2]
                                  AND  dbRecCtr.RecDate >= wsDate[1] AND dbRecCtr.RecDate <= wsDate[2]
                                  AND  dbRecCtr.EOD  = YES) THEN DO:
         /* create temp receipts */
         FOR EACH dbrecctr WHERE dbRecCtr.usercode >= wsOp[1] AND dbRecCtr.usercode <= wsOp[2]
                                  AND  dbRecCtr.RecDate >= wsDate[1] AND dbRecCtr.RecDate <= wsDate[2]
                                  AND  dbRecCtr.EOD  = YES NO-LOCK:
              FOR EACH dbrec WHERE dbrec.opcode = dbRecCtr.usercode AND dbrec.RecDate = dbRecCtr.RecDate 
                  AND dbrec.RecStat = "" NO-LOCK BREAK BY dbRec.Paytype  BY dbRec.rCode:
                  DISPLAY dbRec.RecNo WITH FRAME frmMain.
                  PAUSE 0.
                  
                  IF FIRST-OF(dbRec.Paytype) THEN DO:
                     FIND FIRST dbpay WHERE dbPay.Paytype = dbRec.Paytype NO-LOCK NO-ERROR. 
                     FIND FIRST cbkmf WHERE cbkmf.bank = dbpay.bank NO-LOCK NO-ERROR.
                  END.
                  IF FIRST-OF(dbRec.rCode) THEN DO:
                     FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
                     FIND FIRST bfBank WHERE bfBank.bank =  dbrcod.acb NO-LOCK NO-ERROR.
                  END.
                  CREATE tmpRec.
                  ASSIGN tmprec.opcode  = dbrec.opcode
                         tmprec.recDate = dbrec.RecDate
                         tmpRec.bank    = cbkmf.descrip
                         tmpRec.acb     = bfBank.descrip.
                         tmpRec.Paytype = dbPay.descrip.
                         tmpRec.Amt     = dbRec.Amount.
              END.
         END.
         OUTPUT STREAM b TO VALUE(wsFile).
         EXPORT STREAM b DELIMITER ','  "DATE" "CASHIER" "BANK" "CASHBOOK" "COLLECTION" "AMOUNT".
         EXPORT STREAM b DELIMITER ',' "".
         RUN Expo-ip.
    
        OUTPUT STREAM b CLOSE.
        OUTPUT STREAM a CLOSE.
        /*OS-COMMAND NO-WAIT VALUE(wsFile). */
        OS-COMMAND NO-WAIT VALUE(wsFile2 + "bin\dbrecbnk\dbrecbnk.exe").
    END.
    ELSE DO: 
    MESSAGE "NO Receipts found....." VIEW-AS ALERT-BOX.
    APPLY 'ENTRY' TO btnClose IN FRAME frmMain. 
    END.
    RETURN.
    /*APPLY 'close' TO THIS-PROCEDURE.*/
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
   
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ENABLE ALL  WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frmMain.
HIDE FRAME frmMain.

PROCEDURE Expo-ip.
FOR EACH dbrecctr WHERE dbRecCtr.usercode >= wsOp[1] AND dbRecCtr.usercode <= wsOp[2]
                                  AND  dbRecCtr.RecDate >= wsDate[1] AND dbRecCtr.RecDate <= wsDate[2]
                                  AND  dbRecCtr.EOD  = YES NO-LOCK:
   FOR EACH tmpRec WHERE tmpRec.opcode = dbRecCtr.usercode AND tmpRec.RecDate = dbRecCtr.RecDate NO-LOCK
                       BREAK BY tmpRec.bank  BY tmpRec.acb:
          IF FIRST-OF(tmpRec.acb) THEN
             wsAmt = 0.
          wsAmt = wsAmt + tmpRec.Amt.
          IF LAST-OF(tmpRec.acb) THEN DO:
            EXPORT STREAM b DELIMITER ',' dbRecCtr.recDate dbRecCtr.usercode  tmpRec.bank tmpRec.acb tmpRec.paytype wsAmt.
          END.                 
   END.
END.
END PROCEDURE.
