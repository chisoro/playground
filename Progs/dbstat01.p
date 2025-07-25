/* Program.................dbStat01.p
   Notes:................. Monthly statement print
   Author:.................S. Mawire
*/
/*&SCOPED-DEFINE Pgorientation           "Potrait" */
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR c AS INT.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsTitle AS CHAR FORM "x(80)" INIT "============================ MONTHLY SERVICES STATEMENTS===========================".
DEF VAR varRegNo LIKE SIMCTR.REGNO.
DEF VAR vaAcc LIKE dbcmf.dbacc.
DEF VAR varCouncil LIKE SIMCTR.CONAME.
DEF VAR varAdd1 LIKE SIMCTR.Add1.
DEF VAR varAdd2 LIKE SIMCTR.Add2.
DEF VAR varAdd3 LIKE SIMCTR.Add3.
DEF VAR varVat AS DEC.
DEF VAR varVats AS CHAR.
DEF VAR varTotal AS DEC.
DEF VAR wsline AS CHAR FORM "x(80)"
    INIT "=============================================================================================".
DEF VAR varDate AS DATE LABEL "DATE".
DEF VAR varRef LIKE dbmtf.Ref LABEL "REF".
DEF VAR varDesc LIKE dbmtf.DESCRIP LABEL "DESCRIPTION".
DEF VAR varAmt LIKE dbmtf.Amt LABEL "AMOUNT".
DEF VAR wsAge LIKE dbmtf.Amt EXTENT 6.
DEF VAR wsCr LIKE dbmtf.Amt.
DEF VAR varDue LIKE dbmtf.Amt.
DEF VAR wsMessage1 AS CHAR FORM "X(80)" .
DEF VAR wsMessage2 AS CHAR FORM "X(80)".
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsStatus AS CHAR.
DEF VAR wsZero AS LOGICAL.
DEF VAR wsAccDate AS DATE.
DEF VAR wsDueDate AS DATE.
DEF VAR wsPeriod LIKE simctr.curper.
DEF VAR wsVarTotal AS DEC.
DEF VAR wsCharge AS DEC.
DEF VAR wsAcc LIKE dbmmf.dbacc.
DEF VAR wsTar LIKE dbtmf.Tarif.
DEF VAR wsMeter LIKE dbmmf.serial.

DEF TEMP-TABLE wsDbblf NO-UNDO
     FIELD dbacc LIKE dbblf.dbacc
     FIELD amt LIKE dbblf.amt
     FIELD INT LIKE dbblf.INT.

DEF TEMP-TABLE wsDbcmf no-undo
    FIELD dbacc LIKE dbcmf.dbacc
    FIELD AccBal LIKE dbcmf.AccBal
    FIELD NAME LIKE dbcmf.Name
    FIELD Add1 LIKE dbcmf.Add1 
    FIELD  add2 LIKE dbcmf.add2
    FIELD ADD3 LIKE dbcmf.Add3
    FIELD balbf LIKE dbcmf.balbf
    INDEX acc dbacc ASC.

DEF TEMP-TABLE wsdbtmf no-undo
    FIELD tarif LIKE dbtmf.tarif
    FIELD TYPE LIKE dbtmf.TYPE
    FIELD sgrp LIKE dbtmf.sgrp
    FIELD charge LIKE dbtmf.charge.

DEF TEMP-TABLE tmpwork LIKE dbmtf.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 12.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
    wsAccDate  LABEL "Accounting Date" COLON 30 SPACE(10)
    wsDueDate  LABEL "Due Date"                   SKIP(0.5)
    wsPeriod   LABEL "Accounting Period" COLON 30 SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.5)
     wsmessage1 LABEL "MESSAGE" COLON 15 SKIP          
     wsmessage2 NO-LABEL        COLON 15
    SKIP(1)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 14.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 14 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 105 BY 17.5 KEEP-TAB-ORDER
    TITLE "MONTHLY STATEMENT PRINT" VIEW-AS DIALOG-BOX.   

FORM
     SKIP(1)
    "READING DATE.:   " COLON 5 dbmmf.rDate[12] SKIP
    "TARIF" COLON 5 SPACE(4) "PRESENT" SPACE(5) "PREVIOUS" SPACE(3) "USAGE" SPACE(3)  "BASIC" SPACE(8) "AMOUNT" SPACE(8) "TOTAL" SKIP
    dbmmf.tarif COLON 5 dbmmf.READ[12]  dbmmf.READ[11] dbmmf.consum[12] dbtmf.charge  dbmtf.amt  wsVarTotal 
    WITH WIDTH 132
        NO-BOX
        STREAM-IO
        NO-LABELS
    FRAME frm-water.

FORM
    /* wsTitle               COLON 5*/
     SKIP(10)
     "Account NO.: "         at 52
     dbcmf.dbacc                at 65 SKIP(1)
     /*"VAT REG.: "     COLON 5
     dbcmf.VatNo*/
     "ACC DATE.:      "  AT 52 wsaccDate SKIP
     dbcmf.name         COLON 5
     "DUE DATE.:      " AT 52  wsDueDate   SKIP
     dbcmf.add1           COLON 5
     "PERIOD.:        " AT 52 wsPeriod   SKIP
     dbcmf.add2          COLON 5
     dbcmf.add3          COLON 5
     
	 with width 132
	      no-box
          stream-io
	      no-labels
	      frame frm-hdr.
FORM
    wsline                COLON 5 SKIP
     "DATE" COLON 5 SPACE(5) "REF" SPACE(7) "DESCRIPTION" SPACE(36) "AMOUNT"
    with width 132
	      no-box
          stream-io
	      no-labels
	      frame frm-hdr1.
    

FORM                                              /* tran line */
    varDate  COLON 5 SPACE(1)
     varRef   FORM "X(10)"               
     varDesc   format "x(40)" at 26
     varAmt                    at 68
     varVats FORMAT "x(1)" at 80
     with width 132
	  no-box
	  no-labels
      DOWN
      STREAM-IO
	  frame frm-trans.

FORM
     wsline          COLON 5 SKIP
     "CREDIT"        COLON 5
     "  90-days+"    SPACE (3)
     "   60-days"    SPACE (3)
     "   30-days"    SPACE (5)
     "CURRENT"       SPACE (8)
     "AMOUNT DUE"    SPACE (5)
     skip
     wsAge[1]      format "zzzzzzz9.99-"    
     wsage[5]  format "zzzzzzz9.99-" 
     wsage[4]  format "zzzzzzz9.99-" 
     wsage[3]  format "zzzzzzz9.99-" 
     wsage[2]  format "zzzzzzz9.99-" SPACE(3)      
     wsAge[6]  format "zzzzzzzz9.99-" 
     wsline    COLON 5 SKIP(1)
     wsmessage1                 COLON 5          
     wsmessage2                 COLON 5
     with width 132
	  no-box
	  no-labels
      stream-io
	  FRAME frm-footer.
      
/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  wsStart = DEC(wsStart:SCREEN-VALUE)
           wsEnd   = DEC(wsEnd:SCREEN-VALUE)
           wsAccDate 
           wsDueDate
           wsPeriod
           wsZero =  LOGICAL(wsZero:SCREEN-VALUE).
           wsMessage1 = wsMessage1:SCREEN-VALUE IN FRAME frm-main.
           wsMessage2 = wsMessage2:SCREEN-VALUE IN FRAME frm-main.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  MESSAGE "Statement Print Completed" VIEW-AS ALERT-BOX.
  APPLY 'close' TO THIS-PROCEDURE.
  HIDE FRAME frm-main.
  RETURN.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
IF AVAILABLE simctr THEN
    ASSIGN  varRegNo   = SIMCTR.BPNO.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       wsZero:SCREEN-VALUE = "NO"
       wsPeriod:SCREEN-VALUE = STRING(simctr.curper).
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

 
PROCEDURE Report.ip:
FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd NO-LOCK.
    PAUSE 0 BEFORE-HIDE.
    DISPLAY dbcmf.dbacc @ wssTatus WITH  NO-LABEL OVERLAY FRAME frm-main.
    IF  dbcmf.AccBal = 0 /*AND wsZero = NO */ THEN NEXT.
        ASSIGN  varDue  = 0
                wsAge   = 0
                wsCr    = 0
                j       = 0.
    DISPLAY STREAM a  wsAccDate wsDueDate wsPeriod dbcmf.dbAcc
            dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 WITH FRAME frm-hdr.
    DOWN STREAM a WITH FRAME frm-hdr.
    wsage = 0.
    REPEAT:
        FIND NEXT dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
        IF AVAILABLE dbblf THEN DO:
            ASSIGN wsage[6] = wsage[6] + dbblf.INT.
                   wsage[3] = wsage[3] + dbblf.INT.
            DO X = 1 TO 15:
                ASSIGN wsage[1] = wsage[1] + dbblf.amt[1] WHEN X = 1 AND dbblf.amt[1] < 0
                       wsage[2] = wsage[2] + dbblf.amt[1] WHEN X = 1 AND dbblf.amt[1] > 0
                       wsage[3] = wsage[3] + dbblf.amt[X] WHEN X = 2
                       wsage[4] = wsage[4] + dbblf.amt[X] WHEN X = 3
                       wsage[5] = wsage[5] + dbblf.amt[X] WHEN X >= 4
                       wsage[6] = wsage[6] + dbblf.amt[X]. 
            END. 
        END.  
        IF NOT AVAILABLE dbblf THEN LEAVE.
    END.
    wsmeter = "".
    FIND FIRST dbmmf WHERE dbmmf.dbacc = dbcmf.dbacc AND dbmmf.mstat = 1  AND dbmmf.tarif <> 0 NO-ERROR.
    IF AVAILABLE dbmmf THEN DO:
       FIND FIRST dbtmf WHERE dbmmf.tarif = dbtmf.tarif AND dbtmf.TYPE = 1 NO-LOCK NO-ERROR.
       IF AVAILABLE dbtmf THEN DO:
          FIND FIRST dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc AND dbmtf.ref  = dbmmf.serial NO-LOCK NO-ERROR. 
          IF AVAILABLE dbmtf THEN DO:
             DISPLAY STREAM a dbmmf.rDate[12] dbmmf.tarif dbmmf.READ[12] dbmmf.READ[11] dbmmf.consum[12] 
                         dbtmf.charge dbmtf.amt @ wsVarTotal (dbmtf.amt  - dbtmf.charge) @ dbmtf.amt
                            WITH FRAME frm-water.
             DOWN STREAM a WITH FRAME frm-water.
          END.
       END.
    END.
    DISPLAY STREAM a  wsline WITH FRAME frm-hdr1.
    DOWN STREAM a WITH FRAME frm-hdr.
    DISPLAY STREAM a "Balance b/f " @ varDesc dbcmf.BalBf @ varAmt
                     WITH FRAME frm-trans.
    DOWN STREAM a WITH FRAME frm-trans.
    j = j + 1.
    varVat = 0.
    RUN tmpwork-ip.
    RUN rep-ip.
    j = 24 - j.   
    DOWN j STREAM a WITH FRAME frm-trans.
    DISPLAY STREAM a "VAT ON '*' ITEMS: " + string(varVat)  WHEN varVat <> 0 @ varDesc WITH FRAME frm-trans.
    DOWN STREAM a WITH FRAME frm-trans.
    DISPLAY STREAM a wsage[1] wsage[2] wsage[3] wsage[4] wsage[5] wsage[6]
                  wsline  wsMessage1 wsMessage2  WITH FRAME frm-footer. 
    PAGE STREAM a.
END. /* eo--dbcmf */
RETURN.
END PROCEDURE.

PROCEDURE rep-ip:
    FIND FIRST tmpwork WHERE tmpwork.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
    IF AVAILABLE tmpwork THEN DO:
       varAmt = tmpwork.Amt.
       varVat = varVat + tmpwork.Vat.
       DISPLAY STREAM a tmpwork.trDate @ varDate tmpwork.ref @ varRef 
              tmpwork.DESCRIP @ varDesc varAmt varAmt  "*" WHEN tmpwork.vat <> 0 @ varVats  WITH FRAME frm-trans.
       DOWN STREAM a WITH FRAME frm-trans.
       j = j + 1.
    END.
    REPEAT:
        FIND NEXT tmpwork WHERE tmpwork.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
        IF AVAILABLE tmpwork THEN DO:
            varAmt = tmpwork.Amt.
            varVat = varVat + tmpwork.Vat.
            DISPLAY STREAM a tmpwork.trDate @ varDate tmpwork.ref @ varRef 
                tmpwork.DESCRIP @ varDesc varAmt varAmt  "*" WHEN tmpwork.vat <> 0 @ varVats  WITH FRAME frm-trans.
           DOWN STREAM a WITH FRAME frm-trans.
           j = j + 1.
        END.   
        IF NOT AVAILABLE tmpwork THEN LEAVE.
    END.
END PROCEDURE.

PROCEDURE tmpwork-ip: /*consolidate Receipts and Journals */
    FOR EACH tmpwork.
      DELETE tmpwork.
    END.
    FOR EACH dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc NO-LOCK:
        IF dbmtf.prog = "dbint.p" THEN DO: /* Interest */
           FIND FIRST tmpwork WHERE tmpwork.prog = "dbint.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Interest".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE IF dbmtf.prog = "dbjnl02.p" THEN DO: /* Journals */
           FIND FIRST tmpwork WHERE tmpwork.prog = "dbjnl02.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Adjusting Journals".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE IF dbmtf.prog = "recupd.p" THEN DO: /* Receipts */
           FIND FIRST tmpwork WHERE tmpwork.prog = "recupd.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Receipts".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE DO:
           CREATE tmpwork.
           BUFFER-COPY dbmtf TO tmpwork.
        END.
    END.
END PROCEDURE
