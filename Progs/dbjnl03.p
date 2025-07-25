/* Program.................dbjnl03.p
   Notes:................. Import Debtors Journal Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable        dbBatch
&SCOPED-DEFINE tmpFields       bfr{&tmptable}.trDATE ~
                                        COLUMN-LABEL ' Transaction Date ':C ~
                               bfr{&tmptable}.REF ~
                                        COLUMN-LABEL ' Reference ':C ~
                               bfr{&tmptable}.dbAcc ~
                                        COLUMN-LABEL ' Account ':C ~
                               bfr{&tmptable}.AMT ~
                                        COLUMN-LABEL ' Amount ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

DEF STREAM b.
DEF NEW SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsid LIKE  dbBCtr.intBatch.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wsdel          AS LOGICAL INITIAL NO.
DEF VAR wsbatch        LIKE  dbBCtr.intBatch.
DEF VAR wsrec          LIKE  dbBatch.seq.
DEF VAR wsFile         AS CHAR FORM "x(60)".
DEF VAR wsacc          LIKE dbcmf.dbacc.
DEF VAR wsper          LIKE dbBCtr.period.
DEF VAR wsAmt          LIKE dbhtf.amt.
DEF VAR wsVat          LIKE dbhtf.amt.
DEF VAR wsDate         AS DATE.
DEF  VAR wsdes           AS CHAR.
DEF VAR wsledger       LIKE glmf.acct.
DEF VAR wsref          LIKE dbBatch.ref.
DEF VAR wsSvr          LIKE dbBatch.sgrp.
DEF VAR wsAge          AS INT.
DEF VAR jnltype        AS INT.
DEF BUTTON btnFile   LABEL "LOAD FILE".
DEF BUTTON btnOk     LABEL "PROCESS".
DEF BUTTON btnExit   LABEL "EXIT".

DEF RECT rect-1 SIZE 93 BY 6.5.
DEF RECT rect-2 SIZE 93 BY 2.5.

DEF BUFFER bfr{&tmptable} FOR {&tmptable}.
DEF BUFFER  bfdbctr FOR dbBCtr.

DEFINE FRAME frm-main
     SKIP(0.5)
    btnFile COLON 10 wsFile NO-LABEL SKIP(0.5)
    wsPer  COLON 30  LABEL "Enter Accounting Period" SPACE(10)  wsDate LABEL "Batch Date" SKIP(0.5)
    wsbatch cOLON 30 LABEL "Enter Batch Number"  space(10) 
    jnltype LABEL "Select Journal type" VIEW-AS COMBO-BOX
                    LIST-ITEM-PAIRS "Credit Journal",1, "Debit Journal", 2 SKIP(1)
    wsacc COLON 30 LABEL "Processing....." VIEW-AS TEXT SKIP(0.5)
    rect-1 AT ROW 1 COLUMN 4
    rect-2 AT ROW 7.8 COLUMN 4
    btnok AT ROW 8.6 COLUMN 20 SPACE(40) btnexit
    WITH SIZE 100 BY 11 SIDE-LABEL
     TITLE "IMPORT DEBTORS JOURNALS" VIEW-AS DIALOG-BOX.

ON 'enter':U OF wsPer IN FRAME frm-main
DO:
    ASSIGN wsper.
    IF wsper > simctr.curper THEN DO:
        MESSAGE "Accounting period not yet opened..." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF wsper < simctr.closeper THEN DO:
        MESSAGE "Accounting period has been closed..." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF jnltype IN FRAME frm-main 
    OR 'leave':U OF jnltype IN FRAME frm-main 
DO:
    ASSIGN  jnltype.
    RETURN.
END.

ON 'choose':U OF btnFile IN FRAME frm-main
DO:
    wsFile = "".
    SYSTEM-DIALOG GET-FILE wsFile SAVE-AS TITLE "Select File ...". 
    IF wsFile = "" THEN DO:
         RETURN NO-APPLY.
    END.
    DISPLAY wsfile WITH FRAME frm-main.
    INPUT STREAM b FROM VALUE(wsFile).
END.

ON 'enter':U OF wsbatch IN FRAME frm-main
    OR 'leave':U OF wsbatch IN FRAME frm-main
DO:
    ASSIGN wsbatch.
    FIND FIRST bfdbCtr WHERE  bfdbCtr.intbatch = wsbatch NO-ERROR.
    IF AVAILABLE bfdbCtr THEN DO:
        MESSAGE "Batch number already exists...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF wsdate IN FRAME frm-main
    OR 'leave':U OF wsdate IN FRAME frm-main
DO:
    ASSIGN wsdate.
    RETURN.
END.
ON 'choose':U OF btnOk IN FRAME frm-main
DO:   
    ASSIGN wsper wsbatch wsdate jnltype.
    IF wsper = 0 THEN DO:
        MESSAGE "Accounting period cannot be zero" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF wsdate = ? THEN DO:
        MESSAGE "Transaction date cannot be blank" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF jnltype <> 1 AND jnltype <> 2 THEN DO:
        MESSAGE "Journal type must be either 1 or 2" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    FIND FIRST bfdbctr WHERE bfdbCtr.intbatch = wsbatch NO-ERROR.
    IF NOT AVAILABLE  bfdbctr THEN DO:
        CREATE  bfdbCtr.
        ASSIGN  bfdbCtr.UID      = varUser
                bfdbCtr.intbatch = wsbatch
                bfdbCtr.bdate    = wsDate
                bfdbCtr.period   = wsPer.
        RUN Batch-ip.
    END.
    ELSE DO:
        MESSAGE "Batch already exists...." VIEW-AS ALERT-BOX.
        APPLY 'close' TO THIS-PROCEDURE.
    END.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
ENABLE ALL WITH FRAME fRM-MAIN.
wsdate:SCREEN-VALUE = STRING(TODAY).
WAIT-FOR CHOOSE OF btnexit OR CLOSE OF THIS-PROCEDURE.

PROCEDURE batch-ip:
    REPEAT:
        IMPORT STREAM b DELIMITER "," wsacc wsSvr wsref  wsAmt wsVat wsledger wsdes wsage.
        DISPLAY wsacc WITH FRAME frm-main.
        PAUSE 0.
        FIND FIRST glmf WHERE glmf.acct = wsledger NO-ERROR.
        CREATE  bfr{&tmptable}.
        wsRec = wsRec + 1.
        ASSIGN  bfr{&tmptable}.ILedger  = wsLedger
                bfr{&tmptable}.Dept     = glmf.dept
                bfr{&tmptable}.fund     = glmf.fund
                bfr{&tmptable}.Proj     = glmf.proj
                bfr{&tmptable}.intBatch = wsbatch
                bfr{&tmptable}.seq      = wsrec
                bfr{&tmptable}.dbAcc    = wsacc
                bfr{&tmptable}.Sgrp     = wsSvr
                bfr{&tmptable}.AMT      = wsAmt * -1 WHEN jnltype = 1
                bfr{&tmptable}.Vat      = wsVat * -1 WHEN jnltype = 1
                bfr{&tmptable}.AMT      = wsAmt      WHEN jnltype = 2
                bfr{&tmptable}.Vat      = wsVat      WHEN jnltype = 2
                bfr{&tmptable}.age      = wsage
                bfr{&tmptable}.REF      = wsRef
                bfr{&tmptable}.trDATE   = wsDate
                bfr{&tmptable}.Descrip  =  wsDes.
        ASSIGN  bfdbCtr.amt[1] =  bfdbCtr.amt[1] + bfr{&tmptable}.AMT WHEN bfr{&tmptable}.AMT > 0
                bfdbCtr.amt[2] =  bfdbCtr.amt[2] + bfr{&tmptable}.AMT WHEN bfr{&tmptable}.AMT < 0.
    END.
    MESSAGE "Journal load Completed..." VIEW-AS ALERT-BOX.
END PROCEDURE.
