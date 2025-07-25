/* Program.................KADDISCJNL.p
   Notes:................. gEBNERATE DISCOUNTS
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable        bfr{&tmptable}
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
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR X AS INT.
DEF VAR wsledger LIKE glmf.acct.
DEF  VAR wsdate AS DATE.
DEF  VAR en-date AS DATE.
DEF VAR wsper LIKE simctr.curper.
DEF VAR wsbatch LIKE  dbbatch.intbatch.
DEF VAR wsacc LIKE  dbcmf.dbacc.
DEF VAR wsdisc AS DEC.


DEF BUTTON btnOk     LABEL "PROCESS".
DEF BUTTON btnExit   LABEL "EXIT".

DEF RECT rect-1 SIZE 93 BY 6.5.
DEF RECT rect-2 SIZE 93 BY 2.5.

DEF BUFFER bfr{&tmptable} FOR dbBatch.
DEF BUFFER  bfdbctr FOR dbBCtr.

DEFINE FRAME frm-main
     SKIP(0.5)
    wsdate  COLON 30  LABEL "Receipting Start Date" SPACE(10) en-Date LABEL "End Date"
    SKIP(0.5)  wsDisc LABEL "Incentive %" COLON 30 SKIP(0.5)
    wsbatch cOLON 30 LABEL "Enter Batch Number"  space(10) 
    SKIP(1)
    wsacc COLON 30 LABEL "Processing....." VIEW-AS TEXT SKIP(0.5)
    rect-1 AT ROW 1 COLUMN 4
    rect-2 AT ROW 7.8 COLUMN 4
    btnok AT ROW 8.6 COLUMN 20 SPACE(40) btnexit
    WITH SIZE 100 BY 11 SIDE-LABEL
     TITLE "USD INCENTIVE JOURNALS " VIEW-AS DIALOG-BOX.


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

ON 'enter':U OF en-date IN FRAME frm-main
    OR 'leave':U OF en-date IN FRAME frm-main
DO:
    ASSIGN en-date.
    RETURN.
END.

ON 'choose':U OF btnOk IN FRAME frm-main
DO:   
    ASSIGN wsdate en-date wsdisc wsbatch.
    ASSIGN wsper = DEC(STRING(YEAR(wsdate)) + STRING(MONTH(wsdate),"99")).
    FIND FIRST bfdbctr WHERE bfdbCtr.intbatch = wsbatch NO-ERROR.
    IF NOT AVAILABLE  bfdbctr THEN DO:
        CREATE  bfdbCtr.
        ASSIGN  bfdbCtr.UID      = varUser
                bfdbCtr.intbatch = wsbatch
                bfdbCtr.bdate    = wsDate
                bfdbCtr.period   = wsPer.
        FOR EACH dbhtf WHERE dbhtf.trdate >= wsdate  AND dbhtf.trdate <= en-date 
                         AND txtCur = "USD" AND prog = "recupd.p".
            DISPLAY dbhtf.dbacc @ wsacc WITH FRAM frm-main.
             PAUSE 0.
             wsledger = 0.
             ASSIGN wsledger = 416321903 WHEN dbhtf.sgrp = 1
                    wsledger = 119001903 WHEN dbhtf.sgrp = 2
                    wsledger = 416401903 WHEN dbhtf.sgrp = 3
                    wsledger = 119001903 WHEN dbhtf.sgrp = 5
                    wsledger = 516501903 WHEN dbhtf.sgrp = 21
                    wsledger = 117501903 WHEN dbhtf.sgrp = 25
                    wsledger = 119001903 WHEN dbhtf.sgrp = 30
                    wsledger = 119001903 WHEN dbhtf.sgrp = 34.
             IF (dbhtf.sgrp = 1 OR dbhtf.sgrp = 2 OR dbhtf.sgrp = 3 OR dbhtf.sgrp = 5 OR dbhtf.sgrp = 21
                 OR  dbhtf.sgrp = 25 OR dbhtf.sgrp = 30 OR dbhtf.sgrp = 34) THEN DO:  
                RUN discount-ip. 
            END.
        END.
    MESSAGE "Journal load Completed..." VIEW-AS ALERT-BOX.
    APPLY 'entry' TO btnexit IN FRAME frm-main.
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
en-date:SCREEN-VALUE = STRING(TODAY).
WAIT-FOR CHOOSE OF btnexit OR CLOSE OF THIS-PROCEDURE.




PROCEDURE discount-ip:
    FIND FIRST glmf WHERE glmf.acct = wsledger NO-LOCK NO-ERROR.
    CREATE   bfr{&tmptable}.
        X = X + 1.
        ASSIGN   bfr{&tmptable}.ILedger  = wsledger
                 bfr{&tmptable}.Dept     = glmf.dept
                 bfr{&tmptable}.fund     = glmf.fund
                 bfr{&tmptable}.Proj     = glmf.proj
                 bfr{&tmptable}.intBatch = wsbatch
                 bfr{&tmptable}.seq      = X
                 bfr{&tmptable}.dbAcc    = dbhtf.dbacc
                 bfr{&tmptable}.Sgrp    = dbhtf.sgrp
                 bfr{&tmptable}.AMT      = ROUND((dbhtf.amt * (wsdisc / 100)),2)
                 bfr{&tmptable}.Vat      = ROUND((dbhtf.vat * (wsdisc / 100)),2)
                 bfr{&tmptable}.age      = 3
                 bfr{&tmptable}.REF      = "Incen202303"
                 bfr{&tmptable}.trDATE   = wsdate
                 bfr{&tmptable}.Descrip  =  "USD payment discount".

        ASSIGN  bfdbctr.amt[1] =  bfdbctr.amt[1] +  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.AMT > 0
                bfdbctr.amt[2] =  bfdbctr.amt[2] +  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.AMT < 0.
END PROCEDURE.

