/* Program.................recjnl.p
   Notes:...... ..........Reverse/Refund/RD payments
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE pgorientation "landscape"

DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsStatus      LIKE gltdf.acct.
DEF SHARED VAR varUser LIKE simusr.usercode INIT "1".
DEF VAR wsAmt         AS DEC     FORM "zzzzzzzz9.99-".
DEF VAR wsTotal       AS DEC     FORM "zzzzzzzz9.99-".
DEF VAR wsTitle       AS CHAR FORM "X(60)".
DEF VAR wsTitle0      AS CHAR FORM "X(60)".
DEF VAR wsYear        AS INT FORM "9999".
DEF VAR wsMonth       AS INT FORM "99".
DEF VAR wsTime        AS CHAR FORM "x(8)".
DEF VAR wsper         LIKE simctr.curper.
DEF VAR wsName        LIKE cbkmf.descrip.
DEF VAR wsTitle1      AS CHAR FORM "X(50)" INITIAL "RECEIPT REVERSAL/REFUND UPDATE REPORT".
DEF VAR wsDate        AS DATE.
DEF VAR wsJAmt        LIKE dbRec.Amount.
DEF VAR wsAlloc       LIKE dbRec.Amount.
DEF VAR wsDr          LIKE dbRec.Amount FORM "zz,zzz,zz9.99-".
DEF VAR wsCr          LIKE dbRec.Amount FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr         LIKE wsDr.
DEF VAR wsTCr         LIKE wsDr.
DEF VAR wsNar         AS CHAR FORM "x(60)".
DEF VAR wsRef         AS CHAR FORM "x(20)".
DEF VAR wsTransId     LIKE CBTrans.TransID.
DEF VAR wsSource      LIKE dbgl.SOURCE INITIAL "RE".
DEF VAR varBank           LIKE cbkmf.bank.
DEF VAR X             AS INT.

DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "PROCESS".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF TEMP-TABLE tmpRec
    FIELD wsbank LIKE cbkmf.bank
    FIELD wsacb  LIKE cbkmf.acb
    FIELD wscode LIKE dbRCod.rCode
    FIELD wsAcc  LIKE dbRec.Account
    FIELD wsTar  AS    CHAR
    FIELD wsAmt  LIKE dbRec.Amount   FORM "ZZZ,ZZZ,ZZ9.99-"
    FIELD wsVat  LIKE dbRec.Amount   FORM "Z,ZZZ,ZZ9.99-".

DEF    QUERY qryRec FOR tmpRec SCROLLING.
DEF BROWSE brw-rec QUERY qryRec
     DISPLAY wsDate        LABEL "DATE" 
             tmprec.wsCode LABEL "CODE"
             tmpRec.wsAcc  LABEL "ACCOUNT"
             tmpRec.wsAmt  LABEL "AMOUNT"
             tmpRec.wsVat  LABEL  "VAT" WITH NO-LABEL 8 DOWN SEPARATORS.


DEFINE FRAME frm-main
    SKIP(1)
    wsPer  COLON 30  LABEL "ACCOUNTING PERIOD" SPACE(10)
    wsDate           LABEL "TRANSACTION DATE" SKIP(0.5)
    wsRef  COLON 20  LABEL "JOURNAL REF."     SKIP(0.5)
    wsJAmt COLON 25  LABEL "JOURNAL AMOUNT"  SPACE(5)
    tmpRec.wsBank    LABEL "BANK " cbkmf.descrip NO-LABEL VIEW-AS TEXT
    skip(0.5)
    wsNar  COLON 20  LABEL "NARRATION" SKIP(0.5)
    "...............ALLOCATIONS.................." COLON 30 wsAlloc NO-LABEL VIEW-AS TEXT 
    SKIP(0.5)
    tmpRec.wsCode   COLON 20 LABEL "RECEIPT CODE" dbRCod.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    tmpRec.wsAcc    COLON 20 LABEL "ACCOUNT" dbcmf.NAME NO-LABEL VIEW-AS TEXT SKIP(0.5)
    tmpRec.wsAmt    COLON 20 LABEL "AMOUNT"  SKIP(0.5)
    brw-Rec AT ROW 12.5 COL 42
    btn-ok AT ROW 20.5 COL 20
    btnclose AT ROW 20.5 COL 60
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "RECEIPT REFUND/REVERSAL CAPTURE".

FORM wsDate         LABEL "DATE" AT 10
     wsref          LABEL "REFERENCE"
     tmprec.wsCode  LABEL "CODE"
     tmprec.wsAcc   LABEL "ACCOUNT"
     tmpRec.wsAmt   LABEL "AMOUNT"
     tmpRec.wsVat   LABEL "VAT"
     HEADER SKIP(1) wsTitle0 AT 20 SKIP(3)
     wsTitle1 AT 20  "Page: " AT 110 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
   WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmRpt.

FORM 
     dbgl.Fund             LABEL "SEGMENT" AT 10
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(3) "PAYMENTS UPDATE CONSOLIDATION REPORT - DATE: " AT 20
     wsDate  "PERIOD  :" wsper SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

ON 'enter':U OF wsJAmt IN FRAME frm-main
    OR 'tab':U OF wsJAmt IN FRAME frm-main
DO:
    FOR EACH tmpRec.
        DELETE tmpRec.
    END.
    OPEN QUERY qryRec FOR EACH tmpRec NO-LOCK.
    ASSIGN wsJAmt.
    wsAlloc = wsJAmt.
    DISPLAY wsAlloc WITH FRAME frm-main.
    RETURN.
END.

ON 'enter':U OF tmpRec.wsBank IN FRAME frm-main
    OR 'tab':U OF tmpRec.wsBank IN FRAME frm-main
DO:
    ASSIGN varBank = INT(tmpRec.wsBank:SCREEN-VALUE IN FRAME frm-main).
    FIND FIRST cbkmf WHERE cbkmf.bank = INT(tmpRec.wsBank:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cbkmf THEN DO:
        MESSAGE "Invalid Bank, please try again..." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY cbkmf.descrip WITH FRAME frm-main.
    END.
    RETURN.
END.

ON 'enter':U OF tmpRec.wsCode  IN FRAME frm-main
DO:
   FIND FIRST dbRcod WHERE dbRcod.rCode = tmprec.wscode:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbRcod THEN DO:
       MESSAGE "Invalid Receipting code entered....please try again" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   ELSE DO:
       DISPLAY dbRCod.Descrip WITH FRAME frm-main.
       IF dbRCod.TARGET = "V" THEN DO:
           tmpRec.wsAcc:SCREEN-VALUE = STRING(dbRCod.Ledger).
           DISABLE tmpRec.wsAcc WITH FRAME frm-main.
       END.
   END.
    RETURN.
END.

ON 'enter':U OF tmpRec.wsAcc IN FRAME frm-main
DO:
    FIND FIRST dbcmf WHERE dbcmf.dbAcc = DEC(tmpRec.wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbcmf THEN DO:
        MESSAGE "Invalid Consumer Account....Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE
        DISPLAY dbcmf.NAME WITH FRAME frm-main.
    RETURN.
END.

ON 'enter':U OF tmpRec.wsAmt IN FRAME frm-main
    OR 'tab':U OF tmpRec.wsAmt IN FRAME frm-main
DO:
   FIND FIRST dbRcod WHERE dbRcod.rCode = tmprec.wscode:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbRcod THEN DO:
       MESSAGE "Invalid Receipting code entered....please try again" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   ELSE DO:
       ASSIGN wsAlloc  = wsAlloc - DEC(tmpRec.wsAmt:SCREEN-VALUE).
       DISPLAY wsAlloc WITH FRAME frm-main.
       CREATE tmpRec.
       ASSIGN tmprec.wscode = tmprec.wscode:SCREEN-VALUE
              tmpRec.wsTar  = dbRcod.TARGET
              tmpRec.wsAmt  = DEC(tmpRec.wsAmt:SCREEN-VALUE)
              tmpRec.wsAcc  = DEC(tmpRec.wsAcc:SCREEN-VALUE)
              tmpRec.wsBank = INT(tmpRec.wsBank:SCREEN-VALUE)
              tmpRec.wsAcb  = dbRCod.Acb
              tmpRec.wsVAT  = ROUND (DEC(tmpRec.wsAmt:SCREEN-VALUE) * (dbRCod.vat% / (100 + dbRCod.vat%)),2).
       OPEN QUERY qryRec FOR EACH tmpRec NO-LOCK.
       ENABLE  tmpRec.wsAcc WITH FRAME frm-main.
       ASSIGN tmpRec.wsAcc:SCREEN-VALUE  = "".
       IF wsAlloc <> 0 THEN DO:
           APPLY 'entry' TO tmprec.wscode.
           RETURN NO-APPLY.
       END.
       ELSE DO:
           APPLY 'entry' TO btn-Ok.
           RETURN NO-APPLY.
       END.   
   END.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN wsDate    = DATE(wsDate:SCREEN-VALUE)
            wsPer     = DEC(wsPer:SCREEN-VALUE)
            wsTime    = STRING(TIME,"HH:MM:SS").
            wsTransID = DEC (string(YEAR(TODAY),"9999")
                      + string(MONTH(TODAY),"99" )
                      + string(DAY(TODAY),"99")
                      + SUBSTR(STRING(wsTIME),1,2) 
                      + SUBSTR(STRING(wsTIME),4,2) 
                      + SUBSTR(STRING(wsTIME),7,2) ).
            ASSIGN wsNar wsRef.
    {PrintOpt.i &stream-name="stream a"
                    &print-prog= rep.ip
                    &paged}
    CLEAR FRAME frm-main ALL.
    ASSIGN wsDate:SCREEN-VALUE = STRING(TODAY)
       wsper:SCREEN-VALUE = STRING(SIMCTR.CURPER).
    APPLY 'entry' TO wsPer.
    RETURN NO-APPLY.
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsDate:SCREEN-VALUE = STRING(TODAY)
       wsper:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsTitle0 = simctr.CONAME.    
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Rep.ip:
DO TRANSACTION ON ERROR UNDO, LEAVE:
FOR EACH tmpRec:
    IF tmpRec.wsTar = "V" THEN DO: /* direct TO Vote receipts */
        /* income ledger*/
        FIND FIRST glmf WHERE glmf.acct = tmpRec.wsAcc NO-LOCK NO-ERROR.
        CREATE dbgl.
        ASSIGN dbgl.acct    = tmpRec.wsAcc
               dbgl.proj    = glmf.Proj
               dbgl.dept    = glmf.dept
               dbgl.fund    = glmf.fund
               dbgl.AMT     = tmpRec.wsAmt
               dbgl.CREDATE = TODAY
               dbgl.DESCRIP = wsNar
               dbgl.period  = wsPer
               dbgl.REF     = wsRef
               dbgl.source  = "RE"
               dbgl.TransID = wsTransId
               dbgl.trDATE  = wsDate
               dbgl.UID     = varUser
               dbgl.UID2    = varUser.
        IF tmpRec.wsVat <> 0 THEN DO:
            CREATE dbgl.
            ASSIGN dbgl.acct    = simctr.vat[2]
                   dbgl.proj    = glmf.Proj
                   dbgl.dept    = glmf.dept
                   dbgl.fund    = glmf.fund
                   dbgl.AMT     = tmpRec.wsVat
                   dbgl.CREDATE = TODAY
                   dbgl.DESCRIP = wsNar
                   dbgl.period  = wsPer
                   dbgl.REF     = wsRef
                   dbgl.source  = "RE"
                   dbgl.TransID = wsTransId
                   dbgl.trDATE  = wsDate
                   dbgl.UID     = varUser
                   dbgl.UID2    = varUser.
        END.
    END. 
    ELSE IF tmpRec.wsTar = "C" THEN DO:
        FIND FIRST dbrCod WHERE dbRcod.rCODE = tmpRec.wsCode NO-LOCK NO-ERROR.
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbRCod.Sgrp NO-LOCK NO-ERROR.
        FIND FIRST glmf WHERE glmf.acct = dbsgr.ctrLedger NO-LOCK NO-ERROR.
        FIND FIRST dbcmf WHERE dbcmf.dbAcc = tmpRec.wsAcc EXCLUSIVE-LOCK NO-ERROR.
        CREATE dbhtf.
        ASSIGN dbhtf.Accper = wsPer
               dbhtf.proj   = glmf.Proj
               dbhtf.dept   = glmf.dept
               dbhtf.fund   = glmf.fund
               dbhtf.Amt    = tmpRec.wsAmt
               dbhtf.Vat    = tmpRec.wsVat
               dbhtf.dbacc  = tmpRec.wsAcc
               dbhtf.descrip = wsNar
               dbhtf.PoDate = today
               dbhtf.prog   = "recupd.p"
               dbhtf.Ref    =  wsRef
               dbhtf.Sgrp   =  dbRCod.Sgrp
               dbhtf.Tarif  = 0
               dbhtf.TransID = wsTransId
               dbhtf.trDate  = wsDate
               dbhtf.UID     = varUser.
        CREATE dbmtf.
        ASSIGN dbmtf.Accper = wsPer
               dbmtf.proj   = glmf.Proj
               dbmtf.dept   = glmf.dept
               dbmtf.Amt    = tmpRec.wsAmt
               dbmtf.Vat    = tmpRec.wsVat
               dbmtf.dbacc  = tmpRec.wsAcc
               dbmtf.Ref    = wsRef
               dbmtf.Sgrp   = dbRCod.Sgrp
               dbmtf.Tarif  = 0
               dbmtf.trDate = wsDate
               dbcmf.AccBal = dbcmf.AccBal + tmpRec.wsAmt.
        FIND FIRST dbblf WHERE dbblf.dbacc = tmpRec.wsAcc 
                           AND  dbblf.Sgrp = dbRCod.Sgrp EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE dbblf THEN DO:
            CREATE dbblf.
            ASSIGN dbblf.dbacc = tmpRec.wsAcc
                   dbblf.Sgrp   = dbRCod.Sgrp.
        END.
        ASSIGN dbblf.Amt[1] = dbblf.Amt[1] + tmpRec.wsAmt. 
        /* Consolidate */
        CREATE dbgl.
        ASSIGN dbgl.acct    = dbsgr.ctrLedger
               dbgl.proj    = glmf.Proj
               dbgl.dept    = glmf.dept
               dbgl.fund    = glmf.fund
               dbgl.AMT     = tmpRec.wsAmt
               dbgl.CREDATE = TODAY
               dbgl.DESCRIP = wsNar
               dbgl.period  = wsPer
               dbgl.REF     = wsRef
               dbgl.source  = "RE"
               dbgl.TransID = wsTransId
               dbgl.trDATE  = wsDate
               dbgl.UID     = varUser
               dbgl.UID2    = varUser.
        IF tmpRec.wsVat <> 0 THEN DO:
            X = 1.
            DO  WHILE X <= 2:
                FIND FIRST dbgl WHERE dbgl.acct = simctr.vat[X] AND SOURCE = "RE"
                    AND dbgl.UID2    = varUser NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                    CREATE dbgl.
                    ASSIGN dbgl.acct    = simctr.vat[X]
                           dbgl.proj    = glmf.Proj
                           dbgl.dept    = glmf.dept
                           dbgl.fund    = glmf.fund
                           dbgl.CREDATE = TODAY
                           dbgl.DESCRIP = wsNar
                           dbgl.period  = wsPer
                           dbgl.REF     = wsRef
                           dbgl.source  = "RE"
                           dbgl.TransID = wsTransId
                           dbgl.trDATE  = wsDate
                           dbgl.UID     = varUser
                           dbgl.UID2    = varUser.
                END.
                IF X = 1 THEN 
                    dbgl.AMT = dbgl.AMT + (tmpRec.wsVat * -1).
                ELSE IF X = 2 THEN
                    dbgl.AMT = dbgl.AMT + tmpRec.wsVat.
                X = X + 1.
            END.
        END.
    END. /* eof Target-type */
    /* Create cashbook Transaction */
    FIND FIRST cbkmf WHERE cbkmf.acb = tmpRec.wsAcb NO-ERROR.
    CREATE cbTrans.
    ASSIGN CBTrans.Acb    = tmpRec.wsAcb
           CBTrans.Accper = wsper
           CBTrans.amount = tmpRec.wsAmt * -1
           CBTrans.bank   = 0
           CBTrans.Dept   = glmf.dept
           CBTrans.Descrip = wsNar
           CBTrans.Fund    = glmf.fund
           CBTrans.Ledger  = glmf.acct
           CBTrans.OpCode  = varUser
           CBTrans.PoDate  = TODAY 
           CBTrans.prog    = "recupd.p"
           CBTrans.Proj    = glmf.Proj
           CBTrans.Ref     = wsRef
           CBTrans.TransID  = wsTransId
           CBTrans.TranType = 1
           CBTrans.trDate   = wsDate
           CBTrans.UID      = varUser
           CBTrans.UID2     = varUser
           cbkmf.Bal        = cbkmf.Bal - tmpRec.wsAmt.
    FIND FIRST glmf WHERE glmf.acct = cbkmf.Ledger NO-LOCK NO-ERROR.
    CREATE dbgl.
    ASSIGN dbgl.acct    = cbkmf.Ledger 
           dbgl.proj    = glmf.Proj
           dbgl.dept    = glmf.dept
           dbgl.fund    = glmf.fund
           dbgl.AMT     = tmpRec.wsAmt * -1
           dbgl.CREDATE = TODAY
           dbgl.DESCRIP = wsNar
           dbgl.period  = wsPer
           dbgl.REF     = wsRef
           dbgl.source  = "RE"
           dbgl.TransID = wsTransId
           dbgl.trDATE  = wsDate
           dbgl.UID     = varUser
           dbgl.UID2    = varUser.
    DISPLAY STREAM a wsDate wsref tmpRec.wscode tmpRec.wsAcc tmpRec.wsAmt tmpRec.wsVat WITH FRAME frmRpt.
    DOWN STREAM a WITH FRAME frmRpt.
    DELETE tmpRec.
END.
/* create Bank Transaction */
FIND FIRST cbkmf WHERE cbkmf.bank = varBank NO-ERROR.
cbkmf.Bal = cbkmf.Bal - wsJAmt.
CREATE cbTrans.
ASSIGN CBTrans.bank    = varBank
       CBTrans.Accper = wsper
       CBTrans.amount = wsJAmt * -1
       CBTrans.Descrip = wsNar
       CBTrans.Dept   = 0
       CBTrans.Fund    = 0
       CBTrans.Ledger  = 99
       CBTrans.Proj    = 0
       CBTrans.OpCode  = varUser
       CBTrans.PoDate  = TODAY 
       CBTrans.prog    = "recupd.p"
       CBTrans.Ref     = wsRef
       CBTrans.TransID  = wsTransId
       CBTrans.TranType = 1
       CBTrans.trDate   = wsDate
       CBTrans.UID      = varUser
       CBTrans.UID2     = varUser.
END.
{glcon.i}
RELEASE CBKMF.
RELEASE CBTRANS.
RELEASE GLTDF.
RELEASE GLBAL.
RETURN.
END.
