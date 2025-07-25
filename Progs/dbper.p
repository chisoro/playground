/* Program.................dbper.p
   Notes:...... DEBTORS PERIOD CLOSURE
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF STREAM a.
def  var w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
def  var wsstatus  AS CHAR FORM "X(40)".
def  var varUser LIKE simusr.usercode.
def  var X AS INT.
def  var a AS INT.
def  var wsper LIKE simctr.curper.
def  var wsDate AS DATE.
def  var wsExAmt AS DEC FORM "zzz,ZZZ,ZZZ,ZZ9.99-".
def  var wsAmt      LIKE wsExAmt.
def  var wsDr      LIKE wsExAmt.
def  var wsCr      LIKE wsExAmt.
def  var wsTDr     LIKE wsExAmt.
def  var wsTCr     LIKE wsExAmt.
def  var wsLedger LIKE glmf.acct.
def  var wsProj LIKE glmf.proj.
def  var wsFund LIKE glmf.Fund.
def  var wsDept LIKE glmf.Dept.
def  var dbRate LIKE tblForex.decRate.
def  var wsTransId LIKE gltdf.transid.
def  var wsTime   AS CHAR FORM "x(8)".
def  var wsSource LIKE dbgl.SOURCE  INITIAL "DB".

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

FORM dbgl.Proj          LABEL "PROJECT"
     dbgl.Fund          LABEL "FUND"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT" 
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "         DEBTORS PERIOD CLOSE EXCHANGE GAIN/LOSS CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

DEFINE  FRAME frm-main
    SKIP(3)
    wsPer    COLON 30 LABEL "Enter Debtors Period"
    SKIP(1)
    wsDate  COLON 30 LABEL "Enter Transaction Date"
     SKIP(3)
    wsStatus COLON 30 LABEL "Processing...." VIEW-AS TEXT
    btn-ok AT ROW 12.5 COL 15
    btn-close colon 70
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15.1
    view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DEBTORS PERIOD CLOSURE".

ON 'enter':U OF wsPer IN FRAME frm-main
    OR 'leave':U OF wsper IN FRAME frm-main
DO:
    FIND FIRST simctr NO-LOCK NO-ERROR.
    ASSIGN wsPer.
    /*IF wsPer <= simctr.curPer THEN DO:
        MESSAGE "This Period has been Closed, ...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. */
    RETURN.
END.

ON 'TAB':U OF wsDate IN FRAME frm-main
   OR 'enter':U OF wsDate IN FRAME frm-main
   OR 'leave':U OF wsDate IN FRAME frm-main
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <> wsPer
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN wsDate.
         wsTime    = STRING(TIME,"HH:MM:SS").
         wsTransID = DEC (string(YEAR(TODAY),"9999")
                      + string(MONTH(TODAY),"99" )
                      + string(DAY(TODAY),"99")
                      + SUBSTR(STRING(wsTIME),1,2) 
                      + SUBSTR(STRING(wsTIME),4,2) 
                      + SUBSTR(STRING(wsTIME),7,2) ). 
         
    END.
    RETURN.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
    ASSIGN wsdate = DATE(wsDate:SCREEN-VALUE).
    FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
            ASSIGN dbRate = tblForex.decRate. 
         END.
         IF NOT AVAILABLE tblForex  THEN DO:
             FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                    ASSIGN dbRate = tblForexH.decRate.
                END.
         END.
    RUN perClose-ip.
    APPLY 'close' TO THIS-PROCEDURE.
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsPer = simctr.curper
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsPer:SCREEN-VALUE = STRING(wsPer).
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
IF CAN-FIND (FIRST dbgl WHERE dbgl.TransID = wsTransID AND dbgl.SOURCE = wsSource NO-LOCK) THEN
DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Consol.ip"
                    &paged}
END.
HIDE FRAME frm-main.

PROCEDURE perClose-ip:
   FOR EACH dbcmf USE-INDEX acc:
        wsStatus = "Updating balance b/f Account.... " + STRING(dbcmf.dbAcc).
        DISPLAY wsStatus WITH FRAM frm-main. PAUSE 0.
        dbcmf.BalBf = dbcmf.AccBal.
    END.
    RELEASE dbcmf.
    FOR EACH dbblf BREAK BY dbblf.sgrp: /* age the debtors */
        IF FIRST-OF(dbblf.sgrp) THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.sgrp = dbblf.sgrp NO-LOCK NO-ERROR.
            FIND FIRST gldept WHERE gldept.dept = dbsgr.dept NO-LOCK NO-ERROR.
            IF AVAILABLE gldept THEN
                ASSIGN wsfund = gldept.fund.
            ELSE
                ASSIGN wsfund = 0.
            ASSIGN wsAmt = 0
                   wsExAmt = 0.
        END.
        wsStatus = "Ageing Account.... " + STRING(dbblf.dbAcc).
        DISPLAY wsStatus WITH FRAM frm-main. PAUSE 0.
        ASSIGN X = 14
           amt[15] = amt[15] + amt[14].
        DO WHILE X >= 3:
           ASSIGN amt[X] = amt[X - 1].
                  X = X - 1.
        END.
        IF amt[1] >= 0 THEN
           ASSIGN amt[2] = amt[1]
                  amt[1] = 0.
        ELSE ASSIGN amt[2] = 0.
        ASSIGN wsExAmt = wsExAmt + ROUND((dbAmt[1] * dbRate),2) - dbAmt[2]
               dbAmt[2] = ROUND((dbAmt[1] * dbRate),2).
               IF wsExAmt = ? THEN
                  wsExAmt = 0.
               wsAmt = wsAmt + wsExAmt.
              
        IF LAST-OF(dbblf.sgrp) AND wsExAmt <> 0.00 THEN DO:
             RUN Exchange.ip.
        END.

    END.
    RELEASE dbblf.
    FOR EACH dbmtf: /* clear monthly transaction table */
        wsStatus = "Clearing monthly transaction table" + STRING(dbmtf.dbAcc).
        DISPLAY wsStatus WITH FRAM frm-main.
        DELETE dbmtf.
    END.
    MESSAGE "Processing completed..." VIEW-AS ALERT-BOX.
    RETURN.
END PROCEDURE.

PROCEDURE  Exchange.ip:
    DO X = 1 TO 2:
            ASSIGN wsLedger = simctr.exLedger WHEN X = 1
                   wsLedger = dbsgr.ctrLedger WHEN X = 2.
            FIND FIRST dbgl WHERE dbgl.acct = wsLedger  AND dbgl.period = wsper
                          AND dbgl.TransID = wsTransId AND dbgl.dept = dbsgr.dept
                          AND dbgl.proj = dbsgr.proj AND dbgl.fund = wsfund
                          AND dbgl.SOURCE   = "DB"  NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = wsLedger
                          dbgl.proj     = dbsgr.proj
                          dbgl.dept     = dbsgr.dept
                          dbgl.fund     = wsfund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = "dbper.p"
                          dbgl.DESCRIP  = "Exchange Gain" WHEN wsExAmt > 0
                          dbgl.DESCRIP  = "Exchange Loss" WHEN wsExAmt < 0
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = "DB".
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + (wsExAmt * -1) WHEN X = 1
                   dbgl.AMT = dbgl.AMT + wsExAmt        WHEN X = 2. 
        END.
        RETURN.
END PROCEDURE.


PROCEDURE Consol.ip:
     {glcon.i}
END.

