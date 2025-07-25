/* Program.................hse06.p
   Notes:...... Land Sale interest raising and Instalment raising
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsInstal  LIKE hsecmf.bal.
DEF VAR wsInt    LIKE hsecmf.bal.
DEF VAR wsTear    LIKE dbtmf.Tear.
DEF VAR wsVat     LIKE wsInstal.
DEF VAR wsTotal   LIKE wsInstal.
DEF VAR wsTVat    LIKE wsInstal.
DEF VAR wsDr      LIKE wsInstal.
DEF VAR wsCr      LIKE wsInstal.
DEF VAR wsTDr     LIKE wsInstal.
DEF VAR wsTCr     LIKE wsInstal.
DEF VAR wsRate    LIKE tblForex.decRate.
DEF VAR wsFreq    AS INT.
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsSeq     AS INT.
DEF VAR vatLedger LIKE glmf.acct.
DEF VAR crLedger  LIKE glmf.acct.
DEF VAR drLedger  LIKE glmf.acct.
DEF VAR wsStart   LIKE hsesch.scheme.
DEF VAR wsEnd     LIKE hsesch.scheme.
DEF VAR st-acc    LIKE hsecmf.dbacc.
DEF VAR ed-acc    LIKE hsecmf.dbacc.
DEF VAR wsstatus  AS CHAR FORM "X(40)".
DEF VAR  varDescrip LIKE hsehtf.descrip.
DEF VAR wsm AS INT.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "HS".
DEF VAR wsq       AS DEC EXTENT 5.
DEF VAR X AS INT.
DEF VAR t AS INT.

DEF TEMP-TABLE  hsetmp
    FIELD dbacc  LIKE hsecmf.dbacc
    FIELD NAME   LIKE hsecmf.NAME
    FIELD bal    LIKE hsecmf.bal
    FIELD InsAmt LIKE hsecmf.bal
    FIELD Due    LIKE hsecmf.bal
    FIELD RDue       LIKE hsecmf.bal.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEFINE FRAME frm-main
    SKIP(1)
    wsPer     COLON 30 LABEL "Enter Accounting Period" SKIP(.5)
    wsDate    COLON 30 LABEL "Enter Transaction Date" SKIP(.5)
    wsStart   COLON 30 LABEL "Enter Start Scheme" SKIP(.5)
    wsEnd     COLON 30 LABEL "Enter End Scheme" SKIP(0.5)
    st-acc    COLON 30 LABEL "Enter Start Account" SKIP(.5)
    ed-acc     COLON 30 LABEL "Enter End Account" SKIP(0.5)
    wsStatus  COLON 20 view-as text no-label no-tab-stop
    skip(1)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15 view-as dialog-box keep-tab-order no-validate
      BGCOLOR 3    side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LAND SALES INTEREST AND INSTALMENT RAISING".

FORM SKIP(2)
     hsesch.scheme     LABEL "SCHEME"
     hsesch.Descrip    LABEL "DESCRIPTION"
     Hsecmf.Bal        COLUMN-LABEL "CAPITAL !BALANCE"
     wsInt             LABEL "INTEREST"
     Hsecmf.AmtDue[1]  COLUMN-LABEL "REVENUE !BALANCE"
     Hsecmf.AmtDue[2]  COLUMN-LABEL "ACCOUNTING !BALANCE"
    HEADER skip(4) "                         LAND SALES INTEREST RAISING:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(3) 
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt1.

FORM 
     hsesch.scheme     LABEL "SCHEME"
     hsesch.Descrip    LABEL "DESCRIPTION"
     Hsecmf.Bal        LABEL "CAPITAL"
     Hsecmf.InsAmt     LABEL "INSTALMENT"
     Hsecmf.AmtDue[1]  LABEL "AMOUNT DUE"
     Hsecmf.AmtDue[2]  LABEL "RTGS DUE"
    HEADER skip(2) "                         LAND SALES INTEREST AND INSTALMENT RAISING:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(3) 
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

FORM dbgl.fund          LABEL "FUND"
     dbgl.Proj          LABEL "PROJECT"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT"
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "                       DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.


ON 'tab' OF wsPer IN FRAME frm-main
    OR 'enter' OF wsPer IN FRAME frm-main
DO:
  FIND FIRST SIMCTR NO-LOCK NO-ERROR .
  IF INT(wsPer:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(wsPer:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
END.

ON 'TAB':U OF wsDate IN FRAME frm-main
   OR 'enter':U OF wsDate IN FRAME frm-main
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN vatLedger = simctr.vat[2]
            wsdate
            wsStart = DEC(wsStart:SCREEN-VALUE)
            wsEnd =   DEC(wsEnd:SCREEN-VALUE)
            wsPer = INT(wsPer:SCREEN-VALUE)
            wsTime    = STRING(TIME,"HH:MM:SS")
            st-acc = DEC(st-acc:SCREEN-VALUE)
            ed-acc = DEC(ed-acc:SCREEN-VALUE) .
            wsTransID = DEC (string(YEAR(TODAY),"9999")
                          + string(MONTH(TODAY),"99" )
                          + string(DAY(TODAY),"99")
                          + SUBSTR(STRING(wsTIME),1,2) 
                          + SUBSTR(STRING(wsTIME),4,2) 
                          + SUBSTR(STRING(wsTIME),7,2) ).
    IF wsPer > SIMCTR.CURPER OR wsPer <= SIMCTR.CLOSEPER THEN DO:
        MESSAGE "Accounting Period entered is in valid please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        a = ETIME(YES).
        {PrintOpt.i &stream-name="stream a"
                    &print-prog="Interest.ip"
                    &paged}
    END.
    APPLY 'entry' TO btn-close IN FRAME frm-main.
    RETURN.
END.

/********** MAIN LOGIC **********/
wsSource = "HS".
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsStart:SCREEN-VALUE = "0"
       wsEnd:SCREEN-VALUE = "999999"
       st-acc:SCREEN-VALUE = "0"
       ed-acc:SCREEN-VALUE = "999999999998".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.


PROCEDURE Interest.ip:
/*DO TRANSACTION ON ERROR UNDO, LEAVE: */
FOR EACH hsesch WHERE hsesch.scheme >= wsStart AND hsesch.scheme <= wsEnd:
     wsInt = 0.
    FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
           wsRate = tblForex.decRate.
    END.
    ELSE DO:
        FIND LAST tblForexH WHERE tblForexH.txtCur =  hsesch.txtCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
               wsRate = tblForexH.decRate.
    END.
    wsStatus = "Process Interest on Capital......" + STRING(hsesch.scheme).
    DISPLAY wsStatus WITH FRAME frm-Main.
    IF hsesch.IntRate = 0 THEN NEXT.
    IF CAN-FIND (FIRST hsecmf WHERE hsecmf.scheme = hsesch.scheme AND hsecmf.bal > 0) THEN DO:
         DISPLAY STREAM a hsesch.scheme hsesch.Descrip WITH FRAME frm-rpt1.
         DOWN STREAM a WITH FRAME frmp-rpt1.
    END.
    FOR EACH hsecmf WHERE hsecmf.scheme = hsesch.scheme AND Hsecmf.AccStat = 0
           AND hsecmf.dbacc >= st-acc AND hsecmf.dbacc <= ed-acc:
        wsInt = 0.
        IF CAN-FIND (FIRST hsehtf WHERE hsehtf.dbacc = hsecmf.dbacc 
                     AND hsehtf.Accper = wsPer AND hsehtf.prog = "hse06.p"
                     AND SUBSTR(hsehtf.ref,1,4) = "Inst") THEN NEXT. /* skip if billed already */
       ASSIGN wsInt = Hsecmf.Bal * (hsesch.IntRate / 1200) WHEN hsesch.Freq = "M"
              wsInt = Hsecmf.Bal * (hsesch.IntRate / 400) WHEN hsesch.Freq = "Q"
              wsInt = Hsecmf.Bal * (hsesch.IntRate / 200) WHEN hsesch.Freq = "H"
              wsInt = Hsecmf.Bal * (hsesch.IntRate / 100) WHEN hsesch.Freq = "Y".
        IF wsInt <> 0 THEN DO:
            ASSIGN Hsecmf.Bal = Hsecmf.Bal + wsInt
                   Hsecmf.LBal = Hsecmf.LBal + ROUND((wsInt * wsRate),2)
                   varDescrip = "Interest on Capital"
                   crLedger   = hsesch.IntLedger
                   drLedger   = hsesch.CLedger.
            RUN writ.ip.
        END.
    END.
END. /* eof hsesch */
wsInt = 0.
RUN Bill.ip.
END PROCEDURE.

PROCEDURE Bill.ip:
/*DO TRANSACTION ON ERROR UNDO, LEAVE: */
FOR EACH hsesch WHERE hsesch.scheme >= wsStart AND hsesch.scheme <= wsEnd:
    wsInstal = 0.
    FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
           wsRate = tblForex.decRate.
    END.
    ELSE DO:
        FIND LAST tblForexH WHERE tblForexH.txtCur =  hsesch.txtCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
               wsRate = tblForexH.decRate.
    END.
    wsStatus = "Process Instalments......" + STRING(hsesch.scheme).
    DISPLAY wsStatus WITH FRAME frm-Main.
    FOR EACH hsecmf WHERE hsecmf.scheme = hsesch.scheme AND hsecmf.dbacc >= st-acc AND hsecmf.dbacc <= ed-acc
                      AND Hsecmf.AccStat = 0 AND hsecmf.bal <> 0:
        wsInstal = 0.
        /*IF CAN-FIND (FIRST hsehtf WHERE hsehtf.dbacc = hsecmf.dbacc 
                     AND hsehtf.Accper = wsPer AND hsehtf.prog = "hse06.p"
                     AND SUBSTR(hsehtf.ref,1,4) = "Inst") THEN NEXT. *//* skip if billed already */
        ASSIGN wsInstal   = Hsecmf.InsAmt WHEN Hsecmf.InsAmt < Hsecmf.Bal
               wsInstal   = Hsecmf.Bal    WHEN Hsecmf.InsAmt >= Hsecmf.Bal
               wsVat      = ROUND(wsInstal * (Hsecmf.Vat / (Hsecmf.Vat + 100)),2) 
               varDescrip = "Instalment Raising"
               crLedger   = hsesch.CLedger
               drLedger   = hsesch.CtrLedger.
        IF wsInstal <> 0 THEN DO:
            RUN writ.ip.
            ASSIGN Hsecmf.Bal = Hsecmf.Bal - wsInstal
                   Hsecmf.LBal = Hsecmf.LBal - ROUND((wsInstal * wsRate),2)
                   Hsecmf.Period = Hsecmf.Period - 1 WHEN  Hsecmf.Period >= 1
                   Hsecmf.AmtDue[1] = Hsecmf.AmtDue[1] + wsInstal
                   Hsecmf.AmtDue[2] = Hsecmf.AmtDue[2] + ROUND((wsInstal * wsRate),2)
                   Hsecmf.LAmtDue  = Hsecmf.LAmtDue + ROUND((wsInstal * wsRate),2)
                   Hsecmf.LTAmt     = wsInstal
                   Hsecmf.LTDate    = wsDate
                   Hsecmf.LTRate    = wsRate.
            CREATE hsetmp.
            ASSIGN hsetmp.dbacc = hsecmf.dbacc
                   hsetmp.NAME  = hsecmf.NAME
                   hsetmp.bal   = hsecmf.bal
                   hsetmp.InsAmt = wsInstal
                   hsetmp.Due   = Hsecmf.AmtDue[1]
                   hsetmp.RDue  = ROUND((Hsecmf.AmtDue[1] * wsRate),2).
        END.
    END.
END. /* eof hsesch */
FOR EACH hsetmp:
     DISPLAY STREAM a STRING(hsetmp.dbacc) + "  " + hsetmp.NAME @ hsesch.Descrip  
                 hsetmp.bal @ Hsecmf.Bal hsetmp.InsAmt @ Hsecmf.InsAmt  hsetmp.Due @ Hsecmf.AmtDue[1] 
                 hsetmp.RDue @ Hsecmf.AmtDue[2] WITH FRAME frm-rpt.
     DOWN STREAM a WITH FRAME frm-rpt.
END.
PAGE STREAM a.
{glcon.i}
END PROCEDURE. /*.. EOP */

PROCEDURE Writ.ip:
    IF wsInstal <> 0.00 AND varDescrip <> "Exchange Gain/(loss)" THEN DO:
       FIND FIRST glmf WHERE glmf.acct = crLedger NO-LOCK NO-ERROR.
       CREATE hsemtf.   /* create monthly transaction */
       ASSIGN hsemtf.dbacc   = hsecmf.dbacc
              hsemtf.scheme  = hsesch.scheme
              hsemtf.Accper  = wsPer
              hsemtf.Amt     = wsInstal
              hsemtf.Vat     = wsVat
              hsemtf.CRate   = wsRate
              hsemtf.ILedger = crLedger
              hsemtf.proj    = glmf.Proj
              hsemtf.dept    = glmf.dept
              hsemtf.fund    = glmf.fund
              hsemtf.trDate  = wsDate
              hsemtf.ref     = "Instal" + string(wsPer)
              hsemtf.DESCRIP = varDescrip.       
       CREATE hsehtf.   /* History record */
       ASSIGN hsehtf.Accper  = wsPer
              hsehtf.dbacc   = hsecmf.dbacc
              hsehtf.scheme  = hsesch.scheme
              hsehtf.Amt     = wsInstal
              hsehtf.Vat     = wsVat
              hsehtf.CRate   = wsRate
              hsehtf.tRate   = wsRate
              hsehtf.txtCur  = hsesch.txtCur
              hsehtf.Iledger = crLedger
              hsehtf.proj    = glmf.Proj
              hsehtf.dept    = glmf.dept
              hsehtf.fund    = glmf.fund
              hsehtf.ref     = "Instal" + string(wsPer)
              hsehtf.trDate  = wsDate
              hsehtf.PoDate  = TODAY
              hsehtf.prog    = "hse06.p"
              hsehtf.TransID = wsTransId  
              hsehtf.uid     = varUser
              hsehtf.DESCRIP = varDescrip. 
        FIND FIRST hseblf WHERE hseblf.scheme = hsecmf.scheme AND hseblf.dbacc = hsecmf.dbacc NO-ERROR. /* Age analysis data */
        IF NOT AVAILABLE hseblf THEN DO:
            CREATE hseblf.
            ASSIGN hseblf.scheme = hsecmf.scheme
                   hseblf.dbacc  = hsecmf.dbacc.
        END.
        ASSIGN hseblf.vat = hseblf.vat + hsehtf.Vat
               hseblf.amt[1] = hseblf.amt[1] + wsInstal.
    END.
    IF (wsInstal <> 0.00 OR wsInt <> 0.00) THEN DO:
                /* create record for Capital or Interest Ledger transaction */
        FIND FIRST glmf WHERE glmf.acct = crLedger NO-LOCK NO-ERROR.
        FIND FIRST dbgl WHERE dbgl.acct = crLedger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund
                           AND dbgl.descrip = varDescrip NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = crLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = wsper
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "hse06.p"
                      dbgl.DESCRIP  = varDescrip
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsInstal * wsRate * -1),2) WHEN wsInstal <> 0
               dbgl.AMT = dbgl.AMT + ROUND((wsInt    * wsRate * -1),2) WHEN wsInt    <> 0.
  
       /* Create record for the Debtors Control Ledger record */
         FIND FIRST glmf WHERE glmf.acct = drLedger NO-LOCK NO-ERROR.
        FIND FIRST dbgl WHERE dbgl.acct = drLedger AND dbgl.period = wsper
                          AND dbgl.TransID = wsTransId AND dbgl.dept = glmf.dept
                          AND dbgl.proj = glmf.proj AND dbgl.fund = glmf.fund
                          AND dbgl.descrip = varDescrip NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = drLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = wsper
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "hse06.p"
                      dbgl.DESCRIP  = varDescrip
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsInstal * wsRate),2) WHEN wsInstal <> 0
               dbgl.AMT = dbgl.AMT + ROUND((wsInt * wsRate),2)    WHEN wsInt <> 0.
        ASSIGN wsTotal = wsTotal + ROUND((wsInstal * wsRate),2) + ROUND((wsInt * wsRate),2).
    END. /* eo ledger creation */
    RETURN.
END PROCEDURE.

