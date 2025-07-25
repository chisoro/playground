/* Program.................hse04.p
   Notes:...... ...........Adjusting Journal Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.

&SCOPED-DEFINE tmptable            hsecmf
&SCOPED-DEFINE skey               hsecmf.dbacc

{varlib.i}
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR wsTransId AS DEC FORM "99999999999999". 
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsOpt AS INT VIEW-AS COMBO-BOX SIZE 15 BY 2 
                LIST-ITEM-PAIRS "Capital Journal ", 1, "Interest Journal", 2, "Deposit Journal", 3.
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsAcc     LIKE hsecmf.dbacc.
DEF VAR wsAmt     LIKE hsecmf.bal.
DEF VAR wsVat    LIKE wsAmt.
DEF VAR wsTotal  LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsDr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsCr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsAmt.
DEF VAR wsTCr    LIKE wsAmt.
DEF VAR wsRate    LIKE tblForex.decRate.
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsSeq     AS INT.
DEF VAR vatLedger LIKE glmf.acct.
DEF VAR crLedger  LIKE glmf.acct.
DEF VAR drLedger  LIKE glmf.acct.
DEF VAR wsMcno    AS INT.
DEF VAR wsRecNo   LIKE hsehtf.ref.
DEF VAR wsCur     LIKE hsesch.txtCur.
DEF VAR wsstatus  AS CHAR FORM "X(40)".
DEF VAR  varDescrip LIKE hsehtf.descrip.
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR wsm AS INT.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "HS".
DEF VAR wsq       AS DEC EXTENT 5.
DEF VAR X AS INT.
DEF VAR t AS INT.

/*DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "UPDATE".
DEF BUTTON btnAcc     LABEL "ACCOUNT".
DEF BUTTON btnCur     LABEL "CURRENCY".
*/
DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 12.5.

DEF    QUERY qry-pickAcc FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
        DISPLAY bfr{&tmptable}.dbAcc bfr{&tmptable}.Name bfr{&tmptable}.StandNo bfr{&tmptable}.scheme 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
        brw-pickAcc AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5 LABEL "OK"
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

DEFINE FRAME frm-main
    SKIP(1)
    wsOpt     COLON 30 LABEL "Journal Type" SKIP(.5)
    wsPer     COLON 30 LABEL "Accounting Period" SKIP(.5)
    wsDate    COLON 30 LABEL "Enter Transaction Date" SKIP(.5)
    BtnAcc    COLON 17.5        wsAcc NO-LABEL bfr{&tmptable}.NAME NO-LABEL VIEW-AS TEXT SKIP(.5)
    bfr{&tmptable}.scheme COLON 30 NO-LABEL VIEW-AS TEXT hsesch.descrip NO-LABEL VIEW-AS TEXT SKIP(.5)
    hsehtf.ref    COLON 30 LABEL "Reference" SKIP(0.5)
    hsehtf.descrip COLON 30 LABEL "Narration" SKIP(.5)
    wsAmt  COLON 30 LABEL "Amount" SKIP(.5)
    skip(0.5)
    btn-ok AT ROW 14.5 COL 10
    btn-close colon 70
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 14 COL 3
    with SIZE 99.8 BY 17.5 view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LAND SALES JOURNAL ADJUSTMENTS".

FORM dbgl.fund          LABEL "FUND"
     dbgl.Proj          LABEL "PROJECT"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT"
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "     LAND SALES JOURNAL ADJUSTMENT CONSOLIDATION:  " AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

ON 'enter':U OF wsPer IN FRAME frm-Main
DO:
    ASSIGN wsper.
    IF wsPer  <= SIMCTR.CLOSEPER OR wsPer > SIMCTR.CURPER THEN DO:
        MESSAGE "Invalid Accounting Period...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'TAB':U OF wsDate IN FRAME frm-main
   OR 'enter':U OF wsDate IN FRAME frm-main
    OR 'leave':U OF wsDate IN FRAME frm-main
DO:
    ASSIGN wsDate.
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
    THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF YEAR(wsDate) <> INT(SUBSTR(STRING(wsPer),1,4)) AND MONTH(wsDate)<> INT(SUBSTR(STRING(wsPer),5,2)) THEN DO:
        MESSAGE "Your date is not within the Accounting Period...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnAcc IN FRAME frm-Main
DO: 
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-pickAcc FOR EACH bfr{&tmptable} NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-pickAcc
          OR 'mouse-select-dblclick' OF brw-pickAcc.
  CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsAcc.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-pickAcc 
    OR 'mouse-select-dblclick' OF brw-pickAcc 
DO: 
   GET CURRENT qry-pickAcc  NO-LOCK NO-WAIT.
   DISPLAY  bfr{&tmptable}.dbacc @ wsAcc bfr{&tmptable}.NAME WITH FRAME frm-Main.
   RETURN.
END.

ON 'enter':U OF wsAcc IN FRAME frm-main 
   OR 'tab':U OF wsAcc IN FRAME frm-main
DO:
    wsAcc = DEC(wsAcc:SCREEN-VALUE).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbacc = DEC(wsAcc:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE bfr{&tmptable} THEN DO:
        MESSAGE "Account does not exist.....Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE bfr{&tmptable} THEN DO:
        FIND FIRST hsesch WHERE hsesch.scheme = bfr{&tmptable}.scheme NO-LOCK NO-ERROR.
        DISPLAY bfr{&tmptable}.NAME bfr{&tmptable}.scheme hsesch.descrip WITH FRAME frm-main.
        FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN
            wsRate = tblForex.decRate.
          ELSE IF NOT AVAILABLE tblForex THEN DO:
               FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
               wsRate = tblForexH.decRate.
            END.
    END.
    RETURN.
END.


ON CHOOSE OF btn-Ok IN FRAME frm-main DO:
     ASSIGN wsAmt
            wsOpt wsdate wsper
            wsbtn = "EDIT".
     IF wsper = 0 THEN
         wsper = simctr.curper.
     IF wsOpt = 1 THEN
        RUN CJnl.ip.
     ELSE IF wsOpt = 2 THEN
        RUN IJnl.ip.
     ELSE IF wsOpt = 3 THEN
        RUN DJnl.ip.
  RELEASE bfr{&tmptable}.
  CLEAR FRAME frm-main ALL.
  VIEW FRAME frm-Main.
  APPLY 'entry' TO wsPer.
END.

{audit.i}
/********** MAIN LOGIC **********/
wsSource = "HS".
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsDate:SCREEN-VALUE = STRING(TODAY)
       wsTime    = STRING(TIME,"HH:MM:SS").
       wsTransID = DEC (string(YEAR(TODAY),"9999")
                 + string(MONTH(TODAY),"99" )
                 + string(DAY(TODAY),"99")
                 + SUBSTR(STRING(wsTIME),1,2) 
                 + SUBSTR(STRING(wsTIME),4,2) 
                 + SUBSTR(STRING(wsTIME),7,2) ).
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
IF CAN-FIND (FIRST dbgl WHERE dbgl.TransID = wsTransID AND dbgl.SOURCE = wsSource NO-LOCK) THEN
DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Consol.ip"
                    &paged}
END.
HIDE FRAME frm-main.


PROCEDURE CJnl.ip:
    wsVat = 0.
     ASSIGN varDescrip = "Capital Adjustment" WHEN hsehtf.descrip:SCREEN-VALUE IN FRAME frm-Main = ""
            varDescrip = hsehtf.descrip:SCREEN-VALUE WHEN hsehtf.descrip:SCREEN-VALUE <> ""
            crLedger   = hsesch.ILedger
            drLedger   = Hsesch.CLedger
            wsVat      = ROUND((wsAmt * (bfr{&tmptable}.Vat / (100 + bfr{&tmptable}.Vat))) ,2).
     IF wsAmt <> 0 THEN DO:
            ASSIGN bfr{&tmptable}.PPrice = bfr{&tmptable}.PPrice + wsAmt
                   bfr{&tmptable}.Bal    = bfr{&tmptable}.Bal + wsAmt
                   wsRate = PRate
                   bfr{&tmptable}.LBal = bfr{&tmptable}.LBal + ROUND((wsAmt * PRate),2).
            RUN writ.ip.
     END.
END PROCEDURE. /*.. EOP */

PROCEDURE IJnl.ip: 
    wsVat = 0.
     ASSIGN varDescrip = "Interest Adjustment" WHEN hsehtf.descrip:SCREEN-VALUE IN FRAME frm-Main = ""
            varDescrip = hsehtf.descrip:SCREEN-VALUE WHEN hsehtf.descrip:SCREEN-VALUE <> ""
            crLedger   = hsesch.IntLedger
            drLedger   = Hsesch.CLedger.
     IF wsAmt <> 0 THEN DO:
            ASSIGN bfr{&tmptable}.Bal  = bfr{&tmptable}.Bal + wsAmt
                   bfr{&tmptable}.LBal = bfr{&tmptable}.LBal + ROUND((wsAmt * wsRate),2).
            RUN writ.ip.
     END.
END PROCEDURE. /*.. EOP */

PROCEDURE DJnl.ip:
    wsVat = 0.
     ASSIGN varDescrip = "Deposit Adjustment" WHEN hsehtf.descrip:SCREEN-VALUE IN FRAME frm-Main  = ""
            varDescrip = hsehtf.descrip:SCREEN-VALUE WHEN hsehtf.descrip:SCREEN-VALUE <> ""
            crLedger   = hsesch.CLedger
            drLedger   = Hsesch.CtrLedger.
     IF wsAmt <> 0 THEN DO:
            ASSIGN bfr{&tmptable}.Bal          = bfr{&tmptable}.Bal - wsAmt
                   bfr{&tmptable}.LBal = bfr{&tmptable}.LBal - ROUND((wsAmt * wsRate),2)
                   bfr{&tmptable}.AmtDue[1]    = bfr{&tmptable}.AmtDue[1] + wsAmt
                   bfr{&tmptable}.AmtDue[2]    = bfr{&tmptable}.AmtDue[2]  + ROUND((wsAmt * wsRate),2)
                   bfr{&tmptable}.LAmtDue      = bfr{&tmptable}.LAmtDue + ROUND((wsAmt * wsRate),2).
            MESSAGE wsRate VIEW-AS ALERT-BOX.
            RUN writ.ip.
     END.
END PROCEDURE. /*.. EOP */

PROCEDURE Writ.ip:
    IF wsAmt <> 0.00 AND wsOpt = 3 THEN DO:
       FIND FIRST glmf WHERE glmf.acct = crLedger NO-LOCK NO-ERROR.
       CREATE hsemtf.   /* create monthly transaction */
       ASSIGN hsemtf.dbacc   = bfr{&tmptable}.dbacc
              hsemtf.scheme  = hsesch.scheme
              hsemtf.Accper  = wsPer
              hsemtf.Amt     = wsAmt
              hsemtf.Vat     = ROUND((wsAmt * (bfr{&tmptable}.Vat / (100 + bfr{&tmptable}.Vat))) ,2)
              hsemtf.CRate   = wsRate
              hsemtf.ILedger = crLedger
              hsemtf.proj    = glmf.Proj
              hsemtf.dept    = glmf.dept
              hsemtf.fund    = glmf.fund
              hsemtf.trDate  = wsDate
              hsemtf.ref     = hsehtf.ref:SCREEN-VALUE IN FRAME frm-main
              hsemtf.DESCRIP = varDescrip.       
       CREATE hsehtf.   /* History record */
       ASSIGN hsehtf.Accper  = wsPer
              hsehtf.dbacc   = bfr{&tmptable}.dbacc
              hsehtf.scheme  = hsesch.scheme
              hsehtf.Amt     = wsAmt
              hsehtf.Vat     = ROUND((wsAmt * (bfr{&tmptable}.Vat / (100 + bfr{&tmptable}.Vat))) ,2)
              hsehtf.CRate   = wsRate
              hsehtf.tRate   = wsRate
              hsehtf.txtCur  = wsCur
              hsehtf.Iledger = crLedger
              hsehtf.proj    = glmf.Proj
              hsehtf.dept    = glmf.dept
              hsehtf.fund    = glmf.fund
              hsehtf.ref     = hsehtf.ref:SCREEN-VALUE IN FRAME frm-main
              hsehtf.trDate  = wsDate
              hsehtf.PoDate  = TODAY
              hsehtf.prog    = "hse04.p"
              hsehtf.TransID = wsTransId  
              hsehtf.uid     = varUser
              hsehtf.DESCRIP = varDescrip.
       FIND FIRST hseblf WHERE hseblf.scheme = bfr{&tmptable}.scheme AND hseblf.dbacc = bfr{&tmptable}.dbacc NO-ERROR. /* Age analysis data */
       IF NOT AVAILABLE hseblf THEN DO:
            CREATE hseblf.
            ASSIGN hseblf.scheme = bfr{&tmptable}.scheme
                   hseblf.dbacc  = bfr{&tmptable}.dbacc.
       END.
       ASSIGN  hseblf.amt[1] = hseblf.amt[1] + wsAmt
               hseblf.vat    = hseblf.vat + hsehtf.Vat.
    END.
    IF wsAmt <> 0.00 THEN DO:
                /* create record for income Ledger transaction */
        FIND FIRST glmf WHERE glmf.acct     = crLedger NO-LOCK NO-ERROR.
        FIND FIRST dbgl WHERE dbgl.acct     = crLedger  AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId AND dbgl.proj   = glmf.proj
                           AND dbgl.dept    = glmf.dept AND dbgl.fund   = glmf.fund
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
                      dbgl.REF      = "hse04.p"
                      dbgl.DESCRIP  = varDescrip
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + (ROUND((wsAmt * wsRate * -1),2) - ROUND((wsVat * wsRate * -1),2)).
               /* Create VAT Provision Ledger  Record ???? This must have been created at sales*/
        IF  wsVat <> 0 THEN DO:
           FIND FIRST dbgl WHERE dbgl.period = wsper  AND dbgl.TransID = wsTransId
                             AND dbgl.acct = simctr.vat[1] AND dbgl.proj = glmf.proj
                             AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = simctr.vat[1]
                          dbgl.proj     = glmf.proj
                          dbgl.dept     = glmf.dept
                          dbgl.fund     = glmf.fund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = "hse04.p"
                          dbgl.DESCRIP  = varDescrip
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
                END.
                ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsVat * wsRate * -1),2).   
        END. 
       /* Create record for the Contra Ledger record */
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
                      dbgl.REF      = "hse04.p"
                      dbgl.DESCRIP  = varDescrip
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsAmt * wsRate),2).
    END. /* eo ledger creation */
    RETURN.
END PROCEDURE.

PROCEDURE Consol.ip:
     {glcon.i}
END.
