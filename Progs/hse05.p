/* Program.................hse05.p
   Notes:...... ...........Capture Revenue Debtor Journals.
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.

DEF SHARED VAR varUser    AS CHAR FORM "xxx".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsAcc     LIKE hsecmf.dbacc.
DEF VAR wsAmt     LIKE hsecmf.bal.
DEF VAR wsPAmt    LIKE wsAmt.
DEF VAR wsRAmt    LIKE hsecmf.bal.
DEF VAR wsVat    LIKE wsAmt.
DEF VAR wsTotal  LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsDr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsCr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsAmt.
DEF VAR wsTCr    LIKE wsAmt.
DEF VAR wsRate    LIKE tblForex.decRate.
DEF VAR wstRate    LIKE tblForex.decRate.
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsSeq     AS INT.
DEF VAR vatLedger LIKE glmf.acct.
DEF VAR ctrLedger  LIKE glmf.acct.
DEF VAR wsLedger  LIKE glmf.acct.
DEF VAR wsMcno    AS INT.
DEF VAR wsRef     LIKE hsehtf.ref.
DEF VAR wsCur     LIKE hsesch.txtCur.
DEF VAR wsAge     AS INT FORM "Z9".
DEF VAR wsstatus  AS CHAR FORM "X(40)".
DEF VAR varDescrip LIKE hsehtf.descrip.
DEF VAR wsOpt AS INT VIEW-AS COMBO-BOX SIZE 15 BY 2 
                LIST-ITEM-PAIRS "Adjusting Journal ", 1, "Payment Journal", 2.
DEF VAR wstitle AS CHAR FORM "x(80)".
DEF VAR wsm AS INT.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "HS".
DEF VAR wsq       AS DEC EXTENT 5.
DEF VAR X AS INT.
DEF VAR t AS INT.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "UPDATE".
DEF BUTTON btnAcc     LABEL "ACCOUNT".
DEF BUTTON btnCur     LABEL "CURRENCY".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 14.

DEF    QUERY qry-pickAcc FOR hsecmf SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
        DISPLAY hsecmf.dbAcc hsecmf.Name hsecmf.StandNo hsecmf.scheme 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
        brw-pickAcc AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5 LABEL "OK"
        btn-close colon 60
        with view-as dialog-box keep-tab-order NO-VALIDATE BGCOLOR 3 
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

DEF QUERY qry-Cur FOR tblForex SCROLLING.
DEF BROWSE brw-cur QUERY qry-cur
    DISPLAY txtCur FORM "x(10)"
     WITH NO-LABEL 6 DOWN SEPARATORS.

DEFINE FRAME frm-Cur 
    brw-Cur AT ROW 2 COL 5
    skip(0.5)
    btn-ok COLON 10 LABEL "OK"
    with view-as dialog-box keep-tab-order NO-VALIDATE BGCOLOR 3 
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Currency Selection".
 
DEFINE FRAME frm-main
    SKIP(1)
    wsOpt     COLON 30 LABEL "Journal Type" SKIP(.5)
    wsDate     COLON 30 LABEL "Enter Transaction Date" SKIP(.5)
    BtnAcc     COLON 17.5  wsAcc NO-LABEL hsecmf.NAME NO-LABEL VIEW-AS TEXT SKIP(.5)
    hsecmf.scheme COLON 30 NO-LABEL VIEW-AS TEXT hsesch.descrip NO-LABEL VIEW-AS TEXT SKIP(.5)
    wsRef     COLON 30 LABEL "Journal Reference" SKIP(0.5)
    wsLedger   COLON 30 LABEL "Ledger Allocation" SKIP(0.5)
    btnCur    COLON 16        wsCur NO-LABEL  wstRate NO-LABEL VIEW-AS TEXT SKIP(0.5)
    wsAmt  COLON 30 LABEL "Amount" space(10) wsAge LABEL "Age" SKIP(.5)
    varDescrip  COLON 30 LABEL "Narration" FORM "x(60)"
    skip(1.5)
    btn-ok colon 15
    btn-close colon 70
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 15.5 COL 3
    with SIZE 99.8 BY 18.5 view-as dialog-box keep-tab-order no-validate
     BGCOLOR 3  side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LAND SALES REVENUE DEBTORS JOURNAL CAPTURE".

FORM dbgl.fund          LABEL "FUND"
     dbgl.Proj          LABEL "PROJECT"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT"
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "     LAND SALES PAYMENTS CONSOLIDATION:  " AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.


ON 'TAB':U OF wsDate IN FRAME frm-main
   OR 'enter':U OF wsDate IN FRAME frm-main
DO:
    ASSIGN wsDate.
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnAcc IN FRAME frm-Main
DO: 
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-pickAcc FOR EACH hsecmf NO-LOCK.
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
   DISPLAY  hsecmf.dbacc @ wsAcc hsecmf.NAME WITH FRAME frm-Main.
   RETURN.
END.

ON 'enter':U OF wsAcc IN FRAME frm-main 
   OR 'tab':U OF wsAcc IN FRAME frm-main
DO:
    wsAcc = DEC(wsAcc:SCREEN-VALUE).
    FIND FIRST hsecmf WHERE hsecmf.dbacc = DEC(wsAcc:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE hsecmf THEN DO:
        MESSAGE "Account does not exist.....Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
        DISPLAY hsecmf.NAME hsecmf.scheme hsesch.descrip WITH FRAME frm-main.
    END.
    RETURN.
END.

ON CHOOSE OF btnCur IN FRAME frm-Main
DO:
  VIEW FRAME frm-Cur.
  OPEN QUERY qry-Cur FOR EACH tblforex NO-LOCK.
  ENABLE ALL WITH FRAME frm-Cur.
  WAIT-FOR close of THIS-PROCEDURE IN FRAME frm-Cur
          OR CHOOSE OF btn-ok IN FRAME frm-Cur 
          OR 'enter':u OF brw-Cur
          OR 'mouse-select-dblclick' OF brw-Cur.
  CLOSE QUERY qry-Cur.
  HIDE FRAME frm-Cur.
  APPLY 'tab' TO btnCur.
  APPLY 'tab' TO wsCur.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Cur 
    OR 'enter':u OF brw-Cur
    OR 'mouse-select-dblclick' OF brw-Cur
DO: 
   GET CURRENT qry-Cur EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY tblForex.txtCur @ wsCur WITH FRAME frm-Main.
   APPLY 'TAB' TO wsCur IN FRAME frm-Main.
END.

ON 'enter':U OF wsCur IN FRAME frm-Main
    OR 'leave':U OF wsCur IN FRAME frm-Main
DO:
    ASSIGN wsCur
           wsTRate = 1
           wsRate  = 1.
    IF NOT CAN-FIND (FIRST tblForex WHERE tblForex.txtCur = wsCur) THEN DO:
        MESSAGE "Invalid Currency...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    FIND first tblForex WHERE tblForex.txtCur = wsCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
        ASSIGN wstRate = tblForex.decRate.
    END.
    ELSE DO:
       FIND LAST tblForexH WHERE tblForexH.txtCur = wsCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
             wstRate = tblForexH.decRate.
    END.
    DISPLAY wstRate WITH FRAME frm-Main.
    RETURN.
END.

ON 'enter':U OF wsAmt IN FRAME frm-Main 
    OR 'leave':U OF wsAmt IN FRAME frm-Main
DO:
    IF wsCur <> hsesch.txtCur THEN DO:
        FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
            ASSIGN wsRate = tblForex.decRate.
        END.
        ELSE DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
                 wsRate = tblForexH.decRate.
        END.
        ASSIGN wsRAmt = (DEC(wsAmt:SCREEN-VALUE) * wsTRate) / wsRate
               wsAmt =  (DEC(wsAmt:SCREEN-VALUE) * wsTRate). 
    END.  
    ELSE
        ASSIGN wsRate = wstRate
               wsRAmt =  DEC(wsAmt:SCREEN-VALUE)
               wsAmt =  (DEC(wsAmt:SCREEN-VALUE) * wstRate). 
    RETURN.
END.

ON CHOOSE OF btn-Ok IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN wsdate
            wsPer = INT(STRING(YEAR(wsDate)) + STRING(MONTH(wsDate),"99")).
      ASSIGN wsVat      = ROUND(wsRAmt * (Hsecmf.Vat / (Hsecmf.Vat + 100)),2) 
            varDescrip
            ctrLedger   = hsesch.CtrLedger
            wsLedger 
            wsOpt
            wsRef.
    IF wsPer > SIMCTR.CURPER OR wsPer <= SIMCTR.CLOSEPER THEN DO:
        MESSAGE "Accounting Period entered is in valid please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        RUN UPDATE.ip.
    END.
  RELEASE hsecmf.
  CLEAR FRAME frm-main ALL.
  VIEW FRAME frm-Main.
  APPLY 'entry' TO wsDate.
END.

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


PROCEDURE UPDATE.ip:  
   IF wsAmt <> 0 THEN DO:
      RUN writ.ip.
            
   END.
END PROCEDURE. /*.. EOP */

PROCEDURE Writ.ip:
    IF wsAmt <> 0.00 THEN DO:
       FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
       CREATE hsemtf.   /* create monthly transaction */
       ASSIGN hsemtf.dbacc   = hsecmf.dbacc
              hsemtf.scheme  = hsesch.scheme
              hsemtf.Accper  = wsPer
              hsemtf.CRate   = wsRate
              hsemtf.ILedger = wsLedger
              hsemtf.proj    = glmf.Proj
              hsemtf.dept    = glmf.dept
              hsemtf.fund    = glmf.fund
              hsemtf.trDate  = wsDate
              hsemtf.ref     = wsRef
              hsemtf.DESCRIP = varDescrip.
       IF wsOpt = 1 THEN /* Journal */
           ASSIGN hsemtf.Amt     = ROUND((wsRAmt),2)
                  hsemtf.Vat     = round((wsVat),2).
       ELSE IF wsOpt = 2 THEN /* Receipt */
           ASSIGN hsemtf.Amt     = ROUND((wsRAmt * -1),2)
                  hsemtf.Vat     = round((wsVat * -1),2).
       CREATE hsehtf.   /* History record */
       ASSIGN hsehtf.Accper  = wsPer
              hsehtf.dbacc   = hsecmf.dbacc
              hsehtf.scheme  = hsesch.scheme
              hsehtf.CRate   = wsRate
              hsehtf.tRate   = wstRate
              hsehtf.txtCur  = wsCur
              hsehtf.Iledger = wsLedger
              hsehtf.proj    = glmf.Proj
              hsehtf.dept    = glmf.dept
              hsehtf.fund    = glmf.fund
              hsehtf.ref     = wsRef
              hsehtf.trDate  = wsDate
              hsehtf.PoDate  = TODAY
              hsehtf.prog    = "hse05.p"
              hsehtf.TransID = wsTransId  
              hsehtf.uid     = varUser
              hsehtf.DESCRIP = varDescrip.
       IF wsOpt = 1 THEN
           ASSIGN hsehtf.Amt     = ROUND((wsRAmt),2)
                  hsehtf.Vat     = round((wsVat),2).
       ELSE IF wsOpt = 2 THEN
           ASSIGN hsehtf.Amt     = ROUND((wsRAmt * -1),2)
                  hsehtf.Vat     = round((wsVat * -1),2).
       ASSIGN Hsecmf.AmtDue[1] = Hsecmf.AmtDue[1] + hsehtf.Amt
              Hsecmf.AmtDue[2] = Hsecmf.AmtDue[1] * wsRate
              Hsecmf.LAmtDue   = Hsecmf.LAmtDue + ROUND(wsAmt,2)
              Hsecmf.LTAmt     = hsehtf.Amt
              Hsecmf.LTDate    = wsDate
              Hsecmf.LTRate    = wsRate.
       FIND FIRST hseblf WHERE hseblf.scheme = hsecmf.scheme AND hseblf.dbacc = hsecmf.dbacc NO-ERROR. /* Age analysis data */
       IF NOT AVAILABLE hseblf THEN DO:
            CREATE hseblf.
            ASSIGN hseblf.scheme = hsecmf.scheme
                   hseblf.dbacc  = hsecmf.dbacc.
        END.
        ASSIGN X = 15 /* Update the debtor age oldest first */
               wsPAmt = wsRAmt.
        IF wsOpt = 2 THEN DO:
            DO WHILE X >= 1: 
                IF hseblf.Amt[X] >= wsPAmt  THEN
                    ASSIGN hseblf.Amt[X] = hseblf.Amt[X] - wsPAmt
                           wsPAmt = 0
                           X = 0.
                ELSE IF hseblf.Amt[X] < wsPAmt THEN
                    ASSIGN wsPamt = wsPamt - hseblf.Amt[X] WHEN X <> 1
                           hseblf.Amt[X] = 0 WHEN X <> 1
                           hseblf.Amt[X] = hseblf.Amt[X] - wsPAmt WHEN X = 1
                           wsPAmt = 0 WHEN X = 1.
               IF wsPAmt = 0 THEN
                   X = 0.
               X = X - 1.
            END.
        END.
        ELSE DO:
            IF wsAge <= 2 THEN
                wsAge = 2.
             ASSIGN hseblf.Amt[wsAge] = hseblf.Amt[wsAge] + hsehtf.Amt.
        END.
        hseblf.vat = hseblf.vat + hsehtf.Vat.   
    END.
    IF wsOpt = 2 THEN
       wsAmt = wsAmt * -1.
    ASSIGN wsVat  = ROUND(wsAmt * (Hsecmf.Vat / (Hsecmf.Vat + 100)),2)
           varDescrip = "Payment Journal" WHEN wsOpt = 2
           varDescrip = "Adjusting Journal" WHEN wsOpt = 1.
    IF wsAmt <> 0.00 THEN DO:
                /* create records for Ledger transaction */
            FIND FIRST glmf WHERE glmf.acct = ctrLedger NO-LOCK NO-ERROR.
            FIND FIRST dbgl WHERE dbgl.acct = ctrLedger AND dbgl.period = wsper
                                   AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                                   AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = ctrLedger
                          dbgl.proj     = glmf.proj
                          dbgl.dept     = glmf.dept
                          dbgl.fund     = glmf.fund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = wsRef
                          dbgl.DESCRIP  = varDescrip
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + wsAmt.
            /* Create record for the Contra Ledger record */
            FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
            FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = wsper
                              AND dbgl.TransID = wsTransId AND dbgl.dept = glmf.dept
                              AND dbgl.proj = glmf.proj AND dbgl.fund = glmf.fund NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = wsLedger
                          dbgl.proj     = glmf.proj
                          dbgl.dept     = glmf.dept
                          dbgl.fund     = glmf.fund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = wsRef
                          dbgl.DESCRIP  = varDescrip
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsAmt * -1),2).
    END. /* eo if wsAmt.... */
    /* Create VAT Provision Ledger  Record ???? This must have been created at sales*/
    IF  wsVat <> 0 THEN DO:
        DO X = 1 TO 2:
           FIND FIRST dbgl WHERE dbgl.period = wsper  AND dbgl.TransID = wsTransId
                             AND dbgl.acct = simctr.vat[X] AND dbgl.proj = glmf.proj
                             AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
           IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = simctr.vat[X]
                          dbgl.proj     = glmf.proj
                          dbgl.dept     = glmf.dept
                          dbgl.fund     = glmf.fund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = wsRef
                          dbgl.DESCRIP  = varDescrip
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
           END.
           ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsVat),2)      WHEN X = 2
                  dbgl.AMT = dbgl.AMT + ROUND((wsVat * -1),2) WHEN X = 1.
            END.   
        END.
    RETURN.
END PROCEDURE.



PROCEDURE Consol.ip:
     {glcon.i}
END.
