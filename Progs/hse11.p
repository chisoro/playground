/* Program.................hse11.p
   Notes:...... ..........Period closure and calculation of Exchange gain/loss
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.

DEF VAR wsSource  AS CHAR INITIAL "HS".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsdate    AS DATE.
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR st-per     AS INT FORM "999999".
DEF VAR wsAmt     LIKE hsecmf.bal.
DEF VAR wsVat     LIKE hsecmf.bal.
DEF VAR wsTotal   LIKE wsAmt.
DEF VAR wsTVat    LIKE wsAmt.
DEF VAR wsDr      LIKE wsAmt.
DEF VAR wsCr      LIKE wsAmt.
DEF VAR wsTDr     LIKE wsAmt.
DEF VAR wsTCr     LIKE wsAmt.
DEF VAR wsP       LIKE wsAmt.
DEF VAR wsn       AS INT.
DEF VAR wsr       AS   DEC FORM "zzz9.999999999-".
DEF VAR wsInst    LIKE wsAmt.
DEF VAR wsRate    AS DEC.
DEF VAR wsdes     AS CHAR FORM "x(12)".
DEF  VAR wsLedger LIKE hsesch.CLedger.
DEF VAR X          AS INT.
DEF VAR wsStatus      LIKE hsecmf.dbacc.
DEF VAR wsTitle  AS CHAR FORM "x(80)".

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.


FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION" FORM "x(50)"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "ONLINE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

DEFINE FRAME frm-main
    SKIP(2.5)
    st-per     COLON 40 LABEL "Enter Accounting Period" SKIP(.5)
    wsDate    COLON 40 LABEL "Enter Transaction Date" SKIP(1.5)
    wsStatus  COLON 30 LABEL "Processing....... " view-as text no-tab-stop
    skip(4.5)
    btn-ok colon 15
    btn-close colon 70
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15 view-as dialog-box keep-tab-order no-validate
      BGCOLOR 3    side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "HOUSING MODULE PERIOD END AND EXCHANGE GAIN/LOSS CALCULATIONS".

ON 'tab' OF st-per IN FRAME frm-main
    OR 'leave' OF st-per IN FRAME frm-main
DO:
    ASSIGN st-per.
  IF INT(st-per:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(st-per:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ELSE IF CAN-FIND(FIRST gltdf WHERE gltdf.per >= st-per AND gltdf.descrip = "Exchange Gain/Loss"
                   AND gltdf.ref = "hse11.p") THEN DO:
     MESSAGE "This period has been processed......" VIEW-AS ALERT-BOX.
     CLEAR FRAME frm-main ALL.
     APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-Main.
     HIDE FRAME frm-main.
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

ON CHOOSE OF btn-Ok IN FRAME frm-main 
DO:
     session:set-wait-state("").
     ASSIGN wsdate
            st-per     = INT(st-per:SCREEN-VALUE).
            wsTime    = STRING(TIME,"HH:MM:SS").
    IF st-per > SIMCTR.CURPER OR st-per <= SIMCTR.CLOSEPER THEN DO:
        MESSAGE "Accounting Period entered is in valid please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        RUN update-ip.
        IF CAN-FIND (FIRST dbgl WHERE dbgl.TransID = wsTransID AND dbgl.SOURCE = wsSource NO-LOCK) THEN
        DO:     
            {PrintOpt.i &stream-name="stream a"
                            &print-prog="Consol.ip"
                            &paged}
        END.
    END.
    MESSAGE "Period closure completed...." VIEW-AS ALERT-BOX.
    APPLY 'entry' TO btn-close IN FRAME frm-main.
    RETURN.
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsTime    = STRING(TIME,"HH:MM:SS").
       wsTransID = DEC (string(YEAR(TODAY),"9999")
                 + string(MONTH(TODAY),"99" )
                 + string(DAY(TODAY),"99")
                 + SUBSTR(STRING(wsTIME),1,2) 
                 + SUBSTR(STRING(wsTIME),4,2) 
                 + SUBSTR(STRING(wsTIME),7,2) ). 
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE update-ip:
    FOR EACH hseblf: /* age the debtors */
            DISPLAY hseblf.dbacc @ wsStatus WITH FRAM frm-main. 
            PAUSE 0.
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
    END.
    FOR EACH hsemtf: /*clear monthly table */
      DELETE hsemtf.
    END.
    FOR EACH hsesch:
        FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN
            wsRate = tblForex.decRate.
        ELSE IF NOT AVAILABLE tblForex THEN DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
                 wsRate = tblForexH.decRate.
        END.
        FOR EACH hsecmf WHERE hsecmf.Scheme = hsesch.scheme: /*Capital Exchange gain/loss */
            DISPLAY hsecmf.dbacc @ wsStatus WITH FRAME frm-Main.
            PAUSE 0.
            IF Hsecmf.LBal <> (Hsecmf.Bal * wsRate) THEN DO:
               DO X = 1 TO 2:      
                     ASSIGN wsledger = hsesch.CLedger WHEN X = 1
                            wsledger = hsesch.ExLedger WHEN X = 2
                            wsAmt  = ((Hsecmf.Bal * wsRate) -  Hsecmf.LBal)        WHEN X = 1
                            wsAmt  = (((Hsecmf.Bal * wsRate) -  Hsecmf.LBal)) * -1 WHEN X = 2.
                     IF wsAmt <> 0 THEN DO:
                         FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
                         FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.TransID = wsTransId 
                             AND dbgl.proj = glmf.proj AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
                         IF NOT AVAILABLE dbgl THEN DO:
                               CREATE dbgl.
                               ASSIGN dbgl.acct     = wsLedger
                                      dbgl.proj     = glmf.proj
                                      dbgl.dept     = glmf.dept
                                      dbgl.fund     = glmf.fund
                                      dbgl.period   = st-per
                                      dbgl.TransID  = wsTransId
                                      dbgl.trDATE   = wsDate
                                      dbgl.UID      = varUser
                                      dbgl.REF      = "hse11.p"
                                      dbgl.descrip  = "Exchange Gain/Loss"
                                      dbgl.CREDATE  = TODAY
                                      dbgl.SOURCE   = wsSource.
                         END.
                         dbgl.AMT = dbgl.AMT + wsAmt.
                     END.  
                END.
                ASSIGN Hsecmf.LBal = Hsecmf.Bal * wsRate 
                       Hsecmf.LTDate    = wsDate 
                       Hsecmf.LTRate    = wsrate.
           END.
           IF Hsecmf.AmtDue[2] <> (AmtDue[1] * wsRate) THEN DO:
               DO X = 1 TO 2: /*Revenue Exchange gain/loss */     
                     ASSIGN wsledger = hsesch.CtrLedger WHEN X = 1
                            wsledger = hsesch.ExLedger  WHEN X = 2
                            wsAmt  = ((AmtDue[1] * wsRate) -  Hsecmf.LAmtDue)        WHEN X = 1
                            wsAmt  = (((AmtDue[1] * wsRate) -  Hsecmf.LAmtDue)) * -1 WHEN X = 2.
                     IF wsAmt <> 0 THEN DO:
                         FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
                         FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.TransID = wsTransId 
                             AND dbgl.proj = glmf.proj AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
                         IF NOT AVAILABLE dbgl THEN DO:
                               CREATE dbgl.
                               ASSIGN dbgl.acct     = wsLedger
                                      dbgl.proj     = glmf.proj
                                      dbgl.dept     = glmf.dept
                                      dbgl.fund     = glmf.fund
                                      dbgl.period   = st-per
                                      dbgl.TransID  = wsTransId
                                      dbgl.trDATE   = wsDate
                                      dbgl.UID      = varUser
                                      dbgl.REF      = "hse11.p"
                                      dbgl.descrip  = "Exchange Gain/Loss"
                                      dbgl.CREDATE  = TODAY
                                      dbgl.SOURCE   = wsSource.
                         END.
                         dbgl.AMT = dbgl.AMT + wsAmt.
                     END.  
               END.
               ASSIGN Hsecmf.LAmtDue   = AmtDue[1] * wsRate 
                      Hsecmf.AmtDue[2] = AmtDue[1] * wsRate
                      Hsecmf.LTDate    = wsDate
                      Hsecmf.LTRate    = wsrate.
           END.
        END. /*...each hsecmf */
    END.  /*...each hsesch */
END PROCEDURE.

PROCEDURE Consol.ip:
     {glcon.i}
END.

