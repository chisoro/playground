/* Program.................cbjnl03.p
   Notes:................. Transfer between Cash Books
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR wsAcb   LIKE cbkmf.Acb EXTENT 2.
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsAmt    LIKE cbkmf.bal.
DEF VAR wsBal    LIKE cbkmf.bal.
DEF VAR wsNar    LIKE cbkmf.descrip FORM "X(50)".
DEF VAR wsRef    LIKE cbtrans.ref.
DEF VAR wsDate   LIKE cbTrans.trDate.
DEF VAR wsLedger LIKE cbkmf.ledger EXTENT 3.
DEF VAR wsDesc   LIKE glmf.DESCRIPTION EXTENT 2.
DEF VAR wsFund   LIKE glmf.fund. 
DEF VAR X AS INT.


DEF BUFFER bfCBTrans FOR CBTrans.
DEF TEMP-TABLE tmpTrans LIKE CBTrans.

DEF BUTTON btnAcb1     LABEL "FROM CASHBOOK".
DEF BUTTON btnAcb2     LABEL "TO CASHBOOK".
DEF BUTTON btnPrint    LABEL "TRANSFER".
DEF BUTTON btnok       LABEL "OK".
DEF BUTTON btnClose    LABEL "CANCEL".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 96.5 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 96.5 by 10.8.

DEF    QUERY qry-Acb1 FOR cbkmf SCROLLING.
DEF BROWSE brw-Acb1 QUERY qry-Acb1
     DISPLAY cbkmf.Acb cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Acb2 FOR cbkmf SCROLLING.
DEF BROWSE brw-Acb2 QUERY qry-Acb2
     DISPLAY cbkmf.Acb cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Acb1 
     brw-Acb1 AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Cashbook Selection".

DEFINE FRAME frm-Acb2 
     brw-Acb2 AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Cashbook Selection".

DEF FRAME frm-Input
    SKIP(1)
     st-per      COLON 19  LABEL "Period"    AUTO-RETURN SPACE(30)
     wsDate                LABEL "Date"   SKIP(0.5)
     btnAcb1    COLON 5           NO-TAB-STOP
     wsAcb[1]            NO-LABEL   
     wsDesc[1]            NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
     wsBal       COLON 25 NO-TAB-STOP VIEW-AS TEXT LABEL "Cashbook Balance" SKIP(0.5)
     btnAcb2    COLON 8  NO-TAB-STOP
     wsAcb[2]            NO-LABEL   
     wsDesc[2]            NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
     wsRef       COLON 19 LABEL "Reference" SKIP(0.5)
     wsNar       COLON 19 LABEL "Narration"  SKIP(0.5)
     wsAmt       COLON 19 LABEL "Amount"  SKIP(0.5)
     btnPrint    AT ROW 12.8 COL 20 SPACE(40) btnClose
     rect-1      AT ROW 12.3 COL 2
     rect-2     AT ROW 1.27 COL 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 100 BY 15.5 TITLE "TRANSFER BETWEEN CASHBOOKS".

FORM
    wsDate        LABEL "DATE " SPACE(4)
    st-Per        LABEL "PERIOD" 
    cbkmf.descrip LABEL "CASHBOOK " 
    wsNar         LABEL "NARRATION"  FORM "x(50)"
    wsAmt         LABEL "AMOUNT " 
    wsLedger[1]   LABEL "ALLOCATION"
    HEADER  "TRANSFER BETWEEN CASHBOOKS UPDATE - REFERENCE#" wsRef  "Page: " AT 80 PAGE-NUMBER(a)
    SKIP(1) "                    DATE:" wsDate   "   PERIOD :" st-per SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

ON CHOOSE OF btnAcb1 IN FRAME frm-Input
DO:
  VIEW FRAME frm-Acb1.
  OPEN QUERY qry-Acb1 FOR EACH cbkmf WHERE cbkmf.Acb <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Acb1.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Acb1 
          OR close of THIS-PROCEDURE IN FRAME frm-Acb1
          OR CHOOSE OF btnok IN FRAME frm-Acb1 
          OR 'enter':u OF brw-Acb1
          OR 'mouse-select-dblclick' OF brw-Acb1.
  CLOSE QUERY qry-Acb1.
  HIDE FRAME frm-Acb1.
  APPLY 'tab' TO btnAcb1.
  APPLY 'tab' TO wsAcb[1].
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Acb1 
    OR 'enter':u OF brw-Acb1
    OR 'mouse-select-dblclick' OF brw-Acb1
DO: 
   GET CURRENT qry-Acb1 NO-LOCK.
   DISPLAY cbkmf.Acb @ wsAcb[1] WITH FRAME frm-Input.
   APPLY 'TAB' TO wsAcb[1] IN FRAME frm-Input.
END.

ON 'tab' of wsAcb[1] IN FRAME frm-Input
    OR 'enter' OF wsAcb[1] IN FRAME frm-Input
DO:
    IF  INT(wsAcb[1]:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Input.
    ELSE DO:
        ASSIGN wsAcb[1] =INT(wsAcb[1]:SCREEN-VALUE). 
        FIND FIRST cbkmf WHERE cbkmf.Acb = wsAcb[1] NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Input ALL.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wsDesc[1]   = cbkmf.DESCRIP
                   wsBal       = cbkmf.Bal
                   wsLedger[1] = cbkmf.Ledger.
            DISPLAY cbkmf.Acb @ wsAcb[1]  wsDesc[1] wsBal WITH FRAME frm-Input.
        END.
    END.
END.


ON CHOOSE OF btnAcb2 IN FRAME frm-Input
DO:
  VIEW FRAME frm-Acb2.
  OPEN QUERY qry-Acb2 FOR EACH cbkmf WHERE cbkmf.Acb <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Acb2.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Acb2 
          OR close of THIS-PROCEDURE IN FRAME frm-Acb2
          OR CHOOSE OF btnok IN FRAME frm-Acb2 
          OR 'enter':u OF brw-Acb2
          OR 'mouse-select-dblclick' OF brw-Acb2.
  CLOSE QUERY qry-Acb2.
  HIDE FRAME frm-Acb2.
  APPLY 'tab' TO btnAcb2.
  APPLY 'tab' TO wsAcb[2].
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Acb2 
    OR 'enter':u OF brw-Acb2
    OR 'mouse-select-dblclick' OF brw-Acb2
DO: 
   GET CURRENT qry-Acb2 NO-LOCK.
   DISPLAY cbkmf.Acb @ wsAcb[2] WITH FRAME frm-Input.
   APPLY 'TAB' TO wsAcb[2] IN FRAME frm-Input.
END.

ON 'tab' of wsAcb[2] IN FRAME frm-Input
    OR 'enter' OF wsAcb[2] IN FRAME frm-Input
DO:
    IF  INT(wsAcb[2]:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Input.
    ELSE DO:
        ASSIGN wsAcb[2] = INT(wsAcb[2]:SCREEN-VALUE). 
        FIND FIRST cbkmf WHERE cbkmf.Acb = wsAcb[2] NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Input ALL.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wsDesc[2] = cbkmf.DESCRIP
                   wsLedger[2] = cbkmf.Ledger.
            DISPLAY cbkmf.Acb @ wsAcb[2]  wsDesc[2] WITH FRAME frm-Input.
        END.
    END.
END.

ON 'tab' OF st-Per IN FRAME frm-Input
    OR 'enter' OF st-Per IN FRAME frm-Input
DO:
  FIND FIRST SIMCTR NO-LOCK NO-ERROR .
  IF INT(st-per:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(st-per:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
END.

ON 'TAB':U OF wsAmt IN FRAME frm-Input
   OR 'enter':U OF wsAmt IN FRAME frm-Input
DO:
    IF DEC(wsAmt:SCREEN-VALUE) > DEC(wsBal:SCREEN-VALUE) THEN DO:
        MESSAGE "Transaction is greater than available balance..please try again"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'TAB':U OF wsDate IN FRAME frm-Input
   OR 'enter':U OF wsDate IN FRAME frm-Input
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnPrint IN FRAME frm-Input   
DO: 
    ASSIGN st-per = INT(st-per:SCREEN-VALUE)
           wsYear = INT(SUBSTR(STRING(st-per),1,4))
           wsMonth = INT(SUBSTR(STRING(st-per),5,2))
           wsRef wsDate st-per wsAmt wsNar.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged}
   CLEAR FRAME frm-Input.
   APPLY 'entry' TO btnClose IN FRAME frm-Input.
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
              + string(MONTH(TODAY),"99" )
              + string(DAY(TODAY),"99")
              + SUBSTR(STRING(wsTIME),1,2) 
              + SUBSTR(STRING(wsTIME),4,2) 
              + SUBSTR(STRING(wsTIME),7,2) ).
CLEAR FRAME frm-Input.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY).
ENABLE ALL WITH FRAME frm-Input.
APPLY 'entry' TO st-per IN FRAME frm-Input.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-Input.

PROCEDURE Update-ip:
    DO X = 1 TO 2:
       FIND FIRST cbkmf WHERE cbkmf.Acb = wsAcb[X] EXCLUSIVE-LOCK NO-ERROR.
       FIND FIRST glmf WHERE glmf.acct = cbkmf.ledger NO-ERROR.
       ASSIGN cbkmf.bal = cbkmf.bal - wsAmt WHEN X = 1
              cbkmf.bal = cbkmf.bal + wsAmt WHEN X = 2.
       CREATE bfcbTrans. /* Update and Create Acb transaction */
       ASSIGN bfCBTrans.Accper = st-per
              bfCBTrans.amount = wsAmt * -1 WHEN X = 1
              bfCBTrans.amount = wsAmt      WHEN X = 2
              bfCBTrans.Acb   = wsAcb[X]
              bfCBTrans.Descrip = wsNar
              bfCBTrans.OpCode  = varUser
              bfCBTrans.Ref     = wsRef
              bfCBTrans.TransID = wsTransId
              bfCBTrans.TranType = 4 + X
              bfCBTrans.trDate   = wsDate
              bfCBTrans.ledger   = wsLedger[2]
              bfCBTrans.dept   = glmf.dept
              bfCBTrans.fund   = glmf.fund
              bfCBTrans.proj   = glmf.proj.
       DISPLAY STREAM a wsDate st-Per cbkmf.descrip wsNar bfCBTrans.amount @ wsAmt wsLedger[X] @ wsLedger[1] WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
       IF X = 1 THEN 
           wsFund = glmf.fund.
       CREATE gltdf.
       ASSIGN gltdf.CREDATE = TODAY
              gltdf.DESCRIP  = wsNar
              gltdf.period   = st-per
              gltdf.REF      = wsRef
              gltdf.TransID  = wsTransId
              gltdf.trDATE   = wsDate
              gltdf.UID      = varUser
              gltdf.UID2     = varUser
              gltdf.acct     = cbkmf.ledger
              bfCBTrans.dept   = glmf.dept
              bfCBTrans.fund   = glmf.fund
              bfCBTrans.proj   = glmf.proj
              gltdf.SOURCE   = "CB"
              gltdf.AMT = gltdf.AMT + bfCBTrans.amount.
       RUN ledger.ip.
       IF glmf.fund <> wsFund AND simctr.fundAcc = YES THEN DO: /* Create Inter-Fund transaction */
                 ASSIGN wsLedger[3] = SIMCTR.FUNDDb WHEN X = 1
                        wsLedger[3] = SIMCTR.FUNDCr WHEN X = 2.
                 CREATE gltdf.
                 ASSIGN gltdf.CREDATE = TODAY
                        gltdf.DESCRIP  = "Resource advance: EX#" + STRING(gltdf.fund)
                                       + "/TO#" + string(gltdf.fund) 
                        gltdf.period   = st-per
                        gltdf.REF      = wsRef
                        gltdf.TransID  = wsTransId
                        gltdf.trDATE   = wsDate
                        gltdf.UID      = varUser
                        gltdf.UID2     = varUser
                        gltdf.acct     = wsLedger[3]
                        gltdf.SOURCE   = "CB"
                        gltdf.AMT      = wsAmt      WHEN X = 1
                        gltdf.AMT      = wsAmt * -1 WHEN X = 2
                        gltdf.dept   = glmf.dept
                        gltdf.fund   = glmf.fund
                        gltdf.proj   = glmf.proj.
            RUN ledger.ip.
                DISPLAY STREAM a wsDate st-Per "INTER-FUND TRANSFER" @ wsNar gltdf.AMT @ wsAmt wsLedger[3] @ wsLedger[1] WITH FRAME frm-rpt.
                DOWN STREAM a WITH FRAME frm-rpt.
           END.
    END. /* eof do x = 1 to 2 */
RELEASE CBKMF.
RELEASE CBTRANS.
RELEASE GLTDF.
RELEASE GLBAL.
APPLY 'close' TO THIS-PROCEDURE.
END.

PROCEDURE Ledger.ip:
    FIND FIRST glbal WHERE GLBAL.YEAR = wsYear AND GLBAL.acct = gltdf.Acct NO-ERROR.
       IF NOT AVAILABLE glbal THEN DO:
           CREATE glbal.
           ASSIGN GLBAL.YEAR = wsYear
                  GLBAL.acct = gltdf.Acct.
       END.
       glbal.amt[wsMonth] = glbal.amt[wsMonth] + gltdf.Amt.
       /*fund */
       FIND FIRST glfbal WHERE GLfBAL.YEAR = wsYear AND GLfBAL.acct = gltdf.Acct
           AND GLfBAL.fund = gltdf.fund NO-ERROR.
       IF NOT AVAILABLE glfbal THEN DO:
           CREATE glfbal.
           ASSIGN GLfBAL.YEAR = wsYear
                  GLfBAL.acct = gltdf.Acct
                  GLfBAL.fund = gltdf.fund.
       END.
       glfbal.amt[wsMonth] = glfbal.amt[wsMonth] + gltdf.Amt.
       /*Project */
       FIND FIRST glpbal WHERE GLpBAL.YEAR = wsYear AND GLpBAL.acct = gltdf.Acct
           AND GLpBAL.proj = gltdf.proj NO-ERROR.
       IF NOT AVAILABLE glpbal THEN DO:
           CREATE glpbal.
           ASSIGN GLpBAL.YEAR = wsYear
                  GLpBAL.acct = gltdf.Acct
                  GLpBAL.Proj = gltdf.Proj.
       END.
       glpbal.amt[wsMonth] = glpbal.amt[wsMonth] + gltdf.Amt.
       /*Department */
       FIND FIRST gldbal WHERE GLdBAL.YEAR = wsYear AND GLdBAL.acct = gltdf.Acct
           AND GLdBAL.dept = gltdf.dept NO-ERROR.
       IF NOT AVAILABLE gldbal THEN DO:
           CREATE gldbal.
           ASSIGN GLdBAL.YEAR = wsYear
                  GLdBAL.acct = gltdf.Acct
                  GLdBAL.dept = gltdf.dept.
       END.
       gldbal.amt[wsMonth] = gldbal.amt[wsMonth] + gltdf.Amt.
END.
