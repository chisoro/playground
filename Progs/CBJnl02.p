/* Program.................cbjnl02.p
   Notes:................. Transfer between Banks
   Author:.................S. Mawire
   Edited:.................S. Chisoro
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF  VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR wsBank   LIKE cbkmf.bank EXTENT 2.
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsAmt    LIKE cbkmf.bal.
DEF VAR wsBal    LIKE cbkmf.bal.
DEF VAR wsNar    LIKE cbkmf.descrip FORM "X(50)".
DEF VAR wsRef    LIKE cbtrans.ref.
DEF VAR wsDate   LIKE cbTrans.trDate.
DEF VAR wsDesc   LIKE glmf.DESCRIPTION EXTENT 2.
DEF VAR wsR      LIKE tblforex.decRate EXTENT 2.
DEF VAR wsRate   LIKE tblforex.decRate.
DEF VAR wsAcc    LIKE cbkmf.Ledger EXTENT 2.
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsDr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsCr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsAmt.
DEF VAR wsTCr    LIKE wsAmt.
DEF VAR wsSource LIKE gltdf.SOURCE INITIAL "CB".

DEF BUFFER bfCBTrans FOR CBTrans.
DEF TEMP-TABLE tmpTrans LIKE CBTrans.

DEF BUTTON btnBank1     LABEL "FROM BANK".
DEF BUTTON btnBank2     LABEL "TO BANK".
DEF BUTTON btnPrint    LABEL "TRANSFER".
DEF BUTTON btnok       LABEL "OK".
DEF BUTTON btnClose    LABEL "CANCEL".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 96.5 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 96.5 by 10.8.

DEF    QUERY qry-Bank1 FOR cbkmf SCROLLING.
DEF BROWSE brw-Bank1 QUERY qry-Bank1
     DISPLAY cbkmf.bank cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Bank2 FOR cbkmf SCROLLING.
DEF BROWSE brw-Bank2 QUERY qry-Bank2
     DISPLAY cbkmf.bank cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Bank1 
     brw-Bank1 AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Bank Selection".

DEFINE FRAME frm-Bank2 
     brw-Bank2 AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Bank Selection".

DEF FRAME frm-Input
    SKIP(1)
     st-per      COLON 19  LABEL "Period"    AUTO-RETURN SPACE(30)
     wsDate                LABEL "Date"   SKIP(0.5)
     btnBank1    COLON 5           NO-TAB-STOP
     wsBank[1]            NO-LABEL   
     wsDesc[1]            NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
     wsBal       COLON 19 NO-TAB-STOP VIEW-AS TEXT LABEL "Bank Balance" SKIP(0.5)
     btnBank2    COLON 8  NO-TAB-STOP
     wsBank[2]            NO-LABEL   
     wsDesc[2]            NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
     wsRef       COLON 19 LABEL "Reference" SKIP(0.5)
     /*wsNar       COLON 19 LABEL "Narration"  VIEW-AS FILL-IN SIZE 50 BY 1 SKIP(0.5)*/
     wsAmt       COLON 19 LABEL "Amount"  SKIP(0.5)
     btnPrint    AT ROW 12.8 COL 20 SPACE(40) btnClose
     rect-1      AT ROW 12.3 COL 2
     rect-2     AT ROW 1.27 COL 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 100 BY 15.5 TITLE "TRANSFER BETWEEN BANKS".

FORM
    wsDate        LABEL "DATE " SPACE(4)
    st-Per        LABEL "PERIOD" 
    cbkmf.descrip LABEL "BANK " 
    wsNar         LABEL "NARRATION"  FORM "x(50)"
    wsAmt         LABEL "AMOUNT " 
    HEADER  "TRANSFER BETWEEN BAKS UPDATE - REFERENCE#" wsRef  "Page: " AT 80 PAGE-NUMBER(a)
    SKIP(1) "                    DATE:" wsDate   "   PERIOD :" st-per SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION" FORM "x(50)"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "ONLINE JOURNAL CAPTURE AND UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

ON CHOOSE OF btnBank1 IN FRAME frm-Input
DO:
  VIEW FRAME frm-Bank1.
  OPEN QUERY qry-Bank1 FOR EACH cbkmf WHERE cbkmf.bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Bank1.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Bank1 
          OR close of THIS-PROCEDURE IN FRAME frm-Bank1
          OR CHOOSE OF btnok IN FRAME frm-Bank1 
          OR 'enter':u OF brw-Bank1
          OR 'mouse-select-dblclick' OF brw-Bank1.
  CLOSE QUERY qry-Bank1.
  HIDE FRAME frm-Bank1.
  APPLY 'tab' TO btnBank1.
  APPLY 'tab' TO wsBank[1].
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Bank1 
    OR 'enter':u OF brw-Bank1
    OR 'mouse-select-dblclick' OF brw-Bank1
DO: 
   GET CURRENT qry-Bank1 NO-LOCK.
   DISPLAY cbkmf.Bank @ wsBank[1] WITH FRAME frm-Input.
   APPLY 'TAB' TO wsBank[1] IN FRAME frm-Input.
END.

ON 'tab' of wsBank[1] IN FRAME frm-Input
    OR 'enter' OF wsBank[1] IN FRAME frm-Input
DO:
    IF  INT(wsbank[1]:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Input.
    ELSE DO:
        ASSIGN wsBank[1] =INT(wsBank[1]:SCREEN-VALUE). 
        FIND FIRST cbkmf WHERE cbkmf.bank = wsBank[1] NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Input ALL.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wsDesc[1]   = cbkmf.DESCRIP
                   wsBal       = cbkmf.Bal
                   wsAcc[1]       = cbkmf.Ledger.
            DISPLAY cbkmf.Bank @ wsBank[1]  wsDesc[1] wsBal WITH FRAME frm-Input.
            /*enbale going to history */
            FIND first tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND tblForex.DtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAILABLE tblForex THEN DO:
                ASSIGN wsR[1] = tblForex.DecRate
                       wsRate = tblForex.DecRate.    
            END.
            ELSE DO:
                 FIND LAST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND  tblForexH.dtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
                      ASSIGN wsR[1] = tblForexH.DecRate
                             wsRate = tblForexH.DecRate.
            END.
        END.
    END.
END.


ON CHOOSE OF btnBank2 IN FRAME frm-Input
DO:
  VIEW FRAME frm-Bank2.
  OPEN QUERY qry-Bank2 FOR EACH cbkmf WHERE cbkmf.bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Bank2.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Bank2 
          OR close of THIS-PROCEDURE IN FRAME frm-Bank2
          OR CHOOSE OF btnok IN FRAME frm-Bank2 
          OR 'enter':u OF brw-Bank2
          OR 'mouse-select-dblclick' OF brw-Bank2.
  CLOSE QUERY qry-Bank2.
  HIDE FRAME frm-Bank2.
  APPLY 'tab' TO btnBank2.
  APPLY 'tab' TO wsBank[2].
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Bank2 
    OR 'enter':u OF brw-Bank2
    OR 'mouse-select-dblclick' OF brw-Bank2
DO: 
   GET CURRENT qry-Bank2 NO-LOCK.
   DISPLAY cbkmf.Bank @ wsBank[2] WITH FRAME frm-Input.
   APPLY 'TAB' TO wsBank[2] IN FRAME frm-Input.
END.

ON 'tab' of wsBank[2] IN FRAME frm-Input
    OR 'enter' OF wsBank[2] IN FRAME frm-Input
DO:
    IF  INT(wsbank[2]:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Input.
    ELSE DO:
        ASSIGN wsBank[2] = INT(wsBank[2]:SCREEN-VALUE). 
        FIND FIRST cbkmf WHERE cbkmf.bank = wsBank[2] NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Input ALL.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wsDesc[2] = cbkmf.DESCRIP
                   wsAcc[2]  = cbkmf.Ledger.
            DISPLAY cbkmf.Bank @ wsBank[2]  wsDesc[2] WITH FRAME frm-Input.
            FIND FIRST tblForex WHERE tblForex.txtCur = cbkmf.txtCur NO-LOCK NO-ERROR.
            ASSIGN wsR[2] = tblForex.DecRate.
            FIND first tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND tblForex.DtRate <= date(wsDate:SCREEN-VALUE)  NO-LOCK NO-ERROR.
            IF AVAILABLE tblForex THEN DO:
                ASSIGN wsR[2] = tblForex.DecRate.
            END.
            ELSE DO:
                FIND LAST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND  tblForexH.dtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
                     ASSIGN wsR[2] = tblForexH.DecRate.
            END.
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
           wsRef wsDate st-per wsAmt.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged} 
   CLEAR FRAME frm-Input.
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.

CLEAR FRAME frm-Input.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY).
ENABLE ALL WITH FRAME frm-Input.
APPLY 'entry' TO st-per IN FRAME frm-Input.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-Input.

PROCEDURE Update-ip:
    wsTime    = STRING(TIME,"HH:MM:SS").
    wsTransID = DEC (string(YEAR(TODAY),"9999")
                  + string(MONTH(TODAY),"99" )
                  + string(DAY(TODAY),"99")
                  + SUBSTR(STRING(wsTIME),1,2) 
                  + SUBSTR(STRING(wsTIME),4,2) 
                  + SUBSTR(STRING(wsTIME),7,2) ).
    DO X = 1 TO 2:
       FIND FIRST cbkmf WHERE cbkmf.bank = wsBank[X] EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN cbkmf.bal = cbkmf.bal - wsAmt WHEN X = 1.
       IF X = 2 THEN DO: 
           IF wsr[1] >= wsR[2] THEN
               cbkmf.bal = ROUND(cbkmf.bal + (wsAmt * (wsr[1] / wsR[2])),2).
           ELSE IF wsr[1] < wsR[2] THEN
               cbkmf.bal = ROUND(cbkmf.bal + (wsAmt / (wsr[2] / wsR[1])),2).
       END.
       CREATE bfcbTrans. /* allocation 99 */
       ASSIGN wsNar = "Transfer to " + STRING(wsBank[2]) WHEN X = 1
              wsNar = "Transfer from " + STRING(wsBank[1]) WHEN X = 2.
       ASSIGN bfCBTrans.Accper = st-per
              bfCBTrans.amount = wsAmt * -1 WHEN X = 1
              bfCBTrans.bank   = wsBank[X]
              bfCBTrans.Descrip = wsNar
              bfCBTrans.OpCode  = varUser
              bfCBTrans.Ref     = wsRef
              bfCBTrans.TransID = wsTransId
              bfCBTrans.TranType = 4 + X
              bfCBTrans.trDate   = wsDate
              bfCBTrans.ledger   = 99
              bfCBTrans.seq      = 0.
       IF X = 2 THEN DO: 
           IF wsr[1] >= wsR[2] THEN
               bfCBTrans.amount = ROUND(wsAmt * (wsr[1] / wsR[2]),2).
           ELSE IF wsr[1] < wsR[2] THEN
                bfCBTrans.amount = ROUND(wsAmt / (wsr[2] / wsR[1]),2).
       END.       
       CREATE bfcbTrans. /* True Allocation  */
       ASSIGN bfCBTrans.Accper = st-per
              bfCBTrans.amount = wsAmt * -1 WHEN X = 1
              bfCBTrans.bank   = wsBank[X]
              bfCBTrans.Descrip = wsNar
              bfCBTrans.OpCode  = varUser
              bfCBTrans.Ref     = wsRef
              bfCBTrans.TransID = wsTransId
              bfCBTrans.TranType = 4 + X
              bfCBTrans.trDate   = wsDate
              bfCBTrans.ledger   = wsAcc[1] WHEN X = 2
              bfCBTrans.ledger   = wsAcc[2] WHEN X = 1
              bfCBTrans.seq      = 1.
       IF X = 2 THEN DO: 
           IF wsr[1] >= wsR[2] THEN
               bfCBTrans.amount = ROUND(wsAmt * (wsr[1] / wsR[2]),2).
           ELSE IF wsr[1] < wsR[2] THEN
                bfCBTrans.amount = ROUND(wsAmt / (wsr[2] / wsR[1]),2).
       END.
       FIND FIRST glmf WHERE glmf.acct = cbkmf.Ledger NO-LOCK NO-ERROR.
       FIND FIRST dbgl WHERE dbgl.acct = glmf.acct 
                         AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund
                         AND dbgl.proj = glmf.proj no-error. /* create record for ledger */
       IF NOT AVAILABLE dbgl THEN DO:
          CREATE dbgl.
          ASSIGN  dbgl.CREDATE = TODAY
                  dbgl.DESCRIP  = wsNar
                  dbgl.period   = st-per
                  dbgl.REF      = wsRef
                  dbgl.TransID  = wsTransId
                  dbgl.trDATE   = wsDate
                  dbgl.UID      = varUser
                  dbgl.acct     = cbkmf.Ledger
                  dbgl.Dept     = glmf.Dept
                  dbgl.Proj     = glmf.Proj
                  dbgl.fund     = glmf.fund
                  dbgl.SOURCE   = "CB".
       END.
       ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsAmt * wsRate * -1),2) WHEN X = 1
              dbgl.AMT = dbgl.AMT + ROUND((wsAmt * wsRate),2)       WHEN X = 2.
       DISPLAY STREAM a wsDate st-Per cbkmf.descrip wsNar bfCBTrans.amount @ wsAmt WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
    END. /* eof do x = 1 to 2 */
{glcon.i}
RELEASE CBKMF.
RELEASE CBTRANS.
RELEASE dbgl.
APPLY 'close' TO THIS-PROCEDURE.
END.

