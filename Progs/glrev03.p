/* Program.................glrev03.p
   Notes:................. Transfer posted transactions
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsUser     AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsPer      LIKE gltdf.period.
DEF VAR wsYear     AS INT FORM "9999".
DEF VAR wsMonth    AS INT FORM "99".
DEF VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR OAcc      LIKE gltdf.Acct.
DEF VAR DAcc      LIKE gltdf.Acct.
DEF VAR wsTransId LIKE gltdf.transId.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF VAR wsSearch    AS CHAR FORM "x(40)".
DEF VAR wsfrm      AS CHAR.
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR X         AS INT.

DEF BUFFER bf-gltdf FOR gltdf.

DEF BUTTON btn-exit   LABEL "Close".
DEF BUTTON btn-ok     LABEL "Transfer".
DEF BUTTON btnSearch LABEL "Search".
DEF BUTTON btnAcc    LABEL "Source Ledger".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 105 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 105 by 18.5.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF    QUERY qry-Trans FOR gltdf SCROLLING.
DEF BROWSE brw-Trans QUERY qry-Trans
    DISPLAY gltdf.SOURCE COLUMN-LABEL "SOURCE" gltdf.period gltdf.trDATE  
    gltdf.REF gltdf.AMT gltdf.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 40 WITH 17 DOWN SEPARATORS.

DEF    QUERY qry-Ledger FOR glmf SCROLLING.
DEF BROWSE brw-Ledger QUERY qry-Ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "Decsription" 
    WIDTH 60 glmf.dept WITH 17 DOWN SEPARATORS.

DEFINE FRAME frm-Ledger 
    brw-Ledger AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Exit colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Selection".

DEF FRAME frm-main
    SKIP(1.5) 
    btnAcc    COLON 14 NO-TAB-STOP
    OAcc      NO-LABEL  glmf.DESCRIPTION NO-LABEL NO-TAB-STOP SKIP(0.5)
    wsPer     LABEL "Start Accounting Period" COLON 30 SKIP(0.5)
    brw-Trans AT ROW 5.5 COL 4
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20.2 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 109 BY 23.5
    TITLE "TRANSFER TRANSACTION BETWEEN LEDGERS" VIEW-AS DIALOG-BOX.


DEF FRAME frm-DAcc
    SKIP(1.5) 
    btnAcc    LABEL "Ledger" COLON 14 NO-TAB-STOP
    DAcc      NO-LABEL  glmf.DESCRIPTION NO-LABEL NO-TAB-STOP SKIP(0.5)
    skip(0.5)
    btn-ok LABEL "OK" colon 20
    space(40) btn-exit LABEL "CANCEL" SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Destination Ledger".

FORM gltdf.transId     LABEL "TRANSACTION"
     gltdf.trDate      LABEL "DATE"
     gltdf.acct        LABEL "ACCOUNT"
     gltdf.DESCRIP LABEL "DESCRIPTION"
     wsAmt             LABEL "AMOUNT"
    HEADER skip(1) "POSTED TRANSACTION REVERSAL FOR TRANSACTION: " 
    OAcc "Page: " AT 60 PAGE-NUMBER SKIP(1) 
    " Accounting Period: " AT 10 wsPer SPACE(3) "DONE BY: " wsUser space(3) TODAY SKIP(1)
    "---------------------------------------------------------------------------"
     SKIP(5)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX FRAME frm-rpt.

/* ******** Triggers  ***********/

ON CHOOSE OF btnAcc IN FRAME frm-Main
    OR CHOOSE OF btnAcc IN FRAME frm-DAcc
DO:
  wsfrm = FRAME-NAME.
  VIEW FRAME frm-Ledger.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Ledger.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-Ledger 
          OR close of THIS-PROCEDURE IN FRAME frm-Ledger
          OR CHOOSE OF btn-ok IN FRAME frm-Ledger 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-Ledger.
  IF wsFrm = "frm-Main" THEN
    APPLY 'tab' TO OAcc IN FRAME frm-Main.
  ELSE IF  wsFrm = "frm-DAcc" THEN
      APPLY 'tab' TO DAcc IN FRAME frm-DAcc.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Ledger 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger EXCLUSIVE-LOCK NO-WAIT.
  
   IF wsFrm = "frm-Main" THEN
        DISPLAY glmf.acct @ OAcc glmf.DESCRIPTION WITH FRAME frm-Main.
   ELSE IF  wsFrm = "frm-DAcc" THEN
        DISPLAY glmf.acct @ DAcc glmf.DESCRIPTION WITH FRAME frm-DAcc.
   RETURN.
END.

ON 'enter':U OF OAcc IN FRAME frm-Main 
DO:
    FIND FIRST glmf WHERE glmf.Acct = DEC(OAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
      ASSIGN OAcc wsPer.
      DISPLAY glmf.Acct @ OAcc glmf.DESCRIPTION WITH FRAME frm-Main.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "Invalid Ledger, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF wsPer IN FRAME frm-Main
DO:
    ASSIGN wsPer.
     OPEN QUERY qry-Trans 
         FOR EACH gltdf WHERE gltdf.acct = DEC(OAcc:SCREEN-VALUE)
                          AND gltdf.period = int(wsPer:SCREEN-VALUE) NO-LOCK.
    RETURN.
END.

ON 'mouse-select-dblclick' OF brw-trans
    OR 'choose':U OF btn-ok IN FRAME frm-Main
DO: 
  GET CURRENT qry-Trans EXCLUSIVE-LOCK NO-WAIT.
  IF AVAILABLE gltdf THEN DO:
      VIEW FRAME frm-DAcc.
      ENABLE ALL WITH FRAME frm-DAcc.
      WAIT-FOR CHOOSE OF btn-Ok OR CHOOSE OF btn-exit 
          OR close of THIS-PROCEDURE IN FRAME frm-DAcc.
      HIDE FRAME frm-DAcc.
  END.
  RETURN. 
END.

ON 'enter':U OF DAcc IN FRAME frm-DAcc 
DO:
    FIND FIRST glmf WHERE glmf.Acct = DEC(DAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
      DISPLAY glmf.Acct @ DAcc glmf.DESCRIPTION WITH FRAME frm-DAcc.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "Invalid Ledger, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-DAcc 
DO:
   session:set-wait-state("").
   ASSIGN DAcc.
   ASSIGN wsYear = INT(SUBSTR(STRING(wsPer),1,4))
          wsMonth = INT(SUBSTR(STRING(wsPer),5,2))
          wsTime    = STRING(TIME,"HH:MM:SS").
          wsTransID = DEC (string(YEAR(TODAY),"9999")
                  + string(MONTH(TODAY),"99" )
                  + string(DAY(TODAY),"99")
                  + SUBSTR(STRING(wsTIME),1,2) 
                  + SUBSTR(STRING(wsTIME),4,2) 
                  + SUBSTR(STRING(wsTIME),7,2) ).
   /*{PrintOpt.i &stream-name="stream a"
                    &print-prog="Update.ip" 
                    &paged}*/
                    RUN UPDATE.ip.
                    
  CLEAR FRAME frm-DAcc.
  HIDE FRAME frm-DAcc.
  OPEN QUERY qry-Trans 
         FOR EACH gltdf WHERE gltdf.acct = OAcc AND gltdf.period = wsPer NO-LOCK.
  RETURN.
END.

/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE UPDATE.ip:
   DO TRANSACTION ON ERROR UNDO, LEAVE:
       ASSIGN gltdf.acct = DAcc.
       /* Ledger Balance */
       FIND FIRST glbal WHERE glbal.acct = OAcc 
           AND glbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF AVAILABLE glbal THEN
           glbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                           - gltdf.Amt.
       FIND FIRST glbal WHERE glbal.acct = DAcc 
           AND glbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF NOT AVAILABLE  glbal THEN DO:
           CREATE glbal.
           ASSIGN glbal.acct = dAcc
                  glbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)).
       END.
       glbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                           + gltdf.Amt.
       /* Fund Ledger Balance */
       FIND FIRST glFbal WHERE glFbal.acct = OAcc AND glFbal.fund = gltdf.fund 
           AND glFbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF AVAILABLE glFbal THEN
           glFbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glFbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                           - gltdf.Amt.
       FIND FIRST glFbal WHERE glFbal.acct = DAcc AND glFbal.fund = gltdf.fund
           AND glFbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF NOT AVAILABLE  glFbal THEN DO:
           CREATE glFbal.
           ASSIGN glFbal.acct = dAcc
                  glFbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4))
                  glFbal.fund = gltdf.fund.
       END.
       glFbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glFbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                           + gltdf.Amt.
       /* Proj Ledger Balance */
       FIND FIRST glPbal WHERE glPbal.acct = OAcc AND glPbal.Proj = gltdf.Proj 
           AND glPbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF AVAILABLE glPbal THEN
           glPbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glPbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                           - gltdf.Amt.
       FIND FIRST glPbal WHERE glPbal.acct = DAcc AND glPbal.Proj = gltdf.Proj
           AND glPbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF NOT AVAILABLE  glPbal THEN DO:
           CREATE glPbal.
           ASSIGN glPbal.acct = dAcc
                  glPbal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4))
                  glPbal.Proj = gltdf.Proj.
       END.
       glPbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glPbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                           + gltdf.Amt.

       /* Department Ledger Balance */
       FIND FIRST glDBal WHERE glDBal.acct = OAcc AND glDBal.Dept = gltdf.Dept 
              AND glDBal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF AVAILABLE glDBal THEN
          glDBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glDBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                            - gltdf.Amt.
       FIND FIRST glDBal WHERE glDBal.acct = DAcc AND glDBal.Dept = gltdf.Dept
            AND glDBal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4)) NO-ERROR.
       IF NOT AVAILABLE  glDBal THEN DO:
           CREATE glDBal.
           ASSIGN glDBal.acct = dAcc
                  glDBal.YEAR = INT(SUBSTR(STRING(gltdf.per),1,4))
                  glDBal.Dept = gltdf.Dept.
       END.
       glDBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glDBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] 
                                  + gltdf.Amt.
           /*DISPLAY STREAM a gltdf.transId gltdf.trDate gltdf.acct gltdf.descrip gltdf.amt @ wsAmt
               WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt. */
   END.
END.
