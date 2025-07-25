/* Program.................ordrpt01.p
   Notes:...... Outstanding Orders Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE pgorientation "landscape"

DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsStatus     LIKE gltdf.acct.
DEF VAR wsAmt        AS DEC     FORM "zzzzzzzz9.99-".
DEF VAR wsTotal      AS DEC     FORM "zzzzzzzz9.99-".
DEF VAR wsTitle  AS CHAR FORM "X(60)".
DEF VAR wsTitle0  AS CHAR FORM "X(60)".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsper LIKE simctr.CURPER.
DEF VAR wsName1 LIKE crdmf.NAME.
DEF VAR wsName2 LIKE crdmf.NAME.
DEF VAR wsDate1 LIKE ordmf.ordDate.
DEF VAR wsDate2 LIKE ordmf.ordDate.
DEF VAR wsDescrip AS CHAR FORM "X(75)".
DEF VAR wsTitle1  AS CHAR FORM "X(50)" INITIAL "OUTSTANDING ORDERS' REPORT".

DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "Process".
DEF BUTTON btnAcc1   LABEL "FROM ACCOUNT".
DEF BUTTON btnAcc2   LABEL "TO ACCOUNT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 16.5.

DEF    QUERY qry-PickSup FOR crdmf SCROLLING.
DEF BROWSE brw-PickSup QUERY qry-PickSup
    DISPLAY Crdmf.Acc Crdmf.Name 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickSup 
    brw-PickSup AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btnclose colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "SUPPLIER SELECTION".

DEFINE FRAME frm-main
    SKIP(2)
    wsDate1 COLON 20 LABEL "START DATE" SKIP(0.5)
    wsDate2 COLON 20 LABEL "START DATE" SKIP(2)
    wsStatus  COLON 30 LABEL "        Processing......" view-as text no-tab-stop
    skip(2.2)
    btn-ok AT ROW 18.5 COL 20
    btnclose AT ROW 18.5 COL 60
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 18 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "OUTSTANDING ORDER'S REPORT".

FORM ordmf.ordNo   NO-LABEL 
     ordmf.descrip NO-LABEL  FORM "x(40)"
     ordmf.OrdDate NO-LABEL
     Crdmf.Name    NO-LABEL FORM "x(40)"
     wsAmt         NO-LABEL 
     wsTotal       NO-LABEL
    HEADER SKIP(1) wsTitle0 AT 20 skip(1) wsDescrip AT 5 SKIP(3)
    wsTitle1 AT 5  "Page: " AT 110 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    " ORDER         DESCRIPTION                             DATE     SUPPLIER                                   AMOUNT         BALANCE" SKIP(1)
        WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmRpt.

/*ON CHOOSE OF btnAcc1 IN FRAME frm-main
DO:
  VIEW FRAME frm-PickSup.
  OPEN QUERY qry-PickSup FOR EACH crdmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickSup.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-PickSup 
          OR close of THIS-PROCEDURE IN FRAME frm-PickSup
          OR CHOOSE OF btn-ok IN FRAME frm-PickSup 
          OR 'enter':u OF brw-PickSup
          OR 'mouse-select-dblclick' OF brw-PickSup.
  CLOSE QUERY qry-PickSup.
  HIDE FRAME frm-PickSup.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsName1 IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btnAcc2 IN FRAME frm-main
DO:
  VIEW FRAME frm-PickSup.
  OPEN QUERY qry-PickSup FOR EACH crdmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickSup.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-PickSup 
          OR close of THIS-PROCEDURE IN FRAME frm-PickSup
          OR CHOOSE OF btn-ok IN FRAME frm-PickSup 
          OR 'enter':u OF brw-PickSup
          OR 'mouse-select-dblclick' OF brw-PickSup.
  CLOSE QUERY qry-PickSup.
  HIDE FRAME frm-PickSup.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsName2 IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickSup 
    OR 'enter':u OF brw-PickSup
    OR 'mouse-select-dblclick' OF brw-PickSup
DO: 
   GET CURRENT qry-PickSup EXCLUSIVE-LOCK NO-WAIT.
   wsName1 = crdmf.NAME.
   DISPLAY crdmf.acc @ wsDate1 wsName1 WITH FRAME frm-main.
   APPLY 'tab' TO wsDate1 IN FRAME frm-main.
   RETURN.
END.

ON 'enter':U OF  wsDate1 IN FRAME frm-main 
    OR 'tab':U OF wsDate1 IN FRAME frm-main 
DO:
    FIND FIRST crdmf WHERE crdmf.acc = int( wsDate1:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE crdmf THEN DO:
        DISPLAY crdmf.acc @ wsDate1 crdmf.NAME @ wsName1 WITH FRAME frm-main.
        APPLY 'tab' TO wsDate1 IN FRAME frm-main.
        ASSIGN wsDate1.
    END.
    ELSE IF NOT AVAILABLE crdmf THEN DO:
        MESSAGE  wsDate1 " Invalid Supplier, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF  wsDate2 IN FRAME frm-main 
    OR 'tab':U OF wsDate2 IN FRAME frm-main 
DO:
    ASSIGN wsDate2.
    APPLY 'tab' TO wsDate2 IN FRAME frm-main.
    RETURN.
END.*/


ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN wsDate1 = DATE(wsDate1:SCREEN-VALUE)
            wsDate2 = DATE(wsDate2:SCREEN-VALUE).
    {PrintOpt.i &stream-name="stream a"
                    &print-prog= rep.ip
                    &paged}
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsDate1:SCREEN-VALUE = "01/" + STRING(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY))
       wsDate2:SCREEN-VALUE = STRING(TODAY)
       wsTitle0 = simctr.CONAME.    
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Rep.ip:
    wsTotal = 0.
    FOR EACH ordmf WHERE ordmf.ordDate >= wsDate1 AND ordmf.OrdDate <= wsDate2 
        AND ordmf.ordstat = 1 OR (ordmf.ordstat = 2 AND ordmf.InvAmt < ordmf.ordAmt) NO-LOCK:
        DISPLAY ordmf.OrdNo @ wsStatus WITH FRAME frm-Main.
        PAUSE 0.
        FIND FIRST crdmf WHERE crdmf.acc = ordmf.Acc NO-LOCK NO-ERROR.
        wsTotal = wsTotal + ordmf.OrdAmt.
        DISPLAY STREAM a ordmf.Ordno ordmf.descrip ordmf.OrdDate crdmf.NAME ordmf.OrdAmt @ wsAmt wsTotal WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    UNDERLINE STREAM a wsTotal WITH FRAME frmRpt.
    DOWN STREAM a WITH FRAME frmRpt.
    DISPLAY STREAM a "GRAND TOTAL" @ Crdmf.Name wsTotal WITH FRAME frmRpt.
    DOWN STREAM a WITH FRAME frmRpt.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
