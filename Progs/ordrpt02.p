/* Program.................ordrpt02.p
   Notes:......Orders Report by Supplier
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE pgorientation "PORTRAIT"

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
DEF VAR wsAcc1 LIKE crdmf.Acc.
DEF VAR wsAcc2 LIKE crdmf.Acc.
DEF VAR wsDate1 LIKE ordmf.ordDate.
DEF VAR wsDate2 LIKE ordmf.ordDate.
DEF VAR wsDescrip AS CHAR FORM "X(75)".
DEF VAR wsTitle1  AS CHAR FORM "X(70)" INITIAL "OUTSTANDING ORDERS' REPORT".
DEF VAR wsopt      AS INT VIEW-AS RADIO-SET HORIZONTAL 
    RADIO-BUTTONS "Outstanding Orders", 1,
                  "Invoiced Orders", 2.
DEF VAR wsBtn AS CHAR.
DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "PROCESS".
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
     wsOpt AT ROW 2 COL 10 LABEL "Select Report Filter" AUTO-RETURN SKIP(1)
    wsDate1 COLON 24 LABEL "START DATE" SKIP(0.5)
    wsDate2 COLON 24 LABEL "  END DATE" SKIP(0.5)
    btnAcc1 COLON 5  NO-TAB-STOP wsAcc1 NO-LABEL Crdmf.NAME NO-LABEL VIEW-AS TEXT SKIP(0.5)
    btnAcc2 COLON 8  NO-TAB-STOP wsAcc2 NO-LABEL  SKIP(2)
    wsStatus  COLON 30 LABEL "        Processing......" view-as text no-tab-stop
    skip(2.2)
    btn-ok AT ROW 18.5 COL 20
    btnclose AT ROW 18.5 COL 60
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 18 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "ORDER'S REPORT".

FORM SPACE(5)
     ordmf.descrip NO-LABEL   FORM "x(40)"
     ordmf.ordNo   NO-LABEL
     ordmf.OrdDate NO-LABEL
     wsAmt         NO-LABEL 
     wsTotal       NO-LABEL
    HEADER SKIP(1) wsTitle0 AT 20 skip(1) wsDescrip AT 5 SKIP(3)
    wsTitle1 AT 5 "Page: " AT 75 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    "DESCRIPTION                          ORDER NUMBER   DATE        AMOUNT         TOTAL"  AT 10
        WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 100 FRAME frmRpt.

ON 'mouse-select-click' OF wsopt IN FRAME frm-Main
    OR 'leave' OF wsOpt IN FRAME frm-main
    OR 'enter' OF wsOpt IN FRAME frm-Main
DO:
    ASSIGN wsOpt.
    APPLY 'tab' TO wsOpt.
END.

ON CHOOSE OF btnAcc1 IN FRAME frm-main
DO:
  wsbtn = "Acc1".
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
  APPLY 'tab' TO wsAcc1 IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btnAcc2 IN FRAME frm-main
DO:
  wsbtn = "Acc2".
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
  APPLY 'tab' TO wsAcc2 IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickSup 
    OR 'enter':u OF brw-PickSup
    OR 'mouse-select-dblclick' OF brw-PickSup
DO: 
   GET CURRENT qry-PickSup EXCLUSIVE-LOCK NO-WAIT.
   IF wsbtn = "Acc1" THEN DO:
       DISPLAY crdmf.acc @ wsAcc1 crdmf.NAME WITH FRAME frm-main.
       APPLY 'tab' TO wsAcc1 IN FRAME frm-main.
   END.
   IF wsbtn = "Acc2" THEN DO:
       DISPLAY crdmf.acc @ wsAcc2 WITH FRAME frm-main.
       APPLY 'tab' TO wsAcc2 IN FRAME frm-main.
   END.
   RETURN.
END.

ON 'enter':U OF  wsAcc1 IN FRAME frm-main 
    OR 'tab':U OF wsAcc1 IN FRAME frm-main 
DO:
    FIND FIRST crdmf WHERE crdmf.acc = int( wsAcc1:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE crdmf THEN DO:
        DISPLAY crdmf.acc @ wsAcc1 crdmf.NAME  WITH FRAME frm-main.
        APPLY 'tab' TO wsAcc1 IN FRAME frm-main.
        ASSIGN wsAcc1.
    END.
    ELSE IF NOT AVAILABLE crdmf THEN DO:
        MESSAGE  wsAcc1 " Invalid Supplier, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF  wsAcc2 IN FRAME frm-main 
    OR 'tab':U OF wsAcc2 IN FRAME frm-main 
DO:
    ASSIGN wsAcc2.
    APPLY 'tab' TO wsAcc2 IN FRAME frm-main.
    RETURN.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN wsAcc1 = INT(wsAcc1:SCREEN-VALUE)
            wsAcc2 = INT(wsAcc2:SCREEN-VALUE)
            wsDate1 = DATE(wsDate1:SCREEN-VALUE)
            wsDate2 = DATE(wsDate2:SCREEN-VALUE)
            wsOpt   = INT(wsOpt:SCREEN-VALUE)
            wsTitle1 = "OUTSTANDING ORDERS' REPORT" WHEN wsOpt = 1
            wsTitle1 = "INVOICED ORDERS' REPORT" WHEN wsOpt = 2.
      wsTitle1 = wsTitle1 + " FOR THE PERIOD " + STRING(wsDate1) + " TO " + STRING(wsDate2). 
    {PrintOpt.i &stream-name="stream a"
                    &print-prog= rep.ip
                    &paged}
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsDate1:SCREEN-VALUE = "01/" + STRING(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY))
       wsDate2:SCREEN-VALUE = STRING(TODAY)
       wsAcc1:SCREEN-VALUE = "1"
       wsAcc2:SCREEN-VALUE = "999999"
       wsTitle0 = simctr.CONAME.  
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Rep.ip:
      wsTotal = 0.
    FOR EACH ordmf WHERE ordmf.ordDate >= wsDate1 AND ordmf.OrdDate <= wsDate2 
        AND crdmf.acc >= wsAcc1 AND ordmf.acc <= wsAcc2  AND ordmf.ordstat = wsOpt NO-LOCK
        BREAK BY ordmf.Acc:
        DISPLAY ordmf.OrdNo @ wsStatus WITH FRAME frm-Main.
        PAUSE 0.
        IF FIRST-OF(ordmf.acc) THEN DO:
            wsAmt = 0.
            FIND FIRST crdmf WHERE crdmf.acc = ordmf.acc NO-LOCK NO-ERROR.
            DISPLAY STREAM a crdmf.NAME @ ordmf.descrip WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        ASSIGN wsAmt = wsAmt + ordmf.OrdAmt
               wsTotal = wsTotal + ordmf.OrdAmt.
        DISPLAY STREAM a ordmf.Ordno ordmf.descrip ordmf.OrdDate ordmf.OrdAmt @ wsAmt WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
        IF LAST-OF(ordmf.acc) THEN DO:
            DISPLAY STREAM a wsAmt @ wsTotal WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.
    END.
    UNDERLINE STREAM a wsTotal WITH FRAME frmRpt.
    DOWN STREAM a WITH FRAME frmRpt.
    DISPLAY STREAM a "GRAND TOTAL" @ ordmf.descrip wsTotal WITH FRAME frmRpt.
    DOWN STREAM a WITH FRAME frmRpt.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
