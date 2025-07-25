/* Program.................crdrpt02.p
   Notes:...... Creaditors Transaction Report
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
DEF VAR wsAcc1 LIKE crdmf.acc.
DEF VAR wsAcc2 LIKE crdmf.acc.
DEF VAR wsDate1 LIKE crdtmf.trDate.
DEF VAR wsDate2 LIKE crdtmf.trDate.
DEF VAR wsDescrip AS CHAR FORM "X(40)".
DEF VAR wsTitle1  AS CHAR FORM "X(50)" INITIAL "CREDITORS TRANSACTIONS REPORT".

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

DEF BUFFER bfrcrdtmf FOR crdtmf.

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
    wsDate1 COLON 23.5 LABEL "START DATE" SKIP(0.5)
    wsDate2 COLON 23.5 LABEL "  END DATE" SKIP(0.5)
    Btnacc1 COLON 5 NO-TAB-STOP wsAcc1 NO-LABEL wsName1  NO-LABEL VIEW-AS TEXT SKIP(0.5)
    BtnAcc2 COLON 8 NO-TAB-STOP wsAcc2 NO-LABEL wsName2 NO-LABEL VIEW-AS TEXT SKIP(2)
    wsStatus  COLON 30 LABEL "        Processing......" view-as text no-tab-stop
    skip(2.2)
    btn-ok AT ROW 18.5 COL 20
    btnclose AT ROW 18.5 COL 60
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 18 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "CREDITORS TRANSACTION REPORT".

FORM wsDescrip  AT 10 
     crdtmf.trDate 
     crdtmf.Ref
     crdtmf.Amt 
     crdtmf.decBal
     HEADER SKIP(1) wsTitle0 AT 20 skip(1) 
    wsTitle1 AT 20  "Page: " AT 88 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    "                                         INVOICE                       INVOCE         INVOICE" AT 10 SKIP
    "                                          DATE     REF                 AMOUNT         BALANCE" AT 10
        WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmRpt.

ON CHOOSE OF btnAcc1 IN FRAME frm-main
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
   DISPLAY crdmf.acc @ wsAcc1 wsName1 WITH FRAME frm-main.
   APPLY 'tab' TO wsAcc1 IN FRAME frm-main.
   RETURN.
END.

ON 'enter':U OF  wsAcc1 IN FRAME frm-main 
    OR 'tab':U OF wsAcc1 IN FRAME frm-main 
DO:
    FIND FIRST crdmf WHERE crdmf.acc = int( wsAcc1:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE crdmf THEN DO:
        DISPLAY crdmf.acc @ wsAcc1 crdmf.NAME @ wsName1 WITH FRAME frm-main.
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
            wsName1 = wsName1:SCREEN-VALUE
            wsDate1 wsDate2.
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
   FOR EACH crdtmf WHERE crdtmf.acc >= wsAcc1 AND crdtmf.acc <= wsAcc2 
       AND crdtmf.trDate >= wsDate1 AND crdtmf.trDate <= wsDate2 NO-LOCK
        BREAK BY crdtmf.acc:
        IF FIRST-OF(crdtmf.acc) THEN DO:
            wsTotal = 0.
            FOR EACH bfrcrdtmf WHERE bfrcrdtmf.trDate < wsDate1 AND bfrcrdtmf.acc = crdtmf.acc:
                wsTotal = wsTotal + bfrcrdtmf.decBal.
            END.
            FIND FIRST crdmf WHERE crdmf.acc = crdtmf.acc NO-LOCK NO-ERROR.
            DISPLAY STREAM a crdmf.NAME @ wsDescrip WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
            DISPLAY STREAM a "    " + "BALANCE B/F" @ wsDescrip wsTotal @ crdtmf.decbal WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
            wsTotal = wsTotal + crdtmf.decBal.
            DISPLAY crdmf.acc @ wsStatus WITH FRAME frm-main.
            PAUSE 0.
            wsDescrip = "    " + crdtmf.Descrip.
            DISPLAY STREAM a crdtmf.trDate crdtmf.Ref wsDescrip crdtmf.Amt crdtmf.decBal WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        IF LAST-OF(crdtmf.acc) THEN DO:
            UNDERLINE STREAM a crdtmf.decBal WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
            DISPLAY STREAM a wsTotal @ crdtmf.decBal WITH FRAM frmrpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.
   END.
   DISPLAY STREAM a "GRAND TOTAL" @ wsDescrip wsTotal @ crdtmf.decBal WITH FRAME frmRpt.
   DOWN STREAM a WITH FRAME frmRpt.
   APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
