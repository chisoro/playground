session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF VAR wsfile AS CHAR FORM "x(40)".
DEF BUTTON btnDate  LABEL "Pay Date".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEFINE TEMP-TABLE tmpDate
    FIELD trDate LIKE payhtf.trDate.

DEF  QUERY qry-PickDate FOR tmpDate SCROLLING.
DEF BROWSE brw-PickDate QUERY qry-PickDate
    DISPLAY tmpDate.trDate 
    WITH 10 DOWN SEPARATORS.

DEFINE FRAME frm-PickDate 
    brw-PickDate AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 3
    btn-close colon 10
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Pay Date Selection".


define frame frm-input
    SKIP(2)
    btnDate  COLON 5 NO-LABEL NO-TAB-STOP 
    payhtf.trDATE     NO-LABEL VIEW-AS TEXT
    btn-Ok AT ROW 8.71 COL 10 WIDGET-ID 48
    btn-Close AT ROW 8.71 COL 30 WIDGET-ID 48
    
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         /*AT COL 2.2 ROW 1.19 */
         SIZE 40.8 BY 10.1
         BGCOLOR 11 FGCOLOR 1  WIDGET-ID 100
         TITLE "Historical Paymaster".

/*triggere*/
ON CHOOSE OF BtnDate IN FRAME frm-Input
   
DO:
  
    FOR EACH payhtf BREAK BY payhtf.trDate.
        IF LAST-OF(payhtf.trDate) THEN DO:
            CREATE tmpDate.
            ASSIGN tmpDate.trDate = payhtf.trDate.
        END.
    END.


  VIEW FRAME frm-PickDate.
  OPEN QUERY qry-PickDate FOR EACH tmpDate.
  ENABLE ALL WITH FRAME frm-PickDate.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickDate 
          OR close of THIS-PROCEDURE IN FRAME frm-PickDate
          OR CHOOSE OF btn-ok IN FRAME frm-PickDate 
          OR 'enter':u OF brw-PickDate
          OR 'mouse-select-dblclick' OF brw-PickDate.
  CLOSE QUERY qry-PickDate.
  FOR EACH tmpDate.
      DELETE tmpDate.
  END.
  HIDE FRAME frm-PickDate.
  APPLY 'tab' TO btnDate IN FRAME frm-Input.
  RETURN. 
END.
    
ON CHOOSE OF btn-ok IN FRAME frm-PickDate 
    OR 'enter':u OF brw-PickDate
    OR 'mouse-select-dblclick' OF brw-PickDate
DO: 
   GET CURRENT qry-PickDate NO-LOCK NO-WAIT.
   payhtf.trDate:SCREEN-VALUE IN FRAME frm-Input = string(tmpDate.trDate).
   APPLY 'entry' TO btn-ok IN FRAME frm-Input.
   RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Input 
    
DO: 
   OUTPUT STREAM b TO VALUE(wsFile).
    EXPORT STREAM b DELIMITER "," "NAME" "ITEM" "AMOUNT".
    FOR EACH payitem:
        FOR EACH payhtf WHERE payitem.itemcode = payhtf.itemcode AND payhtf.trDate = DATE(payhtf.trDate:SCREEN-VALUE IN FRAME frm-Input):
            FOR EACH payemf WHERE payhtf.empcode = payemf.empcode:
                EXPORT STREAM b DELIMITER "," FName + " " + SName descrip CurAmt. 
            END.
        END.
    END.
    OUTPUT STREAM b CLOSE.
    OS-COMMAND NO-WAIT VALUE("c:\simacc\bin\paymasterH.xlsm").
    RETURN. 
END.
/*main logic */

FIND FIRST SIMCTR NO-LOCK.
wsFile = simctr.repDir + "pitemH.csv". 
session:set-wait-state("").
ENABLE ALL WITH FRAME frm-input.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.


