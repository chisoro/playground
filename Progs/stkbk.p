/* Program.................Stkbk.p
   Notes:................. Print Stock-take sheets
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE pgorientation     "portrait"
&SCOPED-DEFINE fontsize           12 
DEF STREAM a.

DEF VAR w-orientation AS CHAR.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsper      LIKE simctr.curper.
DEF VAR wsyear     AS INT.
DEF VAR wsmonth    AS INT.
DEF VAR wsQ        AS CHAR INITIAL "______________________".
DEF VAR wsWhse1 LIKE stkwhf.WareHse.
DEF VAR wsWhse2 LIKE stkwhf.WareHse.
DEF VAR wsDescrip LIKE stktrans.Descrip.
DEF VAR wstitle AS CHAR FORM "x(80)".
DEF VAR wsbutton AS INT.

DEF BUTTON btn-Print  LABEL "PRINT".
DEF BUTTON btn-whse1  LABEL "FROM WAREHOUSE".
DEF BUTTON btn-whse2  LABEL "TO WAREHOUSE".
DEF BUTTON btn-Ok     LABEL "OK".
DEF BUTTON btn-Exit   LABEL "CANCEL".

DEFINE RECTANGLE rect1
         EDGE-PIXELS 2 graphic-edge  NO-FILL
         SIZE 98 BY 2.3.

DEFINE RECTANGLE rect2
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
         SIZE 98 BY 10.5.


DEF    QUERY qry-PickWhse FOR stkwhf SCROLLING.
DEF BROWSE brw-PickWhse QUERY qry-PickWhse
    DISPLAY stkwhf.WareHse stkwhf.Descrip   
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickWhse 
    brw-PickWhse AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ware House Selection".


DEFINE FRAME frm-input
    SKIP(1)
      wsper         COLON 35 LABEL "Accounting Period" SKIP(1)
      btn-whse1     COLON 13 NO-TAB-STOP wsWhse1 NO-LABEL
      stkwhf.Descrip         NO-LABEL VIEW-AS TEXT SKIP(0.5)
       btn-whse2     COLON 16 NO-TAB-STOP wsWhse2 NO-LABEL
      btn-Print     AT ROW 12 COL 21 SPACE(45) btn-Exit   LABEL "CLOSE" 
      rect2 AT ROW 1 COL 3
      rect1 AT ROW 11.6 COL 3 
    with view-as dialog-box NO-VALIDATE SIZE 105 BY 15
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PRINT STOKC-TAKE SHEETS".

FORM SPACE(10)
     stkmf.Stkcode           COLUMN-LABEL "STOCK"
     stkmf.descrip           COLUMN-LABEL "DESCRIPTION"
     wsq                     COLUMN-LABEL "QUANTITY" SKIP(1)
     HEADER skip(5) wstitle AT 20 SKIP(1)
           "STORES STOCK-TAKE SHEET   " AT 10 "PERIOD: " wsper " DATE :__________________ " SKIP(1)
          "RECORDED BY:__________________________________________"  AT 10 SKIP(2)
    WITH DOWN STREAM-IO FONT 20 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.


/******* Triggers ***** */
ON CHOOSE OF btn-whse1 IN FRAME  frm-input
    OR CHOOSE OF btn-whse2 IN FRAME  frm-input
DO:
  IF SELF:LABEL = "FROM WAREHOUSE" THEN 
      wsbutton = 1.
  ELSE IF SELF:LABEL = "TO WAREHOUSE" THEN 
      wsbutton = 2.
  VIEW FRAME frm-Pickwhse.
  OPEN QUERY qry-Pickwhse FOR EACH stkwhf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Pickwhse.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-Pickwhse 
          OR close of THIS-PROCEDURE IN FRAME frm-PickWhse 
          OR CHOOSE OF btn-ok IN FRAME frm-PickWhse 
          OR 'enter':u OF brw-Pickwhse
          OR 'mouse-select-dblclick' OF brw-Pickwhse.
  CLOSE QUERY qry-Pickwhse.
  HIDE FRAME frm-Pickwhse.
  APPLY 'tab' TO SELF.
  IF wsbutton = 1 THEN
     APPLY 'tab' TO wsWhse1 IN FRAME frm-input .
  ELSE IF wsbutton = 2 THEN
     APPLY 'tab' TO wsWhse2 IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Pickwhse 
    OR 'enter':u OF brw-Pickwhse 
    OR 'mouse-select-dblclick' OF brw-Pickwhse 
DO: 
   GET CURRENT qry-Pickwhse  EXCLUSIVE-LOCK NO-WAIT.
   IF wsbutton = 1 THEN
       DISPLAY  stkwhf.WareHse  @ wsWhse1 stkwhf.Descrip WITH FRAME frm-input.
   ELSE IF wsbutton = 2 THEN
       DISPLAY  stkwhf.WareHse  @ wsWhse2 WITH FRAME frm-input. 
   RETURN.
END.

ON 'enter' OF wsWhse1 IN FRAME frm-input 
    OR 'tab' OF wsWhse1 IN FRAME frm-input
DO:
    ASSIGN wsWhse1 = INT(wsWhse1:SCREEN-VALUE).
    FIND FIRST stkwhf WHERE stkwhf.WareHse =  wsWhse1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkwhf THEN DO:
              MESSAGE "Warehouse does not exist" VIEW-AS ALERT-BOX.
              RETURN NO-APPLY.
        END.
    DISPLAY stkwhf.Descrip WITH FRAME frm-input.
END.

ON 'tab':U OF wsper IN FRAME frm-input
    OR 'leave':U OF wsper IN FRAME frm-input
DO:
    ASSIGN wsPer = DEC(wsPer:SCREEN-VALUE).
    IF wsPer <= SIMCTR.CLOSEPER THEN DO:
        BELL.
        MESSAGE "Period entered has been closed" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF wsPer > SIMCTR.curper THEN DO:
        BELL.
        MESSAGE "Period entered is outside the open accounting periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-Print IN FRAME frm-Input 
DO:
    ASSIGN wsWhse1 = INT(wsWhse1:SCREEN-VALUE)
           wsWhse2 = INT(wsWhse2:SCREEN-VALUE).
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="rpt-ip"
                    &paged}
    APPLY 'entry' TO btn-exit.
    RETURN.
END.


/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
wstitle = SIMCTR.CONAME.
CLEAR FRAME frm-Input.
ENABLE ALL WITH FRAME frm-Input.
ASSIGN wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsWhse1:SCREEN-VALUE = "1"
       wsWhse2:SCREEN-VALUE = "999".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-main.

PROCEDURE rpt-ip:
    FOR EACH  stkmf WHERE  stkmf.WareHse >= wsWhse1 AND stkmf.WareHse <= wsWhse2 NO-LOCK
                               BREAK BY stkmf.WareHse:
        IF FIRST-OF(stkmf.WareHse) THEN DO:
            FIND FIRST stkwhf WHERE stkwhf.WareHse = stkmf.WareHse NO-LOCK NO-ERROR.
            DISPLAY STREAM a "WAREHOUSE:" @ stkmf.Stkcode stkwhf.Descrip @ stkmf.descrip WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        DISPLAY STREAM a stkmf.Stkcode stkmf.descrip wsq WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
    RETURN.
END.
    
    
