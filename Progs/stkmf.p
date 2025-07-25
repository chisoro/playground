session:DATA-ENTRY-RETURN = TRUE.
/* Program.................Stkmf.p
   Notes:.................Stock file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Stock Code file already exist"
&SCOPED-DEFINE wsTitle         "Stock File Maintenance"
&SCOPED-DEFINE tmptable        Stkmf
&SCOPED-DEFINE skey           stkmf.stkCode
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.WareHse ~
                                        COLUMN-LABEL ' Ware House ':C ~
                              bfr{&tmptable}.cat ~
                                        COLUMN-LABEL ' Category ':C ~
                               bfr{&tmptable}.sub-cat ~
                                        COLUMN-LABEL ' Sub-Category ':C ~
                              btnCat ~
                              btnSubCat ~
                              bfr{&tmptable}.Measure~
                                       COLUMN-LABEL ' Unit of Measure ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.WareHse ~
                                        COLUMN-LABEL ' WAREHOUSE ':C ~
                              bfr{&tmptable}.stkCode ~
                                        COLUMN-LABEL ' STOCK ':C~
                              bfr{&tmptable}.Descrip ~
                                        COLUMN-LABEL ' DESCRIPTION 'WIDTH 35 ~
                              bfr{&tmptable}.Cat ~
                                        COLUMN-LABEL ' CATEGORY ':C ~
                              bfr{&tmptable}.sub-cat ~
                                        COLUMN-LABEL ' SUB-CATEGORY ':C
                              
                              


{varlib.i}

DEF BUTTON btnWHse LABEL "Ware House".

DEF    QUERY qry-whse FOR stkwhf SCROLLING.
DEF BROWSE brw-whse QUERY qry-whse
    DISPLAY stkwhf.WareHse stkwhf.Descrip COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-cat FOR stkcat SCROLLING.
DEF BROWSE brw-cat QUERY qry-cat
    DISPLAY stkcat.CAT stkcat.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-subcat FOR stkscat SCROLLING.
DEF BROWSE brw-subcat QUERY qry-subcat
    DISPLAY stkscat.sub-cat stkscat.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

define frame frm-input
    SKIP(0.5)
     btnWhse                COLON 16 NO-TAB-STOP
     bfr{&tmptable}.WareHse NO-LABEL stkwhf.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    {&skey}                 COLON 30 label "Stock Code" SKIP(0.5)
    bfr{&tmptable}.descrip  COLON 30 LABEL "Description"  SKIP(0.5)   
    bfr{&tmptable}.measure  COLON 30 LABEL "Unit of Measure" SKIP(0.5)
    btnCat                  COLON 18 NO-TAB-STOP 
    bfr{&tmptable}.CAT      COLON 30 NO-LABEL
    stkcat.DESCRIP          VIEW-AS TEXT NO-LABEL SKIP(0.5)
    btnSubCat               COLON 15 NO-TAB-STOP
    bfr{&tmptable}.sub-cat   COLON 30 NO-LABEL
    stkscat.DESCRIP         VIEW-AS TEXT NO-LABEL 
    skip(2.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STOCK MASTER DATA CAPTURE".
    
DEFINE FRAME frm-whse 
    brw-whse AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ware House Selection".

DEFINE FRAME frm-pick 
    brw-cat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEFINE FRAME frm-subcat 
    brw-subcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Sub-Category Selection".

/* ***** Triggers for the main frame **** */

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw-{&tmptable}
        OR 'mouse-select-dblclick' OF brw-{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        DISABLE {&skey} WITH FRAME frm-input.
        run proc-edit.
        RETURN.
END.

ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
   IF wsid = "0" THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          /*CLEAR FRAME frm-input ALL. */
          RETURN NO-APPLY.
       END.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.stkcode.
    FIND FIRST stkTrans WHERE stktrans.stkCode  = wsid NO-ERROR.
    IF AVAILABLE stkTrans THEN DO:
        MESSAGE "Stock has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE stkTrans THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON CHOOSE OF btnwhse IN FRAME frm-input
DO:
  VIEW FRAME frm-whse.
  OPEN QUERY qry-whse FOR EACH stkwhf NO-LOCK.
  ENABLE ALL WITH FRAME frm-whse.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-whse 
          OR close of THIS-PROCEDURE IN FRAME frm-whse
          OR CHOOSE OF btn-ok IN FRAME frm-whse 
          OR 'enter':u OF brw-whse
          OR 'mouse-select-dblclick' OF brw-whse.
  CLOSE QUERY qry-whse.
  HIDE FRAME frm-whse.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON 'enter':U OF bfr{&tmptable}.WareHse IN FRAME frm-input 
DO:
    FIND FIRST stkwhf WHERE stkwhf.WareHse = int(bfr{&tmptable}.WareHse:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE stkwhf THEN DO:
      DISPLAY stkwhf.WareHse @ bfr{&tmptable}.WareHse stkwhf.Descrip WITH FRAME frm-input.
    END.
    ELSE IF NOT AVAILABLE stkwhf THEN DO:
        MESSAGE "Invalid Warehouse, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-whse 
    OR 'enter':u OF brw-whse
    OR 'mouse-select-dblclick' OF brw-whse
DO: 
   GET CURRENT qry-whse EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY stkwhf.WareHse @ bfr{&tmptable}.WareHse stkwhf.Descrip WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnCat IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-cat FOR EACH stkcat NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-cat
          OR 'mouse-select-dblclick' OF brw-cat.
  CLOSE QUERY qry-cat.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON 'enter':U OF bfr{&tmptable}.cat IN FRAME frm-input 
DO:
    FIND FIRST stkcat WHERE stkcat.cat = int(bfr{&tmptable}.cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE stkcat THEN DO:
      DISPLAY stkcat.cat @ bfr{&tmptable}.cat stkcat.DESCRIP WITH FRAME frm-input.
    END.
    ELSE IF NOT AVAILABLE stkcat THEN DO:
        MESSAGE "Invalid Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-cat
    OR 'mouse-select-dblclick' OF brw-cat
DO: 
   GET CURRENT qry-cat EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY stkcat.cat @ bfr{&tmptable}.cat stkcat.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.sub-cat IN FRAME frm-input
    OR 'TAB' OF bfr{&tmptable}.sub-cat IN FRAME frm-input
DO:
    FIND FIRST stkscat WHERE stkscat.sub-cat = DEC(bfr{&tmptable}.sub-cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE stkscat THEN DO:
      DISPLAY stkscat.sub-cat @ bfr{&tmptable}.sub-cat stkscat.DESCRIP WITH FRAME frm-input.
    END.
    ELSE IF NOT AVAILABLE stkscat THEN DO:
        MESSAGE "Invalid Sub-Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnsubCat IN FRAME frm-input
DO:
  VIEW FRAME frm-subcat.
  OPEN QUERY qry-subcat FOR EACH stkscat NO-LOCK.
  ENABLE ALL WITH FRAME frm-subcat.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-subcat 
          OR close of THIS-PROCEDURE IN FRAME frm-subcat
          OR CHOOSE OF btn-ok IN FRAME frm-subcat 
          OR 'enter':u OF brw-subcat
          OR 'mouse-select-dblclick' OF brw-subcat.
  CLOSE QUERY qry-subcat.
  HIDE FRAME frm-subcat.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-subcat 
    OR 'enter':u OF brw-subcat
    OR 'mouse-select-dblclick' OF brw-subcat
DO: 
   GET CURRENT qry-subcat NO-LOCK NO-WAIT.
   DISPLAY stkscat.sub-cat @ bfr{&tmptable}.sub-cat stkscat.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.stkcode  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Stock code already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.stkcode = wsid
                   bfr{&tmptable}.DESCRIP = bfr{&tmptable}.DESCRIP:SCREEN-VALUE IN FRAME frm-input 
                   bfr{&tmptable}.measure = bfr{&tmptable}.measure:SCREEN-VALUE
                   bfr{&tmptable}.WareHse = INT(bfr{&tmptable}.WareHse:SCREEN-VALUE)
                   bfr{&tmptable}.CAT     = INT(bfr{&tmptable}.CAT:SCREEN-VALUE)
                   bfr{&tmptable}.sub-cat = DEC(bfr{&tmptable}.sub-cat :SCREEN-VALUE).
            RELEASE bfr{&tmptable}.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO bfr{&tmptable}.WareHse IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.stkcode = wsid
                   bfr{&tmptable}.DESCRIP = bfr{&tmptable}.DESCRIP:SCREEN-VALUE IN FRAME frm-input 
                   bfr{&tmptable}.measure = bfr{&tmptable}.measure:SCREEN-VALUE
                   bfr{&tmptable}.WareHse = INT(bfr{&tmptable}.WareHse:SCREEN-VALUE)
                   bfr{&tmptable}.CAT     = INT(bfr{&tmptable}.CAT:SCREEN-VALUE)
                   bfr{&tmptable}.sub-cat = DEC(bfr{&tmptable}.sub-cat :SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

/* *********INTERNAL PROCEDURES ***********/
PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.stkcode.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.stkcode  = wsid EXCLUSIVE-LOCK NO-WAIT NO-ERROR .
    FIND FIRST stkwhf WHERE stkwhf.WareHse = bfr{&tmptable}.WareHse NO-LOCK NO-ERROR.
    DISPLAY {&updFields} wsid @ {&skey} stkwhf.Descrip  WITH FRAME frm-input.
    FIND FIRST stkcat WHERE stkcat.cat = bfr{&tmptable}.cat NO-LOCK NO-ERROR. 
    IF AVAILABLE STKCAT THEN
        DISPLAY  stkcat.descrip WITH FRAME frm-input.
    FIND FIRST stkscat WHERE stkscat.sub-cat = dec(bfr{&tmptable}.sub-cat) NO-LOCK NO-ERROR.
    IF AVAILABLE stkscat THEN
        DISPLAY  stkScat.descrip WITH FRAME frm-input.
    ENABLE {&updFields} btnwhse btnCat btnSubCat bfr{&tmptable}.descrip
      bfr{&tmptable}.sub-cat btn-ok btn-close WITH FRAME  frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME  frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME  frm-input.
 END.

    
