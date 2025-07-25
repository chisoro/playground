session:DATA-ENTRY-RETURN = TRUE.
/* Program.................asubcat.p
   Notes:...... Capture Asset Sub-Categories
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Sub-Category file already exist"
&SCOPED-DEFINE wsTitle           "Sub-Category file Maintenance"
&SCOPED-DEFINE tmptable             asubcat
&SCOPED-DEFINE skey                 asubcat.subcat
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.SubCat ~
                                        COLUMN-LABEL ' Sub-Category ':C ~
                              bfr{&tmptable}.Cat ~
                                        COLUMN-LABEL ' Category ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                          WIDTH 60 COLUMN-LABEL ' Description ':C

{varlibrary.i}

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 18.5.

DEF    QUERY qry-Pickcat FOR acat SCROLLING.
DEF BROWSE brw-Pickcat QUERY qry-Pickcat
    DISPLAY aCAT.CAT aCAT.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickCat 
    brw-Pickcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 4
    btn-add AT ROW 20.7 COL 5
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 23.5
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                   COLON 35 LABEL "Sub-Category" SKIP(.5)
    bfr{&tmptable}.descrip    COLON 35 LABEL "Description" SKIP(.5)
    btnCat                   COLON 20
    bfr{&tmptable}.cat NO-LABEL
    ACat.descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    bfr{&tmptable}.DMethod  COLON 35 LABEL "Depreciation Method" VIEW-AS
                        COMBO-BOX LIST-ITEM-PAIRS "Straight Line Method",1,
                                                  "Reducing Method",2,
                                                  "Units Method",3 SKIP(.5)
    bfr{&tmptable}.Alife      COLON 35 LABEL "Useful life in Months"
    bfr{&tmptable}.DPer                LABEL "Depreciation Percent" Skip(0.5) 
    bfr{&tmptable}.LSuffix[1] COLON 35 LABEL "Asset Ledger Suffix" SKIP(.5)
    bfr{&tmptable}.LSuffix[2] COLON 35 LABEL "Accumulated Depreciation Suffix" SKIP(.5)
    bfr{&tmptable}.LSuffix[3] COLON 35 LABEL "Monthly Depreciation Suffix" SKIP(1.5)
    btn-ok colon 5
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for the main frame **** */
{trilib.i}

ON CHOOSE OF btnCat IN FRAME frm-input
    OR CHOOSE OF btnCat IN FRAME frm-input
DO:
  VIEW FRAME frm-PickCat.
  OPEN QUERY qry-PickCat FOR EACH aCat NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickCat.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickCat 
          OR close of THIS-PROCEDURE IN FRAME frm-PickCat
          OR CHOOSE OF btn-ok IN FRAME frm-PickCat 
          OR 'enter':u OF brw-PickCat
          OR 'mouse-select-dblclick' OF brw-PickCat.
  CLOSE QUERY qry-PickCat.
  HIDE FRAME frm-PickCat.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.Cat.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickCat 
    OR 'enter':u OF brw-PickCat
    OR 'mouse-select-dblclick' OF brw-PickCat
DO: 
   GET CURRENT qry-PickCat NO-LOCK NO-WAIT.
   DISPLAY aCat.Cat @ bfr{&tmptable}.Cat aCat.DESCRIP WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.Cat IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Cat IN FRAME frm-input
DO:
    FIND FIRST aCat WHERE aCat.Cat = DEC(bfr{&tmptable}.Cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE aCat THEN
        DISPLAY aCat.DESCRIP  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Category...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.DMethod IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.DMethod IN FRAME frm-input 
DO:
    IF INT(bfr{&tmptable}.DMethod:SCREEN-VALUE) = 1 THEN DO:
        bfr{&tmptable}.Alife:VISIBLE = TRUE.
        bfr{&tmptable}.Dper:VISIBLE = FALSE.
    END.  
    ELSE IF INT(bfr{&tmptable}.DMethod:SCREEN-VALUE) = 2 THEN DO:
        bfr{&tmptable}.Alife:VISIBLE = FALSE.
        bfr{&tmptable}.Dper:VISIBLE = TRUE.
    END.
    APPLY 'tab' TO bfr{&tmptable}.DMethod.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.subcat  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Sub-category already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.subcat = wsid
                bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.Cat    = INT(bfr{&tmptable}.Cat:SCREEN-VALUE)
                bfr{&tmptable}.Alife    = INT(bfr{&tmptable}.Alife:SCREEN-VALUE)
                bfr{&tmptable}.DMethod  = INT(bfr{&tmptable}.DMethod:SCREEN-VALUE)
                bfr{&tmptable}.DPer     = DEC(bfr{&tmptable}.DPer:SCREEN-VALUE)
                bfr{&tmptable}.LSuffix[1] = DEC(bfr{&tmptable}.LSuffix[1]:SCREEN-VALUE)
                bfr{&tmptable}.LSuffix[2] = DEC(bfr{&tmptable}.LSuffix[2]:SCREEN-VALUE)
                bfr{&tmptable}.LSuffix[3] = DEC(bfr{&tmptable}.LSuffix[3]:SCREEN-VALUE).
                OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} No-lock.
                CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.Cat    = INT(bfr{&tmptable}.Cat:SCREEN-VALUE)
                bfr{&tmptable}.Alife    = INT(bfr{&tmptable}.Alife:SCREEN-VALUE)
                bfr{&tmptable}.DMethod  = INT(bfr{&tmptable}.DMethod:SCREEN-VALUE)
                bfr{&tmptable}.DPer     = DEC(bfr{&tmptable}.DPer:SCREEN-VALUE)
                bfr{&tmptable}.LSuffix[1] = DEC(bfr{&tmptable}.LSuffix[1]:SCREEN-VALUE)
                bfr{&tmptable}.LSuffix[2] = DEC(bfr{&tmptable}.LSuffix[2]:SCREEN-VALUE)
                bfr{&tmptable}.LSuffix[3] = DEC(bfr{&tmptable}.LSuffix[3]:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.subCat.
    FIND FIRST assmf WHERE Assmf.subCat  = bfr{&tmptable}.subCat NO-ERROR.
    IF AVAILABLE assmf THEN DO:
        MESSAGE "Sub-Category has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE assmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} No-lock .
ENABLE ALL  WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.subCat).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.subCat  = wsid NO-ERROR.
    FIND FIRST aCat WHERE aCat.Cat = bfr{&tmptable}.Cat NO-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.Cat aCat.descrip 
        bfr{&tmptable}.DMethod bfr{&tmptable}.Alife bfr{&tmptable}.Dper 
        bfr{&tmptable}.LSuffix[1] bfr{&tmptable}.LSuffix[2] bfr{&tmptable}.LSuffix[3] WITH FRAME frm-input. 
    ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
    IF  bfr{&tmptable}.DMethod = 1 THEN DO:
        bfr{&tmptable}.Alife:VISIBLE = TRUE.
        bfr{&tmptable}.Dper:VISIBLE = FALSE.
    END.
    IF  bfr{&tmptable}.DMethod = 2 THEN DO:
        bfr{&tmptable}.Alife:VISIBLE = FALSE.
        bfr{&tmptable}.Dper:VISIBLE = TRUE.
    END.
   /* ELSE  DO:
        bfr{&tmptable}.Alife:VISIBLE = FALSE.
        bfr{&tmptable}.Dper:VISIBLE = FALSE.
    END. */
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} No-lock.
   HIDE FRAME frm-input.
 END.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   bfr{&tmptable}.DMethod:SCREEN-VALUE = "1". 
   bfr{&tmptable}.Alife:VISIBLE = FALSE.
   bfr{&tmptable}.Dper:VISIBLE = FALSE.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.
