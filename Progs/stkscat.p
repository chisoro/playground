/* Program.................stkscat.p
   Notes:................. Sub-category  File Maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg           "Sub-Category  file already exist"
&SCOPED-DEFINE wsTitle         "Sub-Category File Maintenance"
&SCOPED-DEFINE tmptable        stkscat
&SCOPED-DEFINE skey            stkscat.sub-cat
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.sub-cat ~
                                        COLUMN-LABEL ' SUB-CATEGORY ':C ~
                              bfr{&tmptable}.cat ~
                                        COLUMN-LABEL ' CATEGORY ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        WIDTH 55 COLUMN-LABEL ' DESCRIPTION ':C
                                         

{varlib.i}

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.
     
DEF    QUERY qry-cat FOR stkcat SCROLLING.
DEF BROWSE brw-cat QUERY qry-cat
    DISPLAY stkcat.CAT stkcat.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.
    
DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 8
    btn-add AT ROW 20.7 COL 10
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 100 BY 23.2
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    SKIP(1)
    btnCat                colon 10 bfr{&tmptable}.cat NO-LABEL
    stkcat.descrip        NO-LABEL NO-TAB-STOP skip(1)
    {&skey}                colon 25 label "SUB-CATEGORY"  VIEW-AS TEXT skip(0.5)
    bfr{&tmptable}.descrip   colon 25 label "DESCRIPTION" skip(.5) 
    btn-ok colon 5
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

DEFINE FRAME frm-pick 
    brw-cat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

/* ***** Triggers for the main frame **** */

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
         wsbtn = "Add".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw-{&tmptable}
        OR 'mouse-select-dblclick' OF brw-{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        wsbtn = "EDIT".
        GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        DISABLE {&skey} WITH FRAME frm-input.
        run proc-edit.
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
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-cat
    OR 'mouse-select-dblclick' OF brw-cat
DO: 
   GET CURRENT qry-cat EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY stkcat.cat @ bfr{&tmptable}.cat stkcat.DESCRIP WITH FRAME frm-input.
    APPLY 'tab' TO SELF IN FRAME frm-input.
    APPLY 'tab' TO bfr{&tmptable}.cat IN FRAME frm-input.
    APPLY 'ENTRY' TO bfr{&tmptable}.descrip IN FRAME frm-input.
END.

ON 'enter':U OF bfr{&tmptable}.cat IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.cat IN FRAME frm-input
DO: 
  IF DEC(bfr{&tmptable}.cat:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
  END.
  ELSE DO:
      FIND FIRST stkcat WHERE stkcat.cat = DEC(bfr{&tmptable}.cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE stkcat THEN DO:
         MESSAGE "NO such category exist, please try again" VIEW-AS ALERT-BOX.
         CLEAR FRAME frm-input ALL.
         RETURN NO-APPLY.
      END.
      ELSE DO:
          DISPLAY stkcat.cat @ bfr{&tmptable}.cat stkcat.DESCRIP WITH FRAME frm-input.
          FIND LAST stkscat WHERE stkscat.cat = DEC(bfr{&tmptable}.cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
          IF AVAILABLE stkscat THEN
             wsid = stkscat.sub-cat + 0.0001.
          ELSE IF NOT AVAILABLE stkscat THEN
             wsid = stkcat.cat +  0.0001. 
          {&skey}:SCREEN-VALUE = STRING(wsid).    
      END.
  END.
END.

/*ON 'enter':U OF {&skey} IN FRAME frm-input
    OR 'tab':U OF {&skey} IN FRAME frm-input
DO: 
     wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
   IF wsid = 0 THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          RETURN NO-APPLY.
       END.
    END.
END. */

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.sub-cat  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Sub-Category already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.sub-cat = wsid
                   bfr{&tmptable}.cat = DEC(bfr{&tmptable}.cat:SCREEN-VALUE)
                   bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.Devicename = lc-host
                   bfr{&tmptable}.UID = varuser
                   bfr{&tmptable}.creDate = TODAY.
            brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
            
            APPLY 'entry' TO bfr{&tmptable}.cat IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
     CLEAR FRAME frm-input ALL.
END.

ON CHOOSE OF btn-Del DO:
    wsbtn = "DELETE".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.sub-cat.
    FIND FIRST stkmf WHERE stkmf.sub-cat  = bfr{&tmptable}.sub-cat NO-ERROR.
    IF AVAILABLE stkmf THEN DO:
        MESSAGE "Sub-category has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE stkmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

{audit.i}
/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = DEC(bfr{&tmptable}.sub-cat).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.sub-cat  = wsid NO-ERROR.
    FIND FIRST stkcat WHERE stkcat.cat  = bfr{&tmptable}.cat NO-LOCK NO-ERROR.
    DISPLAY bfr{&tmptable}.cat wsid @ {&skey} bfr{&tmptable}.Descrip stkcat.Descrip WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close IN FRAME frm-input 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

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
