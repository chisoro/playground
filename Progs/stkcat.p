session:DATA-ENTRY-RETURN = TRUE.
/* Program.................stkcat.p
   Notes:...... Stock category file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Category file already exist"
&SCOPED-DEFINE wsTitle           "Category file Maintenance"
&SCOPED-DEFINE tmptable             stkcat
&SCOPED-DEFINE skey                 stkcat.CAT
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.cat ~
                                        COLUMN-LABEL ' Category ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

{varlib.i}

define frame frm-input
    {&skey}                   colon 30 label "Category"
    bfr{&tmptable}.descrip     colon 30 label "Description"
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for the main frame **** */
{trilib.i}

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.cat  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Category already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.cat = wsid
                bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
            brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.cat.
    FIND FIRST stkmf WHERE stkmf.cat  = bfr{&tmptable}.cat NO-ERROR.
    IF AVAILABLE stkmf THEN DO:
        MESSAGE "Category has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE stkmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.cat).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.cat  = wsid NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
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
