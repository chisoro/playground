session:DATA-ENTRY-RETURN = TRUE.
/* Program.................cbTType.p
   Notes:...... Cashbook transaction type fime maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Transaction Type already exist"
&SCOPED-DEFINE wsTitle           "Transaction Type File Maintenance"
&SCOPED-DEFINE tmptable             cbtype
&SCOPED-DEFINE skey                 cbtype.TranType
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DrCr ~
                                        COLUMN-LABEL ' Dr/Cr ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.TranType ~
                                        COLUMN-LABEL ' Type ':C ~
                              bfr{&tmptable}.DrCr ~
                                        COLUMN-LABEL ' Dr/Cr ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C WIDTH 40

{varlib.i}
define frame frm-input
    {&skey}                   COLON 30 LABEL "Type"
    bfr{&tmptable}.descrip    COLON 30 LABEL "Description"
     bfr{&tmptable}.DrCr      COLON 30 LABEL "DEBIT/CREDIT" VIEW-AS COMBO-BOX
        SIZE 10 BY 2 LIST-ITEM-PAIRS "DEBIT","D","CREDIT","C"
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    

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
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.TranType.
    FIND FIRST cbTrans WHERE cbTrans.TranType  = bfr{&tmptable}.TranType  NO-ERROR.
    IF AVAILABLE cbTrans THEN DO:
        MESSAGE "Service has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE cbTrans THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         FIND FIRST cbtype WHERE cbtype.TranType  = wsid NO-ERROR.
         IF AVAILABLE cbtype THEN DO:
             MESSAGE  "Transaction Type already exist" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         ELSE DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.TranType = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input 
               bfr{&tmptable}.DrCr    = bfr{&tmptable}.DrCr:SCREEN-VALUE IN FRAME frm-input.
            brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
         END.
    END.
    ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.DrCr    = bfr{&tmptable}.DrCr:SCREEN-VALUE IN FRAME frm-input.
        RELEASE {&tmptable}.
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

PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable}   NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.TranType).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.TranType  = wsid NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.DrCr WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

