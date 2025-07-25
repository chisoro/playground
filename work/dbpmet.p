session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbpMet.p
   Notes:......            Payment Method file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsTitle           "Payment Method File Maintenance"
&SCOPED-DEFINE tmptable             dbPay
&SCOPED-DEFINE skey                 dbPay.Paytype
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C~
                              bfr{&tmptable}.bank ~
                                        COLUMN-LABEL ' Bank ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Paytype ~
                                        COLUMN-LABEL ' Payment Method':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.bank ~
                                        COLUMN-LABEL ' Bank ':C
                              

{varlib.i}

define frame frm-input
    {&skey}                   COLON 30 label "Payment Method"
    bfr{&tmptable}.descrip    COLON 30 label "Description"
    bfr{&tmptable}.bank       COLON 30 LABEL "Bank"
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
DEFINE FRAME frm-edit
    {&skey}                    COLON 30 LABEL "Paytype"
    bfr{&tmptable}.descrip     COLON 30 LABEL "Description"
    bfr{&tmptable}.bank        COLON 30 LABEL "Bank"
    SKIP(0.5)
    btn-upd COLON 5
    btn-cancel COLON 60
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
    TITLE "DATA UPDATE".


{Trilib.i}

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Paytype.
    FIND FIRST dbRec WHERE dbRec.Paytype  = bfr{&tmptable}.Paytype  NO-ERROR.
    IF AVAILABLE Dbrec THEN DO:
        MESSAGE "Payment Method has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
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

{Proclib.i}

    PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Paytype.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Paytype  = wsid NO-ERROR.
    DISPLAY wsid @ {&skey} {&UpdFields} WITH FRAME frm-edit.
    ENABLE {&updFields} btn-upd btn-cancel WITH FRAME frm-edit.
    WAIT-FOR CHOOSE OF btn-cancel 
          OR CHOOSE OF btn-upd OR close of THIS-PROCEDURE IN FRAME frm-edit.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-edit.
 END.

PROCEDURE Validate-ip:
IF wsid = 0 THEN DO:
    MESSAGE "No value entered" VIEW-AS ALERT-BOX.
    CLEAR FRAME frm-input ALL.
    APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
END.
ELSE DO:
        FIND FIRST 
            {&tmptable} WHERE {&tmptable}.Paytype  = wsid NO-ERROR.
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  "Payment Method already exist" VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          VIEW FRAME frm-input.
          RELEASE {&tmptable}.
          APPLY 'tab' TO {&skey}.
       END.
    END.
END.

PROCEDURE Upd.ip:
    IF wsbtn = "New" THEN
    DO:
     FIND FIRST {&tmptable} WHERE {&tmptable}.Paytype  = wsid NO-ERROR.
      IF AVAILABLE {&tmptable} THEN DO:
          MESSAGE  "Payment Method already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.Paytype = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.bank = INT(bfr{&tmptable}.bank:SCREEN-VALUE).
      END. 
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-edit
               bfr{&tmptable}.bank = INT(bfr{&tmptable}.bank:SCREEN-VALUE).
        RELEASE {&tmptable}.
    END.      
END.
