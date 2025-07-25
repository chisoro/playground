session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbwrd.p
   Notes:...... Ward fime maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg             "Ward already exist"
&SCOPED-DEFINE wsTitle           "Ward Code file Maintenance"
&SCOPED-DEFINE tmptable             dbwrd
&SCOPED-DEFINE skey                 dbwrd.ward
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.ward ~
                                        COLUMN-LABEL ' Ward ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

{varlib.i}

define frame frm-input
    {&skey}                   colon 30 label "Ward Code"
    bfr{&tmptable}.descrip     colon 30 label "Description"
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for the main frame **** */
{trilib.i}

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.ward.
    FIND FIRST dbcmf WHERE dbcmf.Ward  = bfr{&tmptable}.ward  NO-ERROR.
    IF AVAILABLE dbcmf THEN DO:
        MESSAGE "ward has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.ward = wsid
                 bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE 
              IN FRAME frm-input.
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
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

{Proclib.i}


PROCEDURE proc-Edit:
   ASSIGN wsid = bfr{&tmptable}.ward.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.ward = wsid EXCLUSIVE-LOCK NO-ERROR.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.

procedure Search.ip.
    hCol = browse brw-{&tmptable}:current-column.
       assign  frame Search-opt:title = hCol:label + " Column".
       IF trim(hCol:label)<> "Description" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
    case trim(hCol:label):
      when "Description" then
         do:
          OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.Descrip >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.Descrip.

            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END.
    RETURN.
END.

