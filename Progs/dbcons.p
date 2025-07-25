session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbctf.p
   Notes:...... cons fime maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg             "Consumer already exist"
&SCOPED-DEFINE wsTitle           "Consumer file Maintenance"
&SCOPED-DEFINE tmptable             dbctf
&SCOPED-DEFINE skey                 dbctf.cons
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.cons ~
                                        COLUMN-LABEL ' Consumer ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

/*{varlib.i} */
DEF VAR hCol        AS WIDGET-HANDLE.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR wsid LIKE {&skey}.
DEF VAR wsbtn AS CHAR.
DEF VAR varLedger LIKE glmf.acct.
DEF VAR wsvalid AS LOGICAL INITIAL YES.
DEF VAR wsSearch   LIKE dbcmf.NAME.



DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-upd    LABEL "UPDATE".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btn-Int    LABEL "ADD INTEREST".

DEF BUFFER bfr{&tmptable} FOR {&tmptable} PRESELECT.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 18.5.

DEF FRAME Search-opt
     wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.
         
DEF QUERY qry-{&tmptable} FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-{&tmptable} QUERY qry-{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF    QUERY qry-dbInt FOR dbInt SCROLLING.
DEF BROWSE brw-dbInt QUERY qry-dbInt
    DISPLAY dbInt.Sgrp LABEL "SERVICE" dbInt.decInt LABEL "INTEREST"
     ENABLE dbInt.Sgrp dbInt.decInt WITH 20 DOWN SEPARATORS NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

/*DEFINE FRAME frm-dbIn 
    brw-dbInt AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Service Group Selection". */
         
DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 8
    btn-add AT ROW 20.7 COL 10
    Space(14) btn-edit
    space(14) btn-del
    space(14) btn-exit SKIP(1)
     brw-dbInt AT ROW 2 COL 62 SKIP(0.2)
     btn-Int COLON 60
    rect-2 AT ROW 1.4 COL 6
    rect-1 AT ROW 20 COL 6
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 90 BY 23.5
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.
/* end of Common Variables */

browse brw-{&tmptable}:allow-column-searching = true.

define frame frm-input
    {&skey}                   colon 30 label "Consumer"
    bfr{&tmptable}.descrip    colon 30 label "Description"
    /*bfr{&tmptable}.interest   COLON 30 LABEL "Interest" */
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
/* ***** Triggers for the main frame **** */
{trilib.i}

ON 'mouse-select-click' OF brw-{&tmptable}
    DO:
        GET CURRENT qry-{&tmptable}.
         ASSIGN wsid = bfr{&tmptable}.cons.
        OPEN QUERY qry-dbInt FOR EACH dbInt WHERE dbInt.cons =  bfr{&tmptable}.cons  
        NO-LOCK BY dbInt.sgrp.
        RETURN.
END.

ON 'mouse-select-dblclick' OF brw-dbInt
    DO:
        GET CURRENT qry-dbInt EXCLUSIVE-LOCK NO-WAIT.
        current-record = ROWID(dbInt).
        RETURN.
END.

ON CHOOSE OF btn-Int DO:
   method-status = brw-dbInt:INSERT-ROW("AFTER").
END.

ON "enter" OF dbInt.sgrp IN BROWSE brw-dbInt
OR "TAB" OF dbInt.sgrp IN BROWSE brw-dbInt DO:
   FIND FIRST dbsgr WHERE dbsgr.sgrp = INT(dbInt.sgrp:SCREEN-VALUE IN BROWSE brw-dbInt) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbsgr THEN DO:
       MESSAGE "Invalid Service..." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
END.

ON ROW-LEAVE OF BROWSE brw-dbInt DO:
    IF NOT CAN-FIND(FIRST dbsgr WHERE dbsgr.sgrp = INT(dbInt.sgrp:SCREEN-VALUE IN BROWSE brw-dbInt))
     THEN DO:
       MESSAGE "Invalid Service..." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    IF brw-dbInt:NEW-ROW IN FRAME frm-main THEN DO:
        DO ON ERROR UNDO, RETURN NO-APPLY:
            CREATE dbInt.
            ASSIGN dbInt.cons = wsId.
            method-status = brw-dbInt:CREATE-RESULT-LIST-ENTRY().
        END.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.cons.
    FIND FIRST dbcmf WHERE dbcmf.cons  = bfr{&tmptable}.cons  NO-ERROR.
    IF AVAILABLE dbcmf THEN DO:
        MESSAGE "Consumer Type has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
     FOR EACH dbint WHERE dbint.cons = bfr{&tmptable}.cons:
        DELETE dbint.
     END.
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.cons = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               /*bfr{&tmptable}.interest = INT(bfr{&tmptable}.interest:SCREEN-VALUE) */.
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               /*bfr{&tmptable}.interest = INT(bfr{&tmptable}.interest:SCREEN-VALUE)*/.
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
   ASSIGN wsid = bfr{&tmptable}.cons.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.cons = wsid EXCLUSIVE-LOCK NO-ERROR.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.

