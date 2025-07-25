session:DATA-ENTRY-RETURN = TRUE.
/* Program.................glitem.p
   Notes:...... GL Item code file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Item file already exist"
&SCOPED-DEFINE wsTitle           "Item Code file Maintenance"
&SCOPED-DEFINE tmptable             glitem
&SCOPED-DEFINE skey                 glitem.ITEM
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.item ~
                                        COLUMN-LABEL ' Item Code ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                            WIDTH 60 COLUMN-LABEL ' Description ':C

{varlib.i}
/*DEF BUTTON  btnSearch LABEL "Search". */
browse brw-{&tmptable}:allow-column-searching = true.

define frame frm-input
    {&skey}                   colon 30 label "Item Code"
    bfr{&tmptable}.descrip     colon 30 label "Description"
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".


DEF FRAME sort-options
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.
    
/* ***** Triggers for the main frame **** */
{trilib.i}

/*on 'start-search':u of browse brw-{&tmptable}
    run browse-sorter.ip. */

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.ITEM  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Sub-Vote already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.ITEM = wsid
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
    ASSIGN wsid = bfr{&tmptable}.item.
    FIND FIRST glmf WHERE glmf.item  = bfr{&tmptable}.item 
                      AND glmf.ACCTYPE = "IE" NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        MESSAGE "Item has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE glmf THEN DO:
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
    ASSIGN wsid = int(bfr{&tmptable}.item).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.item  = wsid NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip WITH FRAME frm-input.
    ENABLE {&updFields} btn-OK btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.


procedure Search.ip.
    hCol = browse  brw-{&tmptable}:current-column.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        case trim(hCol:label):
            when "Item Code" then
            do:
               OPEN QUERY qry-{&tmptable} 
                   FOR EACH bfr{&tmptable} NO-LOCK
                            where bfr{&tmptable}.ITEM >= INT(wsSearch:SCREEN-VALUE)
                               BY bfr{&tmptable}.ITEM.

            END.
            when "Description" then
            do:
               OPEN QUERY qry-{&tmptable} 
                   FOR EACH bfr{&tmptable} NO-LOCK
                            where bfr{&tmptable}.descrip >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.descrip.

            END.
            END.
        RETURN.
    END.
