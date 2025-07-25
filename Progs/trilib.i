
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

ON 'enter':U OF {&skey} IN FRAME frm-input
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
END.

ON 'start-search':u OF BROWSE brw-{&tmptable}
    run SEARCH.ip.





