session:DATA-ENTRY-RETURN = TRUE.

DEF VAR wsid LIKE glrpt.Repno.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR choice AS LOGICAL.

DEFINE QUERY qry-glrpt FOR glrpt scrolling.
DEF BROWSE brw-glrpt QUERY qry-glrpt 
    DISPLAY Repno  COLUMN-LABEL "REPORT" Descrip COLUMN-LABEL "DESCRIPTION" 
    WIDTH 65 WITH 20 DOWN SEPARATORS.

DEF BUTTON btn-Add     LABEL "ADD".
DEF BUTTON btn-del     LABEL "DELETE".
DEF BUTTON btn-exit    LABEL "CLOSE".
DEF BUTTON btn-cancel  LABEL "CANCEL".
DEF BUTTON btn-update  LABEL "UPDATE".
DEF BUTTON btn-Edit    LABEL "EDIT".
DEF BUTTON btn-close   LABEL "CLOSE".
DEF BUTTON btn-ok      LABEL "OK".
DEF BUTTON btn-layout  LABEL "REPORT LAYOUT"
    TRIGGERS:
       ON CHOOSE
       DO:
         Message "RUN glrpt02.p." VIEW-AS ALERT-BOX.
       END.
   END TRIGGERS.
DEF BUTTON btn-Acc     LABEL "REPORT ACCOUNTS"
    TRIGGERS:
       ON CHOOSE
       DO:
         Message "RUN glrpt03.p." VIEW-AS ALERT-BOX.
       END.
   END TRIGGERS.

define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 105 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 105 by 18.5.

define frame frm-input
    glrpt.Repno        colon 30 label "Repno Repno"
    glrpt.descrip     colon 30 label "Description"
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE FGCOLOR 4 "DATA CAPTURE".
    
 define frame frm-Edit
    glrpt.Repno        colon 30 label "Repno Repno"
    glrpt.descrip     colon 30 label "Description"
    skip(0.5)
    btn-update colon 5
    btn-Cancel colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "Report Maintenance Update".

DEF FRAME frm-glrpt
    brw-glrpt AT ROW 2 COL 5
    btn-layout AT ROW 4 COL 85
    btn-Acc    AT ROW 6 COL 85
    btn-add    AT ROW 20.7 COL 5
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 120 BY 24
    TITLE FGCOLOR 3 "Report Maintenance" view-as dialog-box.

/* ***** Triggers  **** */
ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF INT(glrpt.Repno:SCREEN-VALUE) <> 0 THEN
     CREATE glrpt.
     ASSIGN glrpt.Repno = INT(glrpt.Repno:SCREEN-VALUE)
           glrpt.Descrip = glrpt.Descrip:SCREEN-VALUE. 
     CLEAR FRAME frm-input ALL.
     APPLY 'entry' TO glrpt.Repno.
    RETURN.
END.

ON LEAVE OF glrpt.Repno IN FRAME frm-input
DO:
    IF int(glrpt.Repno:SCREEN-VALUE) = 0 THEN
        APPLY 'Close' TO THIS-PROCEDURE.
    ELSE DO:
        FIND FIRST glrpt WHERE glrpt.Repno = int(glrpt.Repno:SCREEN-VALUE) NO-ERROR.
        IF AVAILABLE glrpt THEN DO:
            MESSAGE "Report code already exist" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        RETURN.
    END.   
END.


 ON CHOOSE OF btn-Add IN FRAME frm-glrpt DO:
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-update IN FRAME frm-edit
DO:
    glrpt.Descrip = glrpt.Descrip:SCREEN-VALUE.
    OPEN QUERY qry-glrpt FOR EACH glrpt NO-LOCK BY glrpt.Repno.
    HIDE FRAME frm-edit.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-glrpt 
    OR 'enter':u OF brw-glrpt
    OR 'mouse-select-dblclick' OF brw-glrpt
DO: 
   RUN proc-edit.
   RETURN.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-glrpt EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = glrpt.Repno.
    IF wsid >= 4 THEN DO:
       MESSAGE "Do you really want to delete this Report?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
               TITLE "" UPDATE choice AS LOGICAL.
        CASE choice:
            WHEN TRUE THEN /* Yes */
            DO:
                FOR EACH glrptAc WHERE glrptAc.Repno = wsid:
                    DELETE glrptAc.
                END.
                FOR EACH glrptl WHERE glrptl.Repno = wsid:
                    DELETE glrptl.
                END.
                DELETE glrpt.
                Method-Status = brw-glrpt:DELETE-SELECTED-ROWS().
            END.
            WHEN FALSE THEN /* No */
            DO:
                MESSAGE "Deletion canceled."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RETURN NO-APPLY.
            END.
        END CASE.  
    END.
    ELSE MESSAGE "Reports 1 to 4 Cannot be delete" VIEW-AS ALERT-BOX.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-glrpt FOR EACH glrpt  NO-LOCK BY glrpt.Repno.
ENABLE ALL WITH FRAME Frm-glrpt.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-glrpt.
CLOSE QUERY qry-glrpt.
HIDE FRAME frm-glrpt.

PROCEDURE proc-input:
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
 OPEN QUERY qry-glrpt FOR EACH glrpt  NO-LOCK BY glrpt.Repno.
 HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
   GET CURRENT qry-glrpt EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glrpt.Repno glrpt.Descrip WITH FRAME frm-edit.
   ENABLE ALL EXCEPT glrpt.Repno WITH FRAME frm-edit.
   WAIT-FOR CHOOSE OF btn-update IN FRAME frm-edit 
          OR close of THIS-PROCEDURE IN FRAME frm-edit
          OR CHOOSE OF btn-cancel IN FRAME frm-edit.
   OPEN QUERY qry-glrpt FOR EACH glrpt NO-LOCK BY glrpt.Repno.
   HIDE FRAME frm-edit.
END.


