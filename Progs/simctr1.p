/* Program.................simctr1.p
   Notes:.................Email Parameter File Maintenance
   Author:.................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.

DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF VAR cond AS LOGICAL INITIAL NO.

define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 116 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 116 by 6.4.



define frame frm-input
    SKIP(0.5)
    simctr1.serveradd   colon 30 label "SMTP Server"
    simctr1.serveremail   COLON 30 LABEL "Sending Email"SKIP(0.2)
    SIMCTR1.serverpass    colon 30 LABEL "Sending Email Password"   PASSWORD-FIELD SPACE(5)
    SIMCTR1.serverport              LABEL "SMTP Port Number" SKIP(0.2)
    SIMCTR1.email     COLON 30 LABEL "Reply-to Email"
   
   
    rect-2 AT ROW 1 COL 2
    rect-1 AT ROW 7.5 COL 2
    btn-ok AT ROW 8.3 COL  20
    btn-close colon 80 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "EMAIL PARAMETER FILE MAINTANANCE".
    

/* *** Triggers to input frame frm-input*** */

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
IF cond = YES THEN DO:
    ASSIGN simctr1. 
    RELEASE simctr1.
END.
ELSE IF cond = NO THEN DO:
    CREATE simctr1.
    ASSIGN
            simctr1.serveradd = simctr1.serveradd:SCREEN-VALUE IN FRAME frm-input
            simctr1.email = simctr1.email:SCREEN-VALUE IN FRAME frm-input 
            simctr1.serveremail  = simctr1.serveremail :SCREEN-VALUE IN FRAME frm-input
            simctr1.serverpass = simctr1.serverpass:SCREEN-VALUE IN FRAME frm-input
            simctr1.serverport = int(simctr1.serverport:SCREEN-VALUE IN FRAME frm-input).
 
END.
    
      APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
     APPLY 'quit' TO FRAME frm-input.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR1 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF AVAILABLE SIMCTR1 THEN DO:
    DISPLAY SIMCTR1 WITH FRAME FRM-INPUT.
    cond = YES.
END.
IF NOT AVAILABLE SIMCTR1 THEN
DO:
    MESSAGE "There is no record currently available please create."  VIEW-AS ALERT-BOX.
  END.
VIEW FRAME frm-input.
ENABLE ALL WITH FRAME frm-input.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.
RETURN.

