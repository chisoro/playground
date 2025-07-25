/* Program.................dbqcap.p
   Notes:................. Quary Quantity Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsValid     AS LOGICAL INITIAL YES.
DEF  VAR wsStatus   AS CHAR FORM "X(20)".
DEF VAR wsVar       AS CHAR FORM "X(20)".
DEF VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF  VAR wsUpper AS INT.
DEF VAR X         AS INT.
DEF VAR j         AS INT.

DEF BUFFER bfdbqty FOR dbqty.
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-Acc   LABEL "ACCOUNT".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 8.5.

DEF FRAME frm-main
    SKIP(2)
    /*btn-Acc           COLON 10  NO-LABEL */
    bfdbqty.dbAcc       COLON 22  LABEL "ACCOUNT"
    dbcmf.NAME    VIEW-AS TEXT  NO-LABEL SKIP(0.5)
    bfdbqty.qDate[13]  COLON 22 LABEL "READING DATE"   SKIP(0.5)
    bfdbqty.qty [13]  COLON 20 LABEL "QUANTITY"        SKIP(2)
    btn-exit AT ROW 10.5 COL 35 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 75 BY 13.0
    TITLE "QUANTITY CAPTURE BY ACCOUNT" VIEW-AS DIALOG-BOX.

ON 'LEAVE':U OF bfdbqty.dbAcc IN FRAME frm-main 
DO:
    IF INT(bfdbqty.dbAcc:SCREEN-VALUE) = 0 THEN
      APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
    ELSE DO:
        FIND FIRST bfdbqty WHERE bfdbqty.dbAcc = INT(bfdbqty.dbAcc:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE bfdbqty THEN DO:
            MESSAGE "No Account created" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
        END.
        ELSE DO:
            FIND FIRST dbcmf WHERE dbcmf.dbacc = INT(bfdbqty.dbAcc:SCREEN-VALUE) NO-ERROR.
            DISPLAY bfdbqty.qDate[13] bfdbqty.qty[13]
                    dbcmf.NAME WITH FRAME frm-main.
            RETURN.
    END.  
END.
END.

ON 'tab':U OF bfdbqty.qty[13] IN FRAME frm-main
    OR 'enter' OF bfdbqty.qty[13] IN FRAME frm-main
DO:
    ASSIGN bfdbqty.qDate[13] = DATE(bfdbqty.qDate[13]:SCREEN-VALUE)
           bfdbqty.qty[13]  = DEC(bfdbqty.qty[13]:SCREEN-VALUE).
    RELEASE bfdbqty.
    CLEAR FRAME frm-main ALL.
    APPLY 'entry' TO bfdbqty.dbAcc IN FRAME frm-main.
    RETURN NO-APPLY.
END.
/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */

ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
