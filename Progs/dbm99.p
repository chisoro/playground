/* Program.................dbm99.p
   Notes:................. Delete Meter Capture
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
DEF VAR wsAns AS LOGICAL.

DEF BUFFER bfdbmmf FOR dbmmf.

DEF BUTTON btn-OK  LABEL "UPDATE".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-Acc   LABEL "ACCOUNT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 8.5.

DEF FRAME frm-main
    SKIP(1)
    /*btn-Acc           COLON 10  NO-LABEL */
    bfdbmmf.dbAcc       COLON 22  LABEL "ACCOUNT"
    dbcmf.NAME    VIEW-AS TEXT  NO-LABEL SKIP(0.5)
    bfdbmmf.Meter      COLON 22 LABEL "METER NUMBER"   NO-TAB-STOP 
                  FORM ">>>>>>>>>>>>9" SKIP(0.5)
    bfdbmmf.RDate[12]  COLON 22 LABEL "PREVIOUS DATE" VIEW-AS TEXT
    bfdbmmf.RDate[13]  LABEL "READING DATE" NO-TAB-STOP  SKIP(0.5)
    bfdbmmf.READ [12]  COLON 25 LABEL "PREVIOUS READING" VIEW-AS TEXT
    bfdbmmf.READ [13]  LABEL "READING" NO-TAB-STOP SKIP(0.5)
    bfdbmmf.mStats       COLON 30 LABEL "CHANGE METER STATUS"  SKIP(1)
    btn-ok AT ROW 10.5 COL 10 NO-TAB-STOP
    btn-exit AT ROW 10.5 COL 40 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 75 BY 13.0
    TITLE "METER CAPTURE BY ACCOUNT" VIEW-AS DIALOG-BOX.

ON 'LEAVE':U OF bfdbmmf.dbAcc IN FRAME frm-main 
DO:
    IF INT(bfdbmmf.dbAcc:SCREEN-VALUE) = 0 THEN
      APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
    ELSE DO:
        FIND FIRST bfdbmmf WHERE bfdbmmf.dbAcc = INT(bfdbmmf.dbAcc:SCREEN-VALUE) AND bfdbmmf.mstat <> 3 NO-ERROR.
        IF NOT AVAILABLE bfdbmmf THEN DO:
            MESSAGE "No Meter for the Account entered" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
        END.
        ELSE DO:
            FIND FIRST dbcmf WHERE dbcmf.dbacc = INT(bfdbmmf.dbAcc:SCREEN-VALUE) NO-ERROR.
            DISPLAY bfdbmmf.Meter bfdbmmf.Rdate[13] bfdbmmf.READ[13] bfdbmmf.Rdate[12] bfdbmmf.READ[12]
                    bfdbmmf.mstat dbcmf.NAME WITH FRAME frm-main.
            RETURN.
        END.
    END.
END.

ON 'CHOOSE':U OF btn-ok IN FRAME frm-Main 
DO:
    IF INT(bfdbmmf.mStats:SCREEN-VALUE) = 3 THEN DO:
       ASSIGN bfdbmmf.mStats
              bfdbmmf.Tarif  = 0
              bfdbmmf.wNextP = ?.
       MESSAGE "Meter has been Deleted" VIEW-AS ALERT-BOX.
   END.
   ELSE IF INT(bfdbmmf.mStats:SCREEN-VALUE) = 2 THEN DO:
       ASSIGN bfdbmmf.mStats.     
       MESSAGE "Meter has been marked Non-functional" VIEW-AS ALERT-BOX.
   END.
   ELSE IF INT(bfdbmmf.mStats:SCREEN-VALUE) = 2 AND bfdbmmf.mStats = 1 THEN DO:     
       MESSAGE "Meter has been deleted... cannot be restored" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
    APPLY 'entry' TO bfdbmmf.dbAcc IN FRAME frm-main.
    RELEASE bfdbmmf.
    CLEAR FRAME frm-main ALL.
    RETURN.
END.
/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */

ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
