/* Program.................dbmcap.p
   Notes:................. Meter Capture
   Author:.................S. Mawire
   Modified:..............S. Chisoro
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

DEF BUFFER bfdbmmf FOR dbmmf.
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-Acc   LABEL "ACCOUNT".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.

DEF FRAME frm-main
    SKIP(1)
    /*btn-Acc           COLON 10  NO-LABEL */
    bfdbmmf.dbAcc       COLON 26  LABEL "ACCOUNT"
    dbcmf.NAME    VIEW-AS TEXT  NO-LABEL SKIP(0.5)
    bfdbmmf.Serial      COLON 26 LABEL "METER SERIAL NUMBER"  SKIP(0.5)
    bfdbmmf.RDate[12]  COLON 22 LABEL "PREVIOUS DATE" VIEW-AS TEXT
    bfdbmmf.RDate[13]  LABEL "READING DATE"   SKIP(0.5)
    bfdbmmf.READ [12]  COLON 25 LABEL "PREVIOUS READING" VIEW-AS TEXT
    bfdbmmf.READ [13]  LABEL "READING" SKIP(0.5)
    bfdbmmf.cons[13] COLON 47 LABEL "CONSUMPTION" NO-TAB-STOP SKIP(1)
    btn-exit AT ROW 10.5 COL 35 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13.0
    TITLE "METER CAPTURE BY ACCOUNT" VIEW-AS DIALOG-BOX.

ON 'LEAVE':U OF bfdbmmf.dbAcc IN FRAME frm-main 
DO:
    IF dec(bfdbmmf.dbAcc:SCREEN-VALUE) = 0 THEN
      APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
    ELSE DO:
        FIND FIRST bfdbmmf WHERE bfdbmmf.dbAcc = dec(bfdbmmf.dbAcc:SCREEN-VALUE) AND bfdbmmf.mstat =  1 NO-ERROR.
        IF NOT AVAILABLE bfdbmmf THEN DO:
            MESSAGE "No Active Meter for the Account entered" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
        END.
        ELSE DO:
            FIND FIRST dbcmf WHERE dbcmf.dbacc = dec(bfdbmmf.dbAcc:SCREEN-VALUE) NO-ERROR.
            DISPLAY  dbcmf.NAME WITH FRAME frm-main.
            RETURN.
    END.  
END.
END.

ON 'tab':U OF bfdbmmf.Serial IN FRAME frm-main
    OR 'enter':U OF bfdbmmf.Serial IN FRAME frm-main
DO:
    FIND FIRST bfdbmmf WHERE bfdbmmf.dbAcc = dec(bfdbmmf.dbAcc:SCREEN-VALUE) AND bfdbmmf.mstat =  1 AND bfdbmmf.Serial = (bfdbmmf.Serial:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE bfdbmmf THEN DO:
            MESSAGE "The Meter Entered is Either Not Active or Not Available" VIEW-AS ALERT-BOX.
            CLEAR  FRAME frm-main  ALL.
            APPLY 'entry' TO bfdbmmf.dbAcc IN FRAME frm-main.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY bfdbmmf.Serial bfdbmmf.Rdate[13] bfdbmmf.READ[13] bfdbmmf.Rdate[12] bfdbmmf.READ[12]
                     bfdbmmf.cons[13] WITH FRAME frm-main.
            RETURN.
    END.  
END.

ON 'tab':U OF bfdbmmf.READ[13] IN FRAME frm-main
    OR 'enter' OF bfdbmmf.READ[13] IN FRAME frm-main
DO:
    IF DEC(bfdbmmf.READ[13]:SCREEN-VALUE) < bfdbmmf.READ[12] AND DEC(bfdbmmf.READ[13]:SCREEN-VALUE) > 0.00 THEN DO:
        MESSAGE "Your reading of" bfdbmmf.READ[13]:SCREEN-VALUE "implies a meter turn. Is this correct?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE wschoice AS LOGICAL.
        IF wsChoice = NO THEN
            RETURN NO-APPLY.
        ELSE DO:
            wsUpper = 1.
            DO j = 1 TO LENGTH(STRING(INT(bfdbmmf.READ[12]))): /* Set Maxmum Meter Reading */
                wsUpper = wsUpper * 10. 
            END.
            ASSIGN bfdbmmf.Rdate[13] = DATE(bfdbmmf.Rdate[13]:SCREEN-VALUE)
                   bfdbmmf.READ[13]  = DEC(bfdbmmf.READ[13]:SCREEN-VALUE)
                   bfdbmmf.comm[13] = 99 /* Meter turn */
                   bfdbmmf.consum[13] = ( wsUpper - bfdbmmf.READ[12]) + DEC(bfdbmmf.READ[13]:SCREEN-VALUE) .
        END.
    END.
    ELSE
    ASSIGN bfdbmmf.Rdate[13] = DATE(bfdbmmf.Rdate[13]:SCREEN-VALUE)
           bfdbmmf.READ[13]  = DEC(bfdbmmf.READ[13]:SCREEN-VALUE)
           bfdbmmf.comm[13] = 95 /* Meter Read  code 96 is Avg & code 99 is meter turn */
           bfdbmmf.consum[13] = DEC(bfdbmmf.READ[13]:SCREEN-VALUE) - bfdbmmf.READ[12].
    DISPLAY bfdbmmf.consum[13] WITH FRAME frm-Main.
    RELEASE bfdbmmf.
    CLEAR FRAME frm-main ALL.
    APPLY 'entry' TO bfdbmmf.dbAcc IN FRAME frm-main.
END.
/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */

ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
