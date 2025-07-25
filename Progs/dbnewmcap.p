/* Program.................dbnewmcap.p
   Notes:................. New Meter Capture
   Author:.................S. Chisoro
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
 DEF VAR wsOpt AS CHAR INITIAL "Create".
 DEF VAR wsDescrip AS CHAR FORM "X(20)".
DEF VAR X         AS INT.
DEF VAR j         AS INT.

DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-tar.
DEF BUTTON btn-Acc   LABEL "ACCOUNT".
DEF BUTTON btn-Add  LABEL "ADD".
DEF BUTTON btn-ok LABEL "OK".
DEF BUTTON btn-close LABEL "CLOSE".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.

DEFINE QUERY qry-picktarif FOR dbtmf scrolling.
DEF BROWSE brw-picktarif QUERY qry-picktarif
    DISPLAY  dbtmf.type dbtmf.Tarif dbtmf.Descrip dbtmf.Charge
    WITH 8 DOWN SEPARATORS.

DEFINE FRAME frm-picktarif 
    brw-picktarif AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "Ok"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Selection".

DEF FRAME frm-main
    SKIP(1)
    /*btn-Acc           COLON 10  NO-LABEL */
    dbmmf.dbAcc       COLON 26  LABEL "ACCOUNT"
    dbcmf.NAME    VIEW-AS TEXT  NO-LABEL SKIP(0.5)
    dbmmf.Serial      COLON 26 LABEL "METER SERIAL NUMBER"  SPACE (5)
    dbmmf.meter LABEL "METER NUMBER"SKIP(0.5)
    dbmmf.READ [13] COLON 26 LABEL "READING" SPACE (10)
    dbmmf.RDate[13]  LABEL "READING DATE"   SKIP(0.5)
    dbmmf.wNextP COLON 26 LABEL "NEXT BILLING PERIOD" NO-TAB-STOP SKIP(1)
    btn-Tar          COLON 26 LABEL "Water Tariff" SPACE (5)
    dbmmf.tarif               NO-LABEL VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(1)
    btn-Add AT ROW 10.5 COL 15  NO-TAB-STOP
    btn-exit AT ROW 10.5 COL 60 NO-TAB-STOP
    
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13.0
    TITLE "Add New Meter" VIEW-AS DIALOG-BOX.

ON 'enter':U OF dbmmf.dbAcc IN FRAME frm-main 
    OR  'tab':U OF dbmmf.dbAcc IN FRAME frm-main 
DO:
    IF dec(dbmmf.dbAcc:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Account cannot be blank" VIEW-AS ALERT-BOX.
            CLEAR  FRAME frm-main  ALL.
            APPLY 'entry' TO dbmmf.dbAcc IN FRAME frm-main.
            RETURN NO-APPLY.
     END.
     ELSE DO:
        FIND FIRST dbcmf WHERE dbcmf.dbacc = dec(dbmmf.dbAcc:SCREEN-VALUE) NO-ERROR.
         IF NOT AVAILABLE dbcmf THEN DO:
            MESSAGE "Account Number Does  Exist." VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY  dbcmf.NAME WITH FRAME frm-main.
            APPLY 'entry' TO dbmmf.Serial IN FRAME frm-main.
            RETURN.
        END.
         
    END.
END.

ON 'tab':U OF dbmmf.Serial IN FRAME frm-main
    OR 'enter':U OF dbmmf.Serial IN FRAME frm-main
DO:
    FIND FIRST dbmmf WHERE dbmmf.dbAcc = dec(dbmmf.dbAcc:SCREEN-VALUE) AND dbmmf.Serial = (dbmmf.Serial:SCREEN-VALUE) NO-ERROR.
        IF AVAILABLE dbmmf THEN DO:
            IF dbmmf.mstat <> 1 THEN DO:
                    MESSAGE "This meter is already assigned to this account. Do you want to activate it?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO-CANCEL
                        UPDATE lChoice AS LOGICAL.
                    /*act on response*/
                    CASE lChoice:
                        WHEN TRUE THEN  DO:
                          FIND dbmmf WHERE dbmmf.dbAcc = dec(dbmmf.dbAcc:SCREEN-VALUE) AND dbmmf.Serial = (dbmmf.Serial:SCREEN-VALUE)
                            EXCLUSIVE-LOCK.
                          ASSIGN wsOpt = "Update".
                          FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif NO-LOCK.
                          DISPLAY dbtmf.tarif @ dbmmf.tarif dbtmf.descrip @ wsDescrip WITH FRAME frm-main.
                          btn-Add:LABEL IN FRAME frm-main = "UPDATE".
                          DISPLAY dbmmf.meter WITH FRAME frm-main.
                           APPLY 'entry' TO dbmmf.READ[13] IN FRAME frm-main.
                            RETURN NO-APPLY.
                        END.
                        WHEN FALSE THEN /* No */ DO:
                            MESSAGE "Meter not updated" VIEW-AS ALERT-BOX.
                            CLEAR  FRAME frm-main  ALL.
                           APPLY 'entry' TO dbmmf.dbAcc IN FRAME frm-main.
                            RETURN NO-APPLY.
                        END.
                        OTHERWISE /* Cancel */ STOP.
                    END CASE.
            END.
            ELSE DO:
                MESSAGE "This meter is already assigned to this account." VIEW-AS ALERT-BOX.
                    CLEAR  FRAME frm-main  ALL.
                    APPLY 'entry' TO dbmmf.dbAcc IN FRAME frm-main.
                    RETURN NO-APPLY.
            END.
            
        END.
        ELSE DO:
             ASSIGN wsOpt = "Create".
           APPLY 'entry' TO dbmmf.meter IN FRAME frm-main.
            RETURN.
    END.  
END.

ON 'tab':U OF dbmmf.meter IN FRAME frm-main
    OR 'enter' OF dbmmf.meter IN FRAME frm-main
DO:
    APPLY 'entry' TO dbmmf.READ[13] IN FRAME frm-main.
END.

ON 'tab':U OF dbmmf.READ[13] IN FRAME frm-main
    OR 'enter' OF dbmmf.READ[13] IN FRAME frm-main
DO:
   
    APPLY 'entry' TO dbmmf.rdate[13] IN FRAME frm-main.
END.

ON 'tab':U OF dbmmf.Rdate[13] IN FRAME frm-main
    OR 'enter' OF dbmmf.Rdate[13] IN FRAME frm-main
DO:
   
    APPLY 'entry' TO dbmmf.wNextP IN FRAME frm-main.
END.

ON 'tab':U OF dbmmf.wNextP IN FRAME frm-main
    OR 'enter' OF dbmmf.wNextP IN FRAME frm-main
DO:
   APPLY 'entry' TO btn-tar IN FRAME frm-main.
END.

ON CHOOSE OF btn-Tar IN FRAME frm-main /* Water Tarif */
DO:
    VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 1 NO-LOCK.
  ENABLE ALL WITH FRAME frm-picktarif.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picktarif 
          OR close of THIS-PROCEDURE IN FRAME frm-picktarif
          OR CHOOSE OF btn-ok IN FRAME frm-picktarif 
          OR 'enter':u OF brw-picktarif
          OR 'mouse-select-dblclick' OF brw-picktarif.
  CLOSE QUERY qry-picktarif.
  HIDE FRAME frm-picktarif.
  APPLY 'tab' TO SELF.
  RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-picktarif 
    OR 'enter':u OF brw-picktarif
    OR 'mouse-select-dblclick' OF brw-picktarif
DO: 
   GET CURRENT qry-picktarif EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbtmf.Tarif @ dbmmf.tarif dbtmf.Descrip @ wsDescrip WITH FRAME frm-main.
   
   APPLY 'tab' TO SELF.
   RETURN.
END.

ON CHOOSE OF btn-Add IN FRAME frm-main 
    OR 'enter':u OF btn-Add IN FRAME frm-main 
    OR 'mouse-select-click' OF btn-Add IN FRAME frm-main 
DO: 
    IF wsOpt = "Update" THEN DO:
             FIND dbmmf WHERE dbmmf.dbAcc = dec(dbmmf.dbAcc:SCREEN-VALUE) AND dbmmf.Serial = (dbmmf.Serial:SCREEN-VALUE)
                            EXCLUSIVE-LOCK.
             ASSIGN
                 dbmmf.mstats = 1
                 dbmmf.READ[12] = dec(dbmmf.READ[13]:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.READ[13] = dec(dbmmf.READ[13]:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.rDate[12] = DATE(dbmmf.rdate[13]:SCREEN-VALUE IN FRAME frm-main)
                  dbmmf.rDate[13] = DATE(dbmmf.rdate[13]:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.wNextP = DEC(dbmmf.wNextP:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.Tarif = INT(dbmmf.tarif:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.consum[13] = 0.
    END.
    ELSE DO:
        CREATE dbmmf.
        ASSIGN
            dbmmf.dbacc = dec(dbmmf.dbAcc:SCREEN-VALUE in FRAME frm-main) 
            dbmmf.mstats = 1
                 dbmmf.READ[12] = dec(dbmmf.READ[13]:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.READ[13] = dec(dbmmf.READ[13]:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.rDate[12] = DATE(dbmmf.rdate[13]:SCREEN-VALUE IN FRAME frm-main)
                  dbmmf.rDate[13] = DATE(dbmmf.rdate[13]:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.wNextP = DEC(dbmmf.wNextP:SCREEN-VALUE IN FRAME frm-main)
                  dbmmf.Meter = INT(dbmmf.meter:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.Serial = dbmmf.serial:SCREEN-VALUE IN FRAME frm-main
                 dbmmf.Tarif = INT(dbmmf.tarif:SCREEN-VALUE IN FRAME frm-main)
                 dbmmf.consum[13] = 0.
                

    END.
     ASSIGN wsOpt = "Create".
   MESSAGE "Transaction Complete" VIEW-AS ALERT-BOX.
   CLEAR  FRAME frm-main  ALL.
   APPLY 'entry' TO dbmmf.dbAcc IN FRAME frm-main.
   RETURN NO-APPLY.
END.

/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */

ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
