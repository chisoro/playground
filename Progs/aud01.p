 /* Program.................Aud01.p
   Notes:................. Audit Report on Landsales and Service accounts creation/editting
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR wsTitle       AS CHAR FORM "x(80)".  
DEF VAR wsTitle1      AS CHAR FORM "x(80)".  
DEF VAR st-date       AS DATE.
DEF VAR en-date       AS DATE.
DEF VAR wsprog        AS CHAR.
DEF VAR wsopt1        AS INT VIEW-AS RADIO-SET HORIZONTAL
                RADIO-BUTTONS  "Master Account Creation", 1, "Master Record Editting", 2.
DEF VAR wsopt2        AS INT VIEW-AS RADIO-SET HORIZONTAL
                RADIO-BUTTONS  "Services Accounts", 1, "LandSales Accounts", 2.

DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btn-exit     LABEL "Close".


DEFINE RECTANGLE rect1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 130 by 2.3.
DEFINE RECTANGLE rect2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 130 by 20.5.
DEFINE RECTANGLE rect3 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 2.3.
DEFINE RECTANGLE rect4 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 20.0.

FORM AuditTrail.txtKey    FORM "X(13)"
     AuditTrail.txtField   COLUMN-LABEL "FIELD"
     AuditTrail.txtOld     COLUMN-LABEL "OLD VALUE"
     AuditTrail.txtNew     COLUMN-LABEL "NEW VALUE"
     AuditTrail.trDate     COLUMN-LABEL "DATE" FORMAT "99/99/99"
     AuditTrail.DeviceName COLUMN-LABEL "MACHINE"
     HEADER SKIP(1) wsTitle AT 20 skip(1) "Master record editting Audit Trail Report" AT 10 SPACE(2)
    "Page: " AT 90 PAGE-NUMBER(a) SKIP(1)  "Report Printing date" TODAY SKIP(1)
    WITH DOWN STREAM-IO FONT 10 CENTERED NO-BOX WIDTH 264 FRAME frm-rpt1.

FORM 
    Hsecmf.dbAcc COLUMN-LABEL "USER/ACCOUNT"
    Hsecmf.Name  COLUMN-LABEL "NAME"
    hsecmf.scheme COLUMN-LABEL "SCHEME"
    Hsecmf.PPrice COLUMN-LABEL "PRICE"
    Hsecmf.Ref COLUMN-LABEL "REFRENCE"
    Hsecmf.Credate COLUMN-LABEL "DATE"     
    Hsecmf.DeviceName COLUMN-LABEL "MACHINE"
    HEADER SKIP(1) wsTitle AT 20 skip(1) "Landsales Master Account Record Creation Audit Report" AT 10 SPACE(2)
   "Page: " AT 90 PAGE-NUMBER(a) SKIP(1)  "Report Printing date" TODAY SKIP(1)
   WITH DOWN STREAM-IO FONT 10 CENTERED NO-BOX WIDTH 264 FRAME frm-rpt.

FORM
    dbcmf.dbAcc COLUMN-LABEL "USER/ACCOUNT" 
    dbcmf.Name  COLUMN-LABEL "NAME"
    dbcmf.suburb COLUMN-LABEL "SUBURB"
    dbcmf.StandNo COLUMN-LABEL "STAND NO"
    dbcmf.Credate COLUMN-LABEL "DATE"
    dbcmf.DeviceName COLUMN-LABEL "MACHINE"
    HEADER SKIP(1) wsTitle AT 20 skip(1)  "Services Master Account Record Creation Audit Report" AT 10 SPACE(2)
   "Page: " AT 90 PAGE-NUMBER(a) SKIP(1) "Report Printing date" TODAY SKIP(1)
   WITH DOWN STREAM-IO FONT 10 CENTERED NO-BOX WIDTH 264 FRAME frm-dbrpt.

DEF NEW SHARED FRAME frmMain
    SKIP(2)
    wsOpt1 COLON 40 LABEL "Select option" AUTO-RETURN SKIP(1)
    wsOpt2 COLON 40 LABEL "Select option" AUTO-RETURN SKIP(1)
    st-date COLON 40 LABEL "Start Transaction Date" SKIP(1)
    en-date COLON 40 LABEL "End Transaction Date"
    btn-ok AT ROW 22.7 COL 20
    space(60) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 22 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    /*BGCOLOR 8 FGCOLOR 1 */ SIZE 135 BY 25.3
    TITLE "Debtors Master Record Audit Reports" VIEW-AS DIALOG-BOX.


ON 'choose':U OF btn-ok IN FRAME frmMain 
DO:
    ASSIGN wsopt1 wsopt2 st-date en-date.
    IF wsopt1 = 1 AND wsopt2 = 1 THEN
        wsprog = "dbCrerpt".
    IF wsopt1 = 1 AND wsopt2 = 2 THEN
        wsprog = "hseCrerpt".
    IF wsopt1 = 2 THEN
        wsprog = "Editrpt".
    {PrintOpt.i &stream-name="stream a"
                    &print-prog = VALUE(wsprog)
                    &paged}
    RETURN.
END.

/* Main Block */
FIND FIRST simctr NO-LOCK NO-ERROR.
wstitle = simctr.coname.
 VIEW FRAME frmMain.
 ASSIGN wsopt1:SCREEN-VALUE = "1"
        wsOpt2:SCREEN-VALUE = "1"
        st-date:SCREEN-VALUE = STRING(TODAY)
        en-date:SCREEN-VALUE = STRING(TODAY).
 ENABLE ALL WITH FRAME frmMain.
 WAIT-FOR CHOOSE OF btn-ok OR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
 HIDE FRAME frmMain.

PROCEDURE hseCreRpt.
     FOR EACH hsecmf WHERE Hsecmf.Credate >= st-date AND Hsecmf.Credate <= en-date BREAK BY hsecmf.uid:
         IF FIRST-OF(hsecmf.UID) AND hsecmf.UID <> "" THEN DO:
              FIND FIRST simusr WHERE simusr.usercode = hsecmf.UID NO-LOCK NO-ERROR.
              DISPLAY STREAM a simusr.NAME @ hsecmf.dbAcc WITH FRAME frm-rpt.
              DOWN STREAM a WITH FRAME frm-rpt.
          END.
       DISPLAY STREAM a  Hsecmf.dbAcc Hsecmf.Name Hsecmf.PPrice 
            Hsecmf.Ref Hsecmf.DeviceName Hsecmf.Credate hsecmf.scheme WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
    END.
END PROCEDURE.

PROCEDURE dbCreRpt.
     FOR EACH dbcmf WHERE dbcmf.credate >= st-date AND dbcmf.credate <= en-date BREAK BY dbcmf.uid:
         IF FIRST-OF(dbcmf.UID)  AND dbcmf.UID <> "" THEN DO:
              FIND FIRST simusr WHERE simusr.usercode = dbcmf.UID NO-LOCK NO-ERROR.
              DISPLAY STREAM a simusr.NAME @ dbcmf.dbAcc WITH FRAME frm-dbrpt.
              DOWN STREAM a WITH FRAME frm-dbrpt.
          END.
       DISPLAY STREAM a  dbcmf.dbAcc dbcmf.Name dbcmf.suburb dbcmf.StandNo dbcmf.Credate dbcmf.DeviceName WITH FRAME frm-dbrpt.
       DOWN STREAM a WITH FRAME frm-dbrpt.
    END.
END PROCEDURE.

PROCEDURE EditRpt.
    IF wsopt2 = 2 THEN DO:
      FOR EACH audittrail WHERE AuditTrail.txtTable = "hsecmf" AND date(AuditTrail.trDate) >= st-date AND date(AuditTrail.trDate) <= en-date
             BREAK BY AuditTrail.UID :
          IF FIRST-OF(AuditTrail.UID) THEN DO:
              FIND FIRST simusr WHERE simusr.usercode = AuditTrail.UID NO-LOCK NO-ERROR.
              DISPLAY STREAM a simusr.NAME @ AuditTrail.txtKey WITH FRAME frm-rpt1.
              DOWN STREAM a WITH FRAME frm-rpt1.
          END.
           DISPLAY STREAM a  AuditTrail.txtKey AuditTrail.txtField AuditTrail.txtOld AuditTrail.txtNew 
                  DATE(AuditTrail.trDate)@ AuditTrail.trDate AuditTrail.DeviceName WITH FRAME frm-rpt1.
           DOWN STREAM a WITH FRAME frm-rpt1.
        END.
    END.
    ELSE IF wsopt2 = 1 THEN DO:
      FOR EACH audittrail WHERE AuditTrail.txtTable = "dbcmf" AND date(AuditTrail.trDate) >= st-date AND date(AuditTrail.trDate) <= en-date
             BREAK BY AuditTrail.UID :
          IF FIRST-OF(AuditTrail.UID) THEN DO:
              FIND FIRST simusr WHERE simusr.usercode = AuditTrail.UID NO-LOCK NO-ERROR.
              DISPLAY STREAM a simusr.NAME @ AuditTrail.txtKey WITH FRAME frm-rpt1.
              DOWN STREAM a WITH FRAME frm-rpt1.
          END.
           DISPLAY STREAM a  AuditTrail.txtKey AuditTrail.txtField AuditTrail.txtOld AuditTrail.txtNew 
                  DATE(AuditTrail.trDate) @ AuditTrail.trDate AuditTrail.DeviceName WITH FRAME frm-rpt1.
           DOWN STREAM a WITH FRAME frm-rpt1.
        END.
    END.
END PROCEDURE.
