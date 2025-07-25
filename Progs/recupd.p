/* Program.................recUpd.p
   Notes:......            Receipt update main calling program.
   Author:.................S. Mawire
*/
DEF  SHARED VAR varUser LIKE simusr.usercode.
DEF NEW SHARED VAR wsTitle AS CHAR FORM "x(80)".
DEF NEW SHARED VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF NEW SHARED VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF NEW SHARED VAR wsDate     LIKE dbRecCtr.RecDate.
DEF NEW SHARED VAR wsOp       LIKE dbRecCtr.usercode.
DEF VAR wsName AS CHAR FORM "X(20)".

DEF BUTTON btn-Valid   LABEL "Validate".
DEF BUTTON btn-Update   LABEL "Update".
DEF BUTTON btn-Exit    LABEL "Close".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 85 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 85 BY 17.5.

DEFINE QUERY qry-Recctr FOR dbRecCtr scrolling.
DEF BROWSE brw-Recctr QUERY qry-Recctr
        DISPLAY wsname COLUMN-LABEL "CASHIER"
                dbRecCtr.usercode  COLUMN-LABEL "USER" 
                dbRecCtr.RecDate   COLUMN-LABEL "RECEIPT DATE"
                dbRecCtr.Validate  COLUMN-LABEL "VALIDATED"
                dbRecCtr.EOD       COLUMN-LABEL "EOD" 
                dbRecCtr.RecTotal  COLUMN-LABEL "AMOUNT" FORM "zzz,zzz,zzz,zz9.99-" 
        WITH 20 DOWN SEPARATORS.

DEF FRAME frm-main
    brw-RecCtr AT ROW 2 COL 6
    btn-Update AT ROW 19.7 COL 15 SPACE(50) 
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 22.5
    TITLE "Receipt Validation and  Update" view-as dialog-box.


/******* Triggers ***** */
ON CHOOSE OF btn-Update IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-Recctr
DO:
    GET CURRENT qry-Recctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsDate = dbRecCtr.RecDate
           WsOp = dbRecCtr.usercode.
    RUN recup01.p.
    brw-RecCtr:REFRESH().
END.

ON 'mouse-select-click':U OF brw-RecCtr  
DO:
    IF dbRecCtr.VALIDATE <> YES THEN
        DISABLE btn-update WITH FRAME frm-main.
    ELSE
        ENABLE btn-update WITH FRAME frm-main.
    RETURN.
END.

ON row-display OF brw-RecCtr  IN FRAME frm-Main
DO:
    FIND FIRST simusr WHERE simusr.usercode = dbRecCtr.usercode NO-LOCK NO-ERROR.
    IF AVAILABLE simusr THEN
        wsName = simusr.NAME.
    ELSE
        wsName = "Invalid Operator".
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
wsTitle = simctr.CONAME + "   Report Date: " + STRING(TODAY).
OPEN QUERY qry-RecCtr FOR EACH dbRecCtr NO-LOCK.
ENABLE ALL  WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-RecCtr.
HIDE FRAME frm-main.


