/* Program.................Tarms05.p
   Notes:...... Generate Tarms earning data from payroll
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.

DEFINE VARIABLE chExcel AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRange AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorksheet   AS COM-HANDLE NO-UNDO.  
DEFINE VARIABLE hCellValue   AS COM-HANDLE NO-UNDO.  
DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol         AS INTEGER   NO-UNDO.  
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE oFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE wsFile2 AS CHARACTER NO-UNDO.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF NEW SHARED VAR varUser LIKE simusr.usercode INITIAL "1".
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsStatus LIKE payemf.empcode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsDate AS DATE.
DEF VAR wsName AS CHAR FORM "x(70)".
DEF VAR wsDesc AS CHAR FORM "x(40)".
DEF VAR payrate LIKE tblforex.decrate.
DEF VAR zwRatio AS DEC FORM "zz9.9999999".
DEF VAR usRatio LIKE zwRatio.
DEF VAR po AS INT.
DEF VAR it AS INT.
DEF VAR j AS INT.
DEF VAR X AS INT.
DEF VAR ws AS DEC EXTENT 100.
DEF STREAM a.
DEFINE VARIABLE amtCount      AS INTEGER   NO-UNDO.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btn-Sys   LABEL "Payroll System".

DEF TEMP-TABLE tartbl
    FIELD tin   LIKE payemf.tin
    FIELD idno   LIKE payemf.idno
    FIELD NAME LIKE payemf.SName
    FIELD txtcur  AS CHAR
    FIELD amt     LIKE tarmsmtf.TAmt EXTENT 48.

DEF BUFFER tmpTarms FOR tarms PRESELECT .
DEF BUFFER bfrpayemf FOR payemf PRESELECT.
DEF BUFFER bfrtarmsmtf FOR tarmsmtf.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
            DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
            WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE RECTANGLE rect-1
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
         SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
         SIZE 80 by 8.5.

DEFINE FRAME frm-main
        SKIP(2)
        btn-Sys   COLON 14 NO-LABEL NO-TAB-STOP
        wsSys     NO-LABEL
        Paysys.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
        st-per    COLON 30 LABEL "Payroll Period" VIEW-AS TEXT SKIP(0.5)
        wsDate    COLON 30 LABEL "Run Date" VIEW-AS TEXT SKIP(2)
        wsStatus   COLON 40 LABEL "Generating TARMS Earnings data......" VIEW-AS TEXT NO-TAB-STOP
        skip(1.5)
        btn-ok colon 15
        btn-close colon 50
        rect-2 AT ROW 1.4 COL 3
        rect-1 AT ROW 10 COL 3
        WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
         SIZE 85 BY 13
        TITLE "GENERATE TARMS EARNINGS DATA" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-picksys 
        brw-Sys AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

ON CHOOSE OF btn-sys IN FRAME frm-Main
     OR CHOOSE OF btn-sys IN FRAME frm-Main
 DO:
   VIEW FRAME frm-picksys.
   OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
   ENABLE ALL WITH FRAME frm-picksys.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picksys 
           OR close of THIS-PROCEDURE IN FRAME frm-picksys
           OR CHOOSE OF btn-ok IN FRAME frm-picksys 
           OR 'enter':u OF brw-sys
           OR 'mouse-select-dblclick' OF brw-sys.
   CLOSE QUERY qry-sys.
   HIDE FRAME frm-picksys.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO wsSys.
   RETURN. 
 END.

ON CHOOSE OF btn-ok IN FRAME frm-picksys 
     OR 'enter':u OF brw-sys
     OR 'mouse-select-dblclick' OF brw-sys
 DO: 
    GET CURRENT qry-sys NO-LOCK NO-WAIT.
    DISPLAY Paysys.Paysys @ wsSys WITH FRAME frm-Main.
    RETURN. 
 END.

ON  'enter':U OF wsSys IN FRAME frm-Main
     OR 'tab':U OF wsSys IN FRAME frm-Main
 DO:
    ASSIGN wsSys.
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
              DISPLAY Paysys.Descrip  Paysys.CurDate @ wsDate Paysys.CurPer @ st-per WITH FRAM frm-Main.
         END.
         ELSE DO:
             MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
         END.
     END.  
     ELSE DO:
         MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
 END.

ON CHOOSE OF btn-ok IN FRAME frm-main DO:
         session:set-wait-state("").
         IF (wsSys:SCREEN-VALUE IN FRAME frm-main) = "" THEN DO:
             MESSAGE "Invalid Payroll System....." VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
         END.
         ELSE IF (wsSys:SCREEN-VALUE IN FRAME frm-main) <> "" THEN DO:
             FIND FIRST Paysys WHERE Paysys.paysys = INT(wsSys:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
             ASSIGN st-per = INT(st-per:SCREEN-VALUE IN FRAME frm-main)
                    wsDate = DATE(wsDate:SCREEN-VALUE IN FRAME frm-main)
                    wsSys  = INT(wsSys:SCREEN-VALUE IN FRAME frm-main).
              FOR EACH bfrpayemf WHERE bfrpayemf.paysys = wsSys  AND bfrpayemf.tin <>  0:
                IF  bfrpayemf.estatus = 4 AND bfrpayemf.doe < DATE ("01/01/" + STRING(YEAR(Paysys.CurDate))) THEN NEXT.
                DISPLAY bfrpayemf.empcode @ wsStatus WITH FRAME frm-main.
                PAUSE 0. 
                CREATE tartbl.
                ASSIGN tartbl.NAME = TRIM(bfrpayemf.FName) + " " + TRIM(bfrpayemf.SName)
                       tartbl.txtcur = paysys.txtcur
                       tartbl.tin  = bfrpayemf.tin.
                DO X = 1 TO LENGTH(bfrpayemf.idno):
                    IF SUBSTR(bfrpayemf.idno,x,1) = "" THEN NEXT.
                    ELSE IF SUBSTR(bfrpayemf.idno,x,1) = "/" THEN NEXT.
                    ELSE IF SUBSTR(bfrpayemf.idno,x,1) = "-" THEN NEXT.
                    ELSE DO:
                       tartbl.idno = tartbl.idno + SUBSTR(bfrpayemf.idno,x,1).
                    END.                   
                END.
                tartbl.idno = substr(tartbl.idno,1,2) + "-" + substr(tartbl.idno,3).
                FOR EACH bfrtarmsmtf WHERE bfrtarmsmtf.empcode = bfrpayemf.empcode AND bfrtarmsmtf.ITEM <= 24 BY  bfrtarmsmtf.ITEM:
                    po =  bfrtarmsmtf.ITEM .
                    j = po - 1.
                    po = po + j.
                    DISPLAY bfrtarmsmtf.ITEM @ wsStatus WITH FRAME frm-main.
                    PAUSE 0.     
                    ASSIGN tartbl.amt[po] = bfrtarmsmtf.amt[1]
                           tartbl.amt[po + 1] = bfrtarmsmtf.amt[2].
                     j = j + 1.
                END.
            END.
            
            OUTPUT TO value(cFile).
            
            EXPORT DELIMITER ',' "TIN" "ID" "Name" "Currency" "Amount1" "Amount2" "Amount3"	"Amount4" "Amount5" "Amount6" "Amount7" "Amount8" "Amount9"	"Amount10"	"Amount11"
                                "Amount12" "Amount13"	"Amount14" "Amount15" "Amount16" "Amount17" "Amount18" "Amount19"	"Amount20"	"Amount21" "Amount22" "Amount23"
                                "Amount24" "Amount25"	"Amount26" "Amount27" "Amount28" "Amount29" "Amount30" "Amount31"	"Amount32"	"Amount33" "Amount34" "Amount35"
                                "Amount36" "Amount37" "Amount38" "Amount39" "Amount40" "Amount41" "Amount42" "Amount43"	"Amount44"	"Amount45" "Amount46" "Amount47" "Amount48".
            FOR EACH tartbl.
                EXPORT DELIMITER ',' tartbl.
            END.
           /*RUN wri-Excel.*/
            MESSAGE "Extraction Completed........" VIEW-AS ALERT-BOX.
          OS-COMMAND  NO-WAIT VALUE(wsFile2  + "bin\tarms.exe").
           
         END. 
         APPLY 'entry' TO btn-close IN FRAME frm-main.
END.


    /********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN cFile = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "tarms\employeeEarning.csv".
 /* ASSIGN oFile = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "tarms\earnings.csv".   Change to your actual file path */
wsFile2 = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)).
/*OS-COMMAND SILENT DEL VALUE(oFile).*/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST payctr NO-LOCK NO-ERROR.
ASSIGN st-per:SCREEN-VALUE = ""
       wsDate:SCREEN-VALUE = STRING(TODAY).
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
