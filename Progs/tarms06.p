/* Program.................Tarms06.p
   Notes:...... Generate Tarms Employee upload - employees with no TIN 
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEFINE VARIABLE chExcel AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRange AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE oFile AS CHARACTER NO-UNDO.
DEF VAR X AS INT.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser LIKE simusr.usercode INITIAL '1'.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsStatus LIKE payemf.empcode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsDate AS DATE.
DEF VAR wsName AS CHAR FORM "x(70)".
DEF VAR wsDesc AS CHAR FORM "x(40)".
DEF VAR wsidno LIKE payemf.idno.
DEF VAR payrate LIKE tblforex.decrate.
DEF VAR zwRatio AS DEC FORM "zz9.9999999".
DEF VAR usRatio LIKE zwRatio.
DEF VAR po AS INT.
DEF VAR it AS INT.
DEFINE VARIABLE wsFile2 AS CHARACTER NO-UNDO.
DEF VAR j AS INT.
DEF VAR ws AS DEC EXTENT 100.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btn-Sys   LABEL "Payroll System".

DEF BUFFER bfrpayemf FOR payemf PRESELECT.

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
        wsStatus   COLON 40 LABEL "Generating TARMS Employee upload......" VIEW-AS TEXT NO-TAB-STOP
        skip(1.5)
        btn-ok colon 15
        btn-close colon 50
        rect-2 AT ROW 1.4 COL 3
        rect-1 AT ROW 10 COL 3
        WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
         SIZE 85 BY 13
        TITLE "GENERATE TARMS EMPLOYEE DATA FOR UPLOAD" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-pick 
        brw-Sys AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

ON CHOOSE OF btn-sys IN FRAME frm-main
     OR CHOOSE OF btn-sys IN FRAME frm-main
 DO:
   VIEW FRAME frm-Pick.
   OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
   ENABLE ALL WITH FRAME frm-Pick.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Pick 
           OR close of THIS-PROCEDURE IN FRAME frm-Pick
           OR CHOOSE OF btn-ok IN FRAME frm-Pick 
           OR 'enter':u OF brw-sys
           OR 'mouse-select-dblclick' OF brw-sys.
   CLOSE QUERY qry-sys.
   HIDE FRAME frm-Pick.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO wsSys.
   RETURN. 
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-Pick 
     OR 'enter':u OF brw-sys
     OR 'mouse-select-dblclick' OF brw-sys
 DO: 
    GET CURRENT qry-sys NO-LOCK NO-WAIT.
    DISPLAY Paysys.Paysys @ wsSys WITH FRAME frm-main.
    RETURN. 
 END.

 ON  'enter':U OF wsSys IN FRAME frm-main
     OR 'tab':U OF wsSys IN FRAME frm-main
 DO:
    ASSIGN wsSys.
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
              DISPLAY Paysys.Descrip  Paysys.CurDate @ wsdate Paysys.CurPer @ st-per WITH FRAM frm-main.
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
            /* OUTPUT TO VALUE(cFile).*/
             EXPORT STREAM a DELIMITER ','  "IDNo" "FName" "MName" "SName" "DOB" "DOE" "PDate" "Designation".
             FIND FIRST Paysys WHERE Paysys.paysys = INT(wsSys:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
             ASSIGN st-per = INT(st-per:SCREEN-VALUE IN FRAME frm-main)
                    wsDate = DATE(wsDate:SCREEN-VALUE IN FRAME frm-main)
                    wsSys  = INT(wsSys:SCREEN-VALUE IN FRAME frm-main).
              FOR EACH bfrpayemf WHERE bfrpayemf.paysys = wsSys AND  bfrpayemf.tin = 0 :
                IF  bfrpayemf.estatus = 4 AND bfrpayemf.doe < DATE ("01/01/" + STRING(YEAR(Paysys.CurDate))) THEN NEXT.
                DISPLAY bfrpayemf.empcode @ wsStatus WITH FRAME frm-main.
                PAUSE 0. 
                wsidno = REPLACE(bfrpayemf.idno," ","").
                wsidno = REPLACE(wsidno,"-","").
                wsidno = REPLACE(wsidno,"/","").
                wsidno = substr(wsidno,1,2) + "-" + substr(wsidno,3).
                 IF bfrpayemf.idno <> wsidno  THEN
                     bfrpayemf.idno = wsidno.
                EXPORT STREAM a DELIMITER ','  bfrpayemf.idno bfrpayemf.FName " " bfrpayemf.SName bfrpayemf.dob bfrpayemf.startdate Paysys.CurDate Designation.
              END.
              OUTPUT STREAM a CLOSE. 
            /*RUN wri-excel.*/
            MESSAGE "Extraction Completed........" VIEW-AS ALERT-BOX.
            OS-COMMAND  NO-WAIT VALUE(wsFile2  + "bin\tarms06.exe").
         END.

         APPLY 'entry' TO btn-close IN FRAME frm-main.
END.


    /********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN cFile = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "tarms\employeeDetails.csv".
 wsFile2 = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)).
ENABLE ALL WITH FRAME frm-main.

FIND FIRST payctr NO-LOCK NO-ERROR.
ASSIGN st-per:SCREEN-VALUE = ""
       wsDate:SCREEN-VALUE = STRING(TODAY).
OUTPUT STREAM a TO VALUE(cFile).
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Wri-Excel:
    CREATE "Excel.Application" chExcel NO-ERROR.
    IF NOT VALID-HANDLE(chExcel) THEN DO:
        MESSAGE "Could not start Excel!" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    chExcel:VISIBLE = FALSE. /* Run Excel in the background */
    
    /* Open the workbook */
    chWorkbook = chExcel:Workbooks:OPEN(cFile) NO-ERROR.
    IF NOT VALID-HANDLE(chWorkbook) THEN DO:
        MESSAGE "Could not open Excel file!" VIEW-AS ALERT-BOX ERROR.
        RELEASE OBJECT chExcel.
        RETURN.
    END.
    
    /* Select the first worksheet */
    chWorksheet = chWorkbook:Sheets:Item(1).
    
    /* Find the next empty row */
    iRow = 2.
    /*DO WHILE chWorksheet:Cells:Item(iRow, 1):Value <> ?:
        iRow = iRow + 1.
    END. */
    
    /* Append new data */
    FOR EACH bfrpayemf WHERE bfrpayemf.paysys = wsSys AND  bfrpayemf.tin = 0 :
                IF  bfrpayemf.estatus = 4 AND bfrpayemf.doe < DATE ("01/01/" + STRING(YEAR(Paysys.CurDate))) THEN NEXT.
                DISPLAY bfrpayemf.empcode @ wsStatus WITH FRAME frm-main.
                PAUSE 0. 
                wsidno = REPLACE(bfrpayemf.idno," ","").
                wsidno = REPLACE(wsidno,"-","").
                wsidno = REPLACE(wsidno,"/","").
                wsidno = substr(wsidno,1,2) + "-" + substr(wsidno,3).
                 IF bfrpayemf.idno <> wsidno  THEN
                     bfrpayemf.idno = wsidno.
                chWorksheet:Cells:Item(iRow, 1):Value = STRING( bfrpayemf.idno).
                chWorksheet:Cells:Item(iRow, 2):Value = STRING(bfrpayemf.FName).
                chWorksheet:Cells:Item(iRow, 3):Value = STRING(bfrpayemf.SName).
                chWorksheet:Cells:Item(iRow, 4):Value = "".
                chWorksheet:Cells:Item(iRow, 5):Value = STRING(bfrpayemf.dob).
                chWorksheet:Cells:Item(iRow, 6):Value = STRING(bfrpayemf.startdate).
                chWorksheet:Cells:Item(iRow, 7):Value = STRING(Paysys.CurDate).
                chWorksheet:Cells:Item(iRow, 8):Value = Designation. 
                iRow = iRow + 1.
    END.
    
    /* Save and close */
    chWorkbook:SAVEAS(oFile,,,,,,).
    chWorkbook:CLOSE().
    chExcel:QUIT().
    
    /* Cleanup */
    RELEASE OBJECT chWorksheet.
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chExcel.
END PROCEDURE.
