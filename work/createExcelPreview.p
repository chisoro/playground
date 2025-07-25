/************** Define variables ***********/
DEFINE VARIABLE hExcel          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorkbook       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorksheet      AS COM-HANDLE NO-UNDO.
define variable hPageSetup      as com-handle no-undo.

/*** Open Excel and initialize variables ***/
CREATE "Excel.Application" hExcel.

ASSIGN
    hWorkBook       =   hExcel:WorkBooks:Add()
    hWorkSheet      =   hWorkBook:WorkSheets(1)
    hPageSetup      =   hWorkSheet:PageSetup
    hWorkSheet:Cells(1,1 ) = "Test data"
    hPageSetup:CenterHeader = "First Header Line" + chr(13) + "Second Header Line" + chr(13) + "&D page &P of &N &"
    hExcel:visible = true.

/****** Print Preview Selected Sheets ******/
hExcel:Application:ActiveWindow:SelectedSheets:PrintPreview.

/* Close Workbooks & Quit Excel Application */
hExcel:Application:Workbooks:CLOSE() NO-ERROR.
hExcel:Application:QUIT NO-ERROR.

/********* Delete All Object Handles *********/
IF VALID-HANDLE(hPageSetup) THEN
    RELEASE OBJECT hPageSetup.
IF VALID-HANDLE(hWorksheet) THEN
    RELEASE OBJECT hWorksheet.
IF VALID-HANDLE(hWorkbook) THEN
    RELEASE OBJECT hWorkbook.
IF VALID-HANDLE(hExcel) THEN
RELEASE OBJECT hExcel.
