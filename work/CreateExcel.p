DEFINE VARIABLE chExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE misValue    AS COM-HANDLE NO-UNDO.

CREATE "excel.application" chExcel.

chExcel:visible = true.

chWorkbook  = chExcel:Workbooks:Add().
chWorksheet = chWorkbook:Worksheets(1).

chWorksheet:Range("A1:B1"):NumberFormat = "@".

chWorksheet:Range("A1"):VALUE = "Name".
chWorksheet:Range("B1"):VALUE = "Function".

chWorksheet:Range("A2"):VALUE = "emp1".
chWorksheet:Range("A3"):VALUE = "emp1".
chWorksheet:Range("A4"):VALUE = "emp1".
chWorksheet:Range("A5"):VALUE = "emp2".
chWorksheet:Range("A6"):VALUE = "emp2".
chWorksheet:Range("A7"):VALUE = "emp2".
chWorksheet:Range("B2"):VALUE = "job1".
chWorksheet:Range("B3"):VALUE = "job3".
chWorksheet:Range("B4"):VALUE = "job5".
chWorksheet:Range("B5"):VALUE = "job3".
chWorksheet:Range("B6"):VALUE = "job3".
chWorksheet:Range("B7"):VALUE = "job2".

chExcel:Worksheets("Sheet1"):Activate().

/* chWorksheet.Range(1, 1).CurrentRegion.Subtotal(GroupBy:=1, Function:=-4112, TotalList:=2, Replace:=True, PageBreaks:=False, SummaryBelowData:=False) */

chWorksheet:Range("A1"):CurrentRegion:Subtotal(1, -4112, 2, True, False, False).

/* Quit Excel */

chExcel:Quit().

/* Release Com-handle  */

RELEASE OBJECT chWorksheet.
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chExcel.
