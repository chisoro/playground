
DEF STREAM a.
DEF VAR wslabel AS CHAR FORM "x(10)" EXTENT 5.
DEF VAR wsitem AS INT EXTENT 5.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99" EXTENT 5.

DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 14.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.

DEFINE FRAME frm-Paysys
     SKIP(1.5)
    "ITEM               DESCRIPTION" SKIP
    wsItem[1] NO-LABEL wsLabel[1] NO-LABEL
    SKIP(0.5)
     btn-Ok AT ROW 16.71 COL 15  WIDGET-ID 48
     btn-Close AT ROW 16.71 COL 65 WIDGET-ID 48
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 16.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D
      SIZE 95 BY 19 
    TITLE "PAYROLL REPORTER - EXCEL REPORTS" VIEW-AS DIALOG-BOX.

FORM
    paymtf.empcode NO-LABEL
    wsAmt[1] NO-LABEL
    WITH NO-BOX DOWN  STREAM-IO WIDTH 132 NO-LABELS FRAME frm-rpt1.

FORM
    "EMP-CODE" 
    wslabel[1]
    wslabel[2]
    SKIP
    WITH NO-BOX DOWN  STREAM-IO WIDTH 132 NO-LABELS FRAME frm-rpt.


ON CHOOSE OF btn-ok IN FRAME frm-Paysys 
 DO: 
    ASSIGN wslabel[1] wsitem[1] .
    OUTPUT STREAM a TO TERMINAL.
    DISPLAY STREAM a wsLabel[1] wslabel[2] WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    FIND FIRST paymtf WHERE itemcode = wsitem[1] NO-ERROR.
    ASSIGN wsAmt[1] = paymtf.CurAmt WHEN itemcode = wsitem[1]. 
    DISPLAY STREAM a paymtf.empcode wsAmt[1] WITH FRAME frm-rpt1.
    DOWN STREAM a WITH FRAME frm-rpt1.
    RETURN. 
 END.

/*main logic */
FIND FIRST SIMCTR NO-LOCK.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.

