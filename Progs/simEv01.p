/* Program.................SimEv01.p.p
   Notes:...... Create an Event
   Author:.................S. Mawire
*/session:DATA-ENTRY-RETURN = TRUE.
DEF NEW SHARED VAR wsName LIKE simusr.Name.
DEF NEW SHARED VAR varUser LIKE simusr.usercode.

DEFINE BUTTON btnok 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 10 .

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 9 BY 1.14
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 8.81
     BGCOLOR 6 FGCOLOR 0 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 2.81
     BGCOLOR 6 FGCOLOR 0 .

DEFINE FRAME frm-Main
    SKIP(1)
    simalert.AlertSrc COLON 20 LABEL "SOURCE Module"
                view-as combo-box list-item-pairs
       "N - NONE","NA","D - Debtors","DB","C - Creditors","CR","L - Ledger","GL" SPACE(2) 
    simalert.AlertAcc COLON 20 LABEL "Attached Account" SKIP(0.5)
    simalert.Msg  COLON 20 LABEL "Alert MESSAGE"  VIEW-AS FILL-IN SIZE 45 BY 2 SKIP(0.5)
    simalert.DueDate COLON 20 LABEL "Due DATE" SKIP(0.5)
    simalert.AssignTo COLON 20 LABEL "ASSIGN TO" SKIP(0.5)
     btnok AT ROW 11.38 COL 15 WIDGET-ID 8
     BtnCancel AT ROW 11.38 COL 48 WIDGET-ID 10
     RECT-1 AT ROW 1.48 COL 3 WIDGET-ID 2
    RECT-2 AT ROW 10.48 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10.4 ROW 5.19
         SIZE 72.8 BY 13.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON BtnCancel WIDGET-ID 100 TITLE "EVENT Maintenance"
    VIEW-AS DIALOG-BOX.



/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.

WAIT-FOR CHOOSE OF btncancel  OR CHOOSE OF btnOk OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
