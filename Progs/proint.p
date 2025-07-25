DEF VAR lName AS CHAR INITIAL "INC".
DEF VAR wsOpt   AS INT VIEW-AS RADIO-SET VERTICAL
                RADIO-BUTTONS  "Export Receipts",1 ,
                               "Export Debtors Balances",2 ,
                               "Import Meter Readings", 3,
                               "Import Debtors Account", 4,
                               "Export Receipts from History",5, 
                               "Export Payroll Costing",6.  

DEF BUTTON btn-Ok LABEL "OK".
DEF BUTTON btn-exit LABEL "EXIT".

DEFINE RECTANGLE rect1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 2.3.
DEFINE RECTANGLE rect2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 6.5.


DEF NEW SHARED FRAME frm-Main
    wsOpt AT ROW 2 COL 10 LABEL "Select Option" AUTO-RETURN
    btn-ok AT ROW 8.7 COL 10
    space(30) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 8 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    /*BGCOLOR 8 FGCOLOR 1 */ SIZE 65 BY 11.3
    TITLE "SIMACC / PROMUN DATA EXCHANGE"  VIEW-AS DIALOG-BOX.

ON 'LEAVE':U OF wsOpt IN FRAME frm-main
    OR 'enter':U OF wsOpt IN FRAME frm-main
DO:
    ASSIGN wsOpt.
    APPLY 'tab' TO wsOpt IN FRAME frm-main.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
    IF CONNECTED(LName) THEN DO:
         MESSAGE "Connected to Promun system" VIEW-AS ALERT-BOX.
         IF wsopt = 1 THEN
            RUN recexp.p.
         ELSE  IF wsopt = 2 THEN
            RUN exgainloss.p.
         ELSE  IF wsopt = 3 THEN
            RUN impmeter.p.
         ELSE  IF wsopt = 4 THEN
             RUN proim.p.
         ELSE  IF wsopt = 5 THEN 
              RUN recexph.p.
         ELSE  IF wsopt = 6 THEN 
              RUN payexp.p.
    END.
    ELSE IF NOT CONNECTED(LName) THEN DO:
         CONNECT -db REC    -L 40000 -H 192.168.0.2 -S 50903 -U "user" -P use123 -N TCP NO-ERROR.
         CONNECT -db INC    -L 40000 -H 192.168.0.2 -S 50901 -U "user" -P use123 -N TCP NO-ERROR.
         CONNECT -db EXD    -L 40000 -H 192.168.0.2 -S 50902 -U "user" -P use123 -N TCP NO-ERROR.
         CONNECT -db promun -L 40000 -H 192.168.0.2 -S 50900 -U "user" -P use123 -N TCP NO-ERROR.
         CONNECT -db hrs    -L 40000 -H 192.168.0.2 -S 50904 -U "user" -P use123 -N TCP NO-ERROR.

         IF CONNECTED(LName) THEN DO:
             IF wsopt = 1 THEN
                RUN recexp.p.
             ELSE  IF wsopt = 2 THEN
                RUN exgainloss.p.
             ELSE  IF wsopt = 3 THEN
                RUN impmeter.p.
             ELSE  IF wsopt = 4 THEN
                RUN proim.p.
              ELSE  IF wsopt = 5 THEN
                RUN recexph.p.
              ELSE  IF wsopt = 6 THEN 
                 RUN payexp.p.
         END.
    END.
    IF NOT CONNECTED(lName) THEN DO:
       MESSAGE "Failed to connect to Promun.....Exiting" VIEW-AS ALERT-BOX.
       APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
       HIDE FRAME frm-main.
    END.
    RETURN.
END.

/********** MAIN LOGIC **********/
VIEW FRAME frm-Main.
ENABLE ALL WITH FRAME frm-Main.
FIND FIRST simctr NO-LOCK NO-ERROR.
wsOpt = 1.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Main.
HIDE FRAME frm-Main.

