DEF VAR lName AS CHAR INITIAL "inc".
IF CONNECTED(LName) THEN DO:
     MESSAGE "Connected to Promun Receipting database" VIEW-AS ALERT-BOX.
     RUN impMeter.p.
END.
ELSE IF NOT CONNECTED(LName) THEN DO:
     CONNECT -db inc  -L 40000 -H localhost -S 50901  -U "user" -P use123 -N TCP NO-ERROR.
     IF CONNECTED(LName) THEN
        RUN impMeter.p.
END.
IF NOT CONNECTED(lName) THEN DO:
   MESSAGE "Failed to connect to Promun.....Exiting" VIEW-AS ALERT-BOX.
   APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
   HIDE FRAME frm-main.
END.
IF CONNECTED(LName) THEN
    DISCONNECT inc.
