DEF VAR lname AS CHAR "x(20)" INITIAL dbAc.
DEF VAR lname1 AS CHAR "x(20)" INITIAL db03.
CONNECT -db dbAc    -L 40000 -H ICT -S 19694 -U "Simacc" -P Sim6771 -N TCP NO-ERROR.
CONNECT -db db03    -L 40000 -H ICT -S 19695 -U "Simacc" -P Sim6771 -N TCP NO-ERROR.
IF (CONNECTED(lname)) AND (CONNECTED(lname1)) THEN DO:
    RUN sync1.p.
END.
ELSE DO:
    MESSAGE "Online database not connected" VIEW-AS ALERT-BOX.
END.
