DEFINE VARIABLE lc-line AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-host AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEF VAR j AS INTEGER.
DEF VAR txtUnWanted AS CHAR EXTENT 3.
DEF VAR wsFile AS CHAR.
DEF VAR wsFile1 AS CHAR.
DEFINE STREAM s-load.
DEF VAR wsCommand AS CHAR.

txtUnWanted[1] = "~(".
txtUnWanted[3] = "Preferred".
txtUnWanted[2] = "~)".

FIND FIRST simctr NO-LOCK.
wsFile = simCtr.RepDir  + "outputfile.txt".
wsFile1 = "ipconfig /all >" + wsFile.
dos silent VALUE(wsFile1).
INPUT STREAM s-load FROM value(wsFile).
repeat:
    import stream s-load unformatted lc-line.

    if trim(lc-line) begins "IPv4 Address" then
    do:
        assign lc-address = substring(lc-line, index(lc-line, ":") + 2).
        DO j = 1 TO EXTENT(txtUnWanted):
            IF txtUnWanted[j] <> "" THEN DO:
                lc-address = REPLACE(lc-address, txtUnWanted[j], "").
            END.
        END.
    end.
    if trim(lc-line) begins "Host Name" then
    do:
        assign lc-host = substring(lc-line, index(lc-line, ":") + 2).
        
    end.
 end.
 input stream s-load close.
OS-DELETE value(wsFile).

