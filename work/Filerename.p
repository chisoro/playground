DEF STREAM wslist.
DEF VAR wslist AS CHAR FORM "x(5000)".
DEF VAR fil AS CHAR FORM "x(40)".
DEF VAR rfil AS CHAR FORM "x(40)".
DEF VAR wsdir AS CHAR INITIAL "g:\simacc\work\".
DEFINE VARIABLE i AS INTEGER.
INPUT STREAM wslist FROM OS-DIR("g:\simacc\work").
OS-COMMAND SILENT cd g:\simacc\work.
REPEAT:
    IMPORT STREAM wslist fil.
    IF LENGTH(FIL)> 2 THEN
        rfil = TRIM(FIL) + ".zib".
    DISPLAY fil  rfil WITH WIDTH 120.
    PAUSE 0.
    rfil = substring(fil,1,index(fil,".z") - 1).
    ASSIGN fil = wsdir + fil
           rfil = wsdir + rfil.
    OS-RENAME VALUE(fil) VALUE(rfil).
END.
/*FIL = "g:\simacc\work\test.zib".
rfil = "g:\simacc\work\test.p".
OS-RENAME VALUE(fil) VALUE(rfil). */

