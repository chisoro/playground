/* Program.................usracc.p
   Notes:................. User Access Listing
   Author:.................S. Mawire
*/

DEF VAR wsitem LIKE usrmen.cItem EXTENT 2.
DEF VAR wsit LIKE usrmen.cItem EXTENT 2.
DEF BUFFER altmen FOR men.

FORM altmen.cItem FORM "x(60)"
     WITH DOWN STREAM-IO NO-LABEL CENTERED NO-BOX FRAME a.

FOR  EACH men, EACH usrmen WHERE men.cItem = usrmen.cItem AND usr = "11".
    FIND FIRST altmen WHERE STRING(altmen.submenu) = SUBSTR(STRING(men.submenu),1,1) NO-LOCK NO-ERROR.
    wsItem[1] = altmen.cItem.
    IF wsItem[1] <> wsIt[1] THEN DO:
        DISPLAY usrmen.usr altmen.cItem WITH FRAME a.
        DOWN WITH FRAME a.
        wsIt[1] = altmen.cItem.
    END.
    ELSE IF men.level = 2 THEN DO:
        FIND FIRST altmen WHERE STRING(altmen.submenu) = SUBSTR(STRING(men.submenu),1,3) NO-LOCK NO-ERROR.
        wsItem[2] = altmen.cItem.
        IF wsItem[2] <> wsIt[2] THEN DO:
            DISPLAY "     " + altmen.cItem @  altmen.cItem WITH FRAME a.
            DOWN WITH FRAME a.
        END.
    END.
    DISPLAY "          " + usrmen.cItem @  altmen.cItem WITH FRAME a.
    DOWN WITH FRAME a.
    wsIt[2] = altmen.cItem.
END.
