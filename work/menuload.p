input from g:\simacc\work\simAccMenu.csv.
for each men.
    display men.
    PAUSE 0.
    delete men.
END.
repeat :
    create men.
    import delimiter "," cItem cType submenu prog level.
    IF cType = "MENU-ITEM" THEN DO:
        FIND FIRST usrmen WHERE usrmen.usr = "1" AND usrmen.cItem = men.cItem NO-ERROR.
        IF NOT AVAILABLE usrmen THEN DO:
           CREATE usrmen.
        ASSIGN usrmen.usr = "1"
                usrmen.cItem = men.cItem.
        END.
    END.
END.
