
/*CREATE MENU  MENU-BAR-C-Win.
CREATE SUB-MENU hSubMenu
    ASSIGN PARENT = MENU-BAR-C-Win
    LABEL = "File".
CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = "Change Password"
    TRIGGERS:
        ON CHOOSE DO:
            RUN logon01.p.
        END.
    END TRIGGERS.
CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = "LOG-OFF"
    TRIGGERS:
        ON CHOOSE
            APPLY 'close' TO THIS-PROCEDURE.
    END TRIGGERS.*/

FOR EACH men NO-LOCK:
    if men.cType = "Menu-item" then do:
       find first usrmen where usrmen.usr = varUser  and men.cItem = usrmen.cItem  NO-LOCK no-error.
        if available usrmen THEN DO:
            IF level = 1 THEN DO:
                 IF varUser <> "1" AND men.prog = "simusr01.p" THEN DO:
                     CREATE value(men.cType) hMenuItem
                    ASSIGN PARENT = hSubMenu
                    LABEL = men.cItem
                    SENSITIVE = FALSE
                    TRIGGERS:
                        ON CHOOSE do:
                            FIND FIRST men WHERE men.cItem = SELF:LABEL NO-LOCK NO-ERROR.
                           wsprg = men.prog.
                           RUN VALUE(wsprg).
                        END.
                    END TRIGGERS.
                 END.
                 ELSE DO:
                     CREATE value(men.cType) hMenuItem
                    ASSIGN PARENT = hSubMenu
                    LABEL = men.cItem
                    SENSITIVE = TRUE
                    TRIGGERS:
                        ON CHOOSE do:
                            FIND FIRST men WHERE men.cItem = SELF:LABEL NO-LOCK NO-ERROR.
                           wsprg = men.prog.
                           RUN VALUE(wsprg).
                        END.
                    END TRIGGERS.
                 END. 
            END.
            ELSE IF level = 2 THEN DO:
                CREATE value(men.cType) hMenuItem
                ASSIGN PARENT = h1SubMenu
                LABEL = men.cItem
                SENSITIVE = TRUE
                TRIGGERS:
                    ON CHOOSE do:
                        FIND FIRST men WHERE men.cItem = SELF:LABEL NO-LOCK NO-ERROR.
                       wsprg = men.prog.
                       RUN VALUE(wsprg).
                    END.
                END TRIGGERS.
            END.
        END. /*eof usrmen */
        ELSE if NOT available usrmen THEN DO:
            IF level = 1 THEN DO:
                CREATE value(men.cType) hMenuItem
                ASSIGN PARENT = hSubMenu
                LABEL = men.cItem
                SENSITIVE = FALSE.
            END.
            ELSE IF level = 2 THEN DO:
                CREATE value(men.cType) hMenuItem
                ASSIGN PARENT = h1SubMenu
                LABEL = men.cItem
                SENSITIVE = FALSE .
            END.
        END. /*eof usrmen */
        END. /*eof Ctype Menu-item" */  
    ELSE IF men.cType = "MENU" THEN
        CREATE SUB-MENU hSubMenu
        ASSIGN PARENT =  MENU-BAR-C-Win
        LABEL = men.cItem.
    ELSE if men.cType = "SUB-MENU" THEN
            CREATE SUB-MENU h1SubMenu
            ASSIGN PARENT = hSubMenu
            LABEL = men.cItem.  
END.

   /*WAIT-FOR close of THIS-PROCEDURE IN FRAME frm-Track.
   CLOSE QUERY qry-track.
   HIDE FRAME frm-track.
   RETURN. */
