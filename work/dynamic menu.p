DEFINE VARIABLE  MENU-BAR-C-Win AS HANDLE NO-UNDO.
DEFINE VARIABLE hSubMenu AS HANDLE NO-UNDO.
DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.
DEFINE VARIABLE h1SubMenu AS HANDLE NO-UNDO.
DEF VAR wsprg AS CHAR.
def var cType as char.

CREATE MENU  MENU-BAR-C-Win.
CREATE SUB-MENU hSubMenu
    ASSIGN PARENT = MENU-BAR-C-Win
    LABEL = "File".
CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = "Change Password"
    TRIGGERS:
        ON CHOOSE
            APPLY 'close' TO THIS-PROCEDURE.
    END TRIGGERS.
CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = "LOG-OFF"
    TRIGGERS:
        ON CHOOSE
            APPLY 'close' TO THIS-PROCEDURE.
    END TRIGGERS.

FOR EACH men:
    if men.cType = "Menu-item" then do:
       find first usrmen where usrmen.usr = "1" and men.cItem = usrmen.cItem  no-error.
        if available usrmen THEN DO:
            IF level = 1 THEN DO:
                CREATE value(men.cType) hMenuItem
                ASSIGN PARENT = hSubMenu
                LABEL = men.cItem
                SENSITIVE = TRUE
                TRIGGERS:
                    ON CHOOSE do:
                        FIND FIRST men WHERE men.cItem = SELF:LABEL NO-ERROR.
                       wsprg = men.prog.
                       RUN VALUE(wsprg).
                    END.
                END TRIGGERS.
            END.
            ELSE IF level = 2 THEN DO:
                CREATE value(men.cType) hMenuItem
                ASSIGN PARENT = h1SubMenu
                LABEL = men.cItem
                SENSITIVE = TRUE
                TRIGGERS:
                    ON CHOOSE do:
                        FIND FIRST men WHERE men.cItem = SELF:LABEL NO-ERROR.
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
                SENSITIVE = FALSE.
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


/*CREATE SUB-MENU hSubMenu
    ASSIGN PARENT = hMenu
    LABEL = "Cashbook".
FOR EACH men WHERE subMenu = "Cashbook":
    CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = men.cItem
    TRIGGERS:
        ON CHOOSE DO:
           FIND FIRST men WHERE men.cItem = SELF:LABEL NO-ERROR.
            wsprg = men.prog.
           RUN VALUE(wsprg).
        END.
    END TRIGGERS.
END.
*/

CURRENT-WINDOW:MENUBAR =  MENU-BAR-C-Win.
RUN enable_UI.

PROCEDURE enable_UI :
 VIEW CURRENT-WINDOW.
 WAIT-FOR CLOSE OF THIS-PROCEDURE.
END PROCEDURE.
