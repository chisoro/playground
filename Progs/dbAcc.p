/* Program.................dbmAcc.p
   Notes:...... User menu allocation
   Author:.................S. Mawire
   Edited by:..............S. Chisoro
*/

SESSION:DATA-ENTRY-RETURN = YES.

DEF VAR wsUser LIKE simusr.usercode INITIAL "1".
DEF BUTTON btnClose LABEL "Close".
DEF BUTTON btnOk LABEL "OK".
DEF BUTTON btnCancel LABEL "CANCEL".
DEF BUTTON btn-Add   LABEL "ADD>".
DEF BUTTON btn-Remove   LABEL "<REMOVE".
DEF VAR wsName LIKE simusr.Name.
DEF VAR wsFiller AS CHAR.
DEF VAR wsmn AS  CHAR.
DEF VAR wsMen AS CHAR.
DEF VAR X AS INT.

DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEFINE  SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-host AS CHAR NO-UNDO.

DEF TEMP-TABLE mMenu
    FIELD txtcode AS CHAR
    FIELD txtPnt AS DEC
    FIELD txtType AS CHAR /* menu, sub-menu item */
    FIELD txtMenu AS CHAR
    /*FIELD txtSubM AS CHAR FORM "X(20)" */
    FIELD txtItem AS CHAR FORM "X(40)".
    
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 81 by 2.3.

DEF  QUERY qry-MenuItem FOR mMenu SCROLLING.
DEF BROWSE brw-MenuItem QUERY qry-MenuItem
    DISPLAY mMenu.txtItem LABEL "AVAILABLE Options" FORM "X(30)" WITH 20 DOWN SEPARATORS.

DEF  QUERY qry-UserItem FOR Usrmen SCROLLING.
DEF BROWSE brw-UserItem QUERY qry-UserItem
    DISPLAY Usrmen.cItem  LABEL "ASSIGN Options" FORM "X(30)" WITH 20 DOWN SEPARATORS.

DEF FRAME frm-user
    skip(1)
     space(5)
    wsUser LABEL "Enter User Code to Manage" SPACE(10)
     btnOK SPACE(5) btnCancel
     RECT-1         at row 1.5 col 2
    with view-as dialog-box keep-tab-order 
         side-labels no-underline three-d scrollable .

DEFINE FRAME frm-Access 
     SKIP(0.5)
     wsUser      COLON 20 LABEL "Enter USER CODE" 
     simusr.NAME NO-LABEL VIEW-AS TEXT
     brw-MenuItem AT ROW 3 COL 5
     brw-UserItem AT ROW 3 COL 52
     btn-Add      AT ROW 8 COL 40
     btn-Remove   AT ROW 10 COL 40
    skip(1.5)  
    RECT-1 AT ROW 19.5 COL 5
     btnOK AT ROW 20 COL 15 SPACE(40) btnCancel
    with SIZE 89 BY 22.2  view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED 
        TITLE "USER MENU MANAGEMENT".

ON 'enter':U OF wsUser IN FRAME frm-Access
   OR  'LEAVE':U OF wsUser IN FRAME frm-Access
DO:
    ASSIGN wsUser.
    IF wsUser = "" OR wsuser = "0" THEN DO:
       MESSAGE " NOT valid Usercode...System exiting" VIEW-AS ALERT-BOX.
       APPLY 'close' TO THIS-PROCEDURE.
       HIDE FRAME frm-Access. 
    END.
    ELSE DO:
        FIND FIRST simusr WHERE simusr.usercode = wsUser  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE simusr THEN DO:
            MESSAGE "Invalid USER ENTERED" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            wsName = simusr.Name.
            OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser  NO-LOCK.
            ENABLE ALL WITH FRAME frm-Access.
            DISPLAY wsName @ simusr.NAME WITH FRAME frm-Access.
            APPLY 'ENTRY' TO wsUser.
            RETURN.
        END.
    END.
    RETURN.
END.

ON CHOOSE OF btn-Add 
DO:
    RUN simip.p.
    GET CURRENT qry-MenuItem  NO-LOCK NO-WAIT.
    CASE mMenu.txtType:
        WHEN "MENU" THEN DO:
            wsMn = mMenu.txtCode.
            FOR EACH mMenu WHERE mMenu.txtMenu = wsMn AND mMenu.txtType = "MENU-ITEM":
                IF NOT CAN-FIND (FIRST usrMen WHERE usrMen.Usr = wsUser AND usrMen.cItem = TRIM(mMenu.txtItem)) THEN DO:
                    CREATE usrmen.
                    ASSIGN usrmen.usr = wsUser
                           usrmen.cItem = TRIM(mMenu.txtItem)
                            usrmen.DeviceName = lc-host
                            usrmen.UID = varUser
                             usrmen.creDate =NOW. 
                END.  
                OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser  NO-LOCK.
            END. 
        END.
        WHEN "SUB-MENU" THEN DO:
            ASSIGN wsMn = mMenu.txtCode.
            FOR EACH mMenu WHERE STRING(mMenu.txtPnt) = wsMn AND mMenu.txtType = "MENU-ITEM":
                IF NOT CAN-FIND (FIRST usrMen WHERE usrMen.Usr = wsUser AND usrMen.cItem = TRIM(mMenu.txtItem)) THEN DO:
                    CREATE usrmen.
                    ASSIGN usrmen.usr = wsUser
                           usrmen.cItem = TRIM(mMenu.txtItem)
                            usrmen.DeviceName = lc-host
                            usrmen.UID = varUser
                             usrmen.creDate =NOW.
                END.  
            END.
            OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser  NO-LOCK.
        END.
        OTHERWISE DO:
          IF NOT CAN-FIND (FIRST usrMen WHERE usrMen.Usr = wsUser AND usrMen.cItem = TRIM(mMenu.txtItem)) THEN DO:
              CREATE usrmen.
              ASSIGN usrmen.usr = wsUser
                     usrmen.cItem = TRIM(mMenu.txtItem).
          END.
          OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser NO-LOCK.
        END.
    END CASE.
    RETURN.
END.

ON CHOOSE OF btn-Remove 
DO:
    GET CURRENT qry-UserItem  EXCLUSIVE-LOCK NO-WAIT.
    DELETE usrmen.
    OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser  NO-LOCK.
    RETURN.
END.

ON ROW-DISPLAY OF brw-MenuItem
DO:
    IF mMenu.txtType = "MENU"   THEN
        ASSIGN mMenu.txtItem:FGCOLOR IN BROWSE brw-MenuItem = 12. /* assign red colour */
    ELSE IF mMenu.txtType = "SUB-MENU"   THEN
        ASSIGN mMenu.txtItem:FGCOLOR IN BROWSE brw-MenuItem = 9.
END.

/********** MAIN LOGIC **********/
FOR EACH men:   
   CASE men.cType:
       WHEN "MENU" THEN DO:
        wsMen =  men.submenu.
        CREATE mMenu.
        ASSIGN mMenu.txtcode  = men.submenu
               mMenu.txtItem = men.cItem
               mMenu.txtType  = men.cType
               mMenu.txtPnt   = 0
               mMenu.txtMenu  = wsMen.
       END.
       WHEN "SUB-MENU" THEN DO:
       wsFiller = "".
       DO X = 1 TO (INT(level) * 5):
          wsFiller = wsFiller + " ".
       END.
        CREATE mMenu.
        ASSIGN mMenu.txtcode  = men.submenu
               mMenu.txtItem = wsFiller + men.cItem
               mMenu.txtType  = men.cType
               mMenu.txtPnt   = DEC(SUBSTR(submenu,1,INT(level) + 1))
               mMenu.txtMenu  = wsMen.
       END.
       OTHERWISE DO:
             wsFiller = "".
            DO X = 1 TO (INT(level) * 5):
              wsFiller = wsFiller + " ".
            END.
            CREATE mMenu.
            ASSIGN mMenu.txtcode  = men.submenu
                   mMenu.txtItem = wsFiller + men.cItem
                   mMenu.txtType  = men.cType
                   mMenu.txtPnt   = DEC(SUBSTR(submenu,1,INT(level) + length(wsMen)))
                   mMenu.txtMenu  = wsMen.
       END.
   END.
END.
ENABLE ALL WITH FRAME frm-Access.
APPLY 'ENTRY' TO wsUser IN FRAME frm-Access.
OPEN QUERY qry-MenuItem FOR EACH mMenu.
WAIT-FOR CLOSE OF THIS-PROCEDURE 
     OR CHOOSE OF btnCancel IN FRAME frm-Access
     OR CHOOSE OF btnOk IN FRAME frm-Access.
HIDE FRAME frm-Access.
