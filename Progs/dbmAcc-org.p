/* Program.................dbmAcc.p
   Notes:...... User menu allocation
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = YES.

DEF VAR wsUser LIKE simusr.usercode.
DEF BUTTON btnClose LABEL "Close".
DEF BUTTON btnOk LABEL "OK".
DEF BUTTON btnCancel LABEL "CANCEL".
DEF BUTTON btn-Add   LABEL "ADD>".
DEF BUTTON btn-Remove   LABEL "<REMOVE".
DEF VAR wsName LIKE simusr.Name.


DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 81 by 2.3.

DEF  QUERY qry-MenuItem FOR men SCROLLING.
DEF BROWSE brw-MenuItem QUERY qry-MenuItem
    DISPLAY men.cItem LABEL "AVAILABLE Options" FORM "X(30)" WITH 20 DOWN SEPARATORS.

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
    btn-Add      AT ROW 8 COL 40
    btn-Remove   AT ROW 10 COL 40
    brw-MenuItem AT ROW 1 COL 5
    brw-UserItem AT ROW 1 COL 52
    skip(1.5)  
    RECT-1 AT ROW 17.5 COL 5
    btnclose AT ROW 18 COL 39
    with SIZE 89 BY 20.2  view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED 
        TITLE "USER MENU MANAGEMENT FOR USER:-" + wsName.

ON CHOOSE OF btn-Add 
DO:
    GET CURRENT qry-MenuItem  NO-LOCK NO-WAIT.
    CREATE usrmen.
    ASSIGN usrmen.usr = wsUser
           usrmen.cItem = men.cItem.
    OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser.
    RETURN.
END.

ON CHOOSE OF btn-Remove 
DO:
    GET CURRENT qry-UserItem  EXCLUSIVE-LOCK NO-WAIT.
    DELETE usrmen.
    OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser.
    RETURN.
END.

ON 'enter':U OF wsUser IN FRAME frm-user
    OR CHOOSE OF btnOk IN FRAME frm-user
DO:
    ASSIGN wsUser.
    FIND FIRST simusr WHERE simusr.usercode = wsUser NO-ERROR.
    IF NOT AVAILABLE simusr THEN DO:
        MESSAGE "Invalid USER ENTERED" VIEW-AS ALERT-BOX.
        APPLY 'close' TO THIS-PROCEDURE.
        HIDE FRAME frm-Access.
        RETURN.
    END.
    ELSE DO:
        HIDE FRAME frm-user.
        wsName = simusr.Name.
        OPEN QUERY qry-MenuItem FOR EACH men WHERE men.prog <> "".
        OPEN QUERY qry-UserItem FOR EACH Usrmen WHERE usrmen.usr = wsUser.
        ENABLE ALL WITH FRAME frm-Access.
        APPLY 'ENTRY' TO btnClose.
        WAIT-FOR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frm-Access.
        CLOSE QUERY qry-MenuItem.
        CLOSE QUERY qry-UserItem.
        HIDE FRAME frm-Access.
        RETURN.
    END.
    APPLY 'close' TO THIS-PROCEDURE.
END.

/********** MAIN LOGIC **********/
 ENABLE ALL WITH FRAME frm-user.
APPLY 'ENTRY' TO wsUser IN FRAME frm-user.
WAIT-FOR CLOSE OF THIS-PROCEDURE 
     OR CHOOSE OF btnCancel IN FRAME frm-user
     OR CHOOSE OF btnOk IN FRAME frm-user
     OR 'enter' OF wsUser IN FRAME frm-user.
  HIDE FRAME frm-user.



