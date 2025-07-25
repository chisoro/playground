DEF SHARED VAR Uname       AS CHAR NO-UNDO.
DEF SHARED VAR Upassword   AS CHAR NO-UNDO.
DEF SHARED VAR itry           AS INTEGER NO-UNDO.
/*def image logo FILENAME "g:\simacc\bin\logo.jpg" size-pixels 12 by 8 stretch-to-fit.
*/
DEF BUTTON btnOK     LABEL "&OK"     SIZE 15 BY 1.14 TOOLTIP "Accept And Continue".
DEF BUTTON btnCancel LABEL "&Cancel" SIZE 15 BY 1.14 TOOLTIP "Cancel And Leave".

DEF RECT rect-1 SIZE 60 BY 12 EDGE-PIXELS 2 NO-FILL.

def frame frmlogin
    /*logo                                          AT ROW 1    COL 1 */
    rect-1                                        AT ROW 2    COL 5
    UName         LABEL "User Code" FORM 'xxx'    AT ROW 6.5  COL 8
    SimUsr.Name NO-LABEL NO-TAB-STOP FORM "x(22)" AT ROW 6.5  COL 30 VIEW-AS TEXT
    Upassword LABEL "Password" BLANK              AT ROW 8.6 COL 8.8
    btnok AT ROW 12 COL 8 SPACE(22)
    btncancel
    SKIP 
  WITH THREE-D NO-BOX SIDE-LABELS SIZE 70 BY 14 CENTERED.

/*******************Triggers***************/
ON 'leave':U OF Uname IN FRAME frmlogin 
DO:
    FIND FIRST simusr WHERE simusr.usercode = Uname:SCREEN-VALUE EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE simusr THEN DO:
        DISPLAY simusr.NAME WITH FRAME frmlogin.
        ASSIGN uname = Uname:SCREEN-VALUE.
        APPLY 'tab' TO Uname.
    END.
    ELSE IF NOT AVAILABLE simusr AND iTry <= 3 THEN DO:
        iTry = iTry + 1.
       MESSAGE "User Code does not exist, please try again" VIEW-AS ALERT-BOX.
       CLEAR FRAME frmlogin.
       APPLY 'BACK-TAB' to upassword.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        MESSAGE "Too many attempts Try again later" VIEW-AS ALERT-BOX.
        QUIT.
    END.
        
END.

ON 'choose':U OF btnok IN FRAME frmlogin 
DO:
    ASSIGN upassword.
    IF simusr.txtPass = upassword THEN DO:
        HIDE FRAME frmlogin.
        APPLY 'close'  TO THIS-PROCEDURE IN FRAME frmlogin.
        RETURN.
    END.
    ELSE IF simusr.txtPass <> upassword AND iTry <= 3 THEN DO:
        iTry = iTry + 1.
        MESSAGE "UserCode Password pair is invalid please try again" VIEW-AS ALERT-BOX.
        CLEAR FRAME frmlogin.
       APPLY 'BACK-TAB' to upassword.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        MESSAGE "Too many attempts Try again later" VIEW-AS ALERT-BOX.
        QUIT.
    END.
        
END.

/********** MAIN LOGIC **********/
VIEW FRAME frmlogin.
itry = 1.
ENABLE ALL WITH FRAME frmlogin.
WAIT-FOR CHOOSE OF btncancel OR close of THIS-PROCEDURE IN FRAME frmlogin.
HIDE FRAME  frmlogin.
RETURN.


