session:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrate.p
   Notes:...... Update Payroll Rates
   Author:.................S. Mawire
*/
DEF VAR varUser LIKE simusr.usercode initial "1".
DEF VAR X AS INT.
DEF VAR wsNotch AS DEC.
DEF VAR wsDate AS DATE.
DEF VAR wsAmt LIKE paymtf.curAmt.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsName AS CHAR FORM "x(70)".
DEF VAR wsEmp LIKE payemf.empcode.
DEF VAR wsitem LIKE payitem.itemcode.
DEF VAR wsDesc AS CHAR FORM "x(40)".
DEF VAR wsfrm AS CHAR FORM "x(40)".
DEF VAR wsTitle AS char FORM "x(80)".
DEF VAR wsopt as int LABEL "SELECT Option"
          VIEW-AS RADIO-SET RADIO-BUTTONS "USD Rates Update", 1,
                                          "Salary Scale Rates Update",2.
DEF VAR wsCur LIKE tblforex.txtCur INITIAL "USD".


DEF BUTTON btn-item LABEL "ITEM CODE".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-Emp   LABEL "EMPLOYEE".

DEF RECT rec  SIZE 70 BY 6.
DEF RECT rec1 SIZE 70 BY 2.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
        DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brw-Sys AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE FRAME frm-Paysys
    SKIP(0.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(1)
    Paysys.CurPer  COLON 25 LABEL " Current Period   " VIEW-AS TEXT SKIP(1)
    Paysys.CurDate COLON 25 LABEL "Payroll Date   " VIEW-AS TEXT SKIP(2)
    btn-ok colon 10
    btn-close colon 50
    rec AT ROW 1.2     COL 2
    rec1 AT ROW 7.4   COL 2
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D SIZE 73 BY 10 
    TITLE "PAYROLL SELECTION" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-Data
    SKIP(0.5)
    wsCur label "Salary Scale is Based on" VIEW-AS TEXT colon 50 skip(2)
    /*wsopt COLON 20 skip(1)*/ SKIP(2)
    payemf.empcode colon 20 label "Processing......" view-as text  payemf.notc COLON 50 VIEW-AS TEXT skip(1.2)
    btn-ok colon 10
    btn-close colon 50
    rec AT ROW 1.2     COL 2
    rec1 AT ROW 7.4   COL 2
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D SIZE 73 BY 10
    TITLE "Payroll Rates Update" VIEW-AS DIALOG-BOX.

/* Triggers for the Paysys frame */
ON CHOOSE OF btn-sys IN FRAME frm-Paysys
    OR CHOOSE OF btn-sys IN FRAME frm-Paysys
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-sys
          OR 'mouse-select-dblclick' OF brw-sys.
  CLOSE QUERY qry-sys.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsSys.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-sys
    OR 'mouse-select-dblclick' OF brw-sys
DO: 
   GET CURRENT qry-sys NO-LOCK NO-WAIT.
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
   ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
   APPLY 'tab' TO wsSys.
   RETURN. 
END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
    OR 'tab':U OF wsSys IN FRAME frm-Paysys
    OR 'leave':U OF wsSys IN FRAME frm-Paysys
DO:
    ASSIGN wsSys.
    IF INT(wsSys:SCREEN-VALUE) = 0 THEN
        APPLY 'close' TO THIS-PROCEDURE.
    ELSE DO:
        FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE paysys THEN DO:
           FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
           IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
           DO:
              DISPLAY Paysys.Descrip  Paysys.CurDate Paysys.CurPer WITH FRAM frm-Paysys.
              ENABLE btn-ok WITH FRAME frm-paysys.
              APPLY 'entry' TO btn-ok IN FRAME frm-paysys.
              RETURN NO-APPLY.
           END.
           ELSE DO:
              MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
              DISABLE btn-ok WITH FRAME frm-paysys.
              RETURN NO-APPLY.
           END.
        END.  
        ELSE DO:
            MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    END.
     RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
    HIDE FRAME frm-Paysys.
    VIEW FRAME frm-Data .
    wsCur:SCREEN-VALUE IN FRAME FRM-dATA = WScUR.
    ENABLE ALL WITH FRAME frm-Data.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Data.
    hide frame frm-Data.
    APPLY 'close' TO THIS-PROCEDURE.
    return.
END.

ON 'choose':U OF btn-Ok in frame frm-Data 
DO:
    assign /*wsOpt*/
           wsCur = wsCur:screen-value.
    /*if wsOpt = 1 then do:
        wsCur = "USD".
        find first tblforex where tblforex.txtcur = wsCur no-lock no-error.
        if available tblForex then do:
            for each payemf where payemf.paysys = paysys.paysys no-lock:
                display payemf.empcode with fram frm-Data.
                pause 0.
                for each paymtf where USDAmt <> 0 and paymtf.empcode = payemf.empcode:
                    assign stdAmt = round((USDAmt * tblforex.decRate),2).
                end.
                run basic-ip.
            end.
        end.
        message "Rate update complete...." view-as alert-box.
    end.
    else if wsOpt = 2 then do:
       RUN Scale-ip.
       message "Salary scale Rate update complete...." view-as alert-box.
    end.*/
    RUN Scale-ip.
       message "Salary scale Rate update complete...." view-as alert-box.
   RETURN.
END.

/********** MAIN LOGIC **********/
FIND FIRST payctr NO-LOCK NO-ERROR.
VIEW FRAME frm-Paysys.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
RETURN.

PROCEDURE Scale-ip:
    if wsCur = "USD" THEN DO:
         find first tblforex where tblforex.txtcur = wsCur no-lock no-error.
        message wsCur tblforex.decRate view-as alert-box .
    END.
       
    for each payemf where payemf.paysys = paysys.paysys no-lock:
        display payemf.empcode payemf.notc with FRAME frm-Data.
        pause 0.
        find first payscale where Payscale.Scale = payemf.scale no-lock no-error.
        FIND FIRST paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.itemcode = Payctr.Basic no-error.
        if available paymtf then do:
            if wsCur = "USD" then
            assign paymtf.USDAmt = payscale.notch[payemf.notc]
                  /* paymtf.stdAmt = paymtf.USDAmt * tblForex.decRate*/
                    paymtf.stdAmt = paymtf.USDAmt.
            else 
                assign paymtf.USDAmt = 0
                       paymtf.STDAmt = payscale.notch[payemf.notc].
           run basic-ip.
        end.
    end.
END.

Procedure Basic-ip:
    FIND FIRST paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.itemcode= Payctr.Basic no-lock no-error.
        if available paymtf then do:
            wsAmt = paymtf.stdAmt.
            DO X = 1 TO 3: /*Overtime items */
                   IF Payctr.overtime[X] <> 0 THEN DO:
                       FIND FIRST paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.itemcode = Payctr.overtime[X] NO-ERROR.
                        IF NOT AVAILABLE paymtf THEN DO:
                           CREATE paymtf.
                           ASSIGN paymtf.empcode = payemf.empcode
                                  Paymtf.ACTIVE   = 1
                                  Paymtf.Itemcode = Payctr.overtime[X].
                        END.
                        Paymtf.StdAmt   = wsAmt / payemf.stdhours * Payctr.ORate[X]. 
                   END.
            END.
        end.
end procedure.

