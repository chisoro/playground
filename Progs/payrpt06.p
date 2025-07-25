SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt06.p
    Notes:...... ..........Staff listing Report
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF NEW SHARED VAR varUser LIKE simusr.usercode INITIAL 1.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-" EXTENT 3.
DEF VAR wsTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT.
DEF VAR wsPer  AS INT FORM "999999".
DEF VAR wsCo AS CHAR FORM "x(100)".
DEF VAR wsAge AS INT.
DEF VAR wsStatus AS CHAR FORM "x(15)".
DEF VAR wsTitle AS CHAR FORM "x(90)".
DEF VAR stDate  AS DATE.
DEF VAR enDate  AS DATE.
DEF VAR wsOpt AS INT.

DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "PRINT".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 10.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
 DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 5 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
     brw-Sys AT ROW 2 COL 5
     skip(1.5)
     btn-ok colon 5 LABEL "OK"
     btn-close colon 60 SKIP(1)
     with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

 DEFINE FRAME frm-Paysys
     SKIP(1.5)
     btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     wsOpt     COLON 26 LABEL "Select Report" 
                VIEW-AS COMBO-BOX LIST-ITEM-PAIRS "SEX AND GRADE REPORT",1, "DATE OF BIRTH AND EMPLOYMENT DATE REPORT",2, 
                "DATE OF BIRTH AND AGE REPORT",4, "GRADE AND SALARY REPORT",3 
                SIZE 60 BY 2 SKIP(7.5)
     
     btn-ok colon 20
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 12.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 15 KEEP-TAB-ORDER
     TITLE "STAFF TERMINATIONS/SUSPENSIONS REPORT" VIEW-AS DIALOG-BOX.
 
FORM 
    payemf.empcode LABEL "NUMBER"
    payemf.SName   LABEL "SURNAME"
    payemf.FName   LABEL "FIRSTNAME"
    payemf.IDNo    LABEL "ID NUMBER"
    payemf.sex     LABEL "SEX"
    payemf.Grade   LABEL "GRADE" FORM "zz9"
      HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) wsTitle SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX  FRAME frm-rpt.
FORM 
    payemf.empcode LABEL "NUMBER"
    payemf.SName   LABEL "SURNAME"
    payemf.FName   LABEL "FIRSTNAME"
    payemf.IDNo    LABEL "ID NUMBER"
    payemf.DOB       LABEL "DATE OF BIRTH"
    payemf.StartDate LABEL "EMPLOYMENT DATE" 
      HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) wsTitle SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt1.

FORM 
    payemf.empcode LABEL "NUMBER"
    payemf.SName   LABEL "SURNAME"
    payemf.FName   LABEL "FIRSTNAME"
    payemf.IDNo    LABEL "ID NUMBER"
    payemf.sex     LABEL "SEX"
    payemf.Grade   LABEL "GRADE" FORM "zz9"
    payemf.Notch  LABEL "SALARY"
     HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) wsTitle SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX  FRAME frm-rpt2.

FORM 
    payemf.empcode LABEL "NUMBER"
    payemf.SName   LABEL "SURNAME"
    payemf.FName   LABEL "FIRSTNAME"
    payemf.IDNo    LABEL "ID NUMBER"
    payemf.sex     LABEL "SEX"
    payemf.DOB     LABEL "DATE OF BIRTH"
    wsAge          LABEL "AGE" 
      HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) wsTitle SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX  FRAME frm-rpt3.

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
    DISPLAY Paysys.Paysys @ wsSys WITH FRAME frm-Paysys.
    RETURN. 
 END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
     OR 'tab':U OF wsSys IN FRAME frm-Paysys
 DO:
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
             DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
             ASSIGN wsOpt:SCREEN-VALUE = "1"
                    /*wsFilter:SCREEN-VALUE = "0"
                    stDate:SCREEN-VALUE = "01/01/" + STRING(YEAR(CurDate))
                    enDate:SCREEN-VALUE = STRING(CurDate) */.
         END.
         ELSE DO:
             MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
         END.
     END.  
     ELSE DO:
         MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
 END.

 /*ON 'enter':U OF wsOpt IN FRAME frm-Paysys
     OR 'tab' OF wsOpt IN FRAME frm-Paysys
 DO:
     IF INT(wsOpt:SCREEN-VALUE) = 2  THEN
         DISABLE wsFilter WITH FRAME frm-Paysys.
     ELSE
         ENABLE wsFilter WITH FRAME frm-Paysys.
     RETURN.
 END. */

 ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
 DO:
    ASSIGN wsSys = INT(wsSys:SCREEN-VALUE)
            wsOpt.
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 

 END.
 /********** MAIN LOGIC **********/
 FIND FIRST simctr NO-LOCK NO-ERROR.
 wsCo = SIMCTR.CONAME.
 VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
 ENABLE ALL WITH FRAME frm-Paysys.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
 HIDE FRAME frm-Paysys.
 RETURN.
     
PROCEDURE Report.ip:
    CASE wsOpt:
        WHEN 1 THEN
            RUN Rpt.ip.
        WHEN 2 THEN
            RUN Rpt1.ip.
        WHEN 3 THEN
            RUN Rpt2.ip.
        WHEN 4 THEN
            RUN Rpt3.ip.
    END CASE.
END.

PROCEDURE Rpt.ip:
    wsTitle = "STAFF LISTING WITH SEX AND GRADE BY DEPARTMENT".
    FOR EACH payemf WHERE Paysys = wssys AND estatus <> 4 NO-LOCK BREAK  BY dept BY empcode :
        IF FIRST-OF(dept) THEN DO:
            FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-ERROR.
            DISPLAY STREAM a paydept.descrip @ payemf.Sname WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        DISPLAY STREAM a payemf.empcode payemf.SName payemf.FName payemf.IDNo 
            payemf.sex payemf.Grade WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
END.

PROCEDURE Rpt1.ip:
    wsTitle = "STAFF LISTING WITH SEX, DATE OF BIRTH AND EMPLOYMENT DATE BY DEPARTMENT".
    FOR EACH payemf WHERE Paysys = wssys AND estatus <> 4 NO-LOCK BREAK  BY dept BY empcode :
        IF FIRST-OF(dept) THEN DO:
            FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-ERROR.
            DISPLAY STREAM a paydept.descrip @ payemf.Sname WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
        DISPLAY STREAM a payemf.empcode payemf.SName payemf.FName payemf.IDNo 
            payemf.DOB payemf.startdate WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.
    END.
END.

PROCEDURE Rpt2.ip:
    wsTitle = "STAFF LISTING WITH GRADE AND SALARY BY DEPARTMENT".
    FOR EACH payemf WHERE Paysys = wssys AND estatus <> 4 NO-LOCK BREAK  BY dept
        BY payemf.Grade BY payemf.Notch :
        IF FIRST-OF(dept) THEN DO:
            FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-ERROR.
            DISPLAY STREAM a paydept.descrip @ payemf.Sname WITH FRAME frm-rpt2.
            DOWN STREAM a WITH FRAME frm-rpt2.
        END.
        DISPLAY STREAM a payemf.empcode payemf.SName payemf.FName payemf.IDNo 
            payemf.sex payemf.Grade payemf.Notch WITH FRAME frm-rpt2.
        DOWN STREAM a WITH FRAME frm-rpt2.
    END.
END.

PROCEDURE Rpt3.ip:
    wsTitle = "STAFF LISTING WITH SEX, DATE OF BIRTH AND AGE BY DEPARTMENT".
    FOR EACH payemf WHERE Paysys = wsSys AND estatus <> 4 NO-LOCK BREAK  BY dept
        BY payemf.DOB:
        IF FIRST-OF(dept) THEN DO:
            FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-ERROR.
            DISPLAY STREAM a paydept.descrip @ payemf.Sname WITH FRAME frm-rpt3.
            DOWN STREAM a WITH FRAME frm-rpt3.
        END.
        wsAge = YEAR(TODAY) - YEAR(payemf.DOB).
        DISPLAY STREAM a payemf.empcode payemf.SName payemf.FName payemf.IDNo 
            payemf.sex payemf.dob wsAge WITH FRAME frm-rpt3.
        DOWN STREAM a WITH FRAME frm-rpt3.
    END.
END.
