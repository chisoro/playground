session:DATA-ENTRY-RETURN = TRUE.
/* Program.................GLMF.p
   Notes:.................Ledger file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Ledger file already exist"
&SCOPED-DEFINE wsTitle           "Ledger Account File Maintenance"
&SCOPED-DEFINE tmptable             glmf
&SCOPED-DEFINE skey                 glmf.acct
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Dept ~
                                        COLUMN-LABEL ' Department ':C ~
                              bfr{&tmptable}.ACCTYPE ~
                                        COLUMN-LABEL ' TYPE ':C ~
                              bfr{&tmptable}.DrCr ~
                                        COLUMN-LABEL ' Normal Balance ':C ~
                              bfr{&tmptable}.Cat ~
                                        COLUMN-LABEL ' Category ':C ~
                              bfr{&tmptable}.fund ~
                                        COLUMN-LABEL ' Fund/Segment ':C ~
                              bfr{&tmptable}.Clas ~
                                        COLUMN-LABEL ' Class':C ~
                              bfr{&tmptable}.SubCat ~
                                        COLUMN-LABEL ' Sub-Category ':C ~
                              bfr{&tmptable}.Proj ~
                                        COLUMN-LABEL ' Category ':C ~
                              bfr{&tmptable}.DESCRIPTION ~
                                       COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.ACCT ~
                                        COLUMN-LABEL 'ACCOUNT ':C ~
                              bfr{&tmptable}.DESCRIPTION ~
                                        COLUMN-LABEL ' DESCRIPTION ':C ~
                              bfr{&tmptable}.ACCTYPE ~
                                        COLUMN-LABEL 'TYPE ':C ~
                              bfr{&tmptable}.Clas ~
                                        COLUMN-LABEL 'CLASS ':C ~
                              bfr{&tmptable}.Cat ~
                                        COLUMN-LABEL 'CATEGORY ':C~
                              bfr{&tmptable}.fund ~
                                        COLUMN-LABEL 'FUND/!SEGMENT ':C~
                              bfr{&tmptable}.dept ~
                                        COLUMN-LABEL ' DEPT ':C
                              
                              
&SCOPED-DEFINE dispFields      bfr{&tmptable}.ACCTYPE ~
                                        COLUMN-LABEL ' Type ':C ~
                              bfr{&tmptable}.cat ~
                                        COLUMN-LABEL ' Category ':C ~
                              bfr{&tmptable}.DrCr ~
                                        COLUMN-LABEL ' Normal Balance ':C ~
                              bfr{&tmptable}.Clas ~
                                        COLUMN-LABEL ' Class ':C ~
                              bfr{&tmptable}.Proj ~
                                        COLUMN-LABEL ' Project ':C ~
                             bfr{&tmptable}.SubCat ~
                                        COLUMN-LABEL ' Sub-Category ':C ~
                              bfr{&tmptable}.Dept ~
                                        COLUMN-LABEL ' Department ':C ~
                              bfr{&tmptable}.Fund ~
                                        COLUMN-LABEL ' Fund ':C ~
                             bfr{&tmptable}.DESCRIPTION ~
                                        COLUMN-LABEL ' Description ':C

{varlibrary.i}

DEF VAR wscat LIKE glcat.cat.
DEF VAR wsbal AS CHAR.
DEF VAR frm AS CHAR.
DEF BUTTON btnClass LABEL "CLASS".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 111 by 2.3.
DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 111 by 19.5.



DEF    QUERY qry-Pickcat FOR glcat SCROLLING.
DEF BROWSE brw-Pickcat QUERY qry-Pickcat
    DISPLAY GLCAT.CAT GLCAT.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickCat 
    brw-Pickcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEF    QUERY qry-Pickclass FOR glclass SCROLLING.
DEF BROWSE brw-Pickclass QUERY qry-Pickclass
    DISPLAY glClass.Clas glclass.DESCRIPTION COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickClass 
    brw-Pickclass AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEF    QUERY qry-Picksubcat FOR glsubcat SCROLLING.
DEF BROWSE brw-Picksubcat QUERY qry-Picksubcat
    DISPLAY GLsubCAT.subCAT GLsubCAT.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Picksubcat 
    brw-Picksubcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Sub-Category Selection".


DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 21.7 COL 10
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 21 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 116 BY 24.5
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
     {&skey}                   colon 30 label "Account" SKIP(0.5)
    bfr{&tmptable}.ACCTYPE     colon 30 LABEL "Ledger Type"
     view-as combo-box size 30 by 4 list-item-pairs
      "BS - BALANCE SHEET","BS","I - INCOME","I", "E - EXPENDITURE","E" SKIP(0.5)
    btnFund         colon 11 no-tab-stop
    bfr{&tmptable}.fund        colon 30   no-label
    glfund.descrip             view-as text no-label NO-TAB-STOP SKIP(0.5)
    btnDept          colon 7 no-tab-stop
    bfr{&tmptable}.Dept        colon 30   no-label
    GLDept.DESCRIP             view-as text no-label NO-TAB-STOP SKIP(0.5)
    bfr{&tmptable}.SUBVOTE view-as text colon 30 LABEL "Sub-Vote" no-tab-stop
    GLSUBVOTE.DESCRIP view-as text no-label NO-TAB-STOP SKIP(0.5)
    bfr{&tmptable}.ITEM view-as text colon 30 LABEL "Item"     NO-TAB-STOP SKIP(0.5)
    btnCLass          colon 20 no-tab-stop
    bfr{&tmptable}.Clas       NO-LABEL 
    GLClas.DESCRIP              view-as text no-label NO-TAB-STOP SKIP(0.5)
    btnCat          colon 15 no-tab-stop
    bfr{&tmptable}.Cat NO-LABEL 
    GLCat.DESCRIP             view-as text no-label NO-TAB-STOP SKIP(0.5)
    btnSubCat          colon 11 no-tab-stop
    bfr{&tmptable}.SubCat NO-LABEL 
    GLsubCat.DESCRIP          view-as text no-label NO-TAB-STOP SKIP(0.5)
    bfr{&tmptable}.description colon 29 label "Description "  SKIP(0.5)
    bfr{&tmptable}.DrCr             colon 29 LABEL "Normal Account Balance"
                    view-as combo-box list-item-pairs
                          "1 - Credit",1,"2 - Debit",2 SKIP(0.5)
    btnProj                    colon 16 no-tab-stop
    bfr{&tmptable}.Proj           no-label
    glProj.DESCRIP             view-as text no-label NO-TAB-STOP SKIP(0.5)
    skip(1.5)
    btn-ok colon 5
    btn-close colon 60
    SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LEDGER CODE CAPTURE".
    

/* ***** Triggers for the main frame **** */

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw-{&tmptable}
        OR 'mouse-select-dblclick' OF brw-{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        DISABLE {&skey} WITH FRAME frm-input.
        run proc-edit.
        RETURN.
END.
ON "ESC" ANYWHERE
DO:
   APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
END.

ON 'enter':U OF {&skey} IN FRAME frm-input
    OR 'tab':U OF {&skey} IN FRAME frm-input
DO: 
     wsid = DEC({&skey}:SCREEN-VALUE IN FRAME frm-input).
   IF wsid = 0 THEN DO:
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR.
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          RETURN NO-APPLY.
       END.
       ELSE APPLY 'tab' TO SELF.
    END.  
END.

ON 'tab':U OF bfr{&tmptable}.ACCTYPE IN FRAME frm-input
    OR 'enter' OF bfr{&tmptable}.ACCTYPE IN FRAME frm-input
DO:
    IF simctr.lForm = 1 THEN DO: /* Format 1 */
        ASSIGN bfr{&tmptable}.FUND:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE),"999999999999"),3,2) 
                  WHEN bfr{&tmptable}.FUND:SCREEN-VALUE = "".
        FIND FIRST glfund WHERE glfund.fund = int(bfr{&tmptable}.FUND:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE glfund THEN DO:
            MESSAGE "Fund " + bfr{&tmptable}.FUND:SCREEN-VALUE + " is invalid"  VIEW-AS ALERT-BOX.
            CLEAR  FRAME frm-input.
            APPLY 'BACK-TAB' to self.
            RETURN NO-APPLY.
        END.
        ELSE
            DISPLAY glfund.descrip @ glfund.descrip  WITH FRAME frm-input.
        IF bfr{&tmptable}.acctype:SCREEN-VALUE = "I" OR bfr{&tmptable}.acctype:SCREEN-VALUE = "E" THEN
              ASSIGN bfr{&tmptable}.dept:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE),"999999999999"),5,4)
                     bfr{&tmptable}.subvote:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE),"999999999999"),9,1)
                     bfr{&tmptable}.ITEM:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE),"999999999999"),10,3).
              ELSE IF bfr{&tmptable}.acctype:SCREEN-VALUE = "BS" THEN
                   ASSIGN bfr{&tmptable}.dept:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE),"999999999999"),5,3)
                          bfr{&tmptable}.subvote:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE),"999999999999"),8,2).
              FIND FIRST gldept WHERE gldept.dept = int(bfr{&tmptable}.dept:SCREEN-VALUE) NO-LOCK NO-ERROR.
              IF NOT AVAILABLE gldept THEN DO:
                  MESSAGE "Vote/Department " + bfr{&tmptable}.dept:SCREEN-VALUE + " is invalid"  VIEW-AS ALERT-BOX. 
                  CLEAR  FRAME frm-input.
                  APPLY 'BACK-TAB' to self.
                  RETURN NO-APPLY.
              END.
              ELSE 
                  DISPLAY gldept.DESCRIP @ gldept.DESCRIP WITH FRAME frm-input.             
              FIND FIRST glsubvote WHERE glsubvote.subvote = int(bfr{&tmptable}.subvote:SCREEN-VALUE) NO-LOCK NO-ERROR.
              IF NOT AVAILABLE glsubvote THEN DO:
                  MESSAGE "Sub-Vote " + bfr{&tmptable}.subvote:SCREEN-VALUE + " is invalid"  VIEW-AS ALERT-BOX.
                  CLEAR  FRAME frm-input.
                  APPLY 'BACK-TAB' to self.
                  RETURN NO-APPLY.
              END.
              ELSE 
                  DISPLAY GLsubvote.DESCRIP @ GLSUBVOTE.DESCRIP WITH FRAME frm-input.      
              FIND FIRST glitem WHERE glitem.ITEM = int(bfr{&tmptable}.ITEM:SCREEN-VALUE) NO-LOCK NO-ERROR.
              IF AVAILABLE glitem THEN DO:
                  DISPLAY glitem.descrip @ bfr{&tmptable}.DESCRIPTION WITH FRAME frm-input.
                  APPLY 'TAB' TO bfr{&tmptable}.ACCTYPE IN FRAME frm-input.
              END.
              ELSE IF NOT AVAILABLE glitem THEN NEXT.
    END.
    ELSE IF simctr.Lform = 2 THEN DO: 
        IF bfr{&tmptable}.acctype:SCREEN-VALUE = "I" THEN
               ASSIGN bfr{&tmptable}.Cat:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),simctr.Icp,simctr.Icl)
                  bfr{&tmptable}.SubCat:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),simctr.Iscp,simctr.Iscl).
        IF bfr{&tmptable}.acctype:SCREEN-VALUE = "E" THEN
               ASSIGN bfr{&tmptable}.Cat:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),simctr.Ecp,simctr.Ecl)
                  bfr{&tmptable}.SubCat:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),simctr.Escp,simctr.Escl).
        IF bfr{&tmptable}.acctype:SCREEN-VALUE = "BS" THEN
               ASSIGN bfr{&tmptable}.Cat:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),simctr.Bcp,simctr.Bcl)
                  bfr{&tmptable}.SubCat:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),simctr.Bscp,simctr.Bscl).
                  bfr{&tmptable}.clas:SCREEN-VALUE = SUBSTR(STRING(dec({&skey}:SCREEN-VALUE)),1,1).
           /*IF ( NOT CAN-FIND(FIRST glclas WHERE glclas.clas = INT(bfr{&tmptable}.Clas:SCREEN-VALUE))
              AND NOT CAN-FIND(FIRST glsubcat WHERE glsubca.subcat = INT(bfr{&tmptable}.subcat:SCREEN-VALUE))
              AND NOT CAN-FIND(FIRST glcat WHERE glcat.cat = INT(bfr{&tmptable}.Cat:SCREEN-VALUE)) )
          THEN DO:
            MESSAGE  "Invalid Ledger Code Entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-input ALL.
            RETURN NO-APPLY.
           END.
           ELSE DO: */
              FIND FIRST GLCat WHERE GLCat.Cat = int(bfr{&tmptable}.Cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
              IF NOT AVAILABLE GLCat THEN DO:
                  MESSAGE "Category " + bfr{&tmptable}.Cat:SCREEN-VALUE + " is invalid"  VIEW-AS ALERT-BOX. 
                  /*CLEAR  FRAME frm-input.
                  APPLY 'BACK-TAB' to self.
                  RETURN NO-APPLY. */
              END.
              ELSE DO:
                  DISPLAY GLCat.DESCRIP @ GLCat.DESCRIP WITH FRAME frm-input.
                  /*DISABLE btnCat bfr{&tmptable}.Cat WITH FRAME frm-input.*/ 
                  FIND FIRST GLsubCat WHERE GLsubCat.subCat = int(bfr{&tmptable}.subCat:SCREEN-VALUE) NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE GLsubCat THEN DO:
                      MESSAGE "Sub-Category " bfr{&tmptable}.SubCat:SCREEN-VALUE  " is invalid"  VIEW-AS ALERT-BOX.
                      /*CLEAR  FRAME frm-input.
                      APPLY 'BACK-TAB' to self.
                      RETURN NO-APPLY. */
                  END.
                  ELSE DO:
                      DISPLAY GLsubCat.DESCRIP @ GLsubCat.DESCRIP WITH FRAME frm-input.             
                      FIND FIRST GLsubCat WHERE GLsubCat.subCat = int(bfr{&tmptable}.subCat:SCREEN-VALUE) NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE GLsubCat THEN DO:
                        MESSAGE "Sub-Category " bfr{&tmptable}.SubCat:SCREEN-VALUE  " is invalid"  VIEW-AS ALERT-BOX.
                        /*CLEAR  FRAME frm-input.
                        APPLY 'BACK-TAB' to self.
                        RETURN NO-APPLY. */
                      END.
                      ELSE DO:
                          DISPLAY GLsubCat.DESCRIP @ GLsubCat.DESCRIP WITH FRAME frm-input.
                          /*DISABLE btnsubCat bfr{&tmptable}.subCat WITH FRAME frm-input.*/
                          FIND FIRST GLclas WHERE GLclas.clas = int(bfr{&tmptable}.clas:SCREEN-VALUE) NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE GLclas THEN DO:
                        MESSAGE "Account Clssification " bfr{&tmptable}.clas:SCREEN-VALUE  " is invalid"  VIEW-AS ALERT-BOX.
                        /*CLEAR  FRAME frm-input.
                        APPLY 'BACK-TAB' to self.
                        RETURN NO-APPLY. */
                      END.
                      ELSE 
                         DISPLAY GLclas.DESCRIP @ GLclas.DESCRIP WITH FRAME frm-input.
                         /*DISABLE btnCLass bfr{&tmptable}.clas WITH FRAME frm-input. */
                      END.
                 /* END. */
                  
              END.
           END.
    END.
   APPLY 'TAB' TO bfr{&tmptable}.ACCTYPE IN FRAME frm-input.
   RETURN.    
END.

ON row-display OF brw-glmf DO:
    IF bfr{&tmptable}.DrCr = 2 THEN
        wsbal = "Debit".
    ELSE IF bfr{&tmptable}.DrCr = 1 THEN
        wsbal= "Credit".
END.

ON 'enter':U OF bfr{&tmptable}.DrCr IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.DrCr IN FRAME frm-input
DO:
    APPLY 'tab' TO bfr{&tmptable}.DrCr.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.acct.
    FIND FIRST gltdf WHERE gltdf.acct  = wsid NO-ERROR.
    IF AVAILABLE gltdf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE gltdf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON CHOOSE OF btnDept IN FRAME frm-input
DO:
  VIEW FRAME frm-pickDept.
  OPEN QUERY qry-pickDept FOR EACH gldept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickDept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickDept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickDept
          OR CHOOSE OF btn-ok IN FRAME frm-pickDept 
          OR 'enter':u OF brw-pickDept
          OR 'mouse-select-dblclick' OF brw-pickDept.
  CLOSE QUERY qry-pickDept.
  HIDE FRAME frm-pickDept.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickDept 
    OR 'enter':u OF brw-pickDept
    OR 'mouse-select-dblclick' OF brw-pickDept
DO: 
   GET CURRENT qry-pickDept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glDept.Dept @ bfr{&tmptable}.Dept GLDept.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Dept IN FRAME frm-input 
DO:
    FIND FIRST glDept WHERE glDept.Dept = int(bfr{&tmptable}.Dept:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glDept THEN DO:
      DISPLAY glDept.Dept @ bfr{&tmptable}.Dept GLDept.DESCRIP WITH FRAME frm-input.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glDept THEN DO:
        MESSAGE "Invalid Department, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.


ON CHOOSE OF btnFund IN FRAME frm-input
DO:
  VIEW FRAME frm-PickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-PickFund
          OR CHOOSE OF btn-ok IN FRAME frm-PickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-PickFund.
  APPLY 'entry' TO bfr{&tmptable}.Fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmptable}.Fund GLFund.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Fund IN FRAME frm-input 
DO:
    FIND FIRST glFund WHERE glFund.Fund = int(bfr{&tmptable}.Fund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glFund THEN DO:
      DISPLAY GLFund.DESCRIP WITH FRAME frm-input.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Fund/Segment, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btncAT IN FRAME frm-input
DO:
  VIEW FRAME frm-pickCat.
  OPEN QUERY qry-pickCat FOR EACH glCat NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickCat.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickCat 
          OR close of THIS-PROCEDURE IN FRAME frm-pickCat
          OR CHOOSE OF btn-ok IN FRAME frm-pickCat 
          OR 'enter':u OF brw-pickCat
          OR 'mouse-select-dblclick' OF brw-pickCat.
  CLOSE QUERY qry-pickCat.
  HIDE FRAME frm-pickCat.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickCat 
    OR 'enter':u OF brw-pickCat
    OR 'mouse-select-dblclick' OF brw-pickCat
DO: 
   GET CURRENT qry-pickCat EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glCat.Cat @ bfr{&tmptable}.Cat glCat.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Cat IN FRAME frm-input 
DO:
    FIND FIRST glCat WHERE glCat.Cat = int(bfr{&tmptable}.Cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glCat THEN DO:
      DISPLAY glCat.Cat @ bfr{&tmptable}.Cat glCat.DESCRIP WITH FRAME frm-input.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glCat THEN DO:
        MESSAGE "Invalid Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnSubCat IN FRAME frm-input
DO:
  VIEW FRAME frm-picksubCat.
  OPEN QUERY qry-picksubCat FOR EACH glSubCat NO-LOCK.
  ENABLE ALL WITH FRAME frm-picksubCat.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picksubCat 
          OR close of THIS-PROCEDURE IN FRAME frm-picksubCat
          OR CHOOSE OF btn-ok IN FRAME frm-picksubCat 
          OR 'enter':u OF brw-picksubCat
          OR 'mouse-select-dblclick' OF brw-picksubCat.
  CLOSE QUERY qry-picksubCat.
  HIDE FRAME frm-picksubCat.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-picksubCat 
    OR 'enter':u OF brw-picksubCat
    OR 'mouse-select-dblclick' OF brw-picksubCat
DO: 
   GET CURRENT qry-picksubCat EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glsubCat.SubCat @ bfr{&tmptable}.SubCat glSubCat.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.SubCat IN FRAME frm-input 
DO:
    FIND FIRST glsubCat WHERE glsubCat.SubCat = int(bfr{&tmptable}.SubCat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glsubCat THEN DO:
      DISPLAY glsubCat.subCat @ bfr{&tmptable}.SubCat GLSubcat.DESCRIP WITH FRAME frm-input.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glsubCat THEN DO:
        MESSAGE "Invalid sub-Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.


ON CHOOSE OF btnclass IN FRAME frm-input
DO:
  VIEW FRAME frm-pickclass.
  OPEN QUERY qry-pickclass FOR EACH glclass NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickclass.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickclass 
          OR close of THIS-PROCEDURE IN FRAME frm-pickclass
          OR CHOOSE OF btn-ok IN FRAME frm-pickclass 
          OR 'enter':u OF brw-pickclass
          OR 'mouse-select-dblclick' OF brw-pickclass.
  CLOSE QUERY qry-pickclass.
  HIDE FRAME frm-pickclass.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickclass 
    OR 'enter':u OF brw-pickclass
    OR 'mouse-select-dblclick' OF brw-pickclass
DO: 
   GET CURRENT qry-pickclass EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glclass.clas @ bfr{&tmptable}.clas glclass.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.clas IN FRAME frm-input 
DO:
    FIND FIRST glclass WHERE glclass.clas = int(bfr{&tmptable}.clas:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glclass THEN DO:
      DISPLAY glclass.clas @ bfr{&tmptable}.clas glclass.DESCRIP WITH FRAME frm-input.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glclass THEN DO:
        MESSAGE "Invalid class, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Proj IN FRAME frm-input
    OR 'TAB' OF bfr{&tmptable}.Proj IN FRAME frm-input
DO:
    FIND FIRST glProj WHERE glProj.Proj= INT(bfr{&tmptable}.Proj:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glProj THEN DO:
      DISPLAY glProj.Proj @ bfr{&tmptable}.Proj GLProj.DESCRIP WITH FRAME frm-input.
      APPLY 'tab' TO SELF.
    END.
    ELSE IF NOT AVAILABLE glProj THEN DO:
        MESSAGE "Invalid Sub-Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj NO-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj GLProj.DESCRIP WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnProj IN FRAME frm-input
DO:
  VIEW FRAME frm-PickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickProj.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-PickProj
          OR CHOOSE OF btn-ok IN FRAME frm-PickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-PickProj.
  APPLY 'entry' TO bfr{&tmptable}.Proj.
  RETURN. 
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Acct  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Ledger already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.Acct = wsid
                   bfr{&tmptable}.DESCRIPTION = bfr{&tmptable}.DESCRIPTION:SCREEN-VALUE IN FRAME frm-input 
                   bfr{&tmptable}.fund    = int(bfr{&tmptable}.fund:SCREEN-VALUE)
                   bfr{&tmptable}.Dept    = int(bfr{&tmptable}.Dept:SCREEN-VALUE)
                   bfr{&tmptable}.SUBVOTE = int(bfr{&tmptable}.SUBVOTE:SCREEN-VALUE)
                   bfr{&tmptable}.ITEM    = int(bfr{&tmptable}.ITEM:SCREEN-VALUE)
                   bfr{&tmptable}.Proj    = int(bfr{&tmptable}.Proj:SCREEN-VALUE)
                   bfr{&tmptable}.Clas    = int(bfr{&tmptable}.Clas:SCREEN-VALUE)
                   bfr{&tmptable}.subcat   = int(bfr{&tmptable}.subcat:SCREEN-VALUE)
                   bfr{&tmptable}.CREDATE = TODAY
                   bfr{&tmptable}.CAT     = int(bfr{&tmptable}.CAT:SCREEN-VALUE)
                   bfr{&tmptable}.ACCTYPE = bfr{&tmptable}.ACCTYPE:SCREEN-VALUE
                   bfr{&tmptable}.DrCr    = INT(bfr{&tmptable}.DrCr :SCREEN-VALUE).
            RELEASE bfr{&tmptable}.
            OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.DESCRIPTION = bfr{&tmptable}.DESCRIPTION:SCREEN-VALUE IN FRAME  frm-input
                bfr{&tmptable}.fund    = int(bfr{&tmptable}.fund:SCREEN-VALUE)
                bfr{&tmptable}.Dept    = int(bfr{&tmptable}.Dept:SCREEN-VALUE)
                bfr{&tmptable}.SUBVOTE = int(bfr{&tmptable}.SUBVOTE:SCREEN-VALUE)
                bfr{&tmptable}.ITEM    = int(bfr{&tmptable}.ITEM:SCREEN-VALUE)
                bfr{&tmptable}.Proj    = int(bfr{&tmptable}.Proj:SCREEN-VALUE)
                bfr{&tmptable}.ACCTYPE = bfr{&tmptable}.ACCTYPE:SCREEN-VALUE
                bfr{&tmptable}.DrCr    = INT(bfr{&tmptable}.DrCr:SCREEN-VALUE)
                bfr{&tmptable}.Clas    = int(bfr{&tmptable}.Clas:SCREEN-VALUE)
                bfr{&tmptable}.subcat   = int(bfr{&tmptable}.subcat:SCREEN-VALUE)
                bfr{&tmptable}.CAT     = int(bfr{&tmptable}.CAT:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.


/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

{glmf.i}

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = DEC(bfr{&tmptable}.acct).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.acct  = wsid EXCLUSIVE-LOCK NO-WAIT NO-ERROR .
    DISPLAY  wsid @ {&skey} {&dispfields}  with FRAME  frm-input.
    FIND FIRST GLDept   WHERE GLDept.Dept =bfr{&tmptable}.Dept NO-LOCK  NO-ERROR.
    FIND FIRST GLProj   WHERE GLProj.Proj = bfr{&tmptable}.Proj NO-LOCK NO-ERROR. 
    FIND FIRST glfund   WHERE glfund.fund = bfr{&tmptable}.Fund NO-LOCK NO-ERROR.
    FIND FIRST glclas   WHERE glclas.clas = bfr{&tmptable}.clas NO-LOCK NO-ERROR.
    FIND FIRST glcat    WHERE glcat.cat = bfr{&tmptable}.cat NO-LOCK NO-ERROR. 
    FIND FIRST glsubcat WHERE glsubcat.subcat = bfr{&tmptable}.subcat NO-LOCK NO-ERROR.
    FIND FIRST glsubvote WHERE glsubvote.subvote = bfr{&tmptable}.subvote NO-LOCK NO-ERROR.
    DISPLAY glclas.descrip GLCat.descrip glsubcat.descrip glsubvote.descrip GLdept.descrip glfund.descrip GLProj.descrip   WITH FRAME frm-input.
    ENABLE {&updFields}  btnFund btnClass btnCat btnSubCat btnDept bfr{&tmptable}.description
     btnProj bfr{&tmptable}.Proj btn-ok btn-close WITH FRAME  frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME  frm-input.
    DISPLAY {&tmpFields} WITH BROWSE brw-{&tmptable}.
   HIDE FRAME  frm-input.
 END.

 



    
