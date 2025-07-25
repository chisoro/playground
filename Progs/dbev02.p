session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbev02.p
   Notes:...... ...........Diary event capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg          "Account already exist"
&SCOPED-DEFINE wsTitle        "Accounts Receivable Event Maintenance"
&SCOPED-DEFINE tmptable       dbDiary
&SCOPED-DEFINE skey           {&tmptable}.CreUser
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.CreDate ~
                              bfr{&tmptable}.txtEvent ~
                              bfr{&tmptable}.UserAssign ~
                              bfr{&tmptable}.txtNotes
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.CreDate ~
                              bfr{&tmptable}.txtEvent ~
                              bfr{&tmptable}.UserAssign ~
                              
{varlibrary.i}
DEF SHARED VAR varUser    AS CHAR .
DEF VAR wsdes LIKE dbsmf.Descrip.
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 120 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 120 by 20.8.


DEFINE QUERY qry-Diary FOR bfr{&tmptable} scrolling.
DEF BROWSE brw-Diary QUERY qry-Diary
    DISPLAY bfr{&tmptable}.CreUser  bfr{&tmptable}.CreDate bfr{&tmptable}.txtEvent WIDTH 80
    WITH 16 DOWN SEPARATORS TITLE "Your Event Diary".

DEFINE FRAME frm-main
    SKIP(0.2)
    {&skey}  COLON 20 NO-TAB-STOP NO-LABEL 
    bfr{&tmptable}.CreDate     COLON 30 LABEL "Event Date"
    bfr{&tmptable}.txtEvent    COLON 30 LABEL "Event Description"
    bfr{&tmptable}.UserAssign  COLON 30 LABEL "Assigned to User"
    simusr.NAME NO-LABEL VIEW-AS TEXT
    bfr{&tmptable}.txtNotes    COLON 30 VIEW-AS EDITOR SIZE 80 BY 2 SKIP(0.2)
    brw-Diary COLON 20 
    btn-ok AT ROW 22.7 COL 20 SPACE(50)
    btn-close 
    rect-2 AT ROW 1.0 COL 5
    rect-1 AT ROW 22 COL 5
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR  1*/ SIZE 130 BY 24.8
    TITLE "DIARY EVENT MAINTENANCE" VIEW-AS DIALOG-BOX.
    
/* ***** Triggers  **** */
ON 'tab':U OF bfr{&tmptable}.UserAssign IN FRAME frm-main
    OR 'enter':U OF bfr{&tmptable}.UserAssign IN FRAME frm-main
DO:
    FIND FIRST simusr WHERE simusr.usercode = bfr{&tmptable}.UserAssign:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE simusr THEN DO:
        MESSAGE "User allocated task does not exist....please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        bfr{&tmptable}.UserAssign:SCREEN-VALUE = simusr.usercode.
        DISPLAY simusr.NAME WITH FRAME frm-main.
    END.
    RETURN.
END.

ON 'mouse-select-dblclick' OF brw-Diary
DO: 
  GET CURRENT qry-Diary NO-LOCK NO-WAIT.
  IF AVAILABLE dbDiary THEN
      MESSAGE bfr{&tmptable}.txtNotes VIEW-AS ALERT-BOX.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Main 
DO: 
   CREATE bfr{&tmptable}.
   ASSIGN bfr{&tmptable}.CreDate     = DATE(bfr{&tmptable}.CreDate:SCREEN-VALUE)
          bfr{&tmptable}.UserAssign  = bfr{&tmptable}.UserAssign:SCREEN-VALUE
          bfr{&tmptable}.txtEvent    = bfr{&tmptable}.txtEvent:SCREEN-VALUE 
          bfr{&tmptable}.txtNotes    = bfr{&tmptable}.txtNotes:SCREEN-VALUE
          bfr{&tmptable}.CreUser     = varUser
          bfr{&tmptable}.Stats       = 1.
   OPEN QUERY qry-Diary FOR EACH bfr{&tmptable}  WHERE bfr{&tmptable}.Stats = 1 
                                                    AND bfr{&tmptable}.CreUser = varUser NO-LOCK .
   APPLY 'entry' TO bfr{&tmptable}.CreDate.
   RETURN. 
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main.
ENABLE  {&UpdFields} brw-Diary btn-ok btn-close    WITH FRAME frm-main.
 OPEN QUERY qry-Diary FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.Stats = 1
                                                AND bfr{&tmptable}.CreUser = varUser NO-LOCK .
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
