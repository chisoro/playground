/* Program.................SimAcc.w
   Notes:................. Main Application calling module
   Author:.................S. Mawire
   Modified:.................S. Chisoro
   Date:...................06/04/24
*/

/** This Will Force A Tab Event On ENTER Of Widgets That Normally
   Don't Tab On ENTER, Useful For Data Capturing */
DEFINE QUERY qry-Diary FOR dbDiary scrolling.
DEF BROWSE brw-Diary QUERY qry-Diary
    DISPLAY DBDiary.CreUser  DBDiary.CreDate DBDiary.txtEvent WIDTH 60 DBDiary.UserAssign 
    WITH 22 DOWN SEPARATORS TITLE "Your Event Diary".

&IF DEFINED(no_autotab) &THEN
&ELSE

ON 'enter' ANYWHERE   /* mpm - 29/04/2002 */
DO:
    IF SELF:TYPE = "button" THEN 
        APPLY 'choose' TO SELF.
    ELSE
        APPLY 'tab' TO SELF.
    RETURN NO-APPLY.
END.
&ENDIF

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.

&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
SESSION:DATA-ENTRY-RETURN = TRUE.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */
/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE varUser AS CHARACTER.
DEFINE  NEW SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  NEW SHARED VARIABLE lc-host AS CHAR NO-UNDO.
DEFINE VARIABLE  MENU-BAR-C-Win AS HANDLE NO-UNDO.
DEFINE VARIABLE hSubMenu AS HANDLE NO-UNDO.
DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.
DEFINE VARIABLE h1SubMenu AS HANDLE NO-UNDO.
DEFINE VARIABLE wsFile AS CHAR.
DEF VAR wsprg AS CHAR.
DEF NEW SHARED VAR wsName LIKE simusr.Name.
DEF NEW SHARED VAR wsCo   LIKE SIMCTR.CONAME.
/* Menu Definitions  */
DEF BUTTON btn-ok   LABEL "OK".

DEFINE FRAME frm-main
    SKIP(0.2)
    DBDiary.CreUser    COLON 30 LABEL "Event Creator"
   dbDiary.CreDate     COLON 30 LABEL "Event Date"
   dbDiary.txtEvent    COLON 30 LABEL "Event Description"
   dbDiary.txtNotes    COLON 30 VIEW-AS EDITOR SIZE 80 BY 2 SKIP(0.2)
   dbDiary.Action      COLON 30 LABEL "Action" VIEW-AS COMBO-BOX 
    LIST-ITEM-PAIRS "ACTIVE", 0, "ACTIONED", 1, "APPROVED", 2, "REJECTED", 3
    btn-ok AT ROW 7.7 COL 50
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     BGCOLOR 8 FGCOLOR  1 SIZE 120 BY 9.8
    TITLE "DIARY EVENT MAINTENANCE" VIEW-AS DIALOG-BOX.

CREATE MENU  MENU-BAR-C-Win.
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
        ON CHOOSE DO:
            /*APPLY 'close' TO THIS-PROCEDURE 
             APPLY 'WINDOW-CLOSE' TO CURRENT-WINDOW.
            RUN logon.p.
            RUN simip.p.*/
            CREATE tblLog.
             ASSIGN
                 db005.tblLog.action = "logoff"
                 db005.tblLog.COUNT = 1
                 db005.tblLog.DeviceName = lc-host
                 db005.tblLog.ipaddress = lc-address
                 db005.tblLog.trDate = NOW
                 db005.tblLog.result = "Logging Off"
                 db005.tblLog.UID = varUser.
            FIND FIRST simctr NO-LOCK.
            wsFile =  substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "bin\simacc.lnk".
             RUN simstop.p.
             IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
              THEN DELETE WIDGET C-Win.
              IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
              SESSION:SET-WAIT-STATE("").
              OS-COMMAND SILENT VALUE(wsFile).
        END.     
    END TRIGGERS.
 CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = "About"
    TRIGGERS:
        ON CHOOSE DO:
            RUN about.p.
        END.
    END TRIGGERS.
CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT = hSubMenu
    LABEL = "Exit"
    TRIGGERS:
        ON CHOOSE DO:
             /*RUN simip.p.*/
            CREATE tblLog.
             ASSIGN
                 db005.tblLog.action = "Close"
                 db005.tblLog.COUNT = 1
                 db005.tblLog.DeviceName = lc-host
                 db005.tblLog.ipaddress = lc-address
                 db005.tblLog.trDate = NOW
                 db005.tblLog.result = "System Closed"
                 db005.tblLog.UID = varUser.
            RUN simstop.p.
            QUIT.
            
        END.
     END TRIGGERS.
 RUN logon.p.

{dynamicmenu.i.}
/* Definitions of the field level widgets                               */
DEFINE IMAGE IMAGE-1
     FILENAME "logo19.jpg":U
     SIZE 272.8 BY 28.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     IMAGE-1 AT ROW 1 COL 1 WIDGET-ID 2
     brw-Diary AT ROW 1 COL 95
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 CENTERED
         TITLE FGCOLOR 3 "" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
            wsCo    = SIMCTR.CONAME.
&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = wsCo + ":- " + wsName  + "  - SimAcc Accounting System ver.1.0.0" 
         HEIGHT             = 24.48
         WIDTH              = 250.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 12
         FGCOLOR            = 3
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR =  MENU-BAR-C-Win.

/*ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.*/

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/rbuild%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/rbuild%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JOBSIM SimAcc Accounting System ver.1.0.0 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    
  /*APPLY "CLOSE":U TO THIS-PROCEDURE.
     IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY. */
END.

ON 'mouse-select-dblclick' OF brw-Diary
DO: 
  GET CURRENT qry-Diary EXCLUSIVE-LOCK NO-WAIT.
  IF AVAILABLE dbDiary THEN DO:
      VIEW FRAME frm-main.
      DISPLAY DBDiary.CreDate DBDiary.CreUser DBDiary.Action DBDiary.txtEvent 
          DBDiary.txtNotes WITH FRAME frm-main.
      ENABLE dbDiary.Action btn-Ok WITH FRAME frm-main.
      WAIT-FOR CHOOSE OF btn-Ok OR close of THIS-PROCEDURE IN FRAME frm-main.
      HIDE FRAME frm-main.
  END.
  RETURN. 
END.

ON 'choose':U OF btn-Ok IN FRAME frm-main
DO:
    IF  INT(dbDiary.Action:SCREEN-VALUE IN FRAME frm-main) <> 0 THEN
        ASSIGN dbDiary.Action = INT(dbDiary.Action:SCREEN-VALUE)
               dbDiary.Stat   = 2
               DBDiary.ActDate = TODAY.
    OPEN QUERY qry-Diary FOR EACH dbDiary WHERE DBDiary.Stats = 1 
                          AND (DBDiary.UserAssign = varUser OR DBDiary.CreUser = varUser) NO-LOCK 
                       BY DBDiary.UserAssign BY DBDiary.CreDate.
    RETURN.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JOBSIM SimAcc Accounting System ver.1.0.0 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
/*MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, RETRY MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, RETRY MAIN-BLOCK:  
  RUN enable_UI.
  */ 
 ENABLE IMAGE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
ENABLE brw-Diary WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
OPEN QUERY qry-Diary FOR EACH dbDiary WHERE DBDiary.Stats = 1 
                          AND (DBDiary.UserAssign = varUser OR DBDiary.CreUser = varUser) NO-LOCK 
                       BY DBDiary.UserAssign BY DBDiary.CreDate.   

/* RUN ENABLE_UI.*/
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
/*END.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
  SESSION:SET-WAIT-STATE("").
  QUIT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE IMAGE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

