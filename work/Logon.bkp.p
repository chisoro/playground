&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
/* Program.................Logon.p
   Notes:...... User Logon and verification
   Author:.................S. Mawire
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------
 SESSION:DATA-ENTRY-RETURN = YES.
  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
 SESSION:DATA-ENTRY-RETURN = YES.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           
DEFINE VARIABLE w_usernum LIKE users.userno.
DEFINE VARIABLE w_passwordd LIKE users.passwrd.
 Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 w_userno w_password btLogin BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS w_userno w_password 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets  
                             */
DEF NEW SHARED VAR wsName LIKE simusr.Name.
DEF NEW SHARED VAR wsCo   LIKE SIMCTR.CONAME.

DEFINE BUTTON btLogin 
     LABEL "Login" 
     SIZE 15 BY 1.14
     BGCOLOR 10 .

DEFINE BUTTON btnOk 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 10 .

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 9 BY 1.14
     BGCOLOR 12 .

DEFINE VARIABLE w_password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE NEW SHARED VARIABLE w_userno AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logon Code" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 8.81
     BGCOLOR 6 FGCOLOR 0 .

DEFINE VAR X AS INT.
DEFINE NEW SHARED VARIABLE varUser AS CHARACTER.
DEF VAR w_response LIKE w_password.
DEF VAR w_response1 LIKE w_password.


/* ************************  Frame Definitions  *********************** */
DEFINE FRAME frm-newPass
     w_response AT ROW 1.71 COL 30 COLON-ALIGNED LABEL "NEW Password" PASSWORD-FIELD
     w_response1 AT ROW 2.71 COL 30 COLON-ALIGNED LABEL "Confirm Password" PASSWORD-FIELD
     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10.4 ROW 5.19
         SIZE 70.8 BY 10.91.

DEFINE FRAME fMain
     w_userno AT ROW 4.81 COL 19 COLON-ALIGNED WIDGET-ID 4
     simusr.NAME VIEW-AS FILL-IN NO-LABEL NO-TAB-STOP
     w_password AT ROW 6.71 COL 19 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD 
     btLogin AT ROW 8.38 COL 21 WIDGET-ID 8
     BtnCancel AT ROW 8.38 COL 48 WIDGET-ID 10
     RECT-1 AT ROW 1.48 COL 3 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.4 ROW 1.19
         SIZE 70.8 BY 9.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON BtnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SimAcc LOGON"
         HEIGHT             = 10.86
         WIDTH              = 74
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 6
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin 
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Login To Shmain */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

ON 'enter' OF w_userno IN FRAME fMain
   /*  OR 'tab' OF w_userno IN FRAME fMain  Login */
DO:
    FIND FIRST simusr WHERE simusr.usercode = w_userno:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE simusr THEN DO:
        X = X + 1.
        MESSAGE "Invalid User entered..please try again" VIEW-AS ALERT-BOX.
        IF X < 3 THEN
           RETURN NO-APPLY.
        ELSE QUIT.
    END.
    ELSE IF AVAILABLE simusr AND simusr.txtStatus = "L" THEN DO:
        MESSAGE "USER IS LOCKED... cannot LOG" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY simusr.NAME WITH FRAME fmain.
        IF simusr.txtPass = "sim" THEN DO:
           RUN newPass.ip.
        END.
       APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

&Scoped-define SELF-NAME btLogin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLogin wWin
ON CHOOSE OF btLogin IN FRAME fMain /* Login */
DO:
    IF simusr.usercode = w_userno:SCREEN-VALUE AND simusr.txtPass = w_password:SCREEN-VALUE THEN
    DO:
        ASSIGN varUser = w_userno:SCREEN-VALUE.
        FIND FIRST simusr WHERE simusr.usercode = w_userno:SCREEN-VALUE NO-LOCK NO-ERROR. 
               ASSIGN wsName  = simusr.NAME.
        FIND FIRST SIMCTR NO-LOCK NO-ERROR.
                      wsCo    = SIMCTR.CONAME. 
         wWin:HIDDEN = YES.
        RUN SimAcc.w.
    END.
    ELSE DO:
        IF simusr.txtPass <> w_password:SCREEN-VALUE THEN DO:
            X = X + 1.
            MESSAGE "Invalid Password entered..please try again" VIEW-AS ALERT-BOX.
            IF X < 3 THEN
                 APPLY 'entry' TO w_userno .
            ELSE DO: 
               simusr.txtStatus = "L".
               MESSAGE "Too many failures Account has been LOCKED" VIEW-AS ALERT-BOX.
               QUIT.
            END.
        END.
    END.
END.

 
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel wWin
ON CHOOSE OF BtnCancel IN FRAME fMain /* Cancel */
DO:
    RUN disable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewPass.ip wWin  _ADM-CREATE-OBJECTS

PROCEDURE NewPass.ip:
    UPDATE w_response w_response1 WITH FRAME frm-newPass.
    IF  w_response <> w_response1 THEN DO:
        MESSAGE "Password Creation Failed" VIEW-AS ALERT-BOX.
         QUIT.
    END.
    ELSE DO:
       simusr.txtPass = w_response.
       MESSAGE "New password create please supply this to logon" VIEW-AS ALERT-BOX.
       HIDE FRAME frm-newPass.
       APPLY 'ENTRY' TO w_password IN FRAME fmain.
    END.
    RETURN.
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
  QUIT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY w_userno w_password 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 w_userno w_password btLogin BtnCancel 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

