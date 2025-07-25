/* Program.................SimAcc.p
   Notes:................. Main Application calling module
   Author:.................S. Mawire
*/
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
DEFINE SHARED VARIABLE wsUser AS CHARACTER.

DEFINE VARIABLE  MENU-BAR-C-Win AS HANDLE NO-UNDO.
DEFINE VARIABLE hSubMenu AS HANDLE NO-UNDO.
DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.
DEFINE VARIABLE h1SubMenu AS HANDLE NO-UNDO.
DEF VAR wsprg AS CHAR.
def var cType as char.
/* Menu Definitions  */

/*                                                   
DEFINE SUB-MENU m_Debtors_Parameters 
       MENU-ITEM m-dbparm    LABEL "Debtors Parameter Listing"
       RULE
       MENU-ITEM m_Ward_File_Maintenance    LABEL "Ward File Maintenance"
       MENU-ITEM m_SuburbArea_Maintenance   LABEL "Suburb/Area Maintenance"
       MENU-ITEM m_Consumer_Maitenance      LABEL "Consumer Maitenance"
       MENU-ITEM m_Service_Group_Maintenace LABEL "Service Group Maintenace"
       MENU-ITEM m_Tariff_Maintenance       LABEL "Tariff Maintenance"
       MENU-ITEM m_Receipt_code             LABEL "Receipting Code Maintenance"
                TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbrCode.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_PayM                     LABEL "Payment Method Maintenance"
                TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbpMet.p.
                    END.
                END TRIGGERS.

DEFINE SUB-MENU m_Cashbook_Parameters 
       MENU-ITEM m-cbparm    LABEL "Cashbook Parameter Listing"
       RULE
       MENU-ITEM m_TransType LABEL "Transaction Type"
         TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN cbType.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_cbBnak    LABEL "Bank Account Maintenenace"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN cbBank.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m-cbMaint   LABEL "Cashbook Maintenenace"
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN cbBook.p.
                    END.
                END TRIGGERS.


DEFINE SUB-MENU m_ledger_Parameters 
       MENU-ITEM m-parm LABEL "Parameter Listing"
                TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN parm.p.
                    END.
                END TRIGGERS.
       RULE
       MENU-ITEM m_Ctrl LABEL "Control File Maintenance"
               TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN simctr.p.
                    END.
                END TRIGGERS. 
       MENU-ITEM m_Fund LABEL "Fund File Maintenance"
       MENU-ITEM m_Vote LABEL "Vote File Maintenance"
       MENU-ITEM m_Subvote LABEL "Sub-Vote File Maintenance"
       MENU-ITEM m_Cat LABEL "Category File Maintenance"
       MENU-ITEM m_SCat LABEL "Sub-Category File Maintenance"
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN glsubcat.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_item LABEL "Item File Maintenance".

DEFINE SUB-MENU m_Admin 
       MENU-ITEM m_AddUpdate_Users LABEL "Add/Update Users"
       MENU-ITEM mAccess            LABEL "Access Allocations"
       MENU-ITEM m_Uaccess         LABEL "Add/Update User Access"
       RULE
       SUB-MENU  m_Debtors_Parameters LABEL "Debtors Parameters"
       SUB-MENU  m_Cashbook_Parameters LABEL "Cashbook Parameters"
       SUB-MENU  m_Ledger_Parameters LABEL "General Ledger Parameters"
       RULE
       MENU-ITEM m_Exit         LABEL "Exit".

DEFINE SUB-MENU m_crReports 
       MENU-ITEM m_crHistory LABEL "Transaction History".

DEFINE SUB-MENU m_cbReports 
       MENU-ITEM m_cbHistory LABEL "Transaction History".

DEFINE SUB-MENU m_dbReports 
       MENU-ITEM m_Transaction_History LABEL "Transaction History"
            TRIGGERS:
                  ON CHOOSE DO:
                     RUN dbhist.p.
                  END.
             END TRIGGERS.
       MENU-ITEM m_Age_Analysis LABEL "Age Analysis" 
        TRIGGERS:
              ON CHOOSE DO:
                 RUN dbAge.p.
              END.
         END TRIGGERS.
       MENU-ITEM m_Receipt_Consumer LABEL "Debtors Receipt Reports"
       MENU-ITEM m_Activity LABEL "Activity Report".

DEFINE SUB-MENU m_Accounts_Receivables 
       MENU-ITEM m_dbEnquiries      LABEL "Debtors Enquiries"
        TRIGGERS:
              ON CHOOSE DO:
                 RUN dbEnq.p.
              END.
         END TRIGGERS.
       MENU-ITEM m_dbCapture        LABEL "Journal Capture" 
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbjnl01.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_dbUpdate         LABEL "Journal Update" 
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbjnl02.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_dbMeter          LABEL "Meter Reading Capture"
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbmcap.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_Interest_Raising LABEL "Interest Raising"
       MENU-ITEM m_Debit_Raising    LABEL "Debit Raising" 
         TRIGGERS:
              ON CHOOSE DO:
                 RUN dbsg67.p.
              END.
         END TRIGGERS.
       MENU-ITEM m_Statement_Print  LABEL "Statement Print"
       SUB-MENU  m_dbReports        LABEL "Reports"   
       MENU-ITEM m_dbPeriod         LABEL "Period Move"
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbper.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_dbMaint          LABEL "Debtors Maintenance" 
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbsgr10.p.
                    END.
                END TRIGGERS.

DEFINE SUB-MENU m_Accounts_Payables2 
       MENU-ITEM m_crEnquiries LABEL "Creditor Enquiries"
       MENU-ITEM m_crCapture   LABEL "Journal Capture" 
       MENU-ITEM m_crUpdate    LABEL "Journal Update" 
       SUB-MENU  m_crReports   LABEL "Reports"
       MENU-ITEM m_crMaint     LABEL "Creditors Maintenance".

DEFINE SUB-MENU m_Cashbook2 
       MENU-ITEM m_cbEnquiries LABEL "Bank Enquiries"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN bkEnq.p.
                    END.
                END TRIGGERS. 
       MENU-ITEM m_bkEnquiries LABEL "Cashbook Enquiries"
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN cbEnq.p.
                    END.
                END TRIGGERS. 
       MENU-ITEM m_cbPayment   LABEL "Online Payment"
           TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN bkPay.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_cbCapture   LABEL "Journal Capture"
       MENU-ITEM m_cbUpdate    LABEL "Journal Update"
       SUB-MENU  m_cbReports   LABEL "Reports" .

DEFINE SUB-MENU m_glData
       MENU-ITEM m_glCapture   LABEL "Journal Capture"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN gljnl01.p.
                    END.
                END TRIGGERS. 
       MENU-ITEM m_glUpdate    LABEL "Journal Update"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN gljnl02.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_glRev       LABEL "Reverse Journal update"
         TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN glrev01.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_glMove      LABEL "Move Journal Period"
         TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN glrev02.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_glBudget1   LABEL "Budget Data Capture".
    
DEFINE SUB-MENU m_glReports
       MENU-ITEM m_glPosted   LABEL "Posted Transaction Report"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN glptr01.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_glAudit    LABEL "Journal Audit"
       MENU-ITEM m_glTrial    LABEL "Trial Balance Report"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN gltb01.p.
                    END.
                END TRIGGERS.
       MENU-ITEM m_glBudget   LABEL "Budget Variance Report".

DEFINE SUB-MENU m_General_Ledger2
    MENU-ITEM m_glEnquiry    LABEL "Legder Enquiries"
    SUB-MENU  m_glData      LABEL "Data Processing" 
    SUB-MENU  m_glReports   LABEL "Reports" 
    RULE
    MENU-ITEM m-glPeriod    LABEL "Ledger Period Update"
    SKIP
    MENU-ITEM m-Finstat     LABEL "Financial Statement Reports"
    RULE
    MENU-ITEM m_Ledger_Acct LABEL "Ledger Account Maintenance"
    MENU-ITEM m-Reporter    LABEL "Financial Statement Maintenance"
            TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN glrpt01.p.
                    END.
                END TRIGGERS.

DEFINE SUB-MENU  m_Receipting
    MENU-ITEM m_rec1    LABEL "Receipt Capture"
        TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN dbrec.p.
                    END.
                END TRIGGERS.
    MENU-ITEM m_rec2    LABEL "Receipt Validation"
        TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN recval.p.
                    END.
                END TRIGGERS.
    MENU-ITEM m_rec4    LABEL "Receipt Update"
        TRIGGERS:
                    ON CHOOSE
                    DO:
                     RUN recupd.p.
                    END.
                END TRIGGERS.
  */
/* DEFINE MENU MENU-BAR-C-Win MENUBAR. */
{dynamicmenu.i.}
/*
       SUB-MENU  m_Admin        LABEL "Administration"
       SUB-MENU  m_Accounts_Receivables LABEL "Accounts Receivables"
       SUB-MENU  m_Accounts_Payables2 LABEL "Accounts Payables"
       SUB-MENU  m_Cashbook2    LABEL "Cashbook" 
       SUB-MENU  m_Receipting   LABEL "Receipting"
       SUB-MENU m_General_Ledger2 LABEL "General Ledger".
*/

/* Definitions of the field level widgets                               */
DEFINE IMAGE IMAGE-1
     FILENAME "logo1.jpg":U
     SIZE 272.8 BY 28.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     IMAGE-1 AT ROW 1 COL 1 WIDGET-ID 2
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

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "JOBSIM SimAcc Accounting System ver.1.0.0"
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
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
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

/*
&Scoped-define SELF-NAME m_AddUpdate_Users
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AddUpdate_Users C-Win
ON CHOOSE OF MENU-ITEM m_AddUpdate_Users /* Add/Update Users */
DO:
  RUN simusr01.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&Scoped-define SELF-NAME m_glEnquiry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_glEnquiry C-Win
ON CHOOSE OF MENU-ITEM m_glEnquiry 
DO:
  RUN gle05.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_Service_Group_Maintenace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Service_Group_Maintenace C-Win
ON CHOOSE OF MENU-ITEM m_Service_Group_Maintenace 
DO:
  RUN dbsgr.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_Ward_File_Maintenance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ward_File_Maintenance C-Win
ON CHOOSE OF MENU-ITEM m_Ward_File_Maintenance 
DO:
  RUN dbWard.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_Consumer_Maitenance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Consumer_Maitenance C-Win
ON CHOOSE OF MENU-ITEM m_Consumer_Maitenance
DO:
  RUN dbcons.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME  m_Tariff_Maintenance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL  m_Tariff_Maintenance C-Win
ON CHOOSE OF MENU-ITEM m_Tariff_Maintenance
DO:
  RUN dbtar.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_SuburbArea_Maintenance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_SuburbArea_Maintenance C-Win
ON CHOOSE OF MENU-ITEM m_SuburbArea_Maintenance
DO:
  RUN dbsub.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME  m_dbMaint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL  m_dbMaint C-Win
ON CHOOSE OF MENU-ITEM  m_dbMaint
DO:
  RUN dbcmf.p.
  RUN enable_UI.
END.
/* Ledger Menus */
&Scoped-define SELF-NAME m_Fund
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Fund C-Win
ON CHOOSE OF MENU-ITEM m_Fund
DO:
  RUN glfund.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_vote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_vote C-Win
ON CHOOSE OF MENU-ITEM m_vote
DO:
  RUN glvote.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_cat C-Win
ON CHOOSE OF MENU-ITEM m_cat
DO:
  RUN glcat.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_subvote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_subvote C-Win
ON CHOOSE OF MENU-ITEM m_subvote
DO:
  RUN glsubvote.p.
  RUN enable_UI.
END.

&Scoped-define SELF-NAME m_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_item C-Win
ON CHOOSE OF MENU-ITEM m_item
DO:
  /* RUN glitem.p. */
    RUN glitem.p.
  RUN enable_UI.
END.
 
/* &Scoped-define SELF-NAME m_Ctrl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ctrl C-Win
ON CHOOSE OF MENU-ITEM m_Ctrl
DO:
  RUN simctr.p.
  RUN enable_UI.
END. */

&Scoped-define SELF-NAME m_Ledger_Acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ledger_Acct C-Win
ON CHOOSE OF MENU-ITEM m_Ledger_Acct
DO:
  RUN glmf.p.
  RUN enable_UI.
END.
/* _UIB-CODE-BLOCK-END */ 
&ANALYZE-RESUME

*/
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
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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

