/* Create Excel and ouput 2 clumns data for each dbcmf */
PROCEDURE Word-ip:
    DEFINE VARIABLE chWordApplication AS COM-HANDLE NO-UNDO.
    
    CREATE "Word.application" chWordApplication.
    
    chWordApplication:VISIBLE=TRUE.
    chWordApplication:Documents:ADD().
    
    FOR EACH dbcmf WHERE dbacc < 100020.
      chWordApplication:Selection:typetext(dbcmf.NAME).
      chWordApplication:SELECTION:typetext("   ").
      chWordApplication:Selection:typetext(dbcmf.dbacc).
      chWordApplication:selection:TypeParagraph.
    END.
    
    chWordApplication:Quit().
    
    RELEASE OBJECT chWordApplication.
END.

/* Create Excel and ouput 2 clumns data for each dbcmf */
PROCEDURE Excel-ip:
    DEFINE VARIABLE hExcel          AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hWorkbook       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hWorksheet      AS COM-HANDLE NO-UNDO.
    DEF VAR lin AS INT.
     
    CREATE "Excel.Application" hExcel.
     
    ASSIGN
        hWorkBook       =   hExcel:WorkBooks:Add()
        hWorkSheet      =   hWorkBook:WorkSheets(1)
        lin             = 1.
        FOR EACH dbcmf WHERE dbacc < 100020.
          hWorkSheet:Cells(lin,1 ) = dbcmf.NAME.
          hWorkSheet:Cells(lin,2 ) = dbcmf.dbacc.
          lin = lin + 1.
        END.
    ASSIGN hExcel:visible = true.
    
    hExcel:Quit().
    
    RELEASE OBJECT hExcel.
END.
    
