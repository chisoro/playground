FOR EACH dbhtf WHERE dbAcc = 21177200  AND Tarif = 76 AND accPer = 202312:
   
    DISPLAY Fund ILedger PoDate Seq TransID UID UID2 Vat.
    ASSIGN 
        Fund  = 0
        ILedger  = 12006208
        PoDate = 01/03/24
        Seq  = 0
        TransID  = 20240103163414
        UID = '01'
        UID2 = '00'.
    
END.
