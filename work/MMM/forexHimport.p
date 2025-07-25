input from c:\simacc\work\RATESAPRIL-JULY.csv.
repeat :
    CREATE tblforexH.
    import delimiter "," dtRate DecRate txtCur usercode.
    
    
END.
MESSAGE "import finished" VIEW-AS ALERT-BOX.
