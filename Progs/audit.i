
ON WRITE OF {&tmptable} OLD BUFFER oldBuffer 
    DO:
    IF wsbtn <> "ADD" THEN DO:
       BUFFER-COMPARE {&tmptable} TO oldBuffer SAVE RESULT IN cChangedFields NO-ERROR.
       IF cChangedFields <> "" THEN DO:
             ASSIGN
                hOldRecord = BUFFER {&tmptable}:HANDLE
                hNewRecord  = BUFFER  oldBuffer:HANDLE.     
              DO iChangedFields = 1 TO NUM-ENTRIES(cChangedFields):
                  IF (ENTRY(iChangedFields, cChangedFields) <> "Tarif"
                      AND ENTRY(iChangedFields, cChangedFields) <> "nextP"
                      AND ENTRY(iChangedFields, cChangedFields) <> "Units"
                      AND ENTRY(iChangedFields, cChangedFields) <> "AmtDue"
                      AND ENTRY(iChangedFields, cChangedFields) <> "Cell") THEN DO:
                    CREATE audittrail. 
                    ASSIGN audittrail.rOldRecid  = RECID(oldBuffer)
                            audittrail.rNewRecId  = RECID({&tmptable})   
                            AuditTrail.UID        = varuser
                            AuditTrail.txtkey     = STRING({&sKEY})
                            AuditTrail.DeviceName = lc-host
                            AuditTrail.ipaddress  = lc-address 
                            AuditTrail.trDate     = NOW 
                            AuditTrail.txtcmd     = wsBtn
                            audittrail.txtTable   = SUBSTR(hOldRecord:NAME,1)
                            AuditTrail.txtField   = ENTRY(iChangedFields, cChangedFields)
                            hNewField             = hOldRecord:BUFFER-FIELD(ENTRY(iChangedFields, cChangedFields))
                            AuditTrail.txtNew     = hNewField:STRING-VALUE
                            hOldField             = hNewRecord:BUFFER-FIELD(ENTRY(iChangedFields, cChangedFields))
                            AuditTrail.txtOld     = hOldField:STRING-VALUE.
                  END.
                        
              END.
        END.
        wsbtn = "".
        END.
        RELEASE audittrail.
END.
