EF VAR pdf_editor    AS    HANDLE                  NO-UNDO.
DEF VAR pdf_frame     AS    HANDLE                  NO-UNDO.
DEF VAR prt-file      AS    CHAR                    NO-UNDO.
DEF VAR li-linec      AS INT                        NO-UNDO.
DEF VAR PAGED         AS CHAR INITIAL "PAGED".
DEF VAR VarFileout    AS CHAR                       NO-UNDO.

file-info:file-name = this-procedure:file-name.
prt-file = file-info:file-name.
prt-file = substring(prt-file,1,index(prt-file,".") - 1)..
dislay prt-file.
