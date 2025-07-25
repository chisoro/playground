/* PDF output */
define variable Wh          as    widget-handle             no-undo.
define variable w-orientation as  character                 no-undo.
define variable w-letterhead as  char                       no-undo.
define variable w-fontsize as  decimal                      no-undo. 
define variable ll-ok       as    logical initial no        no-undo.
define variable ll-canc     as    logical initial yes       no-undo.
DEF VAR prn-opt       AS    CHAR FORMAT "x(40)"     NO-UNDO.
DEF VAR pdf_editor    AS    HANDLE                  NO-UNDO.
DEF VAR pdf_frame     AS    HANDLE                  NO-UNDO.
DEF VAR prt-file      AS    CHAR                    NO-UNDO.
DEF VAR li-linec      AS INT                        NO-UNDO.
DEF VAR PAGED         AS CHAR INITIAL "PAGED".
DEF VAR VarFileout    AS CHAR                       NO-UNDO.
DEF VAR wsDir         AS CHAR                       NO-UNDO.

define variable lc-line     as    character format "x(132)" no-undo.

DEF BUTTON btnOK
     LABEL "OK"
     SIZE 15 BY 1.14.

DEF BUTTON btnCancel
     LABEL "&Cancel"
     SIZE 15 BY 1.14.

form lc-line
    with frame file-prt-frame
    no-box stream-io
    down no-labels
    width 132.

DEF RECT rect-1
         EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL 
         SIZE 86 BY 2.

DEF FRAME frm-prn
     skip(1)
     space(5)
     prn-opt VIEW-AS COMBO-BOX
        LIST-ITEMS "Print to PDF file", "Print to System Printer", "Output to File"
        SIZE 35 by 1 INNER-LINES 8
        LABEL "Print Options"
     btnOK btnCancel
     RECT-1         AT ROW 1.5 COL 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         TITLE " Report Output Destination".

ON 'enter' OF prn-opt APPLY 'choose' TO btnOk.

ON 'choose' OF btnOk
DO:
    HIDE FRAME frm-prn.
    CASE prn-opt:screen-value :
        WHEN "Print to PDF file" THEN
        DO:
            FIND FIRST simctr NO-ERROR.
            wsDir = simctr.repDir.
            file-info:file-name = this-procedure:file-name.
            prt-file = file-info:file-name.
            prt-file = substring(prt-file,1,index(prt-file,".") - 1).         
            if r-index(prt-file,"_") <> 0 then
                            prt-file = substring(prt-file,r-index(prt-file,"_") + 1).
                        assign prt-file = prt-file      
                            + "SM"                     
                            + SUBstr(STRING(YEAR(today),"9999"),3,2) 
                            + string(MONTH(today),"99")
                            + string(DAY(today),"99") + "_"
                            + substr(string(time,"HH:MM:SS"),1,2)
                            + substr(string(time,"HH:MM:SS"),4,2)
                            + substr(string(time,"HH:MM:SS"),7,2).
            prt-file = wsdir  + prt-file.
            OUTPUT {&stream-name} TO VALUE(prt-file)PAGED.
            /*RUN {&print-prog}.  */ 
            OUTPUT {&stream-name} close.
            input {&file-stream} close.
            create frame pdf_frame.
            create editor pdf_editor
            assign frame     = pdf_frame
                   large     = true
                   word-wrap = false.
                   pdf_editor:read-file(prt-File).
                   pdf_editor:move-to-eof().
                   li-linec = pdf_editor:cursor-line - 1.
            run pdf_out.p (prt-file, w-fontsize, li-linec,                     
                            w-orientation, w-letterhead).
            varFileOut = prt-file + ".pdf".
            FILE-INFO:FILE-NAME = varFileout.
            IF FILE-INFO:FILE-SIZE <> 0 THEN DO:
                varFileOut = "START" + chr(32) + prt-file + ".pdf".
                OS-COMMAND NO-WAIT VALUE(varFileout).
            END.
            ELSE IF FILE-INFO:FILE-SIZE = 0 THEN  
                MESSAGE "No report was produced due to lack of data."
                        VIEW-AS ALERT-BOX INFO.
            OS-DELETE VALUE(prt-file).
            RETURN. 
        END.
        WHEN "Print to System Printer" THEN
        DO:
            hide frame print-selection no-pause.
            file-info:file-name = this-procedure:file-name.
            prt-file = file-info:file-name.
            prt-file = substring(prt-file,1,index(prt-file,".") - 1).
            /* find proparm where proparm.prolabel = "norepcopies" no-lock no-error.           /* 4.15 */
            if avail proparm then
            do:
                do ix = 1 to num-entries(proparm.provalue) :
                    if index(prt-file,entry(ix,proparm.provalue)) <> 0 then
                    do:
                        system-dialog printer-setup update ll-ok in window current-window.
                        if not ll-ok then 
                        do:
                            assign ll-canc = true.
                            apply 'f13' to frame print-selection.
                            return no-apply.
                        end.
                        g-prt-output = 1.
                        hide frame print-selection no-pause.
                        &if defined(manualstream) &then 
                        &else 
                            output {&stream-name} to printer 
                        &endif
                        &if defined(notpaged) &then &else paged &endif 
                        &if defined(pagesize) &then page-size {&pagesize} &endif.
                        &if defined(to-file) &then
                            output {&file-stream} to value({&to-file}).
                        &endif
                        session:set-wait-state("wait").
                        ll-canc = false.
                        &if defined(use-file) &then
                            input {&file-stream} from {&use-file}.
                            repeat:
                                lc-line = "".
                                import {&file-stream} unformatted lc-line.
                                if index(lc-line,chr(12)) <> 0 then page {&stream-name}.    /* 4.14 */
                                display {&stream-name} lc-line with frame file-prt-frame.
                                down {&stream-name} with frame file-prt-frame.
                            end.
                            input {&file-stream} close.
                        &else
                            run {&print-prog}.
                        &endif
                        session:set-wait-state("").
                        &if defined(keep-open) &then
                        &else
                            &if defined(manualstream) &then 
                            &else 
                                output {&stream-name} close. 
                            &endif
                            &if defined(to-file) &then
                                output {&file-stream} close.
                            &endif
                        &endif
                        apply 'f13' to frame print-selection.
                        return.            
                        return.
                    end.
                end.
            end. */
            if r-index(prt-file,"\") <> 0 then
                prt-file = substring(prt-file,r-index(prt-file,"\") + 1).
            assign prt-file = prt-file      
                            + "SM"                     
                            + SUBstr(STRING(YEAR(today),"9999"),3,2) 
                            + string(MONTH(today),"99")
                            + string(DAY(today),"99") + "_"
                            + substr(string(time,"HH:MM:SS"),1,2)
                            + substr(string(time,"HH:MM:SS"),4,2)
                            + substr(string(time,"HH:MM:SS"),7,2)
                            + ".txt".
            prt-file = wsdir  + prt-file.
            /*    

            find proparm where proparm.prolabel = "ReportsDir" 
                no-lock no-error.
            if not avail proparm then
            do:
               message "System has not been setup to print. ~
Contact R-Data for help." skip
                    view-as alert-box info.
               apply 'f13' to frame print-selection.
               return.
            end.
            assign l-dir-name = string(year(today),"9999") + 
                string(month(today),"99")
                   l-exist    = false.

            input from os-dir(proparm.provalue) no-echo.
            repeat:
               import l-idir-name.
               if l-idir-name[3] <> "D" then next.
               if l-idir-name[1] = "."
               or l-idir-name[1] = ".." then next.
               if l-idir-name[1] = l-dir-name then
                  l-exist = true.
            end.
            input close.
            l-dir-name = proparm.provalue + "\" + l-dir-name.
            if l-exist = false then
            do:
               l-idir-name[1] = "".
               os-create-dir value(l-dir-name).
               l-idir-name[1] = string(os-error).
               if int(l-idir-name[1]) <> 0 then
                  message "Failed to create directory. ~                    
Please contact R-Data for help."
                    view-as alert-box info buttons ok.
            end.
            assign l-dir-name   = l-dir-name + "\"
                   prt-file     = l-dir-name + prt-file
                   g-prt-output = 3
                   g-prt-file   = prt-file.
            &if defined(manualstream) &then 
            &else
                output {&stream-name} to value(prt-file)
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            &if defined(use-file) &then
                input {&file-stream} from {&use-file}.
                repeat:
                    lc-line = "".
                    import {&file-stream} unformatted lc-line.
                    if index(lc-line,chr(12)) <> 0 then page {&stream-name}.    /* 4.14 */
                    display {&stream-name} lc-line with frame file-prt-frame.
                    down {&stream-name} with frame file-prt-frame.
                end.
                input {&file-stream} close.
            &ELSE */
                run {&print-prog}.
           /* &endif 
            session:set-wait-state("").
            &if defined(keep-open) &then
            &else
               &if defined(manualstream) &then
               &else
                   output {&stream-name} close.
               &endif
               &if defined(to-file) &then
                   output {&file-stream} close.
               &endif
            &endif
            file-info:file-name = prt-file.
            if file-info:file-size = 0 then
            do:
                message "No report was produced. Probably due to lack of data."
                    view-as alert-box info.
                os-delete value(prt-file).
                apply 'f13' to frame print-selection.
                return.
            end.*/
            system-dialog printer-setup update ll-ok in window current-window.
            if not ll-ok then 
            do:
                assign ll-canc = true.
                apply 'close' to frame frm-prn.
                return no-apply.
            end.
            /*g-prt-output = 1. */
            hide frame frm-prn no-pause.
            &if defined(manualstream) &then 
            &else 
                output {&stream-name} to printer 
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            input from value(prt-file).
            repeat:
                lc-line = "".
                import unformatted lc-line.
                if index(lc-line,chr(12)) <> 0 then page {&stream-name}.        /* 4.14 */
                display {&stream-name} lc-line with frame file-prt-frame.
                down {&stream-name} with frame file-prt-frame.
            end.
            input close.
            session:set-wait-state("").
            &if defined(keep-open) &then
            &else
                &if defined(manualstream) &then 
                &else 
                    output {&stream-name} close. 
                &endif
                &if defined(to-file) &then
                    output {&file-stream} close.
                &endif
            &endif
            apply 'f13' to frame frm-prn.
            return.            
        end.
        OTHERWISE DO:
            MESSAGE "yes" VIEW-AS ALERT-BOX.
            RETURN.
        END.
    END.
END.

VIEW FRAME frm-prn.
ENABLE ALL WITH FRAME frm-prn.
WAIT-FOR CHOOSE OF btnCancel IN FRAME frm-prn OR CLOSE OF THIS-PROCEDURE.
HIDE FRAME frm-prn.
RETURN.

