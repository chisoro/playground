/* Program.................pdf_out.p
   Notes:................. Create PDF file
   Author:.................S. Mawire
*/
{pdf_inc.i}
def input param parFile as char no-undo.
def input param parFontsize as dec no-undo.
def input param parLinecnt as int no-undo.
def input param parOrientation as char no-undo.
def input param parLetterhead as char no-undo.
def var varCurlines as int no-undo.
def var varFileOut as char no-undo.
def var varline as char format "x(200)" no-undo.
def var varwatermark as char no-undo init "".
def var varletterhead as char no-undo init "".
def var i as int no-undo.
DEF VAR varProcess AS CHAR  NO-UNDO.
def stream strmInput.
DEF FRAME process-frame SKIP 10
    varProcess.

session:set-wait-state("wait").

view frame process-frame.
pause 0.

varFileOut = parFile + ".pdf".

run pdf_new ("Spdf", varFileOut).
run pdf_set_PaperType ("Spdf","A4").
run pdf_set_TopMargin ("Spdf",15).
run pdf_set_BottomMargin ("Spdf",10).
run pdf_set_LeftMargin ("Spdf",10).
run pdf_new_page2 ("Spdf",parOrientation).
run pdf_set_info ("Spdf","Author", user("dictdb")).
run pdf_set_info ("Spdf","Subject", "").
run pdf_set_info ("Spdf","Title", "").
run pdf_set_info ("Spdf","Keywords", "").
run pdf_set_info ("Spdf","Creator","JobSim").
run pdf_set_info ("Spdf","Producer", "Siliot").
run pdf_set_font ("Spdf","Courier",parFontsize).

if parLetterhead <> "" then
do:
   if search(parLetterhead) <> ? then
   do:
       os-copy value(search(parLetterhead)) value(session:temp-dir + parLetterhead).
       varletterhead = session:temp-dir + parLetterhead.
       run pdf_load_image ("Spdf","Letterhead",varletterhead).
       if varletterhead <> "" then do:
          run pdf_place_image ("Spdf","Letterhead",1,155,600,150).
          do i = 1 to 10:
            run pdf_skip ("Spdf").
            varCurlines = varCurlines + 1.
          end.
       end.
   end.
end.
else
if search("watermark.jpg") <> ? then
do:
    os-copy value(search("watermark.jpg")) value(session:temp-dir + "watermark.jpg").
    varwatermark = session:temp-dir + "watermark.jpg".
    run pdf_load_image ("Spdf","Logo",varwatermark).
    if varwatermark <> "" then do:
       if parOrientation = "Landscape" then
          run pdf_place_image ("Spdf","Logo",300,350,200,200).
       else
          run pdf_place_image ("Spdf","Logo",200,450,200,200).
    end.
end.

input stream strmInput from value(parFile).

repeat :
   /*if not ll-process then return "cancelled". */
   varline = "".
   import stream strmInput unformatted varline.
   varCurlines = varCurlines + 1.
   if not (varCurlines / parLinecnt) * 100 > 100 then
    assign Varprocess:screen-value = "Generating PDF... " +
        string((varCurlines / parLinecnt) * 100,"zz9") + "%".
   pause 0.
   process events.
   if index(varline, chr(012)) <> 0 then
   do:
      if parOrientation = "Landscape" then
         run pdf_set_Orientation ("Spdf","Landscape").
      else
         run pdf_set_Orientation ("Spdf","Portrait").

      run pdf_set_TopMargin ("Spdf",15).
      run pdf_set_BottomMargin ("Spdf",10).
      run pdf_set_LeftMargin ("Spdf",10).
      run pdf_new_page2 ("Spdf",parOrientation).

      if varletterhead <> "" then do:
          run pdf_place_image ("Spdf","Letterhead",1,155,600,150).
          do i = 1 to 10:
            run pdf_skip ("Spdf").
            varCurlines = varCurlines + 1.
          end.
      end.

      run pdf_set_PaperType ("Spdf","A4").
      run pdf_set_font ("Spdf","Courier",parFontsize).
      if varwatermark <> "" then do:
         if parOrientation = "Landscape" then
            run pdf_place_image ("Spdf","Logo",300,350,200,200).
         else
            run pdf_place_image ("Spdf","Logo",200,450,200,200).
      end.
   end.
   run pdf_text ("Spdf", varline).
   run pdf_skip ("Spdf").
end.

varprocess:screen-value = "Writing stream data...".

input stream strmInput close.

run pdf_close("Spdf").

if varwatermark <> "" then os-delete value(varwatermark).

session:set-wait-state("").

hide frame process-frame no-pause.

return varFileOut.
