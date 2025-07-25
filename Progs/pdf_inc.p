/******************************************************************************

    Program:        pdf_inc.p

    Written By:     Gordon Campbell - PRO-SYS Consultants Ltd.
    Written On:     November 10, 2003

    Description:    Contains function and variable definitions for
                    generating a PDF document from within Progress

    Note:           This was copied from pdf_inc.i

    --------------------- Revision History ------------------

    Date:     Author        Change Description

    11/10/03  G Campbell    Initial Copy from pdf_inc.i

    11/17/03  G Campbell    Removed hard-coded 'sPDF' strings

                            Added pdf_encrypt procedure to allow for
                            silent or interactive PDF encryption.  Integrates
                            with the PSP (Pretty Safe PDF) product from PDFlib
                            GmbH.

                            Contact gcampbell@epro-sys.com to purchase a copy
                            of the PSP product.  Discounts for PEG Members.

    11/19/03  G Campbell    Changed how the skip was working.  Now skip based
                            on the current PointSize.

    11/20/03  G Campbell    Changed how pdf_text_xy was being processed.  It
                            now processes this text in the Graphic Grid.  It no
                            longer updates the TextX and TextY values.

    11/21/03  G Campbell    Added pdf_text_char procedure

    11/24/03  G Campbell    Fixed replace text bug RE:  Bug #847880

    11/25/03  G Campbell    Fixed European Numberic Format problem
                            RE: Bug #848975

                            This issue was previously identified by Herbert Bayer
                            (Herbert.Bayer@bundrinno.de) and I've used his
                            methodology for accomodating this issue.

    11/27/03  G Campbell    Added ability to add bookmarks to PDF documents. See
                            the custlist.p samples for sample implementation.

                            Included adding the following procedures:

                            pdf_bookmark (public procedure)
                            pdf_load_bookmarks (private procedure)
                            pdf_process_bookmarks (private procedure)

    12/04/03  G Campbell    Added following procedures:

                            pdf_set_linejoin
                            pdf_circle  ( Note: Graphic X and Y points become the
                                                X and Y Parameters - center point)
                            pdf_set_FillRed
                            pdf_set_FillGreen
                            pdf_set_FillBlue

                            Added following functions:

                            pdf_FillRed
                            pdf_FillGreen
                            pdf_FillBlue

                            string2dec (private - as per Peter Kiss
                                                  (peter.kiss@paradyme.be)

                            Plus: a whole whack of procedures used to
                                  merge disparate streams.  See sample
                                  procedures layer1.p and layer2.p.

                                  Thanks to Peter Kiss for this merging
                                  capability.  Hopefully I'm decribing the
                                  procedure uses effectively.

                            pdf_merge_stream
                            pdf_merge_stream_content (private)
                            pdf_merge_stream_link (private)
                            pdf_merge_stream_param (private)
                            pdf_merge_stream_image (private)
                            pdf_merge_stream_diff (private)
                            pdf_merge_stream_book (private)
                            pdf_merge_stream_book_detail (private)
                            pdf_ReplaceText

  12/17/03  G Campbell      Added procedure pdf_set_PageRotation
                            Added function pdf_PageRotation
                            Added temp-table TT_Pages to track Pages.

                            Updated pdf_reset_stream and pdf_reset_all to
                            ensure that all Temp Tables are being cleared
                            correctly.

  12/18/03  G Campbell      Added ability to Compress Page Streams using the
                            zlib (www.zlib.net) compression library.

                            Thanks to Maurits van Rijnen (peg@vanrijnen.nl)
                            for providing the command syntax in zlib.p and
                            zlibbind.p.

                            Added function pdf_get_parameter

                            Added procedures:
                            pdf_set_parameter
                            OutputMemPtr (private) - used to output Compressed
                                                     data

                            Added appropriate indices to Temp Tables.  Helped
                            with performance on large PDF files (thanks again
                            Herbert Bayer).

  12/29/03  G Campbell      Added ability to Encrypt using PDFencrypt.p (which
                            uses a couple of C functions for binary data
                            manipulation)

                            pdfencrypt.p currently only handles 40-bit
                            Standard Adobe Encryption.

  01/08/04  G Campbell      Started implementing Tools functionality.  This
                            included adding:

                            h_PDF-tool  Procedure Handle

                            pdf_tool_add Internal Procedure
                            pdf_tool_create Internal Procedure
                            pdf_set_tool_parameter Internal Procedure
                            pdf_get_tool_parameter Function

  01/09/04  G Campbell      Fixed issue with hardcoding of c:\temp\content.txt.
                            Now uses TT_pdf_stream.obj_UniqueID + "-C.txt" to
                            create Content file.

                            Thanks to Mikael Harjpe [mikael.hjerpe@stamford.se]
                            for pointing out this error.

  01/15/04  G Campbell      As per Gerben Wieringa (g.wieringa@vcd.nl)
  
                            In order to accommodate double-byte codepage 
                            (eg: 1252), with the startup parameter -checkdbe 
                            (check double byte enabled)
                            
                              - Added "character" option to all length() and 
                                substr() functions.
                              - Had to add "-1" in some of the substr() 
                                function-calls as third argument
                              
                            Added SESSION:TEMP-DIR when generating Content
                            File.
  
  01/16/04  G Campbell      Modified how the pdf_wrap_text procedure was 
                            working.  It wasn't correctly accounting for CHR(10)
                            within the text string plus it wasn't determining
                            the maximum allowable characters properly.  
                            
                            NOTE:  If you are currently using pdf_wrap_text 
                                   then these changes may result in changes to 
                                   your output

  01/20/04  G Campbell      Included updated merge methods - supplied by
                            Peter Kiss (peter.kiss@paradyme.be) of Paradyme 
                            Belgium.
                            
  01/20/04  G Campbell      Added procedure pdf_curve - this procedure allows
                            you to create a Bezier curve based on three x,y
                            locations.  If you call pdf_curve you need to also 
                            call pdf_close_path otherwise the curve won't 
                            appear.
                            
                            Also modified pdf_circle to use pdf_curve. 
                            pdf_circle automatically closes the 'path' (via
                            pdf_close_path or the curve.p in the samples/super
                            subdirectory).
                            
  01/21/04  G Campbell      Added function pdf_GetNumFittingChars.  This
                            replaces pdf_get_NumFittingChars (deprecated).
                            
                            Requires that you pass in the From X and and to X
                            point locations.  The difference is then used to
                            determine the maximum allowable characters.

                            Plus, Gerben missed adding one 'character' option on
                            a LENGTH statement.  Resolved.

  01/21/04  G Campbell    Added pdf_text_charxy procedure
                          This allows us to add a special character at a given
                          X/Y position

  01/21/04  G Campbell    Added function pdf_text_widthdec.  Similar to 
                          pdf_text_width except that it returns a decimal value 
                          for the text width.  This is useful for more specific
                          placement and verification of text size.

  01/22/04  G Campbell    Fixed issue with pdf_get_NumFittingChars and 
                          pdf_GetNumFittingChars.  Need to increment the Extent
                          by plus one for External fonts because the AFM file 
                          starts at Character zero.  Also had to increment the
                          FILL in the Base 14 font definitions to accomodate 
                          this.
  
                          Added Symbol and ZapfDingbats Standard Fonts.  The 
                          Base14 Fonts are now complete.

                          Added procedure pdf_text_align.  This allow us to 
                          position text based on an explicit X location. 
                          
                          Possible alignment possiblities are LEFT, RIGHT

                          Removed pdf_get_numFittingChars ... 

  01/28/04  G Campbell    BUG: During the addition of the ZapfDingbats and 
                          Symbol basefonts I had set the default Encoding to
                          StandardEncoding from WinAnsiEncoding.  This caused
                          issues with some installations.  I have restored the
                          default Encoding to WinAnsiEncoding and specifically
                          set the Encoding on the Symbol and ZapfDingbats to
                          StandardEncoding.
                          
  01/28/04  G Campbell    Changed Based Font Names from /Fn to /BFn.  Hopefully
                          this will be unique enough for future processing. At
                          this point I'm currently looking at being able to
                          include Pages from existing PDFs.  Including Fonts
                          may be an issue ... 

  01/29/04  G Campbell    Use dec2string in pdf_text_align - as per Peter Kiss.
  
  02/20/04  G Campbell    Added NO-UNDO to all TEMP-TABLE declarations

  02/24/04  G Campbell    Added new parameter called "LineSpacer".  This
                          parameter allows you to 'add' a number of points 
                          between each line when applying a pdf_skip.  This will
                          hopefully allow people to mimic the older method of
                          handling line feeds.

  02/25/04  G Campbell    Added procedure pdf_exec_footer as per Peter Kiss
  
                          This procedure allows you to call the Footer creation
                          procedure from anywhere.  This was the result of a 
                          small problem using the footer when you are sharing 
                          the stream over diff. programs.

                          Eg: When using a main program (without a footer set) 
                          then calling another program to do some printing 
                          using the same stream (file) and in the called 
                          program you specify a header and footer, 
                          then the last footer will not be printed bacause 
                          this is done in pdf_close

  02/27/04  G Campbell    Added procedures:
   
                          pdf_text_xy_dec 
                          pdf_line_dec
  
                          Changed procedures (do use decimals)
                          
                          pdf_GraphicX
                          pdf_GraphicY
                          pdf_set_GraphicX
                          pdf_set_GraphicY
                          
                          This allows for more exact placement of text, lines, etc
                          No rounding to the nearest point.
                                                    
  02/27/04  G Campbell    Added code to use Zlib to compress the JPEG images.
  
                          Added "NOEMBED" functionality to disallow embedding
                          of a True Type Font.  THIS IS NOT RECOMMENDED but has
                          been added because it could be useful if the PDF file
                          is being generated and printed for local (single
                          machine) purposes only.  This functionality removes
                          the embedded TTF file from the PDF document resulting
                          in a smaller PDF document.
                          
                          Note: The NOEMBED is added as the second entry when
                                loading the font, eg:
                                
                                RUN pdf_load_font  ("Spdf",
                                                    "Code 39|NOEMBED",
                                                    "",
                                                    "\gord\pdfinclude\samples\support\code39.afm","").
                                
                                When not EMBEDing, the AFM file entry is still 
                                required but the TTF font file is not.
                                
                                Also, you must enter the 'real name' of font.
                                For example, instead of using say 'TimesRoman',
                                you should use 'Times Roman'.  Spaces are now
                                important.

  03/01/04  G Campbell  As per Herbert Bayer
  
                        Added additional check when loading a Font or an Image.
                        Check to see if the font/image has been previously 
                        loaded.  If so, then return an error message.  An image
                        or Font can only be loaded once ... that is, can only
                        be loaded once with the same name.  You can in fact load
                        the same image/font multiple times under different names
                        but it doesn't make sense to do this.

  03/01/04  G Campbell  Added code to compress embedded TTF file.

  03/03/04  G Campbell  As per Bruno Van Loon (brunovanloon@hotmail.com)
  
                        Added LineSpacer  to the pdf_text_at procedure.
  
  03/04/04  G Campbell  Changed TT_pdf_annot to TT_pdf_annot
  
                        Added procedures:
                        
                        pdf_note
                        pdf_stamp
                        pdf_markup
                        
                        These procedures allow for additional annotation types.
                        See note.p in the samples/super subdirectory for an
                        example on how to use.  
                        
                        Note:  If using the Stamp or Markup procedures ensure
                               that the Style or Type is entered exactly as 
                               required.  That is, use 'Draft' not 'draft'.  
                               These values are case-sensitive.

  03/08/04  G Campbell  Removed LineSpacer from pdf_text_to and pdf_text_at.  
                        Seemed to be causing issues and the spacing should
                        be handled by pdf_skip.

  03/09/04  G Campbell  Added code in pdf_close to cleanup persistent handles 
                        for Zlib.p and zlibbind.p.
                        
                        Also added code to pdf_new that checks to see if h_zlib
                        is valid or not.  If not, then rerun zlib.p.

  03/22/04  G Campbell  Adjusted code to correctly handle placement of rotated
                        text.
                        
                        Also removed "BY obj_line DESC" when outputting text
                        content.  Not required as the Sequence # is unique 
                        enough.

  03/23/04  J Johnson   Added pdf_insert_page procedure to insert a page
                        somewhere within the document.

                        Modified pdf_new_page to handle inserted pages
                        by always adding the new page to the end of the
                        document.
                        
                        Added "ShowBookmarks" option to pdf_set_info
                        and pdf_get_info.   If set to "YES", Acrobat
                        will open the document with the bookmarks showing.

                        Added dynamic PAGEno function. ("@@PageNo")
                        Allows for dynamic page numbering when inserting pages.

                        Added dynamic justification ability to pdf_text_boxed_xy
                        Added delimited parameters to the "TEXTXY" object type
                        to allow for left, right or center justification
                        to correctly justify strings with functions such as
                        @@PageNo and @@TotalPages
    
                        pdf_text_boxed_xy modified to allow a weight of 0 for
                        the line.  If it is 0, then the text is only justified
                        and a box is not drawn.

                        modified pdf_content to dynamically justify text
                        with left, right or center attributes
                        defined as object type "TEXTXY"
                        re-computes and replaces pdfcolumn value based on
                        text being printed.
                    
                        references to EQ "TEXTXY" have been replaced with
                        BEGINS "TEXTXY" to allow for justification parameters
                        to be included in object type.

                        NOTE:  Someone may want to modify the pdf_text_align
                               procedure to use dynamic justification
                               This would allow for justifying things like
                               @@TotalPages and @@PageNo

  03/23/04  G Campbell  Added ability to use external templates via 
                        pdf_load_template and pdf_use_template - these
                        call internal procedures in the persistent routine
                        pdfTemplate.p.

  03/24/04  G Campbell  Changed delimiter to | (pipe) when using NOEMBED.  This
                        is because you can append Bold or Italic to the font
                        name when not embedded.  
                        
                        For example: Lucida Console,Bold
                                 or  Lucida Console,BoldItalic

  04/01/04  G Campbell  Added the following functions/procedures and modified
            M Edu       code appropriately.  These changes allow PDFinclude to
                        (again) work with Progress V8.
  
                        Added GetFileSize function 
                          - Used to determine the size of a file using 
                            different methods for different Progress versions.
                            
                        Added GetFileContent procedure
                           
                          - Use different methods to load file contents into
                            a MEMPTR variable based on different Progress
                            versions.
                            
                        Added PutFileContent procedure
                           
                          - Use different methods to save the contents of a 
                            MEMPTR variable to the PDF stream based on different
                            Progress Versions.
                            
                        Use PROVERSION preprocessor to determine which Zlib
                        procedure to run (zlib8.p for V8 and zlib.p for
                        everyone else).

  04/01/04  G Campbell  As per Peter Kiss
                        
                        Added 45,135,225,315 DEGREES for text rotation

  04/01/04  G Campbell  Removed VALUE(SEARCH()) when running pdftool.p. This 
                        wouldn't work if you only had the .r in the directory.
  
  04/07/04  G Campbell  Added page_width and page_height fields to the 
                        TT_pdf_page temp-table.  Now we can set the page/size
                        and width per page in the document therefore allowing us
                        to use pdf_set_orientation to change the orientation of
                        each page.  
                        
                        Added procedure pdf_new_page2 to allow us to add a page
                        and define which orientation we want it to be viewable
                        in.  
                        
                        The pdf_new_page2 accepts two parameters - PDF stream 
                        and orientation.

  04/12/04  G Campbell  Added the following parameters (used via 
                        pdf_set_parameter) to set the Viewer Preferences:
                        
                        HideToolbar - view application's toolbar when document
                                      is active.  Valid entry is TRUE/FALSE with
                                      FALSE being the default.
                        HideMenubar - view application's menubar when document
                                      is active.  Valid entry is TRUE/FALSE with
                                      FALSE being the default.
                        HideWindowUI - view user interface elements in the 
                                       document's window (such as scroll bars 
                                       and navigation controls), leaving only 
                                       the document's contents displayed. Valid
                                       entry is TRUE/FALSE with FALSE being the
                                       default.
                        FitWindow - resize the document's window to fit the size 
                                    of the first displayed page.  Valid entry is
                                    TRUE/FALSE with FALSE being the default.
                        CenterWindow - position the document's window in the 
                                       center of the screen.  Valid entry is
                                       TRUE/FALSE with FALSE being the default.
                        DisplayDocTitle - display the document title taken from 
                                          the Title entry of the document 
                                          information dictionary. Valid entry is
                                          TRUE/FALSE with FALSE being the 
                                          default.

                        NOTE: REMOVED ShowBookmarks option from pdf_set_info.
                              -------
                              
  04/12/04  G Campbell  Added the parameter PageMode- valid possibilities
                        are:
                        
                        UseNone     - Neither document outline nor thumbnail 
                                     images visible (DEFAULT)
                        UseOutlines - Document outline visible (this replaces
                                     the ShowBookmarks option in the 
                                     pdf_set_info procedure)
                        UseThumbs   - Thumbnail images visible
                        FullScreen  - Full-screen mode, with no menu bar, 
                                      window controls, or any other window 
                                      visible

                        Added the parameter PageLayout- valid possibilities
                        are:
                        
                        SinglePage     - Display one page at a time (DEFAULT)
                        OneColumn      - Display the pages in one column
                        TwoColumnLeft  - Display the pages in two columns, with 
                                         oddnumbered pages on the left
                        TwoColumnRight - Display the pages in two columns, with 
                                         oddnumbered pages on the right.

  04/12/04  G Campbell  Added procedure pdf_GetBestFont ... as per 
                        James McAteer [James.McAteer@Carltoncards.co.uk]
  
                        This procedure calculates the best font size to use 
                        when inserting text into a given range along the X axis 
                        - it tests in 0.5 point size increments.  
                        
                        Example: This is useful if you are trying to fit 
                        variable-size text within a rectangle and you need to 
                        determine the best font to use to ensure that the full
                        text appears (or use the pdfChopText option to fit the
                        most text with the smallest allowable font).
                        
  04/14/04  G Campbell  Updated the pdf_new_page, pdf_new_page2 and 
                        pdf_insert_page procedures to correctly set the Graphic
                        X/Y coordinates (based on Left Margin and Top Margin) 
                        plus ensure that the last Font/Point Size that was used
                        was being reset.

  04/27/04  G Campbell  Added pdf_place_image2 and pdf_rect2
  
                        pdf_place_image2 - allows you to place images below
                                           the graphical elements (such as 
                                           rectangles, lines etc) and text
                        pdf_rect2        - allows you to draw a rectangle
                                           without filling it
                                           
                        The combination of these two elements allows you to 
                        use an image as a Watermark and then draw rectangles
                        over top of the image.
                        
  06/03/04  G Campbell  Misc minodr bug fixes as identified by Peter Kiss
      
                        Fixed FOR EACH TT_pdf_ReplaceTxt.  Where clause was
                        incorrect.  
                       
                        Missing delete of TT_pdf_tool and  TT_pdf_tool_param
                        in the Reset procedures.
                            
                        In pdf_load_image, set the image_file equal to the
                        SEARCH(imagefile) so that it will be correctly found
                        in pdf_load_images procedure.

  06/03/04  G Campbell  Added LoadExternalXObjects.  Now handles the inclusion
                        of JPEG images when importing a PDF page.
                        
                        renamed pdfextract.i to pdfglobal.i

  06/04/04  G Campbell  Added code to handle inclusion of images when loading
                        a page from an external PDF page.  Has been tested 
                        against PDF documents with GIF and JPEG images 
                        embedded.

  06/10/04  G Campbell  Reworked logic to handle Adobe Distiller v5 and v6 plus
                        modified code to handle the inclusion of the external
                        page onto a separate page.

  06/10/04  G Campbell  Added pdf_rgb routine from Robert Ayris [rayris@comops.com.au]

                        This routine allows you to set colours based on the 
                        Hex or RGB colour values.  The procedure accepts 3
                        parameters:
                        
                        1. Stream Name   - Self-Explanatory
                        2. Function Name - One of:
                                             pdf_text_color
                                             pdf_stroke_color
                                             pdf_stroke_fill
                        3. Colour        - Either a hex value (begining with 0x)
                                           or a 9 digit RGB colour
                                           
                                           eg: hex = 0x006466 (Teal Green)
                                               rgb = 000100102 (Teal Green)

  06/11/04  G Campbell  Added function pdf_font_loaded
  
                        Lets you determine whether a font id has already
                        been loaded or not.  Returns TRUE if ID is loaded or 
                        FALSE if not.

  06/11/04  G Campbell  Modified how the VerticalSpace functionality worked.
  
                        Nobody seemed to be using it, so I implemented the
                        changes required by Robin Smith of Comops Australia
                        (since he seems to be the only person using it).  This
                        also include removing the default setting during the
                        pdf_init_param procedure and setting the return value
                        to 0 in pdf_VerticalSpace (if parameter not found).

  06/15/04  G Campbell  Added code to handle the new MATRIX tool (in pdftool.p)
  
  06/15/04  G Campbell  Added rudimentary XML handling.
                        See procedure pdf_load_xml ... which loads an XML file
                        into the TT_pdf_xml temp-table (defined in pdfglobal.i).
                        
                        You can then use the TT_pdf_xml temp-table (and buffers) 
                        to extract and place the XML data.  See the example in
                        /samples/super/xml.p
                        
                        Also, added GetXMLNodeValue which allows you to retrieve
                        a specific Node value (also used in xml.p).

  06/16/04  G Campbell  Added code to handle the /Encoding and /FontFile3 
                        operators of a Font Dictionary in an opened PDF file

  06/25/04  G Campbell  Copy fonts when merging streams .. as per Peter Kiss
  
  06/28/04  G Campbell  Removed use of TT_pdf_content temporary table. Now
                        writes the page contents out to a text file.  During
                        testing, this seemed to improve the performance.

  07/07/04  G Campbell  Added procedure pdf_fill_text which allows you to 'fill'
                        defined placeholders in an existing PDF document with
                        some data. The placeholders are Adobe Text Form Fields.

  07/15/04  G Campbell  Fixed bug: Was not using obj_stream when counting Pages
                                   in TT_pdf_page.  Thanks to Peter Kiss for
                                   noticing this.

  07/28/04  G Campbell  With the change to using text files (instead of 
                        TT_pdf_content) this introduced an issue when using
                        the pdf_merge_stream functionality.  After some changes
                        and testing, it has been resolved.

  07/30/04  G Campbell  Updated to allow for usage of multi-page PDF template
                        documents with Form Fields.

  08/04/04  G Campbell  Added procedure pdf_fill_multiline.
                        
                            This is used in conjunction with the filling of 
                            Adobe form fields.  Will place multiline text into 
                            the field placeholder using the form field rectangle
                            boundries to wrap/display the text.

                        Added function pdf_text_widthdec2

                            This is used to determine the decimal width of text.
                            Similar to pdf_text_widthdec but you pass in which
                            Fonttag and FontSize you want to use to determine
                            the text width.

  08/05/04  G Campbell  Added published events:

                        MaxPDFPage      - publishes the total number of pages
                                          for a stream
                        BuildPDFPage    - publishes the current page number in
                                          the build process
                        GeneratePDFpage - publishes the current page number when
                                          generating the PDF content

  08/31/04  G Campbell  Modified how the pdf_set_page was being run.  Now it
                        resets the appropriate elements to ensure that if you
                        use this procedure to 'change pages' midstream then the
                        appropriate changes occur.  Basically re-opens the 
                        original page output for re-use placing the Text and 
                        Graphic X/Y coordinates to the appropriate locations 
                        (as if you were re-starting the page .. ie: pdf_new_page)
                        
                        This is useful if you wanted to reprocess pages after 
                        the complete document was done.  For example, allows you
                        to programattically control the "Page 1 of 4".  See
                        example samples/super/multipage.p.

  09/13/04  G Campbell  Added code to accommodate landscape PDF templates.

  09/24/04  G Campbell  Modified how pdf_get_image_wh was determining the image
                        width and height.  Now uses a MEMPTR instead of reading
                        the JPEG file byte-by-byte ... thanks to Sebastian 
                        Lacroix for updating my original readjpeg.w to perform
                        better.

  09/27/04  G Campbell  Added function pdf_text_fontwidth2.

                        Same as pdf_text_fontwidth but it also accepts a Font
                        Size when determining the text width.

  09/28/04  J Johnson   Added function pdf_get_pdf_info
                        NOTES:
                            This funciton is different than pdf_get_info
                            pdf_get_info gets a value for the pdf that you
                            are CREATING
                            pdf_get_pdf_info gets a value from a pdf file
                            loaded with pdf_open_pdf.

                         Returns information about a pdf loaded with pdf_open_pdf 
                         i.e.  numPages = integer(pdf_info("SPDF",1,"pages"))     
                               author   = pdf_info("SPDF",1,"author")             

                         Possible info values are:
                            Pages    - total number of pages in imported PDF
                            Author   - who created the original PDF document
                            Producer - what product created the original PDF 
                                       document
                            Creator  - what procedure created the original PDF
                                       document
                            Title    - the original PDF document title
                            Subject  - the original PDF document Subject
                            Keywords - Keywords contained in the original PDF
                                       document
                            ModDate  - the last Modified Date of the orignal
                                       PDF document
                            ModTime - the last Modified Time of the original
                                      PDF document
                            CreationDate - the creation date of the orignal
                                           PDF document
                            CreationTime - the creation time of the original
                                           PDF document

                        The 'Pages' info is always available for all imported
                        documents but the additional information may or may not
                        be available (depends on if it was included in the 
                        original document or not).

  10/07/04  G Campbell  Added logic to handle multiple external pages with
                        the same Xobject names (eg: using the same image names
                        such as /Img1)

                        Added new parameter UseExternalPageSize.  This will
                        use the MediaBox as identified in the original PDF
                        document, overwriting the current pages Width/Height
                        settings.

  10/12/04  G Campbell  Removed call to zlib.p from zlib.i and placed it into
                        pdf_close procedure.  It isn't needed before that and
                        some people don't use the 'compress' option.

  10/19/04  G Campbell  Added new parameters called ScaleX and ScaleY.  These
                        allow you to specify the scaling of a font when using
                        the XY placement of text elements (eg: using pdf_text_xy
                        procedure).  The default for each of the parameters is
                        1 (scale same as point size).  To double the vertical 
                        size (or height) then:

                           RUN pdf_set_parameter("Spdf","ScaleY","2").

  11/02/04  G Campbell  When parsing AFM file ensure that the afm_LastChar is
                        set to a value greater than 0.  This is because some
                        AFM files set the unused characters to -1 and this
                        causes issues when defining the Font Descriptor.

  12/03/04  G Campbell  Added code to the pdf_load_external procedure that
                        allows us to import external PDF page content that is
                        greater than 31K.  Look at the t4form.p example.  If it
                        didn't have this updated code the Canadian flag image
                        wouldn't appear.

  12/08/04  G Campbell  Added OPSYS specific code to handle issue of using
                        backslashes in the pdf_replace_text procedure.

  01/25/05  G Campbell  Fixed issue with pdf_get_wrap_length.  It now mimics
                        the pdf_wrap_text procedure when determining when the
                        lines of text should break.

  03/09/05  G Campbell  Fixed issue with multiple streams being output at the
                        same time ... data from one stream was being included
                        into another stream.
                        
  03/14/05  G Campbell  Update pdf_set_parameter to allow setting of Temporary
                        parameter values.  These can be used to store some
                        stream specific data in your procedure.  For example,
                        if you want to store the default Base Font Size of a 
                        stream then set via:
                        
                        RUN pdf_set_parameter("MyStream","tmpBaseFontSize","8.0").
                        
                        You can then retrieve this value via the function named
                        pdf_get_parameter.
                        
                        IMPORTANT NOTE: Al temporary parameter values must begin 
                        with 'TMP" (case insensitive).
                        
  03/15/05  G Campbell  Removed the code that output the Text Colour and Font
                        each and every time in the pdf_text_xy_dec routine. 
                        This may get removed from other routines also ... but
                        for now, just the one.

  04/14/05  G Campbell  Added code to handle the cropping of external PDF pages
  
  04/25/05  G Campbell  Added VERSION parameter.  Can only be used with the
                        pdf_get_parameter procedure.  
                        
                        Also, code to handle DescendantFonts is introduced. This
                        basically occurs when you create a document in Word 
                        that includes a 'Symbol' (font).  Adobe Professional 
                        creates these as DescendatFonts which was causing an
                        issue when loading an external PDF doc.  Now, it Seems 
                        to work for most instances (or at least the ones that I
                        created anyway).

  05/18/05  G Campbell  Added code to handle new TABLE setup parameters added
                        by James McAteer (eg: UseFields).

  06/01/05  G Campbell  Added a {&pdfskip} before the endstream when importing
                        external PDF documents.  This is because the last
                        command in the page stream may not have a line feed
                        and therefore the endstream and the last command
                        ran together (creating an invalid statement like
                        ETendstream)
                        
  07/06/05  G Campbell  Locally included the Zlib Compression functionality.  
                        No longer in external procedures.

                        Locally includes the Encryption functionality.
                        No longer in external procedures.

  08/03/05  J Johnson   Fixed pdf_insert_page: when pdf_set_page was called,
                        so that the problem with the
                        the page output stream had being already closed,
                        causing "attempt to write to closed stream" errors
                        Now the stream is closed, the pages re-numbered,
                        and the stream is re-opened so that pdf_set_page
                        can handle the stream normally

  08/24/05  G Campbell  Reworked how Xobjects with contents greater than 32K
                        were concatenated (see tempcat.txt).

  09/22/05  G Campbell  Fixed how the External Resource Font list for Xobjects 
                        was being built (this included add ext_page field to a
                        couple of Temp tables)

  03/02/06  G Campbell  Added code to allow for inline tagging.  That is, when
                        text is output to the PDF that contains a tag (<b>)
                        ensure that the correct formatting occurs.
                        
                        Added Parameters:
                        
                        TagColor:<name> = Sets the name of a valid colour using
                                          comma-delimted RGB values.
                                          
                                          Example.
                                          
                                          RUN pdf_set_parameter("Spdf",
                                                                "TagColor:Black",
                                                                "0,0,0").
                                                                
                        UseTags        = Possible value are: "TRUE" or "FALSE"
                        BoldFont       = name of a valid font
                        ItalicFont     = name of a valid font
                        BoldItalicFont = name of a valid font
                        DefaultFont    = name of a valid font
                        DefaultColor   = name of a previously defined TagColor
                                                
                        If TRUE then accommodate inline tagging of text,
                        otherwise don't.  Is processed during page build time 
                        (not at Close/Write) therefore can to turned on/off
                        during program processing (just in case you had sample
                        HTML you wanted to output).

                        Handles Tags for:
                        
                        <b></b> - Bold
                        <i></i> - Italic
                        <color=<ColorName></color> - setting color
                        
                        *** See samples/super/usetags.p for example of usage.

  02/19/10  JC Cardot   Add functions for binary manipulations
                        (or, and, lshift, rshift). Changed existing function
                        name BinaryXOR to bin_xor.
                        
                        Add function utf8_to_utf16be to transform an utf-8 string
                        into an utf-16be one (utf-8 is not in the
                        pdf specification). First step in utf-8 support for PdfInclude.

                        The document properties (Author, Creator, Producer,
                        Title, Subject, Keywords) are now outputed as utf16-be,
                        thus allowing every possible character.

                        Added support for national codepages in build-in
                        base 14 Type 1 fonts.
                        Based on the work of Tomasz Judycki (02/10/07)
                        http://www.tv.com.pl/stepbystep/pdfinclude/
                        
                        Added pdf_wrap_text_xy and pdf_wrap_text_xy_dec to
                        print text within a box, with left, right or center
                        justification.
                        
                        pdf_load_font, pdf_load_image & pdf_open_pdf now return
                        silently if called twice with the same arguments. As per Lonny L. Granstrom 21/12/2006
                        
                        pdf_close now returns the errors as RETURN-VALUE instead
                        of calling MESSAGE.
                        
                        optimisation: add a used_flag to TT_pdf_font and make use
                        of it to output only the used fonts. Had to rework pdf_xref:
                        when an object is free (not used), tell it correctly in
                        the cross reference table.
                        
                        bug fix: as per Mike Smith 10/7/2007 add fields in the
                        obj_stream index of TT_pdf_font in order to fix the query
                        FOR EACH TT_pdf_font in pdf_fonts in some cases
                        
                        bug fix: in pdf_new del/rm *.txt replaced by os-delete
                        (no more OS message in the standard output)
                        
                        bug fix: the CreationDate property now respects the pdf specification
                        
                        bug fix: as per Stefan Le Jeune 10/7/2007, add OPSYS in
                        pdf_replace_text to handle backslashes correctly. Seems
                        that Gordon did this on 12/08/04 but it had disapeared.
                        
                        bug fix in LoadExternalObjects to correctly process
                        external objects (images, fonts...) loaded from a pdf
                        template -> Can now import pdfs created with OpenOffice
                        containing images or fonts.
                        
                        bug fix: in pdf_set_tool_parameter for the integer  and
                        decimal parameters, the dot has to be replaced by
                        SESSION:NUMERIC-DECIMAL-POINT, when SESSION:NUMERIC-FORMAT
                        is "EUROPEAN", else for example, passing
                        ".5" INTO an EUROPEAN session will lead to 5 instead of 0,5
                        
                        bug fix: pdf_note, pdf_markup a pdf_stamp did not manage
                        correcly decimals with EUROPEAN session -> use dec2string
                        
                        bug fix: @@PageNo & @@TotalPages did not work in batch
                        mode. Fixed as per Jayson Johnson 6/12/2006.
                        
                        new feature: pdf_load_links modified so that the contents
                        of annotations (note, markup, stamp) is utf16be.

  02/19/10  JC Cardot   bug fix: in utf-8 sessions the binary characters in the
                        second line (comment) of the generated pdf file did not
                        appear.
                        
                        bug fix: in utf-8 sessions, utf8_to_utf16be was not
                        working properly.
                        
                        TODO: check and eventually change all ASC and CHR to use
                        the proper codepage.
                        
                        bug fix: as per Donato 3/11/2010, replace STRING by
                        string2dec in pdf_rectdec.
                        
                        bug fix: as per Donato 3/11/2010, change pdf_text_center
                        parameters from INTEGER to DECIMAL.
                        
  04/11/10  JC Cardot   fix pdf_set_base14_codepage: if the code page is 1252
                        then treat it as iso8859-1 ; also empty tt_pdf_diff else
                        the differences could be messed up when generating various
                        pdf files while calling pdf_set_base14_codepage for each one.

  07/07/10  JC Cardot   rewrite pdf_text_rotate according to pdf specification
                        in order not to skew the text (note that it does not render
                        the way it did before, the code using this functionality
                        might have to be adapted).
                        
                        implement pdf_text_scale to scale text on the x or y axis.
                        
                        implement pdf_text_skew to skew the x and/or y axis.
                        
                        fix pdf_set_base14_codepage call to pdf_font_diff_width:
                        ASC() must be called with SESSION:CPINTERNAL as "from"
                        code page, else it might return -1.
                        
                        add a new parameter "CodePage" to convert the strings
                        to this code page before printing them in the pdf.
                        Usefull when running in an utf-8 session, because the text
                        has to be converted to this codepage before being written
                        in the pdf file.
                        
  07/09/10  JC Cardot   Fix pdf_text & pdf_set_textY to handle the angle 90°
                        normally (not a special case anymore).
                        Fix bad placements bugs.

  01/14/11  JC Cardot   fix: filling forms did not work in european sessions
                        fix: external fonts: width is copied from base font when
                        it is possible. Used also for filling forms.
                        new: modified pdfExtract.p so that filling OpenOffice.org
                        generated forms is now supported!

  01/16/11  JC Cardot   fix: page rotate has been hardcoded to zero. Now use the
                        value defined by the user. Thanks to Phil Freed for pointing
                        this out.
                        fix: the fonts used to fill the forms was missing in the
                        resources of the pdf.

  01/20/11  JC Cardot   fix a lot of bugs in the exportation of the external fonts
                        new: the external fonts are now exported only once! This
                        results of course in much smaller pdf files.

  02/01/11  JC Cardot   better font and shading support, + ensure the FONT related
                        objects have a correct object number.

  02/15/11  JC Cardot   Rewrite the part concerning the external pdfs (see comment
                        in PDFExtract.p) - dropped a lot of code

  03/01/11  JC Cardot   Bug fixes so that we can use more than one external pdf
                        (see samples/super/combination*.p)

  04/28/11  JC Cardot   Bug fix: mediaBox and cropBox did not work properly when
                        the values were decimal

  05/25/11  JC Cardot   Fix processFillText and pdf_fill_MultiLine for decimal
                        positions and sizes (use string2dec() instead of DECIMAL()
                        and dec2string() instead of STRING())

  09/30/11  JC Cardot   TODO: fix bug in pdf_text_width* (as done for *dec2)
                        for backslashed strings

  10/21/11  JC Cardot   new functionalities
                        * in pdf_load_image, new procedure pdf_extract_image_info replaces
                          pdf_get_image_wh and allows to load not only jpg files, but
                          also png (including transparency, either one transparent color
                          or a full alpha channel). modified also pdf_load_images
                          to support for the new information.
                        * pdf_place_image[2] now uses the image dimensions
                          if they have not been specified as parameters (set to the ? value)
                        * by default, the generated PDF is v1.4. Calling new procedure
                          pdf_set_MinPdfVersion allows to set a minimum version.
                          Currently used when using 16 bits png (support starts with PDF v1.5)
                        * added pdf_get_widgets API to return the list of fillable
                          fields in a pdf form

                        bug fixes
                        * pdf_text_widthdec2 had bugs when the string contained parenthesis
                          (because it was protected by a backslash). Fixed.
                        * if something went wrong in pdfextract.p and the file for an external
                          page has not been generated, does not crash anymore

                        internal modifications:
                        * function GetWidgetOption rewritten to parse strings key=value[,key=value]*
                          (get the value based on the key)
                        * new function basename to return only the file name from a full path
                        * new procedure pdf_unreplace_text to perform the reverse action
                          as pdf_replace_text (used to fix pdf_text_widthdec[2])

  03/13/12  JC Cardot   Added support for 1254 code page (turkish) - new file
                        1254.diff

  04/10/12  JC Cardot   bug fixes:
                        * pdf_load_external: fix bug for utf-8 sessions
                        * LoadExternalXObjects: fix bug when the external pdf template contains two
                          images with the same name

  08/02/12  JC Cardot   new functionalities
                        * New transaction mecanism, using pdf_transaction_begin/rollback/commit
                          The use of transactions is demonstrated in pdftool.p, for the TABLE tool.
                          The MATRIX tool demonstrate the use of pdf_transaction_buffer.
                        * pdf tools: implement wrap in cells for the TABLE & the MATRIX tools.
                        * the tag functionality now allows for underline <u>, strike <s> and links <url=>.
                          This also works for rotated text.
                          <url=> can be used for http:// links but also to create internal links. In order
                          to achieve this, create a bookmark (named for example "my_bookmark"),
                          then use <url=#my_bookmark>my link</url> whenever you want to create a link.
                        * add pdf_get_parameter2, same as pdf_get_parameter with a default value
                          (also added pdf_get_tool_parameter2)
                        * pdf_wrap_text_xy[_dec] now returns (as return-value not to break the API)
                          the Y coordinate of the last line
                        * new procedures pdf_incr_parameter and pdf_decr_parameter to increment/decrement
                          any integer parameter
                        * Fill pdf forms: take into account the field justification (left, center, right)
                          as defined in the template; also make use of the "multiline" flag defined in the
                          template. Adjust the text position within the fields.
                        * Fill pdf forms: add the ability to fill checkboxes and radio buttons.
                          Only one radio button may be checked within one given radio group.
                        * Fill pdf forms: add support for combo and list boxes.
                        * Fill pdf forms: it is now possible to retain the original form in the created
                          document! This allows e.g. the creation of pre-filed forms.
                          See the "retainAcroForm" parameter.
                        * It is now possible to retain the annotations from the original pdf document (e.g.
                          links, sticky notes...) using the "retainAnnots" parameter.
                        * added _pdf_set_parameter_priv to set private parameters (by standard should begin with 3 underscores)

                        optimizations
                        * big rewrite to optimize the output of the text operators, up to 40% of reduction
                          in the size of the generated pdf (setTextOperator)
                        * idem for graphic operators (setGfxOperator)

                        bug fixes
                        * pdf_text_width[2] & pdf_font_width[2] now remove the tags before computing
                          the text width, using new private procedure pdf_strip_tags. The width is now
                          correct.
                        * pdf_wrap_text_xy[_dec]: when there was too much text to fit in the given box,
                          one line too much was printed.
                        * compression & decompression using zlib: trap and manage return codes <> 0 (zlib error)

                        refactoring
                        * pdf_new_page now calls pdf_new_page2 with the stream orientation, instead
                          of duplicating all the code
                        * PutStreamContent: create _PutStreamContent_flush in order to factorize the code
                        
                        API changes
                        * pdf_curve, pdf_circle & pdf_ellipse now take their parameters as DECIMALs
                        * [de]compressfile return value is now integer instead of logical; it returns zlib
                          return code.
                          
  10/03/12  JC Cardot   new functionalities
                        * integration with PLOP (PDF Linearization, Optimization, Protection)
                          from PDFlib GmbH (PSP has been made obsolete), using pdf_Encrypt.
                          AES-128 & AES-256 encryption is now available (along with RC4-128).
                        API changes
                        * pdf_encrypt API has changed to reflect changes between PSP & PLOP
                          Also the permission list is now separated by "," instead of ";".

  05/25/13  JC Cardot   new functionalities
                        * new parameter formFlattenWithDefaultValues allows to keep the form default values
                          when flattening the widgets (can-do list matching widget names)
                        * new parameter formFlatten (obsoletes retainAcroForm): CAN-DO list of form widgets'
                          names to flatten. Set to "" not to flatten anything (same as retainAcroForm = "TRUE").
                          If is now possible to selectively flatten some widgets and keep some others in the
                          resulting pdf.

  06/07/13  JC Cardot   bug fix: fix a regression in pdf_set_TextRed/Green/Blue
                        and pdf_set_FillRed/Green/Blue introduced in the 8/2/12
                        optimizations.

  07/10/13  JC Cardot   bug fixes:
                        * pdf_text_to was not positioning the text correctly for proportional fonts.
                          Changed FILL(" ",...) to FILL("E",...) to match pdfinclude 3 behaviour.
                        * pdf_text_at also was sometimes not positioning the text correctly.
                          Make use of Tm instead of TD to place the text.

  09/12/13  JC Cardot   bug fix: fix a stupid bug in pdf_text_to which had been introduced at the output
                        optimization time

  09/12/13  JC Cardot   bug fix: fix font widths for all variable fonts. Some (e.g. accentuated) characters
                        did not have any width defined.
                        Make Symbol and ZapfDingbats variable and add proper widths. 

  09/24/13  JC Cardot   pdf encryption: rewrote and optimized encryption code. Make use of new built-ins
                        MD5-DIGEST, HEX-ENCODE & HEX-DECODE for PROVERSION >= 10.
                        This rewrite allowed to make it work also in utf-8 sessions, and to make RC-4 128
                        encryption work (it had never worked before).

  10/06/13  JC Cardot   new functionalities:
                        * pdf_merge_stream: support merging streams using external pdf files. Modified
                          the clean-up code in order not to delete temp files (png images, pdf templates)
                          when they are still in use, in the original or the merged file.
                        * encryption: make it work with external pdf templates: stream encryption
                          (make use of OutputMemPtr in recursivelyExportObject) and strings encrytion
                          (new procedure exportScalar).
                        clean-up: PutFileContent as it only contained EXPORT STREAM S_pdf_inc mPtr.

  10/10/13  JC Cardot   moved the encryption routines back to pdfencrypt.p where it had once belong
                        (see 07/06/05 comment).

  10/14/13  JC Cardot   new functionality: AES-128 encryption is now supported natively. It does not
                        make use of any external dll, but OpenEdge >= 10 is needed.
                        A new parameter "EncryptAlgorithm" has been added, possible values are RC4 & AES.

  11/15/13  JC Cardot   new functionalities:
                        * new API pdf_ext_get_path allows to get any information from an external pdf,
                          using "pdf paths" like "/Root/Pages/Kids[1]/Cropbox" or - more useful -
                          /Root/Pages/Count.
                        * new API pdf_ext_get_page returns a string representation of a page object.
                          This page object can be used as a start for the path in pdf_ext_get_path.
                          (this is a shortcut for "/Root/Pages/Kids[n]")
                        * pdf_new() can now be called with ? as a filename, thus allowing to write
                          code to get information about an external pdf without the need of producing
                          any pdf file (see pdfinfo.p).
                        * the merge functionality now supports external pdf files
                        * the encryption functionality now supports external pdf files
                        * added support for CMYK & Grayscale JPEG pictures (before, only RGB was supported)
                        bug fix: when using pdf_load_template, the specified fonts were forgotten in
                        the pdf file.
                        other: add a "trace" functionality to debug non private procedures

  02/06/14  JC Cardot   new private APIs: putFileAsStream (using new CompressFile2Buffer) & putStringAsStream:
                        export a file (resp. string) as a pdf object, applying compression &
                        encryption if applicable.

  02/13/14  JC Cardot   refactor pdf_Header (see _pdf_Header_Item)
                        refactor all pdf_text_width* (removed a lot of duplicated code)
                        new functionality: Unicode support! It is now possible to use unicode fonts
                        and use all the available characters within a pdf file. Changes include:
                        * pdf_load_fonts now manages unicode fonts, /W (widths) is optimized
                        * pdf_ParseAFMFile can load .ufm files (the API has changed from many
                          parameters to one parameter buffer)
                        * unicode fonts widths are properly computed
                        * putString: new API to put a string to a pdf file, with encryption if necessary
                        * utf8_to_utf16be has been rewritten using codepage-convert. A lot of code
                          has been dropped. This procedure is now used whenever a unicode string
                          or unicode text is to be written to the pdf file
                        Unicode works now; however depending on the size of the ttf files, the generated
                        pdf can be huge. So... Font subsetting is on its way.

  03/17/14  JC Cardot   pdf_text_boxed_xy: the justification parameter is now used to right or
                        center align the text.

  04/09/14  JC Cardot   new functionality: two new boolean parameters "defaultHeader" and
                        "defaultFooter" allow to generate a default header (with title & author) and 
                        a footer (page number / total nb of pages). The header displays 2 lines,
                        defined in the "headerLine1" & "headerLine2" parameters (default values are
                        Title and Author) ; the footer displays one line, defined in "footerLine1"
                        (default value "page n/m").

  04/23/14  JC Cardot   new (?) functionality: dynamic justification for special @@ tags (@@PageNo
                        and @@TotalPages) to be correctly right or center aligned. This had been
                        done in 2004 by J. Johnson (see comments at 03/23/04) but dropped since.
                        Works with pdf_text_center, pdf_text_align & pdf_text_boxed_xy.
                        internal change: ChangePageText can now perform multiple replaces at once.

  04/28/14  JC Cardot   removed a lot of duplicated code by using pdf_get_tool_parameter2 & _pdf_set_parameter_priv

  05/02/14  JC Cardot   new private function _get_page_content_file to get the page content file name
                        (instead of duplicated code everywhere)
                        pdf_text_boxed_xy last parameter (line weight) is now DECIMAL.
                        new API:  pdf_PageNo() (like pdf_TotalPages()).
  
  05/08/14  JC Cardot   new functionality: patterns (or pdf XObjects). You can define blocks of text,
                        graphic, images, etc. and reuse them many times. The new api is:
                        pdf_pattern_begin, pdf_pattern_end and pdf_pattern_use. Example implementation
                        in pdf_default_header.
                        Note: factorized code with the transaction functionality: _pdf_save|restore_stream_parameters

  05/15/14  JC Cardot   cleanup: removed global variables, deleted unused procedure ConvChar2MemPtr.
                        removed a lot of commented code (see previous versions for an history).
                        merged getFileAsMemptr & GetFileContent into getFileAsMemptr.
                        removed GetFileSize. Factorize some code using getFileAsMemptr and putFileAsStream.
                        
                        optimization: implemented a cache for images. image.p goes from 9sec to 6sec.
                        (to clean the cache, empty the folder SESSION:TEMP-DIRECTORY/pdfcache)
                        
                        rewrote pdf_reset_(all|stream) using dynamic queries to shorten the code and ensure
                        all temp-tables are being cleared correctly.

  05/20/14  JC Cardot   new functionality & optimization: add a new parameter "reuseExternal" to enable
                        many uses of an external pdf file opened once. That is to say that the first generated
                        document will be done using pdf_new/pdf_open_pdf/pdf_use_pdf_page/pdf_close whereas
                        all subsequent calls will be done without parsing again the template, like:
                        pdf_new/pdf_use_pdf_page/pdf_close - see adobe-exemple.p sample.
                        Generated file for external pdf templates are now stored in its own subdirectory
                        within SESSION:TEMP-DIR (fixes some bugs, like template name clash)

  05/22/14  JC Cardot   profiled the code to find that exporting a large memptr was sloooow. This was
                        due to UNBUFFERED being specified in the OUTPUT TO statement for the construction
                        of the pdf file. Removing it caused an enormous gain in performance.

  06/05/14  JC Cardot   fix bug in pdf_use_pdf_page: scope tt_pdf_external locally to the procedure
                        (we were getting old records in some cases)

  06/07/14  JC Cardot   factorize code for pdf_wrap_text_xy*; when pdf_wrap_text_xy* triggers a page
                        change, wrap nicely at the top of the next page

  06/12/14  JC Cardot   fix bugs: when changing page automatically, do not restore Y position from previous page

  06/30/14  JC Cardot   new functionality: <font> tag. Usage: <font=myFont>some text</font>, where
                        "myFont" has been loaded previously.

  07/03/14  JC Cardot   New great functionality: Font subsetting! add "|SUBSET" to the font name in
                        pdf_load_font, and enjoy the pdf file size :)
                        2 new APIs to add some characters to the font subset: pdf_subset_add_string
                        and pdf_subset_add_range.
                        Subsetting should not be used if the pdf file is to be modified later, because
                        the font will be missing characters. It still can be modified if the font is
                        installed on the computer where the modification is done.

  09/24/14  JC Cardot   Ensure there is always a newline before each "endstream" to be consistent
                        with the pdf spec.
                        Instead of hardcoding iso8859-1, compute a default code page for code page
                        conversions.
                        When outputting the widths of characters for unicode fonts (/W), do not
                        output widths for characters with the default width (optimize pdf size).
                        In the encryption dictionnary, add /Length for AES else Ghostscript cannot
                        render the pdf file (/Length is optional according to the pdf spec).

  10/16/14  JC Cardot   * Filling forms: the caller can now specify which font and size to use, thus
                          not using the font defined in the template (option font=myFont,fontSize=nn
                          of pdf_fill_text()). This also works with unicode fonts (which of course
                          must have been loaded previously) and font subsets.
                          The code has been reworked / moved (see pdf_Form_Widgets_Content) and some
                          bugs fixed.
                        * Add pdf_(set_)Stroke(Red|Green|Blue)
                        * Moved pdfextract.p y pdfencrypt.p to the lib/ subdirectory
                        Bug fixes:
                        * using <url> changed the current stroke color
                        * bookmarks/links were sometimes corrupted
                        * when using tags and mixing unicode and non-unicode fonts, the text could
                          be corrupted. Fixed by moving pdf_replace_text() at the "right place".

  10/21/14  JC Cardot   * new API: pdf_load_font2(): load a font without the afm/ufm file. The font
                          parsing is full ABL, done in lib/pdf_parse_font.p.
                        * Make font subsetting work with non unicode fonts
                        * Moved some functions and procedures related with memptr, binary manipulations
                          and hexadecimal to inc/pdf_func_bin.i
                        * Moved tt_pdf_error and pdf_error() to inc/pdf_func_error.i
                        * use parseText() to decode the form field names - now an include file.
                        * TT_pdf_page.page_width & height are now DECIMAL instead of INTEGER
                          (in particular it allows to "UseExternalPageSize" correctly).

  11/06/14  JC Cardot   * new functionality: support for BMP images (1, 4, 8 & 24 bits tested)
                        * some bug fixes and refactoring for external pdf files. Better support for
                          external 270º rotated pages.

  11/25/14  JC Cardot   * new functionality: support for GIF images (only 8 bits, non interlaced)
                        * moved the image extracion code in lib/pdf_image_(jpg|png|bmp|gif).p
                        * bug fix: when wrapping text and creating automatically a new page, the
                          underline, strike or color from the tags propagated to the next header/footer.
                        * moved compression procedures to inc/pdf_func_compress.i

  02/03/15  JC Cardot   * new functionality: the default header can now be updated within the same pdf
                          file, if one of the parameters it uses change between two headers (Title,
                          Author, headerLine1, headerLine2, headerSeparator).

  03/31/15  JC Cardot   * move pdftool.p and pdftemplate.p to lib
                        * move pdfglobal.i, pdf_func.i & pdfdebug.i to inc
                        * new preprocesor xcPDFIncVersion
                        * transaction rollback: did not rollback temp-tables (tt_pdf_object, TT_pdf_annot,
                          TT_pdf_bookmark & TT_pdf_FillTxt)
                        * default header: added parameter headerNotOn1stPage
                        * footers are now created in pdf_close at the very end of the pdf generation,
                          in order to ensure they are the last thing printed on each page,
                          and that each page has got one. This might change the result for some documents
                          for custom footers, as they will be called at a different moment.
                        * add pdf_pattern_search_by_key
                        * add pdf[_set]_RightMargin
                        * pdf_set_GraphicX & Y: allow negative values
                        * new API pdf_wrap_text_x (same as pdf_wrap_text but with pdf points instead of chars
                        * new parameter headerLogo "picture_file_path|picture_width|picture_height|alignment"
                          for the default header
                        * spaces in <url> links can lead to strange behaviour: the caller must replace
                          them with %20
                        * handle chr(0) in changePageText

  04/20/15  JC Cardot   * new parameter insertPageMode: append,insert,next. By default append. When
                          we go past the page footer, instead of always creating a new page (append)
                          we can now insert a page, o just go to the next page.
                        * new API pdf_close_path2 (close a path without filling it)
                        * pdf_set_dash now resets the dash when called with two zeroes
                        * new API pdf_set_dash_pattern (allows to set the pattern of dashed lines)
                        * pdf_insert_page now publishes "InsertPDFPage"
                        * bug fix: make sure the pages are output in the right order!
                        * new API pdf_place_pdf_page: like pdf_use_pdf_page but with an extra options
                          parameter with values for Scale,X,Y,Rotate,Background,Border,BorderWidth;
                          allows to embed an external pdf file as if it were a picture

  05/05/15  JC Cardot   * LoadRessources: output only ressources which page has been used (or global ones)
                        * pdf_move_to(): arguments are now DECIMAL instead of INTEGER

  06/23/15  JC Cardot   * pdf_PageFooter: if called with "" as the procedure, then consider that the
                          page has a footer not to add one later in pdf_close: it is the dev intention
                          not to have a footer

  09/23/15  JC Cardot   Bug fixes:
                        * pdf_reset_stream: at pdf close time, do not delete external pdf resources
                          when they are still used in another non closed stream
                        * pdf_merge_stream_external: TT_Info was not copied. Fixed.
                        New funcionalities:
                        * pdf_open_pdf: if the same pdf has already been opened for another stream,
                          then no need to parse it again, we just copy it to the other stream.
                          -> huge optimization for this use case
                        * pdf_open_pdf: implemented a cache for external pdfs! New parameter
                          "usePdfCache" (TRUE by default). Whenever a given pdf has been opened once,
                          it will subsequently be loaded from the cache.
                          -> huge optimization!
                        * pdf_clear_pdf_cache: new API to remove the cache files for a given pdf template.
                        * recursivelyAssign* & Export*: only export the same dictionnary or array once.
                          Useful in case of merged documents: this makes them smaller by deduplicating
                          the common objects.

******************************************************************************/

&GLOBAL-DEFINE xcPDFIncVersion 5.1.12

{inc/pdfglobal.i "SHARED"}

/* &GLOBAL-DEFINE debug YES. */
/* &GLOBAL-DEFINE trace YES. */
{inc/pdfdebug.i}
/* &GLOBAL-DEFINE debugComment + "  % " + {&callStack} */

&GLOBAL-DEFINE BoldOnChar       CHR(1)
&GLOBAL-DEFINE BoldOffChar      CHR(2)
&GLOBAL-DEFINE ItalicOnChar     CHR(3)
&GLOBAL-DEFINE ItalicOffChar    CHR(4)
&GLOBAL-DEFINE ColorOnChar      CHR(5)
&GLOBAL-DEFINE ColorOffChar     CHR(6)
&GLOBAL-DEFINE UnderlineOnChar  CHR(7)
&GLOBAL-DEFINE UnderlineOffChar CHR(8)
&GLOBAL-DEFINE StrikeOnChar     CHR(11)
&GLOBAL-DEFINE StrikeOffChar    CHR(14)
&GLOBAL-DEFINE LinkOnChar       CHR(15)
&GLOBAL-DEFINE LinkOffChar      CHR(16)
&GLOBAL-DEFINE FontOnChar       CHR(17)
&GLOBAL-DEFINE FontOffChar      CHR(18)

/* see PDF 32000-1:2008 page 112 table 51 */
/* DEFINE VARIABLE cTextPosOp   AS CHARACTER   CASE-SENSITIVE INITIAL "Td,TD,Tm,T*" NO-UNDO. */
/* DEFINE VARIABLE cTextShowOp  AS CHARACTER   CASE-SENSITIVE INITIAL "Tj,~',~",TJ" NO-UNDO. */
/* DEFINE VARIABLE cTextStateOp AS CHARACTER   CASE-SENSITIVE INITIAL "Tc,Tw,Tz,TL,Tf,Tr,Ts,rg,g" NO-UNDO. */

DEFINE VARIABLE glNoOptimize AS LOGICAL NO-UNDO. /* 02-AUG-2013 jcc: do not optimize the pdf output */
DEFINE VARIABLE glUnitTest   AS LOGICAL NO-UNDO. /* 09-AUG-2013 jcc: unit tests */

/* ---------------------------- Define TEMP-TABLES -------------------------
   The temp-tables are used to store the PDF streams and resources used when
   generating a PDF document */

/* 13-MAY-2014 jcc: when adding a new temp-table do not forget to empty it in pdf_reset_(all|stream))! */

DEFINE TEMP-TABLE TT_pdf_stream NO-UNDO
    FIELD obj_stream         AS CHARACTER
    FIELD obj_file           AS CHARACTER
    FIELD obj_encrypt        AS LOGICAL     INIT FALSE
    FIELD obj_master         AS CHARACTER
    FIELD obj_user           AS CHARACTER
    FIELD obj_access         AS CHARACTER
    FIELD obj_key            AS INTEGER
    FIELD obj_compat         AS CHARACTER /* pdf version: will select the best encryption algorithm available */
    FIELD obj_silent         AS LOGICAL     INIT FALSE
    FIELD obj_mode           AS CHARACTER
    /* FIELD obj_CallProc       AS HANDLE */ /* 22-APR-2014 jcc: one call proc handle per procedure is better */
    FIELD obj_CallProcFooter AS HANDLE
    FIELD obj_footer         AS CHARACTER
    FIELD obj_footer_flag    AS LOGICAL    /* 23-JUN-2015 jcc: new */
    FIELD obj_CallProcHeader AS HANDLE
    FIELD obj_header         AS CHARACTER
    FIELD obj_CallProcLast   AS HANDLE
    FIELD obj_last           AS CHARACTER
    FIELD obj_id             AS CHARACTER
    FIELD obj_EncryptDict    AS INTEGER
    FIELD obj_UniqueID       AS CHARACTER
    FIELD obj_P              AS INTEGER
    FIELD obj_DoingText      AS LOGICAL
    FIELD obj_DoingGraphic   AS LOGICAL
INDEX obj_stream  AS PRIMARY
      obj_stream.

/* The following temp-table is used to store/track parameters per stream */
DEFINE TEMP-TABLE TT_pdf_param NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD obj_parameter AS CHARACTER
    FIELD obj_valid     AS CHARACTER
    FIELD obj_value     AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream
      obj_parameter.

DEFINE TEMP-TABLE TT_pdf_page NO-UNDO
  FIELD obj_stream         AS CHARACTER  
  FIELD page_nbr           AS INTEGER    
  FIELD page_rotate        AS INTEGER    
  FIELD page_width         AS DECIMAL     /* 27-OCT-2014 jcc: INT -> DEC */
  FIELD page_height        AS DECIMAL     /* 27-OCT-2014 jcc: INT -> DEC */
  FIELD UseTotalPages      AS LOGICAL     /* the page uses the @@TOTALpages tag */
  FIELD UsePageNo          AS LOGICAL     /* the page uses the @@PageNo tag */
  FIELD page_crop          AS CHARACTER  
  FIELD widget_flatten     AS CHARACTER   /* 25-MAY-2013 jcc: list of flattened widgets' names */
  FIELD widget_retain      AS CHARACTER   /* 25-MAY-2013 jcc: list of not flattened widgets' names */
  FIELD has_footer         AS LOGICAL     /* 31-MAR-2015 jcc: footer has been displayed... */
  FIELD has_footer_transac AS LOGICAL     /* 31-MAR-2015 jcc: ... during a transaction */
INDEX obj_stream AS PRIMARY
      obj_stream
      page_nbr.

/* 12-DEC-2014 jcc: new tt to store which template page has been used on which page:
   replaces TT_pdf_page.page_use because more than one template might be used on the same page,
   also replaces TT_pdf_external.page_id for the same reason. */
DEFINE TEMP-TABLE tt_pdf_page_use_ext NO-UNDO
 FIELD obj_stream AS CHARACTER
 FIELD page_nbr   AS INTEGER
 FIELD pdf_id_use AS CHARACTER   /* 02-DEC-2014 jcc: id of the template used in this page */
 FIELD page_use   AS INTEGER     /* page of the pdf template */
 INDEX ix1 obj_stream page_nbr
 INDEX ix2 obj_stream page_use.

DEFINE TEMP-TABLE TT_pdf_tool NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD tool_name    AS CHARACTER
  FIELD tool_type    AS CHARACTER
  FIELD tool_handle  AS HANDLE
INDEX obj_stream AS PRIMARY UNIQUE
      obj_stream
      tool_name.

DEFINE TEMP-TABLE TT_pdf_tool_param NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD tool_name    AS CHARACTER
  FIELD tool_param   AS CHARACTER
  FIELD tool_col     AS INTEGER
  FIELD tool_value   AS CHARACTER
INDEX obj_stream AS PRIMARY UNIQUE
      obj_stream
      tool_name
      tool_param
      tool_col.

/* The following temp-table is used to build a list of objects that will appear
   in the PDF document */
DEFINE TEMP-TABLE TT_pdf_object NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD obj_nbr     AS INTEGER
  FIELD obj_desc    AS CHARACTER
  FIELD obj_offset  AS DECIMAL DECIMALS 0 FORMAT "9999999999"
  FIELD gen_nbr     AS INTEGER FORMAT "99999"
  FIELD obj_type    AS CHARACTER FORMAT "X"
  FIELD obj_page    AS INTEGER
  FIELD obj_extra   AS CHARACTER
  FIELD in_transaction AS LOGICAL
INDEX obj_stream AS PRIMARY
      obj_stream
      obj_nbr
INDEX ix    obj_stream obj_page obj_desc
INDEX ix2   obj_stream obj_desc obj_page
INDEX ix3   obj_stream obj_page in_transaction.

/* The following temp-table is used to build a list of Bookmarks that will appear
   in the PDF document */
DEFINE TEMP-TABLE TT_pdf_bookmark NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD book_obj     AS INTEGER
  FIELD book_nbr     AS INTEGER
  FIELD book_title   AS CHARACTER
  FIELD book_parent  AS INTEGER
  FIELD book_expand  AS LOGICAL
  FIELD book_child   AS INTEGER
  FIELD book_first   AS INTEGER
  FIELD book_last    AS INTEGER
  FIELD book_page    AS INTEGER
  FIELD book_Y       AS INTEGER
  FIELD in_transaction AS LOGICAL
INDEX book_nbr AS PRIMARY UNIQUE
      obj_stream
      book_nbr
INDEX ix obj_stream book_page in_transaction.

/* The following temp-table is used to track Document Information */
DEFINE TEMP-TABLE TT_pdf_info NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD info_attr     AS CHARACTER
    FIELD info_value    AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream
      info_attr.

/* The following temp-table is used to track Links (Annotations)
   loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_annot NO-UNDO
    FIELD obj_stream     AS CHARACTER
    FIELD annot_type     AS CHARACTER
    FIELD annot_color    AS CHARACTER
    FIELD annot_content  AS CHARACTER
    FIELD annot_page     AS INTEGER
    FIELD annot_rect     AS CHARACTER 
    FIELD annot_border   AS INTEGER
    FIELD annot_style    AS CHARACTER
    FIELD annot_obj      AS INTEGER
    FIELD annot_icon     AS CHARACTER
    FIELD annot_add      AS CHARACTER /* For Additional Information */
    FIELD in_transaction AS LOGICAL
INDEX obj_stream  AS PRIMARY
      obj_stream annot_page
INDEX ix obj_stream annot_page in_transaction.

/* The following temp-table is used to track Images loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_image NO-UNDO
    FIELD obj_stream         AS CHARACTER  
    FIELD image_name         AS CHARACTER  
    FIELD image_file         AS CHARACTER  
    FIELD image_type         AS CHARACTER   /* 08-AUG-2011 jcc: added */
    FIELD image_depth        AS INTEGER     /* 08-AUG-2011 jcc: added */
    FIELD image_tag          AS CHARACTER  
    FIELD image_obj          AS INTEGER    
    FIELD image_len          AS INTEGER    
    FIELD image_h            AS INTEGER    
    FIELD image_w            AS INTEGER    
    FIELD image_bpc          AS INTEGER    
    FIELD image_cs           AS CHARACTER  
    FIELD image_alt_cs       AS CHARACTER  /* 14-NOV-2014 jcc */
    FIELD image_devn_comp    AS CHARACTER  /* 14-NOV-2014 jcc */
    FIELD image_func_params  AS CHARACTER  /* 14-NOV-2014 jcc */
    FIELD image_func_ps      AS CHARACTER  /* 14-NOV-2014 jcc */
    FIELD image_filter       AS CHARACTER  
    FIELD image_params       AS CHARACTER  
    FIELD image_data         AS CHARACTER  
    FIELD image_palette      AS CHARACTER  
    FIELD image_pal_length   AS INTEGER    
    FIELD image_transparency AS CHARACTER  
    FIELD image_has_smask    AS LOGICAL    
    FIELD used_flag          AS LOGICAL     /* 04-MAY-2014 jcc: used by the main stream? */
    FIELD used_by_xobject    AS CHARACTER   /* 04-MAY-2014 jcc: list of xobjects id */
INDEX obj_image AS PRIMARY
      obj_stream
      image_name
      image_file.

/* external pdf files */
DEFINE TEMP-TABLE TT_pdf_ext NO-UNDO
  FIELD obj_stream      AS CHARACTER
  FIELD pdf_id          AS CHARACTER
  FIELD pdf_id_orig     AS CHARACTER /* 23-SEP-2015 jcc: added, used when the pdf file has already been opened for another stream */
  FIELD pdf_name        AS CHARACTER
  /* 04-OCT-2013 jcc: remember for each external file the array & dict sequences used */
  FIELD array_seq_start AS INTEGER
  FIELD array_seq_end   AS INTEGER
  FIELD dict_seq_start  AS INTEGER
  FIELD dict_seq_end    AS INTEGER
  FIELD cache_dir       AS CHARACTER /* 19-MAY-2014 jcc: where to store temp files */
INDEX ix obj_stream pdf_id.

/* external pdf files' used pages */
DEFINE TEMP-TABLE TT_pdf_external NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD pdf_id        AS CHARACTER
    FIELD ext_tag       AS CHARACTER
    FIELD ext_obj       AS INTEGER
    FIELD ext_file      AS CHARACTER
    FIELD ext_len       AS INTEGER
    FIELD ext_Media1    AS DECIMAL DECIMALS 5
    FIELD ext_Media2    AS DECIMAL DECIMALS 5
    FIELD ext_Media3    AS DECIMAL DECIMALS 5
    FIELD ext_Media4    AS DECIMAL DECIMALS 5
    FIELD ext_Crop1     AS DECIMAL DECIMALS 5
    FIELD ext_Crop2     AS DECIMAL DECIMALS 5
    FIELD ext_Crop3     AS DECIMAL DECIMALS 5
    FIELD ext_Crop4     AS DECIMAL DECIMALS 5
    FIELD ext_rotate    AS INTEGER
    /* FIELD page_id       AS INTEGER    /* This is the new PDF page # */ */ /* 12-DEC-2014 jcc: replaced by tt_pdf_page_use_ext */
    FIELD ext_page      AS INTEGER    /* This is the external PDF page # */
INDEX obj_image AS PRIMARY
      obj_stream
      ext_tag.

/* 16-OCT-2014 jcc: fonts related temp-tables  */
{{&PDFDIR}inc/pdf_tt_fonts.i "NEW SHARED"}

DEFINE TEMP-TABLE TT_pdf_diff NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD font_name   AS CHARACTER
  FIELD char_num    AS INTEGER
  FIELD PS_name     AS CHARACTER
INDEX obj_stream  AS PRIMARY
      obj_stream
      char_num /* 23-FEB-2010 jcc: added */
INDEX ix obj_stream font_name.

DEFINE TEMP-TABLE TT_pdf_ReplaceTxt /* peki */ NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD mergenr     AS INTEGER
  FIELD txt_from    AS CHARACTER
  FIELD txt_to      AS CHARACTER
INDEX obj_stream    AS PRIMARY
      obj_stream
      mergenr.

DEFINE TEMP-TABLE TT_pdf_FillTxt NO-UNDO
  FIELD obj_stream     AS CHARACTER
  FIELD page_nbr       AS INTEGER
  FIELD fill_from      AS CHARACTER
  FIELD fill_to        AS CHARACTER
  FIELD fill_options   AS CHARACTER
  FIELD in_transaction AS LOGICAL
INDEX obj_stream  AS PRIMARY
      obj_stream
      page_nbr
      fill_from
INDEX ix obj_stream page_nbr in_transaction.

DEFINE TEMP-TABLE TT-Merge-Pages NO-UNDO
  FIELD PageFrom    AS INTEGER
  FIELD PageTo      AS INTEGER
  FIELD MergeNr     AS INTEGER
INDEX MergeNr PageFrom PageTo.

DEFINE TEMP-TABLE hexarray NO-UNDO
   FIELD hex-val   AS CHARACTER
   FIELD chr-val   AS INTEGER
INDEX hex-idx AS PRIMARY
      hex-val
INDEX chr-idx 
      chr-val.
DEFINE NEW SHARED VARIABLE int2hexArray AS CHARACTER   EXTENT 256 NO-UNDO. /* shared with pdfencrypt.p */

/* 24-FEB-2010 jcc */
DEFINE TEMP-TABLE TT_Font_Widths NO-UNDO LABEL "Widths of the fonts"
  FIELD obj_stream  AS CHARACTER
  FIELD font-name   AS CHARACTER  
  FIELD font-widths AS INTEGER     EXTENT 255
INDEX font-name IS PRIMARY UNIQUE
      obj_stream
      font-name.

/* 25-JUL-2012 jcc: buffer underlines & strikes */
DEFINE TEMP-TABLE tt_line NO-UNDO LABEL "Underscore/strike buffer"
    FIELD obj_stream AS CHARACTER
    FIELD startx     AS DECIMAL
    FIELD starty     AS DECIMAL
    FIELD endx       AS DECIMAL
    FIELD endy       AS DECIMAL
    FIELD weight     AS DECIMAL
    FIELD red        AS DECIMAL
    FIELD green      AS DECIMAL
    FIELD blue       AS DECIMAL
    INDEX ix obj_stream red green blue.

/* 10-AUG-2012 jcc: buffer text and graphic state operators */
DEFINE TEMP-TABLE tt_state_op NO-UNDO LABEL "Text & graphic state operators buffer"
    FIELD obj_stream AS CHARACTER
    FIELD type       AS CHARACTER INITIAL "T" /* T or G */
    FIELD operator   AS CHARACTER CASE-SENSITIVE /* because RG and rg is not the same... */
    FIELD opvalue    AS CHARACTER
    FIELD is_dirty   AS LOGICAL INITIAL TRUE /* FALSE when has been written to the stream */
INDEX ix AS PRIMARY
    obj_stream type operator
INDEX ix2 obj_stream type operator is_dirty
INDEX ix3 obj_stream type is_dirty.

/* 24-APR-2014 jcc: XObjects */
DEFINE TEMP-TABLE TT_pdf_xobject NO-UNDO LABEL "Xobjects"
    FIELD obj_stream      AS CHARACTER
    FIELD xobject_obj     AS INTEGER
    FIELD xobject_id      AS INTEGER
    FIELD xobject_tag     AS CHARACTER
    FIELD content_file    AS CHARACTER
    FIELD xobject_height  AS DECIMAL
    FIELD xobject_width   AS DECIMAL
    FIELD used_flag       AS LOGICAL /* used by the main stream? */
    FIELD used_by_xobject AS CHARACTER /* list of xobjects id */
    FIELD UsePageNo       AS LOGICAL
    FIELD UseTotalPages   AS LOGICAL
    FIELD xobject_key     AS CHARACTER /* 02-MAR-2015 jcc: new - allow to search an xobject based on any string stored here */
    INDEX ix AS PRIMARY
        obj_stream xobject_id DESCENDING
    INDEX ix2 obj_stream xobject_key.

/* ---------------------- Define LOCAL VARIABLES -------------------------- */

/* DEFINE VARIABLE pdf_inc_ContentSequence AS INTEGER NO-UNDO. */
DEFINE VARIABLE pdf_inc_ObjectSequence AS INTEGER   NO-UNDO.
/* DEFINE VARIABLE pdf_OutlinesDict       AS INTEGER   NO-UNDO. */ /* 14-MAY-2014 jcc: remove global var */
/* DEFINE VARIABLE pdf_OutlinesLast       AS INTEGER   NO-UNDO. */ /* 14-MAY-2014 jcc: remove global var */

/* The following variables are used to store the Image Height Width */
/* DEFINE VARIABLE pdf_width     AS INTEGER NO-UNDO. */
/* DEFINE VARIABLE pdf_height    AS INTEGER NO-UNDO. */

/* Variables used in conjunction with Header/Footer requirements */
DEFINE VARIABLE pdf_ForFooter       AS LOGICAL   NO-UNDO.

/* Variables used in conjunction with Wrap Text requirements */
DEFINE VARIABLE pdf_WrapText        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE pdf_WrapFont        AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdf_WrapSize        AS DECIMAL   NO-UNDO.
/* DEFINE VARIABLE pdf_WrapFrom        AS INTEGER   NO-UNDO. /* 25-JUL-2012 jcc: added */ */ /* 05-DEC-2016 jcc: removed */

/* Miscellaneous Variables */
DEFINE VARIABLE pdf_CurrentStream     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xml-seq               AS INTEGER NO-UNDO.
/* DEFINE VARIABLE pdf-Res-Object        AS INTEGER NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */
/* DEFINE VARIABLE pdf-Stream-Start      AS INTEGER NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */
/* DEFINE VARIABLE pdf-Stream-End        AS INTEGER NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */
/* DEFINE VARIABLE pdf-EncryptKeyMemPtr  AS MEMPTR  NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars. This one has been moved to pdfencrypt.p */

DEFINE VARIABLE h_PDF-Tool            AS HANDLE NO-UNDO.
DEFINE VARIABLE h_PDF-Template        AS HANDLE NO-UNDO.
DEFINE VARIABLE h_PDF-Encrypt         AS HANDLE NO-UNDO.
DEFINE VARIABLE h_PDF-ParseFont       AS HANDLE NO-UNDO.

/* Used to read the JPEG image file and determine width/height */
/* DEFINE VARIABLE mImage                AS MEMPTR  NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */
/* DEFINE VARIABLE iImageByte            AS INTEGER NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */

/* DEFINE VARIABLE mContent              AS MEMPTR    NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */
/* DEFINE VARIABLE mHolder               AS MEMPTR    NO-UNDO. */ /* 14-MAY-2014 jcc: remove global vars */

/* 25-FEB-2010 jcc: holds the iso8859-1 characters starting at 32 */
/* This is used when outputting the differences (/Diff) between iso8859-1 and another charset */
DEFINE VARIABLE Latin1Chars AS CHARACTER EXTENT 224 /*256-32*/ NO-UNDO INITIAL [
    'space','exclam','quotedbl','numbersign','dollar','percent','ampersand',
    'quotesingle','parenleft','parenright','asterisk','plus','comma','hyphen',
    'period','slash','zero','one','two','three','four','five','six','seven',
    'eight','nine','colon','semicolon','less','equal','greater','question','at',
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S',
    'T','U','V','W','X','Y','Z','bracketleft','backslash','bracketright',
    'asciicircum','underscore','grave','a','b','c','d','e','f','g','h','i','j',
    'k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','braceleft',
    'bar','braceright','asciitilde','','','','','','','','','','','','','','',
    '','','','','','','','','','','','','','','','','','','',
    'space','exclamdown','cent','sterling',
    'currency','yen','brokenbar','section','dieresis','copyright','ordfeminine',
    'guillemotleft','logicalnot','hyphen','registered','macron','degree',
    'plusminus','twosuperior','threesuperior','acute','omicron','paragraph',
    'periodcentered','cedilla','onesuperior','ordmasculine','guillemotright',
    'onequarter','onehalf','threequarters','questiondown','Agrave','Aacute',
    'Acircumflex','Atilde','Adieresis','Aring','AE','Ccedilla','Egrave','Eacute',
    'Ecircumflex','Edieresis','Igrave','Iacute','Icircumflex','Idieresis','Eth',
    'Ntilde','Ograve','Oacute','Ocircumflex','Otilde','Odieresis','multiply',
    'Oslash','Ugrave','Uacute','Ucircumflex','Udieresis','Yacute','Thorn',
    'germandbls','agrave','aacute','acircumflex','atilde','adieresis','aring',
    'ae','ccedilla','egrave','eacute','ecircumflex','edieresis','igrave',
    'iacute','icircumflex','idieresis','eth','ntilde','ograve','oacute',
    'ocircumflex','otilde','odieresis','divide','oslash','ugrave','uacute',
    'ucircumflex','udieresis','yacute','thorn','ydieresis' ].

/* 26-SEP-2014 jcc: default code page */
DEFINE VARIABLE gcDefaultCodePage AS CHARACTER   NO-UNDO.

DEFINE STREAM S_pdf_inc.
DEFINE STREAM S_pdf_inp.
DEFINE STREAM S_pdf_out.

/***************************** Included functions & procedures ***********************************/

{{&PDFDIR}inc/pdf_func_error.i "NEW SHARED"}
{{&PDFDIR}inc/pdf_func_bin.i "NEW SHARED"}
{{&PDFDIR}inc/pdf_func_parseText.i}
{{&PDFDIR}inc/pdf_func_compress.i}
{{&PDFDIR}inc/pdf_func_strconv.i}

/***************************************** Main block ********************************************/

RUN LoadHexArray. /* preload TT HexArray & int2hexArray[] */

/* 26-SEP-2014 jcc: define a default code page - used for double byte code page, e.g. shift-jis or big-5 */
DEFINE VARIABLE iTmp AS INTEGER     NO-UNDO.
iTmp = ASC("A", "iso8859-1") NO-ERROR.
IF iTmp = -1 THEN
    gcDefaultCodePage = SESSION:CPINTERNAL.
ELSE
    gcDefaultCodePage = "iso8859-1".


/*************************************************************************************************/

/* ------------------------------ Math functions --------------------------- */

PROCEDURE sin EXTERNAL '{&mathLib}' CDECL:
    DEFINE INPUT  PARAMETER dblRadian AS DOUBLE   NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE   NO-UNDO.
END PROCEDURE.

PROCEDURE cos EXTERNAL '{&mathLib}' CDECL:
    DEFINE INPUT  PARAMETER dblRadian AS DOUBLE   NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE   NO-UNDO.
END PROCEDURE.

PROCEDURE tan EXTERNAL '{&mathLib}' CDECL:
    DEFINE INPUT  PARAMETER dblRadian AS DOUBLE   NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE   NO-UNDO.
END PROCEDURE.

PROCEDURE atan EXTERNAL '{&mathLib}' CDECL:
    DEFINE INPUT  PARAMETER dblRadian AS DOUBLE   NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE   NO-UNDO.
END PROCEDURE.

PROCEDURE _logb EXTERNAL '{&mathLib}' CDECL:
    DEFINE INPUT  PARAMETER dbl       AS DOUBLE   NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE   NO-UNDO.
END PROCEDURE.

&GLOBAL-DEFINE xdPi 3.14159265358979323846

FUNCTION math_sin RETURNS DECIMAL (dDegree AS DECIMAL):
    DEFINE VARIABLE dRetVal AS DECIMAL   NO-UNDO.
    RUN sin(dDegree * {&XdPi} / 180, OUTPUT dRetVal) NO-ERROR.
    RETURN dRetVal.
END FUNCTION.

FUNCTION math_cos RETURNS DECIMAL (dDegree AS DECIMAL):
    DEFINE VARIABLE dRetVal AS DECIMAL   NO-UNDO.
    RUN cos(dDegree * {&XdPi} / 180, OUTPUT dRetVal) NO-ERROR.
    RETURN dRetVal.
END FUNCTION.

FUNCTION math_tan RETURNS DECIMAL (dDegree AS DECIMAL):
    DEFINE VARIABLE dRetVal AS DECIMAL   NO-UNDO.
    RUN tan(dDegree * {&XdPi} / 180, OUTPUT dRetVal) NO-ERROR.
    RETURN dRetVal.
END FUNCTION.

FUNCTION math_atan RETURNS DECIMAL (dValue AS DECIMAL):
    DEFINE VARIABLE dRetVal AS DECIMAL   NO-UNDO.
    RUN atan(dValue, OUTPUT dRetVal) NO-ERROR.
    RETURN dRetVal / {&XdPi} * 180.
END FUNCTION.

FUNCTION math_log2 RETURNS DECIMAL ( dValue AS DECIMAL ):
    DEFINE VARIABLE dRetVal AS DECIMAL   NO-UNDO.
    RUN _logb(dValue, OUTPUT dRetVal) NO-ERROR.
    RETURN dRetVal.
END FUNCTION.

&UNDEFINE xdPi

/* -------------------------- End of Math functions ------------------------- */

FUNCTION multiplyTransformationMatrices RETURNS CHARACTER /* PRIVATE */
    (M1 AS CHARACTER, M2 AS CHARACTER):
    /* Used to calculate the matrix resulting of a series of transformations */
    DEFINE VARIABLE a AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE b AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE c AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE d AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE e AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE f AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE x AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE y AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE z AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE t AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE u AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE v AS DECIMAL     NO-UNDO.

    ASSIGN
        a = string2dec(ENTRY(1, M1, " "))
        b = string2dec(ENTRY(2, M1, " "))
        c = string2dec(ENTRY(3, M1, " "))
        d = string2dec(ENTRY(4, M1, " "))
        e = string2dec(ENTRY(5, M1, " "))
        f = string2dec(ENTRY(6, M1, " "))
        x = string2dec(ENTRY(1, M2, " "))
        y = string2dec(ENTRY(2, M2, " "))
        z = string2dec(ENTRY(3, M2, " "))
        t = string2dec(ENTRY(4, M2, " "))
        /* u = string2dec(ENTRY(5, M2, " ")) */
        /* v = string2dec(ENTRY(6, M2, " ")) */
        .

    RETURN
        dec2string(a * x + b * z) + " " +
        dec2string(a * y + b * t) + " " +
        dec2string(c * x + d * z) + " " +
        dec2string(c * y + d * t) + " " +
        ENTRY(5, M2, " ") + " " + ENTRY(6, M2, " ").
        /* dec2string(e * x + f * z + u) + " " + */
        /* dec2string(e * y + f * t + v). */
END FUNCTION.

/* ---------------------------- Define FUNCTIONS -------------------------- */

/* 12-JUL-2011 jcc: rewrite to be much more versatile */
FUNCTION GetWidgetOption RETURNS CHARACTER   /* PRIVATE */
    (INPUT pOption  AS CHARACTER,
     INPUT pOptions AS CHARACTER):

    DEFINE VARIABLE cOpt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i    AS INTEGER     NO-UNDO.

    DO i = NUM-ENTRIES(pOptions) TO 1 BY -1:
        cOpt = ENTRY(i, pOptions).
        IF ENTRY(1, cOpt, "=") = pOption THEN
            IF NUM-ENTRIES(cOpt, "=") = 1 THEN
                RETURN "YES".
            ELSE
                RETURN ENTRY(2, cOpt, "=").
    END.
    /* will return ? if the option is not found in the options string */
END FUNCTION. /* GetWidgetOption */

/* 11-AUG-2012 jcc: the same with a default value */
FUNCTION GetWidgetOption2 RETURNS CHARACTER   /* PRIVATE */
    (INPUT pOption  AS CHARACTER,
     INPUT pOptions AS CHARACTER,
     INPUT pDefault AS CHARACTER):

    DEFINE VARIABLE cOpt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i    AS INTEGER     NO-UNDO.

    DO i = NUM-ENTRIES(pOptions) TO 1 BY -1:
        cOpt = ENTRY(i, pOptions).
        IF ENTRY(1, cOpt, "=") = pOption THEN
            IF NUM-ENTRIES(cOpt, "=") = 1 THEN
                RETURN "YES".
            ELSE
                RETURN ENTRY(2, cOpt, "=").
    END.
    RETURN pDefault.
END FUNCTION. /* GetWidgetOption2 */

/* 09-AUG-2011 jcc: created */
FUNCTION basename RETURNS CHARACTER (ipcFile AS CHARACTER):
    RETURN ENTRY(NUM-ENTRIES(REPLACE(ipcFile, "~\", "~/"), "~/"), REPLACE(ipcFile, "~\", "~/"), "~/").
END FUNCTION.

/* 10-OCT-2013 jcc: Hexa conversion functions */

FUNCTION hex RETURNS CHARACTER (INPUT asc-value AS INTEGER).
  DEFINE VARIABLE hexBit AS CHARACTER FORMAT "x(1)" EXTENT 16 INIT ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'] NO-UNDO.
  DEFINE VARIABLE j      AS INTEGER  NO-UNDO.
  DEFINE VARIABLE h      AS CHARACTER NO-UNDO.

  DO WHILE TRUE:
    j = asc-value MODULO 16.
    h = hexbit[j + 1] + h.
    IF asc-value < 16 THEN LEAVE.
    asc-value = (asc-value - j) / 16.
  END.
  IF LENGTH(h, "character":u) = 1 THEN
    h = "0" + h.
  RETURN h.
END FUNCTION.

FUNCTION int2hexchar RETURNS CHARACTER ( vi AS INTEGER ):
  DEFINE VARIABLE chex AS CHARACTER NO-UNDO.
  chex = hex( vi ).
  RETURN '0x' + FILL( '0', 8 - LENGTH( chex ) ) + chex. 
END FUNCTION. 

PROCEDURE LoadHexArray: /* PRIVATE */
  DEFINE VARIABLE vHexLoop AS INTEGER NO-UNDO.

  DO vHexLoop = 0 TO 255:
    CREATE HexArray.
    ASSIGN HexArray.hex-val = hex(vHexLoop)
           HexArray.chr-val = vHexLoop.
    int2hexArray[vHexLoop + 1] = HexArray.hex-val.
  END.
END. /* LoadHexArray */

/* 15-OCT-2014 jcc: new */
FUNCTION innerTrim RETURNS CHARACTER ( pcString AS CHARACTER ):
    DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    cString = REPLACE(pcString, "  ", " ").
    DO WHILE cString <> pcString:
        pcString = cString.
        cString = REPLACE(pcString, "  ", " ").
    END.
    RETURN cString.
END FUNCTION.
/* 15-OCT-2014 jcc: end */

FUNCTION pdf_get_parameter RETURNS CHARACTER
         (INPUT pdfStream     AS CHARACTER,
          INPUT pdfParameter  AS CHARACTER):

  DEFINE BUFFER TT_pdf_param FOR TT_pdf_param.

  FIND TT_pdf_param WHERE TT_pdf_param.obj_stream = pdfStream
                      AND TT_pdf_param.obj_parameter = pdfParameter NO-ERROR.
  IF AVAILABLE TT_pdf_param THEN
    RETURN TT_pdf_param.obj_value.
  ELSE
    RETURN "".

END FUNCTION. /* pdf_get_parameter */

/* 1-AUG-2012 jcc: same as pdf_get_parameter but with a defaut value */
FUNCTION pdf_get_parameter2 RETURNS CHARACTER
         (INPUT pdfStream     AS CHARACTER,
          INPUT pdfParameter  AS CHARACTER,
          INPUT pdfDefault    AS CHARACTER):

  DEFINE BUFFER TT_pdf_param FOR TT_pdf_param.

  FIND TT_pdf_param WHERE TT_pdf_param.obj_stream = pdfStream
                      AND TT_pdf_param.obj_parameter = pdfParameter NO-ERROR.
  IF AVAILABLE TT_pdf_param THEN
    RETURN TT_pdf_param.obj_value.
  ELSE
    RETURN pdfDefault.

END FUNCTION. /* pdf_get_parameter2 */

FUNCTION pdf_Page RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN INTEGER(pdf_get_parameter2(pdfStream, "Page", "0")).

END FUNCTION. /* pdf_Page */


FUNCTION pdf_LeftMargin RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "LeftMargin", "10")).

END FUNCTION. /* pdf_LeftMargin */

/* 23-MAR-2015 jcc: new */
FUNCTION pdf_RightMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN INTEGER(pdf_get_parameter2(pdfStream, "RightMargin", "10")).

END FUNCTION. /* pdf_RightMargin */

FUNCTION pdf_TopMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN INTEGER(pdf_get_parameter2(pdfStream, "TopMargin", "50")).

END FUNCTION. /* pdf_TopMargin */

FUNCTION pdf_BottomMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN INTEGER(pdf_get_parameter2(pdfStream, "BottomMargin", "50")).

END FUNCTION. /* pdf_BottomMargin */

FUNCTION pdf_Font RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN pdf_get_parameter2(pdfStream, "Font", "Courier").

END FUNCTION. /* pdf_Font */

FUNCTION pdf_Font_Loaded RETURN LOGICAL
        ( INPUT pdfStream AS CHARACTER,
          INPUT pdfFont   AS CHARACTER):
  DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_Font WHERE TT_pdf_Font.obj_stream = pdfStream
                           AND TT_pdf_Font.font_name  = pdfFont NO-ERROR.
  RETURN AVAILABLE TT_pdf_font.

END FUNCTION. /* pdf_Font_Loaded */

FUNCTION pdf_FontType RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

  DEFINE VARIABLE L_Font    AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  L_Font = pdf_Font(pdfStream).
  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = L_Font NO-ERROR.
  IF AVAILABLE TT_pdf_font THEN
    RETURN TT_pdf_font.font_pitch.
  ELSE
    RETURN "FIXED".

END FUNCTION. /* pdf_FontType */

FUNCTION pdf_ImageDim RETURN INTEGER ( INPUT pdfStream AS CHARACTER,
                                       INPUT pdfImage  AS CHARACTER,
                                       INPUT pdfDim    AS CHARACTER):

  DEFINE BUFFER TT_pdf_image FOR TT_pdf_image.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream
                            AND TT_pdf_image.image_name = pdfImage NO-ERROR.
  IF AVAILABLE TT_pdf_image THEN DO:
    IF pdfDim = "HEIGHT" THEN
      RETURN TT_pdf_image.image_h.
    ELSE IF pdfDim = "WIDTH" THEN
      RETURN TT_pdf_image.image_w.
    ELSE
      RETURN 0.
  END.
  ELSE
    RETURN 0.

END FUNCTION. /* pdf_ImageDim */

FUNCTION pdf_TextX RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "TextX", "0")).

END FUNCTION. /* pdf_TextX*/

FUNCTION pdf_TextY RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "TextY", "0")).

END FUNCTION. /* pdf_TextY */

FUNCTION pdf_in_transaction RETURNS LOGICAL (pdfStream AS CHARACTER):
    RETURN pdf_get_parameter2(pdfStream, "___inTransaction", "0") <> "0".
END FUNCTION.

/* PRIVATE */
/* 14-OCT-2014 jcc: /!\ must always be called just before outputting the object, else
                    the object offset might be wrong (or use setObjectOffset() to fix it) */
FUNCTION ObjectSequence RETURNS INTEGER ( INPUT pdfStream     AS CHARACTER,
                                          INPUT pdfSequence   AS INTEGER,
                                          INPUT pdfObjectDesc AS CHARACTER,
                                          INPUT pdfOffset     AS INTEGER,
                                          INPUT pdfPage       AS INTEGER,
                                          INPUT pdfExtra      AS CHARACTER ):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  CREATE TT_pdf_object.
  ASSIGN TT_pdf_object.obj_stream = pdfStream
         TT_pdf_object.obj_nbr    = pdfSequence
         TT_pdf_object.obj_desc   = pdfObjectDesc
         TT_pdf_object.obj_offset = IF pdfSequence <> 0 THEN
                                      IF pdfOffset = ? THEN SEEK(S_pdf_inc) + 1 ELSE pdfOffset
                                    ELSE 0
         TT_pdf_object.gen_nbr    = IF pdfSequence <> 0 THEN 0
                                    ELSE 65535
         TT_pdf_object.obj_type   = IF pdfSequence = 0 THEN "f"
                                    ELSE "n"
         TT_pdf_object.obj_page   = pdfPage
         TT_pdf_object.obj_extra  = pdfExtra.
     TT_pdf_object.in_transaction = pdf_in_transaction(pdfStream).

  pdf_inc_ObjectSequence = pdfSequence.

  RETURN pdf_inc_ObjectSequence.

END FUNCTION. /* ObjectSequence */

FUNCTION setObjectOffset RETURNS LOGICAL (INPUT pdfStream   AS CHARACTER,
                                          INPUT pdfSequence AS INTEGER,
                                          INPUT pdfOffset   AS INTEGER):
    DEFINE BUFFER TT_pdf_object FOR TT_pdf_object.
    FIND TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream
        AND TT_pdf_object.obj_nbr = pdfSequence.
    TT_pdf_object.obj_offset = pdfOffset.

END FUNCTION.

PROCEDURE pdf_new :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFileName AS CHARACTER NO-UNDO.

  DEFINE BUFFER TT_pdf_Stream FOR TT_pdf_stream.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFileName)). &ENDIF
  
  IF INDEX(pdfStream, " ") > 0 THEN {pdferror.i &msg="'Cannot have a space in the Stream Name!'" &return=YES}.

  IF CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Stream ' + QUOTER(pdfStream) + ' has already been defined!'" &return=YES &error=YES &returnmsg=NO}.

  CREATE TT_pdf_stream.
  ASSIGN TT_pdf_stream.obj_stream = pdfStream
         TT_pdf_stream.obj_file   = pdfFileName.

  /* Determine Unique Stream ID -- this string is used to output temporary files
     that are used for building content or generating Encryption keys etc */
  TT_pdf_stream.obj_UniqueID = ENCODE(STRING(TODAY) + STRING(ETIME) + TT_pdf_stream.obj_file). /* 3-AUG-2012 jcc: replace TIME by ETIME */

  /* Delete any pre-existing temp files for stream */
  /* 23-JUN-2009 jcc: make use of OS-DELETE instead because 
                      1) this is the good way to do it
                      2) OS-COMMAND outputs useless messages (e.g. in the appserver broker log when there is nothing to delete) */
  /*IF OPSYS = "UNIX" THEN
    OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + pdfStream + "*.txt").
  ELSE
    OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + pdfStream + "*.txt").*/
  DEFINE VARIABLE cFile AS CHARACTER   NO-UNDO.
  INPUT FROM OS-DIR(SESSION:TEMP-DIRECTORY) NO-ATTR-LIST.
  REPEAT:
      IMPORT cFile.
      IF cFile MATCHES pdfStream + "*.txt" THEN
          OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + cFile).
  END.
  INPUT CLOSE.
  /* 23-JUN-2009 jcc: end */

  RUN pdf_LoadBase14 (pdfStream).
  RUN pdf_init_param (pdfStream).

  RUN pdf_set_info(pdfStream,"OutputTo",pdfFileName).

  ASSIGN TT_pdf_stream.obj_DoingText    = FALSE
         TT_pdf_stream.obj_DoingGraphic = FALSE.

  pdf_CurrentStream = pdfStream.

END. /* pdf_new */

FUNCTION pdf_VerticalSpace RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "VerticalSpace", "0")).

END FUNCTION. /* pdf_VerticalSpace */

FUNCTION pdf_PointSize RETURN DECIMAL ( INPUT pdfStream AS CHARACTER ):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "PointSize", "10")).

END FUNCTION. /* pdf_PointSize */

/* 26-JUL-2012 jcc: new */
PROCEDURE pdf_strip_tags: /* PRIVATE */
    DEFINE INPUT-OUTPUT  PARAMETER pcString AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE iTagIndex AS INTEGER     NO-UNDO.

    /* remove simple tags */
    pcString = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
                pcString, "<i>",  ""),
                          "</i>", ""),
                          "<b>",  ""),
                          "</b>", ""),
                          "</color>", ""),
                          "<s>",  ""),
                          "</s>", ""),
                          "<u>",  ""),
                          "</u>", ""),
                          "</url>", ""),
                          "</font>", "").
    /* remove complex tags: <url=*> & <color=*> */
    iTagIndex = INDEX(pcString, "<color").
    DO WHILE iTagIndex > 0 AND INDEX(pcString, ">", iTagIndex) > iTagIndex:
        SUBSTRING(pcString, iTagIndex, INDEX(pcString, ">", iTagIndex) - iTagIndex + 1) = "".
        iTagIndex = INDEX(pcString, "<color").
    END.
    IF iTagIndex > 0 THEN
        pcString = SUBSTRING(pcString, 1, iTagIndex - 1).
    iTagIndex = INDEX(pcString, "<url").
    DO WHILE iTagIndex > 0 AND INDEX(pcString, ">", iTagIndex) > iTagIndex:
        SUBSTRING(pcString, iTagIndex, INDEX(pcString, ">", iTagIndex) - iTagIndex + 1) = "".
        iTagIndex = INDEX(pcString, "<url").
    END.
    IF iTagIndex > 0 THEN
        pcString = SUBSTRING(pcString, 1, iTagIndex - 1).
    iTagIndex = INDEX(pcString, "<font").
    DO WHILE iTagIndex > 0 AND INDEX(pcString, ">", iTagIndex) > iTagIndex:
        SUBSTRING(pcString, iTagIndex, INDEX(pcString, ">", iTagIndex) - iTagIndex + 1) = "".
        iTagIndex = INDEX(pcString, "<font").
    END.
    IF iTagIndex > 0 THEN
        pcString = SUBSTRING(pcString, 1, iTagIndex - 1).
END.
/* jcc end */

/* DEFINE TEMP-TABLE ttWidthCache NO-UNDO LABEL "_pdf_text_width cache temp-table" */
 /* FIELD pdfIx       AS CHARACTER */
 /* FIELD pdfText     AS CHARACTER */
 /* FIELD pdfFont     AS CHARACTER */
 /* FIELD pdfFontSize AS DECIMAL */
 /* FIELD pdfWidth    AS DECIMAL */
 /* INDEX ix pdfIx. */

/* 13-FEB-2014 jcc: the code was duplicated 4 times: factorized here */
FUNCTION _pdf_text_width RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                           INPUT pdfText     AS CHARACTER,
                                           INPUT pdfFont     AS CHARACTER,
                                           INPUT pdfFontTag  AS CHARACTER,
                                           INPUT pdfFontSize AS DECIMAL):
  DEFINE BUFFER TT_pdf_font           FOR TT_pdf_font.
  DEFINE BUFFER TT_pdf_font_character FOR TT_pdf_font_character.

  DEFINE VARIABLE L_font  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_Loop  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_size  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_tot   AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE L_width AS DECIMAL     DECIMALS 5 NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  IF pdfFont > "" THEN
      FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                               AND TT_pdf_font.font_name  = pdfFont
                               NO-LOCK NO-ERROR.
  ELSE IF pdfFontTag > "" THEN
      FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                               AND TT_pdf_font.font_tag   = pdfFontTag
                               NO-LOCK NO-ERROR.
  ELSE
      FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                               AND TT_pdf_font.font_name  = pdf_Font(pdfStream)
                               NO-LOCK NO-ERROR.

  IF NOT AVAILABLE TT_pdf_font THEN RETURN 0.0.


  IF pdfFontSize = ? OR pdfFontSize = 0 THEN
      pdfFontSize = pdf_PointSize(pdfStream).

  /* 04-APR-2015 jcc: cache */
  /* pdfFont = TT_pdf_font.font_name. */
  /* FOR EACH ttWidthCache WHERE ttWidthCache.pdfIx = SUBSTRING(pdfText, 1, 180): */
      /* IF ttWidthCache.pdfText = pdfText THEN LEAVE. */
  /* END. */
  /* IF AVAILABLE ttWidthCache THEN RETURN ttWidthCache.pdfWidth. */

  /* 24-JUL-2012 jcc: strip tags if applicable */
  IF INDEX(pdfText, "<") > 0 AND pdf_get_parameter(pdfStream,"UseTags") = "TRUE" THEN
      RUN pdf_strip_tags(INPUT-OUTPUT pdfText).

  IF TT_pdf_font.is_unicode THEN DO: /* 13-FEB-2014 jcc: width for unicode */
      DEFINE VARIABLE mText AS MEMPTR      NO-UNDO.
      RUN utf8_to_utf16be (pdfStream, pdfText, "raw", ?, OUTPUT mText).
      L_size = GET-SIZE(mText).
      DO L_Loop = 1 TO L_size - 1 BY 2:
          FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
              AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
              AND TT_pdf_font_character.char_id = GET-UNSIGNED-SHORT(mText, L_Loop) NO-ERROR.
          IF AVAILABLE TT_pdf_font_character THEN DO:
              L_tot = L_tot + string2dec(TT_pdf_font_character.char_width).
              /* 24-FEB-2014 jcc: remember used characters when we will be subsetting the font */
              /* 03-JUL-2014 jcc: not using pdf_subset_add_string for performance reason */
              IF TT_pdf_font.font_subset THEN
                  TT_pdf_font_character.used = YES.
          END.
      END.
      SET-SIZE(mText) = 0.
      L_width = L_tot / 1000 * pdfFontSize NO-ERROR.
  END.

  ELSE IF TT_pdf_font.font_pitch = "FIXED" THEN
      L_width = LENGTH(pdfText, "character":u) * INTEGER(TT_pdf_font.font_width) / 1000 * pdfFontSize.

  ELSE DO:
    /* 24-SEP-2014 jcc: must use the codepage defined for the stream instead of hardcoded iso8859-1 */
    DEFINE VARIABLE cCodePage AS CHARACTER   NO-UNDO.
    cCodePage = pdf_get_parameter2(pdfStream, "CodePage", gcDefaultCodePage).
    DO L_loop = 1 TO LENGTH(pdfText, "character":u):
      /* 16-SEP-2013 jcc: make it work in utf-8 sessions */
      /* L_tot = L_tot + INT(ENTRY(INT(ASC(SUBSTR(pdfText,L_Loop,1, "character":u))) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR. */
      L_tot = L_tot + INT(ENTRY(ASC(SUBSTR(pdfText,L_Loop,1, "character":u), cCodePage) + 1, TT_pdf_Font.font_width, " ")) NO-ERROR.
      /* L_tot = L_tot + INT(ENTRY(ASC(SUBSTR(pdfText,L_Loop,1, "character":u), "iso8859-1") + 1, TT_pdf_Font.font_width, " ")) NO-ERROR. */
      /* L_tot = L_tot + INT(ENTRY(ASC(SUBSTR(pdfText,L_Loop,1, "character":u)) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR. */
    END.
    L_width = L_tot / 1000 * pdfFontSize NO-ERROR.
  END. /* Variable Width Font */

  /* CREATE ttWidthCache. */
  /* ASSIGN */
   /* ttWidthCache.pdfIx       = SUBSTRING(pdfText, 1, 180) */
   /* ttWidthCache.pdfText     = pdfText */
   /* ttWidthCache.pdfFont     = pdfFont */
   /* ttWidthCache.pdfFontSize = pdfFontSize */
   /* ttWidthCache.pdfWidth    = L_width. */

  RETURN L_width.
END FUNCTION. /* _pdf_text_width */

FUNCTION pdf_text_width RETURNS INTEGER ( INPUT pdfStream   AS CHARACTER,
                                          INPUT pdfText     AS CHARACTER):
    RETURN INTEGER(_pdf_text_width(pdfStream, pdfText, pdf_Font(pdfStream), "", pdf_PointSize(pdfStream))).
END FUNCTION. /* pdf_text_width */

FUNCTION pdf_text_widthdec RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                             INPUT pdfText     AS CHARACTER):
    RETURN _pdf_text_width(pdfStream, pdfText, pdf_Font(pdfStream), "", pdf_PointSize(pdfStream)).
END FUNCTION. /* pdf_text_widthDec */

FUNCTION pdf_text_widthdec2 RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                              INPUT pdfFontTag  AS CHARACTER, 
                                              INPUT pdfFontSize AS DECIMAL,
                                              INPUT pdfText     AS CHARACTER):
    RETURN _pdf_text_width(pdfStream, pdfText, "", pdfFontTag, pdfFontSize).
END FUNCTION. /* pdf_text_widthdec2 */

FUNCTION pdf_text_fontwidth RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                              INPUT pdfFont     AS CHARACTER,
                                              INPUT pdfText     AS CHARACTER):
    RETURN _pdf_text_width(pdfStream, pdfText, pdfFont, "", pdf_PointSize(pdfStream)).
END FUNCTION. /* pdf_text_fontwidth */

FUNCTION pdf_text_fontwidth2 RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                               INPUT pdfFont     AS CHARACTER,
                                               INPUT pdfFontSize AS DECIMAL,
                                               INPUT pdfText     AS CHARACTER):
    RETURN _pdf_text_width(pdfStream, pdfText, pdfFont, "", pdfFontSize).
END FUNCTION. /* pdf_text_fontwidth2 */

FUNCTION pdf_TextRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "TextRed", "0")).

END FUNCTION. /* pdf_TextRed */

FUNCTION pdf_TextGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "TextGreen", "0")).

END FUNCTION. /* pdf_TextGreen */

FUNCTION pdf_TextBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "TextBlue", "0")).

END FUNCTION. /* pdf_TextBlue */

PROCEDURE pdf_set_TextRed :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "TextRed", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_text_color" THEN
    RUN setTextOperator(pdfStream, "rg", dec2string(pdfValue) + " " + dec2string(pdf_TextGreen(pdfStream)) + " " + dec2string(pdf_TextBlue(pdfStream))).

END. /* pdf_set_TextRed */

PROCEDURE pdf_set_TextGreen :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "TextGreen", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_text_color" THEN
    RUN setTextOperator(pdfStream, "rg", dec2string(pdf_TextRed(pdfStream)) + " " + dec2string(pdfValue) + " " + dec2string(pdf_TextBlue(pdfStream))).

END. /* pdf_set_TextGreen */

PROCEDURE pdf_set_TextBlue :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "TextBlue", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_text_color" THEN
    RUN setTextOperator(pdfStream, "rg", dec2string(pdf_TextRed(pdfStream)) + " " + dec2string(pdf_TextGreen(pdfStream)) + " " + dec2string(pdfValue)).

END. /* pdf_set_TextBlue */

FUNCTION pdf_FillRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "FillRed", "0")).

END FUNCTION. /* pdf_FillRed */

FUNCTION pdf_FillGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "FillGreen", "0")).

END FUNCTION. /* pdf_FillGreen */

FUNCTION pdf_FillBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "FillBlue", "0")).

END FUNCTION. /* pdf_FillBlue */

PROCEDURE pdf_set_FillRed :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "FillRed", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_stroke_fill" THEN
    RUN setGfxOperator(pdfStream, "rg", dec2string(pdfValue) + " " + dec2string(pdf_FillGreen(pdfStream)) + " " + dec2string(pdf_FillBlue(pdfStream))).

END. /* pdf_set_FillRed */

PROCEDURE pdf_set_FillGreen :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "FillGreen", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_stroke_fill" THEN
    RUN setGfxOperator(pdfStream, "rg", dec2string(pdf_FillRed(pdfStream)) + " " + dec2string(pdfValue) + " " + dec2string(pdf_FillBlue(pdfStream))).

END. /* pdf_set_FillGreen */

PROCEDURE pdf_set_FillBlue :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "FillBlue", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_stroke_fill" THEN
    RUN setGfxOperator(pdfStream, "rg", dec2string(pdf_FillRed(pdfStream)) + " " + dec2string(pdf_FillGreen(pdfStream)) + " " + dec2string(pdfValue)).

END. /* pdf_set_FillBlue */

/* 14-OCT-2014 jcc: new */
FUNCTION pdf_StrokeRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "StrokeRed", "0")).

END FUNCTION. /* pdf_StrokeRed */

FUNCTION pdf_StrokeGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "StrokeGreen", "0")).

END FUNCTION. /* pdf_StrokeGreen */

FUNCTION pdf_StrokeBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN string2dec(pdf_get_parameter2(pdfStream, "StrokeBlue", "0")).

END FUNCTION. /* pdf_StrokeBlue */

PROCEDURE pdf_set_StrokeRed :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "StrokeRed", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_stroke_color" THEN
    RUN setGfxOperator(pdfStream, "RG", dec2string(pdfValue) + " " + dec2string(pdf_StrokeGreen(pdfStream)) + " " + dec2string(pdf_StrokeBlue(pdfStream))).

END. /* pdf_set_StrokeRed */

PROCEDURE pdf_set_StrokeGreen :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "StrokeGreen", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_stroke_color" THEN
    RUN setGfxOperator(pdfStream, "RG", dec2string(pdf_StrokeRed(pdfStream)) + " " + dec2string(pdfValue) + " " + dec2string(pdf_StrokeBlue(pdfStream))).

END. /* pdf_set_StrokeGreen */

PROCEDURE pdf_set_StrokeBlue :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "StrokeBlue", dec2string(pdfValue)).

  /* 07-JUN-2013 jcc: fix a regression from the 8/2/12 optimization */
  IF ENTRY(1, PROGRAM-NAME(2), " ") <> "pdf_stroke_color" THEN
    RUN setGfxOperator(pdfStream, "RG", dec2string(pdf_StrokeRed(pdfStream)) + " " + dec2string(pdf_StrokeGreen(pdfStream)) + " " + dec2string(pdfValue)).

END. /* pdf_set_StrokeBlue */
/* 14-OCT-2014 jcc: end */

FUNCTION pdf_PageRotate RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN INTEGER(pdf_get_parameter2(pdfStream, "PageRotate", "0")).

END FUNCTION. /* pdf_PageRotate */

FUNCTION pdf_PageWidth RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "PageWidth", "612")).

END FUNCTION. /* pdf_PageWidth */

FUNCTION pdf_PageHeight RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "PageHeight", "792")).

END FUNCTION. /* pdf_PageHeight */

PROCEDURE pdf_set_PageWidth :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Page Width cannot be zero!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "PageWidth", STRING(pdfValue)).

END. /* pdf_set_PageWidth */

PROCEDURE pdf_set_PageHeight :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Page Height cannot be zero!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "PageHeight", STRING(pdfValue)).

END. /* pdf_set_PageHeight */

/* 02-MAY-2014 jcc: get the page content file ; handle transactions */
FUNCTION _get_page_content_file RETURN CHARACTER (pdfStream AS CHARACTER, pcUniqueID AS CHARACTER, piPage AS INTEGER):
    RETURN SESSION:TEMP-DIR 
         + pcUniqueID  
         + "-Content-" 
         + STRING(piPage)
         + (IF pdf_get_parameter2(pdfStream, "___inTransaction", "0") = "0" THEN "" ELSE ".transac") /* 2-AUG-2012 jcc: add support for transactions */
         + ".txt".
END.

PROCEDURE pdf_set_Page :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  DEFINE BUFFER TT_pdf_stream FOR TT_pdf_stream.

  DEFINE VARIABLE cTransacPages AS CHARACTER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF
  
  FIND FIRST TT_pdf_stream
       WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAILABLE TT_pdf_Stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 28-APR-2014 jcc: can't change page while in a pattern */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") <> "0" THEN
      {pdferror.i &msg="'Cannot change page while creating a pattern. Please end it first.'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Value passed cannot be zero!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "Page", STRING(pdfValue)).

  RUN _pdf_close_output_context (pdfStream).

  OUTPUT STREAM S_pdf_out CLOSE.

  /* 6-AUG-2012 jcc: transactions: make sure the page number we change to is on the list:
     rollback & commit need to know on which pages the transaction has been active */
  cTransacPages = pdf_get_parameter2(pdfStream, "___inTransaction", "0").
  IF cTransacPages <> "0" AND LOOKUP(STRING(pdfValue), cTransacPages) = 0 THEN
      RUN _pdf_set_parameter_priv(pdfStream, "___inTransaction",
                                 cTransacPages + "," + STRING(pdfValue)).

  OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, pdfValue)) BINARY NO-MAP NO-CONVERT NO-ECHO APPEND.

  /* Note: the placement of the following commands is important due to the
      setting of the X and Y attributes.  DO NOT CHANGE unless tested
      thoroughly */
  RUN pdf_set_LeftMargin(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_RightMargin(pdfStream, pdf_RightMargin(pdfStream)).
  RUN pdf_set_TopMargin(pdfStream, pdf_TopMargin(pdfStream)).
  RUN pdf_set_BottomMargin(pdfStream, pdf_BottomMargin(pdfStream)).

  CASE pdf_PageRotate(pdfStream):
    WHEN 0 OR WHEN 180 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream) ).
    END.

    WHEN 90 OR WHEN 270 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageWidth(pdfStream) - pdf_TopMargin(pdfStream) ).
    END.

  END CASE.

  RUN pdf_set_GraphicX(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_GraphicY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

END. /* pdf_set_Page */

PROCEDURE pdf_set_PageRotate :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue <> 0 AND pdfValue <> 90 AND pdfValue <> 180 AND pdfValue <> 270 THEN {pdferror.i &msg="'Page Rotation value must be 0, 90, 180, or 270!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "PageRotate", STRING(pdfValue)).

END. /* pdf_set_PageRotate */

FUNCTION pdf_Angle RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN INTEGER(pdf_get_parameter2(pdfStream, "Angle", ?)).

END. /* pdf_Angle */

FUNCTION pdf_ScaleX RETURN DECIMAL (INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "ScaleX", ?)).

END. /* pdf_ScaleX */

FUNCTION pdf_ScaleY RETURN DECIMAL (INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN DECIMAL(pdf_get_parameter2(pdfStream, "ScaleY", ?)).

END. /* pdf_ScaleY */

FUNCTION pdf_Orientation RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  RETURN pdf_get_parameter2(pdfStream, "Orientation", "Portrait").

END. /* pdf_Orientation */

PROCEDURE pdf_set_TextX :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL   NO-UNDO.

  DEFINE BUFFER L_TT_pdf_param FOR TT_pdf_param.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* IF pdfValue < 0 THEN DO: */
    /* RUN pdf_error(pdfStream,"pdf_set_TextX","Value cannot be less than zero!"). */
    /* RETURN . */
  /* END. */

  /* RUN _pdf_set_parameter_priv (pdfStream, "TextX", STRING(pdfValue)). */
  FIND FIRST L_TT_pdf_param
       WHERE L_TT_pdf_param.obj_stream    = pdfStream
         AND L_TT_pdf_param.obj_parameter = "TextX" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL L_TT_pdf_param THEN DO:
    CREATE L_TT_pdf_param.
    ASSIGN L_TT_pdf_param.obj_stream    = pdfStream
           L_TT_pdf_param.obj_parameter = "TextX".
  END.
  L_TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_TextX */

PROCEDURE pdf_set_TextY :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE cInsertPageMode AS CHARACTER   NO-UNDO.

  DEFINE BUFFER L_TT_pdf_param FOR TT_pdf_param.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* Automatically create a new page if we are trying to go past the bottom margin */
  cInsertPageMode = pdf_get_parameter2(pdfStream, "insertPageMode", "append").
  IF pdf_Angle(pdfStream) = 0 AND pdfValue <= pdf_BottomMargin(pdfStream)
     AND NOT pdf_ForFooter AND pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0"
  THEN DO:
      IF cInsertPageMode = "append" THEN /* 01-APR-2015 jcc: new parameter */
          RUN pdf_new_page2(pdfStream, pdf_Orientation(pdfStream)).
      ELSE IF cInsertPageMode = "insert" THEN
          RUN pdf_insert_page(pdfStream, pdf_Page(pdfStream), "after").
      ELSE DO: /* "next" */
          EMPTY TEMP-TABLE tt_state_op.          
          RUN pdf_set_Page (pdfStream, pdf_Page(pdfStream) + 1).
      END. 
    pdfValue = pdf_TextY(pdfStream).
  END.

  /* RUN _pdf_set_parameter_priv (pdfStream, "TextY", STRING(pdfValue)). */
  FIND FIRST L_TT_pdf_param
       WHERE L_TT_pdf_param.obj_stream    = pdfStream
         AND L_TT_pdf_param.obj_parameter = "TextY" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL L_TT_pdf_param THEN DO:
    CREATE L_TT_pdf_param.
    ASSIGN L_TT_pdf_param.obj_stream    = pdfStream
           L_TT_pdf_param.obj_parameter = "TextY".
  END.
  L_TT_pdf_param.obj_value = STRING(pdfValue).

  /* jcc 7-SEP-2010: remove this code as it caused problems with 90° - fixed the Y increment in pdf_text at the same time. */
  /*
  CASE pdf_Angle(pdfStream):
    WHEN 90 THEN
      L_TT_pdf_param.obj_value = dec2string(pdf_textY(pdfStream) - pdfValue - pdf_PointSize(pdfStream)).
    WHEN 270 THEN
      L_TT_pdf_param.obj_value = STRING(pdfValue).
    OTHERWISE
      L_TT_pdf_param.obj_value = STRING(pdfValue).
  END CASE.
  */

END. /* pdf_set_TextY */

/* 14-AUG-2012 jcc */
PROCEDURE pdf_set_TextXY :
  DEFINE INPUT  PARAMETER pdfStream       AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pdfValueX       AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER pdfValueY       AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER pdfUpdateMatrix AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE cInsertPageMode AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMatrix         AS CHARACTER   NO-UNDO.

  DEFINE BUFFER L_TT_pdf_param FOR TT_pdf_param.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValueX) + "," + dec2string(pdfValueY) + "," + STRING(pdfUpdateMatrix)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* RUN _pdf_set_parameter_priv (pdfStream, "TextX", STRING(pdfValueX)). */
  FIND FIRST L_TT_pdf_param
       WHERE L_TT_pdf_param.obj_stream    = pdfStream
         AND L_TT_pdf_param.obj_parameter = "TextX" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL L_TT_pdf_param THEN DO:
    CREATE L_TT_pdf_param.
    ASSIGN L_TT_pdf_param.obj_stream    = pdfStream
           L_TT_pdf_param.obj_parameter = "TextX".
  END.
  L_TT_pdf_param.obj_value = STRING(pdfValueX).

  /* Automatically create a new page if we are trying to go past the bottom margin */
  cInsertPageMode = pdf_get_parameter2(pdfStream, "insertPageMode", "append").
  IF pdf_Angle(pdfStream) = 0 AND pdfValueY <= pdf_BottomMargin(pdfStream)
     AND NOT pdf_ForFooter AND pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0"
  THEN DO:
    /* 21-NOV-2014 jcc: save, reset, create the new page, then restore the tags parameters
       leading to underline or colors so that the page header/footer does not get underlined/striked...  */
    IF pdf_WrapText THEN DO:
        DEFINE VARIABLE cColor          AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE cLinkCount      AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE cStrikeCount    AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE cUnderlineCount AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE deX             AS DECIMAL     NO-UNDO.
        cColor          = pdf_get_parameter2(pdfStream, "___Color", "0,0,0").
        cLinkCount      = pdf_get_parameter(pdfStream, "LinkCount").
        cStrikeCount    = pdf_get_parameter(pdfStream, "StrikeCount").
        cUnderlineCount = pdf_get_parameter(pdfStream, "UnderlineCount").
        deX             = pdf_TextX(pdfStream).
        RUN pdf_text_color (pdfStream, 0, 0, 0).
        RUN _pdf_set_parameter_priv (pdfStream, "LinkCount",      "0").
        RUN _pdf_set_parameter_priv (pdfStream, "StrikeCount",    "0").
        RUN _pdf_set_parameter_priv (pdfStream, "UnderlineCount", "0").
    END.

    IF cInsertPageMode = "append" THEN /* 01-APR-2015 jcc: new parameter */
        RUN pdf_new_page2(pdfStream, pdf_Orientation(pdfStream)).
    ELSE IF cInsertPageMode = "insert" THEN
        RUN pdf_insert_page(pdfStream, pdf_Page(pdfStream), "after").
    ELSE DO: /* "next" */
        EMPTY TEMP-TABLE tt_state_op.
        RUN pdf_set_Page (pdfStream, pdf_Page(pdfStream) + 1).
    END.

    pdfValueY = pdf_TextY(pdfStream).

    IF pdf_WrapText THEN DO:
        RUN pdf_set_TextX (pdfStream, deX).
        RUN pdf_rgb (pdfStream, "pdf_text_color", cColor).
        RUN _pdf_set_parameter_priv (pdfStream, "___Color",       cColor).
        RUN _pdf_set_parameter_priv (pdfStream, "LinkCount",      cLinkCount).
        RUN _pdf_set_parameter_priv (pdfStream, "StrikeCount",    cStrikeCount).
        RUN _pdf_set_parameter_priv (pdfStream, "UnderlineCount", cUnderlineCount).
    END.
  END.

  /* RUN _pdf_set_parameter_priv (pdfStream, "TextY", STRING(pdfValueY)). */
  FIND FIRST L_TT_pdf_param
       WHERE L_TT_pdf_param.obj_stream    = pdfStream
         AND L_TT_pdf_param.obj_parameter = "TextY" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL L_TT_pdf_param THEN DO:
    CREATE L_TT_pdf_param.
    ASSIGN L_TT_pdf_param.obj_stream    = pdfStream
           L_TT_pdf_param.obj_parameter = "TextY".
  END.
  L_TT_pdf_param.obj_value = STRING(pdfValueY).

  IF pdfUpdateMatrix THEN DO:
      /* 23-APR-2014 jcc: commented: no need to fetch the current matrix, as setTextOperator will multiply it with the matrix we build here */
      /* FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream */
          /* AND tt_state_op.type     = "T" */
          /* AND tt_state_op.operator = "Tm" NO-ERROR. */
      /* IF AVAILABLE tt_state_op THEN DO: */
          /* cMatrix = tt_state_op.opvalue. */
          /* ENTRY(5, cMatrix, " ") = dec2string(pdfValueX). */
          /* ENTRY(6, cMatrix, " ") = dec2string(pdfValueY). */
      /* END. */
      /* ELSE */
          cMatrix = "1 0 0 1 " + dec2string(pdfValueX) + " " + dec2string(pdfValueY).

      RUN setTextOperator (pdfStream, "Tm", cMatrix).
  END.

END. /* pdf_set_TextXY */

PROCEDURE pdf_set_GraphicX :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL DECIMALS 4 NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 31-MAR-2015 jcc: why? It might be needed, and is ok with PDF spec. */
  /* IF pdfValue < 0 THEN {pdferror.i &msg="'Value cannot be less than 0!'" &return=YES}. */

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "GraphicX" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "GraphicX".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_GraphicX */

PROCEDURE pdf_set_GraphicY :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL DECIMALS 4 NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 31-MAR-2015 jcc: why? It might be needed, and is ok with PDF spec. */
  /* IF pdfValue < 0 THEN {pdferror.i &msg="'Value cannot be less than zero!'" &return=YES}. */

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "GraphicY" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "GraphicY".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_GraphicY */

FUNCTION pdf_GraphicX RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "GraphicX"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN DEC(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_GraphicX */

FUNCTION pdf_GraphicY RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "GraphicY"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN DEC(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END. /* pdf_GraphicY */

PROCEDURE pdf_set_info :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAttribute AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfvalue     AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfAttribute) + "," + QUOTER(pdfvalue)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF LOOKUP(pdfAttribute,"Author,Creator,Producer,Keywords,Subject,Title,OutputTo") = 0 THEN
      {pdferror.i &msg="'Invalid Attribute passed!'" &return=YES}.

  IF NOT CAN-FIND( FIRST TT_pdf_info
                   WHERE TT_pdf_info.obj_stream = pdfStream
                     AND TT_pdf_info.info_attr  = pdfAttribute NO-LOCK)
  THEN DO:
    CREATE TT_pdf_info.
    ASSIGN TT_pdf_info.obj_stream = pdfStream
           TT_pdf_info.info_attr  = pdfAttribute.
  END.

  /* RUN pdf_replace_text(INPUT-OUTPUT pdfValue). */ /* 19-FEB-2010 jcc: not needed anymore as we output as unicode */

  TT_pdf_info.info_value = pdfValue.

END. /* pdf_set_info */

FUNCTION pdf_get_info RETURNS CHARACTER ( INPUT pdfStream    AS CHARACTER,
                                          INPUT pdfAttribute AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  IF LOOKUP(pdfAttribute,"Author,Creator,Producer,Keywords,Subject,Title,OutputTo") = 0 THEN
      {pdferror.i &msg="'Invalid Attribute passed!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_info WHERE TT_pdf_info.obj_stream = pdfStream
                           AND TT_pdf_info.info_attr  = pdfAttribute
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_info THEN DO:
    RETURN TT_pdf_info.info_value.
  END.

  ELSE
    RETURN "".
END FUNCTION. /* pdf_get_info */

FUNCTION pdf_get_pdf_info RETURNS CHARACTER
        (pdfSTREAM AS CHARACTER,
         pdfID     AS CHARACTER,
         pInfo     AS CHARACTER):
/* Returns information about a pdf loaded with pdf_open_pdf */
/* i.e.  numPages = integer(pdf_info("SPDF",1,"pages"))     */
/*       author   = pdf_info("SPDF",1,"author")             */

    FOR FIRST TT_info
        WHERE TT_Info.obj_stream = pdfSTREAM /* 16-DEC-2014 jcc: was missing */
          AND TT_info.pdf_id     = pdfID
          AND TT_info.info_name  = pInfo NO-LOCK:
      RETURN info_value.
    END.

  RETURN "".

END FUNCTION.  /* pdf_get_pdf_info */

/* starts a new subpath */
PROCEDURE pdf_move_to :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS DECIMAL NO-UNDO. /* 11-MAY-2015 jcc: INT -> DEC */
  DEFINE INPUT PARAMETER pdfToY       AS DECIMAL NO-UNDO. /* 11-MAY-2015 jcc: INT -> DEC */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfToX) + "," + STRING(pdfToY)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        /* STRING(pdfToX) + " " + STRING(pdfToY) + " m", */
                        dec2string(pdfToX) + " " + dec2string(pdfToY) + " m",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream, pdfToY).
  RUN pdf_set_GraphicX (pdfStream, pdfToX).

END. /* pdf_moveto */

/* 16-AUG-2012 jcc: new */
PROCEDURE pdf_path_add_segment :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToY       AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfToX) + "," + STRING(pdfToY)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        STRING(pdfToX) + " " + STRING(pdfToY) + " l",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream, pdfToY).
  RUN pdf_set_GraphicX (pdfStream, pdfToX).

END. /* pdf_path_add_segment */

PROCEDURE pdf_rect :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.    /* JES ADDED */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfFromX) + "," + STRING(pdfFromY) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "w", dec2string(pdfWeight)).
  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        /* dec2string(pdfWeight) */
                        /* + " w" + CHR(10)  /* JES ADDED */ */
                        STRING(pdfFromX) + " " + STRING(pdfFromY) + " "
                        + STRING(pdfWidth) + " " + STRING(pdfHeight)
                        + " re" + CHR(10)
                        + "B", /* B: fill then stroke */
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream, pdfFromY + pdfHeight).
  RUN pdf_set_GraphicX (pdfStream, pdfFromX + pdfWidth).

END. /* pdf_rect */

PROCEDURE pdf_rectdec :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.    /* JES ADDED */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfFromX) + "," + dec2string(pdfFromY) + "," + dec2string(pdfWidth) + "," + dec2string(pdfHeight) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "w", dec2string(pdfWeight)).
  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        /* dec2string(pdfWeight) */
                        /* + " w" + CHR(10)  /* JES ADDED */ */
                        dec2string(pdfFromX) + " " + dec2string(pdfFromY) + " " /* 11-MAR-2010 jcc: replace STRING by dec2string (as per Donato, 11/3/2010) */
                        + dec2string(pdfWidth) + " " + dec2string(pdfHeight)
                        + " re" + CHR(10)
                        + "B",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream, pdfFromY + pdfHeight).
  RUN pdf_set_GraphicX (pdfStream, pdfFromX + pdfWidth).

END. /* pdf_rectdec */

PROCEDURE pdf_rect2 :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.    /* JES ADDED */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfFromX) + "," + STRING(pdfFromY) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "w", dec2string(pdfWeight)).
  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        /* dec2string(pdfWeight) */
                        /* + " w" + CHR(10)  /* JES ADDED */ */
                        STRING(pdfFromX) + " " + STRING(pdfFromY) + " "
                        + STRING(pdfWidth) + " " + STRING(pdfHeight)
                        + " re" + CHR(10)
                        + "S", /* S: stroke (no fill) */
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream, pdfFromY + pdfHeight).
  RUN pdf_set_GraphicX (pdfStream, pdfFromX + pdfWidth).

END. /* pdf_rect2 */

PROCEDURE pdf_circle :
  /* Note:  pdfX and pdfY represent the center point of the circle.  These
            values become the new Graphic X and Y points after the drawing of
            the circle.  If you want the circle to be filled use pdf_stroke_fill
  */
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfX         AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY         AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfRadius    AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE l_Constant    AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE VARIABLE l_Length      AS DECIMAL DECIMALS 5 NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfX) + "," + dec2string(pdfY) + "," + dec2string(pdfRadius) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  l_Constant = 0.5522847498. /* (4/3)*(sqrt(2)-1) - who knows where this calc came from */

  l_Length = pdfRadius * l_Constant.

  /* First Quadrant - Upper Right */
  RUN pdf_move_to(pdfStream, pdfX + pdfRadius, pdfY).

  RUN pdf_curve(pdfStream,
                pdfX + pdfRadius,
                l_Length + pdfY,
                l_Length + pdfX,
                pdfY + pdfRadius,
                pdfX,
                pdfY + pdfRadius,
                pdfWeight).

  /* Second Quadrant - Upper Left */
  RUN pdf_curve(pdfStream,
                pdf_GraphicX(pdfstream) - l_Length,
                pdf_GraphicY(pdfStream),
                pdfX - pdfRadius,
                pdf_GraphicY(pdfstream) - pdfRadius + l_Length,
                pdfX - pdfRadius,
                pdf_GraphicY(pdfStream) - pdfRadius,
                pdfWeight).

  /* Third Quadrant - Lower Left */
  RUN pdf_curve(pdfStream,
                pdfX - pdfRadius,
                pdfY - l_Length,
                pdfX - l_Length,
                pdfY - pdfRadius,
                pdfX,
                pdfY - pdfRadius,
                pdfWeight).

  /* Fourth Quadrant - Lower Right */
  RUN pdf_curve(pdfStream,
                pdfX + l_Length,
                pdfY - pdfRadius,
                pdfX + pdfRadius,
                pdfY - l_Length,
                pdfX + pdfRadius,
                pdfY,
                pdfWeight).

  /* Close the Path */
  RUN pdf_close_path(pdfStream).

  /* Set the current point to be the current Graphic X/Y Location */
  RUN pdf_set_GraphicY (pdfStream, pdfY).
  RUN pdf_set_GraphicX (pdfStream, pdfX).
  
END. /* pdf_circle */

/* pdf_ellipse (c) teksea.com http://teksea.com/programming.htm */
PROCEDURE pdf_ellipse :
/* Note: pdfX and pdfY represent the center point of the ellipse. These
values become the new Graphic X and Y points after the drawing of
the ellipse. If you want the ellipse to be filled use pdf_stroke_fill
*/
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX         AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfY         AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdf_a        AS DECIMAL     NO-UNDO. /* we will use this for major axis */
  DEFINE INPUT PARAMETER pdf_b        AS DECIMAL     NO-UNDO. /* we will use this for minor axis */
  DEFINE INPUT PARAMETER eccentricity AS DECIMAL     NO-UNDO. /* we will use this for eccentricity, in general it will control the oval shape of the ellipse */
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL     NO-UNDO.

  /* i will use this for eccentricity of the ellipse - it will control the shape of the ellipse */
  DEFINE VARIABLE l_Length AS DECIMAL DECIMALS 5 NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfX) + "," + dec2string(pdfY) + "," + dec2string(pdf_a) + "," + dec2string(pdf_b) + "," + dec2string(eccentricity) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  l_Length = pdf_a * eccentricity.

  /* First Quadrant - Upper Right */
  RUN pdf_move_to(pdfStream, pdfX + pdf_a, pdfY).

  /* 03-APR-2015 jcc: removed useless and bug prone calls to dec2string */

  RUN pdf_curve(pdfStream,
    pdfX + pdf_a,
    l_Length + pdfY,
    l_Length + pdfX,
    pdfY + pdf_b,
    pdfX,
    pdfY + pdf_b,
    pdfWeight).

  /* Second Quadrant - Upper Left */
  RUN pdf_curve(pdfStream,
    pdf_GraphicX(pdfstream) - l_Length,
    pdf_GraphicY(pdfStream),
    pdfX - pdf_a,
    pdf_GraphicY(pdfstream) - pdf_b + l_Length,
    pdfX - pdf_a,
    pdf_GraphicY(pdfStream) - pdf_b,
    pdfWeight).

  /* Third Quadrant - Lower Left */
  RUN pdf_curve(pdfStream,
    pdfX - pdf_a,
    pdfY - l_Length,
    pdfX - l_Length,
    pdfY - pdf_b,
    pdfX,
    pdfY - pdf_b,
    pdfWeight).

  /* Fourth Quadrant - Lower Right */
  RUN pdf_curve(pdfStream,
    pdfX + l_Length,
    pdfY - pdf_b,
    pdfX + pdf_a,
    pdfY - l_Length,
    pdfX + pdf_a,
    pdfY,
    pdfWeight).

  /* Close the Path */
  RUN pdf_close_path(pdfStream).

  /* Set the current point to be the current Graphic X/Y Location */
  RUN pdf_set_GraphicY (pdfStream, pdfY).
  RUN pdf_set_GraphicX (pdfStream, pdfX).
END. /* pdf_ellipse */

PROCEDURE pdf_curve :
  /* A Bezier curve is added from the current Graphic X/Y Location to X3/Y3 
     using X1/Y1 and X2/Y2) as the control points. The X3/Y3 of the curve 
     becomes the new Graphic X/Y Location.  */
     
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX1     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfY1     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfX2     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfY2     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfX3     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfY3     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight AS DECIMAL     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfX1) + "," + dec2string(pdfY1) + "," + dec2string(pdfX2) + "," + dec2string(pdfY2) + "," + dec2string(pdfX3) + "," + dec2string(pdfY3) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdf_GraphicY(pdfStream) = 0 OR pdf_GraphicX(pdfStream) = 0 THEN {pdferror.i &msg="'Graphic X/Y location has not been initialized!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "w", dec2string(pdfWeight)).
  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          /* /* igc - rem CHR(10) + */ dec2string(pdfWeight) */
                        /* + " w" + CHR(10) */
                        /* + STRING(pdf_FillRed(pdfstream)) + " " */
                        /* + STRING(pdf_FillGreen(pdfstream)) + " " */
                        /* + STRING(pdf_FillBlue(pdfstream)) + " rg " + CHR(10) */
                        dec2string(pdfX1) + " " + dec2string(pdfY1)
                        + " " + dec2string(pdfX2) + " " + dec2string(pdfY2) + " "
                        + dec2string(pdfX3) + " " + dec2string(pdfY3) + " c",
                        "",
                        "").

    /* Set the new Graphic Points */
  RUN pdf_set_GraphicY (pdfStream, pdfY3).
  RUN pdf_set_GraphicX (pdfStream, pdfX3).
  
END. /* pdf_curve */

PROCEDURE pdf_close_path:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        /* igc - rem CHR(10) + */ "B",
                        "",
                        "").
END.

/* 03-APR-2015 jcc: new */
PROCEDURE pdf_close_path2:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        "S",
                        "",
                        "").
END.

PROCEDURE pdf_stroke_color :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed       AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen     AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue      AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  RUN pdf_set_StrokeRed(pdfStream, pdfRed).
  RUN pdf_set_StrokeGreen(pdfStream, pdfGreen).
  RUN pdf_set_StrokeBlue(pdfStream, pdfBlue).

  RUN setGfxOperator(pdfStream, "RG", dec2string(pdfRed) + " " + dec2string(pdfGreen) + " " + dec2string(pdfBlue)).
END. /* pdf_stroke_color */

PROCEDURE pdf_stroke_fill :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed       AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen     AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue      AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  RUN pdf_set_FillRed(pdfStream, pdfRed).
  RUN pdf_set_FillGreen(pdfStream, pdfGreen).
  RUN pdf_set_FillBlue(pdfStream, pdfBlue).

  RUN setGfxOperator(pdfStream, "rg", dec2string(pdfRed) + " " + dec2string(pdfGreen) + " " + dec2string(pdfBlue)).
END. /* pdf_stroke_fill */

PROCEDURE pdf_set_dash :
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOn      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOff     AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfOn) + "," + STRING(pdfOff)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfOn = 0 AND pdfOff = 0 THEN DO: /* 02-APR-2015 jcc: add dash reset */
      RUN setGfxOperator(pdfStream, "d", "[] 0").
  END.
  ELSE DO:
      IF pdfOn  < 0 THEN pdfOn = 1.
      IF pdfOff < 0 THEN pdfOff = 1.

      RUN setGfxOperator(pdfStream, "d", "[" + STRING(pdfOn) + " " + STRING(pdfOff) + "] 0").
  END.
END. /* pdf_set_dash */

/* 02-APR-2015 jcc: new */
PROCEDURE pdf_set_dash_pattern :
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER cDashArray AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER iDashPhase AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + cDashArray). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "d", "[" + cDashArray + "] " + STRING(iDashPhase)).
END. /* pdf_set_dash2 */

PROCEDURE pdf_set_linejoin :
  /* Note:  This procedure allows you to define the Line Join Styles.  This will
            typically be used when drawing a Rectangle. Possible values are:
              0 - Miter Join
              1 - Round Join
              2 - Bevel Join
  */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfJoin    AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfJoin)). &ENDIF

  IF pdfJoin  < 0 OR pdfJoin > 2 THEN pdfJoin = 0.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "j", STRING(pdfJoin)).
  /* RUN OutputTextContent(pdfStream, */
                        /* "GRAPHIC", */
                        /* cLineJoin + " j", */
                        /* "", */
                        /* ""). */

END. /* pdf_set_linejoin */

/* 16-AUG-2012 jcc: new */
PROCEDURE pdf_set_linecap :
  /* Note:  This procedure allows you to define the Line Cap Styles.  This will
            typically be used when drawing an open path. Possible values are:
              0 - Butt cap
              1 - Round cap
              2 - Projective square cap
  */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfCap     AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfCap)). &ENDIF

  IF pdfCap  < 0 OR pdfCap > 2 THEN pdfCap = 0.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "J", STRING(pdfCap)).

END. /* pdf_set_linecap */

PROCEDURE pdf_line :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToY       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfFromX) + "," + STRING(pdfFromY) + "," + STRING(pdfToX) + "," + STRING(pdfToY) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN setGfxOperator(pdfStream, "w", dec2string(pdfWeight)).
  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          /* dec2string(pdfWeight) + " w" + CHR(10) */
                        STRING(pdfFromX) + " " + STRING(pdfFromY) + " m" + CHR(10)
                        + STRING(pdfToX) + " " + STRING(pdfToY) + " l" + CHR(10)
                        + "S",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream, pdfToX).
  RUN pdf_set_GraphicY(pdfstream, pdfToY).

END. /* pdf_line */

PROCEDURE pdf_line_dec :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfToY       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfFromX) + "," + dec2string(pdfFromY) + "," + dec2string(pdfToX) + "," + dec2string(pdfToY) + "," + dec2string(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 28-JUL-2012 jcc: fix fromY and ToY, they were somehow wrongly converted to integer */
  RUN setGfxOperator(pdfStream, "w", dec2string(pdfWeight)).
  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          /* dec2string(pdfWeight) + " w" + CHR(10) */
                        dec2string(pdfFromX) + " " + dec2string(pdfFromY) + " m" + CHR(10)
                        + dec2string(pdfToX) + " " + dec2string(pdfToY) + " l" + CHR(10)
                        + "S",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream, pdfToX).
  RUN pdf_set_GraphicY(pdfstream, pdfToY).

END. /* pdf_line_dec */

PROCEDURE pdf_watermark:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFont     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSize     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed      AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen    AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue     AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX        AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY        AS INTEGER   NO-UNDO.

  DEFINE VARIABLE L_Font      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_PointSize AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + QUOTER(pdfFont) + "," + STRING(pdfSize) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue) + "," + STRING(pdfX) + "," + STRING(pdfY)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
  /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfText). */

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = pdfFont
                           /*NO-LOCK*/ NO-ERROR.
  IF NOT AVAIL TT_pdf_font THEN {pdferror.i &msg="'Font ' + QUOTER(pdfFont) + ' has not been loaded!'" &return=YES}.

  /* 22-FEB-2010 jcc: Remember we used the font */
  TT_pdf_font.used_flag = TRUE.

  RUN OutputTextContent(pdfStream, 
                        "WATERMARK",
                          "BT" + CHR(10)
                        + STRING(TT_pdf_font.font_tag) + " " + STRING(pdfSize) +  " Tf" {&debugComment} + CHR(10)
                        + "1 0 0 1 " + STRING(pdfX) +  " " 
                                     + STRING(pdfY) + " Tm" {&debugComment} + CHR(10)
                        + " " + dec2string(pdfRed) + " "
                        + dec2string(pdfGreen) + " "
                        + dec2string(pdfBlue) + " rg " + CHR(10)
                        + "T* (",
                        pdfText,
                        ") Tj" {&debugComment} + CHR(10) + "ET").

END. /* pdf_watermark */

PROCEDURE pdf_text :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Width      AS INTEGER     NO-UNDO.
  /* DEFINE VARIABLE pdfTextOut   AS CHARACTER   NO-UNDO. */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 01-OCT-2014 jcc: now done in _PutStreamContent_flush */
  /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfTextOut). */

  RUN OutputTextContent(pdfStream,
                        "TEXT",
                        "(",
                        pdfText,
                        ") Tj" {&debugComment}).

  L_Width = pdf_text_width (pdfStream, pdfText) * IF pdf_scaleX(pdfStream) <> ? THEN pdf_scaleX(pdfStream) ELSE 1. /* 07-JUL-2010 jcc: manage scale */

  CASE pdf_Angle(pdfStream) MODULO 360:
    WHEN 0 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) + L_width ).
    WHEN 180 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) - L_width ).
    WHEN 90 THEN
        RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) + L_width ).
        /* jcc 7-SEP-2010 - fixed pdf_set_textY to fix a bug and be able to manage Y normally - i.e. 90° is not a special case anymore. */
        /* RUN pdf_set_TextY( pdfStream, - L_width ). */
    WHEN 270 THEN
      RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) - L_width ).
    OTHERWISE DO:
        RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) + L_width * math_cos(DECIMAL(pdf_Angle(pdfStream))) ).
        RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) + L_width * math_sin(DECIMAL(pdf_Angle(pdfStream))) ).
    END.
  END CASE.

END. /* pdf_text */

PROCEDURE pdf_text_char :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Width       AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue <= 0 OR pdfValue > 377 THEN {pdferror.i &msg="'Value must be >= 1 and <= 377!'" &return=YES}.

  RUN OutputTextContent(pdfStream, 
                        "TEXT",
                        "(",
                        "~\" + STRING(pdfValue, "999"),
                        ") Tj" {&debugComment}).

  /* Increase by 1 Character Length */
  L_Width = pdf_text_width (pdfStream, "1").

  /* jcc 07-SEP-2010: also fix here, as in pdf_text */
  CASE pdf_Angle(pdfStream) MODULO 360:
    WHEN 0 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) + L_width ).
    WHEN 180 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) - L_width ).
    WHEN 90 THEN
        RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) + L_width ).
    WHEN 270 THEN
      RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) - L_width ).
    OTHERWISE DO:
        RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) + L_width * math_cos(DECIMAL(pdf_Angle(pdfStream))) ).
        RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) + L_width * math_sin(DECIMAL(pdf_Angle(pdfStream))) ).
    END.
  END CASE.

END. /* pdf_text_char */

PROCEDURE pdf_text_charxy :
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfText   AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX      AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfY      AS INTEGER     NO-UNDO.

  DEFINE VARIABLE dOldX AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dOldY AS DECIMAL     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + STRING(pdfX) + "," + STRING(pdfY)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.
  
  IF pdfX = 0 THEN {pdferror.i &msg="'Column cannot be zero!'" &return=YES}.
  IF pdfY = 0 THEN {pdferror.i &msg="'Row cannot be zero!'" &return=YES}.

  dOldX = pdf_TextX(pdfStream).
  dOldY = pdf_TextY(pdfStream).
  RUN pdf_set_TextXY (pdfStream, pdfX, pdfY, YES).

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                        "(",
                        "~\" + STRING(pdfText),
                        ") Tj" {&debugComment}).

  RUN pdf_set_TextXY (pdfStream, dOldX, dOldY, NO).
END. /* pdf_text_charxy */

PROCEDURE pdf_text_rotate :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAngle    AS INTEGER   NO-UNDO.

  DEFINE VARIABLE deCos    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE deSin    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE L_Rotate AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dAngle   AS DECIMAL     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfAngle)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 07-JUL-2010 jcc: rewrite the function according to pdf specs */
  ASSIGN
    dAngle = pdfAngle - pdf_Angle(pdfStream) /* pdfAngle is the absolute rotation angle: get the relative one */
    deCos = math_cos(DECIMAL(dAngle))
    deSin = math_sin(DECIMAL(dAngle))
    L_Rotate = dec2string(deCos) + " " + dec2string(deSin) + " " +
               dec2string(- deSin) + " " + dec2string(deCos) + " " +
               dec2string(pdf_textX(pdfStream)) + " " + dec2string(pdf_textY(pdfStream)).

  RUN setTextOperator(pdfStream, "Tm", L_Rotate).

  RUN pdf_set_angle(pdfstream, pdfAngle).

END. /* pdf_text_rotate */

PROCEDURE pdf_text_scale :
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfSx     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfSy     AS DECIMAL     NO-UNDO.

  DEFINE VARIABLE L_Scale AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dSx     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSy     AS DECIMAL     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfSx) + "," + dec2string(pdfSy)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  dSx = pdfSx / pdf_ScaleX(pdfStream).
  dSy = pdfSy / pdf_ScaleY(pdfStream).

  L_Scale = dec2string(dSx) + " 0 0 " + dec2string(dSy) + " " +
            dec2string(pdf_textX(pdfStream)) + " " + dec2string(pdf_textY(pdfStream)).

  RUN setTextOperator(pdfStream, "Tm", L_Scale).

  RUN pdf_set_scale(pdfStream, pdfSx, pdfSy).

END. /* pdf_text_scale */

PROCEDURE pdf_text_skew :
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfSa     AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfSb     AS DECIMAL     NO-UNDO.

  DEFINE VARIABLE L_Skew AS CHARACTER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfSa) + "," + dec2string(pdfSb)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  L_Skew = "1 " + dec2string(math_tan(pdfSa)) + " " + dec2string(math_tan(pdfSb)) + " 1 " +
           dec2string(pdf_textX(pdfStream)) + " " + dec2string(pdf_textY(pdfStream)).

  RUN setTextOperator(pdfStream, "Tm", L_Skew).

END. /* pdf_text_skew */

FUNCTION pdf_GetNumFittingChars RETURNS INTEGER
                                ( INPUT pdfStream   AS CHARACTER,
                                  INPUT pdfText     AS CHARACTER,
                                  INPUT pdfFromX    AS INTEGER,
                                  INPUT pdfToX      AS INTEGER ):

  DEFINE VARIABLE dPointSize AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE L_font     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_tot      AS INTEGER     NO-UNDO.

  DEFINE VARIABLE iReqFMWidth AS INTEGER NO-UNDO. /* Required font-metric width */
  DEFINE VARIABLE iCurFMWidth AS DECIMAL NO-UNDO. /* Font-metric width of chars that fit */
  DEFINE VARIABLE iCurChar    AS INTEGER NO-UNDO. /* Char index up to */

  DEFINE BUFFER TT_pdf_font_character FOR TT_pdf_font_character.

  ASSIGN
   L_font      = pdf_Font(pdfStream)
   dPointSize  = pdf_PointSize(pdfStream)
   iReqFMWidth = pdfToX - pdfFromX.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  iCurChar = 0.
  iCurFMWidth = 0.
  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = L_Font
                           NO-LOCK NO-ERROR.
  IF AVAILABLE TT_pdf_font THEN DO:
      /* 06-JUL-2014 jcc: implement it for unicode (tt_pdf_font.font_width = "") */
      IF TT_pdf_font.is_unicode THEN DO:
          DEFINE VARIABLE iNbBytes AS INTEGER   NO-UNDO.
          DEFINE VARIABLE mText  AS MEMPTR    NO-UNDO.
          RUN utf8_to_utf16be (pdfStream, pdfText, "raw", ?, OUTPUT mText).
          iNbBytes = GET-SIZE(mText).
          DO iCurChar = 1 TO iNbBytes - 1 BY 2:
              FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
                  AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                  AND TT_pdf_font_character.char_id = GET-UNSIGNED-SHORT(mText, iCurChar) NO-ERROR.
              IF AVAILABLE TT_pdf_font_character THEN DO:
                  iCurFMWidth = iCurFMWidth + string2dec(TT_pdf_font_character.char_width) / 1000 * dPointSize.
                  /* 24-FEB-2014 jcc: remember used characters when we will be subsetting the font */
                  /* 03-JUL-2014 jcc: not using pdf_subset_add_string for performance reason */
                  IF TT_pdf_font.font_subset THEN
                      TT_pdf_font_character.used = YES.
              END.
              IF iCurFMWidth > iReqFMWidth THEN DO:
                  SET-SIZE(mText) = 0.
                  RETURN INTEGER(TRUNCATE((iCurChar + 1) / 2, 0)) - 1.
              END.
          END.
          SET-SIZE(mText) = 0.
          iCurChar = iCurChar / 2.
      END.
      ELSE DO:
          /* 24-SEP-2014 jcc: must use the codepage defined for the stream instead of hardcoded iso8859-1 */
          DEFINE VARIABLE cCodePage AS CHARACTER   NO-UNDO.
          cCodePage = pdf_get_parameter2(pdfStream, "CodePage", gcDefaultCodePage).

          DO iCurChar = 1 TO LENGTH(pdfText, "character":u):
            /* Keep looping until width is too wide, then return one less char */
            iCurFMWidth = iCurFMWidth +
                           (IF TT_pdf_font.font_pitch = "FIXED" THEN
                             INTEGER(TT_pdf_font.font_width) / 1000 * dPointSize
                           ELSE
                             /* 16-SEP-2013 jcc: make it work in utf-8 sessions */
                             /* DECIMAL(INTEGER(ENTRY(ASC(SUBSTR(pdfText, iCurChar, 1, "character":u)) + 1,  /* ASCII Value of char at cur pos */ */
                             /* DECIMAL(INTEGER(ENTRY(ASC(SUBSTR(pdfText, iCurChar, 1, "character":u), "iso8859-1") + 1,  /* ASCII Value of char at cur pos */ */
                             DECIMAL(INTEGER(ENTRY(ASC(SUBSTR(pdfText, iCurChar, 1, "character":u), cCodePage) + 1,  /* ASCII Value of char at cur pos */
                                                      TT_pdf_Font.font_width,             /* Space-seperated values list widths */
                                                      " "))) / 1000 * dPointSize) NO-ERROR.
            IF iCurFMWidth > iReqFMWidth THEN
              RETURN iCurChar - 1.
          END. /* Loop through text */
      END.
  END.

  RETURN iCurChar.
END FUNCTION. /* pdf_GetNumFittingChars */

/* 21-OCT-2014 jcc: add a parameter buffer to pdf_set_font and create the compatibility procedure */
PROCEDURE _pdf_set_font : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFont     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSize     AS DECIMAL NO-UNDO.
  DEFINE PARAMETER BUFFER TT_pdf_font FOR TT_pdf_font.

  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFont) + "," + dec2string(pdfSize)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  pdfFont = REPLACE(pdfFont," ","#20").

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name = pdfFont NO-ERROR.
  IF NOT AVAIL TT_pdf_font THEN {pdferror.i &msg="'Font (' + pdfFont + ') has not been loaded!'" &return=YES}.

  /* 22-FEB-2010 jcc: Remember we used the font */
  /* 04-MAY-2014 jcc: take xobjects into account */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0" THEN
      TT_pdf_font.used_flag = TRUE.
  ELSE
      TT_pdf_font.used_by_xobject = TT_pdf_font.used_by_xobject + "," + pdf_get_parameter(pdfStream, "___inXobject").

  RUN setTextOperator(pdfStream, "Tf", TT_pdf_font.font_tag + " " + dec2string(pdfSize)).

  /* Set the Stream Font Parameter */
  RUN _pdf_set_parameter_priv (pdfStream, "Font", pdfFont).

  /* Set the Stream Font Size */
  RUN _pdf_set_parameter_priv (pdfStream, "PointSize", dec2string(pdfSize)).

  /* 06-FEB-2014 jcc: set the stream unicode flag */
  RUN _pdf_set_parameter_priv (pdfStream, "unicode", TT_pdf_font.is_unicode).

  /* 03-JUL-2014 jcc: set the stream subset flag */
  RUN _pdf_set_parameter_priv (pdfStream, "subset", TT_pdf_font.font_subset).

END. /* pdf_set_font */

PROCEDURE pdf_set_font:
    DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pdfFont     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pdfSize     AS DECIMAL NO-UNDO.

    DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

    RUN _pdf_set_font(pdfStream, pdfFont, pdfSize, BUFFER TT_pdf_font).

END PROCEDURE. /* pdf_set_font */

PROCEDURE _pdf_get_font: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfFont   AS CHARACTER   NO-UNDO.
    DEFINE PARAMETER BUFFER TT_pdf_font FOR TT_pdf_font.

    pdfFont = REPLACE(pdfFont, " ", "#20").

    FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                             AND TT_pdf_font.font_name = pdfFont NO-ERROR.
    IF NOT AVAIL TT_pdf_font THEN {pdferror.i &msg="'Font (' + pdfFont + ') has not been loaded!'" &return=YES}.

END PROCEDURE.
/* 21-OCT-2014 jcc: end */

PROCEDURE pdf_text_render :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRender   AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfRender)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfRender < 0 OR pdfRender > 3 THEN pdfRender = 0.

  RUN setTextOperator(pdfStream, "Tr", STRING(pdfRender)).

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "Render" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "Render".
  END.
  TT_pdf_param.obj_value = STRING(pdfRender).
END. /* pdf_text_render */

PROCEDURE pdf_text_color :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed      AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen    AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue     AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN pdf_set_TextRed(pdfStream, pdfRed).
  RUN pdf_set_TextGreen(pdfStream, pdfGreen).
  RUN pdf_set_TextBlue(pdfStream, pdfBlue).

  RUN setTextOperator(pdfStream, "rg", dec2string(pdfRed) + " " + dec2string(pdfGreen) + " " + dec2string(pdfBlue)).
END. /* pdf_text_color */

PROCEDURE pdf_load_font :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontAFM  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontDIF  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cFontAFMExt  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFontOptions AS CHARACTER   NO-UNDO. /* 04-JUL-2014 jcc: allow passing more than one option */.
  DEFINE VARIABLE lNoEmbed     AS LOGICAL     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFontName) + "," + QUOTER(pdfFontFile) + "," + QUOTER(pdfFontAFM) + "," + QUOTER(pdfFontDIF)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* Spaces in the Font Name are invalid so replace them with # plus two-digit hex number eg: Spaces is #20 */
  cFontOptions = ENTRY(2, pdfFontName, "|") NO-ERROR.
  pdfFontName  = REPLACE(REPLACE(ENTRY(1, pdfFontName, "|"), " ", "#20"), "#7C", "|"). /* #7C in case the font name contains a pipe */
  IF CAN-FIND(FIRST TT_pdf_font
              WHERE TT_pdf_font.obj_stream = pdfStream 
                AND TT_pdf_font.font_name  = pdfFontName NO-LOCK)
  THEN DO:
    /* 23-FEB-2010 jcc: don't do an error. If the user adds twice the same font, then do nothing */
    /* RUN pdf_error(pdfStream,"pdf_load_font","Font '" + pdfFontName + "' has already been loaded!"). */
    RETURN.
  END.

  cFontAFMExt = SUBSTRING(pdfFontAFM, R-INDEX(pdfFontAFM, ".") + 1).

  lNoEmbed = LOGICAL(GetWidgetOption2("NOEMBED", cFontOptions, "NO")).
  IF NOT lNoEmbed AND SEARCH(pdfFontFile) = ?    THEN {pdferror.i &msg="'Cannot find Font File ' + QUOTER(pdfFontFile) + ' for Loading!'" &return=YES}.
  IF SEARCH(pdfFontAFM) = ?                      THEN {pdferror.i &msg="'Cannot find Font ' + UPPER(cFontAFMExt) + ' file ' + QUOTER(pdfFontAFM) + ' for loading!'" &return=YES}.
  IF pdfFontDIF <> "" AND SEARCH(pdfFontDIF) = ? THEN {pdferror.i &msg="'Cannot find Font DIF file ' + QUOTER(pdfFontDIF) + ' for loading!'" &return=YES}.

  CREATE TT_pdf_font.
  ASSIGN
   TT_pdf_font.obj_stream   = pdfStream
   TT_pdf_font.font_name    = pdfFontName
   TT_pdf_font.font_file    = SEARCH(pdfFontFile)
   TT_pdf_font.font_afm     = pdfFontAFM
   TT_pdf_font.font_dif     = pdfFontDIF
   TT_pdf_font.font_options = cFontOptions /* 04-JUL-2014 jcc: remember the options we were called with */
   TT_pdf_font.used_flag    = TRUE /* 22-FEB-2010 jcc: if the user loads a font, then it be embedded into the pdf */
   TT_pdf_font.font_tag     = "~/" + STRING(TT_pdf_font.font_name)
   TT_pdf_font.font_embed   = NOT lNoEmbed
   TT_pdf_font.font_subset  = LOGICAL(GetWidgetOption2("SUBSET", cFontOptions, "NO"))
   .

  /* 5-FEB-2014 jcc: set the font as unicode when using a .ufm file */
  IF cFontAFMExt = "ufm" THEN
      TT_pdf_font.is_unicode = TRUE.

  RUN pdf_ParseAFMFile (BUFFER TT_pdf_font) NO-ERROR. /* 30-JAN-2014 jcc: replace OUTPUT params by the BUFFER parameter */

   TT_pdf_font.font_pitch = IF TT_pdf_font.afm_IsFixedPitch = "0" THEN "FIXED"
                            ELSE "VARIABLE".

    /* igc - Added March 29, 2003
           - need to assign l_afm_width to TT_pdf_font.font_width so that we
             can find the actual width when using pdf_text_width */
   IF TT_pdf_font.font_pitch = "VARIABLE" THEN
     TT_pdf_font.font_width = TRIM(TT_pdf_font.afm_widths).
   ELSE
     TT_pdf_font.font_width = ENTRY(1, TRIM(TT_pdf_font.afm_widths)," ").
   /* 06-JUL-2014 jcc: note: for unicode fonts, TT_pdf_font.font_width will be empty (else it would be > 32kb) */
   /* 22-OCT-2014 jcc: note: TODO: widths should be adapted when we have a dif file... */
END. /* pdf_load_font */

/* 16-OCT-2014 jcc: Load a font without the afm/ufm file. We do the ttf font parsing ourselves! */
PROCEDURE pdf_load_font2:
    DEFINE INPUT PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfFontName AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfFontFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plUnicode   AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER pdfFontDIF  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfOptions  AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE lNoEmbed AS LOGICAL     NO-UNDO.

    DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFontName) + "," + QUOTER(pdfFontFile) + "," + QUOTER(pdfFontDIF) + "," + QUOTER(pdfOptions)). &ENDIF

    IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    /* Spaces in the Font Name are invalid so replace them with # plus two-digit hex number eg: Spaces is #20 */
    pdfFontName  = REPLACE(REPLACE(pdfFontName, " ", "#20"), "#7C", "|"). /* #7C in case the font name contains a pipe */
    IF CAN-FIND(FIRST TT_pdf_font
                WHERE TT_pdf_font.obj_stream = pdfStream 
                  AND TT_pdf_font.font_name  = pdfFontName NO-LOCK)
    THEN RETURN.

    lNoEmbed = LOGICAL(GetWidgetOption2("NOEMBED", pdfOptions, "NO")).
    IF SEARCH(pdfFontFile) = ?                     THEN {pdferror.i &msg="'Cannot find Font File for Loading!'" &return=YES}.
    IF pdfFontDIF <> "" AND SEARCH(pdfFontDIF) = ? THEN {pdferror.i &msg="'Cannot find Font DIF file for loading!'" &return=YES}.

    IF NOT VALID-HANDLE(h_PDF-ParseFont) THEN DO:
        IF SEARCH("{&PDFDIR}lib/pdf_parse_font.p") = ? THEN {pdferror.i &msg="'pdf_parse_font is not installed'" &return=YES}.
        RUN {&PDFDIR}lib/pdf_parse_font.p PERSISTENT SET h_PDF-ParseFont.
    END.

    /* create tt_pdf_font & tt_pdf_font_character */
    RUN parseFont IN h_PDF-ParseFont
        (pdfStream, pdfFontName, pdfFontFile, BUFFER tt_pdf_font,
         "usecache,cid2gid,charbbox=NO,charname=NO,kerning=NO,unicode=" + STRING(plUnicode) + "," + pdfOptions).

    ASSIGN
     TT_pdf_font.font_dif     = pdfFontDIF
     TT_pdf_font.font_options = pdfOptions /* 04-JUL-2014 jcc: remember the options we were called with */
     TT_pdf_font.used_flag    = TRUE /* 22-FEB-2010 jcc: if the user loads a font, then it be embedded into the pdf */
     TT_pdf_font.font_embed   = NOT lNoEmbed
     TT_pdf_font.font_subset  = LOGICAL(GetWidgetOption2("SUBSET", pdfOptions, "NO"))
     .

END PROCEDURE.
/* 17-OCT-2014 jcc: end */

PROCEDURE pdf_font_diff:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfCharNum  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfPSName   AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFontName) + "," + STRING(pdfCharNum) + "," + QUOTER(pdfPSName)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF NOT CAN-FIND(FIRST TT_pdf_font
                  WHERE TT_pdf_font.obj_stream = pdfStream
                    AND TT_pdf_font.font_name  = pdfFontName NO-LOCK) THEN
      {pdferror.i &msg="'Cannot find Font Name = ' + pdfFontName" &return=YES}.

  CREATE TT_pdf_diff.
  ASSIGN TT_pdf_diff.obj_stream = pdfStream
         TT_pdf_diff.font_name  = pdfFontName
         TT_pdf_diff.char_num   = pdfCharNum
         TT_pdf_diff.PS_Name    = IF pdfPSName BEGINS "~/" THEN pdfPSName
                                  ELSE "~/" + pdfPSName.

END. /* pdf_font_diff */

/* 24-FEB-2010 jcc. Based on an idea from Tomasz Judycki 10/2/2007
      - when we change a character in proportional font we need to adjust
        its width to use for calculation and for final export */
PROCEDURE pdf_font_diff_width:
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfDifCharNum AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfRegCharNum AS INTEGER   NO-UNDO.

  DEFINE VARIABLE L_DifCharWidth        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_RegCharWidth        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_FontWidth           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_FontWidthNumEntries AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_Entry               AS INTEGER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFontName) + "," + STRING(pdfDifCharNum) + "," + STRING(pdfRegCharNum)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  FIND FIRST TT_pdf_font
    WHERE TT_pdf_font.obj_stream = pdfStream
      AND TT_pdf_font.font_name  = pdfFontName NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TT_pdf_font THEN {pdferror.i &msg="'Cannot find Font Name = ' + pdfFontName" &return=YES}.

  IF TT_pdf_font.font_pitch = "VARIABLE" THEN DO:
    L_FontWidth = TRIM(TT_pdf_font.font_width).
    L_FontWidthNumEntries = NUM-ENTRIES( L_FontWidth, " " ).
    /* not all base 14 fonts have full 255 characters (why???) */
    IF pdfRegCharNum >= L_FontWidthNumEntries THEN {pdferror.i &msg="'Font ' + pdfFontName + ' does not contain width definition for ASCII ' + STRING(pdfRegCharNum)" &return=YES}.

    /* in 4GL counting starts with 1 not 0 */
    pdfDifCharNum = pdfDifCharNum + 1.
    pdfRegCharNum = pdfRegCharNum + 1.
    /* check if width is different */
    L_RegCharWidth = INTEGER( ENTRY( pdfRegCharNum, L_FontWidth, " " ) ).
    IF pdfDifCharNum > L_FontWidthNumEntries THEN
      L_DifCharWidth = 0.
    ELSE
      L_DifCharWidth = INTEGER( ENTRY( pdfDifCharNum, L_FontWidth, " " ) ).
    IF L_DifCharWidth <> L_RegCharWidth THEN DO:
      IF pdfDifCharNum > L_FontWidthNumEntries THEN DO:
        /* font does not contain width def for pdfDifCharNum so lets add it */
        TT_pdf_font.font_width = TT_pdf_font.font_width +         /* original content */
          FILL(" 0", pdfDifCharNum - L_FontWidthNumEntries - 1) + /* padd with zeroes */
          " " + TRIM( STRING( l_RegCharWidth, "->>>>>>>>9" ) ).   /* new value */
      END.
      ELSE DO:
        TT_pdf_font.font_width = "".
        DO L_Entry = 1 TO NUM-ENTRIES( L_FontWidth, " " ):
          TT_pdf_font.font_width = TT_pdf_font.font_width +
            (IF L_Entry = 1 THEN "" ELSE " ") +
            (IF L_Entry = pdfDifCharNum
             THEN TRIM( STRING( l_RegCharWidth, "->>>>>>>>9" ) )
             ELSE ENTRY( L_Entry, L_FontWidth, " " )).
        END.
      END.
      /* non empty afm_widths will be exported */
      TT_pdf_font.afm_widths = TT_pdf_font.font_width.
    END.
  END. /* TT_pdf_font.font_pitch = "VARIABLE" */

  RELEASE TT_pdf_font.

END. /* pdf_font_diff_width */

PROCEDURE pdf_set_base14_codepage :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfCodePage AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cRefOrWidth       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_Base14PropFonts AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_CharNum         AS INTEGER     EXTENT 255 NO-UNDO.
  DEFINE VARIABLE L_PostScript      AS CHARACTER   EXTENT 255 NO-UNDO.
  DEFINE VARIABLE L_Regular         AS CHARACTER   EXTENT 255 NO-UNDO.
        
  DEFINE VARIABLE cDiffCodePage AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iChar         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_CodePageNo  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_Count       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_FontName    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_FontNo      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_FontWidth   AS CHARACTER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfCodePage)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* nothing to do with iso8859-1 as it is the default for pdf base14 fonts */
  IF pdfCodePage = 'iso8859-1' OR pdfCodePage = '1252' THEN
    RETURN .

  IF SEARCH(pdfCodePage + '.diff') = ? THEN {pdferror.i &msg="'Code page ' + pdfCodePage + ' not supported!'" &return=YES}.

  /* 24-SEP-2014 jcc: for utf-8 sessions, it is needed to tell pdfinclude which codepage to use */
  IF SESSION:CHARSET = "utf-8" THEN
    RUN pdf_set_parameter("sPDF", "CodePage", pdfCodePage).

  /* Variable length fonts in base 14 Type 1 fonts */
  L_Base14PropFonts = "Times-Roman,Times-Bold,Times-Italic,Times-BoldItalic,Helvetica,Helvetica-Bold,Helvetica-Oblique,Helvetica-BoldOblique".

  EMPTY TEMP-TABLE TT_Font_Widths.
  EMPTY TEMP-TABLE TT_pdf_diff.

  /* Load differences */
  cDiffCodePage = IF SESSION:CPINTERNAL = 'utf-8' THEN 'iso8859-1' ELSE SESSION:CPINTERNAL.
  INPUT STREAM S_pdf_inp FROM VALUE(SEARCH(pdfCodePage + '.diff')) CONVERT SOURCE cDiffCodePage. /* 08-MAR-2010 jcc: BUG: if ref characters, the .diff file might not be read correctly. The .diff file's codepage should be it's own one, not iso8859-1 */
  IMPORT STREAM S_pdf_inp L_PostScript. /* postscript glyph names */
  IMPORT STREAM S_pdf_inp L_CharNum.    /* character code */
  IMPORT STREAM S_pdf_inp cRefOrWidth.  /* ref (reference characters) or width (glyh widths for each base font) */
  IF cRefOrWidth = "ref" THEN
      IMPORT STREAM S_pdf_inp L_Regular.
  ELSE IF cRefOrWidth = "width" THEN DO WHILE TRUE:
    IMPORT STREAM S_pdf_inp L_FontName.
    L_FontName = TRIM(L_FontName).
    IF L_FontName = "endoffile" THEN
      LEAVE.
    CREATE TT_Font_Widths.
    ASSIGN
      TT_Font_Widths.obj_stream = pdfStream
      TT_Font_Widths.font-name  = L_FontName.
    IMPORT STREAM S_pdf_inp TT_Font_Widths.font-widths.
  END.
  ELSE
      {pdferror.i &msg="pdfCodePage + '.diff: ' + cRefOrWidth + ' unknown keyword!'" &return=YES}.
  INPUT STREAM S_pdf_inp CLOSE.

  /* create substitutions */
  DO L_Count = 1 TO 255:
    IF L_CharNum[ L_Count ] = 0 OR L_Postscript[ L_Count ] = "" THEN
      NEXT.

    /* create global substitution */
    CREATE TT_pdf_diff.
    ASSIGN TT_pdf_diff.obj_stream = pdfStream
           TT_pdf_diff.font_name  = "Base14WinAnsiEncoding"
           TT_pdf_diff.char_num   = L_CharNum[ L_Count ]
           TT_pdf_diff.PS_Name    = "~/" + L_PostScript[ L_Count ].
    /* modify character width in each variable length font */
    IF cRefOrWidth = "ref"
    THEN DO L_FontNo = 1 TO NUM-ENTRIES(L_Base14PropFonts):
        IF L_Regular[L_Count] = ? THEN
            {pdferror.i &msg="pdfCodePage + '.diff: reference character #' + STRING(L_Count) + ' is null. If the question mark ("?") is a reference character, then it must be enclosed between quotes.'" &return=YES}.

        RUN pdf_font_diff_width(pdfStream,
                                ENTRY(L_FontNo, L_Base14PropFonts),
                                L_CharNum[L_Count],
                                ASC(L_Regular[L_Count], cDiffCodePage, SESSION:CPINTERNAL)).
    END.
  END.

  /* if the diff file specifies widths instead of reference characters, 
     then update the base fonts' widths */
  IF cRefOrWidth = "width"
  THEN DO L_FontNo = 1 TO NUM-ENTRIES(L_Base14PropFonts):
    FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
        AND TT_pdf_font.font_name  = ENTRY(L_FontNo, L_Base14PropFonts) NO-ERROR.
    IF NOT AVAILABLE TT_pdf_font THEN {pdferror.i &msg="'Cannot find Font Name = ' + ENTRY(L_FontNo, L_Base14PropFonts)" &return=YES}.

    FIND FIRST TT_Font_Widths WHERE TT_Font_Widths.obj_stream = pdfStream
        AND TT_Font_Widths.font-name  = ENTRY(L_FontNo, L_Base14PropFonts) NO-ERROR.
    IF NOT AVAILABLE TT_Font_Widths THEN {pdferror.i &msg="'Cannot find widths for font ' + ENTRY(L_FontNo, L_Base14PropFonts)" &return=YES}.

    IF TT_pdf_font.font_pitch = "VARIABLE" THEN DO:
      L_FontWidth = TRIM(TT_pdf_font.font_width).
      L_FontWidth = L_FontWidth + FILL(" 0", 256 - NUM-ENTRIES(L_FontWidth, " ")). /* padd with zeroes */
      DO iChar = 1 TO 224 /*256-32*/:
        IF L_CharNum[ iChar ] = 0 THEN
          NEXT.
        ENTRY(L_CharNum[ iChar ] + 1, L_FontWidth, " ") = STRING(TT_Font_Widths.font-widths[iChar]).
      END.
      ASSIGN
        TT_pdf_font.afm_Widths = L_FontWidth
        TT_pdf_font.font_width = L_FontWidth.
    END.

  END. /* cRefOrWidth = "width" */

END. /* pdf_set_base14_encoding */
/* 24-FEB-2010 jcc: end */

PROCEDURE pdf_load_image :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageFile   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cAltColorSpace      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cColorSpace         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDataFile           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDeviceNComponents  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFilter             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFunctionParams     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFunctionPostScript AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cImgType            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPalFile            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cParams             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTrns               AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTrnsMaskFile       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iBitDepth           AS INTEGER     NO-UNDO. /* aka bits per component */.
  DEFINE VARIABLE iDepth              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iHeight             AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPalLength          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iWidth              AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfImageName) + "," + QUOTER(pdfImageFile)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF INDEX(pdfImageName," ") > 0 THEN {pdferror.i &msg="'Image Name cannot contain spaces!'" &return=YES}.

  /* 23-FEB-2010 jcc: Lonny L. Granstrom 21/12/2006 */
  /* Return without Error if Image already loaded for use */
  IF CAN-FIND(FIRST TT_pdf_image
              WHERE TT_pdf_image.obj_stream = pdfStream
                AND TT_pdf_image.image_name = pdfImageName
                AND TT_pdf_image.image_file = SEARCH(pdfImageFile)
                AND SEARCH(pdfImageFile)   <> ?
             ) THEN
    RETURN.
  /* 23-FEB-2010 jcc: end */

  IF CAN-FIND(FIRST TT_pdf_image
              WHERE TT_pdf_image.obj_stream = pdfStream 
                AND TT_pdf_image.image_name = pdfImageName NO-LOCK)
  THEN {pdferror.i &msg="'Image ' + QUOTER(pdfImageName) + ' has already been loaded!'" &return=YES}.

  IF SEARCH(pdfImageFile) = ? THEN {pdferror.i &msg="'Cannot find Image File ' + QUOTER(pdfImageFile) + ' when Loading!'" &return=YES}.

  /* 09-AUG-2011 jcc: replaced pdf_get_image_wh by new pdf_extract_image_info (implement native png, including transparency) */
  /*RUN pdf_get_image_wh (INPUT pdfStream,
                        INPUT pdfImageFile).*/
  RUN pdf_extract_image_info (pdfStream, SEARCH(pdfImageFile),
                              OUTPUT cImgType, OUTPUT iWidth, OUTPUT iHeight, OUTPUT iDepth, OUTPUT iBitDepth,
                              OUTPUT cColorSpace, OUTPUT cAltColorSpace, OUTPUT cDeviceNComponents, OUTPUT cFunctionParams, OUTPUT cFunctionPostScript,
                              OUTPUT cFilter, OUTPUT cParams, OUTPUT cDataFile,
                              OUTPUT cPalFile, OUTPUT iPalLength, OUTPUT cTrns, OUTPUT cTrnsMaskFile) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN. /* happens in case the image was unsupported or incorrect */

  CREATE TT_pdf_image.
  ASSIGN TT_pdf_image.obj_stream         = pdfStream
         TT_pdf_image.image_name         = pdfImageName
         TT_pdf_image.image_type         = cImgType
         TT_pdf_image.image_h            = iHeight
         TT_pdf_image.image_w            = iWidth
         TT_pdf_image.image_depth        = iDepth
         TT_pdf_image.image_bpc          = iBitDepth
         TT_pdf_image.image_cs           = cColorSpace
         TT_pdf_image.image_alt_cs       = cAltColorSpace
         TT_pdf_image.image_devn_comp    = cDeviceNComponents
         TT_pdf_image.image_func_params  = cFunctionParams
         TT_pdf_image.image_func_ps      = cFunctionPostScript
         TT_pdf_image.image_filter       = cFilter
         TT_pdf_image.image_params       = cParams
         TT_pdf_image.image_data         = cDataFile
         TT_pdf_image.image_palette      = cPalFile
         TT_pdf_image.image_pal_length   = iPalLength
         TT_pdf_image.image_file         = SEARCH(pdfImageFile)
         TT_pdf_image.image_tag          = "~/Im" + STRING(TT_pdf_image.image_name)
         TT_pdf_image.image_transparency = cTrns
         TT_pdf_image.image_has_smask    = cTrnsMaskFile <> ""
         .

  /* Soft mask (/SMask) for alpha transparent pngs (11-AUG-2011 jcc: added) */
  IF cTrnsMaskFile <> "" THEN DO:
      CREATE TT_pdf_image.
      ASSIGN TT_pdf_image.obj_stream         = pdfStream
             TT_pdf_image.image_name         = pdfImageName + "-mask"
             TT_pdf_image.image_type         = cImgType
             TT_pdf_image.image_h            = iHeight
             TT_pdf_image.image_w            = iWidth
             TT_pdf_image.image_depth        = iDepth / 4 /* Alpha is one of the original image R,G,B,A */
             TT_pdf_image.image_bpc          = iBitDepth
             TT_pdf_image.image_cs           = "~/DeviceGray"
             TT_pdf_image.image_filter       = cFilter
             TT_pdf_image.image_params       = REPLACE(cParams, "~/Colors 3", "~/Colors 1")
             TT_pdf_image.image_data         = cTrnsMaskFile
             TT_pdf_image.image_palette      = ""
             TT_pdf_image.image_pal_length   = 0
             TT_pdf_image.image_file         = SEARCH(pdfImageFile)
             TT_pdf_image.image_tag          = "~/Im" + STRING(TT_pdf_image.image_name)
             TT_pdf_image.image_transparency = ""
             .
  END.

END. /* pdf_load_image */

/* 12-SEP-2012 jcc: new */
PROCEDURE pdf_get_image_info:
    DEFINE INPUT  PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfImageName AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcImgType    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER piHeight     AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER piWidth      AS INTEGER     NO-UNDO.
    
    FIND TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream
                        AND TT_pdf_image.image_name = pdfImageName NO-ERROR.
    IF AVAILABLE TT_pdf_image THEN
        ASSIGN
         pcImgType = TT_pdf_image.image_type
         piHeight  = TT_pdf_image.image_h
         piWidth   = TT_pdf_image.image_w
        .
END.
/* 12-SEP-2012 jcc: end */

PROCEDURE pdf_place_image :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfImageName) + "," + STRING(pdfColumn) + "," + STRING(pdfRow) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  DEFINE VARIABLE L_PageHeight  AS INTEGER NO-UNDO.

  L_PageHeight = pdf_PageHeight(pdfStream).

  FIND FIRST TT_pdf_image
       WHERE TT_pdf_image.obj_stream = pdfStream
         AND TT_pdf_image.image_name = pdfImageName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_image THEN {pdferror.i &msg="'Cannot find Image Name for Placement!'" &return=YES}.

  /* 04-MAY-2014 jcc: Remember we used the image and where */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0" THEN
      TT_pdf_image.used_flag = TRUE.
  ELSE
      TT_pdf_image.used_by_xobject = TT_pdf_image.used_by_xobject + "," + pdf_get_parameter(pdfStream, "___inXobject").

  /* 09-AUG-2011 jcc: use image dimensions if not specified */
  IF pdfWidth = ? THEN
      pdfWidth = TT_pdf_image.image_w.
  IF pdfHeight = ? THEN
      pdfHeight = TT_pdf_image.image_h.

  RUN OutputTextContent(pdfStream, 
                        "IMAGE",
                          STRING(pdfWidth) + " 0 0 " + STRING(pdfHeight)
                        + " " + STRING(pdfColumn) + " "
                        + STRING(L_PageHeight - pdfRow) + " cm "
                        + TT_pdf_image.image_tag + " Do",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream, pdfColumn).
  RUN pdf_set_GraphicY(pdfStream, pdfRow).

END. /* pdf_place_image */

PROCEDURE pdf_place_image2 :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfImageName) + "," + STRING(pdfColumn) + "," + STRING(pdfRow) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  DEFINE VARIABLE L_PageHeight  AS INTEGER NO-UNDO.

  L_PageHeight  = pdf_PageHeight(pdfStream).

  FIND FIRST TT_pdf_image
       WHERE TT_pdf_image.obj_stream = pdfStream
         AND TT_pdf_image.image_name = pdfImageName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_image THEN {pdferror.i &msg="'Cannot find Image Name for Placement!'" &return=YES}.

  /* 04-MAY-2014 jcc: Remember we used the image and where */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0" THEN
      TT_pdf_image.used_flag = TRUE.
  ELSE
      TT_pdf_image.used_by_xobject = TT_pdf_image.used_by_xobject + "," + pdf_get_parameter(pdfStream, "___inXobject").

  /* 09-AUG-2011 jcc: use image dimensions if not specified */
  IF pdfWidth = ? THEN
      pdfWidth = TT_pdf_image.image_w.
  IF pdfHeight = ? THEN
      pdfHeight = TT_pdf_image.image_h.

  RUN OutputTextContent(pdfStream, 
                        "IMAGE2",
                          STRING(pdfWidth) + " 0 0 " + STRING(pdfHeight)
                        + " " + STRING(pdfColumn) + " "
                        + STRING(L_PageHeight - pdfRow) + " cm "
                        + TT_pdf_image.image_tag + " Do",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream, pdfColumn).
  RUN pdf_set_GraphicY(pdfStream, pdfRow).

END. /* pdf_place_image2 */

PROCEDURE pdf_skip :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.

  DEFINE BUFFER tt_state_op FOR tt_state_op.

  DEFINE VARIABLE L_Margin    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE L_Spacer    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE L_VertSpace AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE L_Y         AS DECIMAL   NO-UNDO.
  
  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  L_Spacer = INT(pdf_get_parameter(pdfStream,"LineSpacer")) NO-ERROR.
  L_VertSpace = pdf_VerticalSpace(pdfstream) NO-ERROR. 
  IF L_VertSpace NE ? AND l_VertSpace NE 0 THEN 
    L_Spacer = L_VertSpace - pdf_PointSize(pdfstream). 

  L_Margin = pdf_LeftMargin(pdfStream).

  FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
      AND tt_state_op.type     = "T"
      AND tt_state_op.operator = "Tm" 
      AND tt_state_op.is_dirty = NO NO-ERROR.
  IF AVAILABLE tt_state_op AND string2dec(ENTRY(5, tt_state_op.opvalue, " ")) = L_Margin THEN DO:
      L_Y = pdf_TextY(pdfStream) - pdf_PointSize(pdfStream) - L_Spacer.
      RUN pdf_set_TextXY (pdfStream, L_Margin, L_Y, NO).
      IF pdf_TextY(pdfStream) = L_Y THEN /* 02-OCT-2013 jcc: do the TD only if there has not been a new page created within pdf_set_textXY */
          RUN setTextOperator(pdfStream, "TD", "0 " + dec2string(-1 * (pdf_PointSize(pdfstream) + L_Spacer))). /* 09-JUN-2015 jcc: fix for negative values */
  END.
  ELSE
      RUN pdf_set_TextXY (pdfStream, L_Margin, pdf_TextY(pdfStream) - pdf_PointSize(pdfStream) - L_Spacer, YES).

END. /* pdf_skip */

PROCEDURE pdf_skipn :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfNumber   AS INTEGER NO-UNDO.

  DEFINE VARIABLE l_Ctr              AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfNumber)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfNumber <= 0 THEN {pdferror.i &msg="'Lines to skip cannot be <= zero!'" &return=YES}.

  DO l_Ctr = 1 TO pdfNumber:
    RUN pdf_skip(pdfStream).
  END.

END. /* pdf_skipn */

/* 23-APR-2014 jcc: new procedure to handle dynamic justification */
PROCEDURE _pdf_deferred_rendering: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfAlign  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfColumn AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER pdfRow    AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER pdfText   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_page    FOR TT_pdf_page.
    DEFINE BUFFER TT_pdf_stream  FOR TT_pdf_stream.
    DEFINE BUFFER tt_state_op    FOR tt_state_op.
    DEFINE BUFFER TT_pdf_xobject FOR TT_pdf_xobject.

    DEFINE VARIABLE iXobjectId AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pdfPage    AS INTEGER   NO-UNDO.

    FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream.
    IF NOT TT_pdf_stream.obj_DoingText THEN DO: /* switch to text context */
        IF TT_pdf_stream.obj_DoingGraphic THEN DO:
            TT_pdf_stream.obj_DoingGraphic = NO.
            PUT STREAM S_pdf_out UNFORMATTED "Q" {&pdfSkip}.
        END.
        TT_pdf_stream.obj_DoingText = YES.
        PUT STREAM S_pdf_out UNFORMATTED "BT" {&pdfSkip}.
    END.
    RUN flushTextOperators (pdfStream).
    FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "T"
        AND tt_state_op.operator = "Tm" NO-ERROR.
    /* 07-MAY-2014 jcc: handle xobjects */
    IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0" THEN DO:
        pdfPage = pdf_Page(pdfStream).
        FIND FIRST TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.page_nbr = pdfPage.
        IF INDEX(pdfText,"@@TOTALPages-" + pdfStream) > 0 THEN TT_pdf_page.UseTotalPages = TRUE.
        IF INDEX(pdfText,"@@PAGEno-" + pdfStream) > 0     THEN TT_pdf_page.UsePageNo     = TRUE.
    END.
    ELSE DO:
        iXobjectId = INTEGER(pdf_get_parameter(pdfStream, "___inXobject")).
        FIND FIRST TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream AND TT_pdf_xobject.xobject_id = iXobjectId.
        IF INDEX(pdfText,"@@TOTALPages-" + pdfStream) > 0 THEN TT_pdf_xobject.UseTotalPages = TRUE.
        IF INDEX(pdfText,"@@PAGEno-" + pdfStream) > 0     THEN TT_pdf_xobject.UsePageNo     = TRUE.
    END.
    PUT STREAM S_pdf_out UNFORMATTED {&pdfSkip} CHR(1)
     pdfAlign CHR(1)
     pdf_Font(pdfStream) CHR(1)
     pdf_PointSize(pdfStream) CHR(1)
     pdfColumn CHR(1)
     pdfRow CHR(1)
     IF AVAILABLE tt_state_op THEN tt_state_op.opvalue ELSE "1 0 0 1 0 0" CHR(1)
     pdfText
     {&pdfSkip}.
END PROCEDURE.

PROCEDURE pdf_text_xy :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS INTEGER NO-UNDO.

  DEFINE VARIABLE dOldX AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dOldY AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + STRING(pdfColumn) + "," + STRING(pdfRow)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfColumn = 0 THEN {pdferror.i &msg="'Column cannot be zero!'" &return=YES}.
  IF pdfRow    = 0 THEN {pdferror.i &msg="'Row cannot be zero!'" &return=YES}.

  /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
  /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfText). */

  dOldX = pdf_TextX(pdfStream).
  dOldY = pdf_TextY(pdfStream).
  iPage = pdf_Page(pdfStream).

  RUN pdf_set_TextXY (pdfStream, pdfColumn, pdfRow, YES).

  IF pdf_Page(pdfStream) > iPage THEN /* 12-JUN-2014 jcc: if we changed page, dOldY must be updated */
      dOldY = pdf_TextY(pdfStream).

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                        "(",
                        pdfText,
                        ") Tj" {&debugComment}).

  RUN pdf_set_TextXY (pdfStream, dOldX, dOldY, NO).
END. /* pdf_text_xy */

PROCEDURE pdf_text_xy_dec :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE VARIABLE dOldX AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dOldY AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + dec2string(pdfColumn) + "," + dec2string(pdfRow)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfColumn = 0 THEN {pdferror.i &msg="'Column cannot be zero!'" &return=YES}.
  IF pdfRow    = 0 THEN {pdferror.i &msg="'Row cannot be zero!'" &return=YES}.

  /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
  /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfText). */

  dOldX = pdf_TextX(pdfStream).
  dOldY = pdf_TextY(pdfStream).
  iPage = pdf_Page(pdfStream).

  RUN pdf_set_TextXY (pdfStream, pdfColumn, pdfRow, YES).

  IF pdf_Page(pdfStream) > iPage THEN /* 12-JUN-2014 jcc: if we changed page, dOldY must be updated */
      dOldY = pdf_TextY(pdfStream).

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                        "(",
                        pdfText,
                        ") Tj" {&debugComment}).

  RUN pdf_set_TextXY (pdfStream, dOldX, dOldY, NO).
END. /* pdf_text_xy_dec */

PROCEDURE pdf_text_boxed_xy :
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfText    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn  AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow     AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth   AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight  AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfJustify AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight  AS DECIMAL     NO-UNDO. /* 06-MAY-2014 jcc: INTEGER -> DECIMAL */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + STRING(pdfColumn) + "," + STRING(pdfRow) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight) + "," + QUOTER(pdfJustify) + "," + STRING(pdfWeight)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfColumn = 0 THEN {pdferror.i &msg="'Column cannot be zero!'" &return=YES}.
  IF pdfRow    = 0 THEN {pdferror.i &msg="'Row cannot be zero!'"    &return=YES}.
  IF pdfHeight = 0 THEN {pdferror.i &msg="'Height cannot be zero!'" &return=YES}.
  IF pdfWidth  = 0 THEN {pdferror.i &msg="'Width cannot be zero!'"  &return=YES}.
  IF LOOKUP(pdfJustify,"Left,Right,Center") = 0 THEN {pdferror.i &msg="'Invalid Justification option passed!'" &return=YES}.

  /* 17-MAR-2014 jcc: implement the pdfJustify parameter */
  /* 23-APR-2014 jcc: handle dynamic justification for special @@ tags */
  CASE pdfJustify:
      WHEN "Left" THEN
          RUN pdf_text_xy_dec (pdfStream,pdfText,pdfColumn,pdfRow).
      WHEN "Right" THEN
          IF INDEX(pdfText, "@@TOTALPages-" + pdfStream) > 0 OR INDEX(pdfText, "@@PageNo-" + pdfStream) > 0 THEN
              RUN _pdf_deferred_rendering (pdfStream, pdfJustify, pdfColumn + pdfWidth, pdfRow, pdfText).
          ELSE
              RUN pdf_text_xy_dec (pdfStream,pdfText,pdfColumn + pdfWidth
                                   - DECIMAL(pdf_text_width(pdfStream,pdfText)),pdfRow).
      WHEN "Center" THEN
          IF INDEX(pdfText, "@@TOTALPages-" + pdfStream) > 0 OR INDEX(pdfText, "@@PageNo-" + pdfStream) > 0 THEN
              RUN _pdf_deferred_rendering (pdfStream, pdfJustify, pdfColumn + pdfWidth / 2, pdfRow, pdfText).
          ELSE
              RUN pdf_text_xy_dec (pdfStream,pdfText,pdfColumn + pdfWidth / 2
                                   - DECIMAL(pdf_text_width(pdfStream,pdfText)) / 2,pdfRow).
  END CASE.

  IF pdfWeight gt 0 THEN /* 17-MAR-2014 jcc: create the rectangle using the right API */
    RUN pdf_rect2 (pdfStream,pdfColumn,pdfRow,pdfWidth,pdfHeight,pdfWeight).

  RUN pdf_set_GraphicY(pdfStream, pdfRow).

END. /* pdf_text_boxed_xy */

PROCEDURE pdf_text_center:
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfText   AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow    AS DECIMAL     NO-UNDO.


  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + dec2string(pdfColumn) + "," + dec2string(pdfRow)). &ENDIF

  /* 23-APR-2014 jcc: add dynamic justification when using special @@ tags */
  IF INDEX(pdfText, "@@TOTALPages-" + pdfStream) > 0 OR INDEX(pdfText, "@@PageNo-" + pdfStream) > 0 THEN
      RUN _pdf_deferred_rendering (pdfStream, "center", pdfColumn, pdfRow, pdfText).
  ELSE
      RUN pdf_text_xy_dec (pdfStream,pdfText,pdfColumn
                           - DECIMAL(pdf_text_width(pdfStream,pdfText) / 2),pdfRow).

END. /* pdf_text_center */

PROCEDURE pdf_text_at :
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfText   AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn AS INTEGER     NO-UNDO.

  DEFINE BUFFER tt_state_op FOR tt_state_op.

  DEFINE VARIABLE deLeftMargin AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dePositionAt AS DECIMAL   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + STRING(pdfColumn)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.
  
  IF pdfColumn = 0 THEN {pdferror.i &msg="'Column cannot be zero!'" &return=YES}.

  /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
  /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfTextOut). */

  /* 03-DEC-2016 jcc: new implementation. Write the text directly at the correct position, without
     space chars before, because of an Adobe Reader "bug": when you made 2 calls to pdf_text_at, writing
     "toto" at column 10 and "titi" at column 20 on the same line, then when searching text in the pdf,
     you had to search for "t o t o" in order to find "toto", because of the spaces overwriting "toto".
  /* 10-JUL-2013 jcc: must compare the left margin to the Text Matrix instead of pdf_TextX, because pdf_TextX may be different from the matrix x coordinate */
  /* IF pdf_TextX(pdfStream) <> pdf_LeftMargin(pdfStream) THEN */
  FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
    AND tt_state_op.type     = "T"
    AND tt_state_op.operator = "Tm" NO-ERROR.

  IF AVAILABLE tt_state_op AND string2dec(ENTRY(5, tt_state_op.opvalue, " ")) = pdf_LeftMargin(pdfStream) THEN DO:
      IF pdf_TextX(pdfStream) <> pdf_LeftMargin(pdfStream) THEN
          RUN setTextOperator(pdfStream, "TD", "0 0").
      /* ELSE no need to change the text position */
  END.
  ELSE
      RUN pdf_set_TextXY (pdfStream, pdf_LeftMargin(pdfStream), pdf_TextY(pdfStream), YES).
  */

  IF pdfColumn > 1 THEN
    dePositionAt = pdf_text_widthdec(pdfStream, " ") * (pdfColumn - 1).

  deLeftMargin = pdf_LeftMargin(pdfStream).

  IF pdfColumn = 1 THEN
      FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "T"
        AND tt_state_op.operator = "Tm" NO-ERROR.

  IF pdfColumn = 1 AND AVAILABLE tt_state_op AND string2dec(ENTRY(5, tt_state_op.opvalue, " ")) = deLeftMargin THEN DO:
      IF pdf_TextX(pdfStream) <> deLeftMargin THEN
          RUN setTextOperator(pdfStream, "TD", "0 0").
          /* ELSE no need to change the text position */
  END.
  ELSE
      RUN pdf_set_TextXY (pdfStream, deLeftMargin + dePositionAt, pdf_TextY(pdfStream), YES).

  RUN OutputTextContent(pdfStream, 
                        "TEXTAT",
                        "(",
                        /* FILL(" ", pdfColumn - 1) + pdfText /*Out*/ , */ /* 03-DEC-2016 jcc */
                        pdfText /*Out*/,
                        ") Tj" {&debugComment}).

  /* Increase the TextX dimension to include the new text.  The Y Dimension
     shouldn't change */
  /* RUN pdf_set_TextX(pdfStream,  pdf_TextX(pdfStream) + dePositionAt */
                              /* + pdf_text_width(pdfStream, FILL(" ", pdfColumn - 1) + pdfText)). */
  RUN pdf_set_TextX(pdfStream,  deLeftMargin + dePositionAt
                              + pdf_text_width(pdfStream, pdfText)).
END. /* pdf_text_at */

PROCEDURE pdf_text_to:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + STRING(pdfColumn)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  DEFINE VARIABLE L_LeftMargin AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_X          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_Y          AS INTEGER     NO-UNDO.
  /* DEFINE VARIABLE pdfTextOut   AS CHARACTER   NO-UNDO. */

  L_Y          = pdf_TextY(pdfStream).
  L_LeftMargin = pdf_LeftMargin(pdfStream).

  IF pdfColumn = 0 THEN {pdferror.i &msg="'Column cannot be zero!'" &return=YES}.

  /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
  /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfTextOut). */

  IF pdf_FontType(pdfStream) = "FIXED" THEN DO:

    RUN pdf_set_TextXY (pdfStream, L_LeftMargin + pdf_text_width(pdfstream, FILL(" ", pdfColumn - LENGTH(pdfText, "character":u) - 1)), L_Y, YES).

    RUN OutputTextContent(pdfStream, 
                          "TEXTTO",
                          "(",
                          pdfText /*Out*/,
                          ") Tj" {&debugComment}).

    /* Set the TextX dimension to the new text column.  The Y Dimension
       shouldn't change */
    RUN pdf_set_TextX(pdfStream,  pdf_TextX(pdfStream)
                                + pdf_text_width(pdfStream,
                                                 FILL(" ", pdfColumn - LENGTH(pdfText, "character":u) - 1) + pdfText)).

  END. /* Fixed */

  /* Variable - proportional fonts */
  ELSE DO:
    L_X = pdf_text_width(pdfStream,FILL("E",pdfColumn))
        - pdf_text_width(pdfStream, pdfText).

    RUN pdf_set_TextXY (pdfStream, L_X, L_Y, YES).

    RUN OutputTextContent(pdfStream, 
                          "TEXTTO",
                          "(",
                          pdfText /*Out*/,
                          ") Tj" {&debugComment}).

    /* Set the TextX dimension to the new text column.  The Y Dimension
       shouldn't change */
    RUN pdf_set_TextX(pdfStream, L_X + pdf_text_width(pdfstream,pdfText)).

  END. /* Variable */

END. /* pdf_text_to */

PROCEDURE pdf_text_align:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlign    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfX        AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfY        AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Width      AS DECIMAL     NO-UNDO.
  /* DEFINE VARIABLE pdfTextOut   AS CHARACTER   NO-UNDO. */

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + QUOTER(pdfAlign) + "," + STRING(pdfX) + "," + STRING(pdfY)). &ENDIF

  /* Begin Procedure Validation */
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.
  IF LOOKUP(pdfAlign,"LEFT,RIGHT,CENTER":U) = 0 THEN {pdferror.i &msg="'Invalid Alignment option passed!'" &return=YES}.
  IF pdfX = 0 THEN {pdferror.i &msg="'X location cannot be zero!'" &return=YES}.
  IF pdfY = 0 THEN {pdferror.i &msg="'Y location cannot be zero!'" &return=YES}.
  /* end of validation */

  /* 17-SEP-2013 jcc: BUG: this prevents from right/center alignment taking spaces into account */
  /* pdfText = TRIM(pdfText). */

  L_Width = pdf_text_widthdec(pdfStream, pdftext).

  IF pdfAlign <> "LEFT" AND (INDEX(pdfText, "@@TOTALPages-" + pdfStream) > 0 OR INDEX(pdfText, "@@PageNo-" + pdfStream) > 0) THEN DO:
      RUN _pdf_deferred_rendering (pdfStream, pdfAlign, pdfX, pdfY, pdfText).
      IF pdfAlign = "CENTER" THEN L_Width = L_Width / 2.
  END.
  ELSE DO:
      /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
      /* RUN pdf_replace_text (pdfStream, pdfText, OUTPUT pdfTextOut). */

      CASE pdfAlign:
        WHEN "RIGHT" THEN DO:
          /* RUN pdf_set_TextX(pdfStream, pdfX ). */
          RUN pdf_set_TextXY (pdfStream, pdfX - L_Width, pdfY, YES).
        END. /* Right Alignment */

        WHEN "LEFT" THEN DO:
          /* RUN pdf_set_TextX(pdfStream, INT(pdfX + L_Width)). */
          RUN pdf_set_TextXY (pdfStream, pdfX, pdfY, YES).
        END. /* Left Alignment */

        WHEN "CENTER" THEN DO:
          L_Width = L_Width / 2.
          /* RUN pdf_set_TextX(pdfStream, INT(pdfX + L_Width)). */
          RUN pdf_set_TextXY (pdfStream, pdfX - L_Width, pdfY, YES).
        END. /* Centered Alignment */
      END CASE.

      RUN OutputTextContent(pdfStream, 
                          "TEXTXY",
                          "(",
                          pdfText /*Out*/,
                          ") Tj" {&debugComment}).
  END.

  CASE pdfAlign:
    WHEN "RIGHT"  THEN RUN pdf_set_TextX(pdfStream, pdfX).
    WHEN "LEFT"   THEN RUN pdf_set_TextX(pdfStream, pdfX + L_Width).
    WHEN "CENTER" THEN RUN pdf_set_TextX(pdfStream, pdfX + L_Width). /* L_Width has already been divided by 2 */
  END CASE.

END. /* pdf_text_align */

PROCEDURE pdf_set_Angle :  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 07-JUL-2010 jcc: all angles are now possible */
  /*
  DEFINE VARIABLE L_option  AS CHARACTER NO-UNDO.
  L_Option = "0,45,90,135,180,225,270,315".

  IF LOOKUP(STRING(pdfValue),L_option) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_Angle","Invalid Angle option passed!").
    RETURN .
  END.
  */
  RUN _pdf_set_parameter_priv (pdfStream, "Angle", STRING(pdfValue)).

END. /* pdf_set_Angle */

PROCEDURE pdf_set_Scale :  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSx           AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfSy           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  RUN _pdf_set_parameter_priv (pdfStream, "ScaleX", STRING(pdfSx)).
  
  RUN _pdf_set_parameter_priv (pdfStream, "ScaleY", STRING(pdfSy)).

END. /* pdf_set_Scale */

PROCEDURE pdf_set_Orientation :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cOrientation AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_height     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_width      AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF LOOKUP(pdfValue,"Portrait,Landscape") = 0 THEN
      {pdferror.i &msg="'Invalid Orientation option passed!'" &return=YES}.

  /* 14-AUG-2012 jcc: do not switch width and height when using the external page size */
  IF pdfValue = "Landscape" AND pdf_get_parameter(pdfStream, "UseExternalPageSize") = "TRUE" THEN
      RETURN.

  cOrientation = pdf_get_parameter2(pdfStream, "Orientation", "").

  /* IF B_TT_pdf_param.obj_value <> pdfValue THEN DO: */
  IF cOrientation <> pdfValue THEN DO:
    L_width  = pdf_PageWidth(pdfStream).
    L_height = pdf_PageHeight(pdfStream).
    IF pdfValue = "Landscape" THEN DO:
      RUN pdf_set_PageWidth (pdfStream, L_height).
      RUN pdf_set_PageHeight(pdfStream, L_width).
    END.
    ELSE DO:
      /* IF B_TT_pdf_param.obj_value = "Landscape" THEN DO: */
      IF cOrientation = "Landscape" THEN DO:
        L_width  = pdf_PageHeight(pdfStream).
        L_height = pdf_PageWidth(pdfStream).
      END.

      RUN pdf_set_PageWidth (pdfStream, L_width).
      RUN pdf_set_PageHeight(pdfStream, L_height).
    END.
  END.

  /* B_TT_pdf_param.obj_value = pdfValue. */
  RUN _pdf_set_parameter_priv (pdfStream, "Orientation", pdfValue).

END. /* pdf_set_Orientation */

PROCEDURE pdf_set_VerticalSpace :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Vertical Space cannot be zero!'" &return=YES}.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "VerticalSpace" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "VerticalSpace".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_VerticalSpace */

PROCEDURE pdf_set_LeftMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Left Margin cannot be zero!'" &return=YES}.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "LeftMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "LeftMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_LeftMargin */

/* 23-MAR-2015 jcc: new */
PROCEDURE pdf_set_RightMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Right Margin cannot be zero!'" &return=YES}.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "RightMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "RightMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_RightMargin */

PROCEDURE pdf_set_TopMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue = 0 THEN {pdferror.i &msg="'Top Margin cannot be zero!'" &return=YES}.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "TopMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "TopMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_TopMargin */

PROCEDURE pdf_set_BottomMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfValue <= 0 THEN {pdferror.i &msg="'Bottom Margin cannot be <= zero!'" &return=YES}.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "BottomMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "BottomMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_BottomMargin */

PROCEDURE pdf_set_PaperType :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_width   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_height  AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfValue)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF LOOKUP(pdfValue,"A0,A1,A2,A3,A4,A5,A6,B5,LETTER,LEGAL,LEDGER") = 0 THEN
      {pdferror.i &msg="'Invalid Paper Type option passed!'" &return=YES}.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "PaperType" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "PaperType".
  END.

  /* Set the Paper Type */
  TT_pdf_param.obj_value = pdfValue.

  /* Determine the Paper Height and Width */
  CASE pdfValue:
    WHEN "A0" THEN
      ASSIGN L_width  = 2380
             L_height = 3368.
    WHEN "A1" THEN
      ASSIGN L_width  = 1684
             L_height = 2380.
    WHEN "A2" THEN
      ASSIGN L_width  = 1190
             L_height = 1684.
    WHEN "A3" THEN
      ASSIGN L_width  = 842
             L_height = 1190.
    WHEN "A4" THEN
      ASSIGN L_width  = 595
             L_height = 842.
    WHEN "A5" THEN
      ASSIGN L_width  = 421
             L_height = 595.
    WHEN "A6" THEN
      ASSIGN L_width  = 297
             L_height = 421.
    WHEN "B5" THEN
      ASSIGN L_width  = 501
             L_height = 709.
    WHEN "LETTER" THEN
      ASSIGN L_width  = 612
             L_height = 792.
    WHEN "LEGAL" THEN
      ASSIGN L_width  = 612
             L_height = 1008.
    WHEN "LEDGER" THEN
      ASSIGN L_width  = 1224
             L_height = 792.
    OTHERWISE
      ASSIGN L_width  = 612
             L_height = 792.
  END CASE.

  /* Now Set the Page Height and Width Parameters */
  IF pdf_Orientation(pdfStream) = "Portrait" THEN DO:
    RUN pdf_set_PageWidth(pdfStream,L_width).
    RUN pdf_set_PageHeight(pdfStream,L_height).
  END.
  ELSE DO:
    RUN pdf_set_PageWidth(pdfStream,L_height).
    RUN pdf_set_PageHeight(pdfStream,L_width).
  END.

END. /* pdf_set_PaperType */

FUNCTION pdf_PaperType RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "PaperType"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN RETURN TT_pdf_param.obj_value.
  ELSE RETURN "LETTER".

END. /* pdf_PaperType */

FUNCTION pdf_Render RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=YES &returnmsg=NO}.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "Render"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN RETURN INT(TT_pdf_param.obj_value).
  ELSE RETURN 0.

END. /* pdf_Render */

FUNCTION pdf_get_wrap_length RETURNS INTEGER ( INPUT pdfStream   AS CHARACTER,
                                               INPUT pdfText AS CHARACTER,
                                               INPUT pdfWidth AS INTEGER ):
  DEFINE VAR pdfHeight    AS INTEGER NO-UNDO.

  DEFINE VAR v-thisline   AS CHAR NO-UNDO.
  DEFINE VAR v-lastline   AS CHAR NO-UNDO.
  DEFINE VAR i            AS INTEGER NO-UNDO.
  DEFINE VAR v-pointsize  AS INTEGER NO-UNDO.
  DEFINE VAR v-maxwidth   AS INTEGER NO-UNDO.

  v-maxwidth = pdf_getnumfittingchars(pdfStream,
                                      FILL("W",pdfWidth),
                                      0,
                                      pdf_text_width(pdfstream,FILL("W",pdfWidth))).

  v-pointsize = PDF_VerticalSpace(pdfStream).
  IF v-PointSize = 0 THEN
    v-pointsize = pdf_PointSize(pdfStream).

  ASSIGN pdfText = REPLACE(pdfText,"|","&pipe;").
  /* Spaces */
  ASSIGN pdfText = REPLACE(pdfText," ","| ").
  /* Hyphens */
  ASSIGN pdfText = REPLACE(pdfText,"-","-|").
  /* Commas */
  ASSIGN pdfText = REPLACE(pdfText,",",",|").
  /* 13-JUL-2015 jcc: Line feed */
  /* ASSIGN pdfText = REPLACE(pdfText,CHR(10),"&skip;|"). */

  /* Divide up the pdf text into lines of width less than the
     available width */
  DO i = 1 TO NUM-ENTRIES(pdfText,"|"):
    ASSIGN v-lastline = v-thisline.
    ASSIGN v-thisline = v-thisline
                      + REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|").

    IF pdf_getnumfittingchars(pdfStream,
                              TRIM(v-thisline),
                              0,
                              pdf_text_width(pdfstream,TRIM(v-thisline))) > v-maxwidth
    THEN DO:
        ASSIGN pdfHeight = pdfHeight + v-pointsize.
        ASSIGN v-thisline = TRIM(REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|")).
    END.
  END.

  IF v-thisline NE "" THEN
    ASSIGN pdfHeight = pdfHeight + v-pointsize.

  RETURN pdfHeight.
END FUNCTION. /* pdf_get_wrap_length */

/* ---------------------- Define INTERNAL PROCEDURES ----------------------- */
PROCEDURE pdf_init_param:  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER NO-UNDO.

  /* Create Parameters */
  RUN pdf_set_Orientation(pdfStream,"Portrait").
  RUN pdf_set_PaperType(pdfStream,"LETTER").  /* This also default the PageWidth
                                             and PageHeight Parameters */
  RUN pdf_set_Font(pdfStream,"Courier",10.0). /* This also sets the PointSize */
  RUN pdf_set_LeftMargin(pdfStream,10).
  RUN pdf_set_RightMargin(pdfStream,10).
  RUN pdf_set_TopMargin(pdfStream,50).
  RUN pdf_set_BottomMargin(pdfStream,1).
  RUN pdf_set_Angle(pdfStream,0).
  RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).
  RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_GraphicY(pdfStream,0).
  RUN pdf_set_GraphicX(pdfStream,0).
  RUN pdf_text_color(pdfStream,.0,.0,.0).
  RUN pdf_stroke_fill (pdfStream,1.0,1.0,1.0).
  RUN pdf_set_PageRotate(pdfStream,0).    /* Default to zero rotation */

  RUN pdf_set_parameter(pdfStream,"PageMode","UseNone").
  RUN pdf_set_parameter(pdfStream,"PageLayout","SinglePage").

  RUN pdf_set_Scale (pdfStream, 1, 1).

  RUN pdf_set_parameter(pdfStream,"VERSION","{&xcPDFIncVersion}"). /* Do not change */

  RUN pdf_set_parameter(pdfStream,"pdfVersion","1.4"). /* 20-OCT-2011 jcc: by default generate PDF v1.4 */

END. /* pdf_init_param */

/* 11-FEB-2014 jcc: factorize the code */
PROCEDURE _pdf_Header_Item: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcItemName   AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plEncrypt    AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER piObjId      AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pmEncryptKey AS MEMPTR      NO-UNDO.

    DEFINE VARIABLE cItem AS CHARACTER   NO-UNDO.

    cItem = pdf_get_info(pdfStream, pcItemName).
    IF LENGTH(cItem, "character":u) > 0 THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED "/" pcItemName.
        RUN putString (pdfStream, "(", cItem, YES, YES, plEncrypt, piObjId, pmEncryptKey).
        PUT STREAM S_pdf_inc UNFORMATTED {&pdfSKIP}.
    END.
END.
/* 11-FEB-2014 jcc: end */

PROCEDURE pdf_Header : /* PRIVATE */
  DEFINE INPUT PARAMETER P_Stream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER p_Encrypt  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE cCodePage      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE l_CreationDate AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE m_EncryptKey   AS MEMPTR      NO-UNDO.

  /* Version Compatibilities */
  PUT STREAM S_pdf_inc UNFORMATTED
      "%PDF-" pdf_get_parameter(P_Stream, "pdfVersion") {&pdfSKIP}.

  /* Output 4 Binary Characters (greater than ASCII 128) to indicate to a binary
     file -- randomly selected codes */
  /* 08-MAR-2010 jcc: added cCodePage else it does not work with utf-8 sessions */
  cCodePage = IF SESSION:CPINTERNAL = 'utf-8' THEN 'iso8859-1' ELSE SESSION:CPINTERNAL.
  PUT STREAM S_pdf_inc UNFORMATTED
      "%" CHR(228, cCodePage, cCodePage) CHR(227, cCodePage, cCodePage) CHR(207, cCodePage, cCodePage) CHR(210, cCodePage, cCodePage) {&pdfSKIP}.

  l_CreationDate = "D:" + TRIM(STRING(YEAR(TODAY),"9999"))
                 + TRIM(STRING(MONTH(TODAY),"99"))
                 + TRIM(STRING(DAY(TODAY),"99"))
                 + REPLACE(STRING(TIME,"hh:mm:ss"),":","") + "+01'00". /* 19-FEB-2010 jcc: added ' to conform to pdf specification and changed -0800 (U.S. Pacific time) to +01'00 (France UTC time) */

  ObjectSequence( p_Stream, 1, "Info", ?, 0, "" ).

  /* Display Creation, Title, Producer etc Information */
  PUT STREAM S_pdf_inc UNFORMATTED
      "1 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}.

  RUN _pdf_Header_Item(P_Stream, "Author",   p_Encrypt, 1, m_EncryptKey).

  IF NOT glUnitTest THEN DO: /* 09-AUG-2013 jcc: as we compare the reference pdf with the current pdf when testing, we cannot write the creation date, else they always will be different */
      PUT STREAM S_pdf_inc UNFORMATTED "/CreationDate".
      RUN putString(P_Stream, "(", l_CreationDate, NO, YES, p_Encrypt, 1, m_EncryptKey).
      PUT STREAM S_pdf_inc UNFORMATTED {&pdfSKIP}.
  END.

  RUN _pdf_Header_Item(P_Stream, "Producer", p_Encrypt, 1, m_EncryptKey).
  RUN _pdf_Header_Item(P_Stream, "Creator",  p_Encrypt, 1, m_EncryptKey).
  RUN _pdf_Header_Item(P_Stream, "Subject",  p_Encrypt, 1, m_EncryptKey).
  RUN _pdf_Header_Item(P_Stream, "Title",    p_Encrypt, 1, m_EncryptKey).
  RUN _pdf_Header_Item(P_Stream, "Keywords", p_Encrypt, 1, m_EncryptKey).

  SET-SIZE(m_EncryptKey) = 0.

  PUT STREAM S_pdf_inc UNFORMATTED
      " >>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_header */

PROCEDURE pdf_LoadBase14: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Object          AS INTEGER NO-UNDO.

  L_Object = 5.

  /* ---- Beginning of Courier Fonts ---- */

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF1"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier-Oblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF2"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier-Bold"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF3"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier-BoldOblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF4"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* ---- End of Courier Fonts ---- */

  /* ---- Beginning of Helvetica Fonts ---- */

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF5"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "278 278 355 556 556 889 667 191 333 333 389 584 278 333 278 278 "
                                + "556 556 556 556 556 556 556 556 556 556 278 278 584 584 584 556 "
                                + "1015 667 667 722 722 667 611 778 722 278 500 667 556 833 722 778 "
                                + "667 778 722 667 611 722 667 944 667 667 611 278 278 278 469 556 "
                                + "333 556 556 500 556 556 278 556 556 222 222 500 222 833 556 556 "
                                + "556 556 333 500 278 556 500 722 500 500 500 334 260 334 584 350 "
                                + "556 333 222 556 333 1000 556 556 333 1000 667 333 1000 333 611 350 "
                                + "350 222 222 333 333 350 556 1000 333 1000 500 333 944 350 500 667 "
                                + "278 333 556 556 556 556 260 556 333 737 377 556 584 333 737 367 "
                                + "400 584 333 333 333 556 537 278 333 333 365 556 834 834 834 611 "
                                + "667 667 667 667 667 667 1000 722 667 667 667 667 278 278 278 278 "
                                + "722 722 778 778 778 778 778 584 778 722 722 722 722 667 667 611 "
                                + "556 556 556 556 556 556 889 500 556 556 556 556 278 278 278 278 "
                                + "556 556 556 556 556 556 556 584 611 556 556 556 556 500 556 500".

  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica-Oblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF6"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "278 278 355 556 556 889 667 191 333 333 389 584 278 333 278 278 "
                                + "556 556 556 556 556 556 556 556 556 556 278 278 584 584 584 556 "
                                + "1015 667 667 722 722 667 611 778 722 278 500 667 556 833 722 778 "
                                + "667 778 722 667 611 722 667 944 667 667 611 278 278 278 469 556 "
                                + "333 556 556 500 556 556 278 556 556 222 222 500 222 833 556 556 "
                                + "556 556 333 500 278 556 500 722 500 500 500 334 260 334 584 350 "
                                + "556 350 222 556 333 1000 556 556 333 1000 667 333 1000 350 611 350 "
                                + "350 222 222 333 333 350 556 1000 333 1000 500 333 944 350 500 667 "
                                + "278 333 556 556 556 556 260 556 333 737 377 556 584 333 737 372 "
                                + "400 584 333 333 333 556 537 278 333 333 365 556 834 834 834 611 "
                                + "667 667 667 667 667 667 1000 722 667 667 667 667 278 278 278 278 "
                                + "722 722 778 778 778 778 778 584 778 722 722 722 722 667 667 611 "
                                + "556 556 556 556 556 556 889 500 556 556 556 556 278 278 278 278 "
                                + "556 556 556 556 556 556 556 584 611 556 556 556 556 500 556 500".

  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica-Bold"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF7"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "278 333 474 556 556 889 722 238 333 333 389 584 278 333 278 278 "
                                + "556 556 556 556 556 556 556 556 556 556 333 333 584 584 584 611 "
                                + "975 722 722 722 722 667 611 778 722 278 556 722 611 833 722 778 "
                                + "667 778 722 667 611 722 667 944 667 667 611 333 278 333 584 556 "
                                + "333 556 611 556 611 556 333 611 611 278 278 556 278 889 611 611 "
                                + "611 611 389 556 333 611 556 778 556 556 500 389 280 389 584 350 "
                                + "556 350 278 556 500 1000 556 556 333 1000 667 333 1000 350 611 350 "
                                + "350 278 278 500 500 350 556 1000 333 1000 556 333 944 350 500 667 "
                                + "278 333 556 556 556 556 280 556 333 737 377 556 584 333 737 367 "
                                + "400 584 333 333 333 611 556 278 333 333 365 556 834 834 834 611 "
                                + "722 722 722 722 722 722 1000 722 667 667 667 667 278 278 278 278 "
                                + "722 722 778 778 778 778 778 584 778 722 722 722 722 667 667 611 "
                                + "556 556 556 556 556 556 889 556 556 556 556 556 278 278 278 278 "
                                + "611 611 611 611 611 611 611 584 611 611 611 611 611 556 611 556".

  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica-BoldOblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF8"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "278 333 474 556 556 889 722 238 333 333 389 584 278 333 278 278 "
                                + "556 556 556 556 556 556 556 556 556 556 333 333 584 584 584 611 "
                                + "975 722 722 722 722 667 611 778 722 278 556 722 611 833 722 778 "
                                + "667 778 722 667 611 722 667 944 667 667 611 333 278 333 584 556 "
                                + "333 556 611 556 611 556 333 611 611 278 278 556 278 889 611 611 "
                                + "611 611 389 556 333 611 556 778 556 556 500 389 280 389 584 350 "
                                + "556 350 278 556 500 1000 556 556 333 1000 667 333 1000 350 611 350 "
                                + "350 278 278 500 500 350 556 1000 333 1000 556 333 944 350 500 667 "
                                + "278 333 556 556 556 556 280 556 333 737 377 556 584 333 737 372 "
                                + "400 584 333 333 333 611 556 278 333 333 365 556 834 834 834 611 "
                                + "722 722 722 722 722 722 1000 722 667 667 667 667 278 278 278 278 "
                                + "722 722 778 778 778 778 778 584 778 722 722 722 722 667 667 611 "
                                + "556 556 556 556 556 556 889 556 556 556 556 556 278 278 278 278 "
                                + "611 611 611 611 611 611 611 584 611 611 611 611 611 556 611 556".

  L_Object = L_Object + 1.

  /* ---- End of Helvetica Fonts ---- */

  /* ---- Beginning of Times Roman Fonts ---- */

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-Roman"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF9"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "250 333 408 500 500 833 778 180 333 333 500 564 250 333 250 278 "
                                + "500 500 500 500 500 500 500 500 500 500 278 278 564 564 564 444 "
                                + "921 722 667 667 722 611 556 722 722 333 389 722 611 889 722 722 "
                                + "556 722 667 556 611 722 722 944 722 722 611 333 278 333 469 500 "
                                + "333 444 500 444 500 444 333 500 500 278 278 500 278 778 500 500 "
                                + "500 500 333 389 278 500 500 722 500 500 444 480 200 480 541 350 "
                                + "500 350 333 500 444 1000 500 500 333 1000 556 333 889 350 611 350 "
                                + "350 333 333 444 444 350 500 1000 333 980 389 333 722 350 444 722 "
                                + "250 333 500 500 500 500 200 500 333 760 278 500 564 333 760 355 "
                                + "400 564 300 300 333 500 453 250 333 300 310 500 750 750 750 444 "
                                + "722 722 722 722 722 722 889 667 611 611 611 611 333 333 333 333 "
                                + "722 722 722 722 722 722 722 564 722 722 722 722 722 722 556 500 "
                                + "444 444 444 444 444 444 667 444 444 444 444 444 278 278 278 278 "
                                + "500 500 500 500 500 500 500 564 500 500 500 500 500 500 500 500".

  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-Italic"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF10"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "250 333 420 500 500 833 778 214 333 333 500 675 250 333 250 278 "
                                + "500 500 500 500 500 500 500 500 500 500 333 333 675 675 675 500 "
                                + "920 611 611 667 722 611 611 722 722 333 444 667 556 833 667 722 "
                                + "611 722 611 500 556 722 611 833 611 556 556 389 278 389 422 500 "
                                + "333 500 500 444 500 444 278 500 500 278 278 444 278 722 500 500 "
                                + "500 500 389 389 278 500 444 667 444 444 389 400 275 400 541 350 "
                                + "500 350 333 500 556 889 500 500 333 1000 500 333 944 350 556 350 "
                                + "350 333 333 556 556 350 500 889 333 980 389 333 667 350 389 556 "
                                + "250 389 500 500 500 500 275 500 333 760 278 500 675 333 760 360 "
                                + "400 675 300 300 333 500 523 250 333 300 310 500 750 750 750 500 "
                                + "611 611 611 611 611 611 889 667 611 611 611 611 333 333 333 333 "
                                + "722 667 722 722 722 722 722 675 722 722 722 722 722 556 611 500 "
                                + "500 500 500 500 500 500 667 444 444 444 444 444 278 278 278 278 "
                                + "500 500 500 500 500 500 500 675 500 500 500 500 500 444 500 444".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-Bold"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF11"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "250 333 555 500 500 1000 833 278 333 333 500 570 250 333 250 278 "
                                + "500 500 500 500 500 500 500 500 500 500 333 333 570 570 570 500 "
                                + "930 722 667 722 722 667 611 778 778 389 500 778 667 944 722 778 "
                                + "611 778 722 556 667 722 722 1000 722 722 667 333 278 333 581 500 "
                                + "333 500 556 444 556 444 333 500 556 278 333 556 278 833 556 500 "
                                + "556 556 444 389 333 556 500 722 500 500 444 394 220 394 520 350 "
                                + "500 350 333 500 500 1000 500 500 333 1000 556 333 1000 350 667 350 "
                                + "350 333 333 500 500 350 500 1000 333 1000 389 333 722 350 444 722 "
                                + "250 333 500 500 500 500 220 500 333 747 300 500 570 333 747 360 "
                                + "400 570 300 300 333 556 540 250 333 300 330 500 750 750 750 500 "
                                + "722 722 722 722 722 722 1000 722 667 667 667 667 389 389 389 389 "
                                + "722 722 778 778 778 778 778 570 778 722 722 722 722 722 611 556 "
                                + "500 500 500 500 500 500 722 444 444 444 444 444 278 278 278 278 "
                                + "500 556 500 500 500 500 500 570 500 556 556 556 556 500 556 500".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-BoldItalic"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF12"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "250 389 555 500 500 833 778 278 333 333 500 570 250 333 250 278 "
                                + "500 500 500 500 500 500 500 500 500 500 333 333 570 570 570 500 "
                                + "832 667 667 667 722 667 667 722 778 389 500 667 611 889 722 722 "
                                + "611 722 667 556 611 722 667 889 667 611 611 333 278 333 570 500 "
                                + "333 500 500 444 500 444 333 500 556 278 278 500 278 778 556 500 "
                                + "500 500 389 389 278 556 444 667 500 444 389 348 220 348 570 350 "
                                + "500 350 333 500 500 1000 500 500 333 1000 556 333 944 350 611 350 "
                                + "350 333 333 500 500 350 500 1000 333 1000 389 333 722 350 389 611 "
                                + "250 389 500 500 500 500 220 500 333 747 278 500 606 333 747 370 "
                                + "400 570 300 300 333 576 500 250 333 300 300 500 750 750 750 500 "
                                + "667 667 667 667 667 667 944 667 667 667 667 667 389 389 389 389 "
                                + "722 722 722 722 722 722 722 570 722 722 722 722 722 611 611 500 "
                                + "500 500 500 500 500 500 722 444 444 444 444 444 278 278 278 278 "
                                + "500 556 500 500 500 500 500 570 500 556 556 556 556 444 500 444".

  /* ---- End of Times Roman Fonts ---- */

  
  
  /* ---- Beginning of Symbol Font ---- */

  /* Create Associated Object */
  L_Object = L_Object + 1.
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Symbol"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF13"
         TT_pdf_font.obj_stream = pdfStream
         /* TT_pdf_font.font_pitch = "FIXED" */
         /* TT_pdf_font.font_width = "600". */
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "250 333 713 500 549 833 778 439 333 333 500 549 250 549 250 278 "
                                + "500 500 500 500 500 500 500 500 500 500 278 278 549 549 549 444 "
                                + "549 722 667 722 612 611 763 603 722 333 631 722 686 889 722 722 "
                                + "768 741 556 592 611 690 439 768 645 795 611 333 863 333 658 500 "
                                + "500 631 549 549 494 439 521 411 603 329 603 549 549 576 521 549 "
                                + "549 521 549 603 439 576 713 686 493 686 494 480 200 480 549 788 "
                                + "788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 "
                                + "788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 "
                                + "750 620 247 549 167 713 500 753 753 753 753 1042 987 603 987 603 "
                                + "400 549 411 549 549 713 494 460 549 549 549 549 1000 603 1000 658 "
                                + "823 686 795 987 768 768 823 768 768 713 713 713 713 713 713 713 "
                                + "768 713 790 790 890 823 549 250 713 603 603 1042 987 603 987 603 "
                                + "494 329 790 790 786 713 384 384 384 384 384 384 494 494 494 494 "
                                + "788 329 274 686 686 686 384 384 384 384 384 384 494 494 494 788".
  
  /* ---- End of Symbol Font ---- */

  /* ---- Beginning of ZapfDingbats Font ---- */

  /* Create Associated Object */
  L_Object = L_Object + 1.
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "ZapfDingbats"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF14"
         TT_pdf_font.obj_stream = pdfStream
         /* TT_pdf_font.font_pitch = "FIXED" */
         /* TT_pdf_font.font_width = "600". */
         TT_pdf_font.font_pitch = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32)
                                + "278 974 961 974 980 719 789 790 791 690 960 939 549 855 911 933 "
                                + "911 945 974 755 846 762 761 571 677 763 760 759 754 494 552 537 "
                                + "577 692 786 788 788 790 793 794 816 823 789 841 823 833 816 831 "
                                + "923 744 723 749 790 792 695 776 768 792 759 707 708 682 701 826 "
                                + "815 789 789 707 687 696 689 786 787 713 791 785 791 873 761 762 "
                                + "762 759 759 892 892 788 784 438 138 277 415 392 392 668 668 788 "
                                + "390 390 317 317 276 276 509 509 410 410 234 234 334 334 788 788 "
                                + "788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 "
                                + "788 732 544 544 910 667 760 760 776 595 694 626 788 788 788 788 "
                                + "788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 "
                                + "788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 788 "
                                + "788 788 788 788 894 838 1016 458 748 924 748 918 927 928 928 834 "
                                + "873 828 924 924 917 930 931 463 883 836 836 867 867 696 696 874 "
                                + "788 874 760 946 771 865 771 888 967 888 831 873 927 970 918 788".

  /* ---- End of Symbol Font ---- */

  pdf_inc_ObjectSequence = L_Object.

END. /* pdf_LoadBase14 */

PROCEDURE pdf_Encoding: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iLastCharNum AS INTEGER INITIAL -1 NO-UNDO.

  ObjectSequence(pdfStream,4, "Encoding", ?, 0, "").
  PUT STREAM S_pdf_inc UNFORMATTED
      "4 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Encoding" {&pdfSKIP}
      "/BaseEncoding /WinAnsiEncoding" {&pdfSKIP}.
  /* 23-FEB-2010 jcc: based on an idea from Tomasz Judycki 10/2/2007
        - this is global change of encoding */
  IF CAN-FIND( FIRST TT_pdf_diff 
               WHERE TT_pdf_diff.obj_stream = pdfStream
                 AND TT_pdf_diff.font_name  = "Base14WinAnsiEncoding"
               NO-LOCK ) THEN DO:
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Differences [" {&pdfSKIP}.
    FOR EACH TT_pdf_diff
      WHERE TT_pdf_diff.obj_stream = pdfStream
        AND TT_pdf_diff.font_name  = "Base14WinAnsiEncoding"
        AND TT_pdf_diff.PS_name <> "~/.notdef"
        AND TT_pdf_diff.PS_name <> "~/" + Latin1Chars[TT_pdf_diff.char_num - 31]
        NO-LOCK:
      IF iLastCharNum + 1 <> TT_pdf_diff.char_num THEN DO:
          IF iLastCharNum <> -1 THEN
              PUT STREAM S_pdf_inc UNFORMATTED
                  {&pdfSKIP}.
          PUT STREAM S_pdf_inc UNFORMATTED
              TT_pdf_diff.char_num.
      END.
      PUT STREAM S_pdf_inc UNFORMATTED
          " " TT_pdf_diff.PS_name.
      iLastCharNum = TT_pdf_diff.char_num.
    END.
    PUT STREAM S_pdf_inc UNFORMATTED
        {&pdfSKIP} "]" {&pdfSKIP}.
  END.
  PUT STREAM S_pdf_inc UNFORMATTED
  /* 23-FEB-2010 jcc: end */
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Encoding */

PROCEDURE pdf_Resources: /* PRIVATE */
  DEFINE INPUT  PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT PARAMETER pdf-Res-Object AS INTEGER     NO-UNDO.

  DEFINE VARIABLE cFontList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_fontobj AS CHARACTER   NO-UNDO.

  DEFINE BUFFER TT_pdf_font         FOR TT_pdf_font.
  DEFINE BUFFER TT_pdf_image        FOR TT_pdf_image.
  DEFINE BUFFER TT_pdf_xobject      FOR TT_pdf_xobject.
  DEFINE BUFFER TT_pdf_external     FOR TT_pdf_external.
  DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.

  FOR EACH TT_pdf_font NO-LOCK
     WHERE TT_pdf_font.obj_stream = pdfStream 
       AND TT_pdf_font.font_pitch <> "EXTERNAL" 
       AND TT_pdf_font.font_obj <> 0 
       AND TT_pdf_font.used_flag /* 22-FEB-2010 jcc: added */
      BY TT_pdf_font.font_tag: /* 14-OCT-2013 jcc: sort by tag so that unit tests do not fail */
      IF LOOKUP(TT_pdf_font.font_tag, cFontList) = 0 THEN
          ASSIGN
              L_fontobj = L_fontobj + " " + TT_pdf_font.font_tag + " "
                          + STRING(TT_pdf_font.font_obj) + " 0 R"
              cFontList = cFontList + "," + TT_pdf_font.font_tag.
  END.

  ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Resource", ?, 0, "").
  pdf-Res-Object = pdf_inc_ObjectSequence.
  PUT STREAM S_pdf_inc UNFORMATTED
      pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Font << " L_fontobj " >>" {&pdfSKIP}
      "/ProcSet [ /PDF /Text /ImageB /ImageC /ImageI ]" {&pdfSKIP} /* /Procset is obsolete, starting with PDF 1.4 */
      "/XObject << ".

  /* Output Image Definitions */
  FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream
      AND TT_pdf_image.used_flag = TRUE:
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_image.image_tag " " TT_pdf_image.image_obj " 0 R".
  END.

  /* 03-MAY-2014 jcc: Output XObjects */
  FOR EACH TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream
      AND TT_pdf_xobject.used_flag = TRUE: 
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_xobject.xobject_tag " " TT_pdf_xobject.xobject_obj " 0 R".
  END.

  /* Output External Pages */
  FOR EACH TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStream,
      /* 03-DEC-2014 jcc: only export used external pages: */
    FIRST tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
      AND tt_pdf_page_use_ext.pdf_id_use = TT_pdf_external.pdf_id: /* 02-DEC-2014 jcc */
        /* AND tt_pdf_page.page_nbr   = tt_pdf_external.page_id: */
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_external.ext_tag " " TT_pdf_external.ext_obj " 0 R".
  END.

  PUT STREAM S_pdf_inc UNFORMATTED
      " >>" {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Resources */

/* 16-OCT-2014 jcc: refactor code */
PROCEDURE pdf_Form_Widgets_Content: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piTotalPages AS INTEGER     NO-UNDO.
    DEFINE PARAMETER BUFFER TT_pdf_page FOR TT_pdf_page.

    DEFINE BUFFER TT_Widget           FOR TT_Widget.
    DEFINE BUFFER TT_pdf_FillTxt      FOR TT_pdf_FillTxt.
    DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.

    /* IF pdf_get_parameter(pdfStream, "retainAcroForm") <> "TRUE" THEN DO: /* 24-AUG-2012 jcc: if we keep the form then we do not want this, as the widgets themselves will be filled */ */
    IF TT_pdf_page.widget_flatten > "" THEN DO: /* 24-AUG-2012 jcc: if we keep the form then we do not want this, as the widgets themselves will be filled */
        /* 24-MAY-2013 jcc: if the widget had a value, keep it when flattening the form
                            (widget names from which to keep the default value defined in the parameter formFlattenWithDefaultValues) */
        IF pdf_get_parameter(pdfStream, "formFlattenWithDefaultValues") > "" THEN
            FOR EACH tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
                 AND tt_pdf_page_use_ext.page_nbr = TT_pdf_page.page_nbr,
                EACH TT_Widget
                WHERE TT_Widget.obj_stream     = pdfStream
                  AND TT_Widget.pdf_id         = tt_pdf_page_use_ext.pdf_id_use
                  AND TT_Widget.widget_page    = tt_pdf_page_use_ext.page_use
                  AND TT_Widget.widget_value   > ""
                  AND LOOKUP(TT_Widget.widget_name, TT_pdf_page.widget_flatten, CHR(1)) > 0:
                IF NOT CAN-DO(pdf_get_parameter(pdfStream, "formFlattenWithDefaultValues"), TT_Widget.widget_name) THEN
                    NEXT.
                IF NOT CAN-FIND(TT_pdf_FillTxt WHERE TT_pdf_FillTxt.obj_Stream = pdfStream
                                                 AND TT_pdf_FillTxt.page_nbr   = TT_pdf_page.page_nbr
                                                 AND TT_pdf_FillTxt.fill_from  = TT_Widget.widget_name) THEN DO:
                    CREATE TT_pdf_FillTxt.
                    ASSIGN TT_pdf_FillTxt.obj_stream   = pdfStream.
                    ASSIGN TT_pdf_FillTxt.page_nbr     = TT_pdf_page.page_nbr
                           TT_pdf_FillTxt.fill_from    = TT_Widget.widget_name
                           TT_pdf_FillTxt.fill_to      = IF ENTRY(1, TT_Widget.widget_type) <> "~/Ch" THEN TT_Widget.widget_value ELSE STRING(LOOKUP(TT_Widget.widget_value,TT_Widget.widget_values,CHR(10)))
                           TT_pdf_FillTxt.fill_options = "".
                END.
            END. /* FOR EACH TT_Widget */
        /* 24-AUG-2012 jcc: for list boxes, ensure they have a TT_pdf_FillTxt, so that the possible values will be shown even if none is selected */
        FOR EACH tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
             AND tt_pdf_page_use_ext.page_nbr = TT_pdf_page.page_nbr,
            EACH TT_Widget
            WHERE TT_Widget.obj_stream     = pdfStream
              AND TT_Widget.pdf_id         = tt_pdf_page_use_ext.pdf_id_use
              AND TT_Widget.widget_page    = tt_pdf_page_use_ext.page_use
              AND TT_Widget.widget_type    = "~/Ch,list"
              AND LOOKUP(TT_Widget.widget_name, TT_pdf_page.widget_flatten, CHR(1)) > 0:
            IF NOT CAN-FIND(TT_pdf_FillTxt WHERE TT_pdf_FillTxt.obj_Stream = pdfStream
                                             AND TT_pdf_FillTxt.page_nbr   = TT_pdf_page.page_nbr
                                             AND TT_pdf_FillTxt.fill_from  = TT_Widget.widget_name) THEN DO:
                CREATE TT_pdf_FillTxt.
                ASSIGN TT_pdf_FillTxt.obj_stream   = pdfStream.
                ASSIGN TT_pdf_FillTxt.page_nbr     = TT_pdf_page.page_nbr
                       TT_pdf_FillTxt.fill_from    = TT_Widget.widget_name
                       TT_pdf_FillTxt.fill_to      = ""
                       TT_pdf_FillTxt.fill_options = "".
            END.
        END.
        /* Write down the widgets */
        IF CAN-FIND(FIRST TT_pdf_FillTxt
                WHERE TT_pdf_FillTxt.obj_Stream = pdfStream
                  AND TT_pdf_FillTxt.page_nbr   = TT_pdf_page.page_nbr NO-LOCK) THEN DO:
          EMPTY TEMP-TABLE tt_state_op. /* 30-AUG-2012 jcc: same as in pdf_new_page2 */
          FOR EACH tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
               AND tt_pdf_page_use_ext.page_nbr = TT_pdf_page.page_nbr:
              RUN ProcessFillText
                  (pdfStream,
                   TT_pdf_page.page_nbr,
                   piTotalPages,
                   tt_pdf_page_use_ext.pdf_id_use,
                   tt_pdf_page_use_ext.page_use,
                   TT_pdf_page.widget_flatten).
          END.
        END.
    END. /* IF TT_pdf_page.widget_flatten > "" */
END PROCEDURE. /* pdf_Form_Widgets_Content */
/* 16-OCT-2014 jcc: end */

PROCEDURE pdf_Content: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdf-Res-Object AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt     AS LOGICAL     NO-UNDO.

  DEFINE BUFFER TT_pdf_Stream FOR TT_pdf_Stream.

  DEFINE VARIABLE L_Loop           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_Page           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE pdf_OutlinesDict AS INTEGER     NO-UNDO.

  PUBLISH "MaxPDFPage" (INPUT pdfStream, INPUT pdf_Page(pdfStream)).

  FIND FIRST TT_pdf_stream WHERE TT_pdf_Stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAILABLE TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* Produce each Page one at a time */
  DO L_Loop = 1 TO pdf_Page(pdfStream):
    L_page = L_page + 1.

    PUBLISH "BuildPDFPage" (INPUT pdfStream, INPUT L_Page).

    FIND FIRST TT_pdf_page 
         WHERE TT_pdf_page.obj_stream = pdfStream
           AND TT_pdf_page.page_nbr = L_Page NO-LOCK NO-ERROR.
    /* 23-APR-2014 jcc: modified ChangePageText in order to perform all the replaces in the same call */
    IF TT_pdf_page.UseTotalPages OR TT_pdf_page.UsePageNo THEN
      RUN ChangePageText
          (pdfStream,
           _get_page_content_file(pdfStream, TT_pdf_Stream.obj_UniqueID, L_Page),
           TT_pdf_page.UsePageNo OR TT_pdf_page.UseTotalPages,
           (IF TT_pdf_page.UseTotalPages THEN "@@TotalPages-" + pdfStream + CHR(2) ELSE "") +
            (IF TT_pdf_page.UsePageNo THEN "@@PageNo-" + pdfStream /*+ CHR(2) */ ELSE ""),
           (IF TT_pdf_page.UseTotalPages THEN STRING(pdf_Page(pdfStream)) + CHR(2) ELSE "") +
            (IF TT_pdf_page.UsePageNo THEN STRING(L_Page) /*+ CHR(2) */ ELSE "")).

    /* 16-OCT-2014 jcc: moved at a better place in pdf_close. See comment there. */
    /* RUN pdf_Form_Widgets_Content (pdfStream, BUFFER TT_pdf_page). */

    /* Start Page Definition */
    RUN pdf_Definition (pdfStream , pdf-Res-Object, L_page, IF L_page = 1 THEN FALSE ELSE TRUE, pdfEncrypt).

    /* PDFinclude Content Stream */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Content - PDFinclude", ?, 0, "").

    /* 15-OCT-2015 jcc: all the code below can be replaced by this */
    RUN putFileAsStream (pdfStream, _get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, L_Page), pdf_inc_ObjectSequence, "", "", pdfEncrypt, YES, YES).

    /*
    PUT STREAM S_pdf_inc UNFORMATTED
        pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}.
        "/Length " (pdf_inc_ObjectSequence + 1) " 0 R" {&pdfSKIP}.


    /* Only add the Filter if 'Compress' is turned on */
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Filter /FlateDecode" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP} .

    pdf-Stream-Start = SEEK(S_pdf_inc).

    SET-SIZE(mContent) = 0.
    RUN getFileAsMemptr(pdfStream, _get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, L_Page), ?,
                        INPUT-OUTPUT mContent, OUTPUT iSize).
    mHolder = mContent.

    /** Compression happens before Encryption **/
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
      L_Stat = compressbuffer( mHolder,
                               INPUT-OUTPUT vDest,
                               OUTPUT vDestLen).
      IF L_Stat = 0 THEN DO:
          SET-SIZE(mHolder) = 0.
          SET-SIZE(mHolder) = GET-SIZE(vDest).
          mHolder = Vdest.
      END.
      ELSE
          {pdferror.i &msg="'Zlib error: ' + STRING(L_Stat)" &return=NO}. /* continue without compression */
    END.
      

    RUN OutputMemPtr(pdfStream, 
                     FALSE, 
                     TT_pdf_stream.obj_UniqueID, 
                     pdf_inc_ObjectSequence, 
                     mHolder,
                     L_EncryptKey).

    pdf-Stream-End = SEEK(S_pdf_inc).

    IF GET-SIZE(L_EncryptKey) > 0 THEN /* 13-OCT-2015 jcc: ensure a CR precedes endstream */
        PUT STREAM S_pdf_inc UNFORMATTED {&pdfSKIP}.

    SET-SIZE(L_EncryptKey) = 0.
    SET-SIZE(mHolder) = 0.
    SET-SIZE(mContent) = 0.
    /* SET-SIZE(vSource) = 0. */
    SET-SIZE(vDest)   = 0.

    /* End Page Definition */
    PUT STREAM S_pdf_inc UNFORMATTED
        "endstream" {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    /* Output Length */
    RUN pdf_length (pdfStream, pdf-Stream-End - pdf-Stream-Start).
    */
  END. /* loop for each page */

  OS-DELETE VALUE(SESSION:TEMP-DIR + TT_pdf_stream.obj_UniqueID + "-C.txt").

  /* SET-SIZE(mHolder) = 0. */
  /* SET-SIZE(mContent) = 0. */
  /* SET-SIZE(vSource) = 0. */
  /* SET-SIZE(vDest)   = 0. */

  /* Load Bookmarks */
  IF CAN-FIND( FIRST TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_Stream = pdfStream NO-LOCK)
  THEN RUN pdf_Load_Bookmarks (pdfStream, OUTPUT pdf_OutlinesDict).

  ASSIGN TT_pdf_Stream.obj_DoingText    = FALSE
         TT_pdf_Stream.obj_DoingGraphic = FALSE.

  /* This will set the PDF Page to the max actual Page number */
  RUN pdf_set_Page(pdfStream, L_page).

  RUN pdf_catalog (pdfStream, pdf_OutlinesDict, pdfEncrypt).
  RUN pdf_ListPages (pdfStream).

END. /* pdf_Content */

PROCEDURE pdf_definition: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdf-Res-Object AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER P_page         AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER P_incl-annot   AS LOGICAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt     AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE cAnnots    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE curPageObj AS INTEGER     NO-UNDO.

  DEFINE BUFFER bTT_Resource        FOR TT_Resource.
  DEFINE BUFFER TT_pdf_page         FOR TT_pdf_page.
  DEFINE BUFFER TT_pdf_object       FOR TT_pdf_object.
  DEFINE BUFFER TT_pdf_annot        FOR TT_pdf_annot.
  DEFINE BUFFER TT_Object           FOR TT_Object.
  DEFINE BUFFER TT_Resource         FOR TT_Resource.
  DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.


  FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                     AND TT_pdf_page.page_nbr   = p_Page NO-ERROR.
  IF NOT AVAIL TT_pdf_page THEN DO:
    MESSAGE "Error occurred - could not find TT_pdf_page for Page=" p_Page
            VIEW-AS ALERT-BOX.
    RETURN.
  END.

  curPageObj = pdf_inc_ObjectSequence + 1.

  /* 14-SEP-2012 jcc: output deferred reference objects */
  FOR EACH tt_pdf_object WHERE tt_pdf_object.obj_stream = pdfStream
      AND tt_pdf_object.obj_page = P_page
      AND tt_pdf_object.obj_desc = "Deferred-Page" :
      setObjectOffset(pdfStream, tt_pdf_object.obj_nbr, SEEK(S_pdf_inc) + 1).
      PUT STREAM S_pdf_inc UNFORMATTED
          tt_pdf_object.obj_nbr " 0 obj" {&pdfSKIP}
          REPLACE(tt_pdf_object.obj_extra, "@PAGE-OBJ@", STRING(curPageObj)) {&pdfSKIP}
          "endobj" {&pdfSKIP}.
  END.

  ObjectSequence(pdfStream, curPageObj, "PageDefinition", ?, p_Page, "").
  PUT STREAM S_pdf_inc UNFORMATTED
      curPageObj " 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Page" {&pdfSKIP}
      "/Parent 3 0 R" {&pdfSKIP}
      "/Resources " pdf-Res-Object " 0 R" {&pdfSKIP}.

  PUT STREAM S_pdf_inc UNFORMATTED
      "/Rotate " TT_pdf_page.page_rotate {&pdfSKIP}.
  /* 16-JAN-2011 jcc: for some unknown reason, was hardcoded to zero 
     (as per Phil Freed 2010-12-28) */
   /* "/Rotate 0" /* STRING(TT_pdf_page.page_rotate) */ {&pdfSKIP}. */

  /* 05-DEC-2014 jcc: why swap the sizes when the external file is rotated? */
  /* IF TT_pdf_page.page_use <> 0 THEN DO: */
    /* FIND FIRST TT_pdf_external */
         /* WHERE TT_pdf_external.obj_stream = pdfStream */
           /* AND TT_pdf_external.pdf_id     = TT_pdf_page.pdf_id_use /* 02-DEC-2014 jcc */ */
           /* AND TT_pdf_external.ext_page   = tt_pdf_page.page_use NO-ERROR. /* 02-DEC-2014 jcc: page_id -> ext_page !!! */ */
    /* IF AVAILABLE TT_pdf_External AND (TT_pdf_External.ext_rotate = 90 OR TT_pdf_external.ext_rotate = 270) THEN /* 03-NOV-2014 jcc: 270 */ */
      /* PUT STREAM S_pdf_inc UNFORMATTED */
          /* "/MediaBox[0 0 "  dec2string(TT_pdf_Page.page_height) " " dec2string(TT_pdf_page.page_width) */
          /* "]" {&pdfSKIP}. */
    /* ELSE */
      /* PUT STREAM S_pdf_inc UNFORMATTED */
          /* "/MediaBox[0 0 "  dec2string(TT_pdf_Page.page_width) " " dec2string(TT_pdf_page.page_height) */
          /* "]" {&pdfSKIP}. */
  /* END. */
  /* ELSE /* TT_pdf_page.page_use = 0 */ */
    PUT STREAM S_pdf_inc UNFORMATTED
        "/MediaBox[0 0 "  dec2string(TT_pdf_Page.page_width) " " dec2string(TT_pdf_page.page_height)
        "]" {&pdfSKIP}.

  IF TT_pdf_page.page_crop <> "" THEN
    PUT STREAM S_pdf_inc UNFORMATTED
        "/CropBox[" + TT_pdf_page.page_crop + "]" {&pdfSKIP}.

  /* 10-NOV-2011 jcc: only output /Annots if there are some (add the following IF) */
  /* IF CAN-FIND(FIRST TT_pdf_annot WHERE TT_pdf_annot.obj_stream = pdfStream */
                                   /* AND TT_pdf_annot.annot_page = p_Page) */
      /* 24-AUG-2012 jcc: /Annots are also for form fields, so: */
      /* OR ( (   pdf_get_parameter(pdfStream, "retainAcroForm") = "TRUE" */
      /* OR ( (   TT_pdf_page.widget_retain > "" */
            /* OR pdf_get_parameter(pdfStream, "retainAnnots") > "" ) */
           /* AND CAN-FIND(FIRST TT_Resource WHERE TT_Resource.obj_stream = pdfStream */
                                            /* AND TT_Resource.pdf_id     = TT_pdf_page.pdf_id_use /* 02-DEC-2014 jcc */ */
                                            /* AND TT_Resource.page_id    = TT_pdf_page.page_use */
                                            /* AND TT_Resource.res_type   = "Annot") ) THEN DO: */
      /* Output Link Definitions */
      FOR EACH TT_pdf_annot WHERE TT_pdf_annot.obj_stream = pdfStream
                              AND TT_pdf_annot.annot_page  = p_Page:
        /* PUT STREAM S_pdf_inc UNFORMATTED */
            /* TT_pdf_annot.annot_obj " 0 R ". */
            cAnnots = cAnnots + " " + STRING(TT_pdf_annot.annot_obj) + " 0 R".
      END.

      /* 23-AUG-2012 jcc: external annotations (AcroForm) */
      /* IF pdf_get_parameter(pdfStream, "retainAcroForm") = "TRUE" */
      IF   TT_pdf_page.widget_retain > "" 
        OR pdf_get_parameter(pdfStream, "retainAnnots") > "" THEN
      FOR EACH tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
           AND tt_pdf_page_use_ext.page_nbr = TT_pdf_page.page_nbr,
          EACH TT_Resource WHERE TT_Resource.obj_stream = pdfStream
                             AND TT_Resource.pdf_id     = tt_pdf_page_use_ext.pdf_id_use /* 02-DEC-2014 jcc */
                             AND TT_Resource.page_id    = tt_pdf_page_use_ext.page_use
                             AND TT_Resource.res_type   = "Annot",
          FIRST TT_Object WHERE TT_Object.obj_stream = pdfStream
                            AND TT_Object.pdf_id     = TT_Resource.pdf_id
                            AND TT_Object.obj_id     = TT_Resource.res_obj
                            AND TT_Object.gen_id     = TT_Resource.res_gen:
          
          /* 26-MAY-2013 jcc: skip flattened form widgets */
          IF CAN-FIND(bTT_Resource WHERE bTT_Resource.obj_stream = pdfStream
                              AND bTT_Resource.res_type BEGINS "FormWidget"
                              AND bTT_Resource.res_obj = TT_Resource.res_obj
                              AND bTT_Resource.res_gen = TT_Resource.res_gen
                              AND LOOKUP(bTT_Resource.res_text, TT_pdf_page.widget_retain, CHR(1)) = 0) THEN
              NEXT.

          /* IF TT_Object.obj_id <> 0 AND TT_Object.new_obj <= 0 THEN */
          RUN recursivelyAssignNewObjs (pdfStream
                                       ,BUFFER TT_Object
                                       ,TT_Object.obj_value_type
                                       ,IF TT_Object.obj_value_type = "ARRAY" THEN TT_Object.obj_array_id ELSE IF TT_Object.obj_value_type = "DICT" THEN TT_Object.obj_dict_id ELSE -1
                                       ,"Annot"
                                       ,"_callBackAcroForm"
                                       ,STRING(curPageObj) + "," + STRING(P_page) /* argument for the callback */ ).
          /* TT_Resource.new_obj = TT_Object.new_obj. */

          /* PUT STREAM S_pdf_inc UNFORMATTED */
              /* TT_Object.new_obj " 0 R ". */
              cAnnots = cAnnots + " " + STRING(TT_Object.new_obj) + " 0 R".
      END.

      IF cAnnots > "" THEN
          PUT STREAM S_pdf_inc UNFORMATTED
              "/Annots[" SUBSTRING(cAnnots, 2) "]" {&pdfSKIP}.
  /* END. */

  /* 27-AUG-2012 jcc: move /Contents here, as we need to know the new obj, after exporting all the annotations */
  PUT STREAM S_pdf_inc UNFORMATTED
      "/Contents " (pdf_inc_ObjectSequence + 1) " 0 R" {&pdfSKIP}.

  PUT STREAM S_pdf_inc UNFORMATTED
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

  /* Export only the fields we really used (in case of a form spanning on many pages,
     we have to check which page has been used through TT_pdf_page.page_use) */
  /* IF   pdf_get_parameter(pdfStream, "retainAcroForm") = "TRUE" */
  IF   TT_pdf_page.widget_retain > ""
    OR pdf_get_parameter(pdfStream, "retainAnnots") > "" THEN
  FOR EACH tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
       AND tt_pdf_page_use_ext.page_nbr = TT_pdf_page.page_nbr,
      EACH TT_Resource WHERE TT_Resource.obj_stream = pdfStream
                         AND TT_Resource.pdf_id     = tt_pdf_page_use_ext.pdf_id_use /* 02-DEC-2014 jcc */
                         AND TT_Resource.page_id    = tt_pdf_page_use_ext.page_use
                         AND TT_Resource.res_type   = "Annot",
      FIRST TT_Object  WHERE TT_Object.obj_stream = pdfStream
                         AND TT_Object.pdf_id     = TT_Resource.pdf_id
                         AND TT_Object.obj_id     = TT_Resource.res_obj
                         AND TT_Object.gen_id     = TT_Resource.res_gen.

      /* 26-MAY-2013 jcc: skip flattened form widgets */
      IF CAN-FIND(bTT_Resource WHERE bTT_Resource.obj_stream = pdfStream
                          AND bTT_Resource.res_type BEGINS "FormWidget"
                          AND bTT_Resource.res_obj = TT_Resource.res_obj
                          AND bTT_Resource.res_gen = TT_Resource.res_gen
                          AND LOOKUP(bTT_Resource.res_text, TT_pdf_page.widget_retain, CHR(1)) = 0) THEN
          NEXT.

      RUN recursivelyExportObject(pdfStream, BUFFER TT_Object, "Annot", pdfEncrypt).
      /* TT_Resource.new_obj = TT_Object.new_obj. */
  END.

END. /* pdf_definition */

/* 24-AUG-2012 jcc: call back for recursivelyAssignNewObjs in order to change the /P (page number) and /V (value) */
PROCEDURE _callBackAcroForm: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream       AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcValueType     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piDictOrArrayId AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcParams        AS CHARACTER   NO-UNDO. /* we will receive a list (page obj,page nbr) in the parameters */

    DEFINE BUFFER TT_dict             FOR TT_dict.
    DEFINE BUFFER bTT_dict            FOR TT_dict.
    DEFINE BUFFER bbTT_dict           FOR TT_dict.
    DEFINE BUFFER bbbTT_dict          FOR TT_dict.
    DEFINE BUFFER bbbbTT_dict         FOR TT_dict.
    DEFINE BUFFER TT_array            FOR TT_array.
    DEFINE BUFFER TT_pdf_FillTxt      FOR TT_pdf_FillTxt.
    DEFINE BUFFER TT_Object           FOR TT_Object.
    DEFINE BUFFER TT_pdf_page         FOR TT_pdf_page.
    DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.

    DEFINE VARIABLE cValue   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE obj-name AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE obj-type AS CHARACTER   NO-UNDO.

    IF pcValueType <> "DICT" THEN RETURN.

    IF NUM-ENTRIES(pcParams) > 3 THEN ASSIGN
        obj-type = ENTRY(3, pcParams)
        obj-name = ENTRY(4, pcParams).
    ELSE DO:
        /* filter on the fields */
        {getDict TT_dict piDictOrArrayId "SCALAR" "~/FT"}.
        IF AVAILABLE TT_dict THEN DO:
            obj-type = TT_dict.value_scalar.
            /* get field name */
            {getDict TT_dict piDictOrArrayId "SCALAR" "~/T"}.
            IF AVAILABLE TT_dict THEN
                obj-name = parseText(TT_dict.value_scalar).
        END.
    END.

    IF obj-type > "" THEN DO:
        /* propagate to kids */
        {getDict TT_dict piDictOrArrayId "ARRAY" "~/Kids"}.
        IF AVAILABLE TT_dict THEN DO:
            FOR EACH TT_array WHERE TT_array.array_id = TT_dict.value_array_id:
                RUN _callBackAcroForm (pdfStream, "DICT", TT_array.value_dict_id, pcParams + "," + obj-type + "," + obj-name).
            END.
            /* RETURN. */ /* 28-MAY-2013 jcc: commented else /P was not set! */
        END.

        /* Change /P (page) */
        {getDict TT_dict piDictOrArrayId "DICT" "~/P"}.
        IF AVAILABLE TT_dict THEN ASSIGN
            TT_dict.value_type = "SCALAR"
            TT_dict.value_scalar = ENTRY(1,pcParams) + " 0 R".

        /* Fill the widgets with the values defined calling pdf_fill_text */
        IF obj-name > "" THEN DO:
            FIND TT_pdf_FillTxt
                WHERE TT_pdf_FillTxt.obj_stream = pdfStream
                  AND TT_pdf_FillTxt.page_nbr   = INTEGER(ENTRY(2, pcParams))
                  AND TT_pdf_FillTxt.fill_from  = obj-name NO-ERROR.
            IF AVAILABLE TT_pdf_FillTxt THEN DO:
                {getDict TT_dict piDictOrArrayId "~/V"}.
                IF NOT AVAILABLE TT_dict THEN DO:
                    CREATE TT_dict.
                    ASSIGN
                     TT_dict.dict_id        = piDictOrArrayId
                     TT_dict.dict_key       = "~/V"
                     TT_dict.value_type     = "SCALAR"
                     TT_dict.value_array_id = -1
                     TT_dict.value_dict_id  = -1.
                END.
                IF obj-type = "~/Tx" THEN
                    TT_dict.value_scalar = "(" + REPLACE(REPLACE(TT_pdf_FillTxt.fill_to, "(", "~\("), ")", "~\)") + ")".
                ELSE IF obj-type = "~/Btn" THEN DO:
                    TT_dict.value_scalar = IF TT_pdf_FillTxt.fill_to = "NO" THEN "~/Off" ELSE IF TT_pdf_FillTxt.fill_to = "YES" THEN "~/Yes" ELSE ("~/" + TT_pdf_FillTxt.fill_to).
                    /* also update /AS */
                    {getDict bTT_dict piDictOrArrayId "ARRAY" "~/Kids"}.
                    IF AVAILABLE bTT_dict THEN DO: /* radio */
                        FOR EACH TT_array WHERE TT_array.array_id = bTT_dict.value_array_id:
                            {getDict bbTT_dict TT_array.value_dict_id "DICT" "~/AP"}.
                            IF AVAILABLE bbTT_dict THEN DO:
                                {getDict bbbTT_dict bbTT_dict.value_dict_id "DICT" "~/N"}.
                                IF AVAILABLE bbbTT_dict THEN DO:
                                    IF NOT CAN-FIND(bbbbTT_dict WHERE bbbbTT_dict.dict_id = bbbTT_dict.value_dict_id
                                                                 AND bbbbTT_dict.dict_key = TT_dict.value_scalar) THEN
                                        cValue = "~/Off".
                                    ELSE
                                        cValue = TT_dict.value_scalar.
                                END.
                            END.
                            {getDict bbTT_dict TT_array.value_dict_id "SCALAR" "~/AS"}.
                            IF AVAILABLE bbTT_dict THEN bbTT_dict.value_scalar = cValue.
                        END.
                    END.
                    ELSE DO: /* checkbox */
                        {getDict bTT_dict piDictOrArrayId "SCALAR" "~/AS"}.
                        IF AVAILABLE bTT_dict THEN bTT_dict.value_scalar = TT_dict.value_scalar.
                    END.
                END.
                ELSE IF obj-type = "~/Ch" THEN DO:
                    {getDict bTT_dict piDictOrArrayId "SCALAR" "~/Ff"}.
                    /* combo */
                    IF LOGICAL(GET-BITS(INTEGER(bTT_dict.value_scalar), 18, 1)) THEN DO:
                        IF GetWidgetOption("text", TT_pdf_fillTxt.fill_options) = "YES" THEN
                            TT_dict.value_scalar = "(" + TT_pdf_FillTxt.fill_to + ")".
                        ELSE DO:
                            {getDict bTT_dict piDictOrArrayId "ARRAY" "~/Opt"}.
                            IF AVAILABLE bTT_dict THEN DO:
                                FIND TT_array WHERE TT_array.array_id = bTT_dict.value_array_id
                                                AND TT_array.value_id = INTEGER(TT_pdf_FillTxt.fill_to) NO-ERROR.
                                IF AVAILABLE TT_array THEN
                                    TT_dict.value_scalar = TT_array.value_scalar.
                            END.
                        END.
                    END.
                    /* list box */
                    ELSE DO:
                        cValue = "[".
                        {getDict bTT_dict piDictOrArrayId "ARRAY" "~/Opt"}.
                        IF AVAILABLE bTT_dict THEN DO i = 1 TO NUM-ENTRIES(TT_pdf_FillTxt.fill_to):
                            FIND TT_array WHERE TT_array.array_id = bTT_dict.value_array_id
                                            AND TT_array.value_id = INTEGER(ENTRY(i,TT_pdf_FillTxt.fill_to)) NO-ERROR.
                            IF AVAILABLE TT_array THEN
                                cValue = cValue + TT_array.value_scalar.
                        END.
                        TT_dict.value_scalar = cValue + "]".
                        TT_dict.value_type   = "SCALAR".
                    END.
                END.
            END.
        END.

        /* 26-OCT-2016 jcc: handle rotated templates */
        {getDict TT_dict piDictOrArrayId "DICT" "~/MK"}.
        IF AVAILABLE TT_dict THEN blkMK: DO:
            FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.page_nbr = INTEGER(ENTRY(2, pcParams)) NO-ERROR.
            IF NOT AVAILABLE TT_pdf_page THEN LEAVE blkMK.
            FIND tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
                AND tt_pdf_page_use_ext.page_nbr = TT_pdf_page.page_nbr NO-ERROR.
            IF NOT AVAILABLE tt_pdf_page_use_ext THEN LEAVE blkMK.
            FIND FIRST TT_Object WHERE TT_Object.obj_stream = pdfStream
                AND TT_Object.pdf_ID     = tt_pdf_page_use_ext.pdf_id_use
                AND TT_Object.obj_type   = "Page"
                AND TT_Object.page_id    = tt_pdf_page_use_ext.page_use NO-ERROR.
            IF NOT AVAILABLE TT_Object THEN LEAVE blkMK.

            IF TT_Object.Rotate <> 90 THEN LEAVE blkMK.

            {getDict bTT_dict TT_dict.value_dict_id "SCALAR" "~/R"}.
            IF AVAILABLE bTT_dict AND bTT_dict.value_scalar = "90" THEN DO:
                bTT_dict.value_scalar = "0".
                DEFINE VARIABLE deRectOrig AS DECIMAL  EXTENT 4   NO-UNDO.
                DEFINE VARIABLE deRectNew  AS DECIMAL  EXTENT 4   NO-UNDO.
                {getDict TT_dict piDictOrArrayId "ARRAY" "~/Rect"}.
                IF AVAILABLE TT_dict THEN DO:
                    DO i = 1 TO 4:
                        FIND TT_array WHERE TT_array.array_id = TT_dict.value_array_id
                            AND TT_array.value_id = i NO-ERROR.
                        IF AVAILABLE TT_array THEN
                            deRectOrig[i] = string2dec(TT_array.value_scalar).
                    END.
                    ASSIGN
                     deRectNew[1] = deRectOrig[2]
                     deRectNew[2] = TT_Object.obj_Media3 - deRectOrig[3]
                     deRectNew[3] = deRectOrig[4]
                     deRectNew[4] = TT_Object.obj_Media3 - deRectOrig[1]
                     .
                    DO i = 1 TO 4:
                        FIND TT_array WHERE TT_array.array_id = TT_dict.value_array_id
                            AND TT_array.value_id = i NO-ERROR.
                        TT_array.value_scalar = dec2string(deRectNew[i]).
                    END.
                END.
            END.
        END.
    END.
    ELSE DO: /* in case we have an annotation which is not a form widget */
        {getDict TT_dict piDictOrArrayId "DICT" "~/P"}. /* Parent page */
        IF AVAILABLE TT_dict THEN ASSIGN
            TT_dict.value_type = "SCALAR"
            TT_dict.value_scalar = ENTRY(1,pcParams) + " 0 R".
    END.
END. /* _callBackAcroForm */
/* 24-AUG-2012 jcc: end */

PROCEDURE pdf_Length: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER P_length   AS INTEGER NO-UNDO.

  ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Length", ?, 0, "").
  PUT STREAM S_pdf_inc UNFORMATTED
      pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
      P_length {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Length */

PROCEDURE pdf_Catalog : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream        AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdf_OutlinesDict AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt       AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE cObjectsToExport AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lNeedAppearances AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE mEncryptKey      AS MEMPTR      NO-UNDO.

  DEFINE BUFFER TT_Resource  FOR TT_Resource.
  DEFINE BUFFER bTT_Resource FOR TT_Resource.
  DEFINE BUFFER TT_Object    FOR TT_Object.
  DEFINE BUFFER TT_dict      FOR TT_dict.

  ObjectSequence(pdfStream, 2, "Catalog", ?, 0, "").
  PUT STREAM S_pdf_inc UNFORMATTED
      "2 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Catalog" {&pdfSKIP}
      "/Pages 3 0 R" {&pdfSKIP}.

  IF pdf_OutlinesDict <> 0 THEN
    PUT STREAM S_pdf_inc UNFORMATTED
      "/Outlines " pdf_OutlinesDict " 0 R" {&pdfSKIP}.

  /* Determine PageMode */
  PUT STREAM S_pdf_inc UNFORMATTED
       "/PageMode /" TRIM(pdf_get_parameter(pdfStream,"PageMode")) {&pdfSkip}.

  /* Determine PageLayout */
  PUT STREAM S_pdf_inc UNFORMATTED
       "/PageLayout /" TRIM(pdf_get_parameter(pdfStream,"PageLayout")) {&pdfSkip}.

  /* 23-AUG-2012 jcc: External AcroForm */
  /* IF pdf_get_parameter(pdfStream, "retainAcroForm") = "TRUE" THEN DO: */
  IF CAN-FIND(FIRST TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.widget_retain > "") THEN DO:
      PUT STREAM S_pdf_inc UNFORMATTED "/AcroForm<</Fields[".
      FOR EACH TT_Resource WHERE TT_Resource.obj_stream = pdfStream
                             AND TT_Resource.page_id    = 0
                             AND TT_Resource.res_type   = "FormWidget",
          /* 28-MAY-2013 jcc: cannot use "Annot", as widgets with kids do not have any associated Annot. Instead, use the TT_Object. */
          /* FIRST bTT_Resource WHERE bTT_Resource.obj_stream = pdfStream */
                               /* AND bTT_Resource.res_type   = "Annot" */
                               /* AND bTT_Resource.res_obj    = TT_Resource.res_obj */
                               /* AND bTT_Resource.res_gen    = TT_Resource.res_gen */
                               /* AND bTT_Resource.new_obj <> 0: /* bTT_Resource.new_obj = 0 when the object has not been exported (it must be on a page we have not used => do not write it down) */ */
          LAST TT_Object
              WHERE TT_Object.obj_stream = pdfStream
                AND TT_Object.pdf_id     = TT_Resource.pdf_id
                AND TT_Object.obj_id     = TT_Resource.res_obj
                AND TT_Object.gen_id     = TT_Resource.res_gen
                AND TT_Object.new_obj   <> 0:
          /* PUT STREAM S_pdf_inc UNFORMATTED bTT_Resource.new_obj " 0 R ". */
          PUT STREAM S_pdf_inc UNFORMATTED TT_Object.new_obj " 0 R ".
      END.
      PUT STREAM S_pdf_inc UNFORMATTED "]".
      /* Export all other fields from the original AcroForm dictionary */
      FOR FIRST TT_Resource WHERE TT_Resource.obj_stream = pdfStream
                              AND TT_Resource.page_id    = 0
                              AND TT_Resource.res_type   = "AcroForm",
          FIRST TT_Object WHERE TT_Object.obj_stream = pdfStream
                            AND TT_Object.pdf_id     = TT_Resource.pdf_id
                            AND TT_Object.obj_id     = TT_Resource.res_obj
                            AND TT_Object.gen_id     = TT_Resource.res_gen,
          EACH TT_dict WHERE TT_dict.dict_id = TT_Object.obj_dict_id
                         AND TT_dict.dict_key <> "~/Fields":
          PUT STREAM S_pdf_inc UNFORMATTED TT_dict.dict_key.
          IF TT_dict.dict_key = "~/NeedAppearances" THEN
              lNeedAppearances = TRUE.
          CASE TT_dict.value_type:
              WHEN "SCALAR" THEN
                  PUT STREAM S_pdf_inc UNFORMATTED " " TT_dict.value_scalar.
              WHEN "ARRAY" THEN
                  RUN recursivelyExportArray(pdfStream
                                            ,TT_dict.value_array_id
                                            ,"AcroForm"
                                            ,INPUT-OUTPUT cObjectsToExport
                                            ,pdfEncrypt
                                            ,2
                                            ,mEncryptKey).
              WHEN "DICT" THEN
                  RUN recursivelyExportDict (pdfStream
                                            ,TT_dict.value_dict_id
                                            ,"AcroForm"
                                            ,INPUT-OUTPUT cObjectsToExport
                                            ,pdfEncrypt
                                            ,2
                                            ,mEncryptKey).
          END CASE.
      END.  
      IF NOT lNeedAppearances THEN
          PUT STREAM S_pdf_inc UNFORMATTED "~/NeedAppearances true". /* 28-AUG-2012 jcc: seems to be needed for Adobe reader to display the fields contents */
      PUT STREAM S_pdf_inc UNFORMATTED ">>" {&pdfSKIP}.
  END.

  /* Do Viewer Preferences */
  PUT STREAM s_pdf_inc UNFORMATTED
    "/ViewerPreferences << ".

  IF TRIM(pdf_get_parameter(pdfStream,"HideToolbar")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/HideToolbar true ".

  IF TRIM(pdf_get_parameter(pdfStream,"HideMenubar")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/HideMenubar true ".

  IF TRIM(pdf_get_parameter(pdfStream,"HideWindowUI")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/HideWindowUI true ".

  IF TRIM(pdf_get_parameter(pdfStream,"FitWindow")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/FitWindow true ".

  IF TRIM(pdf_get_parameter(pdfStream,"CenterWindow")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/CenterWindow true ".

  IF TRIM(pdf_get_parameter(pdfStream,"DisplayDocTitle")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/DisplayDocTitle true ".
 
  PUT STREAM s_pdf_inc UNFORMATTED
      " >>" {&pdfSKIP}.

  /* end of Viewer Preferences */

  PUT STREAM S_pdf_inc UNFORMATTED
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

  /* 02-JUN-2013 jcc: Now export all the Acroform dependant objects */
  /* Never encountered any example needing this, but for the sake of completeness... */
  IF cObjectsToExport > "" THEN DO:
      DO i = 2 TO NUM-ENTRIES(cObjectsToExport):
          FIND TT_Object WHERE ROWID(TT_Object) = TO-ROWID(ENTRY(i,cObjectsToExport)).
          RUN recursivelyExportObject(pdfStream, BUFFER TT_Object, "AcroForm", pdfEncrypt).
      END.
  END.

END. /* pdf_Catalog */

PROCEDURE pdf_ListPages : /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  ObjectSequence(pdfStream, 3, "Pages", ?, 0, "").

  PUT STREAM S_pdf_inc UNFORMATTED
      "3 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Pages" {&pdfSKIP}
      "/Count " pdf_Page(pdfStream) {&pdfSKIP}
      /* igc - don't need a default page size as it is defined in each page
               definition
      "/MediaBox [ 0 0 " pdf_PageWidth(pdfStream)
      " " pdf_PageHeight(pdfStream) " ]" {&pdfSKIP} */
      "/Kids [ " {&pdfSKIP}.

  FOR EACH TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream
                           AND TT_pdf_object.obj_desc   = "PageDefinition"
      BY TT_pdf_object.obj_page: /* 04-APR-2015 jcc: added BY... */
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_object.obj_nbr " 0 R " {&pdfSKIP}.
  END. /* Display Pages */

  PUT STREAM S_pdf_inc UNFORMATTED
      "]"  {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_ListPages */

PROCEDURE pdf_xref : /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE cFreeObjects     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_ctr            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_obj            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE pdf-Stream-Start AS INTEGER     NO-UNDO.

  DEFINE BUFFER TT_pdf_object FOR TT_pdf_object.

  ObjectSequence(pdfStream, 0, "Xref", ?, 0, "").
  /* 23-FEB-2010 jcc: get last object number */
  FIND LAST TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream NO-ERROR.
  IF AVAILABLE TT_pdf_object THEN
    L_ctr = TT_pdf_object.obj_nbr.
  ELSE
    L_ctr = 0.
  /* FOR EACH TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream: */
    /* L_ctr = L_ctr + 1. */
  /* END. */
  /* 23-FEB-2010 jcc: end */

  /* Get the Xref start point */
  pdf-Stream-Start = SEEK(S_pdf_inc).

  PUT STREAM S_pdf_inc UNFORMATTED
      "xref" {&pdfSKIP}
      "0 " L_ctr + 1 {&pdfSKIP}.

  /* 02-MAR-2010 jcc: correctly mark the free objects: there may be now, for
                      the optimisation of outputting only used fonts (TT_pdf_font.used_flag) */
  DO i = 0 TO L_ctr:
      FIND TT_pdf_object
          WHERE TT_pdf_object.obj_stream = pdfStream
            AND TT_pdf_object.obj_nbr = i NO-ERROR.
      IF AVAILABLE TT_pdf_object THEN DO: /* used object (except 0 which is always free) */
          IF TT_pdf_object.obj_nbr = 0 THEN
              PUT STREAM S_pdf_inc CONTROL
                  "0000000000"
                  " "
                  TT_pdf_object.gen_nbr " "
                  TT_pdf_object.obj_type CHR(13) CHR(10).
          ELSE DO:
              TT_pdf_object.obj_off = TT_pdf_object.obj_off - 1.
              PUT STREAM S_pdf_inc CONTROL
                  STRING(TT_pdf_object.obj_off, "9999999999")
                  " "
                  STRING(TT_pdf_object.gen_nbr, "99999") " "
                  TT_pdf_object.obj_type CHR(13) CHR(10).
          END.
      END.
      ELSE DO: /* free object */
          PUT STREAM S_pdf_inc CONTROL
              "0000000000 65635 f" CHR(13) CHR(10).
      END.
  END.
  /* 02-MAR-2010 jcc: end */

  FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.

  /* FIND LAST TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream NO-LOCK NO-ERROR. */

  PUT STREAM S_pdf_inc UNFORMATTED
      "trailer" {&pdfSKIP}
      "<<" {&pdfSKIP}
      /* "/Size " (TT_pdf_object.obj_nbr + 1) {&pdfSKIP} */
      "/Size " (L_ctr + 1) {&pdfSKIP}
      "/Root 2 0 R" {&pdfSKIP}
      "/Info 1 0 R" {&pdfSKIP}.

  IF pdfEncrypt THEN DO:
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Encrypt " TT_pdf_stream.obj_EncryptDict " 0 R" {&pdfSKIP}
        "/ID [<" TT_pdf_stream.obj_ID "><" TT_pdf_stream.obj_ID ">]" {&pdfSKIP}.
  END.

  PUT STREAM S_pdf_inc UNFORMATTED
      ">>" {&pdfSKIP}
      "startxref" {&pdfSKIP}
      pdf-Stream-Start {&pdfSKIP}
      "%%EOF" {&pdfSKIP}.

END. /* pdf_xref */

PROCEDURE pdf_Fonts: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                         AND TT_pdf_font.font_file  = "PDFBASE14"
                         AND (TT_pdf_font.used_flag  = TRUE /* 22-FEB-2010 jcc: added */
                           OR TT_pdf_font.used_by_xobject > "") /* 04-MAY-2014 jcc: added */
      BY TT_pdf_font.font_obj:

    ObjectSequence(pdfStream, TT_pdf_font.font_obj, "Font", ?, 0, "").
    
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_font.FONT_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /Font" {&pdfSKIP}
        "/Subtype /Type1" {&pdfSKIP}
        "/Name " TT_pdf_font.font_tag {&pdfSKIP}.

    IF TT_pdf_font.font_name = "Symbol" 
    OR TT_pdf_font.font_name = "ZapfDingbats" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Encoding << /BaseEncoding /StandardEncoding >>" {&pdfSKIP}.
    ELSE
      PUT STREAM S_pdf_inc UNFORMATTED
        "/Encoding 4 0 R" {&pdfSKIP}.

      PUT STREAM S_pdf_inc UNFORMATTED
        "/BaseFont /" TT_pdf_font.font_name {&pdfSKIP}.
    /* 23-FEB-2010 jcc: Tomasz Judycki 10/2/2007
          - PDFBASE14 fonts width must be adjusted if we replace some chars */
    IF TT_pdf_font.afm_widths <> "" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
        "/FirstChar 0" {&pdfSKIP}
        "/LastChar " TRIM( STRING( NUM-ENTRIES( TRIM(TT_pdf_font.afm_widths), " " ) - 1, "->>>>>9" ) ) {&pdfSKIP}
        "/Widths [ " TT_pdf_font.afm_widths " ]" {&pdfSKIP}.
    PUT STREAM S_pdf_inc UNFORMATTED
    /* 23-FEB-2010 jcc: end */
        ">>" {&pdfSKIP}
        "endobj" {&pdfSKIP}.
  END.

END. /* pdf_Fonts */

/* 18-FEB-2014 jcc: font subsetting */
/* We will remove from the font file all the glyphs description for the non used ones ('glyf' table),
   and rebuild the table of pointers to the glyphs (loca); we will also drop useless tables.
   The characters' glyphs have already been assigned through pdf_parseAfmFile (pdf_load_font) or
   pdf_parse_font.p (pdfload_font2) */
PROCEDURE buildFontSubset: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream     AS CHARACTER   NO-UNDO.
    DEFINE PARAMETER BUFFER TT_pdf_font FOR TT_pdf_font.
    DEFINE OUTPUT PARAMETER mFontSubset   AS MEMPTR      NO-UNDO.
    DEFINE OUTPUT PARAMETER pSubsetPrefix AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE c                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iChar              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFlags             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFontFileSize      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iGlyph             AS INTEGER   NO-UNDO. /* iter */.
    DEFINE VARIABLE iGlyphIndex        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLength            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumberOfContours  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumberOfGlyphs    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumTables         AS INTEGER   NO-UNDO. /* number of tables */.
    DEFINE VARIABLE iOffset            AS INTEGER   INITIAL 1 NO-UNDO.
    DEFINE VARIABLE iShortOffset       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubsetPrefix      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTable             AS INTEGER   NO-UNDO. /* iter */.
    DEFINE VARIABLE j                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE mFont              AS MEMPTR    NO-UNDO.

    DEFINE BUFFER TT_pdf_font_table              FOR TT_pdf_font_table.
    DEFINE BUFFER tt_pdf_font_index_to_loc       FOR tt_pdf_font_index_to_loc.
    DEFINE BUFFER TT_pdf_font_character          FOR TT_pdf_font_character.
    DEFINE BUFFER TT_pdf_font_compound_character FOR TT_pdf_font_character.


    SET-BYTE-ORDER(mFont) = BIG-ENDIAN.
    SET-BYTE-ORDER(mFontSubset) = BIG-ENDIAN.
    RUN getFileAsMemptr(pdfStream, TT_pdf_font.font_file, ?, INPUT-OUTPUT mFont, OUTPUT iFontFileSize).

    IF GET-UNSIGNED-LONG(mFont, iOffset) <> 65536 THEN DO:
        mFontSubset = mFont.
        RETURN.
    END.
    iOffset = iOffset + 4.

    iNumTables = GET-UNSIGNED-SHORT(mFont, iOffset).
    iOffset = iOffset + 2.

    /* skip searchRange, entrySelector and rangeShift */
    iOffset = iOffset + 6.

    /* get info about the font tables */
    DO iTable = 1 TO iNumTables:
        CREATE TT_pdf_font_table.
        ASSIGN
         TT_pdf_font_table.table_name = GET-STRING(mFont, iOffset, 4)
         TT_pdf_font_table.checksum   = GET-LONG(mFont, iOffset + 4)
         TT_pdf_font_table.offset     = GET-UNSIGNED-LONG(mFont, iOffset + 8) + 1 /* + 1 : the offset for the first byte is 1 for Progress in GET-* functions, whereas it is 0 is many other languages */
         TT_pdf_font_table.length     = GET-UNSIGNED-LONG(mFont, iOffset + 12).
        iOffset = iOffset + 16.
    END.

    /* check font magic number, must be 0x5F0F3CF5 */
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "head".
    IF GET-UNSIGNED-LONG(mFont, tt_pdf_font_table.offset + 12) <> 1594834165 /*0x5F0F3CF5*/ THEN DO:
        mFontSubset = mFont.
        MESSAGE "bad magic at" tt_pdf_font_table.offset + 12 ":" HEX-ENCODE(GET-BYTES(mFont, tt_pdf_font_table.offset + 12, 4)) "expected 0x5F0F3CF5"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    /* get offset mode (indexToLocFormat : 0 = short, 1 = long) */
    iShortOffset = GET-SHORT(mFont, tt_pdf_font_table.offset + 50).
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "loca".
    iOffset = tt_pdf_font_table.offset.
    IF iShortOffset = 0 THEN DO:
        iNumberOfGlyphs = TRUNCATE(tt_pdf_font_table.length / 2, 0).
        DO iGlyph = 1 TO iNumberOfGlyphs:
            CREATE tt_pdf_font_index_to_loc.
            ASSIGN
             tt_pdf_font_index_to_loc.glyph = iGlyph - 1
             tt_pdf_font_index_to_loc.loc   = GET-UNSIGNED-SHORT(mFont, iOffset) * 2.
            iOffset = iOffset + 2.
        END.
    END. ELSE DO:
        iNumberOfGlyphs = tt_pdf_font_table.length / 4.
        DO iGlyph = 1 TO iNumberOfGlyphs:
            CREATE tt_pdf_font_index_to_loc.
            ASSIGN
             tt_pdf_font_index_to_loc.glyph = iGlyph - 1
             tt_pdf_font_index_to_loc.loc   = GET-UNSIGNED-LONG(mFont, iOffset).
            iOffset = iOffset + 4.
        END.
    END.

    /* always add glyph zero (.notdef): the first glyph in any TrueType font must be the MISSING CHARACTER GLYPH */
    iChar = iChar - 1. /* iChar < 0 */
    CREATE TT_pdf_font_character.
    ASSIGN
     TT_pdf_font_character.obj_stream = pdfStream
     TT_pdf_font_character.font_name  = TT_pdf_font.font_name
     TT_pdf_font_character.char_id    = iChar /* no char id */
     TT_pdf_font_character.glyph      = 0
     TT_pdf_font_character.used       = YES
     TT_pdf_font_character.alldone    = YES.

    /* add all composite glyphs parts for used characters */
    /* according to the TTF spec, a component cannot be himself compound: "In TrueType 1.0, the maximum legal value for maxComponentDepth is one." */
    /* nevertheless, I implemented it so that it can support various levels of recursion, as fonts do not always respect the spec. */
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "glyf".
    DO WHILE CAN-FIND(FIRST TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
                      AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                      AND TT_pdf_font_character.alldone = NO
                      AND TT_pdf_font_character.used = YES)
             OR CAN-FIND(FIRST TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
                      AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                      AND TT_pdf_font_character.alldone = NO
                      AND TT_pdf_font_character.used_as_component = YES):

        FOR EACH TT_pdf_font_character WHERE
               (TT_pdf_font_character.obj_stream = pdfStream
                AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                AND TT_pdf_font_character.alldone = NO
                AND TT_pdf_font_character.used = YES)
            OR (TT_pdf_font_character.obj_stream = pdfStream
                AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                AND TT_pdf_font_character.alldone = NO
                AND TT_pdf_font_character.used_as_component = YES),
            FIRST tt_pdf_font_index_to_loc WHERE tt_pdf_font_index_to_loc.glyph = TT_pdf_font_character.glyph:

            ASSIGN
             iSubsetPrefix = (iSubsetPrefix + TT_pdf_font_character.glyph) MODULO 1000000
             iOffset       = tt_pdf_font_table.offset + tt_pdf_font_index_to_loc.loc.
            iNumberOfContours = GET-SHORT(mFont, iOffset).
            iOffset = iOffset + 2.
            IF iNumberOfContours < 0 THEN DO: /* If the number of contours is -1, the glyph is compound */
                /* RUN debugLog("Glyph " + STRING(TT_pdf_font_character.glyph) + " is compound"). */
                iOffset = iOffset + 8. /* skip xMin, yMin, xMax, yMax (bounding box) */
                /* read the compound glyph data to know all the used glyph indices */
                DO WHILE TRUE:
                    /* RUN debugLog("Offset: " + STRING(iOffset - 1)). */
                    iFlags = GET-UNSIGNED-SHORT(mFont, iOffset).
                    iOffset = iOffset + 2.
                    iGlyphIndex = GET-UNSIGNED-SHORT(mFont, iOffset).
                    iOffset = iOffset + 2.
                    FIND FIRST TT_pdf_font_compound_character WHERE TT_pdf_font_compound_character.obj_stream = pdfStream
                        AND TT_pdf_font_compound_character.font_name = TT_pdf_font.font_name
                        AND TT_pdf_font_compound_character.glyph     = iGlyphIndex NO-ERROR.
                    /* this should not happen, but sometimes, some glyph are missing from the afm/ufm file */
                    /* IF NOT AVAILABLE TT_pdf_font_compound_character THEN DO: */
                        /* MESSAGE "not avail glyph" iGlyphIndex "for char" TT_pdf_font_character.char_id "glyph" TT_pdf_font_character.glyph */
                            /* VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                        /* iChar = iChar - 1. */
                        /* CREATE TT_pdf_font_compound_character. */
                        /* ASSIGN */
                         /* TT_pdf_font_compound_character.obj_stream = pdfStream */
                         /* TT_pdf_font_compound_character.font_name  = TT_pdf_font.font_name */
                         /* TT_pdf_font_compound_character.char_id    = iChar - 65536 /* no char id */ */
                         /* TT_pdf_font_compound_character.glyph      = iGlyphIndex. */
                        /* RUN debugLog("Add glyph " + STRING(iGlyphIndex)). */
                    /* END. */
                    IF AVAILABLE TT_pdf_font_compound_character THEN /* 19-OCT-2014 jcc: should always be the case */
                        TT_pdf_font_compound_character.used_as_component = YES.
                        /* TT_pdf_font_compound_character.used = YES */ /* 20-OCT-2014 jcc: make a distinction between really used characters, and glyphs used because they are a component of a used one */
                    /* Skip some bytes depending on iFlags bits */
                    IF GET-BITS(iFlags, 1, 1) = 1 THEN /* ARG_1_AND_2_ARE_WORDS */
                        iOffset = iOffset + 4.
                    ELSE
                        iOffset = iOffset + 2.
                    IF GET-BITS(iFlags, 4, 1) = 1 THEN /* WE_HAVE_A_SCALE */
                        iOffset = iOffset + 2.
                    ELSE IF GET-BITS(iFlags, 7, 1) = 1 THEN /* WE_HAVE_AN_X_AND_Y_SCALE */
                        iOffset = iOffset + 4.
                    ELSE IF GET-BITS(iFlags, 8, 1) = 1 THEN /* WE_HAVE_A_TWO_BY_TWO */
                        iOffset = iOffset + 8.
                    IF GET-BITS(iFlags, 6, 1) = 0 THEN /* no MORE_COMPONENTS */
                        LEAVE.
                END.
            END.
            TT_pdf_font_character.alldone = YES.
        END.
    END.

    /* Now rebuild the font file */
    DEFINE VARIABLE cTable            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTablesToCopy     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iEntrySelector    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iFontLength       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iGlyphOffset      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iGlyphStartOffset AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iLocaOffset       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNumLongs         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNumSteps         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iOffset2          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iPadding          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iRangeShift       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSearchRange      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSum              AS INT64       NO-UNDO.
    DEFINE VARIABLE mFontSubsetTmp    AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE mLoca             AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE mGlyf             AS MEMPTR      NO-UNDO.

    DEFINE BUFFER btt_pdf_font_index_to_loc FOR tt_pdf_font_index_to_loc.

    /* These are the minimum tables to be present in an embedded font program (+ loca & glyf)
       For simple font dictionaries, cmap has to be included, as there is no CidToGidMap there. */
    cTablesToCopy = "cvt ,fpgm,head,hhea,hmtx,maxp,prep" + IF NOT TT_pdf_font.is_unicode THEN ",cmap" ELSE "".

    SET-BYTE-ORDER(mFontSubsetTmp) = BIG-ENDIAN.
    SET-SIZE(mFontSubsetTmp) = GET-SIZE(mFont). /* will be shorter, we will have to truncate it later */
    iOffset = 1.
    &IF INTEGER(ENTRY(1, PROVERSION, ".")) < 10 &THEN
    PUT-UNSIGNED-LONG(mFontSubsetTmp, 1, 65536).
    &ELSE
    PUT-UNSIGNED-LONG(mFontSubsetTmp, 1) = 65536.
    &ENDIF
    iOffset = iOffset + 4.
    iNumTables = NUM-ENTRIES(cTablesToCopy) + 2. /* + 'glyf' & 'loca' */
    PUT-UNSIGNED-SHORT(mFontSubsetTmp, iOffset) = iNumTables. /* number of tables */
    iOffset = iOffset + 2.
    iEntrySelector = TRUNCATE(math_log2(iNumTables), 0).
    iSearchRange   = EXP(2, iEntrySelector) * 16.
    iRangeShift    = iNumTables * 16 - iSearchRange.
    PUT-UNSIGNED-SHORT(mFontSubsetTmp, iOffset) = iSearchRange.
    iOffset = iOffset + 2.
    PUT-UNSIGNED-SHORT(mFontSubsetTmp, iOffset) = iEntrySelector.
    iOffset = iOffset + 2.
    PUT-UNSIGNED-SHORT(mFontSubsetTmp, iOffset) = iRangeShift.
    iOffset = iOffset + 2.

    /* skip header (will be written later) */
    iOffset = iOffset + (NUM-ENTRIES(cTablesToCopy) + 2) * 16.

    /* copy the tables to preserve (loca and glyf tables will be added later) */
    /* the cmap table is not needed and shall not be present, since the mapping from character codes
       to glyph descriptions is provided separately (see /CIDToGIDMap) */
    DO i = 1 TO NUM-ENTRIES(cTablesToCopy):
        cTable = ENTRY(i, cTablesToCopy).
        FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = cTable NO-ERROR.
        IF NOT AVAILABLE tt_pdf_font_table THEN NEXT.
        /* copy the table by chunks of 30000 bytes */
        ASSIGN
         iNumSteps = TRUNCATE(tt_pdf_font_table.length / 30000, 0)
         iOffset2  = 0.
        DO j = 1 TO iNumSteps:
            PUT-BYTES(mFontSubsetTmp, iOffset + iOffset2) = GET-BYTES(mFont, tt_pdf_font_table.offset + iOffset2, 30000).
            iOffset2 = iOffset2 + 30000.
        END.
        IF tt_pdf_font_table.length > iOffset2 THEN DO:
            PUT-BYTES(mFontSubsetTmp, iOffset + iOffset2) = GET-BYTES(mFont, tt_pdf_font_table.offset + iOffset2,
                                                                   tt_pdf_font_table.length - iOffset2).
        END.
        /* Set the checkSumAdjustment to 0 - will be used by the entire font checksum */
        IF cTable = "head" THEN DO:
            PUT-BYTE(mFontSubsetTmp, iOffset + 8)  = 0.
            PUT-BYTE(mFontSubsetTmp, iOffset + 9)  = 0.
            PUT-BYTE(mFontSubsetTmp, iOffset + 10) = 0.
            PUT-BYTE(mFontSubsetTmp, iOffset + 11) = 0.
        END.
        /* pad the table length to 4 bytes */
        iPadding = 4 - tt_pdf_font_table.length MODULO 4.
        IF iPadding <> 4 THEN
            tt_pdf_font_table.length = tt_pdf_font_table.length + iPadding.
        tt_pdf_font_table.offset = iOffset.

        iOffset = iOffset + tt_pdf_font_table.length.
    END.

    /* add new 'glyf' table and build 'loca' table */
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "loca".
    SET-SIZE(mLoca) = tt_pdf_font_table.length.
    SET-BYTE-ORDER(mLoca) = BIG-ENDIAN.
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "glyf".
    SET-SIZE(mGlyf) = tt_pdf_font_table.length.
    ASSIGN
     iGlyphOffset      = 0
     iLocaOffset       = 1
     iGlyphStartOffset = iOffset.
    DO iGlyph = 1 TO iNumberOfGlyphs:
        /* RUN debugLog("Glyph " + STRING(iGlyph - 1)). */
        FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
            AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
            AND TT_pdf_font_character.glyph     = iGlyph - 1
            AND TT_pdf_font_character.used      = YES
            NO-ERROR.
        IF NOT AVAILABLE TT_pdf_font_character THEN
            FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
                AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                AND TT_pdf_font_character.glyph     = iGlyph - 1
                AND TT_pdf_font_character.used_as_component = YES
                NO-ERROR.
        IF AVAILABLE TT_pdf_font_character THEN DO:
            FIND  tt_pdf_font_index_to_loc WHERE  tt_pdf_font_index_to_loc.glyph = TT_pdf_font_character.glyph.
            FIND btt_pdf_font_index_to_loc WHERE btt_pdf_font_index_to_loc.glyph = TT_pdf_font_character.glyph + 1.
            iLength = btt_pdf_font_index_to_loc.loc - tt_pdf_font_index_to_loc.loc.
            PUT-BYTES(mGlyf, iGlyphOffset + 1) = GET-BYTES(mFont, tt_pdf_font_table.offset + tt_pdf_font_index_to_loc.loc, iLength) NO-ERROR.
            /* RUN debugLog("Glyf: copy " + STRING(iLength) + " bytes from font at " + STRING(tt_pdf_font_table.offset + tt_pdf_font_index_to_loc.loc - 1) + " to glyf at " + STRING(iGlyphOffset)). */
        END.
        ELSE DO:
            iLength = 0.
            /* RUN debugLog("No char available. Length=0."). */
        END.
        IF iShortOffset = 0 THEN DO:
            PUT-UNSIGNED-SHORT(mLoca, iLocaOffset) = INTEGER(TRUNCATE(iGlyphOffset / 2, 0)).
            /* RUN debugLog("Loca: put " + STRING(INTEGER(TRUNCATE(iGlyphOffset / 2, 0))) + " at " + STRING(iLocaOffset)). */
            iLocaOffset = iLocaOffset + 2.
        END. ELSE DO:
            &IF INTEGER(ENTRY(1, PROVERSION, ".")) < 10 &THEN
            PUT-UNSIGNED-LONG(mLoca, iLocaOffset, iGlyphOffset).
            &ELSE
            PUT-UNSIGNED-LONG(mLoca, iLocaOffset) = iGlyphOffset.
            &ENDIF
            /* RUN debugLog("Loca: put " + STRING(iGlyphOffset) + " at " + STRING(iLocaOffset - 1)). */
            iLocaOffset = iLocaOffset + 4.
        END.
        ASSIGN
         iGlyphOffset = iGlyphOffset + iLength.
    END.
    /* pad the table length to 4 bytes */
    tt_pdf_font_table.length   = iGlyphOffset.
    iPadding = 4 - tt_pdf_font_table.length MODULO 4.
    IF iPadding <> 4 THEN ASSIGN
        tt_pdf_font_table.length = tt_pdf_font_table.length + iPadding.

    /* add 'loca' table */
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "loca".
    ASSIGN
     tt_pdf_font_table.offset = iOffset
     tt_pdf_font_table.length = iLocaOffset - 1.
    /* copy the table by chunks of 30000 bytes */
    ASSIGN
     iNumSteps = TRUNCATE(tt_pdf_font_table.length / 30000, 0)
     iOffset2  = 0.
    DO j = 1 TO iNumSteps:
        PUT-BYTES(mFontSubsetTmp, iOffset + iOffset2) = GET-BYTES(mLoca, iOffset2 + 1, 30000).
        iOffset2 = iOffset2 + 30000.
    END.
    IF tt_pdf_font_table.length > iOffset2 THEN
        PUT-BYTES(mFontSubsetTmp, iOffset + iOffset2) = GET-BYTES(mLoca, iOffset2 + 1,
                                                               tt_pdf_font_table.length - iOffset2).
    iPadding = 4 - tt_pdf_font_table.length MODULO 4.
    IF iPadding <> 4 THEN
        tt_pdf_font_table.length = tt_pdf_font_table.length + iPadding.
    iOffset = iOffset + tt_pdf_font_table.length.
    /* compute 'loca' checksum */ /*
    iNumLongs = tt_pdf_font_table.length / 4 - 1.
    iOffset2 = 1.
    DO i = 1 TO iNumLongs:
        iSum = iSum + GET-UNSIGNED-LONG(mLoca, iOffset2). /* MODULO 4294967296 */ /*0x100000000*/.
        iOffset2 = iOffset2 + 4.
    END.
    iSum = iSum MODULO 4294967296.
    tt_pdf_font_table.checksum = IF iSum > 2147483647 THEN iSum - 4294967296 ELSE iSum. */
    SET-SIZE(mLoca) = 0.

    /* 25-FEB-2014 jcc: add 'glyf' table */
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "glyf".
    tt_pdf_font_table.offset = iOffset.
    /* copy the table by chunks of 30000 bytes */
    ASSIGN
     iNumSteps = TRUNCATE(tt_pdf_font_table.length / 30000, 0)
     iOffset2  = 0.
    DO j = 1 TO iNumSteps:
        PUT-BYTES(mFontSubsetTmp, iOffset + iOffset2) = GET-BYTES(mGlyf, iOffset2 + 1, 30000).
        iOffset2 = iOffset2 + 30000.
    END.
    IF tt_pdf_font_table.length > iOffset2 THEN
        PUT-BYTES(mFontSubsetTmp, iOffset + iOffset2) = GET-BYTES(mGlyf, iOffset2 + 1,
                                                               tt_pdf_font_table.length - iOffset2).
    iPadding = 4 - tt_pdf_font_table.length MODULO 4.
    IF iPadding <> 4 THEN
        tt_pdf_font_table.length = tt_pdf_font_table.length + iPadding.
    iOffset = iOffset + tt_pdf_font_table.length.
    /* total font length */
    iFontLength = iOffset - 1.
    /* compute 'glyf' checksum */ /*
    iNumLongs = tt_pdf_font_table.length / 4 - 1.
    iOffset2 = 1.
    DO i = 1 TO iNumLongs:
        iSum = iSum + GET-UNSIGNED-LONG(mGlyf, iOffset2). /* MODULO 4294967296 */ /*0x100000000*/.
        iOffset2 = iOffset2 + 4.
    END.
    iSum = iSum MODULO 4294967296.
    tt_pdf_font_table.checksum = IF iSum > 2147483647 THEN iSum - 4294967296 ELSE iSum. */
    SET-SIZE(mGlyf) = 0.

    /* now that we have the correct checksums, offsets & lengths, put them in the font header */
    iOffset = 13. /* go just at the 1st table name */
    DO i = 1 TO NUM-ENTRIES(cTablesToCopy + ",loca,glyf"):
        cTable = ENTRY(i, cTablesToCopy + ",loca,glyf").
        FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = cTable NO-ERROR.
        IF NOT AVAILABLE tt_pdf_font_table THEN NEXT.
        PUT-STRING(mFontSubsetTmp, iOffset) = cTable.
        iOffset = iOffset + 4.
        /* 03-JUL-2014 jcc: it seems useless to compute the checksums, it displays well without it, but something has to be written there. I chose 1728 = 12^3. So for performance reasons I commented out the checksums computations. */
        /* PUT-LONG(mFontSubsetTmp, iOffset) = tt_pdf_font_table.checksum. */
        PUT-LONG(mFontSubsetTmp, iOffset) = 1728.
        iOffset = iOffset + 4.
        &IF INTEGER(ENTRY(1, PROVERSION, ".")) < 10 &THEN
        PUT-UNSIGNED-LONG(mFontSubsetTmp, iOffset, tt_pdf_font_table.offset - 1). /* - 1 : the 1st offset is at offset 0 */
        &ELSE
        PUT-UNSIGNED-LONG(mFontSubsetTmp, iOffset) = tt_pdf_font_table.offset - 1. /* - 1 : the 1st offset is at offset 0 */
        &ENDIF
        iOffset = iOffset + 4.
        &IF INTEGER(ENTRY(1, PROVERSION, ".")) < 10 &THEN
        PUT-UNSIGNED-LONG(mFontSubsetTmp, iOffset, tt_pdf_font_table.length).
        &ELSE
        PUT-UNSIGNED-LONG(mFontSubsetTmp, iOffset) = tt_pdf_font_table.length.
        &ENDIF
        iOffset = iOffset + 4.
    END.

    /* 03-JUL-2014 jcc: seems useless, the pdf displays fine without it
    /* set the checkSumAdjustment in the head table */
    FIND tt_pdf_font_table WHERE tt_pdf_font_table.table_name = "head".
    /* compute the font checksum */
    iNumLongs = tt_pdf_font_table.length / 4.
    iOffset = 1.
    DO i = 1 TO iNumLongs:
        iSum = iSum + GET-UNSIGNED-LONG(mFontSubsetTmp, iOffset). /* MODULO 4294967296 */ /*0x100000000*/.
        iOffset = iOffset + 4.
    END.
    /* store the checkSumAdjustment */
    iSum = 2981146554 /*0xB1B0AFBA*/ - (iSum MODULO 4294967296).
    /* PUT-UNSIGNED-LONG(mFontSubsetTmp, tt_pdf_font_table.offset + 8) = GET-BITS(iSum, 1, 32). */ */

    /* finally truncate the memptr so that its size is correct */
    SET-SIZE(mFontSubset) = 0. /* just to be sure ;) */
    SET-SIZE(mFontSubset) = iFontLength.
    ASSIGN
     iNumSteps = TRUNCATE(iFontLength / 30000, 0)
     iOffset = 1.
    DO i = 1 TO iNumSteps:
        PUT-BYTES(mFontSubset, iOffset) = GET-BYTES(mFontSubsetTmp, iOffset, 30000).
        iOffset = iOffset + 30000.
    END.
    IF iFontLength > iOffset THEN
        PUT-BYTES(mFontSubset, iOffset) = GET-BYTES(mFontSubsetTmp, iOffset, iFontLength - iOffset + 1).
    SET-SIZE(mFontSubsetTmp) = 0.

    /* font subset prefix */
    /* (in section 9.6.4, PDF 1.7): For a font subset, the PostScript name of the font—the value of the  */
    /* font's BaseFont entry and the font descriptor's FontName entry— shall begin with a tag followed   */
    /* by a plus sign (+). The tag shall consist of exactly six uppercase letters; the choice of letters */
    /* is arbitrary, but different subsets in the same PDF file shall have different tags.               */
    pSubsetPrefix = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
        STRING(iSubsetPrefix, "999999"), "0", "A"), "1", "B"), "2", "C"), "3", "D"), "4", "E"), "5", "F"), "6", "G"), "7", "H"), "8", "I"), "9", "J")
        + "+".

    /* clean-up */
    SET-SIZE(mFont) = 0.
    EMPTY TEMP-TABLE tt_pdf_font_table.
    EMPTY TEMP-TABLE tt_pdf_font_index_to_loc.

END PROCEDURE. /* buildFontSubset */

/* 03-JUL-2014 jcc: new */
PROCEDURE pdf_subset_add_string: /* flag the string characters as used (for subsetting) */
    DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcFontName AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcString   AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plUnicode  AS LOGICAL     NO-UNDO.

    DEFINE BUFFER TT_pdf_font_character FOR TT_pdf_font_character.

    DEFINE VARIABLE mString AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE iSize AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.

    IF plUnicode THEN DO:
        RUN utf8_to_utf16be (pdfStream, pcString, "raw", ?, OUTPUT mString).
        iSize = GET-SIZE(mString).
        DO iChar = 1 TO iSize - 1 BY 2:
            FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
                AND TT_pdf_font_character.font_name = pcFontName
                AND TT_pdf_font_character.char_id = GET-UNSIGNED-SHORT(mString, iChar) NO-ERROR.
            IF AVAILABLE TT_pdf_font_character THEN
                TT_pdf_font_character.used = YES.
        END.
        SET-SIZE(mString) = 0.
    END.
    ELSE DO:
        iSize = LENGTH(pcString, "raw").
        SET-SIZE(mString) = iSize + 1.
        PUT-STRING(mString, 1) = pcString.
        DO iChar = 1 TO iSize:
            FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
                AND TT_pdf_font_character.font_name = pcFontName
                AND TT_pdf_font_character.char_id = GET-BYTE(mString, iChar) NO-ERROR.
            IF AVAILABLE TT_pdf_font_character THEN
                TT_pdf_font_character.used = YES.
        END.
        SET-SIZE(mString) = 0.
    END.
END PROCEDURE.

PROCEDURE pdf_subset_add_range: /* flag a range of characters as used (for subsetting) */
    DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcFontName AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piFrom     AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER piTo       AS INTEGER     NO-UNDO.

    DEFINE BUFFER TT_pdf_font_character FOR TT_pdf_font_character.

    DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.

    DO iChar = piFrom TO piTo:
        FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = pdfStream
            AND TT_pdf_font_character.font_name = pcFontName
            AND TT_pdf_font_character.char_id = iChar NO-ERROR.
        IF AVAILABLE TT_pdf_font_character THEN
            TT_pdf_font_character.used = YES.
    END.
END.
/* 03-JUL-2014 jcc: end */

/* 12-MAR-2014 jcc: new */
PROCEDURE _pdf_load_fonts_flushW: /* PRIVATE */
    DEFINE INPUT-OUTPUT PARAMETER iNumBuffer    AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER iPrevCharId   AS INTEGER     EXTENT 3 NO-UNDO.
    DEFINE INPUT        PARAMETER cPrevWidth    AS CHARACTER   EXTENT 3 NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER cSep          AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER cArrayClose   AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.

    IF cArrayClose = "]" THEN DO:
        DO i = 1 TO iNumBuffer:
            PUT STREAM S_pdf_inc UNFORMATTED cSep cPrevWidth[i].
            cSep = " ".
        END.
        PUT STREAM S_pdf_inc UNFORMATTED "]".
    END.
    ELSE DO:
        PUT STREAM S_pdf_inc UNFORMATTED iPrevCharId[1] "[".
        DO i = 1 TO iNumBuffer:
            PUT STREAM S_pdf_inc UNFORMATTED cSep cPrevWidth[i].
            cSep = " ".
        END.
        PUT STREAM S_pdf_inc UNFORMATTED "]".
    END.
    ASSIGN
     iNumBuffer  = 0
     cArrayClose = ""
     cSep        = "".
END PROCEDURE.

PROCEDURE pdf_load_fonts : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE cArrayClose   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPrevWidth    AS CHARACTER   EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cSep          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSubsetPrefix AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNumBuffer    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPrevChar     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPrevCharId   AS INTEGER     EXTENT 3 NO-UNDO.
  DEFINE VARIABLE iRetCode      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lWFormat2     AS LOGICAL     NO-UNDO. /* 26-JAN-2017 jcc */
  DEFINE VARIABLE L_count       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_data        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_end         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_Size        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_start       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE mFontSubset   AS MEMPTR      NO-UNDO.
  DEFINE VARIABLE m_EncryptKey  AS MEMPTR      NO-UNDO.

  DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

  FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                         AND TT_pdf_font.font_file <> "PDFBASE14"
                         AND TT_pdf_font.font_File <> "EXTERNAL"
      USE-INDEX obj_stream:

    /* 24-FEB-2014 jcc: font subsetting */
    cSubsetPrefix = "".
    IF TT_pdf_font.font_subset THEN
        RUN buildFontSubset (pdfStream, BUFFER TT_pdf_font, OUTPUT mFontSubset, OUTPUT cSubsetPrefix).

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontDescriptor", ?, 0, "").
    TT_pdf_font.font_descr  = pdf_inc_ObjectSequence.

    /* 21-OCT-2014 jcc: optimization not to create a new /Encoding when reusing 4 0 R is sufficient. */
    DEFINE VARIABLE lWeHaveDiff                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lWeNeedOurOwnEncodingObject AS LOGICAL   NO-UNDO.
    lWeHaveDiff = TT_pdf_font.font_dif <> ""
        OR CAN-FIND(FIRST TT_pdf_diff
                    WHERE TT_pdf_diff.obj_stream = pdfStream
                    AND TT_pdf_diff.font_name  = TT_pdf_font.font_name).
    lWeNeedOurOwnEncodingObject = lWeHaveDiff
        OR CAN-FIND(FIRST TT_pdf_diff
                    WHERE TT_pdf_diff.obj_stream = pdfStream
                    AND TT_pdf_diff.font_name  = "Base14WinAnsiEncoding").
                  
    /* Output the Font Descriptor */
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_font.font_descr " 0 obj" {&pdfSKIP}
        "<< /Type /FontDescriptor" {&pdfSKIP}
        "   /Ascent " TT_pdf_font.afm_Ascender {&pdfSKIP}
        "   /Descent " TT_pdf_font.afm_Descender {&pdfSKIP}
        "   /CapHeight " TT_pdf_font.afm_Ascender {&pdfSKIP}
        "   /Flags " TT_pdf_font.afm_Flags {&pdfSKIP}
        "   /FontBBox [" TT_pdf_font.afm_FontBBox "]" {&pdfSKIP}
        /* "   /FontName /" cSubsetPrefix + IF TT_pdf_font.is_unicode THEN TT_pdf_font.afm_FontName ELSE TT_pdf_font.font_name {&pdfSKIP} */
        "   /FontName /" cSubsetPrefix + TT_pdf_font.font_name {&pdfSKIP}
        "   /ItalicAngle " TT_pdf_font.afm_ItalicAngle  {&pdfSKIP}.

    IF TT_pdf_font.font_embed THEN
      PUT STREAM S_pdf_inc UNFORMATTED
        "   /FontFile2 " (TT_pdf_font.font_descr + (IF TT_pdf_font.is_unicode THEN 5 ELSE 2 + IF lWeNeedOurOwnEncodingObject THEN 1 ELSE 0)) " 0 R" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    /* 5-FEB-2014 jcc: manage unicode fonts */
    IF TT_pdf_font.is_unicode THEN DO:
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Font", ?, 0, "").
        TT_pdf_font.font_obj    = pdf_inc_ObjectSequence.

        PUT STREAM S_pdf_inc UNFORMATTED
            TT_pdf_font.font_obj " 0 obj" {&pdfSKIP}
            "<<" {&pdfSKIP}
            "/Type /Font" {&pdfSKIP}
            "/Subtype /Type0" {&pdfSKIP}
            /* "/BaseFont /" cSubsetPrefix + TT_pdf_font.afm_FontName {&pdfSKIP} */
            "/BaseFont /" cSubsetPrefix + TT_pdf_font.font_name {&pdfSKIP}
            "/Name /" TT_pdf_font.font_name {&pdfSKIP}
            "/Encoding /Identity-H" {&pdfSKIP}
            "/DescendantFonts [" (pdf_inc_ObjectSequence + 1) " 0 R]" {&pdfSKIP}
            "/ToUnicode " (pdf_inc_ObjectSequence + 2) " 0 R" {&pdfSKIP}
            ">>" {&pdfSKIP}
            "endobj" {&pdfSKIP}.

        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "DescendantFont", ?, 0, "").
        PUT STREAM S_pdf_inc UNFORMATTED
            pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
            "<<" {&pdfSKIP}
            "/Type /Font" {&pdfSKIP}
            "/Subtype /CIDFontType2" {&pdfSKIP}
            /* "/BaseFont /" cSubsetPrefix + TT_pdf_font.afm_FontName {&pdfSKIP} */
            "/BaseFont /" cSubsetPrefix + TT_pdf_font.font_name {&pdfSKIP}
            "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >>" {&pdfSKIP}
            "/FontDescriptor " TT_pdf_font.font_descr " 0 R" {&pdfSKIP}
            IF TT_pdf_font.afm_MissingWidth > "" THEN "/DW " + TT_pdf_font.afm_MissingWidth + "~n" ELSE ""
            IF TT_pdf_font.font_embed THEN "/CIDToGIDMap " + STRING(pdf_inc_ObjectSequence + 2) + " 0 R~n" ELSE "".

        /* 06-MAR-2014 jcc: output optimized version of /W */
        ASSIGN
            cArrayClose = ""
            lWFormat2   = NO.
        IF CAN-FIND(FIRST TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = TT_pdf_font.obj_stream
                    AND TT_pdf_font_character.font_name = TT_pdf_font.font_name) THEN DO:
            PUT STREAM S_pdf_inc UNFORMATTED "/W [".
            iNumBuffer = 0.
            blkChars:
            REPEAT PRESELECT EACH TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = TT_pdf_font.obj_stream
                AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                AND TT_pdf_font_character.char_id > 0
                AND (TT_pdf_font_character.used = YES OR NOT TT_pdf_font.font_subset) /* 03-JUL-2014 jcc: only output the widths of the characters we used */
                AND TT_pdf_font_character.char_width <> TT_pdf_font.afm_MissingWidth /* 18-SEP-2014 jcc: do not consider chars with the default width (/DW) */
                BY TT_pdf_font_character.char_id:

                FIND NEXT TT_pdf_font_character. /* first character */
                iPrevChar = TT_pdf_font_character.char_id - 1.

                DO WHILE TRUE:
                    /* if the characters are not contiguous, then flush */
                    IF iPrevChar + 1 <> TT_pdf_font_character.char_id AND iNumBuffer > 0 THEN
                        RUN _pdf_load_fonts_flushW(INPUT-OUTPUT iNumBuffer, iPrevCharId, cPrevWidth, INPUT-OUTPUT cSep, INPUT-OUTPUT cArrayClose).

                    IF iNumBuffer < 3 THEN DO: /* read the 3 first characters */
                        iNumBuffer = iNumBuffer + 1.
                        ASSIGN
                         iPrevCharId[iNumBuffer] = TT_pdf_font_character.char_id
                         cPrevWidth[iNumBuffer]  = TT_pdf_font_character.char_width.
                    END.
                    ELSE DO: /* iNumBuffer = 3 */
                        /* 4 consecutive widths are equal: format "start end width", else "start [width width...]" */
                        IF   cPrevWidth[1] = cPrevWidth[2]
                         AND cPrevWidth[2] = cPrevWidth[3]
                         AND cPrevWidth[3] = TT_pdf_font_character.char_width THEN DO:
                            lWFormat2 = YES.
                            /* find all the following successive characters with the same width */
                            DO WHILE iPrevChar + 1 = TT_pdf_font_character.char_id
                                     AND TT_pdf_font_character.char_width = cPrevWidth[2]:
                                iPrevChar = TT_pdf_font_character.char_id.
                                FIND NEXT TT_pdf_font_character NO-ERROR.
                                IF NOT AVAILABLE TT_pdf_font_character THEN LEAVE blkChars.
                            END.
                            FIND PREV TT_pdf_font_character.
                            /* flush */
                            PUT STREAM S_pdf_inc UNFORMATTED cArrayClose iPrevCharId[1] " " TT_pdf_font_character.char_id " " cPrevWidth[2] " ".
                            ASSIGN
                             iNumBuffer  = 0
                             cArrayClose = ""
                             cSep        = "".
                             lWFormat2   = NO.
                        END.
                        /* not equal: flush the 1st char in the buffer and shift the buffer */
                        ELSE DO:
                            IF cArrayClose = "]" THEN
                                PUT STREAM S_pdf_inc UNFORMATTED cSep cPrevWidth[1].
                            ELSE DO:
                                PUT STREAM S_pdf_inc UNFORMATTED iPrevCharId[1] "[" cPrevWidth[1].
                                ASSIGN
                                 cArrayClose = "]"
                                 cSep        = " ".
                            END.
                            ASSIGN
                             iPrevCharId[1] = iPrevCharId[2]
                             cPrevWidth[1]  = cPrevWidth[2]
                             iPrevCharId[2] = iPrevCharId[3]
                             cPrevWidth[2]  = cPrevWidth[3]
                             iPrevCharId[3] = TT_pdf_font_character.char_id
                             cPrevWidth[3]  = TT_pdf_font_character.char_width.
                        END.
                    END.

                    iPrevChar = TT_pdf_font_character.char_id.
                    FIND NEXT TT_pdf_font_character NO-ERROR.
                    IF NOT AVAILABLE TT_pdf_font_character THEN LEAVE blkChars.

                END. /* DO WHILE TRUE */
            END. /* blkChars: REPEAT PRESELECT EACH TT_pdf_font_character */
            /* Flush remaining characters */
            IF iNumBuffer > 0 THEN DO:
                IF lWFormat2 THEN /* 26-JAN-2017 jcc: fixed: when the very last block is in format 2 it was incorrectly flushed as format 1 */
                    PUT STREAM S_pdf_inc UNFORMATTED cArrayClose iPrevCharId[1] " " iPrevChar " " cPrevWidth[2] " ".
                ELSE
                    RUN _pdf_load_fonts_flushW(INPUT-OUTPUT iNumBuffer, iPrevCharId, cPrevWidth, INPUT-OUTPUT cSep, INPUT-OUTPUT cArrayClose).
            END.

            PUT STREAM S_pdf_inc UNFORMATTED "]" {&pdfSKIP}.
        END. /* /W */

        PUT STREAM S_pdf_inc UNFORMATTED
            ">>" {&pdfSKIP}
            "endobj" {&pdfSKIP}.

        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ToUnicode", ?, 0, "").
        RUN putStringAsStream(pdfStream,
              "/CIDInit /ProcSet findresource begin~n"
            + "12 dict begin~n"
            + "begincmap~n"
            + "/CIDSystemInfo <</Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def~n"
            + "/CMapName /Adobe-Identity-UCS def~n"
            + "/CMapType 2 def~n"
            + "1 begincodespacerange~n"
            + "<0000> <FFFF>~n"
            + "endcodespacerange~n"
            + "1 beginbfrange~n"
            + "<0000> <FFFF> <0000>~n"
            + "endbfrange~n"
            + "endcmap~n"
            + "CMapName currentdict /CMap defineresource pop~n"
            + "end~n"
            + "end~n", pdf_inc_ObjectSequence, "", "", pdfEncrypt, YES, YES).

        IF TT_pdf_font.font_embed THEN DO:
            ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "CIDToGIDMap", ?, 0, "").
            RUN putFileAsStream(pdfStream, TT_pdf_font.cid2gid_file, pdf_inc_ObjectSequence, "", "", pdfEncrypt, YES, YES).
        END.
    END. /* IF TT_pdf_font.is_unicode */
    ELSE DO:
        /* 21-OCT-2014 jcc: only create the encoding if we have differences. Else use 4 0 R. */
        TT_pdf_font.font_encoding = IF lWeNeedOurOwnEncodingObject THEN pdf_inc_ObjectSequence + 2 ELSE 4.

        /* igc - Added Sept 10, 2002 */
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Font", ?, 0, "").
        TT_pdf_font.font_obj    = pdf_inc_ObjectSequence.

        PUT STREAM S_pdf_inc UNFORMATTED
            TT_pdf_font.font_obj " 0 obj" {&pdfSKIP}
            "<<" {&pdfSKIP}
            "/Type /Font" {&pdfSKIP}
            "/Subtype /TrueType" {&pdfSKIP}
            "/FirstChar " TT_pdf_font.afm_FirstChar {&pdfSKIP}
            "/LastChar " TT_pdf_font.afm_LastChar {&pdfSKIP}
            "/Widths [ " TT_pdf_font.afm_widths " ]" {&pdfSKIP}
            "/Encoding " TT_pdf_font.font_encoding " 0 R" {&pdfSKIP}
            "/BaseFont /" cSubsetPrefix + TT_pdf_font.font_name {&pdfSKIP}
            "/FontDescriptor " TT_pdf_font.font_descr " 0 R" {&pdfSKIP}
            ">>" {&pdfSKIP}
            "endobj" {&pdfSKIP}.

        /* igc - Aug 28 - Added this code to allow for remapping of characters */
        IF lWeNeedOurOwnEncodingObject THEN DO: /* 21-OCT-2014 jcc: moved everything /Encoding related within this IF */
          ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Encoding", ?, 0, "").

          PUT STREAM S_pdf_inc UNFORMATTED
              TT_pdf_font.font_encoding " 0 obj" {&pdfSKIP}
              "<< /Type /Encoding" {&pdfSKIP}
              "/BaseEncoding /WinAnsiEncoding" {&pdfSKIP}.

          IF lWeHaveDiff THEN DO:
              PUT STREAM S_pdf_inc UNFORMATTED "/Differences [ " {&pdfSKIP}.

              IF TT_pdf_font.font_dif <> "" THEN DO:
                INPUT STREAM S_pdf_inp FROM VALUE(SEARCH(TT_pdf_font.font_dif)) BINARY NO-CONVERT NO-MAP NO-ECHO.

                  REPEAT:
                    IMPORT STREAM S_pdf_inp UNFORMATTED L_data.
                    PUT STREAM S_pdf_inc UNFORMATTED
                        L_Data {&pdfSKIP}.
                  END.

                INPUT STREAM S_pdf_inp CLOSE.
              END. /* Differences File */

              FOR EACH TT_pdf_diff WHERE TT_pdf_diff.obj_stream = pdfStream
                  AND TT_pdf_diff.font_name = TT_pdf_font.font_name:
                PUT STREAM S_pdf_inc UNFORMATTED
                    TT_pdf_diff.char_num
                    " "
                    TT_pdf_diff.PS_name {&pdfSKIP}.
              END.

              PUT STREAM S_pdf_inc UNFORMATTED "]". /* Close the Differences Array */
          END.

          PUT STREAM S_pdf_inc UNFORMATTED
              ">>" {&pdfSKIP}
              "endobj" {&pdfSKIP}.

        END. /* Font Difference File exists */
        /* igc - Aug 28 - end of character mapping code */
    END. /* IF TT_pdf_font.is_unicode, ELSE */

    IF TT_pdf_font.font_embed THEN DO:
      /* igc - Added Sept 10, 2002 */
      ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontStream", ?, 0, "").
      TT_pdf_font.font_stream = pdf_inc_ObjectSequence.
      IF TT_pdf_font.font_subset THEN DO:
          RUN putMemptrAsStream(pdfStream, mFontSubset, pdf_inc_ObjectSequence, "/Length1 " + STRING(GET-SIZE(mFontSubset)) , "", pdfEncrypt, YES, YES).
          SET-SIZE(mFontSubset) = 0.
      END. ELSE DO:
          FILE-INFO:FILE-NAME = TT_pdf_font.font_file.
          RUN putFileAsStream(pdfStream, TT_pdf_font.font_file, pdf_inc_ObjectSequence, "/Length1 " + STRING(FILE-INFO:FILE-SIZE), "", pdfEncrypt, YES, YES).
      END.
    END. /* Embed */    
  END. /* each TTfont */
END. /* pdf_Load_fonts */

PROCEDURE pdf_ParseAFMFile: /* PRIVATE */
  DEFINE PARAMETER BUFFER TT_pdf_font FOR TT_pdf_font. /* 30-JAN-2014 jcc: replace many OUTPUT params by one BUFFER parameter */

  DEFINE VARIABLE L_data  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_key   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_flag  AS CHARACTER INITIAL "00000000000000000000000000100010" NO-UNDO.
  /* Bit 6 (above) is set to identify NonSymbolic Fonts -- or Fonts that use
     the Standard Latin Character Set */

  DEFINE VARIABLE L_int   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_exp   AS INTEGER NO-UNDO.

  DEFINE VARIABLE mCid2Gid AS MEMPTR NO-UNDO.

  /* 11-FEB-2014 jcc: ensure the glyph bytes are ordered correctly in the cid to gid map, see PUT-SHORT below */
  SET-BYTE-ORDER(mCid2Gid) = BIG-ENDIAN.

  IF TT_pdf_font.is_unicode THEN DO:
    SET-SIZE(mCid2Gid) = 131072. /* 256 * 256 * 2 */
    L_flag = "00000000000000000000000000000110". /* PDF32000 2008: any font whose character set is not a subset of the Adobe standard character set shall be considered to be symbolic */
  END.

  ASSIGN TT_pdf_font.afm_ItalicAngle  = "0"
         TT_pdf_font.afm_Descender    = ""
         TT_pdf_font.afm_FontBBox     = ""
         TT_pdf_font.afm_IsFixedPitch = ""
         TT_pdf_font.afm_FirstChar    = ""
         TT_pdf_font.afm_LastChar     = ""
         TT_pdf_font.afm_Widths       = "".

  INPUT STREAM S_pdf_inp FROM VALUE(SEARCH(TT_pdf_font.font_afm)) BINARY NO-CONVERT NO-MAP NO-ECHO.

    REPEAT:
      IMPORT STREAM S_pdf_inp UNFORMATTED L_data.
      L_Key = ENTRY(1, L_data, " ") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.

      CASE L_key:
        WHEN "ItalicAngle" THEN
          TT_pdf_font.afm_ItalicAngle = dec2string(string2dec(ENTRY( 2, L_data, " "))) NO-ERROR.

        WHEN "Ascender" THEN
          TT_pdf_font.afm_Ascender = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "Descender" THEN
          TT_pdf_font.afm_Descender = ENTRY( 2, L_data, " ") .

        WHEN "FontBBox" THEN
          TT_pdf_font.afm_FontBBox = REPLACE(L_data,"FontBBox ","").

        WHEN "IsFixedPitch" THEN
          TT_pdf_font.afm_IsFixedPitch = IF ENTRY(2,L_data, " ") = "True" THEN "0" ELSE "1".

        /* 30-JAN-2014 jcc: added the next keys */
        WHEN "FontName" THEN
          TT_pdf_font.afm_FontName = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "FullName" THEN
          TT_pdf_font.afm_FullName = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "FamilyName" THEN
          TT_pdf_font.afm_FamilyName = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "Weight" THEN
          TT_pdf_font.afm_Weight = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "Characters" THEN
          TT_pdf_font.afm_Characters = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "UnderlineThickness" THEN
          TT_pdf_font.afm_UnderlineThickness = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "UnderlinePosition" THEN
          TT_pdf_font.afm_UnderlinePosition = ENTRY( 2, L_data, " ") NO-ERROR.

        /* C 45 ; WX 333 ; N hyphen ; B 44 232 289 322 ; */
        WHEN "C" THEN DO:
          IF TT_pdf_font.afm_FirstChar = "" THEN
            TT_pdf_font.afm_FirstChar = ENTRY(2, L_data, " ") NO-ERROR.

          ASSIGN TT_pdf_font.afm_Widths = TT_pdf_font.afm_widths + " "
                                        + ENTRY(5, L_data, " ") NO-ERROR.

          IF INT(ENTRY(2, L_data, " ")) > 0 THEN
            TT_pdf_font.afm_LastChar = ENTRY(2, L_data, " ") NO-ERROR.
        END.

        /* 29-JAN-2014 jcc: UFM files - for unicode fonts */
        /* U 32 ; WX 278 ; N space ; G 3 ; B 176 0 399 1466 ; */
        WHEN "U" THEN DO:
          DEFINE VARIABLE cName    AS CHARACTER   NO-UNDO.
          DEFINE VARIABLE cWidth   AS CHARACTER   NO-UNDO.
          DEFINE VARIABLE iChar    AS INTEGER     NO-UNDO.
          DEFINE VARIABLE iGlyph   AS INTEGER     NO-UNDO.

          IF NOT TT_pdf_font.is_unicode THEN NEXT. /* should never happen as "U" entries are only seen in ufm files (unicode) */

          iChar  = INTEGER(ENTRY(2, L_data, " ")) NO-ERROR.
          iGlyph = INTEGER(ENTRY(11, L_data, " ")) NO-ERROR.
          cName  = ENTRY(8, L_data, " ") NO-ERROR.
          cWidth = ENTRY(5, L_data, " ") NO-ERROR.

          IF TT_pdf_font.afm_FirstChar = "" THEN
              TT_pdf_font.afm_FirstChar = STRING(iChar) NO-ERROR.

          /* build CID to GID mapping */
          IF iChar >= 0 AND iChar <= 65535 /* 0xFFFF */ AND iGlyph <> 0 THEN DO:
              PUT-SHORT(mCid2Gid, iChar * 2 + 1) = iGlyph.
              /* also save the font width to output it later in the font object */
              IF iChar > 0 THEN DO:
                  /* 07-FEB-2014 jcc: cannot use TT_pdf_font.afm_Widths to concatenate all the widths;
                     must use a temp-table instead because this sometimes is > 32kb */
                  FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = TT_pdf_font.obj_stream
                      AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                      AND TT_pdf_font_character.char_id   = iChar NO-ERROR.
                  IF NOT AVAILABLE TT_pdf_font_character THEN DO:
                      CREATE TT_pdf_font_character.
                      ASSIGN
                       TT_pdf_font_character.obj_stream = TT_pdf_font.obj_stream
                       TT_pdf_font_character.font_name  = TT_pdf_font.font_name
                       TT_pdf_font_character.char_id    = iChar
                       TT_pdf_font_character.glyph      = iGlyph.
                  END.
                  TT_pdf_font_character.char_width = cWidth.
              END.
          END.
          ELSE IF iChar < 0 THEN DO: /* 19-OCT-2014 jcc: load all the glyphs, event if U = -1. This is needed for some composite glyphs when subsetting the font */
              DEFINE VARIABLE iDummyChar AS INTEGER  INITIAL -2   NO-UNDO.
              
              FIND TT_pdf_font_character WHERE TT_pdf_font_character.obj_stream = TT_pdf_font.obj_stream
                  AND TT_pdf_font_character.font_name = TT_pdf_font.font_name
                  AND TT_pdf_font_character.char_id   = iChar NO-ERROR.
              IF NOT AVAILABLE TT_pdf_font_character THEN DO:
                  CREATE TT_pdf_font_character.
                  ASSIGN
                   TT_pdf_font_character.obj_stream = TT_pdf_font.obj_stream
                   TT_pdf_font_character.font_name  = TT_pdf_font.font_name
                   TT_pdf_font_character.char_id    = iDummyChar
                   TT_pdf_font_character.glyph      = iGlyph.
                  iDummyChar = iDummyChar - 1.
              END.
              TT_pdf_font_character.char_width = cWidth.
          END.

          IF TT_pdf_font.afm_MissingWidth = "" AND iChar = -1 AND cName = ".notdef" THEN
              TT_pdf_font.afm_MissingWidth = cWidth.

          IF iChar > 0 THEN
              TT_pdf_font.afm_LastChar = STRING(iChar) NO-ERROR.

        END. /* WHEN "U" */

        END CASE.
    END. /* REPEAT */

  INPUT STREAM S_pdf_inp CLOSE.

  /* IF TT_pdf_font.is_unicode THEN */
    /* TT_pdf_font.afm_Widths = SUBSTRING(TT_pdf_font.afm_Widths, 2) + "]". */

  /* Determine Font Flags */
  IF TT_pdf_font.afm_IsFixedPitch = "0" THEN
      OVERLAY(L_Flag, 32, 1, "CHARACTER") = "1".

  DO L_loop = LENGTH(L_Flag, "character":u) TO 1 BY -1 :
      IF SUBSTR(L_flag, L_loop, 1, "character":u) = "1" THEN
          L_int = L_int + EXP(2, L_exp).
      L_exp = L_exp + 1.
  END.

  TT_pdf_font.afm_Flags = STRING(L_int).

  /* 5-FEB-2014 jcc: create CidToGid stream */
  IF TT_pdf_font.is_unicode THEN DO:
      TT_pdf_font.cid2gid_file = SESSION:TEMP-DIRECTORY + TT_pdf_font.afm_FontName + ".cid2gid".
      OUTPUT TO VALUE(TT_pdf_font.cid2gid_file) BINARY NO-MAP NO-CONVERT NO-ECHO.
      EXPORT mCid2Gid.
      OUTPUT CLOSE.
      SET-SIZE(mCid2Gid) = 0.
  END.

END. /* pdf_ParseAFMFile */

PROCEDURE pdf_load_images: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE cImageFile     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iRetCode       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lCompress      AS LOGICAL     NO-UNDO.

  FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream:

    lCompress = FALSE.
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE"
        AND TT_pdf_image.image_filter <> "/FlateDecode"
        AND TT_pdf_image.image_filter <> "/LZWDecode" THEN DO:
      lCompress = TRUE.
      iRetCode = compressfile( TT_pdf_image.image_data /*TT_pdf_image.image_file*/,
                               SESSION:TEMP-DIR + TT_pdf_image.image_name + ".cmp").
      IF iRetCode = 0 THEN
          cImageFile = SESSION:TEMP-DIR + TT_pdf_image.image_name + ".cmp".
      ELSE DO:
          {pdferror.i &msg="'Zlib error: ' + STRING(iRetCode)" &return=NO}. /* continue without compression */
          cImageFile = TT_pdf_image.image_data.
      END.
    END.
    ELSE
      cImageFile = TT_pdf_image.image_data.
      /*FILE-INFO:FILE-NAME = TT_pdf_image.image_file.*/

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Image", ?, 0, "").
    TT_pdf_image.image_obj = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_image.image_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /XObject" {&pdfSKIP}
        "/Subtype /Image" {&pdfSKIP}
        "/Name " TT_pdf_image.image_tag {&pdfSKIP}
        "/Width " TT_pdf_image.image_w {&pdfSKIP}
        "/Height " TT_pdf_image.image_h {&pdfSKIP}.
    
    /* 09-AUG-2011 jcc: take into account new info to output jpeg but also png images */
    PUT STREAM S_pdf_inc UNFORMATTED
        "/BitsPerComponent " TT_pdf_image.image_bpc {&pdfSKIP}.
    PUT STREAM S_pdf_inc UNFORMATTED
            "/ColorSpace ".
    IF TT_pdf_image.image_cs = "~/Indexed" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            "[/Indexed " TT_pdf_image.image_alt_cs " " TT_pdf_image.image_pal_length / 3 - 1 " " TT_pdf_image.image_obj + 1 " 0 R]" {&pdfSKIP}.
    ELSE IF TT_pdf_image.image_cs = "~/DeviceN" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            "[/DeviceN[" TT_pdf_image.image_devn_comp "]" TT_pdf_image.image_alt_cs " " TT_pdf_image.image_obj + 1 " 0 R]" {&pdfSKIP}.
    ELSE DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            TT_pdf_image.image_cs {&pdfSKIP}.
        /* 25-NOV-2014 jcc: /Decode is already in the image_params */
        /* IF TT_pdf_image.image_cs = "~/DeviceCMYK" THEN */
            /* PUT STREAM S_pdf_inc UNFORMATTED */
                /* "/Decode [1 0 1 0 1 0 1 0]" {&pdfSKIP}. */
    END.
    IF TT_pdf_image.image_filter > "" THEN
        IF lCompress THEN
            PUT STREAM S_pdf_inc UNFORMATTED
                "/Filter [/FlateDecode " TT_pdf_image.image_filter "]" {&pdfSKIP}.
        ELSE
            PUT STREAM S_pdf_inc UNFORMATTED
                "/Filter " TT_pdf_image.image_filter {&pdfSKIP}.
    IF TT_pdf_image.image_params > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            TT_pdf_image.image_params {&pdfSKIP}.
    IF TT_pdf_image.image_transparency > "" THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Mask [" TT_pdf_image.image_transparency "]" {&pdfSKIP}.
    END.
    IF TT_pdf_image.image_has_smask THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/SMask " TT_pdf_image.image_obj + 1 " 0 R" {&pdfSKIP}.
    END.
    /* 09-AUG-2011 jcc: end */

    /* 14-OCT-2015 jcc: refactor: remove a lot of code, replaced by the following line: */
    RUN putFileAsStream (pdfStream, cImageFile, TT_pdf_image.image_obj, "", "", pdfEncrypt, NO, NO).

    /* 09-AUG-2011 jcc: put the palette object for indexed images */
    IF TT_pdf_image.image_cs = "/Indexed" THEN DO:
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Palette", ?, 0, "").
        RUN putFileAsStream (pdfStream, TT_pdf_image.image_palette, pdf_inc_ObjectSequence, "", "", pdfEncrypt, YES, YES).
    END.
    /* 09-AUG-2011 jcc: end */

    /* 06-NOV-2014 jcc: put the [A]BGR => RGB transformation function for /DeviceN images (BMP) */
    ELSE IF TT_pdf_image.image_cs = "/DeviceN" THEN DO:
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Function", ?, 0, "").
        RUN putStringAsStream (pdfStream, TT_pdf_image.image_func_ps, pdf_inc_ObjectSequence,
                               "/FunctionType 4" + TT_pdf_image.image_func_params, "", pdfEncrypt, YES, YES).
    END.

    /* Remove Compressed File, if any */
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      OS-DELETE VALUE(SESSION:TEMP-DIR + TT_pdf_image.image_name + ".cmp").

  END. /* each TT_pdf_image */

END. /* pdf_load_images */

PROCEDURE putResources: /* PRIVATE */
    DEFINE INPUT PARAMETER pcType    AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piPageId  AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfID     AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cResList AS CHARACTER  NO-UNDO.

    DEFINE BUFFER TT_Resource FOR TT_Resource.

    IF CAN-FIND(FIRST TT_Resource
               WHERE TT_Resource.obj_stream = pdfStream
                  AND TT_Resource.pdf_id     = pdfID
                  AND TT_Resource.res_type   = pcType
                  AND (TT_Resource.page_id   = piPageId
                    OR TT_Resource.page_id   = 0) /* 28-FEB-2011 jcc: added page 0 resources */
                  AND TT_Resource.new_obj   <> 0) THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "~/" pcType " <<".
        FOR EACH TT_Resource
            WHERE TT_Resource.obj_stream = pdfStream
              AND TT_Resource.pdf_id     = pdfID
              AND TT_Resource.res_type   = pcType
              AND (TT_Resource.page_id   = piPageId
                OR TT_Resource.page_id   = 0) /* 28-FEB-2011 jcc: added page 0 resources */
              AND TT_Resource.new_obj   <> 0:
            IF LOOKUP(TT_Resource.res_text, cResList) = 0 THEN DO:
                PUT STREAM S_pdf_inc UNFORMATTED
                    " " TT_Resource.res_text " " TT_Resource.new_obj " " TT_Resource.new_gen " R".
                cResList = cResList + "," + TT_Resource.res_text.
            END.
        END.
        PUT STREAM S_pdf_inc UNFORMATTED
            " >>" {&pdfSKIP}.
    END.
END PROCEDURE. /* putResources */

PROCEDURE pdf_load_external: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE cFontList    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_fontobj    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vDestLen     AS INTEGER     NO-UNDO.

  DEFINE BUFFER TT_pdf_external     FOR TT_pdf_external.
  DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.
  DEFINE BUFFER TT_pdf_font         FOR TT_pdf_font.

  RUN LoadExternalFonts (pdfStream, pdfEncrypt).
  /* 14-NOV-2014 jcc: refactoring */
  /* RUN LoadExtGStates (pdfStream, pdfEncrypt). */
  /* RUN LoadColorSpaces (pdfStream, pdfEncrypt). */
  /* RUN LoadShading (pdfStream, pdfEncrypt). */
  /* RUN LoadExternalXObjects (pdfStream, pdfEncrypt). */
  /* RUN LoadPatterns (pdfStream, pdfEncrypt). */
  /* RUN LoadProperties (pdfStream, pdfEncrypt). */
  RUN LoadResources(pdfStream, pdfEncrypt, "ExtGState").
  RUN LoadResources(pdfStream, pdfEncrypt, "ColorSpace").
  RUN LoadResources(pdfStream, pdfEncrypt, "Shading").
  RUN LoadResources(pdfStream, pdfEncrypt, "XObject").
  RUN LoadResources(pdfStream, pdfEncrypt, "Pattern").
  RUN LoadResources(pdfStream, pdfEncrypt, "Properties").

  FOR EACH TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStream,
      /* 03-DEC-2014 jcc: only export used external pages: */
      FIRST tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
        AND tt_pdf_page_use_ext.pdf_id_use = TT_pdf_external.pdf_id /* 02-DEC-2014 jcc */
        AND tt_pdf_page_use_ext.page_use   = tt_pdf_external.ext_page:

    ASSIGN
        cFontList = ""
        L_FontObj = "".
    FOR EACH TT_pdf_font 
       WHERE TT_pdf_font.obj_stream = pdfStream 
         AND TT_pdf_font.pdf_id     = TT_pdf_external.pdf_id
         AND (   TT_pdf_font.ext_page = TT_pdf_external.ext_page
              OR TT_pdf_font.ext_page = 0 AND TT_pdf_font.font_file = "EXTERNAL" ) /* 17-JAN-2011 jcc: the page also uses the common resources (page = 0) */
         AND TT_pdf_font.font_pitch <> "EXTERNAL" 
         AND TT_pdf_font.font_obj <> 0
       BY TT_pdf_font.font_tag:
        IF LOOKUP(TT_pdf_font.font_tag, cFontList) = 0 THEN
            ASSIGN
                L_fontobj = L_fontobj + " " + TT_pdf_font.font_tag + " "
                            + STRING(TT_pdf_font.font_obj) + " 0 R"
                cFontList = cFontList + "," + TT_pdf_font.font_tag.
    END.

    /* 23-JAN-2011 jcc: check that the file exists, else it could crash the whole session */
    IF SEARCH(TT_pdf_external.ext_file) = ? THEN DO:
        {pdferror.i &msg="'Cannot find external file ' + QUOTER(TT_pdf_external.ext_file) + '!'" &return=NO}.
        NEXT.
    END.

    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "External", ?, 0, "").
    TT_pdf_external.ext_obj = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_external.ext_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /XObject" {&pdfSKIP}
        "/Subtype /Form" {&pdfSKIP}
        "/FormType 1" {&pdfSKIP}.

    /* Handle an External Object that was produced in Landscape Mode */
    IF TT_pdf_external.ext_rotate = 90 OR TT_pdf_external.ext_rotate = 270 THEN DO: /* 03-NOV-2014 jcc: 270 */
      /* 03-DEC-2014 jcc: move this FIND at the FOR EACH above */
      /* FIND FIRST tt_pdf_page */
           /* WHERE tt_pdf_page.obj_stream = pdfStream */
             /* AND tt_pdf_page.pdf_id_use = TT_pdf_external.pdf_id /* 02-DEC-2014 jcc */ */
             /* AND TT_pdf_Page.page_nbr   = tt_pdf_external.page_id */
             /* NO-LOCK NO-ERROR. */
      IF TT_pdf_external.ext_rotate = 90 THEN PUT STREAM S_pdf_inc UNFORMATTED
          "/Matrix [0 -1 1 0 0 " dec2string(TT_pdf_external.ext_Media3) "]" {&pdfSKIP}.
          /* "/Matrix [0 -1 1 0 0 " dec2string(TT_pdf_page.page_height) "]" {&pdfSKIP}. */ /* 11-DEC-2014 jcc:  */
          /* "/Matrix [0 -1 1 0 0 " dec2string(TT_pdf_page.page_width) "]" {&pdfSKIP}. */ /* 09-DEC-2014 jcc:  */
      ELSE PUT STREAM S_pdf_inc UNFORMATTED /* 03-NOV-2014 jcc: 270 */
          "/Matrix [0 1 -1 0 " dec2string(TT_pdf_external.ext_Media4) " 0]" {&pdfSKIP}.
          /* "/Matrix [0 1 -1 0 " dec2string(TT_pdf_page.page_height) " 0]" {&pdfSKIP}. */ /* 11-DEC-2014 jcc:  */
      PUT STREAM S_pdf_inc UNFORMATTED
          "/BBox [0 0 " dec2string(TT_pdf_external.ext_Media3) " " dec2string(TT_pdf_external.ext_Media4) "]" {&pdfSKIP}.
          /* "/BBox [0 0 " dec2string(TT_pdf_Page.page_height) " " dec2string(TT_pdf_Page.page_width) "]" {&pdfSKIP}. */ /* 11-DEC-2014 jcc:  */
          /* "/BBox [0 0 " dec2string(TT_pdf_Page.page_width) " " dec2string(TT_pdf_Page.page_height) "]" {&pdfSKIP}. */ /* 09-DEC-2014 jcc:  */
    END.
    ELSE
      PUT STREAM S_pdf_inc UNFORMATTED
        "/Matrix [1 0 0 1 0 0]" {&pdfSKIP}
        "/BBox [" dec2string(TT_pdf_external.ext_media1) " " dec2string(TT_pdf_external.ext_media2) " " dec2string(TT_pdf_external.ext_media3) " " dec2string(TT_pdf_external.ext_media4) "]" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Resources <<" {&pdfSKIP}.

    /* Font */
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Font <<" L_Fontobj " >>" {&pdfSKIP}.

    /* PUT the different resource sub-dictionaries */
    RUN putResources("XObject",    tt_pdf_external.ext_page, pdfStream, TT_pdf_external.pdf_id).
    RUN putResources("ExtGState",  tt_pdf_external.ext_page, pdfStream, TT_pdf_external.pdf_id).
    RUN putResources("ColorSpace", tt_pdf_external.ext_page, pdfStream, TT_pdf_external.pdf_id).
    RUN putResources("Shading",    tt_pdf_external.ext_page, pdfStream, TT_pdf_external.pdf_id).
    RUN putResources("Pattern",    tt_pdf_external.ext_page, pdfStream, TT_pdf_external.pdf_id).
    RUN putResources("Properties", tt_pdf_external.ext_page, pdfStream, TT_pdf_external.pdf_id).

    /* End of /Resources */
    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}.

    /* 15-OCT-2015 jcc: all the code below can be replaced by this */
    RUN putFileAsStream IN TARGET-PROCEDURE (pdfStream, TT_pdf_external.ext_file, TT_pdf_external.ext_obj, "", "", pdfEncrypt, NO, YES).

    /*
        "/Length " (TT_pdf_external.ext_obj + 1) " 0 R" {&pdfSKIP}.

    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Filter /FlateDecode" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP}.

    /* Get PDF Stream Start Offset */
    L_start = SEEK(S_pdf_inc).

    RUN getFileAsMemptr(pdfStream, TT_pdf_external.ext_file, ?, INPUT-OUTPUT mFile, OUTPUT iFileSize).

    /* Replace the XObject tags */
    /* 28-FEB-2011 jcc: I still do not get why it is necessary (see comment 10/07/04  G Campbell, both there and in pdfextract), as an XObject is an independant entity: it should be ok to have resources with the same name that belong to different XObjects or to the pages */
    /* 27-OCT-2014 jcc: removed this code. It is useless to replace the resources names, as says the PDF spec (7.8.3 Resource Dictionaries):
       "Named resources shall be meaningful only in the context of a content stream. The scope of a resource name
       shall be local to a particular content stream and shall be unrelated to externally known identifiers for objects
       such as fonts. References from one object outside of content streams to another outside of content streams
       shall be made by means of indirect object references rather than named resources." */

    /** Compression happens before Encryption **/
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
      L_Stat = compressbuffer( mFile,
                               INPUT-OUTPUT vDest,
                               OUTPUT vDestLen).
      IF L_Stat = 0 THEN DO:
          SET-SIZE(mFile) = 0.
          SET-SIZE(mFile) = GET-SIZE(vDest).
          mFile = vDest.
      END.
      ELSE
          {pdferror.i &msg="'Zlib error: ' + STRING(L_Stat)" &return=NO}. /* continue without compression */
    END.     

    RUN OutputMemPtr(pdfStream,
                     FALSE,
                     TT_pdf_stream.obj_UniqueID, 
                     pdf_inc_ObjectSequence, 
                     mFile,
                     L_EncryptKey).
    SET-SIZE(L_EncryptKey) = 0.
    SET-SIZE(mFile)        = 0. /* Release memory */
    SET-SIZE(vDest)        = 0. /* Release memory */

    L_end = SEEK(S_pdf_inc).

    PUT STREAM S_pdf_inc UNFORMATTED
               "~n"  /* igc - added to ensure that the endstream was
                              separated from the last command correctly 
                     */
               "endstream" {&pdfSKIP}
               "endobj" {&pdfSKIP}.

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ExternalLen", ?, 0, "").
    TT_pdf_external.ext_len = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_external.ext_len " 0 obj" {&pdfSKIP}
        "  " (L_end - L_start) {&pdfSKIP}
        "endobj" {&pdfSKIP}.
    */

  END. /* each TT_pdf_external */

END. /* pdf_load_external */

PROCEDURE pdf_load_links: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE m_EncryptKey  AS MEMPTR NO-UNDO.

  DEFINE BUFFER tt_pdf_bookmark FOR tt_pdf_bookmark.
  DEFINE BUFFER tt_pdf_object   FOR tt_pdf_object.
  DEFINE BUFFER tt_pdf_annot    FOR tt_pdf_annot.

  FOR EACH TT_pdf_annot WHERE TT_pdf_annot.obj_stream = pdfStream:

    /* 14-SEP-2012 jcc: sanity check for GoTo Links */
    IF tt_pdf_annot.annot_type = "Link"
        AND SUBSTRING(TT_pdf_annot.annot_content,1,1) = "#" THEN DO:
        RUN pdf_unreplace_text(INPUT-OUTPUT TT_pdf_annot.annot_content).
        /* Find the bookmark corresponding to the link */
        FIND FIRST tt_pdf_bookmark WHERE tt_pdf_bookmark.obj_stream = pdfStream
            AND tt_pdf_bookmark.book_title = SUBSTRING(TT_pdf_annot.annot_content, 2) NO-ERROR. /* the first character of TT_pdf_annot.annot_content is "#" */
        IF NOT AVAILABLE tt_pdf_bookmark THEN DO:
            {pdferror.i &msg="'GoTo link: did not find the bookmark ' + QUOTER(SUBSTRING(TT_pdf_annot.annot_content, 2))" &return=NO}.
            NEXT.            
        END.
    END.

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Link", ?, 0, "").
    TT_pdf_annot.annot_obj = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_annot.annot_obj " 0 obj" CHR(13)
        "<<" CHR(13)
        "/Type /Annot" CHR(13)
        "/Subtype /" TT_pdf_annot.annot_type CHR(13)
        "/Rect [ " TT_pdf_annot.annot_rect "]" CHR(13)
        "/C [ " TT_pdf_annot.annot_color " ]" CHR(13).

    CASE TT_pdf_annot.annot_type:
      WHEN "Link" THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Border [0 0 " TT_pdf_annot.annot_border "]" CHR(13)
            "/H /" TT_pdf_annot.annot_style CHR(13)
            "/A << " CHR(13)
            "  /Type /Action" CHR(13).

        /* 14-SEP-2012 jcc: implement GoTo links (internal links) */
        /* GoTo */
        IF SUBSTRING(TT_pdf_annot.annot_content,1,1) = "#" THEN DO:
            /* The corresponding page obj number needed in the destination /D has not been created yet, so defer until we know the page_obj */
            /* We reserve an object that will be printed once we know the obj for the page (see pdf_definition) */
            ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Deferred-Page", 0, tt_pdf_bookmark.book_page, "[ @PAGE-OBJ@ 0 R /XYZ 0 " + STRING(tt_pdf_bookmark.book_Y) + " 0 ]").
            PUT STREAM S_pdf_inc UNFORMATTED
                "  /S /GoTo" CHR(13)
                "  /D " pdf_inc_ObjectSequence " 0 R" CHR(13).
        END.
        /* URI */
        ELSE DO:
            PUT STREAM S_pdf_inc UNFORMATTED
                "  /S /URI" CHR(13)
                "  /URI ".
            RUN putString(pdfStream, "(", TT_pdf_annot.annot_content, NO, YES, pdfEncrypt, TT_pdf_annot.annot_obj, m_EncryptKey).
            PUT STREAM S_pdf_inc UNFORMATTED CHR(13).
        END.

        PUT STREAM S_pdf_inc UNFORMATTED
            "  >>" CHR(13).
      END. /* Link */

      WHEN "Text" OR WHEN "Stamp" THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/T ".
        RUN putString(pdfStream, "(", TT_pdf_annot.annot_style, YES, YES, pdfEncrypt, TT_pdf_annot.annot_obj, m_EncryptKey).
        PUT STREAM S_pdf_inc UNFORMATTED
            CHR(13)
            "/Name /" TT_pdf_annot.annot_icon CHR(13)
            "/Border [0 0 " TT_pdf_annot.annot_border "]" CHR(13)
            "/Contents " CHR(13).
        RUN putString(pdfStream, "(", TT_pdf_annot.annot_content, YES, YES, pdfEncrypt, TT_pdf_annot.annot_obj, m_EncryptKey).
      END. /* Text */

      WHEN "Highlight" OR WHEN "Underline" OR WHEN "Squiggly" OR WHEN "Strikeout"
      THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/QuadPoints [" TT_pdf_annot.annot_add "]" CHR(13) 
            "/T ".
        RUN putString(pdfStream, "(", TT_pdf_annot.annot_style, YES, YES, pdfEncrypt, TT_pdf_annot.annot_obj, m_EncryptKey).
        PUT STREAM S_pdf_inc UNFORMATTED
            CHR(13)
            "/Border [0 0 " TT_pdf_annot.annot_border "]" CHR(13)
            "/Contents " CHR(13).
        RUN putString(pdfStream, "(", TT_pdf_annot.annot_content, YES, YES, pdfEncrypt, TT_pdf_annot.annot_obj, m_EncryptKey).
        PUT STREAM S_pdf_inc UNFORMATTED CHR(13).
      END. /* Highlight,Underline,Squiggly,StrikeOut */

    END CASE.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" CHR(13)
        "endobj" CHR(13).

    SET-SIZE(m_EncryptKey) = 0.

  END. /* each TT_pdf_annot */

END. /* pdf_load_links */

/* 08-AUG-2011 jcc: start */
PROCEDURE pdf_extract_image_info: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream             AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pImage                AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType               AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiWidth              AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiHeight             AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDepth              AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBitDepth           AS INTEGER     NO-UNDO. /* aka bits per component */
    DEFINE OUTPUT PARAMETER opcColorSpace         AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAltColorSpace      AS CHARACTER   NO-UNDO. /* 14-NOV-2014 jcc */
    DEFINE OUTPUT PARAMETER opcDeviceNComponents  AS CHARACTER   NO-UNDO. /* 13-NOV-2014 jcc */
    DEFINE OUTPUT PARAMETER opcFunctionParams     AS CHARACTER   NO-UNDO. /* 13-NOV-2014 jcc */
    DEFINE OUTPUT PARAMETER opcFunctionPostScript AS CHARACTER   NO-UNDO. /* 13-NOV-2014 jcc */
    DEFINE OUTPUT PARAMETER opcFilter             AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcParams             AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDataFile           AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPalFile            AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPalLength          AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTrns               AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTrnsMaskFile       AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cTmpImgName    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE mImage         AS MEMPTR      NO-UNDO.

    /* 16-MAY-2014 jcc: implement a cache for images */
    OS-CREATE-DIR VALUE(SESSION:TEMP-DIR + "pdfcache").
    IF OS-ERROR <> 0 THEN {pdferror.i &msg="'Cannot create cache directory' + QUOTER(SESSION:TEMP-DIR + 'pdfcache') + '. OS Error #' + STRING(OS-ERROR)" &return=YES}.
    cTmpImgName = SESSION:TEMP-DIR + "pdfcache/" + ENCODE(REPLACE(pImage, "~\", "~/")). /* ENCODE to have a unique name */
    IF SEARCH(cTmpImgName + ".cache") <> ? THEN DO:
        /* just import the values from the cache; the files must already be there */
        INPUT FROM VALUE(cTmpImgName + ".cache").
        IMPORT DELIMITER "~t" opcType opiWidth opiHeight opiDepth opiBitDepth opcColorSpace opcAltColorSpace opcDeviceNComponents opcFunctionParams opcFunctionPostScript opcFilter opcParams opcDataFile opcPalFile opiPalLength opcTrns opcTrnsMaskFile.
        INPUT CLOSE.
        IF opcType = "PNG" AND opiBitDepth >= 16 THEN
            RUN pdf_set_MinPdfVersion(pdfStream,"1.5"). /* 20-OCT-2011 jcc: 16 bit images supported only starting with PDF v1.5 */
        RETURN.
    END.

    SET-SIZE(mImage) = 32.
    INPUT  STREAM S_pdf_inp FROM VALUE(SEARCH(pImage)) BINARY NO-MAP NO-CONVERT NO-ECHO.
    IMPORT STREAM S_pdf_inp mImage.
    INPUT  STREAM S_pdf_inp CLOSE.

    IF    GET-BYTE(mImage, 1) = 137
      AND GET-BYTES(mImage, 2, 3) = "PNG"
      AND GET-BYTE(mImage, 5) = 13 AND GET-BYTE(mImage, 6) = 10
      AND GET-BYTE(mImage, 7) = 26 AND GET-BYTE(mImage, 8) = 10 THEN DO:
        RUN {&PDFDIR}lib/pdf_img_png.p (pdfStream
                              ,pImage
                              ,OUTPUT opcType
                              ,OUTPUT opiWidth
                              ,OUTPUT opiHeight
                              ,OUTPUT opiDepth
                              ,OUTPUT opiBitDepth
                              ,OUTPUT opcColorSpace
                              ,OUTPUT opcAltColorSpace
                              ,OUTPUT opcDeviceNComponents
                              ,OUTPUT opcFunctionParams
                              ,OUTPUT opcFunctionPostScript
                              ,OUTPUT opcFilter
                              ,OUTPUT opcParams
                              ,OUTPUT opcDataFile
                              ,OUTPUT opcPalFile
                              ,OUTPUT opiPalLength
                              ,OUTPUT opcTrns
                              ,OUTPUT opcTrnsMaskFile
                              ,cTmpImgName
                              ,mImage) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN {pdferror.i &msg=RETURN-VALUE &return=YES &error=YES}.
        IF opiBitDepth >= 16 THEN
            RUN pdf_set_MinPdfVersion(pdfStream,"1.5"). /* 20-OCT-2011 jcc: 16 bit images supported only starting with PDF v1.5 */
    END. /* PNG */

    &GLOBAL-DEFINE M_SOI   216 /* 0xD8 */      /* Start Of Image (beginning of datastream) */
    &GLOBAL-DEFINE M_MARK  255 /* 0xFF */      /* Marker */
    ELSE IF   GET-BYTE(mImage, 1) = {&M_Mark}
          AND GET-BYTE(mImage, 2) = {&M_SOI} THEN DO: /* might be JPG */
        RUN {&PDFDIR}lib/pdf_img_jpg.p (pdfStream
                              ,pImage
                              ,OUTPUT opcType
                              ,OUTPUT opiWidth
                              ,OUTPUT opiHeight
                              ,OUTPUT opiDepth
                              ,OUTPUT opiBitDepth
                              ,OUTPUT opcColorSpace
                              ,OUTPUT opcAltColorSpace
                              ,OUTPUT opcDeviceNComponents
                              ,OUTPUT opcFunctionParams
                              ,OUTPUT opcFunctionPostScript
                              ,OUTPUT opcFilter
                              ,OUTPUT opcParams
                              ,OUTPUT opcDataFile
                              ,OUTPUT opcPalFile
                              ,OUTPUT opiPalLength
                              ,OUTPUT opcTrns
                              ,OUTPUT opcTrnsMaskFile
                              ,cTmpImgName
                              ,mImage) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN {pdferror.i &msg=RETURN-VALUE &return=YES &error=YES}.
    END. /* JPG */

    ELSE IF GET-BYTES(mImage, 1, 6) MATCHES "GIF..a" /* GIF87a or GIF89a */ THEN DO:
        RUN {&PDFDIR}lib/pdf_img_gif.p (pdfStream
                              ,pImage
                              ,OUTPUT opcType
                              ,OUTPUT opiWidth
                              ,OUTPUT opiHeight
                              ,OUTPUT opiDepth
                              ,OUTPUT opiBitDepth
                              ,OUTPUT opcColorSpace
                              ,OUTPUT opcAltColorSpace
                              ,OUTPUT opcDeviceNComponents
                              ,OUTPUT opcFunctionParams
                              ,OUTPUT opcFunctionPostScript
                              ,OUTPUT opcFilter
                              ,OUTPUT opcParams
                              ,OUTPUT opcDataFile
                              ,OUTPUT opcPalFile
                              ,OUTPUT opiPalLength
                              ,OUTPUT opcTrns
                              ,OUTPUT opcTrnsMaskFile
                              ,cTmpImgName
                              ,mImage) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN {pdferror.i &msg=RETURN-VALUE &return=YES &error=YES}.
    END. /* GIF */
    
    ELSE IF GET-BYTES(mImage, 1, 2) = "BM" THEN DO:
        RUN {&PDFDIR}lib/pdf_img_bmp.p (pdfStream
                              ,pImage
                              ,OUTPUT opcType
                              ,OUTPUT opiWidth
                              ,OUTPUT opiHeight
                              ,OUTPUT opiDepth
                              ,OUTPUT opiBitDepth
                              ,OUTPUT opcColorSpace
                              ,OUTPUT opcAltColorSpace
                              ,OUTPUT opcDeviceNComponents
                              ,OUTPUT opcFunctionParams
                              ,OUTPUT opcFunctionPostScript
                              ,OUTPUT opcFilter
                              ,OUTPUT opcParams
                              ,OUTPUT opcDataFile
                              ,OUTPUT opcPalFile
                              ,OUTPUT opiPalLength
                              ,OUTPUT opcTrns
                              ,OUTPUT opcTrnsMaskFile
                              ,cTmpImgName
                              ,mImage) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN {pdferror.i &msg=RETURN-VALUE &return=YES &error=YES}.
    END. /* BMP */

    ELSE {pdferror.i &msg="'Unsupported image type!'" &cleanup="SET-SIZE(mImage) = 0." &return=YES &error=YES}.

    SET-SIZE(mImage) = 0.

    /* 16-MAY-2014 jcc: write the cache info */
    DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
    OUTPUT TO VALUE(cTmpImgName + ".cache").
    EXPORT DELIMITER "~t" opcType opiWidth opiHeight opiDepth opiBitDepth opcColorSpace opcAltColorSpace opcDeviceNComponents opcFunctionParams opcFunctionPostScript opcFilter opcParams opcDataFile opcPalFile opiPalLength opcTrns opcTrnsMaskFile.
    OUTPUT CLOSE.
END. /* pdf_extract_image_info */
/* 20-OCT-2011 jcc: end */

/* 12-FEB-2010 jcc: start */
PROCEDURE utf8_to_utf16be: /* PRIVATE */
  DEFINE INPUT  PARAMETER pdfStream             AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pdfText               AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pcFrom                AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER plEncrypt             AS LOGICAL     NO-UNDO.
  DEFINE OUTPUT PARAMETER out                   AS MEMPTR      NO-UNDO.

  DEFINE VARIABLE bom      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cTmp     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iLength  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lProtect AS LOGICAL     NO-UNDO.

  IF COMPARE(pdfText, "=", "", "RAW") THEN RETURN. /* 18-FEB-2014 jcc: must use COMPARE, else e.g. "   " would be transformed to "" */
  /* 25-SEP-2013 jcc: ensure pdfText is utf-8 before the utf-16 conversion */
  /* 10-FEB-2014 jcc: except when outputting unicode! */
  /* IF pdf_get_parameter(pdfStream, "unicode") = "NO" THEN */
      /* pdfText = CODEPAGE-CONVERT(pdfText, "utf-8"). */

  /* 11-FEB-2014 jcc: make use of CODEPAGE-CONVERT instead of crafting it "manually": it will be much faster ;
     note: bom is used for metadata for which the input must come encoded with SESSION:CPINTERNAL */

  CASE pcFrom:
      WHEN "streamContent" THEN DO: /* page content: no BOM, special chars replaced, input is UTF-8 */
          ASSIGN
           cTmp     = CODEPAGE-CONVERT(pdfText, "utf-16be", "utf-8")
           bom      = NO
           lProtect = YES.
      END.
      WHEN "putString" THEN DO: /* string value: with BOM, special chars replaced only when not encrypted */
          ASSIGN
           cTmp     = CODEPAGE-CONVERT(pdfText, "utf-16be")
           bom      = YES
           lProtect = NOT plEncrypt.
          IF SESSION:CPINTERNAL = "utf-8" THEN /* 07-JUL-2014 jcc: remove last chr(0) */
              cTmp = SUBSTRING(cTmp, 1, LENGTH(cTmp, "RAW") - 1, "RAW").
      END.
      WHEN "raw" THEN DO: /* no BOM, no replace */
          ASSIGN
           cTmp     = IF pdf_get_parameter(pdfStream, "unicode") = "YES"
                      THEN CODEPAGE-CONVERT(pdfText, "utf-16be", "utf-8")
                      ELSE CODEPAGE-CONVERT(pdfText, "utf-16be")
           bom      = NO
           lProtect = NO.
      END.
  END CASE.

  /* 26-SEP-2014 jcc: codepage-convert within double byte code page sessions (e.g. shift-js) add an extra chr(0) at the end of the string: remove it */
  /* iLength = LENGTH(cTmp, "RAW"). */
  /* IF iLength MODULO 2 = 1 THEN */
      /* SUBSTRING(cTmp, iLength, 1) = "". */

  IF lProtect THEN
      cTmp = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(cTmp
             ,"~\","~\~\"), "(","~\("), ")","~\)"), "[","~\["), "]","~\]"), CHR(10), "\n"), CHR(13), "\r").

  iLength = LENGTH(cTmp, "RAW") + IF bom THEN 2 ELSE -1.
  SET-SIZE(out) = 0.
  SET-BYTE-ORDER(out) = BIG-ENDIAN.
  SET-SIZE(out) = iLength.
  IF bom THEN DO:
      PUT-BYTE(out,1) = 254.
      PUT-BYTE(out,2) = 255.
      PUT-STRING(out,3,iLength - 2) = cTmp.
  END.
  ELSE
      PUT-STRING(out,1,iLength) = cTmp.

  /* 16-MAY-2014 jcc: see older versions (< 4.2) for an ABL implementation of utf8_to_utf16be, without CODEPAGE-CONVERT */
END PROCEDURE. /* utf8_to_utf16be */
/* 12-FEB-2010 jcc: end */

PROCEDURE pdf_replace_text: /* PRIVATE */
  DEFINE INPUT  PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pdfText    AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER plUnicode  AS LOGICAL     NO-UNDO.
  DEFINE OUTPUT PARAMETER pdfTextOut AS CHARACTER   NO-UNDO.

  /* 10-FEB-2014 jcc: only when not outputting as unicode! */
  IF plUnicode THEN DO:
      pdfTextOut = pdfText.
      RETURN.
  END.

  /* Replace any special characters in the data string since this
     will create a bad PDF document */
  pdfText = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
      pdfText,"~\","~\~\")
             ,"(","~\(")
             ,")","~\)")
             ,"[","~\[")
             ,"]","~\]").

  /* 26-SEP-2013 jcc: input string must be converted to single byte codepage before outputting */
  DEFINE VARIABLE cCodePage AS CHARACTER  NO-UNDO.
  cCodePage = pdf_get_parameter(pdfStream, "CodePage").
  IF SESSION:CPINTERNAL = "utf-8" AND cCodePage = "" THEN cCodePage = "iso8859-1".
  pdfTextOut = IF cCodePage <> "" THEN CODEPAGE-CONVERT(pdfText, cCodePage) ELSE pdfText.
END. /* pdf_replace_text */

/* 30-SEP-2011 jcc: perform the reverse pdf_replace_text */
PROCEDURE pdf_unreplace_text: /* PRIVATE */
    DEFINE INPUT-OUTPUT PARAMETER pdfText AS CHARACTER   NO-UNDO.
    pdfText = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
        pdfText,"~\(","(")
               ,"~\)",")")
               ,"~\[","[")
               ,"~\]","]")
               ,"~\~\","~\").
END PROCEDURE. /* pdf_unreplace_text */
/* 30-SEP-2011 jcc: end */


&SCOPED-DEFINE tablesToClean    TT_pdf_stream,TT_pdf_page,tt_pdf_page_use_ext,TT_pdf_xobject,TT_pdf_bookmark,TT_pdf_ReplaceTxt,TT_pdf_error,TT_pdf_object,TT_pdf_info,TT_pdf_image,TT_pdf_font,TT_pdf_annot,TT_pdf_tool,TT_pdf_tool_param,TT_pdf_xml,TT_pdf_FillTxt,tt_line,tt_state_op,TT_pdf_diff,TT_pdf_font_character,TT_Font_Widths,TT_pdf_param
&SCOPED-DEFINE tablesToCleanExt TT_pdf_external,TT_pdf_ext,TT_Font,TT_Info,TT_Object,TT_Resource,tt_dict,tt_array,TT_Widget

PROCEDURE pdf_reset_all: 
  /* clear out all streams, temp-tables and reset variables as required */

  DEFINE VARIABLE cTable  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(""). &ENDIF

  /* These are the only two variables that don't apear to be reset anywhere */
  ASSIGN /* pdf_inc_ContentSequence = 0 */
         pdf_inc_ObjectSequence  = 0
         glUnitTest   = NO /* 12-MAY-2014 jcc: glUnitTest & glNoOptimize were missing */
         glNoOptimize = NO
         array_seq    = 0
         dict_seq     = 0.

  /* 15-MAY-2014 jcc: do it dynamically: shorter code, and it will be sufficient to add new tables to the list above */
  DO i = NUM-ENTRIES("{&tablesToClean},{&tablesToCleanExt}") TO 1 BY -1:
      cTable = ENTRY(i, "{&tablesToClean},{&tablesToCleanExt}").
      IF LOOKUP(cTable, "TT_pdf_ext") > 0 THEN NEXT.
      CREATE BUFFER hBuffer FOR TABLE cTable.
      hBuffer:EMPTY-TEMP-TABLE().
      DELETE OBJECT hBuffer.
  END.

  /* 20-MAY-2014 jcc: we don't delete image files anymore (cache) */
  /* FOR EACH TT_pdf_image:
    /* 07-OCT-2013 jcc: temp files cleanup ; moved here from pdf_load_images */
    IF REPLACE(TT_pdf_image.image_data,"~\","~/") <> REPLACE(TT_pdf_image.image_file,"~\","~/") THEN
        OS-DELETE VALUE(TT_pdf_image.image_data).
    IF TT_pdf_image.image_palette > "" THEN
        OS-DELETE VALUE(TT_pdf_image.image_palette).
    /* 07-OCT-2013 jcc: end */
    DELETE TT_pdf_image.
  END. */

  FOR EACH TT_pdf_ext:
    IF NOT LOGICAL(pdf_get_parameter2(TT_pdf_ext.obj_stream, "usePdfCache", "Yes")) THEN /* 01-OCT-2015 jcc: pdf cache */
    IF OPSYS = "UNIX" THEN
        OS-COMMAND SILENT VALUE("rm -Rf " + TT_pdf_ext.cache_dir).
      /* OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt"). */
    ELSE
        OS-COMMAND SILENT VALUE("rmdir /q /s " + RIGHT-TRIM(REPLACE(TT_pdf_ext.cache_dir, "~/", "~\"), "~\")).
      /* OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt"). */

    DELETE TT_pdf_ext.
  END.

END. /* pdf_reset_all */

PROCEDURE pdf_reset_stream:
  /* Clear out an individual stream - reset the variables */
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.

  DEFINE BUFFER bTT_pdf_ext   FOR TT_pdf_ext.
  DEFINE BUFFER bTT_pdf_image FOR TT_pdf_image.
  DEFINE BUFFER TT_pdf_ext    FOR TT_pdf_ext.
  DEFINE BUFFER TT_pdf_image  FOR TT_pdf_image.

  DEFINE VARIABLE cTable         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hBuffer        AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hQuery         AS HANDLE      NO-UNDO.
  DEFINE VARIABLE i              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lReuseExternal AS LOGICAL     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

  /* These are the only two variables that don't appear to be reset anywhere */
  ASSIGN pdf_inc_ObjectSequence  = 0
         glUnitTest   = NO /* 12-MAY-2014 jcc: glUnitTest & glNoOptimize were missing */
             WHEN NOT CAN-FIND(FIRST TT_pdf_param WHERE TT_pdf_param.obj_parameter = "unitTest") /* 07-OCT-2015 jcc: only when we close the last stream */
         glNoOptimize = NO.

  /* 20-MAY-2014 jcc: "reuseExternal" means that next generated document will reuse the information produced by pdf_open_PDF without having to call it again */
  lReuseExternal = LOGICAL(pdf_get_parameter2(pdfStream, "reuseExternal", "FALSE")).

  /* 15-MAY-2014 jcc: do it dynamically: shorter code, and it will be sufficient to add new tables to the list above */
  DO i = NUM-ENTRIES("{&tablesToClean},{&tablesToCleanExt}") TO 1 BY -1:
      cTable = ENTRY(i, "{&tablesToClean},{&tablesToCleanExt}").
      IF LOOKUP(cTable, "TT_pdf_ext,TT_dict,TT_array") > 0 
        OR (lReuseExternal AND LOOKUP(cTable, "{&tablesToCleanExt}") > 0)
          THEN NEXT.
      CREATE BUFFER hBuffer FOR TABLE cTable.
      CREATE QUERY hQuery.
      hQuery:SET-BUFFERS(hBuffer).
      hQuery:FORWARD-ONLY = YES.
      hQuery:QUERY-PREPARE("FOR EACH " + cTable + " WHERE " + cTable + ".obj_stream = '" + pdfStream + "'").
      hQuery:QUERY-OPEN().
      REPEAT:
          hQuery:GET-NEXT().
          IF hQuery:QUERY-OFF-END THEN LEAVE.
          IF lReuseExternal AND cTable = "tt_pdf_font" AND hBuffer:BUFFER-FIELD("font_file"):BUFFER-VALUE = "EXTERNAL" THEN
              NEXT. /* do not delete the fonts created through the use of an external pdf file */
          hBuffer:BUFFER-DELETE() NO-ERROR.
      END.
      hQuery:QUERY-CLOSE().
      DELETE OBJECT hQuery.
      DELETE OBJECT hBuffer.
  END.

  /* 20-MAY-2014 jcc: we don't delete image files anymore (cache) */
  /* FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream:
    /* 07-OCT-2013 jcc: temp files cleanup ; moved here from pdf_load_images ;
                        do not delete if still used (e.g. through pdf_merge_stream) */
    IF NOT CAN-FIND(FIRST bTT_pdf_image WHERE bTT_pdf_image.obj_stream <> pdfStream
                                          AND bTT_pdf_image.image_file = TT_pdf_image.image_file) THEN DO:
        /* 09-AUG-2011 jcc: cleanup of image temp files */
        IF REPLACE(TT_pdf_image.image_data,"~\","~/") <> REPLACE(TT_pdf_image.image_file,"~\","~/") THEN
            OS-DELETE VALUE(TT_pdf_image.image_data).
        IF TT_pdf_image.image_palette > "" THEN
            OS-DELETE VALUE(TT_pdf_image.image_palette).
    END.
    /* 07-OCT-2013 jcc: end */
    DELETE TT_pdf_image.
  END. */

  FOR EACH TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream:
    /* 06-OCT-2013 jcc: do not delete if the files are still used */
    IF NOT LOGICAL(pdf_get_parameter2(pdfStream, "usePdfCache", "Yes")) /* 01-OCT-2015 jcc: pdf cache */
       AND NOT CAN-FIND(FIRST bTT_pdf_ext WHERE bTT_pdf_ext.obj_stream <> TT_pdf_ext.obj_stream
                                            AND bTT_pdf_ext.pdf_id = TT_pdf_ext.pdf_id)
       /* 23-SEP-2015 jcc: also do not delete if another stream is using the same pdf file: they share the same files */
       AND NOT CAN-FIND(FIRST bTT_pdf_ext WHERE bTT_pdf_ext.obj_stream <> TT_pdf_ext.obj_stream
                                            AND bTT_pdf_ext.pdf_id_orig = TT_pdf_ext.pdf_id)
       THEN DO: 
        IF OPSYS = "UNIX" THEN DO:
            /* OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt"). */
            IF lReuseExternal THEN
                OS-COMMAND SILENT VALUE("rm -f " + TT_pdf_ext.cache_dir + TT_pdf_ext.pdf_id + "*ObjStm*.txt").
            ELSE
                OS-COMMAND SILENT VALUE("rm -Rf " + TT_pdf_ext.cache_dir).
        END.
        ELSE DO:
            /* OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt"). */
            IF lReuseExternal THEN
                OS-COMMAND SILENT VALUE("del " + REPLACE(TT_pdf_ext.cache_dir, "~/", "~\") + TT_pdf_ext.pdf_id + "*ObjStm*.txt").
            ELSE
                OS-COMMAND SILENT VALUE("rmdir /q /s " + RIGHT-TRIM(REPLACE(TT_pdf_ext.cache_dir, "~/", "~\"), "~\")).
        END.
    END.

    IF NOT lReuseExternal THEN DO:
        /* 04-OCT-2013 jcc: clean tt_dict & tt_array */
        IF NOT CAN-FIND(FIRST bTT_pdf_ext WHERE bTT_pdf_ext.obj_stream <> TT_pdf_ext.obj_stream
                                            AND bTT_pdf_ext.dict_seq_start = TT_pdf_ext.dict_seq_start) THEN DO:
            FOR EACH TT_dict WHERE TT_dict.dict_id >= TT_pdf_ext.dict_seq_start AND TT_dict.dict_id <= TT_pdf_ext.dict_seq_end:
                DELETE TT_dict.
            END.
            FOR EACH TT_array WHERE TT_array.array_id >= TT_pdf_ext.array_seq_start AND TT_array.array_id <= TT_pdf_ext.array_seq_end:
                DELETE TT_array.
            END.
        END.

        DELETE TT_pdf_ext.
    END.
    ELSE DO:
        FOR EACH TT_Object WHERE TT_Object.obj_stream = pdfStream
            AND TT_Object.is_dirty = NO:
            /* set the objects without new obj & dirty, else they won't be reusable */
            ASSIGN
                TT_Object.new_obj  = 0
                TT_Object.is_dirty = YES.
        END.
        /* 04-NOV-2014 jcc: also reset tt_pdf_font.font_obj */
        FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream:
            TT_pdf_font.font_obj = 0.
        END.
    END.

  END.
END. /* pdf_reset_stream */

&UNDEFINE tablesToClean
&UNDEFINE tablesToCleanExt

PROCEDURE pdf_wrap_text :
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromColumn AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlignMent  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pdfMaxY      AS INTEGER NO-UNDO.

  DEFINE VARIABLE deLeftMargin AS DECIMAL     NO-UNDO.
  DEFINE VAR v-thisword  AS CHAR NO-UNDO.
  DEFINE VAR v-thisline  AS CHAR NO-UNDO.
  DEFINE VAR v-lastline  AS CHAR NO-UNDO.
  /* DEFINE VAR v-fontsize  AS DECIMAL NO-UNDO. */ /* 10-JUL-2013 jcc: unused */
  DEFINE VAR v-mywidth   AS INTEGER NO-UNDO.
  DEFINE VAR v-maxwidth  AS INTEGER NO-UNDO.
  /* DEFINE VAR v-originalY AS INTEGER NO-UNDO. */ /* 10-JUL-2013 jcc: unused */
  DEFINE VAR i           AS INTEGER NO-UNDO.
  DEFINE VAR v-index     AS INTEGER NO-UNDO.
  DEFINE VAR v-start     AS INTEGER NO-UNDO.
  DEFINE VAR v-leave     AS LOGICAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + STRING(pdfFromColumn) + "," + STRING(pdfToColumn) + "," + QUOTER(pdfAlignMent) + ",OUTPUT pdfMaxY"). &ENDIF

  /* ASSIGN v-fontsize  = PDF_text_width(pdfStream,"i") * .81 */
  ASSIGN deLeftMargin = pdf_LeftMargin(pdfStream)
         v-mywidth    = pdfToColumn - pdfFromColumn.
         /* v-originalY = PDF_TextY(pdfStream). */

  /* 05-DEC-2016 jcc: following pdf_text_at new implementation, adapt pdf_wrap_text so that pdf_skip will issue TD instead of Tm for carriage returns within the same paragraph */
  IF pdfAlignMent = "LEFT" THEN
    RUN pdf_set_LeftMargin(pdfStream, deLeftMargin + IF pdfFromColumn > 1 THEN pdf_text_widthdec(pdfStream, " ") * (pdfFromColumn - 1) ELSE 0).

  pdfText = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
      pdfText, "|", "&pipe;") /* Replace Pipes in text string with processor directive &pipe; */
             , " ", "| ") /* Spaces */
             , "-", "-|") /* Hyphens */
             , ",", ",|") /* Commas */
             , CHR(10), "|&skip;|"). /* Line Feeds */

  /* Determine the maximum possible column using the W (assuming it as the 
    largest possible character.  This is useful when using Proportional fonts -
    it won't matter when using Fixed since they are all the same width */
  v-maxwidth = pdf_getnumfittingchars(pdfStream,
                                      FILL("W",v-mywidth),
                                      0,
                                      pdf_text_width(pdfstream,FILL("W",v-mywidth))).

  pdf_WrapText = TRUE.
  pdf_WrapFont = pdf_Font(pdfStream).
  pdf_WrapSize = pdf_PointSize(pdfStream).
  /* pdf_WrapFrom = pdfFromColumn. */ /* 05-DEC-2016 jcc: not needed anymore: we now set the left margin so that we always wrap from column 1 */

  /* 17-AUG-2012 jcc: ensure we start at the left margin */
/*   IF pdf_TextX(pdfStream) <> pdf_LeftMargin(pdfStream) THEN DO:                                                    */
/*       FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream                                                    */
/*         AND tt_state_op.type     = "T"                                                                             */
/*         AND tt_state_op.operator = "Tm" NO-ERROR.                                                                  */
/*       IF AVAILABLE tt_state_op AND string2dec(ENTRY(5, tt_state_op.opvalue, " ")) = pdf_LeftMargin(pdfStream) THEN */
/*           RUN setTextOperator(pdfStream, "TD", "0 0").                                                             */
/*       ELSE                                                                                                         */
/*           RUN pdf_set_TextXY (pdfStream, pdf_LeftMargin(pdfStream), pdf_TextY(pdfStream), YES).                    */
/*   END.                                                                                                             */

  /* Divide up the pdf text into lines of width less than the
     available width */
  THIS-LOOP:
  DO i = 1 TO NUM-ENTRIES(pdfText,"|"):
    ASSIGN v-lastline = v-thisline.

    /* carriage return forced by a CHR(10) */
    IF ENTRY(i,pdfText,"|") = "&skip;" THEN DO:
      CASE pdfAlignment:
        WHEN "left" THEN
          RUN pdf_text_at(pdfStream,v-thisline,1).
          /* RUN pdf_text_at(pdfStream,v-thisline,pdfFromColumn). */
        WHEN "right" THEN
          RUN pdf_text_to(pdfStream,v-thisline,pdfToColumn).
      END CASE.

      v-thisline = "".
      RUN pdf_skip(pdfStream).
      NEXT THIS-LOOP.
    END.

    v-thisword = REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|").

    ASSIGN v-thisline = v-thisline
                      + v-thisword.

    IF pdf_getnumfittingchars(pdfStream,
                              TRIM(v-thisline),
                              0,
                              pdf_text_width(pdfstream,TRIM(v-thisline))) > v-maxwidth THEN DO:

      ASSIGN v-index = 0
             v-start = 1
             v-leave = FALSE.

      ASSIGN v-lastline = TRIM(v-lastline).

      DO WHILE TRUE:
        v-index = INDEX(v-lastline,CHR(10), v-start + 1).

        IF v-index = 0 THEN
          ASSIGN v-leave = TRUE
                 v-index = LENGTH(v-lastline, "character":u).
        ELSE v-leave = FALSE.

        CASE pdfAlignment:
          WHEN "left" THEN
            RUN pdf_text_at(pdfStream,SUBSTR(v-lastline,v-start,v-index, "character":u), 1).
            /* RUN pdf_text_at(pdfStream,SUBSTR(v-lastline,v-start,v-index, "character":u),pdfFromColumn). */
          WHEN "right" THEN
            RUN pdf_text_to(pdfStream,SUBSTR(v-lastline,v-start,v-index, "character":u),pdfToColumn).
        END CASE.

        IF v-leave THEN LEAVE.

        ELSE DO:
          v-start = INDEX(v-lastline,CHR(10), v-index + 1).
          RUN pdf_skip(pdfStream).
        END.

      END. /* while true */

      RUN pdf_skip(pdfStream).
      ASSIGN v-thisline = TRIM(REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|")).
    END.
  END. /* i = NUM-ENTRIES */

  IF v-thisline NE "" THEN DO:
    ASSIGN v-index = 0
           v-start = 1
           v-leave = FALSE.

    ASSIGN v-lastline = TRIM(v-thisline).
    DO WHILE TRUE:
      ASSIGN v-lastline = SUBSTR(v-lastline,v-start,-1, "character":u)
             v-index    = INDEX(v-lastline,CHR(10), v-start + 1).

      IF v-index = 0 THEN
        ASSIGN v-leave = TRUE
               v-index = LENGTH(v-lastline, "character":u).
      ELSE
        ASSIGN v-leave = FALSE.

      IF v-index <> 0 THEN DO:
        CASE pdfAlignment:
          WHEN "left" THEN
            RUN pdf_text_at(pdfStream, SUBSTR(v-lastline,v-start,v-index, "character":u), 1).
            /* RUN pdf_text_at(pdfStream, SUBSTR(v-lastline,v-start,v-index, "character":u),pdfFromColumn). */
          WHEN "right" THEN
            RUN pdf_text_to(pdfStream, SUBSTR(v-lastline,v-start,v-index, "character":u),pdfToColumn).
        END CASE.
      END.
      ELSE
        v-leave = TRUE.

      IF v-leave THEN LEAVE.

      ELSE DO:
        v-lastline = SUBSTR(v-lastline,v-index + 1,-1, "character":u) .
        v-start = INDEX(v-lastline,CHR(10), v-index + 1).
        RUN pdf_skip(pdfStream).
      END.

    END. /* while true */
  END. /* ThisLine <> "" */

  ASSIGN pdfMaxY = PDF_TextY(pdfStream).

  pdf_WrapText = FALSE.
  /* pdf_WrapFrom = 0. */ /* 05-DEC-2016 jcc */

  RUN pdf_set_LeftMargin(pdfStream, deLeftMargin). /* 05-DEC-2016 jcc: restore the original left margin */

END PROCEDURE. /* pdf_wrap_text */

/* 20-MAR-2015 jcc: new */
PROCEDURE pdf_wrap_text_x:    
    DEFINE INPUT  PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfText      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfFromX     AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pdfToX       AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pdfAlignMent AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pdfY         AS DECIMAL     NO-UNDO.
    
    RUN pdf_wrap_text_xy_dec (pdfStream
                             ,pdfText
                             ,pdfFromX
                             ,pdf_TextY(pdfStream)
                             ,pdfToX - pdfFromX
                             ,pdf_PageHeight(pdfStream) /*pdf_TextY(pdfStream) - pdf_BottomMargin(pdfStream)*/
                             ,pdf_PointSize(pdfStream)
                             ,pdfAlignMent).
    pdfY = DECIMAL(RETURN-VALUE).
    RUN pdf_set_textY(pdfStream, pdfY).
END PROCEDURE.

/* 07-JUN-2014 jcc: factorize code */
PROCEDURE _pdf_wrap_text_xy_flush: /* PRIVATE */
    DEFINE INPUT        PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pdfColumn    AS DECIMAL     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pdfRow       AS DECIMAL     NO-UNDO.
    DEFINE INPUT        PARAMETER pdfWidth     AS INTEGER     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pdfHeight    AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER pdfAlignMent AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER v-thisline   AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER v-rowmove    AS DECIMAL     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iPage        AS INTEGER     NO-UNDO.

    CASE pdfAlignment:
        WHEN "LEFT" THEN
            RUN pdf_text_xy_dec(pdfStream, v-thisline,
                                pdfColumn,
                                pdfRow - v-rowmove).
        WHEN "RIGHT" THEN
            RUN pdf_text_xy_dec(pdfStream, v-thisline,
                                pdfColumn + pdfWidth - pdf_text_widthdec (pdfStream, v-thisline),
                                pdfRow - v-rowmove).
        WHEN "CENTER" THEN
            RUN pdf_text_xy_dec(pdfStream, v-thisline,
                                pdfColumn + ((pdfWidth - pdf_text_widthdec (pdfStream, v-thisline)) / 2),
                                pdfRow - v-rowmove).
    END CASE.
    /* 06-JUN-2014 jcc: take into account page change (when we went below the bottom margin) */
    IF pdf_Page(pdfStream) > iPage THEN ASSIGN
        iPage     = pdf_Page(pdfStream)
        pdfRow    = pdf_TextY(pdfStream) /* get last Y coordinate from pdf_new_page (incl. header if it exists) */
        pdfHeight = pdfHeight - v-rowmove
        v-rowmove = 0.
END PROCEDURE.

/* 27-FEB-2009 jcc: start */
PROCEDURE pdf_wrap_text_xy :
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow        AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSkip       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlignMent  AS CHARACTER NO-UNDO.

  /* 08-JUN-2014 jcc: simply call pdf_wrap_text_xy_dec - too much duplicate code */
  RUN pdf_wrap_text_xy_dec (pdfStream,pdfText,pdfColumn,pdfRow,pdfWidth,pdfHeight,pdfSkip,pdfAlignMent).
  RETURN RETURN-VALUE.
END PROCEDURE. /* pdf_wrap_text_xy */

PROCEDURE pdf_wrap_text_xy_dec :
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow        AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth      AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfSkip       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlignMent  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPage      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE v-index    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE v-lastline AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE v-leave    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE v-rowmove  AS DECIMAL     DECIMALS 4 NO-UNDO.
  DEFINE VARIABLE v-start    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE v-thisline AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE v-thisword AS CHARACTER   NO-UNDO.


  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfText) + "," + dec2string(pdfColumn) + "," + dec2string(pdfRow) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight) + "," + dec2string(pdfSkip) + "," + QUOTER(pdfAlignMent)). &ENDIF

  pdfText = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
    pdfText, "|", "&pipe;") /* Replace Pipes in text string with processor directive &pipe; */
           , " ", "| ") /* Spaces */
           , "-", "-|") /* Hyphens */
           , ",", ",|") /* Commas */
           , CHR(10), "|&skip;|"). /* Line Feeds */

  /* Divide up the pdf text into lines of width less than the available width */
  pdf_WrapText = TRUE.
  pdf_WrapFont = pdf_Font(pdfStream).
  pdf_WrapSize = pdf_PointSize(pdfStream).

  iPage = pdf_Page(pdfStream).

  MAIN-BLOCK:
  DO:
      /* 14-jun-2012 jcc: v-rowmove = pdfSkip / 2. / * initial row movement: middle of the first line  * / */
      THIS-LOOP:
      DO i = 1 TO NUM-ENTRIES(pdfText,"|"):
        ASSIGN v-lastline = v-thisline.

        IF ENTRY(i,pdfText,"|") = "&skip;" THEN DO:
          RUN _pdf_wrap_text_xy_flush (pdfStream, pdfColumn, INPUT-OUTPUT pdfRow, pdfWidth, INPUT-OUTPUT pdfHeight, pdfAlignMent, v-thisline, INPUT-OUTPUT v-rowmove, INPUT-OUTPUT iPage).
          v-thisline = "".
          v-rowmove = v-rowmove + pdfSkip.
          IF v-rowmove >= pdfHeight - pdfSkip THEN /* 21-AUG-2012 jcc: add "- pdfSkip" in order to stop before the end of the box */
              LEAVE MAIN-BLOCK.
          NEXT THIS-LOOP.
        END.

        ASSIGN
         v-thisword = REPLACE(ENTRY(i,pdfText,"|"), "&pipe;", "|")
         v-thisline = v-thisline + v-thisword.

        /* if the line is greater than the width we have to place it, then skip to the next line.
           We do this always except with the first word... */
        IF (i > 1 AND pdf_text_widthdec (pdfStream, v-thisline) > pdfWidth) THEN DO:
          ASSIGN
           v-index = 0
           v-start = 1
           v-leave = FALSE.

          ASSIGN v-lastline = TRIM(v-lastline).

          DO WHILE TRUE:
            v-index = INDEX(v-lastline, CHR(10), v-start + 1).

            IF v-index = 0 THEN ASSIGN
                v-leave = TRUE
                v-index = LENGTH(v-lastline, "character":u).
            ELSE
                v-leave = FALSE.

            RUN _pdf_wrap_text_xy_flush (pdfStream, pdfColumn, INPUT-OUTPUT pdfRow, pdfWidth, INPUT-OUTPUT pdfHeight, pdfAlignMent, SUBSTR(v-lastline,v-start,v-index, "character":u), INPUT-OUTPUT v-rowmove, INPUT-OUTPUT iPage).

            IF v-leave THEN LEAVE.

            ELSE DO:
              v-start = INDEX(v-lastline, CHR(10), v-index + 1).
              v-rowmove = v-rowmove + pdfSkip.
              IF v-rowmove >= pdfHeight - pdfSkip THEN /* 21-AUG-2012 jcc: add "- pdfSkip" in order to stop before the end of the box */
                  LEAVE MAIN-BLOCK.
            END.
          END. /* while true */

          ASSIGN v-thisline = TRIM(REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|")).
          v-rowmove = v-rowmove + pdfSkip.
          IF v-rowmove >= pdfHeight - pdfSkip THEN /* 21-AUG-2012 jcc: add "- pdfSkip" in order to stop before the end of the box */
              LEAVE MAIN-BLOCK.
        END. /* IF pdf_text_widthdec (pdfStream, v-thisline) > pdfWidth */
      END. /* THIS-LOOP: i = NUM-ENTRIES */

      IF v-thisline NE "" THEN DO:
        ASSIGN v-index = 0
               v-start = 1
               v-leave = FALSE.

        ASSIGN v-lastline = TRIM(v-thisline).
        DO WHILE TRUE:
          ASSIGN v-lastline = SUBSTR(v-lastline,v-start,-1, "character":u)
                 v-index    = INDEX(v-lastline,CHR(10), v-start + 1).

          IF v-index = 0 THEN
            ASSIGN v-leave = TRUE
                   v-index = LENGTH(v-lastline, "character":u).
          ELSE
            ASSIGN v-leave = FALSE.

          IF v-index <> 0 THEN
            RUN _pdf_wrap_text_xy_flush (pdfStream, pdfColumn, INPUT-OUTPUT pdfRow, pdfWidth, INPUT-OUTPUT pdfHeight, pdfAlignMent, SUBSTR(v-lastline,v-start,v-index,"character":u), INPUT-OUTPUT v-rowmove, INPUT-OUTPUT iPage).
          ELSE
            v-leave = TRUE.

          IF v-leave THEN LEAVE.

          ELSE DO:
            v-lastline = SUBSTR(v-lastline,v-index + 1,-1, "character":u) .
            v-start = INDEX(v-lastline,CHR(10), v-index + 1).
            v-rowmove = v-rowmove + pdfSkip.
            IF v-rowmove >= pdfHeight - pdfSkip THEN /* 21-AUG-2012 jcc: add "- pdfSkip" in order to stop before the end of the box */
                LEAVE MAIN-BLOCK.
          END.

        END. /* while true */
      END. /* ThisLine <> "" */
  END. /* MAIN-BLOCK */

  pdf_WrapText = FALSE.

  /* 14-JUN-2012 jcc: tell the caller about the last line */
  IF v-rowmove >= pdfHeight THEN
      v-rowmove = v-rowmove - pdfSkip.
  RETURN STRING(pdfRow - v-rowmove).

END PROCEDURE. /* pdf_wrap_text_xy_dec */
/* 23-FEB-2010 jcc: end */

FUNCTION pdf_TotalPages RETURN CHARACTER
   (INPUT pdfStream AS CHARACTER).
  RETURN "@@TotalPages-" + pdfStream.
END FUNCTION. /* pdf_TotalPages */

/* 02-MAY-2014 jcc: new */
FUNCTION pdf_PageNo RETURN CHARACTER
   (INPUT pdfStream AS CHARACTER).
  RETURN "@@PageNo-" + pdfStream.
END FUNCTION. /* pdf_PageNo */

PROCEDURE pdf_link:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfLink     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed      AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen    AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue     AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfBorder   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfStyle    AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pdfFromX) + "," + STRING(pdfFromY) + "," + STRING(pdfWidth) + "," + STRING(pdfHeight) + "," + QUOTER(pdfLink) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue) + "," + STRING(pdfBorder) + "," + QUOTER(pdfStyle)). &ENDIF

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  IF LOOKUP(pdfStyle,"N,I,O,P") = 0 THEN
    pdfStyle = "N".

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = "Link"
         TT_pdf_annot.annot_content = pdfLink.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_rect    = STRING(pdfFromX) + " "
                                    + STRING(pdfFromY) + " "
                                    + STRING(pdfWidth) + " "
                                    + STRING(pdfHeight)
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                  + dec2string(pdfGreen) + " "
                                  + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = IF pdfBorder < 0 THEN 0 ELSE pdfBorder
         TT_pdf_annot.annot_style   = pdfStyle
         TT_pdf_annot.in_transaction = pdf_in_transaction(pdfStream).

END. /* pdf_link */

/* 02-MAR-2015 jcc: new */
FUNCTION pdf_pattern_search_by_key RETURNS INTEGER (pdfStream AS CHARACTER, pcKey AS CHARACTER):
    DEFINE BUFFER TT_pdf_xobject FOR TT_pdf_xobject.
    FIND FIRST TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream AND TT_pdf_xobject.xobject_key = pcKey NO-ERROR.
    RETURN IF AVAILABLE TT_pdf_xobject THEN TT_pdf_xobject.xobject_id ELSE 0.
END FUNCTION.
/* 02-MAR-2015 jcc: end */

/* 09-APR-2014 jcc: new */
PROCEDURE pdf_default_header:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    &SCOPED-DEFINE xdHeaderPointSize 10
    &SCOPED-DEFINE xcHeaderFont      "Helvetica"
    &SCOPED-DEFINE xcHeaderFontBold  "Helvetica-Bold"

    DEFINE VARIABLE cAuthor          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHeaderLine1     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHeaderLine2     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHeaderLogo      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cSearchKey       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cText            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTitle           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dLeftMargin      AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dPageHeight      AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dRightMargin     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dX               AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dY               AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iHeaderXObjectId AS INTEGER     NO-UNDO.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF
    
    /* 30-MAR-2015 jcc: headerNotOn1stPage */
    IF LOGICAL(pdf_get_parameter2(pdfStream, "headerNotOn1stPage", "NO")) AND pdf_Page(pdfStream) = 1 THEN RETURN.

    /* 02-MAR-2015 jcc: we will generate one different xobject for each tuple of different parameters used in the header */
    /* iHeaderXObjectId = INTEGER(pdf_get_parameter2(pdfStream, "___HeaderXObjectId", "0")). */
    cHeaderLine1 = pdf_get_parameter(pdfStream, "headerLine1").
    IF cHeaderLine1 = "" THEN cTitle = pdf_get_info(pdfStream, "Title").
    cHeaderLine2 = pdf_get_parameter(pdfStream, "headerLine2").
    IF cHeaderLine2 = "" THEN cAuthor = pdf_get_info(pdfStream, "Author").

    cSearchKey = ENCODE((IF cHeaderLine1 > "" THEN cHeaderLine1 ELSE cTitle) +
                        (IF cHeaderLine2 > "" THEN cHeaderLine2 ELSE cAuthor) +
                        pdf_get_parameter2(pdfStream, "headerSeparator", "TRUE")).
    iHeaderXObjectId = pdf_pattern_search_by_key(pdfStream, cSearchKey).

    IF iHeaderXObjectId = 0 THEN DO: /* create the xobject the first time we are called */
        dLeftMargin  = pdf_LeftMargin(pdfStream).
        dRightMargin = pdf_RightMargin(pdfStream).
        RUN pdf_pattern_begin(pdfStream, OUTPUT iHeaderXObjectId, pdf_PageWidth(pdfStream), pdf_TopMargin(pdfStream) - {&xdHeaderPointSize}, cSearchKey).
        /* Note: pdf_pattern_begin() sets the page width & height to the pattern dimensions - until pdf_pattern_end() */

        dPageHeight  = pdf_PageHeight(pdfStream). /* after pdf_pattern_begin, it is the height of the header */
        dX = pdf_PageWidth(pdfStream) / 2.
        dY = dPageHeight - {&xdHeaderPointSize} * 1.5.

        /* 30-MAR-2015 jcc: headerLogo */
        cHeaderLogo = pdf_get_parameter(pdfStream, "headerLogo").
        IF cHeaderLogo > "" THEN DO:
            DEFINE VARIABLE cLogoFile   AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE iLogoHeight AS INTEGER     NO-UNDO.
            DEFINE VARIABLE iLogoWidth  AS INTEGER     NO-UNDO.
            DEFINE VARIABLE cLogoAlign AS CHARACTER   NO-UNDO.
            cLogoFile   = ENTRY(1, cHeaderLogo, "|").
            iLogoWidth  = INTEGER(ENTRY(2, cHeaderLogo, "|")).
            iLogoHeight = INTEGER(ENTRY(3, cHeaderLogo, "|")).
            cLogoAlign  = ENTRY(4, cHeaderLogo, "|").
            RUN pdf_load_image (pdfStream, basename(cLogoFile), cLogoFile).
            IF cLogoAlign = "LEFT" THEN
                RUN pdf_place_image (pdfStream, basename(cLogoFile),
                                     dLeftMargin,
                                     dPageHeight - IF dPageHeight > iLogoHeight THEN (dPageHeight - iLogoHeight) / 2 ELSE 0,
                                     INTEGER(ENTRY(2, cHeaderLogo, "|")), INTEGER(ENTRY(3, cHeaderLogo, "|"))).
            ELSE /* RIGHT */
                RUN pdf_place_image (pdfStream, basename(cLogoFile),
                                     pdf_PageWidth(pdfStream) - dRightMargin - iLogoWidth,
                                     dPageHeight - IF dPageHeight > iLogoHeight THEN (dPageHeight - iLogoHeight) / 2 ELSE 0,
                                     INTEGER(ENTRY(2, cHeaderLogo, "|")), INTEGER(ENTRY(3, cHeaderLogo, "|"))).
        END.

        RUN pdf_set_font (pdfStream, {&xcHeaderFontBold}, {&xdHeaderPointSize}).
        cText = IF cHeaderLine1 > "" THEN cHeaderLine1 ELSE cTitle.
        IF cText > "" THEN DO:
            RUN pdf_text_center (pdfStream, cText, dX, dY).
            dY = dY - {&xdHeaderPointSize}.        
        END.
        IF dY > 0 AND
           (cHeaderLine2 > "" OR cAuthor > "") THEN DO:
                RUN pdf_set_font (pdfStream, {&xcHeaderFont}, {&xdHeaderPointSize} - 2).
                cText = IF cHeaderLine2 > "" THEN cHeaderLine2 ELSE cAuthor.
                RUN pdf_text_center (pdfStream, cText, dX, dY).
        END.
        /* 24-APR-2014 jcc: put the separator line at the bottom of the head (take into account pdf_TopMargin) */
        IF LOGICAL(pdf_get_parameter2(pdfStream, "headerSeparator", "TRUE")) THEN
            RUN pdf_line (pdfStream, dLeftMargin, 0, pdf_PageWidth(pdfStream) - dRightMargin, 0, 0.5).

        RUN pdf_pattern_end (pdfStream).
        /* RUN _pdf_set_parameter_priv (pdfStream, "___HeaderXObjectId", iHeaderXObjectId). */
    END.

    RUN pdf_pattern_use (pdfStream, iHeaderXObjectId, 0, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream) + {&xdHeaderPointSize}, ?, ?).
    
    &UNDEFINE xdHeaderPointSize
    &UNDEFINE xcHeaderFont
    &UNDEFINE xcHeaderFontBold

END PROCEDURE. /* pdf_default_header */

/* 09-APR-2014 jcc: new */
PROCEDURE pdf_default_footer:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    &SCOPED-DEFINE xdFooterPointSize 8
    &SCOPED-DEFINE xcFooterFont      "Helvetica"

    DEFINE VARIABLE cOldFont      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cText         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dOldBlue      AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOldGreen     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOldPointSize AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOldRed       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOldX         AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOldY         AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dX            AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dY            AS DECIMAL     NO-UNDO.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF
    
    ASSIGN
     cOldFont      = pdf_Font(pdfStream)
     dOldPointSize = pdf_PointSize(pdfStream)
     dOldX         = pdf_TextX(pdfStream)
     dOldY         = pdf_TextY(pdfStream)
     dOldRed       = pdf_TextRed(pdfStream)
     dOldGreen     = pdf_TextGreen(pdfStream)
     dOldBlue      = pdf_TextBlue(pdfStream)
     dX            = pdf_PageWidth(pdfStream) / 2
     dY            = pdf_BottomMargin(pdfStream).

    IF LOGICAL(pdf_get_parameter2(pdfStream, "footerSeparator", "TRUE")) THEN
        RUN pdf_line (pdfStream, pdf_LeftMargin(pdfStream), dY, pdf_PageWidth(pdfStream) - pdf_RightMargin(pdfStream), dY, 0.5).
    RUN pdf_text_color(pdfStream,.0,.0,.0).
    RUN pdf_set_font (pdfStream, {&xcFooterFont}, {&xdFooterPointSize}).
    dY = dY - {&xdFooterPointSize}.
    cText = pdf_get_parameter2(pdfStream, "footerLine1", ?).
    IF cText = ? THEN cText = "Page " + STRING(pdf_Page(pdfStream)) + "/" + pdf_TotalPages(pdfStream).
    RUN pdf_text_center (pdfStream, cText, dX, dY).
    /* dY = dY - {&xdFooterPointSize}. */
    /* RUN pdf_text_center (pdfStream, "Pages: " + pdf_TotalPages(pdfStream), dX, dY). */

    /* restore initial settings */
    RUN pdf_set_TextXY (pdfStream, dOldX, dOldY, YES).
    RUN pdf_text_color (pdfStream, dOldRed, dOldGreen, dOldBlue).
    RUN pdf_set_font (pdfStream, cOldFont, dOldPointSize).

    &UNDEFINE xdFooterPointSize
    &UNDEFINE xcFooterFont

END PROCEDURE. /* pdf_default_footer */

/* 14-MAY-2014 jcc: factorize code */
PROCEDURE _doFooter: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER phProc    AS HANDLE      NO-UNDO.
    DEFINE INPUT PARAMETER pcProc    AS CHARACTER   NO-UNDO.

    pdf_ForFooter = TRUE.
    RUN invalidateAllTextOperators (pdfStream).
    RUN pdf_set_TextXY (pdfStream, pdf_LeftMargin(pdfStream), pdf_BottomMargin(pdfStream), YES).
    IF phProc = THIS-PROCEDURE:HANDLE THEN
        RUN VALUE(pcProc) IN phProc (pdfStream) NO-ERROR.
    ELSE
        RUN VALUE(pcProc) IN phProc NO-ERROR.
    RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream)).
    pdf_ForFooter = FALSE.
END PROCEDURE. /* _doFooter */

PROCEDURE pdf_new_page :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

  /* 6-AUG-2012 jcc: the following line removes a lot of duplicated code... */
  RUN pdf_new_page2(pdfStream, pdf_Orientation(pdfStream)).

END. /* pdf_new_page */

PROCEDURE pdf_new_page2 :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOrientation AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iPageNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE maxPageNumber AS INTEGER   NO-UNDO.

  DEFINE BUFFER TT_pdf_Stream FOR TT_pdf_Stream.
  DEFINE BUFFER TT_pdf_tool   FOR TT_pdf_tool.
  DEFINE BUFFER TT_pdf_page   FOR TT_pdf_page.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfOrientation)). &ENDIF

  FIND FIRST TT_pdf_stream
       WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 31-OCT-2013 jcc: protection for when we run without a filename */
  IF TT_pdf_stream.obj_UniqueID = ? THEN {pdferror.i &msg="'Cannot create a new page when not using a filename!'" &return=YES}.

  /* 28-APR-2014 jcc: can't insert a page while in a pattern */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") <> "0" THEN
      {pdferror.i &msg="'Cannot create a new page while creating a pattern. Please end it first.'" &return=YES}.

  iPageNumber = pdf_Page(pdfStream).

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF iPageNumber >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF  TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF TT_pdf_stream.obj_Footer <> "" AND iPageNumber >= 1 THEN DO:
      FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.page_nbr = iPageNumber.
      /* 31-MAR-2015 jcc: only output the footer once per page */
      IF NOT TT_pdf_page.has_footer THEN DO:
          RUN _doFooter(pdfStream, TT_pdf_stream.obj_CallProcFooter, TT_pdf_stream.obj_Footer).
          ASSIGN
           TT_pdf_page.has_footer         = YES
           TT_pdf_page.has_footer_transac = pdf_in_transaction(pdfStream).
      END.
  END.
  /* 23-JUN-2015 jcc: if pdf_PageFooter has been called with "" as the procedure, then consider that the page has a footer not to add onle later in pdf_close: it is the dev intention not to have a footer */
  ELSE IF TT_pdf_stream.obj_footer_flag AND iPageNumber >= 1 THEN DO:
      FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.page_nbr = iPageNumber.
      TT_pdf_page.has_footer = YES.
  END.

  /* 04-OCT-2013 jcc: code factorized in the following procedure */
  RUN _pdf_close_output_context (pdfStream).

  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                          BY tt_pdf_page.page_nbr DESCENDING:
    maxPageNumber = TT_pdf_page.page_nbr.
    LEAVE.
  END.

  CREATE TT_pdf_page.
  ASSIGN
   TT_pdf_page.obj_stream  = pdfStream
   /* TT_pdf_page.page_nbr    = pdf_Page(pdfStream) */
   TT_pdf_page.page_nbr    = maxPageNumber + 1
   TT_pdf_page.page_rotate = pdf_PageRotate(pdfStream).

  /* Reset all text operators when changing page */
  EMPTY TEMP-TABLE tt_state_op.

  RUN pdf_set_Page(pdfStream, maxPageNumber + 1).

  RUN pdf_set_Orientation(pdfStream,pdfOrientation).
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).

  RUN pdf_set_TextY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

  IF TT_pdf_stream.obj_Header <> "" THEN DO:
      IF TT_pdf_stream.obj_CallProcHeader = THIS-PROCEDURE:HANDLE THEN
          RUN VALUE(TT_pdf_stream.obj_Header) IN TT_pdf_Stream.obj_CallProcHeader (pdfStream) NO-ERROR.
      ELSE
          RUN VALUE(TT_pdf_stream.obj_Header) IN TT_pdf_Stream.obj_CallProcHeader NO-ERROR.
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) > 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableHeader IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF pdf_WrapText THEN
    RUN pdf_set_font(pdfStream, pdf_WrapFont, pdf_WrapSize).
  ELSE
    RUN pdf_set_font(pdfStream, pdf_Font(pdfStream), pdf_PointSize(pdfStream)).

  PUBLISH "GeneratePDFPage" (INPUT pdfStream, INPUT pdf_Page(pdfStream)).

END. /* pdf_new_page2 */

PROCEDURE pdf_insert_page :
  DEFINE INPUT PARAMETER pdfStream       AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER pageNo          AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER BeforeOrAfter   AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE maxPageNumber AS INTEGER   NO-UNDO.
  DEFINE VARIABLE newPageNo     AS INTEGER   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.
  DEFINE BUFFER b_b_tt_pdfStream FOR TT_pdf_Stream.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + STRING(pageNo) + "," + QUOTER(BeforeOrAfter)). &ENDIF

  /* 3-AUG-2012 jcc: can't insert a page while in a transaction */
  IF pdf_get_parameter2(pdfStream, "___inTransaction", "0") <> "0" THEN
      {pdferror.i &msg="'Cannot insert page while in a transaction. Commit first.'" &return=YES}.
  /* 28-APR-2014 jcc: can't insert a page while in a pattern */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") <> "0" THEN
      {pdferror.i &msg="'Cannot insert page while creating a pattern. Please end it first.'" &return=YES}.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL B_TT_pdf_Stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF  TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  RUN _pdf_close_output_context (pdfStream).

  OUTPUT STREAM S_pdf_out CLOSE.

  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page 
     WHERE TT_pdf_page.obj_stream = pdfStream
       AND tt_pdf_page.page_nbr >= PageNo
     BREAK BY tt_pdf_page.page_nbr descending:
    IF FIRST(TT_pdf_page.page_nbr) THEN
      maxPageNumber = TT_pdf_page.page_nbr.
    
    IF TT_pdf_page.page_nbr = PageNo AND BeforeOrAfter = "AFTER" THEN NEXT.

    OS-RENAME VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, TT_pdf_page.page_nbr))
              VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, TT_pdf_page.page_nbr + 1)).
  END. /* determine maxpagenumber and renumber temp file holders */

/* Re-open page stream for current page using the
   updated file name 
   If the current page is greater or equal to the page we are inserting
   then the file will have been renamed.
   Otherwise, our current page came before the inserted page and the name
   remains the same
*/
  IF pdf_Page(pdfStream) GE pageNo THEN
      newPageNO = pdf_Page(pdfStream) + 1.
  ELSE
      newPageNo = pdf_Page(pdfStream).

  OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, newPageNo)) BINARY NO-MAP NO-CONVERT APPEND.

  IF maxPageNumber = 0 or (pageNo gt maxPageNumber) THEN {pdferror.i &msg="'Invalid page number!'" &return=YES}.

  /* 31-MAR-2015 jcc: footers are now created in pdf_close at the very end of the pdf generation, in order to ensure they are the last thing printed on each page, and that each page has got one */
  /* IF B_TT_pdf_stream.obj_Footer <> "" AND pdf_Page(pdfStream) >= 1 THEN */
      /* RUN _doFooter(pdfStream, B_TT_pdf_stream.obj_CallProcFooter, B_TT_pdf_stream.obj_Footer). */

  IF beforeOrAfter = "AFTER" THEN pageNo = pageNo + 1.

  /* Move all pages after the insert point ahead one */
  FOR each tt_pdf_object where TT_pdf_object.obj_stream  = pdfStream
                         and TT_pdf_object.obj_page    ge pageNo
                          by tt_pdf_object.obj_page descending:
        tt_pdf_object.obj_page = tt_pdf_object.obj_page + 1.
  END.

  FOR each tt_pdf_page where TT_pdf_page.obj_stream  = pdfStream
                         and TT_pdf_page.page_nbr    ge pageNo
                          by tt_pdf_page.page_nbr descending:
     tt_pdf_page.page_nbr = tt_pdf_page.page_nbr + 1.
  END.

  FOR each tt_pdf_bookmark where TT_pdf_bookmark.obj_stream  = pdfStream
                         and TT_pdf_bookmark.book_page    ge pageNo
                          by tt_pdf_bookmark.book_page descending:
    tt_pdf_bookmark.book_page = tt_pdf_bookmark.book_page + 1.
  END.

  FOR each tt_pdf_annot where TT_pdf_annot.obj_stream  = pdfStream
                         and TT_pdf_annot.annot_page    ge pageNo
                          by tt_pdf_annot.annot_page descending:
    tt_pdf_annot.annot_page = tt_pdf_annot.annot_page + 1.
  END.
  /* Done moving pages */

  /* igc
  OUTPUT STREAM S_pdf_inc CLOSE.

  OUTPUT STREAM S_pdf_out TO 
         VALUE(  SESSION:TEMP-DIR 
               + TT_pdf_stream.obj_UniqueID  
               + "-Content-" 
               + STRING(pageNo) + ".txt") 
               BINARY NO-MAP NO-CONVERT APPEND.
  */

  CREATE TT_pdf_page.
  ASSIGN
   TT_pdf_page.obj_stream   = pdfStream
   /* TT_pdf_page.page_nbr     = pdf_Page(pdfStream) */
   TT_pdf_page.page_nbr     = pageNo
   TT_pdf_page.page_rotate  = pdf_PageRotate(pdfStream).
   /* TT_pdf_page.content_file = _get_page_content_file(pdfStream, B_TT_pdf_stream.obj_UniqueID, pageNo). */

  EMPTY TEMP-TABLE tt_state_op. /* Reset all text operators */

  /* This closes the output stream for the current page
     and re-opens the stream for the new page-number */
  RUN pdf_set_Page(pdfStream,pageNo).

  /* igc - Copied from below */
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).

  /********* igc
  /* Note: the placement of the following commands is important due to the
      setting of the X and Y attributes.  DO NOT CHANGE unless tested
      thoroughly */
  RUN pdf_set_LeftMargin(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_TopMargin(pdfStream, pdf_TopMargin(pdfStream)).
  RUN pdf_set_BottomMargin(pdfStream, pdf_BottomMargin(pdfStream)).
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).
 
  CASE pdf_PageRotate(pdfStream):
    WHEN 0 OR WHEN 180 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream) ).

    END.

    WHEN 90 OR WHEN 270 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageWidth(pdfStream) - pdf_TopMargin(pdfStream) ).
    END.

  END CASE.

  RUN pdf_set_GraphicX(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_GraphicY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).
  **/

  RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

  IF B_TT_pdf_stream.obj_Header <> "" THEN DO:
      IF B_TT_pdf_stream.obj_CallProcHeader = THIS-PROCEDURE:HANDLE THEN
          RUN VALUE(B_TT_pdf_stream.obj_Header) IN B_TT_pdf_Stream.obj_CallProcHeader (pdfStream) NO-ERROR.
      ELSE
          RUN VALUE(B_TT_pdf_stream.obj_Header) IN B_TT_pdf_Stream.obj_CallProcHeader NO-ERROR.
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) > 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableHeader IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF pdf_WrapText THEN
    RUN pdf_set_font(pdfStream, pdf_WrapFont, pdf_WrapSize).
  ELSE
    RUN pdf_set_font(pdfStream, pdf_Font(pdfStream), pdf_PointSize(pdfStream)).

  PUBLISH "InsertPDFPage" (INPUT pdfStream, INPUT pdf_Page(pdfStream)). /* 01-APR-2015 jcc: added */
END. /* pdf_insert_page */

/* 05-NOV-2013 jcc: new */
PROCEDURE pdf_set_xy_offset:
    DEFINE INPUT PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdeX        AS DECIMAL     NO-UNDO. /* X offset */
    DEFINE INPUT PARAMETER pdeY        AS DECIMAL     NO-UNDO. /* Y offset */

    DEFINE VARIABLE cOffset AS CHARACTER   NO-UNDO.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + dec2string(pdeX) + "," + dec2string(pdeY)). &ENDIF

    cOffset = pdf_get_parameter(pdfStream, "___xyOffset").

    IF cOffset > "" THEN
        RUN _pdf_close_output_context (pdfStream).

    IF pdeX = 0 AND pdeY = 0 THEN
        RUN _pdf_set_parameter_priv (pdfStream, "___xyOffset", "").
    ELSE DO:
        RUN _pdf_set_parameter_priv (pdfStream, "___xyOffset", dec2string(pdeX) + "|" + dec2string(pdeY)).
        PUT STREAM s_pdf_out UNFORMATTED "q" {&pdfSKIP} "1 0 0 1 " + dec2string(pdeX) + " " + dec2string(pdeY) + " cm" {&pdfSKIP}.
        /* RUN OutputTextContent(pdfStream, "GRAPHIC", "", "1 0 0 1 " + dec2string(pdeX) + " " + dec2string(pdeY) + " cm", ""). */
    END.
END PROCEDURE.
/* 05-NOV-2013 jcc: end */

/* 04-OCT-2013 jcc: new */
PROCEDURE _pdf_close_output_context: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_Stream FOR TT_pdf_Stream.

    IF pdf_Page(pdfStream) <= 0 THEN RETURN.

    FIND FIRST TT_pdf_Stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAIL TT_pdf_Stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    IF TT_pdf_Stream.obj_DoingText THEN
        RUN OutputTextContent(pdfStream, "TEXT", "ET", "", "").
    ELSE IF TT_pdf_Stream.obj_DoingGraphic THEN
        RUN OutputTextContent(pdfStream, "GRAPHIC", "Q", "", "").

    IF pdf_get_parameter(pdfStream, "___xyOffset") > "" THEN DO:
        PUT STREAM S_pdf_out "Q" {&pdfSKIP}.
        /* RUN OutputTextContent(pdfStream, "GRAPHIC", "Q", "", ""). */
        RUN _pdf_set_parameter_priv (pdfStream, "___xyOffset", "").
    END.

    ASSIGN TT_pdf_Stream.obj_DoingText = FALSE
           TT_pdf_Stream.obj_DoingGraphic = FALSE.
END PROCEDURE. /* _pdf_close_output_context */
/* 04-OCT-2013 jcc: end */

PROCEDURE pdf_messages: /* 12-AUG-2013 jcc: used in pdf_close & for unit tests */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.

    IF CAN-FIND (FIRST TT_pdf_error
                 WHERE TT_pdf_error.obj_stream = pdfStream NO-LOCK) THEN DO:
        /* 27-FEB-2009 jcc: instead of doing MESSAGEs, we will return the error messages */
        /* FOR EACH TT_pdf_error: */
          /* MESSAGE TT_pdf_error.obj_func TT_pdf_error.obj_error SKIP */
                  /* "stream=" TT_pdf_error.obj_stream view-as alert-box. */
        /* END. */
        FOR EACH TT_pdf_error:
            ASSIGN cMessage = cMessage + CHR(10) +
                              TT_pdf_error.obj_func + '-' +  TT_pdf_error.obj_error + CHR(10) +
                              'stream = ' + TT_pdf_error.obj_stream.
        END.
    END.
    RETURN SUBSTRING(cMessage, 2).
END. /* pdf_messages */

/* 27-JAN-2016 jcc: attempt to fix Error 13, see pdf_close */
&IF OPSYS BEGINS "WIN" &THEN
PROCEDURE Sleep EXTERNAL "KERNEL32":
DEFINE INPUT PARAMETER piMilliseconds AS LONG NO-UNDO.
END PROCEDURE.
DEFINE STREAM ErrorStream.
&ENDIF
/* 27-JAN-2016 jcc: end */

PROCEDURE pdf_close :
  DEFINE INPUT  PARAMETER  pdfStream AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cMessage        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPage           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE L_Content       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE L_Encrypt       AS LOGICAL     NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE L_EncryptStatus AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE maxPageNumber   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE pdf-Res-Object  AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* 02-JUN-2015 jcc: has at least one page been created? */
  IF NOT CAN-FIND(FIRST TT_pdf_page) THEN
      {pdferror.i &msg="'No page has been created.'" &return=NO}.

  /* 3-AUG-2012 jcc: the transaction must be commited before closing */
  IF pdf_get_parameter2(pdfStream, "___inTransaction", "0") <> "0" THEN
      {pdferror.i &msg="'Cannot close the pdf while a transaction has not been commited.'" &return=NO}.
  /* 28-APR-2014 jcc: can't close the pdf while in a pattern */
  IF pdf_get_parameter2(pdfStream, "___inXobject", "0") <> "0" THEN
      {pdferror.i &msg="'Cannot close the pdf while creating a pattern. Please end it first.'" &return=NO}.

  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream
       NO-ERROR.

  /* 31-OCT-2013 jcc: TT_pdf_stream.obj_file can be ? when pdf_new was called with ? as a filename */
  IF TT_pdf_stream.obj_file = ? THEN DO:
      RUN pdf_reset_stream(pdfStream).
      RUN pdf_messages (pdfStream).
      RETURN RETURN-VALUE.
  END.

  iPage = pdf_Page(pdfStream).

  OUTPUT STREAM S_pdf_out CLOSE.

  /* 27-JAN-2016 jcc: attempt to fix Error 13 */
  &IF OPSYS BEGINS "WIN" &THEN
  DEFINE VARIABLE cErrMsg     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iTimeOut    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lOutputToOK AS LOGICAL     NO-UNDO.
  OUTPUT STREAM ErrorStream TO VALUE(SESSION:TEMP-DIRECTORY + "ErrorMessage.log") KEEP-MESSAGES.

  OutputToBlk:
  DO ON ERROR UNDO, RETRY:
    IF RETRY THEN DO:
        OUTPUT STREAM S_pdf_out CLOSE.
        OUTPUT STREAM ErrorStream CLOSE.
        iTimeOut = iTimeOut + 1.
        IF iTimeOut > 100 THEN /* 10 seconds time out */
            LEAVE OutputToBlk.
        RUN sleep(100).
        OUTPUT STREAM ErrorStream TO VALUE(SESSION:TEMP-DIRECTORY + "ErrorMessage.log") KEEP-MESSAGES.
    END.
    OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, iPage)) BINARY NO-MAP NO-CONVERT APPEND.
    OUTPUT STREAM ErrorStream CLOSE.
    lOutputToOK = TRUE.
  END.
  IF NOT lOutputToOK THEN DO: /* abort */
      OUTPUT STREAM S_pdf_out CLOSE.
      INPUT FROM VALUE(SESSION:TEMP-DIRECTORY + "ErrorMessage.log") NO-ECHO.
      IMPORT UNFORMATTED cErrMsg NO-ERROR.
      INPUT CLOSE.
      RUN pdf_error (pdfStream, "pdf_close", cErrMsg).
      RUN pdf_messages (pdfStream).
      RETURN RETURN-VALUE.
  END.
  &ELSE
    OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, iPage)) BINARY NO-MAP NO-CONVERT APPEND.
  &ENDIF
  /* 27-JAN-2016 jcc: end */

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF iPage >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  /* 31-MAR-2015 jcc: footers are now created below for each page, in order to ensure they are the last thing printed on each page, and that each page has got one */
  /* Ensure that Footer is run for the Last Page */
  /* IF TT_pdf_stream.obj_Footer <> "" THEN */
      /* RUN _doFooter(pdfStream, TT_pdf_stream.obj_CallProcFooter, TT_pdf_stream.obj_Footer). */
  IF TT_pdf_stream.obj_Footer <> "" THEN DO:
      FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.page_nbr = iPage NO-ERROR.
      IF AVAILABLE TT_pdf_page AND NOT TT_pdf_page.has_footer THEN DO:
          RUN _doFooter(pdfStream, TT_pdf_stream.obj_CallProcFooter, TT_pdf_stream.obj_Footer).
          TT_pdf_page.has_footer = YES.
      END.
  END.

  RUN _pdf_close_output_context (pdfStream).

  /* 31-MAR-2015 jcc: done in the above FOR EACH, one is enough */
  /* Reset pdf_page to the last page of the document */
  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                          BY tt_pdf_page.page_nbr DESCENDING:
    maxPageNumber = TT_pdf_page.page_nbr.
    LEAVE.
  END.

  /* 16-OCT-2014 jcc: move here the code to fill the form widgets, which is a better place, as resources has not been output yet, thus we can add a font or characters in a font subset */
  /* 31-MAR-2015 jcc: take opportunity of this loop to get maxPageNumber & create missing footers */
  /* 13-FEB-2017 jcc: nope, we need maxPageNumber sent to pdf_Form_Widgets_Content, in case @@TotalPages
     would be present in one field; as since 16-OCT-2014 pdf_Page(), so uncomenting the maxPageNumber FOR EACH
     and moving this code after. */
  FOR EACH tt_pdf_page WHERE tt_pdf_page.obj_stream = pdfStream
    BY  tt_pdf_page.page_nbr:
      RUN pdf_set_Page (pdfStream, tt_pdf_page.page_nbr).
      RUN pdf_Form_Widgets_Content (pdfStream, maxPageNumber, BUFFER TT_pdf_page).
      IF TT_pdf_stream.obj_Footer <> "" AND NOT tt_pdf_page.has_footer THEN DO:
          RUN _doFooter(pdfStream, TT_pdf_stream.obj_CallProcFooter, TT_pdf_stream.obj_Footer). /* 31-MAR-2015 jcc: ensures each page has a footer , and that it is the last thing displayed */
          tt_pdf_page.has_footer = YES.
      END.
      RUN _pdf_close_output_context (pdfStream).
  END.

  RUN pdf_set_Page(pdfStream,maxPageNumber).

  OUTPUT STREAM S_pdf_out CLOSE.

  /* Now Build PDF File */
  /* OUTPUT STREAM S_pdf_inc TO VALUE( TT_pdf_stream.obj_file ) UNBUFFERED NO-CONVERT. */ /* 22-MAY-2014 jcc: removing UNBUFFERED causes a tremendous increase of the performance! */
  OUTPUT STREAM S_pdf_inc TO VALUE( TT_pdf_stream.obj_file ) NO-MAP NO-CONVERT NO-ECHO.

    IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN DO:

      RUN {&PDFDIR}lib/pdfencrypt.p PERSISTENT SET h_PDF-Encrypt (THIS-PROCEDURE, TABLE hexarray).

      RUN BuildDocID IN h_PDF-Encrypt
                     (INPUT  TT_pdf_stream.obj_UniqueID,
                      OUTPUT TT_pdf_stream.obj_id).

      ASSIGN TT_pdf_stream.obj_master = SUBSTRING(pdf_get_parameter(pdfStream,"MasterPassword"), 1, 32)
             TT_pdf_stream.obj_user   = SUBSTRING(pdf_get_parameter(pdfStream,"UserPassword"),   1, 32).

      RUN DetermineOwnerKey IN h_PDF-Encrypt 
                            (INPUT pdfStream,
                             INPUT TT_pdf_stream.obj_UniqueID,
                             INPUT TT_pdf_stream.obj_user,
                             INPUT-OUTPUT TT_pdf_stream.obj_master).

      RUN DetermineUserKey IN h_PDF-Encrypt
                            (INPUT pdfStream,
                             INPUT TT_pdf_stream.obj_UniqueID,
                             INPUT TT_pdf_stream.obj_ID,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowPrint") = "FALSE" THEN 0 ELSE 1,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowCopy") = "FALSE" THEN 0 ELSE 1,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowModify") = "FALSE" THEN 0 ELSE 1,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowAnnots") = "FALSE" THEN 0 ELSE 1,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowForms") = "FALSE" THEN 0 ELSE 1,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowExtract") = "FALSE" THEN 0 ELSE 1,
                             INPUT IF pdf_get_parameter(pdfStream,"AllowAssembly") = "FALSE" THEN 0 ELSE 1,
                             /* OUTPUT pdf-EncryptKeyMemPtr, */
                             INPUT-OUTPUT TT_pdf_stream.obj_user,
                             OUTPUT TT_pdf_stream.obj_P).

      L_Encrypt = TRUE.
    END. /* Encryption has been set */

    /* Output PDF Header Requirements */
    RUN pdf_Header (INPUT TT_pdf_stream.obj_stream, l_Encrypt).

    /* Load 14 Base Fonts */
    RUN pdf_Encoding (pdfStream).
    RUN pdf_Fonts (pdfStream).

    IF NOT CAN-FIND (FIRST TT_pdf_error
                     WHERE TT_pdf_error.obj_stream = pdfStream NO-LOCK) THEN DO:
      /* Load Embedded Fonts */
      IF CAN-FIND( FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                                       AND TT_pdf_font.font_file <> "PDFBASE14" 
                                       AND TT_pdf_Font.font_file <> "EXTERNAL" NO-LOCK)
      THEN RUN pdf_Load_Fonts (pdfStream, L_Encrypt).

      /* Load Embedded Images */
      IF CAN-FIND( FIRST TT_pdf_image 
                   WHERE TT_pdf_image.obj_stream = pdfStream NO-LOCK)
      THEN RUN pdf_Load_Images (pdfStream,L_Encrypt).

      /* 03-MAY-2014 jcc: Load Embedded XObjects */
      IF CAN-FIND( FIRST TT_pdf_xobject 
                   WHERE TT_pdf_xobject.obj_stream = pdfStream NO-LOCK)
      THEN RUN pdf_Load_XObjects (pdfStream,L_Encrypt).

      /* Load External Pages */
      IF CAN-FIND( FIRST TT_pdf_external 
                   WHERE TT_pdf_external.obj_stream = pdfStream NO-LOCK)
      THEN RUN pdf_Load_External (pdfStream,L_Encrypt).

      /* Load Annotations */
      IF CAN-FIND( FIRST TT_pdf_annot
                   WHERE TT_pdf_annot.obj_stream = pdfStream NO-LOCK)
      THEN RUN pdf_Load_Links (pdfStream, L_Encrypt).

      IF L_Encrypt THEN DO:
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Encrypt", ?, 0, "").
        TT_pdf_stream.obj_encryptdict = pdf_inc_ObjectSequence.

        RUN pdf_EncryptDict(pdfStream, pdf_inc_ObjectSequence).
      END.

      RUN pdf_Resources (pdfStream, OUTPUT pdf-Res-Object).
      RUN pdf_Content (pdfStream, pdf-Res-Object, L_Encrypt).

      RUN pdf_Xref (pdfStream, L_Encrypt).

      IF TT_pdf_stream.obj_Last <> "" THEN
        RUN VALUE(TT_pdf_Stream.obj_Last) IN TT_pdf_stream.obj_CallProcLast NO-ERROR.

    END.

  OUTPUT STREAM S_pdf_inc CLOSE.

  /* 14-SEP-2012 jcc: moved after the pdf_load_* calls so that they can also create error messages */
  RUN pdf_messages (pdfStream).
  cMessage = RETURN-VALUE.

  /* Ensure we clean up some memory */
  /* SET-SIZE(mContent) = 0. */
  /* SET-SIZE(mHolder) = 0. */

  IF VALID-HANDLE(h_PDF-tool) THEN
    DELETE PROCEDURE h_PDF-tool.

  IF VALID-HANDLE(h_PDF-template) THEN
    DELETE PROCEDURE h_PDF-template.

  RUN ReleaseZlib.

  IF VALID-HANDLE(h_PDF-Encrypt) THEN DO:
    RUN ReleasePDFencryptlib IN h_PDF-encrypt.
    DELETE PROCEDURE h_PDF-encrypt.
  END.

  IF VALID-HANDLE(h_PDF-ParseFont) THEN
    DELETE PROCEDURE h_PDF-ParseFont.

  OUTPUT STREAM S_pdf_out CLOSE. /* 03-JUN-2016 jcc: this is needed because pdf_Content calls pdf_Set_page which leaves the last document page Content file open */

  /* 03-OCT-2012 jcc: move from PSP to PLOP (successor to PSP) */
  IF TT_pdf_stream.obj_encrypt THEN DO:
    /* RUN PDFpsp.w */
    RUN PDFplop.w
        (INPUT TT_pdf_stream.obj_file,
         INPUT TT_pdf_stream.obj_master,
         INPUT TT_pdf_stream.obj_user,
         INPUT TT_pdf_stream.obj_access,
         INPUT TT_pdf_stream.obj_compat,
         INPUT TT_pdf_stream.obj_mode,
         INPUT TT_pdf_stream.obj_silent,
         OUTPUT l_EncryptStatus).
  END.

  /* To Debug page content - comment this code out 
     Delete any pre-existing temp files for stream */
  /* MESSAGE "check temp files in" session:temp-directory
      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF OPSYS = "UNIX" THEN
    OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + TT_pdf_stream.obj_UniqueID + "*.txt").
  ELSE
    OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + TT_pdf_Stream.obj_UniqueID + "*.txt").

  RUN pdf_reset_stream(pdfStream).

  /* 24-FEB-2010 jcc: return error messages instead of a new output parameter for compatibility */
  RETURN cMessage.
  
END PROCEDURE. /* pdf_Close */

FUNCTION pdf_PageFooter RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                        INPUT pdfProcHandle AS HANDLE,
                                        INPUT pdfFooterProc AS CHARACTER):

  DEFINE BUFFER B_TT_pdf_stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream ' + QUOTER(pdfStream)" &return=YES &return-value=FALSE}.

  ASSIGN B_TT_pdf_stream.obj_CallProcFooter = pdfProcHandle
         B_TT_pdf_stream.obj_Footer         = pdfFooterProc
         B_TT_pdf_stream.obj_Footer_flag    = YES. /* 23-JUN-2015 jcc: remember that the user has called pdf_PageFooter */

  RETURN TRUE.
END FUNCTION. /* pdf_PageFooter */

FUNCTION pdf_PageHeader RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                        INPUT pdfProcHandle AS HANDLE,
                                        INPUT pdfHeaderProc AS CHARACTER):
  DEFINE BUFFER B_TT_pdf_stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream ' + QUOTER(pdfStream)" &return=YES &return-value=FALSE}.

  ASSIGN B_TT_pdf_stream.obj_CallProcHeader = pdfProcHandle
         B_TT_pdf_stream.obj_Header         = pdfHeaderProc.

  RETURN TRUE.
END FUNCTION. /* pdf_PageHeader */

FUNCTION pdf_LastProcedure RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                           INPUT pdfProcHandle AS HANDLE,
                                           INPUT pdfLastProc   AS CHARACTER):

  DEFINE BUFFER B_TT_pdf_stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream ' + QUOTER(pdfStream)" &return=YES &return-value=FALSE}.

  ASSIGN B_TT_pdf_stream.obj_CallProcLast = pdfProcHandle
         B_TT_pdf_stream.obj_Last         = pdfLastProc.

  RETURN TRUE.

END FUNCTION. /* pdf_LastProcedure */

PROCEDURE pdf_set_parameter:
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfParameter AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue     AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_pdf_param FOR TT_pdf_param.
  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  DEFINE VARIABLE L_Integer       AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Decimal       AS DECIMAL NO-UNDO.

  DEFINE VARIABLE L_Error         AS LOGICAL INIT FALSE.
  DEFINE VARIABLE L_ErrorMsg      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Valid_params  AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfParameter) + "," + QUOTER(pdfValue)). &ENDIF

  L_Valid_params = "Compress,Encrypt,UserPassword,MasterPassword,EncryptKey,EncryptAlgorithm,CodePage"
                 + ",AllowPrint,AllowCopy,AllowModify,AllowAnnots,AllowForms"
                 + ",AllowExtract,AllowAssembly"
                 + ",LineSpacer"
                 + ",HideToolbar,HideMenubar,HideWindowUI,FitWindow,CenterWindow,DisplayDocTitle"
                 + ",PageMode,PageLayout"
                 + ",UseExternalPageSize"
                 + ",ScaleX,ScaleY"
                 + ",Version,pdfVersion"
                 + ",UseTags,BoldFont,ItalicFont,BoldItalicFont,DefaultFont,DefaultColor,LinkColor,ItalicCount,BoldCount,UnderlineCount,StrikeCount,LinkCount,ColorLevel" /* tag related */
                 + ",retainAcroForm" /* 23-AUG-2012 jcc: (TRUE/FALSE) retain the form from the external document */ /* 28-MAY-2013 jcc: obsoleted by formFlatten */
                 + ",formFlatten" /* 26-MAY-2013 jcc: can-do list of widgets to flatten (default value "*") */
                 + ",formFlattenWithDefaultValues" /* 25-MAY-2013 jcc: widget names for which retain the default value when flattening the form */
                 + ",retainAnnots" /* 28-AUG-2012 jcc: list of subtypes of annotations to retain (Text,Link,FreeText,Line,Square,Circle,Polygon,PolyLine,Highlight,Underline,Squiggly,StrikeOut,Stamp,Caret,Ink,Popup,FileAttachment,Sound,Movie,Widget,Screen */
                 + ",drawFormFieldRect" /* 21-AUG-2012 jcc: (R,G,B) draw a box around the form fields (use e.g. to debug) */
                 + ",optimizePDFOutput" /* 02-AUG-2013 jcc: as it sets a global variable, all streams are impacted */
                 + ",unitTest" /* 09-AUG-2013 jcc: idem ; used for prounit tests */
                 + ",defaultHeader,defaultFooter,headerNotOn1stPage,headerLogo,headerLine1,headerLine2,headerSeparator,footerLine1,footerSeparator" /* 08-APR-2014 jcc: default header/footer */
                 + ",reuseExternal" /* 20-MAY-2014 jcc: do not delete external pdf files & temp-tables */
                 + ",insertPageMode" /* 01-APR-2015 jcc: when reaching page bottom: new page or insert? */
                 + ",usePdfCache" /* 05-OCT-2015 jcc: activate the external pdf cache; default value: TRUE */
                 .

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF (NOT pdfParameter BEGINS "tmp" AND NOT pdfParameter BEGINS "TagColor:")
  AND LOOKUP(pdfParameter,L_valid_params) = 0 THEN {pdferror.i &msg="'Invalid parameter (' + pdfParameter + ') trying to be set!'" &return=YES}.

  IF NOT pdfParameter BEGINS "tmp" 
  AND NOT pdfParameter BEGINS "TagColor:" THEN DO:
    CASE pdfParameter:
      WHEN "Compress" OR WHEN "Encrypt" OR WHEN "AllowPrint"
        OR WHEN "AllowCopy" OR WHEN "AllowModify" OR WHEN "AllowAnnots"
        OR WHEN "AllowForms" OR WHEN "AllowExtract" OR WHEN "AllowAssembly" 
        OR WHEN "HideToolBar" OR WHEN "HideMenuBar" OR WHEN "HideWindowUI"
        OR WHEN "FitWindow" OR WHEN "CenterWindow" OR WHEN "DisplayDocTitle"
        OR WHEN "UseExternalPageSize" OR WHEN "reuseExternal" OR WHEN "UseTags"
        OR WHEN "retainAcroForm" OR WHEN "optimizePDFOutput" OR WHEN "unitTest"
        OR WHEN "defaultHeader" OR WHEN "headerSeparator" OR WHEN "headerNotOn1stPage"
        OR WHEN "defaultFooter" OR WHEN "footerSeparator" OR WHEN "usePdfCache"
        THEN DO:
          IF pdfValue <> "TRUE" AND pdfValue <> "FALSE" AND pdfValue <> "" THEN
            ASSIGN L_Error    = TRUE
                   L_ErrorMsg = "Only TRUE, FALSE or blank allowed for '" + pdfParameter + "' Parameter!".

        CASE pdfParameter:
            WHEN "Encrypt" THEN DO:
                IF pdfValue = "TRUE" THEN DO:
                    IF SEARCH("{&PDFDIR}lib/pdfencrypt.p") = ? THEN
                        ASSIGN L_Error    = TRUE
                               L_ErrorMsg = "Cannot activate encryption: pdfencrypt is not installed.".
                    /* Default the encryption to RC4 40 Bits - Rev 2 */
                    RUN pdf_set_parameter(pdfStream,"EncryptAlgorithm","RC4").
                    RUN pdf_set_parameter(pdfStream,"EncryptKey","40").
                END.
            END.
            /* 28-MAY-2013 jcc: retainAcroForm = "TRUE" => formFlatten = "" */
            WHEN "retainAcroForm" THEN
                IF pdfValue = "TRUE" THEN RUN pdf_set_parameter(pdfStream,"formFlatten","").
            /* 02-AUG-2013 jcc */
            WHEN "optimizePDFOutput" THEN
                glNoOptimize = NOT pdfValue = "TRUE".
            /* 09-AUG-2013 jcc */
            WHEN "unitTest" THEN
                glUnitTest = pdfValue = "TRUE".
            /* 08-APR-2014 jcc */
            WHEN "defaultHeader" THEN
                pdf_PageHeader(pdfStream, THIS-PROCEDURE:HANDLE, "pdf_default_header").
            WHEN "defaultFooter" THEN DO:
                pdf_PageFooter(pdfStream, THIS-PROCEDURE:HANDLE, "pdf_default_footer").
                IF pdf_BottomMargin(pdfStream) < 10 THEN
                    RUN pdf_set_BottomMargin(pdfStream, 10).
            END.
        END CASE.

      END.

      WHEN "EncryptKey" THEN DO:
        /* Currently only allow for 40-Bit Encryption */
        IF pdfValue <> "40" AND pdfValue <> "128" THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Only a value of 40 or 128 is allowed for the '" + pdfParameter + "' Parameter!".
        IF pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "AES" AND pdfValue = "40" THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Only a value of 128 is allowed for the '" + pdfParameter + "' Parameter for AES encryption!".
      END.

      WHEN "EncryptAlgorithm" THEN DO:
        IF pdfValue <> "RC4" AND pdfValue <> "AES" THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Supported encryption algorithms are RC4 or AES ('" + pdfParameter + "' Parameter)".
        ELSE IF pdfValue = "AES" THEN DO:
          IF INTEGER(ENTRY(1, PROVERSION, ".")) < 10 THEN
              ASSIGN L_Error    = TRUE
                     L_ErrorMsg = "AES encryption algorithm is supported starting from OpenEdge 10 ('" + pdfParameter + "' Parameter)".
          ELSE
            RUN pdf_set_parameter(pdfStream,"EncryptKey","128"). /* currently only AES-128 is supported */
            RUN pdf_set_MinPdfVersion(pdfStream,"1.5").
        END.
      END.

      WHEN "UserPassword" OR WHEN "MasterPassword" THEN DO:
        IF LENGTH(pdfValue, "character":u) > 32 THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Password string cannot be greater than 32 characters for '" + pdfParameter + "' Parameter!".
      END.

      WHEN "LineSpacer" THEN DO:
        L_Integer = INT(pdfValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "'LineSpacer' Parameter must be an integer value!".
      END.

      WHEN "ScaleX" OR WHEN "ScaleY" THEN DO:
        L_Decimal = DEC(pdfValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "'Scale X/Y' Parameters must be decimal values!".
        /* 09-JUL-2016 jcc: fix regresion from v3: update text matrix */
        RUN setTextOperator(pdfStream, "Tm", 
                            dec2string(IF pdfParameter = "ScaleX" THEN L_Decimal / pdf_ScaleX(pdfStream) ELSE 1)
                            + " 0 0 "
                            + dec2string(IF pdfParameter = "ScaleY" THEN L_Decimal / pdf_ScaleY(pdfStream) ELSE 1) + " "
                            + dec2string(pdf_TextX(pdfStream)) + " "
                            + dec2string(pdf_TextY(pdfStream))).
      END.

      WHEN "PageMode" THEN DO:
        IF LOOKUP(pdfValue,"UseNone,UseOutlines,UseThumbs,FullScreen") = 0 THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Invalid entry (" + pdfValue + ") used for PageMode parameter!".
      END. /* PageMode */

      WHEN "PageLayout" THEN DO:
        IF LOOKUP(pdfValue,"SinglePage,OneColumn,TwoColumnLeft,TwoColumnRight") = 0 THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Invalid entry (" + pdfValue + ") used for PageLayout parameter!".
      END. /* PageMode */

      WHEN "LinkColor" OR WHEN "drawFormFieldRect" THEN DO:
        IF NUM-ENTRIES(pdfValue) <> 3 THEN
            ASSIGN L_Error    = TRUE
                   L_ErrorMsg = "Invalid value (" + pdfValue + ") used for " + pdfParameter + " parameter! Should be R,G,B.".
      END.

      WHEN "insertPageMode" THEN DO:
          IF LOOKUP(pdfValue, "insert,append,next") = 0 THEN
              ASSIGN L_Error    = TRUE
                     L_ErrorMsg = "Invalid value (" + pdfValue + ") used for " + pdfParameter + " parameter! Should be append or insert or next.".
      END.

    END CASE.

    IF L_Error THEN {pdferror.i &msg=L_ErrorMsg &return=YES}.
  END.

  RUN _pdf_set_parameter_priv (pdfStream, pdfParameter, pdfValue).

END. /* pdf_set_parameter */

/* 29-AUG-2012 jcc: new */
PROCEDURE _pdf_set_parameter_priv: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pdfParameter AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pdfValue     AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER TT_pdf_param FOR TT_pdf_param.
    
    FIND TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                        AND TT_pdf_param.obj_parameter = pdfParameter NO-ERROR.
    IF NOT AVAILABLE TT_pdf_param THEN DO:
        CREATE TT_pdf_param.
        ASSIGN TT_pdf_param.obj_stream    = pdfStream
               TT_pdf_param.obj_parameter = pdfParameter.
    END.
    ASSIGN TT_pdf_param.obj_value = pdfValue.
END. /* _pdf_set_parameter_priv */
/* 29-AUG-2012 jcc: end */

/* 20-OCT-2011 jcc: added */
PROCEDURE pdf_set_MinPdfVersion: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfVersion AS CHARACTER   NO-UNDO.

    IF pdfVersion > pdf_get_parameter(pdfStream, "pdfVersion") THEN
        RUN pdf_set_parameter(pdfStream, "pdfVersion", pdfVersion).

END PROCEDURE.

/* 25-JUL-2012 jcc: add the 2 following procedures */
PROCEDURE pdf_incr_parameter:
    DEFINE INPUT  PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcParamName AS CHARACTER   NO-UNDO.
    RUN pdf_set_parameter(pdfStream,
                          pcParamName,
                          STRING(INTEGER(pdf_get_parameter(pdfStream,
                                                           pcParamName)) + 1)).
END.

PROCEDURE pdf_decr_parameter:
    DEFINE INPUT  PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcParamName AS CHARACTER   NO-UNDO.
    RUN pdf_set_parameter(pdfStream,
                          pcParamName,
                          STRING(INTEGER(pdf_get_parameter(pdfStream,
                                                           pcParamName)) - 1)).
END.
/* jcc: end */

/* igc - added Novermber 17, 2003 */
/* Encrypt using PLOP */
/* 03-OCT-2012 jcc: move from PSP to PLOP (successor to PSP) */
PROCEDURE pdf_Encrypt:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfMaster  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfUser    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAccess  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfCompat  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfMode    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSilent  AS LOGICAL   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  DEFINE VARIABLE l_Loop        AS INTEGER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfMaster) + "," + QUOTER(pdfUser) + "," + QUOTER(pdfAccess) + "," + QUOTER(pdfCompat) + "," + QUOTER(pdfMode) + "," + STRING(pdfSilent)). &ENDIF

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  /* if either of the passwords have been supplied ensure that it doesn't match
     the other -- they must be different */
  IF (pdfMaster <> "" OR pdfUser <> "") AND ENCODE(pdfMaster) = ENCODE(pdfUser) THEN
      {pdferror.i &msg="'Master and User Passwords must be different!'" &return=YES}.

  IF pdfMaster = "" AND pdfUser <> "" THEN
      {pdferror.i &msg="'User password requires a Master password to be defined!'" &return=YES}.

  /* Ensure that the correct Access permissions have been specified -- if not
     error out and do not perform encryption */
  DO l_Loop = 1 TO NUM-ENTRIES(pdfAccess):
    IF LOOKUP(ENTRY(l_Loop,pdfAccess),
              "noprint,nohiresprint,nomodify,nocopy,noannots,noforms,noaccessible,noassemble") = 0 THEN
        {pdferror.i &msg="'Invalid Access Type passed - ' + ENTRY(l_Loop,pdfAccess)" &return=YES}.
  END.

  IF LOOKUP(STRING(pdfCompat),"1.4,1.5,1.6,1.7,2.0") = 0 THEN {pdferror.i &msg="'Invalid pdf version passed (must be 1.4 to 1.7 or 2.0)!'" &return=YES}.

  IF LOOKUP(pdfMode,"COM,SHARED,OS") = 0 THEN {pdferror.i &msg="'Invalid MODE passed - only COM, SHARED or OS allowed!'" &return=YES}.

  ASSIGN B_TT_pdf_stream.obj_encrypt = TRUE
         B_TT_pdf_stream.obj_master  = pdfMaster
         B_TT_pdf_stream.obj_user    = pdfUser
         B_TT_pdf_stream.obj_access  = pdfAccess
         B_TT_pdf_stream.obj_compat  = pdfCompat
         B_TT_pdf_stream.obj_mode    = pdfMode
         B_TT_pdf_stream.obj_silent  = pdfSilent.

END. /* pdf_Encrypt */

/* igc - added November 25, 2003 */
PROCEDURE pdf_Bookmark:
  DEFINE INPUT  PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pdfTitle     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pdfParent    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER pdfExpand    AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER pdfBookmark  AS INTEGER   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_bookmark FOR TT_pdf_bookmark.
  DEFINE BUFFER BB_TT_pdf_bookmark FOR TT_pdf_bookmark.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfTitle) + "," + STRING(pdfParent) + "," + STRING(pdfExpand) + "," + STRING(pdfBookmark)). &ENDIF

  FIND FIRST TT_pdf_stream
       WHERE TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF pdfParent <> 0
  AND NOT CAN-FIND(FIRST TT_pdf_bookmark
                   WHERE TT_pdf_bookmark.obj_stream  = pdfStream
                     AND TT_pdf_bookmark.book_parent = 0 NO-LOCK) THEN
    {pdferror.i &msg="'Trying to add a Bookmark without having a Main Level defined!'" &return=YES}.

  /* Find last Bookmark --- used to increment the unique Bookmark Number */
  FIND LAST TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStream NO-LOCK NO-ERROR.

  CREATE B_TT_pdf_bookmark.
  ASSIGN B_TT_pdf_bookmark.obj_stream  = pdfStream
         B_TT_pdf_bookmark.book_nbr    = IF NOT AVAILABLE TT_pdf_bookmark THEN 1
                                         ELSE TT_pdf_bookmark.book_nbr + 1
         B_TT_pdf_bookmark.book_title  = pdfTitle
         B_TT_pdf_bookmark.book_parent = pdfParent
         B_TT_pdf_bookmark.book_expand = pdfExpand
         B_TT_pdf_bookmark.book_page   = pdf_page(pdfstream)
         B_TT_pdf_bookmark.book_Y      = pdf_TextY(pdfstream)
                                       + pdf_PointSize(pdfStream)
         B_TT_pdf_bookmark.in_transaction = pdf_in_transaction(pdfStream).

  pdfBookmark = B_TT_pdf_bookmark.book_nbr.

  /* Now that we've added the Bookmark, find the parent and increment the
     parents child counter.  But only when the Parent is not 0 (Main Level) */
  IF pdfParent <> 0 THEN DO:
    FIND FIRST BB_TT_pdf_bookmark
         WHERE BB_TT_pdf_bookmark.obj_stream = pdfStream
           AND BB_TT_pdf_bookmark.book_nbr   = pdfParent NO-ERROR.
    IF AVAIL BB_TT_pdf_bookmark THEN DO:
      IF BB_TT_pdf_bookmark.book_expand THEN
        BB_TT_pdf_bookmark.book_child = BB_TT_pdf_bookmark.book_child + 1.
      ELSE
        BB_TT_pdf_bookmark.book_child = BB_TT_pdf_bookmark.book_child - 1.
    END.
  END. /* Increment Parent Children */

END. /* pdf_Bookmark */

PROCEDURE pdf_process_bookmarks:  /* PRIVATE */
  DEFINE INPUT PARAMETER pStream          AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pParentNbr       AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pParentObj       AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdf_OutlinesDict AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pdf_OutlinesLast AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pEncrypt         AS LOGICAL     NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Bookmark    FOR TT_pdf_bookmark.
  DEFINE BUFFER LB_TT_pdf_Bookmark   FOR TT_pdf_bookmark.
  DEFINE BUFFER LBB_TT_pdf_Bookmark  FOR TT_pdf_bookmark.
  DEFINE BUFFER LBBB_TT_pdf_Bookmark FOR TT_pdf_bookmark.

  DEFINE VARIABLE L_Count      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_Last       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_PageObject AS INTEGER   NO-UNDO.
  DEFINE VARIABLE m_EncryptKey AS MEMPTR    NO-UNDO.

  FIND FIRST LBB_TT_pdf_Bookmark
       WHERE LBB_TT_pdf_Bookmark.obj_stream = pStream
         AND LBB_TT_pdf_bookmark.book_nbr   = pParentNbr NO-LOCK NO-ERROR.

  FOR EACH LB_TT_pdf_bookmark
     WHERE LB_TT_pdf_bookmark.obj_stream  = pStream
       AND LB_TT_pdf_bookmark.book_parent = pParentNbr
       BREAK BY LB_TT_pdf_bookmark.book_nbr:

    /* 14-OCT-2014 jcc: move below, else the "NEXT" statement below would result in a corrupt xref */
    /* ObjectSequence(pStream, pdf_inc_ObjectSequence + 1, "BookMark", ?, 0, ""). */
    /* LB_TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence. */

    /* Find the associated Page Dictionary Object Number */
    FIND FIRST TT_pdf_object
         WHERE TT_pdf_object.obj_stream = pStream
           AND TT_pdf_object.obj_page   = LB_TT_pdf_bookmark.book_page
           AND TT_pdf_object.obj_desc   = "PageDefinition" /* 14-OCT-2014 jcc: else it could pick a deferred reference object */
           NO-ERROR.
    IF NOT AVAIL TT_pdf_object THEN NEXT.
    L_PageObject = TT_pdf_object.obj_nbr.

    ObjectSequence(pStream, pdf_inc_ObjectSequence + 1, "BookMark", ?, 0, "").
    LB_TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence.
    PUT STREAM S_pdf_inc UNFORMATTED
        LB_TT_pdf_Bookmark.book_obj " 0 obj" CHR(13)
        "<<" CHR(13)
        "/Title ".
    RUN putString(pStream, "(", LB_TT_pdf_bookmark.book_title, NO, YES, pEncrypt, LB_TT_pdf_Bookmark.book_obj, m_EncryptKey).

    PUT STREAM S_pdf_inc UNFORMATTED
        CHR(13) "/Parent " LBB_TT_pdf_Bookmark.book_obj " 0 R" CHR(13).

   /* Determine if a Previous Bookmark exists on the same level */
    FIND LAST LBBB_TT_pdf_Bookmark
         WHERE LBBB_TT_pdf_Bookmark.obj_stream  = pStream
           AND LBBB_TT_pdf_Bookmark.book_parent = pParentNbr
           AND LBBB_TT_pdf_Bookmark.book_nbr    < LB_TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL LBBB_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Prev " LBBB_TT_pdf_Bookmark.book_obj " 0 R" CHR(13).

   /* Determine if a Following (Next) Bookmark exists on the same level */
    FIND FIRST LBBB_TT_pdf_Bookmark
         WHERE LBBB_TT_pdf_Bookmark.obj_stream  = pStream
           AND LBBB_TT_pdf_Bookmark.book_parent = pParentNbr
           AND LBBB_TT_pdf_Bookmark.book_nbr    > LB_TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL LBBB_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Next " pdf_OutlinesDict + LBBB_TT_pdf_Bookmark.book_nbr " 0 R" CHR(13).

    /* If Children are associated with this Bookmark then add some processing */
    IF LB_TT_pdf_bookmark.book_child <> 0 THEN DO:
      FIND LAST B_TT_pdf_Bookmark
          WHERE B_TT_pdf_Bookmark.obj_stream  = pStream
            AND B_TT_pdf_Bookmark.book_parent = LB_TT_pdf_bookmark.book_nbr
            NO-LOCK NO-ERROR.
      IF AVAIL B_TT_pdf_bookmark THEN DO:
        L_Last = pdf_OutlinesDict + B_TT_pdf_Bookmark.book_nbr.
      END.
      ELSE
        L_Last = pdf_OutlinesLast.

      PUT STREAM S_pdf_inc UNFORMATTED
          "~/First " LB_TT_pdf_Bookmark.book_obj + 1 " 0 R" CHR(13)
          "~/Last "  L_Last " 0 R" CHR(13)
          "~/Count " LB_TT_pdf_Bookmark.book_child CHR(13).

    END. /* Child <> 0 */

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Dest [ " L_PageObject " 0 R /XYZ 0 " LB_TT_pdf_bookmark.book_Y " 0 ]"
        CHR(13)
         ">>" CHR(13)
        "endobj" CHR(13).

    SET-SIZE(m_EncryptKey) = 0.

    IF LB_TT_pdf_Bookmark.book_child <> 0 THEN
      RUN pdf_process_bookmarks(pStream, LB_TT_pdf_bookmark.book_nbr, LBB_TT_pdf_bookmark.book_obj, 
                                pdf_OutlinesDict, pdf_OutlinesLast, pEncrypt).
  END.

END. /* pdf_process_bookmarks */

PROCEDURE pdf_load_bookmarks:  /* PRIVATE */
  DEFINE INPUT  PARAMETER pdfStream        AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT PARAMETER pdf_OutlinesDict AS INTEGER     NO-UNDO.

  DEFINE VARIABLE L_Encrypt        AS LOGICAL   NO-UNDO INIT FALSE.
  DEFINE VARIABLE L_Last           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_PageObject     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE m_EncryptKey     AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE pdf_OutlinesLast AS INTEGER   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_bookmark FOR TT_pdf_bookmark.

  IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN
    L_Encrypt = TRUE.

  FOR EACH TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStream
                             AND TT_pdf_bookmark.book_parent = 0
     BREAK BY TT_pdf_bookmark.book_nbr:

    IF FIRST(TT_pdf_bookmark.book_nbr) THEN DO:
      ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "BookMark", ?, 0, "").
      TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence.
      pdf_OutlinesDict = pdf_inc_ObjectSequence.

      /* Determine the total number of Bookmarks in the document */
      FIND LAST B_TT_pdf_bookmark WHERE B_TT_pdf_bookmark.obj_stream = pdfStream NO-LOCK NO-ERROR.
      pdf_OutlinesLast = pdf_OutlinesDict + B_TT_pdf_bookmark.book_nbr.

      /* Output the Bookmark Dictionary */
      PUT STREAM S_pdf_inc UNFORMATTED
         pdf_OutlinesDict " 0 obj" CHR(13)
          "<< " CHR(13)
          "/Type /Outlines" CHR(13)
          "/First " pdf_OutlinesDict + 1 " 0 R" CHR(13)
          "/Last " pdf_OutlinesLast " 0 R" CHR(13)
          "/Count " B_TT_pdf_Bookmark.book_nbr CHR(13)
          ">>" CHR(13)
          "endobj" CHR(13).

    END. /* FIRST */

    /* 14-OCT-2014 jcc: see above */
    /* ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "BookMark", ?, 0, ""). */
    /* TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence. */

    /* Find the associated Page Dictionary Object Number */
    FIND FIRST TT_pdf_object
         WHERE TT_pdf_object.obj_stream = pdfStream
           AND TT_pdf_object.obj_page   = TT_pdf_bookmark.book_page
           AND TT_pdf_object.obj_desc   = "PageDefinition" /* 14-OCT-2014 jcc: see above */
           NO-ERROR.
    IF NOT AVAIL TT_pdf_object THEN NEXT.
    L_PageObject = TT_pdf_object.obj_nbr.

    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "BookMark", ?, 0, "").
    TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence.
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_Bookmark.book_obj " 0 obj" CHR(13)
        "<<" CHR(13)
        "/Title ".
    RUN putString(pdfStream, "(", TT_pdf_bookmark.book_title, NO, YES, l_Encrypt, TT_pdf_Bookmark.book_obj, m_EncryptKey).
    PUT STREAM S_pdf_inc UNFORMATTED CHR(13).

    PUT STREAM S_pdf_inc UNFORMATTED
         "/Parent " pdf_OutlinesDict " 0 R" CHR(13).

   /* Determine if a Previous Bookmark exists on the same level */
    FIND LAST B_TT_pdf_Bookmark
         WHERE B_TT_pdf_Bookmark.obj_stream  = pdfStream
           AND B_TT_pdf_Bookmark.book_parent = 0
           AND B_TT_pdf_Bookmark.book_nbr    < TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL B_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
         "/Prev " B_TT_pdf_Bookmark.book_obj " 0 R" CHR(13).

   /* Determine if a Following (Next) Bookmark exists on the same level */
    FIND FIRST B_TT_pdf_Bookmark
         WHERE B_TT_pdf_Bookmark.obj_stream  = pdfStream
           AND B_TT_pdf_Bookmark.book_parent = 0
           AND B_TT_pdf_Bookmark.book_nbr    > TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL B_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "~/Next " pdf_OutlinesDict + B_TT_pdf_Bookmark.book_nbr " 0 R" CHR(13).

    /* If Children are associated with this Bookmark then add some processing */
    IF TT_pdf_bookmark.book_child <> 0 THEN DO:
      FIND LAST B_TT_pdf_Bookmark
          WHERE B_TT_pdf_Bookmark.obj_stream  = pdfStream
            AND B_TT_pdf_Bookmark.book_parent = TT_pdf_bookmark.book_nbr
            NO-LOCK NO-ERROR.
     IF AVAIL B_TT_pdf_bookmark THEN DO:
       L_Last = pdf_OutlinesDict + B_TT_pdf_Bookmark.book_nbr.
     END.
     ELSE
       L_Last = pdf_OutlinesLast.

     PUT STREAM S_pdf_inc UNFORMATTED
          "~/First " TT_pdf_Bookmark.book_obj + 1 " 0 R" CHR(13)
          "~/Last "  L_Last " 0 R" CHR(13)
          "~/Count " TT_pdf_Bookmark.book_child CHR(13).
    END.

    SET-SIZE(m_EncryptKey) = 0.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Dest [ " L_PageObject " 0 R /XYZ 0 " TT_pdf_bookmark.book_Y " 0 ]" CHR(13)
        ">>" CHR(13)
        "endobj" CHR(13).

    RUN pdf_process_bookmarks(pdfStream, TT_pdf_bookmark.book_nbr, TT_pdf_bookmark.book_obj,
                              pdf_OutlinesDict, pdf_OutlinesLast, L_Encrypt).

  END. /* each TT_pdf_annot */

  SET-SIZE(m_EncryptKey) = 0.
END. /* pdf_load_bookmarks */

/* ************************************************* */
/*                  PEKI's procedures                */
/* ************************************************* */
PROCEDURE pdf_merge_stream :
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfNbrCopies  AS INTEGER     NO-UNDO.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStreamFrom) + "," + QUOTER(pdfStreamTo) + "," + STRING(pdfNbrCopies)). &ENDIF

    /* Empty Temp-Table */
    FOR EACH TT-Merge-Pages:
       DELETE TT-Merge-Pages.
    END.

    /* No Merge done when error is found */
    IF CAN-FIND (FIRST TT_pdf_error
                 WHERE TT_pdf_error.obj_stream = pdfStreamFrom NO-LOCK)
    OR TRIM(pdfStreamFrom) = ""
    OR TRIM(pdfStreamTo  ) = ""
       THEN RETURN.

    IF pdfNbrCopies <= 0
       THEN ASSIGN pdfNbrCopies = 1.

    /* 04-OCT-2013 jcc: close the output context of both pdf streams */
    RUN _pdf_close_output_context(pdfStreamFrom).
    RUN _pdf_close_output_context(pdfStreamTo).

    /* 04-OCT-2013 jcc: also merge external content (pdf_open_pdf) */
    RUN pdf_merge_stream_external(INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_content(INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_page   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_page_use(INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_param  (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_image  (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_diff   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_link   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_book   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_Fonts  (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).

END PROCEDURE. /* pdf_merge_stream */

/* 04-OCT-2013 jcc: external resources when pdf_open_pdf() has been used */
PROCEDURE pdf_merge_stream_external: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_ext       FOR TT_pdf_ext.
    DEFINE BUFFER bTT_pdf_ext      FOR TT_pdf_ext.
    DEFINE BUFFER TT_pdf_external  FOR TT_pdf_external.
    DEFINE BUFFER bTT_pdf_external FOR TT_pdf_external.
    DEFINE BUFFER TT_Resource      FOR TT_Resource.
    DEFINE BUFFER bTT_Resource     FOR TT_Resource.
    DEFINE BUFFER TT_Object        FOR TT_Object.
    DEFINE BUFFER bTT_Object       FOR TT_Object.
    DEFINE BUFFER TT_pdf_font      FOR TT_pdf_font.
    DEFINE BUFFER bTT_pdf_font     FOR TT_pdf_font.
    DEFINE BUFFER TT_Info          FOR TT_Info.
    DEFINE BUFFER bTT_Info         FOR TT_Info.

    FOR EACH TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStreamFrom:
        BUFFER-COPY TT_pdf_ext  EXCEPT TT_pdf_ext.obj_stream
                 TO bTT_pdf_ext ASSIGN bTT_pdf_ext.obj_stream = pdfStreamTo.
    END.

    FOR EACH TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStreamFrom:

        BUFFER-COPY TT_pdf_external  EXCEPT TT_pdf_external.obj_stream
                 TO bTT_pdf_external ASSIGN bTT_pdf_external.obj_stream = pdfStreamTo.

        FOR EACH TT_Resource WHERE TT_Resource.obj_stream = pdfStreamFrom
          AND TT_Resource.pdf_id = TT_pdf_external.pdf_id :
            BUFFER-COPY TT_Resource  EXCEPT TT_Resource.obj_stream
                     TO bTT_Resource ASSIGN bTT_Resource.obj_stream = pdfStreamTo.
        END.

        FOR EACH TT_Object WHERE TT_Object.obj_stream = pdfStreamFrom
          AND TT_Object.pdf_id = TT_pdf_external.pdf_id:
            BUFFER-COPY TT_Object  EXCEPT TT_Object.obj_stream
                     TO bTT_Object ASSIGN bTT_Object.obj_stream = pdfStreamTo.
        END.

        FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStreamFrom
          AND TT_pdf_font.pdf_id     = TT_pdf_external.pdf_id:
            BUFFER-COPY TT_pdf_font  EXCEPT TT_pdf_font.obj_stream
                     TO bTT_pdf_font ASSIGN bTT_pdf_font.obj_stream = pdfStreamTo.
        END.

        /* 30-SEP-2015 jcc: also copy TT_Info */
        FOR EACH TT_Info WHERE TT_Info.obj_stream = pdfStreamFrom
          AND TT_Info.pdf_id = TT_pdf_external.pdf_id:
            BUFFER-COPY TT_Info  EXCEPT TT_Info.obj_stream
                     TO bTT_Info ASSIGN bTT_Info.obj_stream = pdfStreamTo.
        END.
    END.

END PROCEDURE. /* pdf_merge_stream_external */

PROCEDURE pdf_merge_stream_content : /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfNbrCopies  AS INTEGER     NO-UNDO.

    DEFINE VARIABLE vCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vLastPage AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vPage     AS INTEGER   NO-UNDO.

    DEFINE BUFFER to_TT_pdf_stream   FOR TT_pdf_stream.
    DEFINE BUFFER from_TT_pdf_stream FOR TT_pdf_stream.

    FIND to_TT_pdf_stream
         WHERE to_TT_pdf_stream.obj_stream = pdfStreamTo NO-LOCK NO-ERROR.
    IF NOT AVAIL to_TT_pdf_stream THEN DO:
      RUN pdf_error(pdfStreamTo,"pdf_merge_stream_content","Cannot find 'To' Stream = " + pdfStreamTo).
      RETURN.
    END.

    FIND from_TT_pdf_stream
         WHERE from_TT_pdf_stream.obj_stream = pdfStreamFrom NO-LOCK NO-ERROR.
    IF NOT AVAIL from_TT_pdf_stream THEN DO:
      RUN pdf_error(pdfStreamTo,"pdf_merge_stream_content","Cannot find 'From' Stream = " + pdfStreamFrom).
      RETURN.
    END.
    
    /* Run the Page Footer for the parent stream before merging the child 
       streams */
    OUTPUT STREAM S_pdf_out CLOSE.
    OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStreamTo, to_TT_pdf_stream.obj_UniqueID, pdf_Page(pdfStreamTo))) BINARY NO-MAP NO-CONVERT APPEND.

    IF to_TT_pdf_stream.obj_footer <> "" THEN DO:
        IF to_TT_pdf_stream.obj_CallProcFooter = THIS-PROCEDURE:HANDLE THEN
            RUN VALUE(to_TT_pdf_stream.obj_footer) IN to_TT_pdf_stream.obj_CallProcFooter (pdfStreamTo) NO-ERROR.
        ELSE
            RUN VALUE(to_TT_pdf_stream.obj_footer) IN to_TT_pdf_stream.obj_CallProcFooter NO-ERROR.
    END.

    RUN _pdf_close_output_context (pdfStreamFrom).

    OUTPUT STREAM S_pdf_out CLOSE.

    DO vCount = 1 TO pdfNbrCopies:

        /* Find Current Page */
        ASSIGN vLastPage = pdf_Page(pdfStreamTo).
        IF NOT ( vLastPage >= 1 )
           THEN ASSIGN vLastPage = 0.

        /* Now copy the pages of the 'From' stream */
        DO vPage = 1 TO pdf_Page(pdfStreamFrom):
          vLastPage = vLastPage + 1.

          OS-COPY VALUE(_get_page_content_file(pdfStreamFrom, from_TT_pdf_stream.obj_UniqueID, vPage))
                  VALUE(_get_page_content_file(pdfStreamTo,   to_TT_pdf_stream.obj_UniqueID,   vLastPage)).

          /* Save the NEW page indications for other copies */
          IF NOT CAN-FIND (TT-Merge-Pages WHERE TT-Merge-Pages.PageFrom = vPage
                                            AND TT-Merge-Pages.PageTo   = vLastPage
                                            AND TT-Merge-Pages.MergeNr  = vCount)
            THEN DO:
                 CREATE TT-Merge-Pages.
                 ASSIGN TT-Merge-Pages.PageFrom = vPage
                        TT-Merge-Pages.PageTo   = vLastPage
                        TT-Merge-Pages.MergeNr  = vCount.
            END.

            /* Replace a piece of text, for example: 'Title' to 'Copy Title' */
            /* 23-APR-2014 jcc: make use of the new possibility to replace multiple strings at once in ChangePageText */
            DEFINE VARIABLE cFromList AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cToList   AS CHARACTER   NO-UNDO.
            ASSIGN
             cFromList = ""
             cToList   = "".
            FOR EACH TT_pdf_ReplaceTxt WHERE TT_pdf_ReplaceTxt.obj_Stream = pdfStreamFrom
                                         AND TT_pdf_ReplaceTxt.MergeNr    = vCount:
                ASSIGN
                 cFromList = cFromList + CHR(2) + TT_pdf_ReplaceTxt.txt_from
                 cToList   = cToList   + CHR(2) + TT_pdf_ReplaceTxt.txt_to.
            END.
            IF cFromList > "" THEN
                RUN ChangePageText
                    (pdfStreamTo,
                     _get_page_content_file(pdfStreamTo, to_TT_pdf_stream.obj_UniqueID, vLastPage),
                     NO,
                     SUBSTRING(cFromList, 2),
                     SUBSTRING(cToList, 2)).
        END. /* each page of 'From' Stream */

        /* Set the last page */
        RUN pdf_set_Page(pdfStreamTo, vLastPage).

    END. /* END vCount = 1 TO pdfNbrCopies: */

    /* Reset the pointer to current Stream, is disturbed by calling pdf_Page function */
    FIND FIRST TT_pdf_stream
         WHERE TT_pdf_stream.obj_stream = pdfStreamFrom NO-LOCK NO-ERROR.
    
END PROCEDURE. /* pdf_merge_stream_content */

PROCEDURE pdf_merge_stream_page: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfNbrCopies  AS INTEGER     NO-UNDO.

    DEFINE BUFFER B_TT_pdf_page FOR TT_pdf_page.

    DEFINE VARIABLE vCount AS INTEGER   NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount
                                NO-LOCK
             ,FIRST TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStreamFrom
                                  AND TT_pdf_page.page_nbr   = TT-Merge-Pages.PageFrom
                                NO-LOCK :

                CREATE B_TT_pdf_page.
                BUFFER-COPY TT_pdf_page   EXCEPT TT_pdf_page.obj_stream
                                                 TT_pdf_page.page_nbr
                         TO B_TT_pdf_page ASSIGN B_TT_pdf_page.page_nbr   = TT-Merge-Pages.PageTo
                                                 B_TT_pdf_page.obj_stream = pdfStreamTo.

                RELEASE B_TT_pdf_page.

        END.
    END.

END. /* pdf_merge_stream_page */


/* 15-DEC-2014 jcc */
PROCEDURE pdf_merge_stream_page_use: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfNbrCopies  AS INTEGER     NO-UNDO.

    DEFINE BUFFER B_tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.

    DEFINE VARIABLE vCount AS INTEGER   NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount
                                NO-LOCK
             ,FIRST tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStreamFrom
                                          AND tt_pdf_page_use_ext.page_nbr   = TT-Merge-Pages.PageFrom
                                        NO-LOCK :

                CREATE B_tt_pdf_page_use_ext.
                BUFFER-COPY tt_pdf_page_use_ext   EXCEPT tt_pdf_page_use_ext.obj_stream
                                                         tt_pdf_page_use_ext.page_nbr
                         TO B_tt_pdf_page_use_ext ASSIGN B_tt_pdf_page_use_ext.page_nbr   = TT-Merge-Pages.PageTo
                                                         B_tt_pdf_page_use_ext.obj_stream = pdfStreamTo.

                RELEASE B_tt_pdf_page_use_ext.
        END.
    END.
END. /* pdf_merge_stream_page_use */


PROCEDURE pdf_merge_stream_link : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfNbrCopies          AS INT          NO-UNDO.

    DEFINE BUFFER B_TT_pdf_annot FOR TT_pdf_annot.

    DEFINE VARIABLE vCount AS INTEGER   NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount
                                NO-LOCK
           ,EACH TT_pdf_annot    WHERE TT_pdf_annot.obj_stream = pdfStreamFrom
                                  AND TT_pdf_annot.annot_page  = TT-Merge-Pages.PageFrom
                                NO-LOCK:
    
                CREATE B_TT_pdf_annot.
                BUFFER-COPY TT_pdf_annot   EXCEPT TT_pdf_annot.obj_stream
                                                 TT_pdf_annot.annot_page
                         TO B_TT_pdf_annot.

                ASSIGN B_TT_pdf_annot.obj_stream = pdfStreamTo.
                ASSIGN B_TT_pdf_annot.annot_page  = TT-Merge-Pages.PageTo.

                RELEASE B_TT_pdf_annot.
        END.
    END.

END PROCEDURE. /* pdf_merge_stream_link */


PROCEDURE pdf_merge_stream_book : /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfNbrCopies  AS INTEGER     NO-UNDO.

    DEFINE BUFFER B_TT_pdf_bookmark FOR TT_pdf_bookmark.

    DEFINE VARIABLE vBookNr AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vCount  AS INTEGER   NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount NO-LOCK :

            /* Set the Qty to add to TT_pdf_bookmark.book_nbr */
            FIND LAST TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream  = pdfStreamTo NO-LOCK NO-ERROR.
            IF AVAILABLE TT_pdf_bookmark
               THEN ASSIGN vBookNr = TT_pdf_bookmark.book_nbr.

            FOR EACH TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStreamFrom
                                       AND TT_pdf_bookmark.book_page  = TT-Merge-Pages.PageFrom
                                     NO-LOCK
                                     BREAK BY TT_pdf_bookmark.book_nbr:

                CREATE B_TT_pdf_bookmark.
                BUFFER-COPY TT_pdf_bookmark EXCEPT TT_pdf_bookmark.obj_stream
                                                   TT_pdf_bookmark.book_nbr
                                                   TT_pdf_bookmark.book_obj
                                                   TT_pdf_bookmark.book_first
                                                   TT_pdf_bookmark.book_last
                         TO B_TT_pdf_bookmark.

                ASSIGN B_TT_pdf_bookmark.book_nbr    = TT_pdf_bookmark.book_nbr + vBookNr.
                ASSIGN B_TT_pdf_bookmark.obj_stream  = pdfStreamTo.
                ASSIGN B_TT_pdf_bookmark.book_page   = TT-Merge-Pages.PageTo.

                IF TT_pdf_bookmark.book_parent <> 0
                   THEN ASSIGN B_TT_pdf_bookmark.book_parent = TT_pdf_bookmark.book_parent + vBookNr.

                RELEASE B_TT_pdf_bookmark.
           END.
        END.
    END.

END PROCEDURE. /* pdf_merge_stream_book */

PROCEDURE pdf_merge_stream_Fonts : /* PRIVATE */
  DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Font FOR TT_pdf_Font.

  FOR EACH TT_pdf_Font 
     WHERE TT_pdf_Font.obj_stream = pdfStreamFrom
     NO-LOCK TRANSACTION:

    IF NOT CAN-FIND(FIRST B_TT_pdf_Font WHERE B_TT_pdf_Font.obj_stream = pdfStreamTo
                                              AND B_TT_pdf_Font.font_name  = TT_pdf_Font.font_name)
    THEN DO:
      CREATE B_TT_pdf_Font.
      BUFFER-COPY TT_pdf_Font EXCEPT   TT_pdf_Font.obj_stream
               TO B_TT_pdf_Font ASSIGN B_TT_pdf_Font.obj_Stream = pdfStreamTo.
    END.
    /* 04-OCT-2013 jcc: set the used flag so that the font will go in the resources */
    ELSE IF TT_pdf_Font.used_flag THEN DO:
        FIND FIRST B_TT_pdf_Font WHERE B_TT_pdf_Font.obj_stream = pdfStreamTo
                                   AND B_TT_pdf_Font.font_name  = TT_pdf_Font.font_name.
        B_TT_pdf_Font.used_flag = TT_pdf_Font.used_flag.
    END.
    
    RELEASE B_TT_pdf_Font NO-ERROR.

  END.

END PROCEDURE.  /* pdf_merge_stream_fonts */

PROCEDURE pdf_merge_stream_param : /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER B_TT_pdf_param FOR TT_pdf_param.

    FOR EACH TT_pdf_param WHERE TT_pdf_param.obj_stream     = pdfStreamFrom
                            AND TT_pdf_param.obj_parameter <> "Page"
                          NO-LOCK TRANSACTION:
    
        FIND FIRST B_TT_pdf_param WHERE B_TT_pdf_param.obj_stream    = pdfStreamTo
                                    AND B_TT_pdf_param.obj_parameter = TT_pdf_param.obj_parameter
                                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE B_TT_pdf_param
        THEN DO:
             CREATE B_TT_pdf_param.
             ASSIGN B_TT_pdf_param.obj_stream    = pdfStreamTo
                    B_TT_pdf_param.obj_parameter = TT_pdf_param.obj_parameter.
        END.

        ASSIGN B_TT_pdf_param.obj_valid = TT_pdf_param.obj_valid
               B_TT_pdf_param.obj_value = TT_pdf_param.obj_value.

        RELEASE B_TT_pdf_param.

    END.

END PROCEDURE.  /* pdf_merge_stream_param */

PROCEDURE pdf_merge_stream_image : /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER B_TT_pdf_image FOR TT_pdf_image.

    FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStreamFrom
                          NO-LOCK TRANSACTION:
    
        IF CAN-FIND(FIRST B_TT_pdf_image WHERE B_TT_pdf_image.obj_stream = pdfStreamTo
                                           AND B_TT_pdf_image.image_name = TT_pdf_image.image_name
                                         NO-LOCK)
        THEN NEXT.

        BUFFER-COPY TT_pdf_image  EXCEPT TT_pdf_image.obj_stream
                 TO B_TT_pdf_image.

        ASSIGN B_TT_pdf_image.obj_stream = pdfStreamTo.

        RELEASE B_TT_pdf_image.

    END.

END PROCEDURE. /* pdf_merge_stream_image */

PROCEDURE pdf_merge_stream_diff : /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStreamFrom AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfStreamTo   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER B_TT_pdf_diff FOR TT_pdf_diff.

    FOR EACH TT_pdf_diff WHERE TT_pdf_diff.obj_stream = pdfStreamFrom
                         NO-LOCK TRANSACTION:
    
        IF CAN-FIND(FIRST B_TT_pdf_diff WHERE B_TT_pdf_diff.obj_stream = pdfStreamTo
                                          AND B_TT_pdf_diff.font_name  = TT_pdf_diff.font_name
                                        NO-LOCK)
        THEN NEXT.

        BUFFER-COPY TT_pdf_diff  EXCEPT TT_pdf_diff.obj_stream
                 TO B_TT_pdf_diff.

        ASSIGN B_TT_pdf_diff.obj_stream = pdfStreamTo.

        RELEASE B_TT_pdf_diff.

    END.

END PROCEDURE.  /* pdf_merge_stream_diff */

PROCEDURE pdf_ReplaceText:
   DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pdfMergeNbr   AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER pdfTextFrom   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pdfTextTo     AS CHARACTER NO-UNDO.

   CREATE TT_pdf_ReplaceTxt.
   ASSIGN TT_pdf_ReplaceTxt.obj_stream  = pdfStream
          TT_pdf_ReplaceTxt.mergenr     = pdfMergeNbr
          TT_pdf_ReplaceTxt.txt_from    = pdfTextFrom
          TT_pdf_ReplaceTxt.txt_to      = pdfTextTo.

END PROCEDURE. /* pdf_ReplaceText */

/* END PEKI's procedures ******************************* */

PROCEDURE pdf_EncryptDict: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfObject AS INTEGER   NO-UNDO.

  FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.

  PUT STREAM S_pdf_inc UNFORMATTED
      pdfObject " 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Filter /Standard" {&pdfSKIP}.

  /* 13-OCT-2013 jcc: AES-128 encryption */
  IF pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "RC4" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/V " (IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN "1" ELSE "2") {&pdfSKIP}
          "/R " (IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN "2" ELSE "3") {&pdfSKIP}.
  ELSE /* AES 128 */
      PUT STREAM S_pdf_inc UNFORMATTED
          "/V 4" {&pdfSKIP}
          "/R 4" {&pdfSKIP}
          "/CF<</StdCF<</Length 16/CFM/AESV2/AuthEvent/DocOpen>>>>/StmF/StdCF/StrF/StdCF" {&pdfSKIP}.

  IF  (pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "RC4" AND pdf_get_parameter(pdfStream, "EncryptKey") = "128")
    OR pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "AES" THEN /* 22-SEP-2014 jcc: add /Length for AES else Ghostscript cannot render the pdf file */
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Length " pdf_get_parameter(pdfStream, "EncryptKey") {&pdfSKIP}.

  PUT STREAM S_pdf_inc UNFORMATTED
      "/O<" TT_pdf_stream.obj_master ">" {&pdfSKIP}
      "/U<" TT_pdf_stream.obj_user  ">" {&pdfSKIP}
      "/P " TT_pdf_stream.obj_P {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_EncryptDict */

/* 14-OCT-2013 jcc: encrypt a memptr, get the encrypt key if necessary */
PROCEDURE EncryptMemPtr:
    DEFINE INPUT  PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pUniqueID    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pObjSeq      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pmMemPtr     AS MEMPTR      NO-UNDO.
    DEFINE INPUT  PARAMETER pmEncryptKey AS MEMPTR      NO-UNDO.
    DEFINE OUTPUT PARAMETER pmEncrypted  AS MEMPTR      NO-UNDO.

    /* 08-OCT-2013 jcc: get the encryption key if not provided */
    IF pmEncryptKey = ? OR GET-SIZE(pmEncryptKey) = 0 THEN
        RUN GetEncryptKey IN h_PDF-Encrypt
            (INPUT  pdfStream,
             INPUT  pUniqueID,
             INPUT  pObjSeq,
             INPUT  0,
             /* INPUT  pdf-EncryptKeyMemPtr, */
             OUTPUT pmEncryptKey).

    RUN EncryptContent IN h_PDF-Encrypt
                       (INPUT  pdfStream,
                        INPUT  pmEncryptKey,
                        INPUT  pmMemPtr,
                        OUTPUT pmEncrypted).
END PROCEDURE.

PROCEDURE OutputMemPtr: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pHasStream   AS LOGICAL     NO-UNDO.
  DEFINE INPUT PARAMETER pUniqueID    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pObjSeq      AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pMemPtr      AS MEMPTR      NO-UNDO.
  DEFINE INPUT PARAMETER pmEncryptKey AS MEMPTR      NO-UNDO.

  DEFINE VARIABLE L_EncryptMem AS MEMPTR      NO-UNDO.

  /* 14-OCT-2015 jcc:  not used anymore, for now I just comment it
  /* DEFINE VARIABLE l_Bytes      AS INTEGER     NO-UNDO. */
  /* DEFINE VARIABLE l_Header     AS CHARACTER   NO-UNDO. */
  /* DEFINE VARIABLE l_Loop       AS INTEGER     NO-UNDO. */
  /* DEFINE VARIABLE L_TempMem    AS MEMPTR      NO-UNDO. */

  /* If the Word Stream will appear in the MemPtr then extract the text
     previous to and including string 'stream' from the MemPtr as we don't
     want that encrypted -- only the stream contents get encryted */
  IF pHasStream THEN DO:
    /* message "HasStream=" get-string(pMemPtr,1,100) view-as alert-box. */

    l_Bytes = GET-SIZE(pMemPtr).
    DO l_Loop = 1 TO l_Bytes:
      IF GET-BYTES(pMemPtr,l_Loop,6) = "stream" THEN DO:
        IF GET-BYTE(pMemPtr,l_Loop + 6) = 10 THEN DO:
          l_Header = GET-STRING(pMemPtr,1,l_Loop + 6) NO-ERROR.
          SET-SIZE(L_TempMem) = GET-SIZE(pMemPtr) - (L_Loop + 6).
          L_TempMem = GET-BYTES(pMemPtr,L_Loop + 6, GET-SIZE(l_TempMem)).
        END.

        SET-SIZE(pMemPtr) = 0.
        SET-SIZE(pMemPtr) = GET-SIZE(L_TempMem).
        pMemPtr = l_TempMem.

        SET-SIZE(l_TempMem) = 0.
        LEAVE.
      END.

    END. /* iLoop */

    IF l_Header <> "" THEN
      PUT STREAM S_PDF_inc UNFORMATTED l_Header.

  END. /* HasStream */
  */

  IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN DO:
      RUN EncryptMemPtr (pdfStream,
                         pUniqueID,
                         pObjSeq,
                         pMemPtr,
                         pmEncryptKey,
                         OUTPUT L_EncryptMem).
      EXPORT STREAM S_PDF_inc L_EncryptMem.
      SET-SIZE(L_EncryptMem) = 0.
  END. /* Encrypted */
  ELSE
      EXPORT STREAM S_PDF_inc pMemPtr.

END. /* OutputMemPtr */

PROCEDURE pdf_tool_add : 
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolType    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolData    AS HANDLE    NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfToolName) + "," + QUOTER(pdfToolType) + "," + QUOTER(pdfToolData)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF INDEX(pdfToolName," ") > 0 THEN {pdferror.i &msg="'Tool Name cannot contain spaces!'" &return=YES}.

  CREATE TT_pdf_tool.
  ASSIGN TT_pdf_tool.obj_stream   = pdfStream
         TT_pdf_tool.tool_name    = pdfToolName
         TT_pdf_tool.tool_Type    = pdfToolType
         TT_pdf_tool.tool_handle  = pdfToolData.

  /* Now configure a bunch of defaults for the Tool */
  CASE TT_pdf_tool.tool_type:
    WHEN "TABLE" THEN DO:
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"Outline",0,"0").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderFont",0,"Courier-Bold").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderFontSize",0,"10").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderBGColor",0,"255,255,255").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderTextColor",0,"0,0,0").

      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailFont",0,"Courier").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailFontSize",0,"10").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailBGColor",0,"255,255,255").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailTextColor",0,"0,0,0").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"ColumnPadding",0,"5").

      /*
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"StartY",0,"0").
      */
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"CellUnderline",0,"0.5").

    END.
  END CASE.

  IF NOT VALID-HANDLE(h_PDF-Tool) THEN
    RUN {&PDFDIR}lib/pdftool.p PERSISTENT SET h_PDF-Tool
        (INPUT THIS-PROCEDURE:HANDLE).

END. /* pdf_tool_add */

PROCEDURE pdf_tool_create : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
       AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN {pdferror.i &msg="'Cannot find Tool ' + QUOTER(pdfToolName) + ' for Stream ' + pdfStream + '!'" &return=YES}.

  IF NOT VALID-HANDLE(h_PDF-Tool) THEN
    RUN {&PDFDIR}lib/pdftool.p PERSISTENT SET h_PDF-Tool
        (INPUT THIS-PROCEDURE:HANDLE).

  /* If a call the pdf_new_page hasn't been done then ensure that we do it */
  IF pdf_Page(pdfStream) = 0 THEN
    RUN pdf_new_page2(pdfStream, pdf_Orientation(pdfStream)).

  CASE TT_pdf_tool.tool_type:
    WHEN "TABLE" THEN 
      RUN BuildTable IN h_PDF-Tool (INPUT pdfStream,
                                    INPUT pdfToolName,
                                    INPUT TT_pdf_tool.Tool_handle).
    WHEN "CALENDAR" THEN
      RUN BuildCalendar IN h_PDF-Tool (INPUT pdfStream,
                                       INPUT pdfToolName).
    WHEN "MATRIX" THEN
      RUN BuildMatrix IN h_PDF-Tool (INPUT pdfStream,
                                     INPUT pdfToolName).
  END CASE.

END. /* pdf_tool_create */

PROCEDURE pdf_set_tool_parameter :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolParam   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolCol     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolValue   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Integer      AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Decimal      AS DECIMAL NO-UNDO.

  DEFINE VARIABLE L_TableParams  AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfToolName) + "," + QUOTER(pdfToolParam) + "," + STRING(pdfToolCol) + "," + QUOTER(pdfToolValue)). &ENDIF

  L_TableParams = "Outline,CellUnderline,"
                + "ColumnHeader,ColumnWidth,ColumnX,ColumnPadding,MaxX,MaxY,"
                + "HeaderFont,HeaderFontSize,HeaderBGColor,HeaderTextColor,"
                + "DetailFont,DetailFontSize,DetailBGColor,DetailTextColor,"
                + "UseFields,StartY,Pages,ColumnVerticalPadding,WrapText,LastY". /* 14-JUN-2012 jcc: add WrapText ; 7-AUG-2012 jcc: add LastY */

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
         AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN {pdferror.i &msg="'Cannot find Tool ' + QUOTER(pdfToolName) + ' for Stream ' + pdfStream + '!'" &return=YES}.

  /* 01-MAR-2010 jcc: manage properly EUROPEAN sessions */
  IF SESSION:NUMERIC-FORMAT = "EUROPEAN" 
     AND LOOKUP(pdfToolParam,  "ColumnWidth,ColumnX,MaxX,MaxY,Outline,CellUnderline,Pages,StartY,"
                             + "Width,Height,X,Y,"
                             + "HeaderFontSize,DetailFontSize,HeaderHeight,"
                             + "ColumnPadding,ColumnVerticalPadding,"
                             + "DayLabelFontSize,DayFontSize,DayLabelHeight,DayLabelY,"
                             + "FontSize,GridWeight,Columns,Rows" ) > 0 THEN
    pdfToolValue = REPLACE(pdfToolValue, ".", ",").

  CASE TT_pdf_tool.tool_type:
    WHEN "TABLE" THEN DO:
      IF LOOKUP(pdfToolParam,L_TableParams) = 0 THEN
          {pdferror.i &msg="'Invalid Table Parameter ' + pdfToolParam + ' entered (' + pdfToolName + ')!'" &return=YES}. /* 14-JUN-2012 jcc: add pdfToolParam in the error message */

      /* Verify for Integer Content */
      IF LOOKUP(pdfToolParam,"ColumnWidth,ColumnX,MaxX,MaxY,Outline,CellUnderline,Pages,StartY") > 0 
      THEN DO:
        L_Integer = INT(pdfToolValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            {pdferror.i &msg="'Parameter (' + pdfToolParam + ') requires an Integer value!'" &return=YES}.
        IF LOOKUP(pdfToolParam,"Outline") = 0 AND L_Integer <= 0 THEN
            {pdferror.i &msg="'Parameter (' + pdfToolParam + ') requires an a positive (non-zero) value!'" &return=YES}.
      END. /* Integer Verification */

      ELSE IF LOOKUP(pdfToolParam,"HeaderFontSize,DetailFontSize") > 0 THEN DO:
        L_Decimal = DEC(pdfToolValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            {pdferror.i &msg="'Parameter (' + pdfToolParam + ') requires a Decimal value!'" &return=YES}.
      END. /* Decimal Verification */

      ELSE IF LOOKUP(pdfToolParam,"HeaderBGColor,DetailBGColor,HeaderTextColor,DetailTextColor") > 0 THEN DO:
        IF NUM-ENTRIES(pdfToolValue) <> 3 THEN
            {pdferror.i &msg="'Parameter (' + pdfToolParam + ') requires 3 Entries in RGB sequence comma-delimited!'" &return=YES}.
      END. /* Color Verification */

      ELSE IF LOOKUP(pdfToolParam,"HeaderFont,DetailFont") > 0 THEN DO:
        IF NOT CAN-FIND(FIRST TT_pdf_font
                        WHERE TT_pdf_font.obj_stream = pdfStream
                          AND TT_pdf_font.font_name  = pdfToolValue) THEN
            {pdferror.i &msg="'Parameter (' + pdfToolParam + ') requires a Valid Font Name!'" &return=YES}.
      END. /* Font Name Verification */

    END. /* Table Parameter Check */
  END CASE.

  FIND TT_pdf_tool_param
       WHERE TT_pdf_tool_param.obj_stream = pdfStream
         AND TT_pdf_tool_param.tool_name  = pdfToolName
         AND TT_pdf_tool_param.tool_param = pdfToolParam
         AND TT_pdf_tool_param.tool_col   = pdfToolCol
         NO-ERROR.
  IF NOT AVAILABLE TT_pdf_tool_param THEN DO:
    CREATE TT_pdf_tool_param.
    ASSIGN TT_pdf_tool_param.obj_stream   = pdfStream
           TT_pdf_tool_param.tool_name    = pdfToolName
           TT_pdf_tool_param.tool_param   = pdfToolParam
           TT_pdf_tool_param.tool_col     = pdfToolCol.
  END.

  TT_pdf_tool_param.tool_value = pdfToolValue.

END. /* pdf_set_tool_parameter */

FUNCTION pdf_get_tool_parameter RETURNS CHARACTER
        (INPUT  pdfStream      AS CHARACTER,
         INPUT  pdfToolName    AS CHARACTER,
         INPUT  pdfToolParam   AS CHARACTER,
         INPUT  pdfToolCol     AS INTEGER):

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
         AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN {pdferror.i &msg="'Cannot find Tool ' + QUOTER(pdfToolName) + ' for Stream ' + pdfStream + '!'" &return=YES &return-value="''"}.

  FIND TT_pdf_tool_param
       WHERE TT_pdf_tool_param.obj_stream = pdfStream
         AND TT_pdf_tool_param.tool_name  = pdfToolName
         AND TT_pdf_tool_param.tool_param = pdfToolParam
         AND TT_pdf_tool_param.tool_col   = pdfToolCol NO-LOCK NO-ERROR.

  RETURN IF AVAIL TT_pdf_tool_param THEN TT_pdf_tool_param.tool_value
         ELSE "".

END FUNCTION. /* pdf_get_tool_parameter */

/* 5-AUG-2012 jcc: copy from above for default value */
FUNCTION pdf_get_tool_parameter2 RETURNS CHARACTER
        (INPUT  pdfStream      AS CHARACTER,
         INPUT  pdfToolName    AS CHARACTER,
         INPUT  pdfToolParam   AS CHARACTER,
         INPUT  pdfToolCol     AS INTEGER,
         INPUT  pdfDefault     AS CHARACTER):

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
         AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN {pdferror.i &msg="'Cannot find Tool ' + QUOTER(pdfToolName) + ' for Stream ' + pdfStream + '!'" &return=YES &return-value="''"}.

  FIND TT_pdf_tool_param
       WHERE TT_pdf_tool_param.obj_stream = pdfStream
         AND TT_pdf_tool_param.tool_name  = pdfToolName
         AND TT_pdf_tool_param.tool_param = pdfToolParam
         AND TT_pdf_tool_param.tool_col   = pdfToolCol NO-LOCK NO-ERROR.

  RETURN IF AVAIL TT_pdf_tool_param THEN TT_pdf_tool_param.tool_value
         ELSE pdfDefault.

END FUNCTION. /* pdf_get_tool_parameter */
/* jcc end */

PROCEDURE pdf_tool_destroy : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.

  FIND TT_pdf_tool WHERE TT_pdf_tool.obj_stream = pdfStream
                     AND TT_pdf_tool.tool_name  = pdfToolName
                     EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL TT_pdf_Tool THEN {pdferror.i &msg="'Cannot find Tool ' + QUOTER(pdfToolName) + ' for Stream ' + pdfStream + '!'" &return=YES}.

  /* Firstly, remove all of the tool parameters */
  FOR EACH TT_pdf_tool_param WHERE TT_pdf_tool_param.obj_stream = pdfStream
                               AND TT_pdf_tool_param.tool_name  = pdfToolName
                               EXCLUSIVE-LOCK:
     DELETE TT_pdf_tool_param.
  END.

  /* The delete the tool */
  DELETE TT_pdf_tool.
END. /* pdf_tool_destroy */

PROCEDURE pdf_note :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfNoteText    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfNoteTitle   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfIcon        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue        AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE VARIABLE iIconPos AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfNoteText) + "," + QUOTER(pdfNoteTitle) + "," + QUOTER(pdfIcon) + "," + dec2string(pdfLLX) + "," + dec2string(pdfLLY) + "," + dec2string(pdfURX) + "," + dec2string(pdfURY) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue)). &ENDIF

  iIconPos = LOOKUP(pdfIcon, "Note,Comment,Insert,Key,Help,NewParagraph,Paragraph").
  IF iIconPos > 0 THEN
      pdfIcon = ENTRY(iIconPos, "Note,Comment,Insert,Key,Help,NewParagraph,Paragraph"). /* 25-MAR-2015 jcc: ensure pdfIcon has the right character case */
  ELSE IF pdfIcon = "" THEN
      pdfIcon = "Note".
  /* 25-MAR-2015 jcc: PDF Spec.: "Additional names may be supported as well. Default value: Note." */

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = "Text"
         TT_pdf_annot.annot_content = pdfNoteText.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_icon    = pdfIcon
         TT_pdf_annot.annot_rect    = dec2string(pdfLLX) + " "
                                    + dec2string(pdfLLY) + " "
                                    + dec2string(pdfURX) + " " 
                                    + dec2string(pdfURY) 
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                    + dec2string(pdfGreen) + " "
                                    + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = 0
         TT_pdf_annot.annot_style   = pdfNoteTitle
         TT_pdf_annot.in_transaction = pdf_in_transaction(pdfStream).

END. /* pdf_note */

PROCEDURE pdf_stamp :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfStampText   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTitle       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfStamp       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue        AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE VARIABLE iStampPos     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE l_ValidStamps AS CHARACTER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfStampText) + "," + QUOTER(pdfTitle) + "," + QUOTER(pdfStamp) + "," + dec2string(pdfLLX) + "," + dec2string(pdfLLY) + "," + dec2string(pdfURX) + "," + dec2string(pdfURY) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue)). &ENDIF

  L_ValidStamps = "Approved,Experimental,NotApproved,"
                + "AsIs,Expired,NotForPublicRelease,"
                + "Confidential,Final,Sold,"
                + "Departmental,ForComment,TopSecret,"
                + "Draft,ForPublicRelease".

  iStampPos = LOOKUP(pdfStamp,L_ValidStamps).
  IF iStampPos > 0 THEN
      pdfStamp = ENTRY(iStampPos, l_ValidStamps). /* 25-MAR-2015 jcc: ensure pdfStamp has the right character case */
  ELSE IF pdfStamp = "" THEN
      pdfStamp = "Draft".
  /* 25-MAR-2015 jcc: PDF Spec.: "Additional names may be supported as well. Default value: Draft." */

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = "Stamp"
         TT_pdf_annot.annot_content = pdfStampText.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_icon    = pdfStamp
         TT_pdf_annot.annot_rect    = dec2string(pdfLLX) + " "
                                    + dec2string(pdfLLY) + " "
                                    + dec2string(pdfURX) + " "
                                    + dec2string(pdfURY)
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                    + dec2string(pdfGreen) + " "
                                    + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = 0
         TT_pdf_annot.annot_style   = pdfTitle
         TT_pdf_annot.in_transaction = pdf_in_transaction(pdfStream).

END. /* pdf_stamp */

PROCEDURE pdf_Markup:
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfContent     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTitle       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfStyle       AS CHARACTER NO-UNDO CASE-SENSITIVE.

  DEFINE INPUT PARAMETER pdfX1          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY1          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfX2          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY2          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfX3          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY3          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfX4          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY4          AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE INPUT PARAMETER pdfRed         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue        AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE VARIABLE iMarkupPos AS INTEGER     NO-UNDO.
  
  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfContent) + "," + QUOTER(pdfTitle) + "," + QUOTER(pdfStyle) + "," + dec2string(pdfX1) + "," + dec2string(pdfY1) + "," + dec2string(pdfX2) + "," + dec2string(pdfY2) + "," + dec2string(pdfX3) + "," + dec2string(pdfY3) + "," + dec2string(pdfX4) + "," + dec2string(pdfY4) + "," + dec2string(pdfRed) + "," + dec2string(pdfGreen) + "," + dec2string(pdfBlue)). &ENDIF

  iMarkupPos = LOOKUP(pdfStyle,"Highlight,Underline,Squiggly,StrikeOut").
  IF iMarkupPos > 0 THEN 
      pdfStyle = ENTRY(iMarkupPos, "Highlight,Underline,Squiggly,StrikeOut"). /* 25-MAR-2015 jcc: Ensure proper character case */
  ELSE IF pdfStyle = "" THEN
      pdfStyle = "Highlight".

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = pdfStyle
         TT_pdf_annot.annot_content = pdfContent.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_icon    = ""
         TT_pdf_annot.annot_rect    = dec2string(pdfX1) + " "
                                    + dec2string(pdfY3) + " "
                                    + dec2string(pdfX2) + " "
                                    + dec2string(pdfY1)
         TT_pdf_annot.annot_add     = dec2string(pdfX1) + " "
                                    + dec2string(pdfY1) + " "
                                    + dec2string(pdfX2) + " "
                                    + dec2string(pdfY2) + " " 
                                    + dec2string(pdfX3) + " "
                                    + dec2string(pdfY3) + " " 
                                    + dec2string(pdfX4) + " "
                                    + dec2string(pdfY4)
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                    + dec2string(pdfGreen) + " "
                                    + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = 0
         TT_pdf_annot.annot_style   = pdfTitle
         TT_pdf_annot.in_transaction = pdf_in_transaction(pdfStream).

END. /* pdf_Markup */

PROCEDURE pdf_load_template:
  DEFINE INPUT PARAMETER pdfStream        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTemplateID    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTemplateFile  AS CHARACTER NO-UNDO.

  DEFINE BUFFER TT_pdf_font  FOR TT_pdf_font.
  DEFINE BUFFER TT_pdf_image FOR TT_pdf_image.

  DEFINE VARIABLE cFontList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cImgList  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER     NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfTemplateID) + "," + QUOTER(pdfTemplateFile)). &ENDIF

  IF NOT VALID-HANDLE(h_PDF-template) THEN
    RUN {&PDFDIR}lib/pdfTemplate.p PERSISTENT SET h_PDF-Template
        (INPUT THIS-PROCEDURE:HANDLE).

  RUN LoadTemplate IN h_PDF-template(pdfStream,
                                     pdfTemplateID,
                                     pdfTemplateFile,
                                     OUTPUT cFontList,
                                     OUTPUT cImgList).

  /* 18-NOV-2013 jcc: mark fonts as used */
  DO i = 1 TO NUM-ENTRIES(cFontList, CHR(1)):
      FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
          AND TT_pdf_font.font_tag = "~/" + ENTRY(i, cFontList, CHR(1)) NO-ERROR.
      IF AVAILABLE TT_pdf_font THEN
          TT_pdf_font.used_flag = YES.
  END.

  /* 07-MAY-2014 jcc: mark images as used */
  DO i = 1 TO NUM-ENTRIES(cImgList, CHR(1)):
      FIND FIRST TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream
        AND TT_pdf_image.image_name = ENTRY(i, cImgList, CHR(1)) NO-ERROR.
      IF AVAILABLE TT_pdf_image THEN
          TT_pdf_image.used_flag = YES.
  END.

  IF RETURN-VALUE <> "" THEN
      {pdferror.i &msg=RETURN-VALUE}.

END. /* pdf_load_template */

PROCEDURE pdf_use_template:
  DEFINE INPUT PARAMETER pdfStream        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTemplateID    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE c_Content AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfTemplateID)). &ENDIF

  RUN GetContent IN h_PDF-template(pdfStream,
                                   pdfTemplateID,
                                   OUTPUT c_Content).
  IF RETURN-VALUE <> "" THEN
      {pdferror.i &msg=RETURN-VALUE}.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        c_Content,
                        "",
                        "").

END. /* pdf_use_template */

PROCEDURE pdf_GetBestFont:
/* Calculate the best font size to use to insert text into a given range along 
  the X axis - tests in 0.5 point size increments */
  DEFINE INPUT        PARAMETER pdfStream     AS CHARACTER  NO-UNDO. /* Stream name */
  DEFINE INPUT        PARAMETER pdfFont       AS CHARACTER  NO-UNDO. /* Font to use */
  DEFINE INPUT-OUTPUT PARAMETER pdfText       AS CHARACTER  NO-UNDO. /* Text to measure */
  DEFINE INPUT-OUTPUT PARAMETER pdfFontSize   AS DECIMAL    NO-UNDO. /* Start font size */
  DEFINE INPUT        PARAMETER pdfSmallest   AS DECIMAL    NO-UNDO. /* Smallest font size to use */
  DEFINE INPUT        PARAMETER pdfChopText   AS LOGICAL    NO-UNDO. /* If the smallest font is too */
                                                                       /* big then cut text to fit? */
  DEFINE INPUT        PARAMETER pdfFromX      AS INTEGER    NO-UNDO. /* Start X co-ord */
  DEFINE INPUT        PARAMETER pdfToX        AS INTEGER    NO-UNDO. /* End X co-ord   */

  DEFINE VARIABLE w-loop      AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE w-chars2fit AS INTEGER    NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFont) + "," + QUOTER(pdfText) + "," + dec2string(pdfFontSize) + "," + dec2string(pdfSmallest) + "," + STRING(pdfChopText) + "," + STRING(pdfFromX) + "," + STRING(pdfToX)). &ENDIF

  BESTLOOP:
  DO w-loop = pdfFontSize TO pdfSmallest BY -0.5:
    RUN pdf_set_font (pdfStream, pdfFont, w-loop).
        
    ASSIGN w-chars2fit = pdf_GetNumFittingChars(pdfStream, pdfText, pdfFromX, pdfToX)
           pdfFontSize = w-loop.
    
    IF w-chars2fit >= LENGTH(pdfText) THEN
      LEAVE BESTLOOP.
    
    IF (w-loop = pdfSmallest) AND (w-chars2fit < length(pdfText)) AND (pdfChopText = TRUE) THEN
        ASSIGN pdfText = SUBSTR(pdfText, 1, w-chars2fit).
  END.
END PROCEDURE. /* pdf_GetBestFont */

/* 05-OCT-2015 jcc: new */
FUNCTION getPdfCacheDir RETURNS CHARACTER ( pdfStream AS CHARACTER,
                                            pdfName   AS CHARACTER ):  /* PRIVATE */
    RETURN SESSION:TEMP-DIR + "pdfcache/"
           + ENCODE(REPLACE(pdfName, "~\", "~/")
                    + pdf_get_parameter2(pdfStream, "formFlatten", "*") /* 01-OCT-2015 jcc: cannot share cache when not flattening: TT_Resource would miss the Acroform & Annots */
                    + pdf_get_parameter(pdfStream, "retainAnnots") /* 05-OCT-2015 jcc: idem for other annotations */
                    ). 
END FUNCTION.

PROCEDURE pdf_open_PDF:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfID      AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iArraySeq             AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iDictSeq              AS INTEGER     NO-UNDO.

  DEFINE BUFFER bTT_pdf_font FOR TT_pdf_font.
  DEFINE BUFFER TT_pdf_font  FOR TT_pdf_font.
  DEFINE BUFFER TT_pdf_ext   FOR TT_pdf_ext.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfName) + "," + QUOTER(pdfID)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  IF SEARCH(pdfName) = ? THEN {pdferror.i &msg="'Cannot find pdf file ' + QUOTER(pdfName)" &return=YES}.

  FIND FIRST TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream
                          AND TT_pdf_ext.pdf_id     = pdfID NO-ERROR.
  IF AVAILABLE TT_pdf_ext THEN DO:
      IF pdfName = TT_pdf_ext.pdf_name AND pdf_get_parameter(pdfStream, "reuseExternal") = "TRUE" THEN RETURN. /* 03-DEC-2014 jcc: when "reusing external", do not do an error if we are called one time too much, instead just return */
      ELSE {pdferror.i &msg="'PDF ID (' + pdfID + ') has been used already'" &return=YES}.
  END.

  /* 23-FEB-2010 jcc: Lonny L. Granstrom 21/12/2006 */
  /* Return without Error if PDF already loaded for use */
  IF CAN-FIND(FIRST TT_pdf_ext
              WHERE TT_pdf_ext.obj_stream = pdfStream
                AND TT_pdf_ext.pdf_id     = pdfID
                AND TT_pdf_ext.pdf_name   = pdfName
             ) THEN
    RETURN.
  /* 23-FEB-2010 jcc: end */

  /* FIND FIRST TT_pdf_ext WHERE TT_pdf_ext.obj_Stream = pdfStream */
                          /* AND TT_pdf_ext.pdf_name   = pdfName NO-ERROR. */
  /* IF AVAIL TT_pdf_ext THEN */
      /* {pdferror.i &msg="'PDF File Name (' + pdfName + ') has already been opened with PDF ID = ' + TT_pdf_ext.pdf_ID" &return=YES}. */

  /* 23-SEP-2015 jcc: if the same pdf has already been opened for another stream, then no need to parse it again! -> big optimization :) */
  /* Notes: 1. no need to copy tt_dict nor tt_array: by leaving their ids in the copied TT_Object, they will be reused. */
  /*        2. added pdf_id_orig field in both TT_Object and tt_pdf_ext in order to be able to find the files later, in order to export them. */
  FIND FIRST TT_pdf_ext WHERE TT_pdf_ext.obj_stream <> pdfStream AND TT_pdf_ext.pdf_name = pdfName NO-ERROR.
  IF AVAILABLE TT_pdf_ext THEN DO:
      /* we don't parse the pdf file, but we need to copy all the temp-tables */
      DEFINE VARIABLE cExtTable   AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE cExtTables  AS CHARACTER   INITIAL "tt_pdf_ext,tt_pdf_font,TT_Font,TT_Info,TT_Object,TT_Resource,TT_Widget" NO-UNDO.
      DEFINE VARIABLE hBufferFrom AS HANDLE      NO-UNDO.
      DEFINE VARIABLE hBufferTo   AS HANDLE      NO-UNDO.
      DEFINE VARIABLE hQuery      AS HANDLE      NO-UNDO.
      DEFINE VARIABLE i           AS INTEGER     NO-UNDO.

      DO i = 1 TO NUM-ENTRIES(cExtTables):
          cExtTable = ENTRY(i, cExtTables).
          CREATE BUFFER hBufferFrom FOR TABLE cExtTable.
          CREATE BUFFER hBufferTo   FOR TABLE cExtTable.
          CREATE QUERY hQuery.
          hQuery:SET-BUFFERS(hBufferFrom).
          hQuery:FORWARD-ONLY = YES.
          hQuery:QUERY-PREPARE("FOR EACH " + cExtTable + " WHERE " + cExtTable + ".obj_stream = '" + TT_pdf_ext.obj_stream + "' AND " + cExtTable + ".pdf_id = '" + TT_pdf_ext.pdf_id + "'").
          hQuery:QUERY-OPEN().
          REPEAT:
              hQuery:GET-NEXT().
              IF hQuery:QUERY-OFF-END THEN LEAVE.
              hBufferTo:BUFFER-CREATE().
              hBufferTo:BUFFER-COPY(hBufferFrom, "obj_stream,pdf_id").
              ASSIGN
               hBufferTo:BUFFER-FIELD("obj_stream"):BUFFER-VALUE  = pdfStream
               hBufferTo:BUFFER-FIELD("pdf_id"):BUFFER-VALUE      = pdfID
               hBufferTo:BUFFER-FIELD("pdf_id_orig"):BUFFER-VALUE = TT_pdf_ext.pdf_id_orig WHEN cExtTable = "TT_Object" OR cExtTable = "tt_pdf_ext".
          END.
          hQuery:QUERY-CLOSE().
          DELETE OBJECT hQuery.
          DELETE OBJECT hBufferFrom.
          DELETE OBJECT hBufferTo.
      END.
      RETURN.
  END. /* 23-SEP-2015 jcc: end */

  /* 19-MAY-2014 jcc: each template will store its temp files in its own directory, e.g. for cache */
  DEFINE VARIABLE cCacheDir    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTmpCacheDir AS CHARACTER   NO-UNDO. /* 14-MAR-2017 jcc: first create the cache in a unique name directory, then move it to the final cache dir */
  OS-CREATE-DIR VALUE(SESSION:TEMP-DIR + "pdfcache").
  IF OS-ERROR <> 0 THEN {pdferror.i &msg="'Cannot create cache directory' + QUOTER(SESSION:TEMP-DIR + 'pdfcache') + '. OS Error #' + STRING(OS-ERROR)" &return=YES}.
  /* unique name for the cache directory */
  cCacheDir = getPdfCacheDir(pdfStream, pdfName) + "/".

  /* 24-SEP-2015 jcc: if the cache dir already exists, then use it instead of parsing the pdf file */
  /* We have to update the dict & array ids because the cache has potentially been written in another context. */
  /* Note: did the cache loading and writing statically because IMPORT and EXPORT cannot work with a buffer handle. */
  IF LOGICAL(pdf_get_parameter2(pdfStream, "usePdfCache", "Yes")) AND SEARCH(cCacheDir + "tt_pdf_ext.cache") <> ? THEN DO:
      /* TODO: check cache integrity? check that the pdf has changed? */
      DEFINE VARIABLE iArrayIdDelta AS INTEGER   NO-UNDO.
      DEFINE VARIABLE iDictIdDelta  AS INTEGER   NO-UNDO.
      INPUT FROM VALUE(cCacheDir + "tt_pdf_ext.cache").
      REPEAT:
          CREATE TT_pdf_ext.
          ASSIGN
           TT_pdf_ext.obj_stream = pdfStream
           TT_pdf_ext.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" TT_pdf_ext EXCEPT obj_stream pdf_id.
      END.
      INPUT CLOSE.
      DELETE TT_pdf_ext.
      /* Up to now, the next dict/array_id will be resp. array_seq + 1 and dict_seq + 1 */
      /* When the cache was written, the values were potentially different. So compute then apply the delta to the ids. */
      FIND TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream AND TT_pdf_ext.pdf_id = pdfID.
      ASSIGN
       iDictIdDelta  = dict_seq  - TT_pdf_ext.dict_seq_start  + 1
       iArrayIdDelta = array_seq - TT_pdf_ext.array_seq_start + 1
       dict_seq      = TT_pdf_ext.dict_seq_end  + iDictIdDelta /* update global sequence variables */
       array_seq     = TT_pdf_ext.array_seq_end + iArrayIdDelta.
      INPUT FROM VALUE(cCacheDir + "tt_dict.cache").
      REPEAT:
          CREATE tt_dict.
          IMPORT DELIMITER "~t" tt_dict.
          ASSIGN
           tt_dict.dict_id        = tt_dict.dict_id + iDictIdDelta
           tt_dict.value_dict_id  = tt_dict.value_dict_id + iDictIdDelta   WHEN tt_dict.value_dict_id > 0
           tt_dict.value_array_id = tt_dict.value_array_id + iArrayIdDelta WHEN tt_dict.value_array_id > 0.
      END.
      INPUT CLOSE.
      DELETE tt_dict.
      INPUT FROM VALUE(cCacheDir + "tt_array.cache").
      REPEAT:
          CREATE tt_array.
          IMPORT DELIMITER "~t" tt_array.
          ASSIGN
           tt_array.array_id       = tt_array.array_id + iArrayIdDelta
           tt_array.value_dict_id  = tt_array.value_dict_id + iDictIdDelta   WHEN tt_array.value_dict_id > 0
           tt_array.value_array_id = tt_array.value_array_id + iArrayIdDelta WHEN tt_array.value_array_id > 0.
      END.
      INPUT CLOSE.
      DELETE tt_array.
      INPUT FROM VALUE(cCacheDir + "tt_pdf_font.cache").
      REPEAT:
          CREATE tt_pdf_font.
          ASSIGN
           tt_pdf_font.obj_stream = pdfStream
           tt_pdf_font.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" tt_pdf_font EXCEPT obj_stream pdf_id.
      END.
      INPUT CLOSE.
      DELETE tt_pdf_font.
      INPUT FROM VALUE(cCacheDir + "TT_Font.cache").
      REPEAT:
          CREATE TT_Font.
          ASSIGN
           TT_Font.obj_stream = pdfStream
           TT_Font.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" TT_Font EXCEPT obj_stream pdf_id.
      END.
      INPUT CLOSE.
      DELETE TT_Font.
      INPUT FROM VALUE(cCacheDir + "TT_Info.cache").
      REPEAT:
          CREATE TT_Info.
          ASSIGN
           TT_Info.obj_stream = pdfStream
           TT_Info.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" TT_Info EXCEPT obj_stream pdf_id.
      END.
      INPUT CLOSE.
      DELETE TT_Info.
      INPUT FROM VALUE(cCacheDir + "TT_Object.cache").
      REPEAT:
          CREATE TT_Object.
          ASSIGN
           TT_Object.obj_stream = pdfStream
           TT_Object.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" TT_Object EXCEPT obj_stream pdf_id.
          ASSIGN
           TT_Object.pdf_id_orig  = TT_pdf_ext.pdf_id_orig /* pdfid used when the cache files were created */
           TT_Object.obj_dict_id  = TT_Object.obj_dict_id + iDictIdDelta   WHEN TT_Object.obj_dict_id > 0
           TT_Object.obj_array_id = TT_Object.obj_array_id + iArrayIdDelta WHEN TT_Object.obj_array_id > 0.
      END.
      INPUT CLOSE.
      DELETE TT_Object.
      INPUT FROM VALUE(cCacheDir + "TT_Resource.cache").
      REPEAT:
          CREATE TT_Resource.
          ASSIGN
           TT_Resource.obj_stream = pdfStream
           TT_Resource.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" TT_Resource EXCEPT obj_stream pdf_id.
      END.
      INPUT CLOSE.
      DELETE TT_Resource.
      INPUT FROM VALUE(cCacheDir + "TT_Widget.cache").
      REPEAT:
          CREATE TT_Widget.
          ASSIGN
           TT_Widget.obj_stream = pdfStream
           TT_Widget.pdf_id     = pdfID.
          IMPORT DELIMITER "~t" TT_Widget EXCEPT obj_stream pdf_id.
      END.
      INPUT CLOSE.
      DELETE TT_Widget.
      RETURN.
  END. /* 24-SEP-2015 jcc: end load pdf cache */

  /* if we arrive there, then we will really parse the pdf file given as input */

  /* 14-MAR-2017 jcc: create a truly unique name for the cache generation; we will rename it to cCacheDir at the end, if no other session generated the same cache at the same time */
  cTmpCacheDir = RIGHT-TRIM(cCacheDir, "~/") + STRING(ETIME) + STRING(RANDOM(1,1000),"9999") + "/".
  OS-CREATE-DIR VALUE(cTmpCacheDir).
  IF OS-ERROR <> 0 THEN {pdferror.i &msg="'Cannot create cache directory' + QUOTER(cTmpCacheDir) + '. OS Error #' + STRING(OS-ERROR)" &return=YES}.

  ASSIGN /* 04-OCT-2013 jcc: remember first id for array & dict */
      iArraySeq = array_seq + 1
      iDictSeq  = dict_seq  + 1.

  RUN {&PDFDIR}lib/pdfextract.p (pdfStream, SEARCH(pdfName), pdfID, cTmpCacheDir, cCacheDir) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
      /* MESSAGE RETURN-VALUE */
          /* VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      {pdferror.i &msg="REPLACE(RETURN-VALUE,CHR(3),'~n')" &return=YES}.
  END.

  /* Create an entry for each External font so that they can be used by 
     PDFinclude */
  FOR EACH TT_Font WHERE TT_Font.obj_stream = pdfStream
      AND TT_Font.pdf_id = pdfID:
    CREATE TT_pdf_font.
    ASSIGN TT_pdf_font.font_file  = "EXTERNAL"
           TT_pdf_font.font_tag   = tt_font.font_tag
           TT_pdf_font.font_name  = TT_Font.font_name
           TT_pdf_font.font_width = TT_Font.font_width
           TT_pdf_font.ext_page   = TT_Font.page_id
           TT_pdf_font.used_flag  = TT_Font.used_flag /* 16-JAN-2011 jcc: so that the font used in the fill-ins of the forms will go in the resources */
           TT_pdf_font.obj_stream = pdfStream
           TT_pdf_font.pdf_id     = pdfID.

    /* 14-JAN-2011 jcc: in case the font has no width, copy the base font width */
    IF TT_pdf_font.font_width = "" THEN DO:
        FIND bTT_pdf_font WHERE bTT_pdf_font.font_name = LEFT-TRIM(TT_Font.font_base, "~/") NO-ERROR.
        IF AVAILABLE bTT_pdf_font THEN
            TT_pdf_font.font_width = bTT_pdf_font.font_width.
    END.
  END.

  CREATE TT_pdf_ext.
  ASSIGN TT_pdf_ext.obj_Stream      = pdfStream
         TT_Pdf_ext.pdf_ID          = pdfID
         TT_Pdf_ext.pdf_ID_orig     = pdfID /* 23-SEP-2015 jcc: added */
         TT_pdf_ext.pdf_name        = pdfName
         /* 04-OCT-2013 jcc: store the array & dict sequences used for future cleanup */
         TT_pdf_ext.array_seq_start = iArraySeq
         TT_pdf_ext.array_seq_end   = array_seq
         TT_pdf_ext.dict_seq_start  = iDictSeq
         TT_pdf_ext.dict_seq_end    = dict_seq
         TT_pdf_ext.cache_dir       = cCacheDir.

  /* 24-SEP-2015 jcc: now save the temp-tables for the pdf cache */
  OUTPUT TO VALUE(cTmpCacheDir + "tt_dict.cache").
  FOR EACH tt_dict WHERE tt_dict.dict_id >= TT_pdf_ext.dict_seq_start AND TT_dict.dict_id <= TT_pdf_ext.dict_seq_end:
      EXPORT DELIMITER "~t" tt_dict.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "tt_array.cache").
  FOR EACH tt_array WHERE tt_array.array_id >= TT_pdf_ext.array_seq_start AND TT_array.array_id <= TT_pdf_ext.array_seq_end:
      EXPORT DELIMITER "~t" tt_array.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "tt_pdf_ext.cache").
  FOR EACH TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream AND TT_pdf_ext.pdf_id = pdfID:
      EXPORT DELIMITER "~t" TT_pdf_ext EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "tt_pdf_font.cache").
  FOR EACH tt_pdf_font WHERE tt_pdf_font.obj_stream = pdfStream AND tt_pdf_font.pdf_id = pdfID:
      EXPORT DELIMITER "~t" tt_pdf_font EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "TT_Font.cache").
  FOR EACH TT_Font WHERE TT_Font.obj_stream = pdfStream AND TT_Font.pdf_id = pdfID:
      EXPORT DELIMITER "~t" TT_Font EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "TT_Info.cache").
  FOR EACH TT_Info WHERE TT_Info.obj_stream = pdfStream AND TT_Info.pdf_id = pdfID:
      EXPORT DELIMITER "~t" TT_Info EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "TT_Object.cache").
  FOR EACH TT_Object WHERE TT_Object.obj_stream = pdfStream AND TT_Object.pdf_id = pdfID:
      EXPORT DELIMITER "~t" TT_Object EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "TT_Resource.cache").
  FOR EACH TT_Resource WHERE TT_Resource.obj_stream = pdfStream AND TT_Resource.pdf_id = pdfID:
      EXPORT DELIMITER "~t" TT_Resource EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(cTmpCacheDir + "TT_Widget.cache").
  FOR EACH TT_Widget WHERE TT_Widget.obj_stream = pdfStream AND TT_Widget.pdf_id = pdfID:
      EXPORT DELIMITER "~t" TT_Widget EXCEPT obj_stream pdf_id.
  END.
  OUTPUT CLOSE.

  /* 14-MAR-2017 jcc: now rename cTmpCacheDir to cCacheDir */
  /* but first check if meantime it has not been created meanwhile by another session;
     just delete it if this is the case. */
  IF SEARCH(cCacheDir + "tt_pdf_ext.cache") = ? THEN
      OS-RENAME VALUE(cTmpCacheDir) VALUE(cCacheDir).
  ELSE DO:
      IF OPSYS = "UNIX" THEN
          OS-COMMAND SILENT VALUE("rm -Rf " + cTmpCacheDir).
      ELSE
          OS-COMMAND SILENT VALUE("rmdir /q /s " + RIGHT-TRIM(REPLACE(cTmpCacheDir, "~/", "~\"), "~\")).
  END.
END. /* pdf_open_PDF */

PROCEDURE pdf_clear_pdf_cache: /* 05-OCT-2015 jcc: new */
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfName   AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cCacheDir AS CHARACTER   NO-UNDO.

    cCacheDir = getPdfCacheDir(pdfStream, pdfName).

    IF OPSYS = "UNIX" THEN
        OS-COMMAND SILENT VALUE("rm -Rf " + cCacheDir).
    ELSE
        OS-COMMAND SILENT VALUE("rmdir /q /s " + RIGHT-TRIM(REPLACE(cCacheDir, "~/", "~\"), "~\")).

END PROCEDURE.

/* 11-DEC-2014 jcc: new API (= pdf_use_pdf_page extended with pcOptions) */
PROCEDURE pdf_place_PDF_page:
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfID     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfPage   AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcOptions AS CHARACTER   NO-UNDO. /* Rotate, Scale, X, Y, Background, Border, UsePdfPage */

    DEFINE VARIABLE cBackground   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cBorder       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFoundWidget  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMatrix       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dAngle        AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE dBorderWeight AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE deCos         AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE deSin         AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE dPageHeight   AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE dPageWidth    AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE dRadius       AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE dScale        AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE dTmp          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dX            AS DECIMAL     DECIMALS 5 INITIAL ? NO-UNDO.
    DEFINE VARIABLE dY            AS DECIMAL     DECIMALS 5 INITIAL ? NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iRotate       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE L_Page        AS INTEGER     NO-UNDO.

    DEFINE BUFFER TT_pdf_page         FOR TT_pdf_page.
    DEFINE BUFFER B_TT_pdf_page       FOR TT_pdf_page.
    DEFINE BUFFER TT_Widget           FOR TT_Widget.
    DEFINE BUFFER TT_pdf_ext          FOR TT_pdf_ext.
    DEFINE BUFFER TT_pdf_external     FOR TT_pdf_external. /* 05-JUN-2014 jcc: see below */
    DEFINE BUFFER TT_Object           FOR TT_Object.
    DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfID) + "," + STRING(pdfPage)). &ENDIF

    IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    FIND FIRST TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream AND TT_pdf_ext.pdf_id = pdfID NO-ERROR.
    IF NOT AVAILABLE TT_pdf_ext THEN {pdferror.i &msg="'Cannot find PDF ID (' + pdfID + ') !'" &return=YES}.

    ASSIGN
     dPageWidth  = pdf_PageWidth(pdfStream)
     dPageHeight = pdf_PageHeight(pdfStream).

    /* 11-DEC-2014 jcc: rotate the template within the page */
    iRotate = INTEGER(GetWidgetOption2("Rotate", pcOptions, "0")) NO-ERROR. IF ERROR-STATUS:ERROR THEN iRotate = 0.

    IF NOT CAN-FIND(FIRST TT_info
                    WHERE TT_info.pdf_id = pdfID
                      AND TT_info.info_name = "Page"
                      AND TT_info.info_value = STRING(pdfPage) NO-LOCK)
    THEN {pdferror.i &msg="'Invalid Page # for PDF ID = ' + pdfID" &return=YES}.

    /* 27-AUG-2012 jcc: limitation when retaining the annots: the form page can be used only once */
    IF pdf_get_parameter(pdfStream, "retainAnnots") > ""
        AND CAN-FIND(FIRST tt_pdf_page_use_ext 
                WHERE tt_pdf_page_use_ext.obj_Stream = pdfStream
                  AND tt_pdf_page_use_ext.pdf_id_use = pdfID /* 02-DEC-2014 jcc */
                  AND tt_pdf_page_use_ext.page_use   = pdfPage)
        /* 15-DEC-2014 jcc: don't prohibit this if the external file has no annotations */
        AND CAN-FIND(FIRST TT_Resource
                WHERE TT_Resource.obj_stream = pdfStream
                  AND TT_Resource.pdf_id     = pdfID
                  AND TT_Resource.page_id    = pdfPage
                  AND TT_Resource.res_type   = "Annot")
        /* AND CAN-FIND(FIRST B_TT_pdf_page */
                /* WHERE B_TT_pdf_page.obj_Stream = pdfStream */
                  /* AND B_TT_pdf_page.pdf_id_use = pdfID /* 02-DEC-2014 jcc */ */
                  /* AND B_TT_pdf_page.page_use   = pdfPage) */
    THEN {pdferror.i &msg="'Cannot use twice the same page #' + STRING(pdfPage) + ' for PDF ID ' + QUOTER(pdfID) + ' when using the retainAnnots parameter!'" &return=YES}.

    /* 05-DEC-2014 jcc: moved after having changed the page size!!! */
    /* RUN OutputTextContent(pdfStream, */
                          /* "IMAGE-EXT", */
                          /* "~/" + pdfID + STRING(pdfPage) + " Do", */
                          /* "", */
                          /* ""). */

    /* 05-JUN-2014 jcc: must use FIND else we get an old record of tt_pdf_external outside of the IF CAN-FIND() ... END block, because the buffer was not locally scoped */
    /* IF NOT CAN-FIND(FIRST TT_pdf_external */
                    /* WHERE TT_pdf_external.obj_stream = pdfStream */
                      /* AND TT_pdf_external.ext_tag    = "~/" + pdfID + STRING(pdfPage)) */
    FIND FIRST TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStream
      AND TT_pdf_external.ext_tag    = "~/" + pdfID + STRING(pdfPage) NO-ERROR.
    IF NOT AVAILABLE TT_pdf_external THEN DO:
      FIND FIRST TT_Object 
           WHERE TT_Object.obj_stream = pdfStream
             AND TT_Object.pdf_id     = pdfID
             AND TT_Object.obj_type   = "Page"
             AND TT_Object.page_id    = pdfPage NO-ERROR.

      CREATE TT_pdf_external.
      ASSIGN TT_pdf_external.obj_stream  = pdfStream
             TT_pdf_external.ext_page    = pdfPage.
             /* TT_pdf_external.page_id     = pdf_Page(pdfStream). */
      ASSIGN TT_pdf_external.ext_tag     = "~/" + pdfID + STRING(pdfPage)
             TT_pdf_external.pdf_id      = pdfID
             /* 23-SEP-2015 jcc: now must use the pdf_id with which the file has been extracted, i.e. pdf_id_orig. See pdf_open_pdf. */
             /* TT_pdf_external.ext_file    = TT_pdf_ext.cache_dir + pdfID + STRING(pdfPage) + ".txt" */
             TT_pdf_external.ext_file    = TT_pdf_ext.cache_dir + TT_pdf_ext.pdf_id_orig + STRING(pdfPage) + ".txt"
             /* 15-DEC-2014 jcc: what is the point in limiting/extending the external page mediabox to the current page dimensions? */
             /* TT_pdf_external.ext_media1  = 0 */
             /* TT_pdf_external.ext_media2  = 0 */
             /* TT_pdf_external.ext_media3  = DEC(pdf_PageWidth(pdfstream)) */
             /* TT_pdf_external.ext_media4  = DEC(pdf_PageHeight(pdfstream)) */
             TT_pdf_external.ext_media1 = IF AVAILABLE TT_Object THEN TT_Object.obj_Media1 ELSE 0
             TT_pdf_external.ext_media2 = IF AVAILABLE TT_Object THEN TT_Object.obj_Media2 ELSE 0
             TT_pdf_external.ext_media3 = IF AVAILABLE TT_Object THEN TT_Object.obj_Media3 ELSE dPageWidth
             TT_pdf_external.ext_media4 = IF AVAILABLE TT_Object THEN TT_Object.obj_Media4 ELSE dPageHeight
             /* 15-DEC-2014 jcc: end */
             TT_pdf_external.ext_rotate  = IF AVAILABLE TT_Object THEN TT_Object.rotate ELSE 0.
      IF pdf_get_parameter(pdfStream,"UseExternalPageSize") = "TRUE" AND AVAILABLE TT_Object THEN DO:
        ASSIGN TT_pdf_external.ext_media1 = TT_Object.obj_Media1
               TT_pdf_external.ext_media2 = TT_Object.obj_Media2
               TT_pdf_external.ext_media3 = TT_Object.obj_Media3
               TT_pdf_external.ext_media4 = TT_Object.obj_Media4.

        ASSIGN TT_pdf_external.ext_Crop1 = TT_Object.obj_Crop1
               TT_pdf_external.ext_Crop2 = TT_Object.obj_Crop2
               TT_pdf_external.ext_Crop3 = TT_Object.obj_Crop3
               TT_pdf_external.ext_Crop4 = TT_Object.obj_Crop4.
      END.
    END.
    /* 03-DEC-2014 jcc: tt_pdf_external might exist when we "reuseExternal" */
    /* ELSE ASSIGN */
        /* TT_pdf_external.page_id = pdf_Page(pdfStream). */

    /* Find the current page record and tell it what external page we are using */
    L_Page = pdf_page(pdfStream).
    FIND FIRST B_TT_pdf_page 
         WHERE B_TT_pdf_page.obj_Stream = pdfStream
           AND B_TT_pdf_page.page_nbr   = L_Page NO-ERROR.
    IF AVAILABLE B_TT_pdf_page AND LOGICAL(GetWidgetOption2("UsePdfPage", pcOptions, "NO")) THEN DO:
      IF pdf_get_parameter(pdfStream,"UseExternalPageSize") = "TRUE" THEN DO: /* 05-DEC-2014 jcc: add this condition */
          IF TT_pdf_external.ext_rotate = 90 OR TT_pdf_external.ext_rotate = 270 THEN ASSIGN
              B_TT_pdf_page.page_width  = TT_pdf_external.ext_media4
              B_TT_pdf_page.page_height = TT_pdf_external.ext_media3.
          ELSE ASSIGN /*B_TT_pdf_page.page_use    = pdfPage*/
              B_TT_pdf_page.page_width  = TT_pdf_external.ext_media3
              B_TT_pdf_page.page_height = TT_pdf_external.ext_media4.
          /* 11-DEC-2014 jcc: now rotate the external pdf within the page: if we use the external page size, swap the page dimensions */
          IF iRotate = 90 OR iRotate = 270 THEN ASSIGN
              dTmp                      = B_TT_pdf_page.page_width
              B_TT_pdf_page.page_width  = B_TT_pdf_page.page_height
              B_TT_pdf_page.page_height = dTmp.

          /* 05-JUN-2014 jcc: as we change the page size, reset the text position and update pageWidth & pageHeight parameters */
          RUN pdf_set_PageWidth (pdfStream, B_TT_pdf_page.page_width).
          RUN pdf_set_PageHeight(pdfStream, B_TT_pdf_page.page_height).
          ASSIGN
           dPageWidth  = B_TT_pdf_page.page_width
           dPageHeight = B_TT_pdf_page.page_height.
          /* 15-APR-2015 jcc: moved below, as this depends if the external page has a cropbox or not */
          /* RUN pdf_set_TextXY(pdfStream, pdf_LeftMargin(pdfStream), B_TT_pdf_page.page_height - pdf_TopMargin(pdfStream) */
                             /* 01-APR-2015 jcc: take into account the case when the external pdf has got a cropbox larger than its mediabox */
                             /* - TT_pdf_external.ext_Crop2 */
                             /* + IF tt_pdf_external.ext_rotate = 90 OR TT_pdf_external.ext_rotate = 270 */
                               /* THEN TT_pdf_external.ext_Crop3 - TT_pdf_external.ext_media3 */
                               /* ELSE TT_pdf_external.ext_Crop4 - TT_pdf_external.ext_media4, YES). */
      END.
      /* 03-NOV-2014 jcc: when using the external page size, use also its orientation: not done this way, but through a Matrix in pdf_load_external */
      /* IF pdf_get_parameter(pdfStream, "UseExternalPageSize") = "TRUE" THEN */
          /* B_TT_pdf_page.page_rotate = TT_pdf_external.ext_rotate. */
      IF NOT (    TT_pdf_external.ext_Crop1 = 0 AND TT_pdf_external.ext_Crop2 = 0
              AND TT_pdf_external.ext_Crop3 = 0 AND TT_pdf_external.ext_Crop3 = 0) THEN DO:
          IF tt_pdf_external.ext_rotate = 90 OR TT_pdf_external.ext_rotate = 270 THEN /* 03-NOV-2014 jcc: 270 */
              B_TT_pdf_page.page_crop = dec2string(TT_pdf_external.ext_Crop1) + " "
                                      + dec2string(TT_pdf_external.ext_Crop2) + " "
                                      + dec2string(TT_pdf_external.ext_Crop4) + " "
                                      + dec2string(TT_pdf_external.ext_Crop3).
          ELSE
              B_TT_pdf_page.page_crop = dec2string(TT_pdf_external.ext_Crop1) + " "
                                      + dec2string(TT_pdf_external.ext_Crop2) + " "
                                      + dec2string(TT_pdf_external.ext_Crop3) + " " 
                                      + dec2string(TT_pdf_external.ext_Crop4).
              RUN pdf_set_TextXY(pdfStream, pdf_LeftMargin(pdfStream), B_TT_pdf_page.page_height - pdf_TopMargin(pdfStream)
                                 /* 01-APR-2015 jcc: take into account the case when the external pdf has got a cropbox larger than its mediabox */
                                 - TT_pdf_external.ext_Crop2
                                 + IF tt_pdf_external.ext_rotate = 90 OR TT_pdf_external.ext_rotate = 270
                                   THEN TT_pdf_external.ext_Crop3 - TT_pdf_external.ext_media3
                                   ELSE TT_pdf_external.ext_Crop4 - TT_pdf_external.ext_media4, YES).
      END.
      ELSE
          RUN pdf_set_TextXY(pdfStream, pdf_LeftMargin(pdfStream), B_TT_pdf_page.page_height - pdf_TopMargin(pdfStream), YES).
    END.

    /* 15-APR-2015 jcc: Scale option */
    dScale  = string2dec(GetWidgetOption2("Scale", pcOptions, "1")) NO-ERROR. IF ERROR-STATUS:ERROR THEN dScale = 1.

    /* compute the rotation matrix */
    CASE iRotate:
        WHEN   0 THEN cMatrix = "".
        WHEN  90 THEN cMatrix = "0 1 -1 0 0 0".
        WHEN 180 THEN cMatrix = "-1 0 0 -1 0 0".
        WHEN 270 THEN cMatrix = "0 -1 1 0 0 0".
        OTHERWISE ASSIGN
            dRadius = SQRT(EXP(TT_pdf_external.ext_Media3, 2) + EXP(TT_pdf_external.ext_Media4, 2)) / 2 * dScale
            dAngle  = math_atan(TT_pdf_external.ext_Media3 / TT_pdf_external.ext_Media4) + 90
            deCos   = math_cos(DECIMAL(iRotate))
            deSin   = math_sin(DECIMAL(iRotate))
            cMatrix = dec2string(deCos) + " " + dec2string(deSin) + " " + dec2string(- deSin) + " " + dec2string(deCos) + " 0 0".
    END CASE.

    /* 15-APR-2015 jcc: Scale option */
    IF cMatrix > "" AND dScale <> 1.0 THEN
        cMatrix = multiplyTransformationMatrices(dec2string(dScale) + " 0 0 " + dec2string(dScale) + " 0 0", cMatrix).
    ELSE IF dScale <> 1.0 THEN
        cMatrix = dec2string(dScale) + " 0 0 " + dec2string(dScale) + " 0 0".

    /* 16-APR-2015 jcc: Position (X & Y options or center in page) */
    IF pcOptions > "" THEN DO:
        dX = string2dec(GetWidgetOption2("X", pcOptions, ?)) NO-ERROR. IF ERROR-STATUS:ERROR THEN dX = ?.
        dY = string2dec(GetWidgetOption2("Y", pcOptions, ?)) NO-ERROR. IF ERROR-STATUS:ERROR THEN dY = ?.
    END.
    CASE iRotate:
        WHEN 0 THEN IF cMatrix > "" THEN DO:
            ENTRY(5, cMatrix, " ") = IF dX <> ? THEN dec2string(dX) ELSE IF dScale = 1 THEN "0" ELSE dec2string((dPageWidth - TT_pdf_external.ext_Media3 * dScale) / 2).
            ENTRY(6, cMatrix, " ") = IF dY <> ? THEN dec2string(dY) ELSE IF dScale = 1 THEN "0" ELSE dec2string((dPageHeight - TT_pdf_external.ext_Media4 * dScale) / 2).
        END. ELSE IF dX <> ? OR dY <> ? THEN DO:
            cMatrix = "1 0 0 1 " +
                      (IF dX <> ? THEN dec2string(dX) ELSE "0") + " " +
                      (IF dY <> ? THEN dec2string(dY) ELSE "0").
        END. 
        WHEN 90 THEN DO:
            ENTRY(5, cMatrix, " ") = IF dX <> ? THEN dec2string(dX) ELSE IF dScale = 1 THEN dec2string(dPageWidth) ELSE dec2string((dPageWidth + TT_pdf_external.ext_Media4 * dScale) / 2).
            ENTRY(6, cMatrix, " ") = IF dY <> ? THEN dec2string(dY) ELSE IF dScale = 1 THEN "0" ELSE dec2string((dPageHeight - TT_pdf_external.ext_Media3 * dScale) / 2).
        END.
        WHEN 180 THEN DO:
            ENTRY(5, cMatrix, " ") = IF dX <> ? THEN dec2string(dX) ELSE IF dScale = 1 THEN dec2string(dPageWidth) ELSE dec2string((dPageWidth + TT_pdf_external.ext_Media3 * dScale) / 2).
            ENTRY(6, cMatrix, " ") = IF dY <> ? THEN dec2string(dY) ELSE IF dScale = 1 THEN dec2string(dPageHeight) ELSE dec2string((dPageHeight + TT_pdf_external.ext_Media4 * dScale) / 2).
        END.
        WHEN 270 THEN DO:
            ENTRY(5, cMatrix, " ") = IF dX <> ? THEN dec2string(dX) ELSE IF dScale = 1 THEN "0" ELSE dec2string((dPageWidth - TT_pdf_external.ext_Media4 * dScale) / 2).
            ENTRY(6, cMatrix, " ") = IF dY <> ? THEN dec2string(dY) ELSE IF dScale = 1 THEN dec2string(dPageHeight) ELSE dec2string((dPageHeight + TT_pdf_external.ext_Media3 * dScale) / 2).
        END.
        OTHERWISE DO:
            ENTRY(5, cMatrix, " ") = IF dX <> ? THEN dec2string(dX) ELSE dec2string(ROUND(dRadius * math_cos(iRotate - dAngle) + dPageWidth / 2, 5)).
            ENTRY(6, cMatrix, " ") = IF dY <> ? THEN dec2string(dY) ELSE dec2string(ROUND(dRadius * math_sin(iRotate - dAngle) + dPageHeight / 2, 5)).
        END.
    END.

    IF cMatrix > "" THEN cMatrix = cMatrix + " cm ".

    RUN OutputTextContent(pdfStream, 
                          "QOPEN", /* open a new graphic context */
                          cMatrix,
                          "",
                          "").
    ASSIGN
     cBackground   = GetWidgetOption("Background", pcOptions)
     cBorder       = GetWidgetOption("Border", pcOptions)
     dBorderWeight = string2dec(GetWidgetOption2("BorderWeight", pcOptions, "0.5")).
    IF cBorder > "" OR cBackground > "" THEN DO:
        IF cBorder > "" THEN
            RUN pdf_stroke_color (pdfStream, string2dec(ENTRY(1, cBorder, ";")), string2dec(ENTRY(2, cBorder, ";")), string2dec(ENTRY(3, cBorder, ";"))).
        IF cBackground > "" THEN DO:
            RUN pdf_stroke_fill (pdfStream, string2dec(ENTRY(1, cBackground, ";")), string2dec(ENTRY(2, cBackground, ";")), string2dec(ENTRY(3, cBackground, ";"))).
            RUN pdf_rectdec (pdfStream,0,0,TT_pdf_external.ext_Media3,TT_pdf_external.ext_Media4,dBorderWeight).
        END.
        ELSE
            RUN pdf_rect2(pdfStream,0,0,TT_pdf_external.ext_Media3,TT_pdf_external.ext_Media4,dBorderWeight).
    END.

    /* Put the external page */
    RUN OutputTextContent(pdfStream, 
                          "QCLOSE", /* close the current graphic context, so that further graphic API calls will have their context reset */
                          (IF cBackground > "" THEN "0 0 0 rg 0 0 0 RG " ELSE "") + "~/" + pdfID + STRING(pdfPage) + " Do",
                          "",
                          "").

    IF AVAILABLE B_TT_pdf_page AND LOGICAL(GetWidgetOption2("UsePdfPage", pcOptions, "NO")) THEN DO:
      /* Now manage annotations */

      /* 25-MAY-2013 jcc: build the lists of flattened and retained widgets for the page */
      /* 30-OCT-2014 jcc: added "OR TT_Widget.widget_page = 0" because the /P (page) of the widget is optional, thus it possible we do not have the page number */
      /* 15-DEC-2014 jcc: "AND TT_Widget.pdf_id = pdfID" was missing */
      DEFINE VARIABLE cFormFlatten AS CHARACTER   NO-UNDO.
      cFormFlatten = pdf_get_parameter2(pdfStream, "formFlatten", "*").
      FOR EACH TT_Widget
          WHERE TT_Widget.obj_stream   = pdfStream
            AND TT_Widget.pdf_id       = pdfID
            AND (TT_Widget.widget_page = pdfPage OR TT_Widget.widget_page = 0):
          IF CAN-DO(cFormFlatten, TT_Widget.widget_name) THEN
              B_TT_pdf_page.widget_flatten = B_TT_pdf_page.widget_flatten + CHR(1) + TT_Widget.widget_name.
          ELSE
              B_TT_pdf_page.widget_retain  = B_TT_pdf_page.widget_retain + CHR(1) + TT_Widget.widget_name.
      END.

      /* 28-MAY-2013 jcc: Cannot use twice the same page when some form widgets are retained */
      /* IF B_TT_pdf_page.widget_retain > "" AND CAN-FIND(FIRST TT_pdf_page WHERE TT_pdf_page.page_use = pdfPage AND TT_pdf_page.widget_retain > "") THEN */
          /* {pdferror.i &msg="'Cannot use twice the same page #' + STRING(pdfPage) + ' for PDF ID (' + pdfID + ') when some form widgets are retained!'" &return=YES}. */
      /* 31-MAY-2013 jcc: instead, check that no widget is displayed twice. This allows exotic things like flattening one part on the form widgets on one page, and the other part on another page. */
      cFoundWidget = "".
      IF B_TT_pdf_page.widget_retain > "" THEN checkPage: DO:
          /* FOR EACH TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream */
            /* AND TT_pdf_page.pdf_id_use = pdfID /* 02-DEC-2014 jcc */ */
            /* AND TT_pdf_page.page_use = pdfPage */
            /* AND TT_pdf_page.widget_retain > "": */
          FOR EACH tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
                AND tt_pdf_page_use_ext.pdf_id_use = pdfID
                AND tt_pdf_page_use_ext.page_use = pdfPage,
              FIRST TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                AND TT_pdf_page.page_nbr = tt_pdf_page_use_ext.page_nbr
                AND TT_pdf_page.widget_retain > "":
              DO i = NUM-ENTRIES(B_TT_pdf_page.widget_retain, CHR(1)) TO 1 BY -1:
                  IF LOOKUP(ENTRY(i, B_TT_pdf_page.widget_retain, CHR(1)), TT_pdf_page.widget_retain, CHR(1)) > 0 THEN DO:
                      cFoundWidget = ENTRY(i, B_TT_pdf_page.widget_retain, CHR(1)).
                      LEAVE checkPage.
                  END.
              END.
          END.
      END.
      IF cFoundWidget > "" THEN
          {pdferror.i &msg="'Cannot use the widget ' + QUOTER(cFoundWidget) + ' (defined in PDF ID ' + QUOTER(pdfID) + ', page #' + STRING(pdfPage) + ') twice on the same document (already used on page #' + STRING(TT_pdf_page.page_nbr) + ', attempting to use it on page #' + STRING(pdf_Page(pdfStream)) + ')'" &return=YES}.
    END.

    IF AVAILABLE B_TT_pdf_page THEN DO:
      /* 28-MAY-2013 jcc: move this assignation here else the test above will always be true, for the current page */
      /* ASSIGN */
       /* B_TT_pdf_page.pdf_id_use = pdfID /* 02-DEC-2014 jcc */ */
       /* B_TT_pdf_page.page_use   = pdfPage. */
      CREATE tt_pdf_page_use_ext.
      ASSIGN
          tt_pdf_page_use_ext.obj_stream = pdfStream
          tt_pdf_page_use_ext.page_nbr   = B_TT_pdf_page.page_nbr
          tt_pdf_page_use_ext.page_use   = pdfPage
          tt_pdf_page_use_ext.pdf_id_use = pdfID.
    END. /* avail b_tt_pdf_page */
END. /* pdf_place_PDF_page */

PROCEDURE pdf_use_PDF_page:
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfID     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfPage   AS INTEGER     NO-UNDO.

    RUN pdf_place_PDF_page (pdfStream, pdfID, pdfPage, "UsePdfPage").
END. /* pdf_use_PDF_page */

/* 30-OCT-2013 jcc: this new API allows to get any information from an external pdf. It returns
   the value for a given pdf path, e.g. /Root/Pages/Kids[1]/Cropbox or /Root/Pages/Count */
PROCEDURE pdf_ext_get_path:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfID     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfObject AS CHARACTER   NO-UNDO. /* can be ?, in this case we start at the root */
    DEFINE INPUT  PARAMETER pdfPath   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER ocPath    AS CHARACTER   INITIAL ? NO-UNDO.
    DEFINE OUTPUT PARAMETER ocType    AS CHARACTER   INITIAL ? NO-UNDO.
    DEFINE OUTPUT PARAMETER ocValue   AS CHARACTER   INITIAL ? NO-UNDO.

    DEFINE BUFFER TT_Object FOR TT_Object.
    DEFINE BUFFER TT_dict   FOR TT_dict.
    DEFINE BUFFER TT_array  FOR TT_array.

    DEFINE VARIABLE cContainerType AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEntry         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cType          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cValue         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i              AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iDictOrArray   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER     NO-UNDO.

    pdfPath = TRIM(pdfPath, "~/"). /* remove leading / */
    cEntry  = ENTRY(1, pdfPath, "~/").

    /* start at the document's root */
    IF pdfObject = ? THEN DO:
        IF cEntry <> "Root" AND cEntry <> "Info" THEN RETURN.

        ocPath = "~/" + cEntry.

        FIND FIRST TT_Object WHERE TT_Object.obj_stream = pdfStream
            AND TT_Object.pdf_id = pdfID
            AND TT_Object.obj_type = "~/" + cEntry NO-ERROR.

        IF NOT AVAILABLE TT_Object THEN RETURN.

        ASSIGN
         iDictOrArray   = TT_Object.obj_dict_id
         cContainerType = "DICT"
         cType          = TT_Object.obj_value_type.
    END.
    /* start at a given point: pdfObject <> ? */
    ELSE DO:
        IF NOT pdfObject MATCHES "DICT#*" THEN RETURN.

        iDictOrArray = INTEGER(ENTRY(2, ENTRY(1, pdfObject, CHR(1)), "#")) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN.

        ocPath = "~/" + cEntry.

        IF INDEX(cEntry, "[") > 0 THEN DO:
            iIndex = INTEGER(ENTRY(1, ENTRY(2, cEntry, "["), "]")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN.
            cEntry = ENTRY(1, cEntry, "[").
        END.
        ELSE
            iIndex = 0.

        FIND TT_dict WHERE TT_dict.dict_id = iDictOrArray AND TT_dict.dict_key = "~/" + cEntry NO-ERROR.
        IF NOT AVAILABLE TT_dict THEN RETURN.

        ASSIGN
         iDictOrArray   = IF TT_dict.value_dict_id <> -1 THEN TT_dict.value_dict_id ELSE TT_dict.value_array_id
         cContainerType = "DICT"
         cType          = TT_dict.value_type.

        /* if an index has been specified for an array then get the nth element */
        IF iIndex > 0 AND cType = "ARRAY" THEN DO:
            FIND TT_array WHERE TT_array.array_id = iDictOrArray
                AND TT_array.value_id = iIndex NO-ERROR.
            IF NOT AVAILABLE TT_array THEN RETURN.
            ASSIGN
             iDictOrArray   = IF TT_array.value_dict_id <> -1 THEN TT_array.value_dict_id ELSE TT_array.value_array_id
             cContainerType = cType
             cType          = TT_array.value_type.
        END.
    END.

    /* now go through the path */
    IF NUM-ENTRIES(pdfPath, "~/") > 1 THEN
    DO i = 2 TO NUM-ENTRIES(pdfPath, "~/"):
        ASSIGN
         cEntry = ENTRY(i, pdfPath, "~/")
         ocPath = ocPath + "~/" + cEntry.
        IF INDEX(cEntry, "[") > 0 THEN DO:
            iIndex = INTEGER(ENTRY(1, ENTRY(2, cEntry, "["), "]")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN.
            cEntry = ENTRY(1, cEntry, "[").
        END.
        ELSE
            iIndex = 0.

        IF cType = "DICT" THEN DO:
            FIND TT_dict WHERE TT_dict.dict_id = iDictOrArray
                AND TT_dict.dict_key = "~/" + cEntry NO-ERROR.
            IF NOT AVAILABLE TT_dict THEN RETURN.
            ASSIGN
             iDictOrArray   = IF TT_dict.value_dict_id <> -1 THEN TT_dict.value_dict_id ELSE TT_dict.value_array_id
             cContainerType = cType
             cType          = TT_dict.value_type.
            /* if an index has been specified for an array then get the nth element */
            IF iIndex > 0 AND cType = "ARRAY" THEN DO:
                FIND TT_array WHERE TT_array.array_id = iDictOrArray
                    AND TT_array.value_id = iIndex NO-ERROR.
                IF NOT AVAILABLE TT_array THEN RETURN.
                ASSIGN
                 iDictOrArray   = IF TT_array.value_dict_id <> -1 THEN TT_array.value_dict_id ELSE TT_array.value_array_id
                 cContainerType = cType
                 cType          = TT_array.value_type.
            END.
        END.
        ELSE
            RETURN.
    END. /* DO i = 2 TO NUM-ENTRIES(pdfPath, ",") */

    /* now build the value to return */
    RUN _pdf_ext_build_value(cContainerType, cType, iDictOrArray, BUFFER TT_dict, BUFFER TT_array, OUTPUT ocValue).
    ocType  = cType.

END PROCEDURE. /* pdf_ext_get_path */

PROCEDURE _pdf_ext_build_value: /* PRIVATE */
    DEFINE INPUT  PARAMETER icContainerType AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER icValueType     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iiDictOrArray   AS INTEGER     NO-UNDO.
    DEFINE PARAMETER BUFFER TT_dict  FOR TT_dict.
    DEFINE PARAMETER BUFFER TT_array FOR TT_array.
    DEFINE OUTPUT PARAMETER ocValue         AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bTT_dict  FOR TT_dict.
    DEFINE BUFFER bTT_array FOR TT_array.

    /* now build the value to return */
    CASE icValueType:
        WHEN "DICT" THEN DO:
            ocValue = "DICT#" + STRING(iiDictOrArray).
            FOR EACH bTT_dict WHERE bTT_dict.dict_id = iiDictOrArray:
                ocValue = ocValue + CHR(1)
                        + bTT_dict.dict_key + " "
                        + IF bTT_dict.value_type = "SCALAR" THEN bTT_dict.value_scalar
                          ELSE IF bTT_dict.value_type = "DICT" THEN "DICT#" + STRING(bTT_dict.value_dict_id)
                          ELSE IF bTT_dict.value_type = "ARRAY" THEN "ARRAY#" + STRING(bTT_dict.value_array_id)
                          ELSE " ".
            END.
        END.
        WHEN "ARRAY" THEN DO:
            ocValue = "ARRAY#" + STRING(iiDictOrArray).
            FOR EACH bTT_array WHERE bTT_array.array_id = iiDictOrArray BY bTT_array.value_id:
                ocValue = ocValue + CHR(1)
                        + IF bTT_array.value_type = "SCALAR" THEN bTT_array.value_scalar
                          ELSE IF bTT_array.value_type = "DICT" THEN "DICT#" + STRING(bTT_array.value_dict_id)
                          ELSE IF bTT_array.value_type = "ARRAY" THEN "ARRAY#" + STRING(bTT_array.value_array_id)
                          ELSE " ".
            END.
        END.
        WHEN "SCALAR" THEN
            ocValue = IF icContainerType = "DICT" THEN TT_dict.value_scalar ELSE TT_array.value_scalar.
    END CASE.
END PROCEDURE.

PROCEDURE pdf_ext_get_page:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfID     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfPage   AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER ocPage    AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cPath  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cType  AS CHARACTER   NO-UNDO.

    RUN pdf_ext_get_path(pdfStream, pdfID, ?, "/Root/Pages/Kids[" + STRING(pdfPage) + "]", OUTPUT cPath, OUTPUT cType, OUTPUT ocPage).
END PROCEDURE. /* pdf_ext_get_page */

PROCEDURE pdf_ext_get_nb_pages:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfID     AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oiPages   AS INTEGER     NO-UNDO.

    DEFINE VARIABLE cPath  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cType  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cValue AS CHARACTER   NO-UNDO.

    RUN pdf_ext_get_path(pdfStream, pdfID, ?, "/Root/Pages/Count", OUTPUT cPath, OUTPUT cType, OUTPUT cValue).
    oiPages = INTEGER(cValue).
    IF oiPages = ? THEN DO:
        RUN pdf_ext_get_path(pdfStream, pdfID, ?, "/Root/Pages/Kids", OUTPUT cPath, OUTPUT cType, OUTPUT cValue).
        IF cValue <> ? THEN
            oiPages = NUM-ENTRIES(cValue, CHR(1)) - 1.
    END.
END PROCEDURE. /* pdf_ext_get_page */
/* 31-OCT-2013 jcc: end */

/* Procedures used to export back external pdf files */

PROCEDURE recursivelyAssignNewObjs: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream        AS CHARACTER   NO-UNDO.
    DEFINE PARAMETER BUFFER TT_Object FOR TT_Object.
    DEFINE INPUT PARAMETER pcValueType      AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piDictOrArrayId  AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcDesc           AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcCallBack       AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcCallBackParams AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bTT_Object FOR TT_Object.
    DEFINE BUFFER TT_dict FOR TT_dict.
    DEFINE BUFFER TT_array FOR TT_array.

    IF AVAILABLE TT_Object AND TT_Object.obj_id <> 0 THEN DO:
        /* Do not process twice an object */
        IF TT_Object.new_obj > 0 THEN RETURN. /* 18-DEC-2014 jcc: now done outside of recursivelyAssignNewObjs to save calls */
        /* Choose the new object id */
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, pcDesc, 0, 0, "").
        TT_Object.new_obj = pdf_inc_ObjectSequence.
    END.

    IF pcCallBack > "" THEN
        RUN VALUE(pcCallBack) (pdfStream, pcValueType, piDictOrArrayId, pcCallBackParams).

    CASE pcValueType:
        WHEN "SCALAR" THEN RETURN. /* should never happen, as scalars are resolved by resolveIndirects */
        WHEN "ARRAY" THEN DO:
            FOR EACH TT_array WHERE TT_array.array_id = piDictOrArrayId: 
                CASE TT_array.value_type:
                    WHEN "ARRAY" THEN DO:
                        IF TT_array.value_array_id = -1 THEN NEXT. /* skip empty arrays */
                        FIND FIRST bTT_Object /* 01-OCT-2015 jcc: add FIRST: only export the same array once in case of merged files */
                            WHERE bTT_Object.obj_array_id = TT_array.value_array_id
                              AND bTT_Object.obj_stream = pdfStream
                              AND bTT_Object.obj_value_type = "ARRAY"  NO-ERROR.
                        /* IF NOT AVAILABLE bTT_Object OR (AVAILABLE bTT_Object AND bTT_Object.obj_id <> 0 AND bTT_Object.new_obj <= 0) THEN */
                        RUN recursivelyAssignNewObjs(pdfStream, BUFFER bTT_Object, "ARRAY", TT_array.value_array_id, pcDesc, pcCallBack, pcCallBackParams).
                    END.
                    WHEN "DICT" THEN DO:
                        IF TT_array.value_dict_id = -1 THEN NEXT. /* skip empty dictionaries */
                        FIND FIRST bTT_Object /* 01-OCT-2015 jcc: add FIRST: only export the same dictionnary once in case of merged files */
                            WHERE bTT_Object.obj_dict_id = TT_array.value_dict_id
                              AND bTT_Object.obj_stream = pdfStream
                              AND bTT_Object.obj_value_type = "DICT" NO-ERROR.
                        /* IF NOT AVAILABLE bTT_Object OR (AVAILABLE bTT_Object AND bTT_Object.obj_id <> 0 AND bTT_Object.new_obj <= 0) THEN */
                        RUN recursivelyAssignNewObjs(pdfStream, BUFFER bTT_Object, "DICT", TT_array.value_dict_id, pcDesc, pcCallBack, pcCallBackParams).
                    END.
                END CASE.
            END.
        END.
        WHEN "DICT" THEN DO:
            FOR EACH TT_dict WHERE TT_dict.dict_id = piDictOrArrayId: 
                CASE TT_dict.value_type:
                    WHEN "ARRAY" THEN DO:
                        IF TT_dict.value_array_id = -1 THEN NEXT. /* skip empty arrays */
                        FIND FIRST bTT_Object /* 01-OCT-2015 jcc: idem */
                            WHERE bTT_Object.obj_array_id = TT_dict.value_array_id
                              AND bTT_Object.obj_stream = pdfStream
                              AND bTT_Object.obj_value_type = "ARRAY" NO-ERROR.
                        /* IF NOT AVAILABLE bTT_Object OR (AVAILABLE bTT_Object AND bTT_Object.obj_id <> 0 AND bTT_Object.new_obj <= 0) THEN */
                        RUN recursivelyAssignNewObjs(pdfStream, BUFFER bTT_Object, "ARRAY", TT_dict.value_array_id, pcDesc, pcCallBack, pcCallBackParams).
                    END.
                    WHEN "DICT" THEN DO:
                        IF TT_dict.value_dict_id = -1 THEN NEXT. /* skip empty dictionaries */
                        FIND FIRST bTT_Object /* 01-OCT-2015 jcc: idem */
                            WHERE bTT_Object.obj_dict_id = TT_dict.value_dict_id
                              AND bTT_Object.obj_stream = pdfStream
                              AND bTT_Object.obj_value_type = "DICT" NO-ERROR.
                        /* IF NOT AVAILABLE bTT_Object OR (AVAILABLE bTT_Object AND bTT_Object.obj_id <> 0 AND bTT_Object.new_obj <= 0) THEN */
                        RUN recursivelyAssignNewObjs(pdfStream, BUFFER bTT_Object, "DICT", TT_dict.value_dict_id, pcDesc, pcCallBack, pcCallBackParams).
                    END.
                END CASE.
            END.
        END.
    END CASE.
END PROCEDURE.

/* 07-OCT-2013 jcc: export scalar values, encrypting strings when the pdf is encrypted */
PROCEDURE exportScalar: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream             AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcSeparator           AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcValue               AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plEncrypt             AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER piObj                 AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pmEncryptKey          AS MEMPTR      NO-UNDO.

    IF CAN-DO("(,<", SUBSTRING(pcValue,1,1)) THEN DO: /* string */
        PUT STREAM S_pdf_inc UNFORMATTED pcSeparator.
        RUN putString (pdfStream,
            SUBSTRING(pcValue,1,1), SUBSTRING(pcValue, 2, LENGTH(pcValue, "character":u) - 2),
            NO, NO, plEncrypt, piObj, pmEncryptKey).
    END.
    ELSE /* other: numeric, /Name */
        PUT STREAM S_pdf_inc UNFORMATTED pcSeparator pcValue.

END PROCEDURE.

/* 13-FEB-2014 jcc: put a string to the pdf file, encrypting if necessary */
PROCEDURE putString: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream             AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcValueType           AS CHARACTER   NO-UNDO. /* "(" or "<" */
    DEFINE INPUT PARAMETER pcValue               AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plUnicode             AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER plProtectSpecialChars AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER plEncrypt             AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER piObj                 AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pmEncryptKey          AS MEMPTR      NO-UNDO.

    DEFINE VARIABLE iLength AS INTEGER   NO-UNDO.
    DEFINE VARIABLE mString AS MEMPTR    NO-UNDO.

    IF plEncrypt AND (pmEncryptKey = ? OR GET-SIZE(pmEncryptKey) = 0) THEN
        RUN GetEncryptKey IN h_PDF-Encrypt (pdfStream, TT_pdf_stream.obj_UniqueID, piObj, 0,
                                            /*pdf-EncryptKeyMemPtr,*/ OUTPUT pmEncryptKey).

    IF plUnicode THEN DO: /* 11-FEB-2014 jcc: added */
        IF plEncrypt THEN DO:
            RUN utf8_to_utf16be (pdfStream, pcValue, "putString", YES, OUTPUT mString).
            RUN EncryptContent IN h_PDF-Encrypt (pdfStream, pmEncryptKey, mString, OUTPUT mString).
            PUT STREAM S_pdf_inc UNFORMATTED "<".
            PUT STREAM S_PDF_inc UNFORMATTED STRING(HEX-ENCODE(mString)).
            PUT STREAM S_pdf_inc UNFORMATTED ">".
        END.
        ELSE DO:
            RUN utf8_to_utf16be (pdfStream, pcValue, "putString", NO, OUTPUT mString).
            PUT STREAM S_pdf_inc UNFORMATTED "(".
            EXPORT STREAM S_pdf_inc mString.
            PUT STREAM S_pdf_inc UNFORMATTED ")".
        END.
        SET-SIZE(mString) = 0.
    END.
    ELSE DO:
        IF plEncrypt THEN DO:
            IF pcValueType = "(" THEN DO:
                iLength = LENGTH(pcValue, "character":u).
                SET-SIZE(mString) = iLength.
                PUT-STRING(mString, 1, iLength) = pcValue.
            END.
            ELSE /* < > delimited string */
                mString = HEX-DECODE(pcValue).
            RUN EncryptContent IN h_PDF-Encrypt (pdfStream, pmEncryptKey, mString, OUTPUT mString).
            PUT STREAM S_pdf_inc UNFORMATTED "<".
            PUT STREAM S_PDF_inc UNFORMATTED STRING(HEX-ENCODE(mString)). /* cannot export mString directly as it may contain the "(" character which would "corrupt" the string */
            PUT STREAM S_pdf_inc UNFORMATTED ">".
            SET-SIZE(mString) = 0.
        END.
        ELSE DO:
            IF plProtectSpecialChars THEN
                pcValue = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(pcValue
                                ,"~\","~\~\"), "(","~\("), ")","~\)"), "[","~\["), "]","~\]").
            PUT STREAM S_pdf_inc UNFORMATTED pcValueType pcValue IF pcValueType = "(" THEN ")" ELSE ">".
        END.
    END.
END PROCEDURE.

/* 06-FEB-2014 jcc: export a file to a stream object, compressing & encrypting the contents when applicable */
PROCEDURE putFileAsStream: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcFileName     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piObj          AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcExtraContent AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcExtraFilter  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfEncrypt     AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: added */
    DEFINE INPUT PARAMETER plCreateNewObj AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: output /Length, close an existing dict */
    DEFINE INPUT PARAMETER plCompress     AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: so that e.g. we do not zip jpeg images */

    DEFINE VARIABLE mContent AS MEMPTR      NO-UNDO.

    IF SEARCH(pcFileName) = ? THEN
        {pdferror.i &msg="'Cannot find file ' + QUOTER(pcFileName)" &return=YES &error=YES &returnmsg=YES}.

    SET-SIZE(mContent) = 0.
    FILE-INFO:FILE-NAME = pcFileName.
    SET-SIZE(mContent) = FILE-INFO:FILE-SIZE.

    INPUT FROM VALUE(pcFileName) BINARY NO-CONVERT NO-MAP NO-ECHO.
    IMPORT mContent.
    INPUT CLOSE.

    RUN putMemptrAsStream (pdfStream
                          ,mContent
                          ,piObj
                          ,pcExtraContent
                          ,pcExtraFilter
                          ,pdfEncrypt
                          ,plCreateNewObj
                          ,plCompress).

    SET-SIZE(mContent) = 0.

    /* 16-OCT-2015 jcc: refactor using putMemptrAsStream
    DEFINE VARIABLE iLength      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRetCode     AS INTEGER   INITIAL ? NO-UNDO.
    DEFINE VARIABLE mContent     AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE m_Encrypted  AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE m_EncryptKey AS MEMPTR    NO-UNDO.

    IF plCreateNewObj THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            piObj " 0 obj" {&pdfSKIP}
            "<<" {&pdfSKIP}.

    IF pcExtraContent > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            pcExtraContent {&pdfSKIP}.

    IF plCompress AND pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
        iRetCode = CompressFile2Buffer(pcFileName, INPUT-OUTPUT mContent, OUTPUT iLength).
        IF iRetCode = 0 THEN DO:
            PUT STREAM S_pdf_inc UNFORMATTED
                "/Filter " (IF pcExtraFilter > ""
                            THEN "[/FlateDecode " + pcExtraFilter + "]"
                            ELSE "/FlateDecode") {&pdfSKIP}.
        END.
        ELSE DO:
            {pdferror.i &msg="'Zlib error: ' + STRING(iRetCode)" &return=NO}. /* continue without compression */
            SET-SIZE(mContent) = 0.
            iRetCode = ?.
        END.
    END.
    ELSE IF pcExtraFilter > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Filter " pcExtraFilter {&pdfSKIP}.

    IF iRetCode = ? THEN
        RUN getFileAsMemptr(pdfStream, pcFileName, ?, INPUT-OUTPUT mContent, OUTPUT iLength).

    /* 13-OCT-2015 jcc: get the encrypted length when encrypting AES (may differ from original length) */
    IF pdfEncrypt AND pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "AES" THEN DO:
        RUN EncryptMemPtr (pdfStream,
                           TT_pdf_stream.obj_UniqueID,
                           piObj,
                           mContent,
                           m_EncryptKey,
                           OUTPUT m_Encrypted).
        SET-SIZE(mContent) = 0.
        /* change the /Length value */
        iLength = GET-SIZE(m_Encrypted).
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Length " iLength {&pdfSKIP}
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP}.

    IF GET-SIZE(m_Encrypted) > 0 THEN DO: /* 13-OCT-2015 jcc: reuse the encrypted result, when available */
        EXPORT STREAM S_PDF_inc m_Encrypted.
        SET-SIZE(m_Encrypted) = 0.
    END.
    ELSE
        RUN OutputMemPtr(pdfStream,
                         FALSE,
                         TT_pdf_stream.obj_UniqueID, 
                         piObj, 
                         mContent,
                         m_EncryptKey).
    SET-SIZE(mContent) = 0. /* Release memory */
    SET-SIZE(m_EncryptKey) = 0.

    PUT STREAM S_pdf_inc UNFORMATTED
        "~n" "endstream" {&PDFSKIP}
        "endobj" {&pdfSKIP}.
    */
END PROCEDURE. /* putFileAsStream */

PROCEDURE putMemptrAsStream: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pmPtr          AS MEMPTR      NO-UNDO.
    DEFINE INPUT PARAMETER piObj          AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcExtraContent AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcExtraFilter  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfEncrypt     AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: added */
    DEFINE INPUT PARAMETER plCreateNewObj AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: output /Length, close an existing dict */
    DEFINE INPUT PARAMETER plCompress     AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: so that e.g. we do not zip jpeg images */

    DEFINE VARIABLE iLength      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRetCode     AS INTEGER   INITIAL ? NO-UNDO.
    DEFINE VARIABLE mPtrZ        AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE m_Encrypted  AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE m_EncryptKey AS MEMPTR    NO-UNDO.

    IF plCreateNewObj THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            piObj " 0 obj" {&pdfSKIP}
            "<<" {&pdfSKIP}.

    IF pcExtraContent > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            pcExtraContent {&pdfSKIP}.

    IF plCompress AND pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
        iRetCode = CompressBuffer(pmPtr, INPUT-OUTPUT mPtrZ, OUTPUT iLength).
        IF iRetCode = 0 THEN DO:
            PUT STREAM S_pdf_inc UNFORMATTED
                "/Filter " (IF pcExtraFilter > ""
                            THEN "[/FlateDecode " + pcExtraFilter + "]"
                            ELSE "/FlateDecode") {&pdfSKIP}.
        END.
        ELSE DO:
            {pdferror.i &msg="'Zlib error: ' + STRING(iRetCode)" &return=NO}. /* continue without compression */
            SET-SIZE(mPtrZ) = 0.
            iRetCode = ?.
        END.
    END.
    ELSE IF pcExtraFilter > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Filter " pcExtraFilter {&pdfSKIP}.

    IF iRetCode = ? THEN
        iLength = GET-SIZE(pmPtr).

    /* 13-OCT-2015 jcc: get the encrypted length when encrypting AES (may differ from original length) */
    IF pdfEncrypt AND pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "AES" THEN DO:
        RUN EncryptMemPtr (pdfStream,
                           TT_pdf_stream.obj_UniqueID,
                           piObj,
                           pmPtr,
                           m_EncryptKey,
                           OUTPUT m_Encrypted).
        /* change the /Length value */
        iLength = GET-SIZE(m_Encrypted).
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Length " iLength {&pdfSKIP}
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP}.

    IF GET-SIZE(m_Encrypted) > 0 THEN DO: /* 13-OCT-2015 jcc: reuse the encrypted result, when available */
        EXPORT STREAM S_PDF_inc m_Encrypted.
        SET-SIZE(m_Encrypted) = 0.
    END.
    ELSE
        RUN OutputMemPtr(pdfStream,
                         FALSE,
                         TT_pdf_stream.obj_UniqueID, 
                         piObj, 
                         IF iRetCode = ? THEN pmPtr ELSE mPtrZ,
                         m_EncryptKey).
    SET-SIZE(m_EncryptKey) = 0.
    SET-SIZE(mPtrZ)        = 0.

    PUT STREAM S_pdf_inc UNFORMATTED
        {&PDFSKIP} "endstream" {&PDFSKIP}
        "endobj" {&pdfSKIP}.
END PROCEDURE. /* putMemPtrAsStream */

/* 06-FEB-2014 jcc: export a string to a stream object, compressing & encrypting the contents when applicable */
PROCEDURE putStringAsStream: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcString       AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piObj          AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcExtraContent AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcExtraFilter  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfEncrypt     AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: added */
    DEFINE INPUT PARAMETER plCreateNewObj AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: output /Length, close an existing dict */
    DEFINE INPUT PARAMETER plCompress     AS LOGICAL     NO-UNDO. /* 14-OCT-2015 jcc: so that e.g. we do not zip jpeg images */

    DEFINE VARIABLE iLength      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE mContent AS MEMPTR      NO-UNDO.

    SET-SIZE(mContent) = 0.
    iLength = LENGTH(pcString, "RAW").
    SET-SIZE(mContent) = iLength.
    PUT-STRING(mContent, 1, iLength) = pcString.

    RUN putMemptrAsStream (pdfStream
                          ,mContent
                          ,piObj
                          ,pcExtraContent
                          ,pcExtraFilter
                          ,pdfEncrypt
                          ,plCreateNewObj
                          ,plCompress).

    SET-SIZE(mContent) = 0.

    /* 16-OCT-2015 jcc: refactor using putMemptrAsStream
    DEFINE VARIABLE iLength      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRetCode     AS INTEGER   INITIAL ? NO-UNDO.
    DEFINE VARIABLE mContent     AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE mContentZ    AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE m_Encrypted  AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE m_EncryptKey AS MEMPTR    NO-UNDO.

    IF plCreateNewObj THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            piObj " 0 obj" {&pdfSKIP}
            "<<" {&pdfSKIP}.

    IF pcExtraContent > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            pcExtraContent {&pdfSKIP}.

    iLength = LENGTH(pcString, "RAW").
    SET-SIZE(mContent) = iLength.
    PUT-STRING(mContent, 1, iLength) = pcString.

    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
        iRetCode = CompressBuffer(mContent, INPUT-OUTPUT mContentZ, OUTPUT iLength).
        IF iRetCode = 0 THEN DO:
            PUT STREAM S_pdf_inc UNFORMATTED
                "/Filter " (IF pcExtraFilter > ""
                            THEN "[/FlateDecode " + pcExtraFilter + "]"
                            ELSE "/FlateDecode") {&pdfSKIP}.
        END.
        ELSE DO:
            {pdferror.i &msg="'Zlib error: ' + STRING(iRetCode)" &return=NO}. /* continue without compression */
            iRetCode = ?.
        END.
    END.
    ELSE IF pcExtraFilter > "" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Filter " pcExtraFilter {&pdfSKIP}.

    /* 13-OCT-2015 jcc: get the encrypted length when encrypting AES (may differ from original length) */
    IF pdfEncrypt AND pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "AES" THEN DO:
        RUN EncryptMemPtr (pdfStream,
                           TT_pdf_stream.obj_UniqueID,
                           piObj,
                           mContent,
                           m_EncryptKey,
                           OUTPUT m_Encrypted).
        SET-SIZE(mContent) = 0.
        /* change the /Length value */
        iLength = GET-SIZE(m_Encrypted).
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Length " iLength {&pdfSKIP}
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP}.

    IF GET-SIZE(m_Encrypted) > 0 THEN DO: /* 13-OCT-2015 jcc: reuse the encrypted result, when available */
        EXPORT STREAM S_PDF_inc m_Encrypted.
        SET-SIZE(m_Encrypted) = 0.
    END.
    ELSE
        RUN OutputMemPtr(pdfStream,
                         FALSE,
                         TT_pdf_stream.obj_UniqueID, 
                         piObj, 
                         IF iRetCode = ? THEN mContent ELSE mContentZ,
                         m_EncryptKey).
    SET-SIZE(mContent) = 0.
    SET-SIZE(mContentZ) = 0.
    SET-SIZE(m_EncryptKey) = 0.

    PUT STREAM S_pdf_inc UNFORMATTED
        "~n" "endstream" {&PDFSKIP}
        "endobj" {&pdfSKIP}.
    */
END PROCEDURE. /* putStringAsStream */

PROCEDURE recursivelyExportArray: /* PRIVATE */
    DEFINE INPUT        PARAMETER pdfStream         AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER piArrayId         AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER pcDesc            AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pcObjectsToExport AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER plEncrypt         AS LOGICAL     NO-UNDO.
    DEFINE INPUT        PARAMETER piObj             AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER pmEncryptKey      AS MEMPTR      NO-UNDO.

    DEFINE VARIABLE lFirst AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bTT_Object FOR TT_Object.
    DEFINE BUFFER TT_array FOR TT_array.

    PUT STREAM S_pdf_inc UNFORMATTED "[".
    lFirst = YES.
    FOR EACH TT_array WHERE TT_array.array_id = piArrayId
        BY TT_array.value_id:
        CASE TT_array.value_type:
            WHEN "ARRAY" THEN DO:
                IF TT_array.value_array_id = -1 THEN DO:
                    PUT STREAM S_pdf_inc UNFORMATTED "[]".
                END.
                ELSE IF TT_array.value_indirect[1] = 0 THEN DO:
                    RUN recursivelyExportArray(pdfStream, TT_array.value_array_id, pcDesc, INPUT-OUTPUT pcObjectsToExport, plEncrypt, piObj, pmEncryptKey).
                    lFirst = YES.
                END.
                ELSE DO:
                    FIND FIRST bTT_Object WHERE bTT_Object.obj_stream = pdfStream /* 01-OCT-2015 jcc: add FIRST: only export the same array once in case of merged files */
                                      AND bTT_Object.obj_array_id = TT_array.value_array_id.
                    PUT STREAM S_pdf_inc UNFORMATTED
                        (IF lFirst THEN "" ELSE " ") bTT_Object.new_obj " 0 R".
                    IF bTT_Object.is_dirty THEN
                        pcObjectsToExport = pcObjectsToExport + "," + STRING(ROWID(bTT_Object)).
                    lFirst = NO.
                END.
            END.
            WHEN "DICT" THEN DO:
                IF TT_array.value_dict_id = -1 THEN DO:
                    PUT STREAM S_pdf_inc UNFORMATTED " << >>".
                END.
                ELSE IF TT_array.value_indirect[1] = 0 THEN DO:
                    RUN recursivelyExportDict(pdfStream, TT_array.value_dict_id, pcDesc, INPUT-OUTPUT pcObjectsToExport, plEncrypt, piObj, pmEncryptKey).
                    lFirst = YES.
                END.
                ELSE DO:
                    FIND FIRST bTT_Object WHERE bTT_Object.obj_stream = pdfStream /* 01-OCT-2015 jcc: add FIRST: only export the same dictionnary once in case of merged files */
                                      AND bTT_Object.obj_dict_id = TT_array.value_dict_id.
                    PUT STREAM S_pdf_inc UNFORMATTED
                        (IF lFirst THEN "" ELSE " ") bTT_Object.new_obj " 0 R".
                    IF bTT_Object.is_dirty THEN
                        pcObjectsToExport = pcObjectsToExport + "," + STRING(ROWID(bTT_Object)).
                    lFirst = NO.
                END.
            END.
            WHEN "SCALAR" THEN DO:
                RUN exportScalar(pdfStream, IF lFirst OR CAN-DO("~/,(,<", SUBSTRING(TT_array.value_scalar,1,1)) THEN "" ELSE " ", TT_array.value_scalar, plEncrypt, piObj, pmEncryptKey).
                lFirst = NO.
            END.
        END CASE.
    END.
    PUT STREAM S_pdf_inc UNFORMATTED "]".
END PROCEDURE. /* recursivelyExportArray */

PROCEDURE recursivelyExportDict: /* PRIVATE */
    DEFINE INPUT        PARAMETER pdfStream         AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER piDictId          AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER pcDesc            AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pcObjectsToExport AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER plEncrypt         AS LOGICAL     NO-UNDO.
    DEFINE INPUT        PARAMETER piObj             AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER pmEncryptKey      AS MEMPTR      NO-UNDO.

    DEFINE BUFFER bTT_Object FOR TT_Object.
    DEFINE BUFFER TT_dict FOR TT_dict.

    PUT STREAM S_pdf_inc UNFORMATTED "<<".
    /* FIND TT_dict WHERE TT_dict.dict_id = piDictId AND TT_dict.dict_key = "~/Type" NO-ERROR. */
    /* IF AVAILABLE TT_dict THEN DO: */
        /* pcDesc = TT_dict.value_scalar. */
        /* PUT STREAM S_pdf_inc UNFORMATTED TT_dict.dict_key TT_dict.value_scalar {&pdfSKIP}. */
    /* END. */
    /* FIND TT_dict WHERE TT_dict.dict_id = piDictId AND TT_dict.dict_key = "~/Subtype" NO-ERROR. */
    /* IF AVAILABLE TT_dict THEN */
        /* PUT STREAM S_pdf_inc UNFORMATTED TT_dict.dict_key TT_dict.value_scalar {&pdfSKIP}. */
    FOR EACH TT_dict WHERE TT_dict.dict_id = piDictId:
        /* AND NOT CAN-DO("~/Type,~/Subtype", TT_dict.dict_key): */
        PUT STREAM S_pdf_inc UNFORMATTED TT_dict.dict_key.
        CASE TT_dict.value_type:
            WHEN "ARRAY" THEN DO:
                IF TT_dict.value_array_id = -1 THEN DO:
                    PUT STREAM S_pdf_inc UNFORMATTED "[]".
                END.
                ELSE IF TT_dict.value_indirect[1] = 0 THEN DO:
                    RUN recursivelyExportArray(pdfStream, TT_dict.value_array_id, pcDesc, INPUT-OUTPUT pcObjectsToExport, plEncrypt, piObj, pmEncryptKey).
                END.
                ELSE DO:
                    FIND FIRST bTT_Object WHERE bTT_Object.obj_stream = pdfStream /* 01-OCT-2015 jcc: add FIRST: only export the same array once in case of merged files */
                                      AND bTT_Object.obj_array_id = TT_dict.value_array_id.
                    PUT STREAM S_pdf_inc UNFORMATTED
                        " " bTT_Object.new_obj " 0 R".
                    IF bTT_Object.is_dirty THEN
                        pcObjectsToExport = pcObjectsToExport + "," + STRING(ROWID(bTT_Object)).
                END.
            END.
            WHEN "DICT" THEN DO:
                IF TT_dict.value_dict_id = -1 THEN DO:
                    PUT STREAM S_pdf_inc UNFORMATTED "<<>>".
                END.
                ELSE IF TT_dict.value_indirect[1] = 0 THEN DO:
                    RUN recursivelyExportDict(pdfStream, TT_dict.value_dict_id, pcDesc, INPUT-OUTPUT pcObjectsToExport, plEncrypt, piObj, pmEncryptKey).
                END.
                ELSE DO:
                    FIND FIRST bTT_Object WHERE bTT_Object.obj_stream = pdfStream /* 01-OCT-2015 jcc: add FIRST: only export the same dictionnary once in case of merged files */
                                      AND bTT_Object.obj_dict_id = TT_dict.value_dict_id.
                    PUT STREAM S_pdf_inc UNFORMATTED
                        " " bTT_Object.new_obj " 0 R".
                    IF bTT_Object.is_dirty THEN
                        pcObjectsToExport = pcObjectsToExport + "," + STRING(ROWID(bTT_Object)).
                END.
            END.
            WHEN "SCALAR" THEN
                RUN exportScalar(pdfStream, IF CAN-DO("~/,(,<", SUBSTRING(TT_dict.value_scalar,1,1)) THEN "" ELSE " ", TT_dict.value_scalar, plEncrypt, piObj, pmEncryptKey).
        END CASE.
    END.
    PUT STREAM S_pdf_inc UNFORMATTED ">>".
END PROCEDURE. /* recursivelyExportDict */

PROCEDURE recursivelyExportObject: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE PARAMETER BUFFER TT_Object FOR TT_Object.
    DEFINE INPUT PARAMETER pcDesc    AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plEncrypt AS LOGICAL     NO-UNDO.

    DEFINE VARIABLE cFileName        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cObjectsToExport AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cValue           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iFileSize        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iLength          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE mEncrypted       AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE mEncryptKey      AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE mStream          AS MEMPTR      NO-UNDO.

    DEFINE BUFFER bTT_Object FOR TT_Object.
    DEFINE BUFFER TT_dict    FOR TT_dict.

    /* do not do anything if the object has already been exported */
    IF NOT TT_Object.is_dirty THEN RETURN.

    /* mEncryptKey = ?. /* initialize the key so that if needed it will be computed for this object */ */
    SET-SIZE(mEncryptKey) = 0. /* 10-MAY-2016 jcc: mEncryptKey = ? leads to subsequent fail starting with OE 11.6 */

    FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK.

    IF TT_Object.obj_value_type = "DICT" OR TT_Object.obj_value_type = "ARRAY" THEN DO:
        /* IF TT_Object.obj_id <> 0 AND TT_Object.new_obj <= 0 THEN */
        RUN recursivelyAssignNewObjs(pdfStream, BUFFER tt_object, TT_Object.obj_value_type, IF TT_Object.obj_value_type = "ARRAY" THEN TT_Object.obj_array_id ELSE TT_Object.obj_dict_id, pcDesc, "", "").
        /* we need to update the offset, as ObjectSequence has been called when we were at the "wrong" place in the file */
        setObjectOffset(pdfStream, TT_Object.new_obj, SEEK(S_pdf_inc) + 1).
    END.

    /* 14-OCT-2013 jcc: get the encrypted length (may differ from original length for AES) */
    /* This must be done before exporting the object's dictionary */
    IF plEncrypt AND TT_Object.stream_ptr > 0 AND pdf_get_parameter(pdfStream, "EncryptAlgorithm") = "AES" THEN DO:
        {getDict TT_dict tt_object.obj_dict_id "SCALAR" "~/Length"}.
        IF INTEGER(TT_dict.value_scalar) > 0 THEN DO: /* 28-AUG-2012 jcc: bug fix */
            RUN getFileAsMemptr (pdfStream,
                                 /* TT_Object.cache_dir + (IF TT_Object.pdf_id_orig = "" THEN TT_Object.pdf_id ELSE TT_Object.pdf_id_orig) + "-" + STRING(TT_Object.obj_dict_id) + ".stm", */
                                 TT_Object.cache_dir + (IF TT_Object.pdf_id_orig = "" THEN TT_Object.pdf_id ELSE TT_Object.pdf_id_orig) + "-" + STRING(TT_Object.obj_id) + ".stm",
                                 INTEGER(TT_dict.value_scalar),
                                 INPUT-OUTPUT mStream, OUTPUT iFileSize) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN.

            RUN EncryptMemPtr (pdfStream,
                               TT_pdf_stream.obj_UniqueID,
                               TT_Object.new_obj,
                               mStream,
                               mEncryptKey,
                               OUTPUT mEncrypted).
            SET-SIZE(mStream) = 0.
            /* change the /Length value */
            TT_dict.value_scalar = STRING(GET-SIZE(mEncrypted)).
        END.
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_Object.new_obj " 0 obj" /* {&pdfSKIP} */.

    CASE TT_Object.obj_value_type:
        WHEN "ARRAY" THEN
            RUN recursivelyExportArray(pdfStream, TT_Object.obj_array_id, pcDesc, INPUT-OUTPUT cObjectsToExport, plEncrypt, TT_Object.new_obj, mEncryptKey).
        WHEN "DICT" THEN
            RUN recursivelyExportDict(pdfStream, TT_Object.obj_dict_id, pcDesc, INPUT-OUTPUT cObjectsToExport, plEncrypt, TT_Object.new_obj, mEncryptKey).
        WHEN "SCALAR" THEN
           RUN exportScalar(pdfStream, "", TT_Object.obj_scalar, plEncrypt, TT_Object.new_obj, mEncryptKey).
    END CASE.

    IF TT_Object.stream_ptr > 0 THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED 
            /* {&pdfSKIP} */ "stream" {&pdfSKIP}.
        {getDict TT_dict tt_object.obj_dict_id "SCALAR" "~/Length"}.
        IF INTEGER(TT_dict.value_scalar) > 0 THEN DO: /* 28-AUG-2012 jcc: bug fix */
            IF GET-SIZE(mEncrypted) > 0 THEN DO: /* 14-OCT-2013 jcc: reuse the encrypted result, when available */
                EXPORT STREAM S_PDF_inc mEncrypted.
                SET-SIZE(mEncrypted) = 0.
            END.
            ELSE DO:
                RUN getFileAsMemptr (pdfStream,
                                     /* TT_Object.cache_dir + (IF TT_Object.pdf_id_orig = "" THEN TT_Object.pdf_id ELSE TT_Object.pdf_id_orig) + "-" + STRING(TT_Object.obj_dict_id) + ".stm", */
                                     TT_Object.cache_dir + (IF TT_Object.pdf_id_orig = "" THEN TT_Object.pdf_id ELSE TT_Object.pdf_id_orig) + "-" + STRING(TT_Object.obj_id) + ".stm",
                                     INTEGER(TT_dict.value_scalar),
                                     INPUT-OUTPUT mStream, OUTPUT iFileSize) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN.
                /* 07-OCT-2013 jcc: use OutputMemPtr to enable encryption */
                RUN OutputMemPtr(pdfStream, FALSE, TT_pdf_stream.obj_UniqueID, TT_Object.new_obj, mStream, mEncryptKey).
                SET-SIZE(mStream) = 0.
            END.
        END.
        PUT STREAM S_pdf_inc UNFORMATTED
            "~n" "endstream".
    END.
    PUT STREAM S_pdf_inc UNFORMATTED
        {&pdfSKIP} "endobj" {&pdfSKIP}.

    /* Mark the object as exported */
    TT_Object.is_dirty = NO.

    SET-SIZE(mEncryptKey) = 0.

    /* Now export all the dependant objects */
    IF cObjectsToExport > "" THEN DO:
        DO i = 2 TO NUM-ENTRIES(cObjectsToExport):
            FIND bTT_Object WHERE ROWID(bTT_Object) = TO-ROWID(ENTRY(i,cObjectsToExport)).
            RUN recursivelyExportObject(pdfStream, BUFFER bTT_Object, pcDesc, plEncrypt).
        END.
    END.
END PROCEDURE. /* recursivelyExportObject */

/* End of the procedures used to export back external pdf files */

PROCEDURE LoadExternalFonts: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE iObj         AS INTEGER NO-UNDO.

  DEFINE BUFFER TT_Resource FOR TT_Resource.
  DEFINE BUFFER TT_Object   FOR TT_Object.
  DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

  FOR EACH TT_Resource
     WHERE TT_Resource.obj_Stream = pdfStream
       AND TT_Resource.res_type   = "Font",
  /* 03-DEC-2014 jcc: only export used external pages' resources: */
  FIRST tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
    AND tt_pdf_page_use_ext.pdf_id_use = TT_Resource.pdf_id /* 02-DEC-2014 jcc */
    BREAK BY TT_Resource.pdf_id /* 01-MAR-2011 jcc */
          BY TT_Resource.res_obj
          BY TT_Resource.res_text:

    FIND FIRST TT_Object
         WHERE TT_Object.obj_stream = pdfStream
           AND TT_Object.pdf_id     = TT_Resource.pdf_id
           AND TT_Object.obj_id     = TT_Resource.res_obj
           AND TT_Object.gen_id     = TT_Resource.res_gen.

    IF CAN-FIND(FIRST TT_pdf_font
                WHERE TT_pdf_font.obj_Stream = pdfStream
                  AND TT_pdf_font.pdf_id     = TT_Resource.pdf_id
                  AND TT_pdf_font.font_tag   = TT_resource.res_text
                  AND (   TT_pdf_font.ext_page = TT_Resource.page_id
                       OR TT_pdf_font.ext_page = 0 )
                  AND TT_pdf_font.font_obj   = 0 ) THEN DO:
      RUN recursivelyExportObject(pdfStream, BUFFER TT_Object, "Font", pdfEncrypt).
      iObj = TT_Object.new_obj.

      FOR EACH TT_pdf_Font
        WHERE TT_pdf_Font.obj_Stream = pdfStream
          AND TT_pdf_font.pdf_id     = TT_Resource.pdf_id
          AND TT_pdf_Font.font_tag   = TT_Resource.res_text
          AND (   TT_pdf_font.ext_page = TT_Resource.page_id
               OR TT_pdf_font.ext_page = 0 ):
        TT_pdf_font.font_obj = iObj.
      END.
    END.

    TT_Resource.new_obj = iObj.
  END. /* Each Font Resource */
END. /* LoadExternalFonts */

PROCEDURE LoadResources: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL     NO-UNDO.
  DEFINE INPUT PARAMETER pcResType  AS CHARACTER   NO-UNDO.

  DEFINE BUFFER TT_Resource         FOR TT_Resource.
  DEFINE BUFFER TT_Object           FOR TT_Object.
  DEFINE BUFFER tt_pdf_page_use_ext FOR tt_pdf_page_use_ext.

  FOR EACH TT_Resource
     WHERE TT_Resource.obj_stream = pdfStream
       AND TT_Resource.res_type   = pcResType,
  /* 03-DEC-2014 jcc: only export used external pages' resources: */
  FIRST tt_pdf_page_use_ext WHERE tt_pdf_page_use_ext.obj_stream = pdfStream
    AND tt_pdf_page_use_ext.pdf_id_use = TT_Resource.pdf_id /* 02-DEC-2014 jcc */
    AND (tt_pdf_page_use_ext.page_use = TT_Resource.page_id OR TT_Resource.page_id = 0) /* 05-MAY-2015 jcc: only used resources */
     BY TT_Resource.res_text:
    
      FIND FIRST TT_Object
           WHERE TT_Object.obj_stream = pdfStream
             AND TT_Object.pdf_id = TT_Resource.pdf_id /* 13-APR-2012 jcc: was missing */
             AND TT_Object.obj_id = TT_Resource.res_obj
             AND TT_Object.gen_id = TT_Resource.res_gen.

      RUN recursivelyExportObject(pdfStream, BUFFER TT_Object, pcResType, pdfEncrypt).

      TT_Resource.new_obj = TT_Object.new_obj.

  END. /* Each Resource */
END. /* LoadResources */

/* Added functionality from Robert Ayris [rayris@comops.com.au] */

FUNCTION GetPDFColor returns dec
        (input vp_Color      as char) :  /* PRIVATE */

   DEFINE VARIABLE vl_Num AS INTEGER   NO-UNDO.
   
   if substring(vp_Color, 1, 2) = "0x":U then
      vl_Num = Hex2Int(substring(vp_Color, 3)).
   else
      vl_Num = int(vp_Color).
      
   if vl_Num GT 255 then return 1.0.      
      
   return round(vl_Num / 255, 4).
         
END FUNCTION.         

PROCEDURE pdf_rgb :
   def input param vp_Stream              as char no-undo.
   def input param vp_Function            as char no-undo.
   def input param vp_Color               as char no-undo.

   DEFINE VARIABLE vl_rgb AS DECIMAL   NO-UNDO EXTENT 3.

   &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(vp_Stream) + "," + QUOTER(vp_Function) + "," + QUOTER(vp_Color)). &ENDIF

   /* 24-OCT-2011 jcc: move this test at the beginning and issue an error if called with an unknown function */
   if NOT can-do("pdf_text_color,pdf_stroke_color,pdf_stroke_fill", vp_Function) then do:
      RUN pdf_error(vp_Stream,"pdf_rgb","Invalid function. Possible values are pdf_text_color,pdf_stroke_color,pdf_stroke_fill").
      RETURN.
   END.

   if num-entries(vp_color) = 3 then
     assign vl_rgb[1] = GetPDFColor(ENTRY(1,vp_color))              /* RED */
            vl_rgb[2] = GetPDFColor(ENTRY(2,vp_color))              /* GREEN */
            vl_rgb[3] = GetPDFColor(ENTRY(3,vp_color))              /* BLUE */
            .
   else if substring(vp_Color, 1, 2) = "0x":U and length(vp_Color) = 8 then  /* Hex Value 0xRRGGBB */
      assign vl_rgb[1] = GetPDFColor("0x":U + substring(vp_color, 3, 2))     /* RED */
             vl_rgb[2] = GetPDFColor("0x":U + substring(vp_color, 5, 2))     /* GREEN */
             vl_rgb[3] = GetPDFColor("0x":U + substring(vp_color, 7, 2))     /* BLUE */
             .
   else if substring(vp_color, 1, 1) = "#":U and length(vp_color) = 7 then   /* Hex Value #RRGGBB */
      assign vl_rgb[1] = GetPDFColor("0x":U + substring(vp_color, 2, 2))     /* RED */
             vl_rgb[2] = GetPDFColor("0x":U + substring(vp_color, 4, 2))     /* GREEN */
             vl_rgb[3] = GetPDFColor("0x":U + substring(vp_color, 6, 2))     /* BLUE */
             .
   else if length(vp_color) = 9 then                                         /* Dec Value RRRGGGBBB */
      assign vl_rgb[1] = GetPDFColor(substring(vp_color, 1, 3))              /* RED */
             vl_rgb[2] = GetPDFColor(substring(vp_color, 4, 3))              /* GREEN */
             vl_rgb[3] = GetPDFColor(substring(vp_color, 7, 3))              /* BLUE */
             .

   run value(vp_Function)
      (vp_Stream, vl_rgb[1], vl_rgb[2], vl_rgb[3]).

END PROCEDURE. /* pdf_rgb */

PROCEDURE pdf_exec_footer:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.
  
  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream
       NO-ERROR.

  /* Print the footer */
  IF TT_pdf_stream.obj_Footer <> "" AND pdf_Page(pdfStream) >= 1 THEN
      RUN _doFooter(pdfStream, TT_pdf_stream.obj_CallProcFooter, TT_pdf_stream.obj_Footer).

END PROCEDURE. /* pdf_exec_footer */

PROCEDURE pdf_load_xml :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfXMLFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE hDoc    AS HANDLE NO-UNDO.
  DEFINE VARIABLE hRoot   AS HANDLE NO-UNDO.
  DEFINE VARIABLE hNode   AS HANDLE NO-UNDO.

  DEFINE VARIABLE Good    AS LOGICAL NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfXMLFile)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

  CREATE X-DOCUMENT hDoc.
  CREATE X-NODEREF  hRoot.
  CREATE X-NODEREF  hNode.

  Good = hDoc:LOAD("FILE",pdfXMLFile,FALSE) NO-ERROR.
  Good = hdoc:GET-DOCUMENT-ELEMENT(hroot) NO-ERROR.

  IF NOT Good THEN STOP.

  RUN LoadXMLNode(pdfstream, hRoot,"~/" + hRoot:NAME,0).

  DELETE OBJECT hRoot.
  DELETE OBJECT hDoc.
  DELETE OBJECT hNode.

END. /* pdf_load_xml */

PROCEDURE LoadXMLNode: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pNode       AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER pNodeName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pParentNode AS INTEGER NO-UNDO.

  DEFINE VARIABLE X-Child AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE Good    AS LOGICAL NO-UNDO.

  DEFINE VARIABLE hNode        AS HANDLE NO-UNDO.
  DEFINE VARIABLE hChildValue  AS HANDLE NO-UNDO.

  DEFINE BUFFER B_TT_pdf_xml FOR TT_pdf_xml.

  CREATE X-NODEREF  hNode.
  CREATE X-NODEREF  hChildValue.

  DO X-Child = 1 TO pNode:NUM-CHILDREN:

    Good = pNode:GET-CHILD(hNode,x-child) NO-ERROR.

    IF NOT Good THEN NEXT.

    IF INDEX(hNode:NAME,"#text") > 0 THEN NEXT.

    Good = hNode:GET-CHILD(hChildValue,1) NO-ERROR.
    IF NOT Good THEN NEXT.

    CREATE B_TT_pdf_xml.
    ASSIGN B_TT_pdf_xml.obj_stream = pdfStream
           B_TT_pdf_xml.xml_parent = pNodeName
           B_TT_pdf_xml.xml_pnode  = pParentNode
           B_TT_pdf_xml.xml_node   = hNode:NAME
           B_TT_pdf_xml.xml_value  = hChildValue:NODE-VALUE
           B_TT_pdf_xml.xml_seq    = xml-seq + 1
           xml-seq                 = xml-seq + 1.

    /* remove CHR(10) from value -- don't know why it's setting this */
    IF B_TT_pdf_xml.xml_value = CHR(10) THEN
      B_TT_pdf_xml.xml_value = "".

    RUN LoadXMLNode(pdfStream, hNode,pNodeName + "~/" + hNode:Name,xml-seq).

  END.

  DELETE OBJECT hNode.
  DELETE OBJECT hChildValue.
END. /* LoadXMLNode */

FUNCTION GetXMLNodeValue RETURNS CHARACTER 
  (INPUT pParent  AS CHARACTER,
   INPUT pNode    AS CHARACTER ):
  
  DEFINE BUFFER B_TT_pdf_xml FOR TT_pdf_xml.

  FIND FIRST B_TT_pdf_xml 
       WHERE B_TT_pdf_xml.xml_parent = pParent
         AND B_TT_pdf_xml.xml_node   = pNode 
         NO-LOCK NO-ERROR.
  IF AVAIL B_TT_pdf_xml THEN
    RETURN B_TT_pdf_xml.xml_value.
  ELSE
    RETURN "".
END. /* GetXMLNodeValue */

/* 2-AUG-2012 jcc: implementation of transactions: begin, rollback & commit */
&SCOPED-DEFINE xcTransactionParamBackup "Font,PointSize,TextX,TextY,GraphicX,GraphicY,Angle,TextRed,TextGreen,TextBlue,FillRed,FillGreen,FillBlue,PageWidth,PageHeight,PageRotate,Orientation,LeftMargin,TopMargin,BottomMargin,Papertype"

/* 03-MAY-2014 jcc: save & restore stream parameters */
PROCEDURE _pdf_save_stream_parameters: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.

    DO i = NUM-ENTRIES({&xcTransactionParamBackup}) TO 1 BY -1:
        RUN _pdf_set_parameter_priv(pdfStream,
                                   "___bkp-" + ENTRY(i, {&xcTransactionParamBackup}),
                                    DYNAMIC-FUNCTION("pdf_" + ENTRY(i, {&xcTransactionParamBackup}), pdfStream)).
    END.
END PROCEDURE.
PROCEDURE _pdf_restore_stream_parameters: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cFont      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cParam     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dPointSize AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER     NO-UNDO.

    DO i = NUM-ENTRIES({&xcTransactionParamBackup}) TO 1 BY -1:
        cParam = ENTRY(i, {&xcTransactionParamBackup}).
        CASE cParam:
            WHEN "Font"      THEN cFont      = pdf_get_parameter(pdfStream, "___bkp-" + cParam).
            WHEN "PointSize" THEN dPointSize = DECIMAL(pdf_get_parameter(pdfStream, "___bkp-" + cParam)).
            OTHERWISE
                RUN VALUE("pdf_set_" + cParam) (pdfStream, pdf_get_parameter(pdfStream, "___bkp-" + cParam)).
        END CASE.
    END.
    RUN pdf_set_font (pdfStream, cFont, dPointSize).
END PROCEDURE.

/* start a transaction */
PROCEDURE pdf_transaction_begin:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_stream  FOR TT_pdf_stream.
    DEFINE BUFFER bTT_pdf_stream FOR TT_pdf_stream. /* will hold a backup of tt_pdf_stream */

    DEFINE VARIABLE i AS INTEGER   NO-UNDO.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

    FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAIL TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    IF pdf_get_parameter2(pdfStream, "___inTransaction", "0") <> "0" THEN
        {pdferror.i &msg="'Attempt to begin a transaction while there is already one!'" &return=YES}.

    /* itransnum = itransnum + 1. */
    /* put stream s_pdf_out UNFORMATTED "% before transaction " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G") {&pdfSKIP}. */
    OUTPUT STREAM S_pdf_out CLOSE.

    /* The inTransaction parameter contains the list of pages where the transaction has been active.
       Initialize it with the curent page number. */
    RUN _pdf_set_parameter_priv(pdfStream, "___inTransaction", pdf_Page(pdfStream)).

    /* transactions are hold in .transac.txt files; see _get_page_content_file */
    OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, pdf_Page(pdfStream))) BINARY NO-MAP NO-CONVERT NO-ECHO.
    /* put stream s_pdf_out UNFORMATTED "% transaction start " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */

    /* backup tt_pdf_stream */
    CREATE bTT_pdf_stream.
    BUFFER-COPY TT_pdf_stream TO bTT_pdf_stream 
        ASSIGN bTT_pdf_stream.obj_stream = pdfStream + "-transac".

    /* backup some parameters in case of rollback */
    RUN _pdf_save_stream_parameters (pdfStream).
END.

/* cancel a transaction */
PROCEDURE pdf_transaction_rollback:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cFile             AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTransacPages     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i                 AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iPage             AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTransacStartPage AS INTEGER     NO-UNDO.

    DEFINE BUFFER TT_pdf_stream  FOR TT_pdf_stream.
    DEFINE BUFFER bTT_pdf_stream FOR TT_pdf_stream.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

    FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAIL TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    cTransacPages = pdf_get_parameter2(pdfStream, "___inTransaction", "0").
    iTransacStartPage = INTEGER(ENTRY(1, cTransacPages)).
    IF iTransacStartPage = 0 THEN
        {pdferror.i &msg="'Attempt to rollback a transaction while none as been started!'" &return=YES}.

    cFile = SESSION:TEMP-DIR
          + TT_pdf_stream.obj_UniqueID
          + "-Content-".

    /* Tell that no transaction is active anymore. Must be done before pdf_set_page. */
    RUN _pdf_set_parameter_priv(pdfStream, "___bufferedTransaction", "0").
    RUN _pdf_set_parameter_priv(pdfStream, "___inTransaction", "0").
    
    /* pdf_set_page will redirect correctly the output to the S_pdf_out stream to the page where the transaction started. */
    /* This must be done before the OS-DELETE below, else the file will still be opened, thus impossible to delete. */
    RUN pdf_set_page(pdfStream, iTransacStartPage).
    /* put stream s_pdf_out UNFORMATTED "% transaction rollback " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */

    /* For each page where the transaction has been active: */
    DO i = NUM-ENTRIES(cTransacPages) TO 1 BY -1:
        iPage = INTEGER(ENTRY(i, cTransacPages)).
        OS-DELETE VALUE(cFile + STRING(iPage) + ".transac.txt") NO-ERROR.
        /* if all the page was in the transaction then completely delete it */
        IF iPage <> iTransacStartPage AND SEARCH(cFile + STRING(iPage) + ".txt") = ? THEN DO:
            FIND TT_pdf_page WHERE TT_pdf_page.obj_stream  = pdfStream
                               AND TT_pdf_page.page_nbr    = iPage.
            DELETE TT_pdf_page.
            /* Delete all records related to the page we just removed */
            FOR EACH tt_pdf_object WHERE tt_pdf_object.obj_stream = pdfStream AND tt_pdf_object.obj_page = iPage:
                DELETE tt_pdf_object.
            END.
            FOR EACH tt_pdf_bookmark WHERE tt_pdf_bookmark.obj_stream = pdfStream AND tt_pdf_bookmark.book_page = iPage:
                DELETE tt_pdf_bookmark.
            END.
            FOR EACH tt_pdf_annot WHERE tt_pdf_annot.obj_stream = pdfStream AND tt_pdf_annot.annot_page = iPage:
                DELETE tt_pdf_annot.
            END.
            FOR EACH tt_pdf_FillTxt WHERE tt_pdf_FillTxt.obj_stream = pdfStream AND tt_pdf_FillTxt.page_nbr = iPage:
                DELETE tt_pdf_FillTxt.
            END.
        END.
        /* 30-MAR-2015 jcc: also delete elements created on the transaction start page */
        ELSE DO:
            FOR EACH tt_pdf_object WHERE tt_pdf_object.obj_stream = pdfStream AND tt_pdf_object.obj_page = iPage AND tt_pdf_object.in_transaction = YES:
                DELETE tt_pdf_object.
            END.
            FOR EACH tt_pdf_bookmark WHERE tt_pdf_bookmark.obj_stream = pdfStream AND tt_pdf_bookmark.book_page = iPage AND tt_pdf_bookmark.in_transaction = YES:
                DELETE tt_pdf_bookmark.
            END.
            FOR EACH tt_pdf_annot WHERE tt_pdf_annot.obj_stream = pdfStream AND tt_pdf_annot.annot_page = iPage AND tt_pdf_annot.in_transaction = YES:
                DELETE tt_pdf_annot.
            END.
            FOR EACH tt_pdf_FillTxt WHERE tt_pdf_FillTxt.obj_stream = pdfStream AND tt_pdf_FillTxt.page_nbr = iPage AND tt_pdf_FillTxt.in_transaction = YES:
                DELETE tt_pdf_FillTxt.
            END.
        END.
    END.
    FIND TT_pdf_page WHERE TT_pdf_page.obj_stream  = pdfStream
                       AND TT_pdf_page.page_nbr    = iTransacStartPage.

    /* 31-MAR-2015 jcc: if the footer has been displayed while in the transaction, tell the page does not have (yet) footer */
    IF TT_pdf_page.has_footer_transac THEN ASSIGN
        TT_pdf_page.has_footer         = NO
        TT_pdf_page.has_footer_transac = NO.

    /* Restore saved parameters */
    RUN _pdf_restore_stream_parameters (pdfStream).

    /* Restore tt_pdf_stream */
    FIND bTT_pdf_stream WHERE bTT_pdf_stream.obj_stream = pdfStream + "-transac".
    BUFFER-COPY bTT_pdf_stream EXCEPT obj_stream TO TT_pdf_stream.
    DELETE bTT_pdf_stream.
END.

/* buffer a transaction = remember for later */
PROCEDURE pdf_transaction_buffer:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_stream  FOR TT_pdf_stream.
    DEFINE BUFFER bTT_pdf_stream FOR TT_pdf_stream.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

    FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAIL TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    IF pdf_get_parameter2(pdfStream, "___inTransaction", "0") = "0" THEN
        {pdferror.i &msg="'Attempt to buffer a transaction while none as been started!'" &return=YES}.

    /* put stream s_pdf_out UNFORMATTED "% end of the transaction buffer " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */
    OUTPUT STREAM S_pdf_out CLOSE.

    /* Store the current context (text or graphic) at the end of the buffer */
    /* Note: the context at the beginning of the buffer is in bTT_pdf_stream_obj */
    RUN _pdf_set_parameter_priv(pdfStream, "___bufferedTransaction-streamState", STRING(TT_pdf_stream.obj_DoingText, "T/G")).

    /* restore the context text/graphic to what it was before the transaction */
    FIND bTT_pdf_stream WHERE bTT_pdf_stream.obj_stream = pdfStream + "-transac".
    BUFFER-COPY bTT_pdf_stream USING obj_DoingGraphic obj_DoingText TO TT_pdf_stream.
    /* put stream s_pdf_out UNFORMATTED "% stream will continue after trans " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */

    RUN _pdf_set_parameter_priv(pdfStream, "___bufferedTransaction", pdf_get_parameter(pdfStream, "___inTransaction")).
    RUN _pdf_set_parameter_priv(pdfStream, "___inTransaction", "0").

    OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, pdf_Page(pdfStream))) BINARY NO-MAP NO-CONVERT NO-ECHO APPEND.
    /* put stream s_pdf_out UNFORMATTED "% after transaction buffered " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */
END.

/* write the transaction contents to the pdf */
PROCEDURE pdf_transaction_commit:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cFile             AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLine             AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStreamContext    AS CHARACTER   NO-UNDO. /* set only when buffering, stores the state of the stream (text/graphic) at the end of the buffer */.
    DEFINE VARIABLE cTransacFile      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTransacPages     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i                 AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iPage             AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTransacFileSize  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTransacStartPage AS INTEGER     NO-UNDO.
    DEFINE VARIABLE mFileContents     AS MEMPTR      NO-UNDO.

    DEFINE BUFFER TT_pdf_stream  FOR TT_pdf_stream.
    DEFINE BUFFER bTT_pdf_stream FOR TT_pdf_stream.

    &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream)). &ENDIF

    FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAIL TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    cTransacPages = pdf_get_parameter2(pdfStream, "___inTransaction", "0").
    IF cTransacPages = "0" THEN
        cTransacPages = pdf_get_parameter2(pdfStream, "___bufferedTransaction", "0").
    iTransacStartPage = INTEGER(ENTRY(1,cTransacPages)).
    IF iTransacStartPage = 0 THEN
        {pdferror.i &msg="'Attempt to commit a transaction while none as been started!'" &return=YES}.

    cFile = SESSION:TEMP-DIR
          + TT_pdf_stream.obj_UniqueID
          + "-Content-".

    /* put stream s_pdf_out UNFORMATTED "% start of commit " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */

    FIND bTT_pdf_stream WHERE bTT_pdf_stream.obj_stream = pdfStream + "-transac".
    cStreamContext = pdf_get_parameter(pdfStream, "___bufferedTransaction-streamState").

    /* When we were buffering, if the stream context (text vs graphic) has changed between
       pdf_transaction_begin and now, then close the current context and open the new one
       before writing the buffer */
    /* BUG: should be one a page level, in the loop below */
    IF cStreamContext <> "" /* i.e. we were buffering */ THEN blkBufferCtx: DO:
        /* do not do that when the transaction file is empty! */
        IF NUM-ENTRIES(cTransacPages) = 1 THEN DO:
            FILE-INFO:FILE-NAME = cFile + cTransacPages + ".transac.txt".
            IF FILE-INFO:FILE-SIZE = 0 THEN
                LEAVE blkBufferCtx.
        END.

        /* if the context we had before the buffer is different from the current one... */
        IF bTT_pdf_stream.obj_DoingText AND NOT TT_pdf_stream.obj_DoingText THEN DO:
            PUT STREAM S_pdf_out UNFORMATTED "Q" {&pdfSKIP} "BT" {&pdfSKIP}.
            RUN initTextOperators(pdfStream). /* will be useful if, after writing the buffer, we continue with the same context */
        END.
        ELSE IF bTT_pdf_stream.obj_DoingGraphic AND NOT TT_pdf_stream.obj_DoingGraphic THEN DO:
            PUT STREAM S_pdf_out UNFORMATTED "ET" {&pdfSKIP} "q" {&pdfSKIP}.
            RUN initGfxOperators(pdfStream).
        END.
        /* set the context to what it was at the end of the buffer */
        ASSIGN
         TT_pdf_stream.obj_DoingText    = cStreamContext = "T"
         TT_pdf_stream.obj_DoingGraphic = cStreamContext = "G".
        RUN _pdf_set_parameter_priv(pdfStream, "___bufferedTransaction-streamState", "").
    END. /* blkBufferCtx: IF cStreamContext <> "" */

    /* For each page where the transaction has been active: */
    DO i = NUM-ENTRIES(cTransacPages) TO 1 BY -1:
        iPage = INTEGER(ENTRY(i, cTransacPages)).
        IF SEARCH(cFile + STRING(iPage) + ".txt") = ? THEN DO: /* all of the page contents is in a transaction file -> just rename it */
            OUTPUT STREAM S_pdf_out CLOSE.
            OS-RENAME VALUE(cFile + STRING(iPage) + ".transac.txt") VALUE(cFile + STRING(iPage) + ".txt").
        END.
        ELSE DO: /* part of the page contents is in a transaction file : append it to the page contents file */
            OUTPUT STREAM S_pdf_out CLOSE.

            OUTPUT STREAM S_pdf_out TO VALUE(cFile + STRING(iPage) + ".txt") BINARY NO-MAP NO-CONVERT NO-ECHO APPEND.

            cTransacFile = cFile + STRING(iPage) + ".transac.txt".

            RUN getFileAsMemptr (pdfStream, cTransacFile, ?, INPUT-OUTPUT mFileContents, OUTPUT iTransacFileSize).
            EXPORT STREAM S_pdf_out mFileContents.
            SET-SIZE(mFileContents) = 0.
            OS-DELETE VALUE(cTransacFile).
        END.
    END.
    IF iPage <> pdf_Page(pdfStream) THEN DO:
        OUTPUT STREAM S_pdf_out CLOSE.
        OUTPUT STREAM S_pdf_out TO VALUE(cFile + STRING(pdf_Page(pdfStream)) + ".txt") BINARY NO-MAP NO-CONVERT NO-ECHO APPEND.
    END.
    /* put stream s_pdf_out UNFORMATTED "% after transaction ended " itransnum " " STRING(TT_pdf_stream.obj_DoingText,"T/G")  {&pdfSKIP}. */

    /* 30-MAR-2015 jcc: reset transaction flag on dependant records */
    FOR EACH tt_pdf_object WHERE tt_pdf_object.obj_stream = pdfStream AND tt_pdf_object.obj_page > 0 AND tt_pdf_object.in_transaction = YES:
        tt_pdf_object.in_transaction = NO.
    END.
    FOR EACH tt_pdf_bookmark WHERE tt_pdf_bookmark.obj_stream = pdfStream AND tt_pdf_bookmark.book_page > 0 AND tt_pdf_bookmark.in_transaction = YES:
        tt_pdf_bookmark.in_transaction = NO.
    END.
    FOR EACH tt_pdf_annot WHERE tt_pdf_annot.obj_stream = pdfStream AND tt_pdf_annot.annot_page > 0 AND tt_pdf_annot.in_transaction = YES:
        tt_pdf_annot.in_transaction = NO.
    END.
    FOR EACH tt_pdf_FillTxt WHERE tt_pdf_FillTxt.obj_stream = pdfStream AND tt_pdf_FillTxt.page_nbr > 0 AND tt_pdf_FillTxt.in_transaction = YES:
        tt_pdf_FillTxt.in_transaction = NO.
    END.

    /* cleanup */
    DELETE bTT_pdf_stream.
    RUN _pdf_set_parameter_priv(pdfStream, "___bufferedTransaction", "0").
    RUN _pdf_set_parameter_priv(pdfStream, "___inTransaction", "0").
END.
/* jcc end */

/* 24-APR-2014 jcc: new xobject functionality */
PROCEDURE pdf_pattern_begin:
    DEFINE INPUT  PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER piXobjectId AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER piWidth     AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER piHeight    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pcKey       AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_stream  FOR TT_pdf_stream.
    DEFINE BUFFER bTT_pdf_stream FOR TT_pdf_stream.
    DEFINE BUFFER TT_pdf_xobject FOR TT_pdf_xobject.

    DEFINE VARIABLE i AS INTEGER   NO-UNDO.

    FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAILABLE TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    IF pdf_get_parameter2(pdfStream, "___inTransaction", "0") <> "0" THEN
        {pdferror.i &msg="'Cannot start a pattern within a transaction!'" &return=YES}.
    IF pdf_get_parameter2(pdfStream, "___inXobject", "0") <> "0" THEN
        {pdferror.i &msg="'Cannot start a pattern within another one. Please first end the current pattern.'" &return=YES}.

    piXobjectId = 0.
    FOR EACH TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream
        BY TT_pdf_xobject.xobject_id DESCENDING:
        piXobjectId = TT_pdf_xobject.xobject_id.
        LEAVE.
    END.
    piXobjectId = piXobjectId + 1.
    RUN _pdf_set_parameter_priv(pdfStream, "___inXobject", piXobjectId).

    CREATE TT_pdf_xobject.
    ASSIGN
     TT_pdf_xobject.obj_stream     = pdfStream
     TT_pdf_xobject.xobject_id     = piXobjectId
     TT_pdf_xobject.xobject_tag    = "/Xo" + STRING(piXobjectId)
     TT_pdf_xobject.content_file   = SESSION:TEMP-DIRECTORY
        + TT_pdf_stream.obj_UniqueID
        + "-Xobject-"
        + STRING(piXobjectId) + ".txt"
     TT_pdf_xobject.xobject_height = piHeight
     TT_pdf_xobject.xobject_width  = piWidth
     TT_pdf_xobject.xobject_key    = pcKey.

    /* redirect output to the xobject file */
    RUN _pdf_close_output_context (pdfStream).
    OUTPUT STREAM S_pdf_out CLOSE.
    OUTPUT STREAM S_pdf_out TO VALUE(TT_pdf_xobject.content_file) BINARY NO-MAP NO-CONVERT NO-ECHO.

    /* backup tt_pdf_stream */
    CREATE bTT_pdf_stream.
    BUFFER-COPY TT_pdf_stream TO bTT_pdf_stream 
        ASSIGN bTT_pdf_stream.obj_stream = pdfStream + "-xobject".

    /* backup some parameters */
    RUN _pdf_save_stream_parameters (pdfStream).

    /* Reset all text operators */
    EMPTY TEMP-TABLE tt_state_op.

    /* set some default parameters */
    RUN pdf_set_LeftMargin(pdfStream, 1).
    RUN pdf_set_TopMargin(pdfStream, 1).
    RUN pdf_set_BottomMargin(pdfStream, 1).
    RUN pdf_set_Font(pdfStream, "Courier", 10.0).
    RUN pdf_set_TextX(pdfStream, 0).
    RUN pdf_set_TextY(pdfStream, piHeight - pdf_PointSize(pdfStream)).
    RUN pdf_set_Angle(pdfStream, 0).
    RUN pdf_set_GraphicY(pdfStream, piHeight).
    RUN pdf_set_GraphicX(pdfStream, 0).
    RUN pdf_text_color(pdfStream, 0, 0, 0).
    RUN pdf_stroke_fill(pdfStream, 1, 1, 1).

    RUN pdf_set_PageWidth (pdfStream, piWidth).
    RUN pdf_set_PageHeight (pdfStream, piHeight).

END PROCEDURE. /* pdf_pattern_begin */

PROCEDURE pdf_pattern_end:
    DEFINE INPUT PARAMETER pdfStream   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_pdf_stream FOR TT_pdf_stream.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.

    FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
    IF NOT AVAILABLE TT_pdf_stream THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    RUN _pdf_close_output_context (pdfStream).

    OUTPUT STREAM S_pdf_out CLOSE.

    /* Restore saved parameters */
    RUN _pdf_restore_stream_parameters (pdfStream).

    RUN _pdf_set_parameter_priv(pdfStream, "___inXobject", 0).

    OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, pdf_Page(pdfStream))) BINARY NO-MAP NO-CONVERT NO-ECHO APPEND.

END PROCEDURE. /* pdf_pattern_end */

PROCEDURE pdf_pattern_use:
    DEFINE INPUT PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piXobjectId AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER piColumn    AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER piRow       AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER piWidth     AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER piHeight    AS DECIMAL     NO-UNDO.

    DEFINE BUFFER TT_pdf_xobject FOR TT_pdf_xobject.

    DEFINE VARIABLE dSx AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dSy AS DECIMAL     NO-UNDO.

    IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES}.

    FIND TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream AND TT_pdf_xobject.xobject_id = piXobjectId NO-ERROR.
    IF NOT AVAILABLE TT_pdf_xobject THEN {pdferror.i &msg="'The pattern ' + STRING(piXobjectId) + ' does not exist.'" &return=YES}.

    IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = STRING(piXobjectId) THEN
    {pdferror.i &msg="'Cannot use a pattern within itself (pattern ' + STRING(piXobjectId) + '). Please first end the current pattern.'" &return=YES}.

    /* 04-MAY-2014 jcc: Remember we used the XObject and where */
    IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0" THEN
        TT_pdf_xobject.used_flag = TRUE.
    ELSE
        TT_pdf_xobject.used_by_xobject = TT_pdf_xobject.used_by_xobject + "," + pdf_get_parameter(pdfStream, "___inXobject").

    dSx = IF piWidth = ?  THEN 1 ELSE piWidth  / TT_pdf_xobject.xobject_width.
    dSy = IF piHeight = ? THEN 1 ELSE piHeight / TT_pdf_xobject.xobject_height.

    RUN OutputTextContent(pdfStream, 
                          "IMAGE-EXT",
                          dec2string(dSx) + " 0 0 " + dec2string(dSy) + " " + dec2string(piColumn) + " " + dec2string(/*TT_pdf_xobject.xobject_height - */ piRow) + " cm " +
                          "~/Xo" + STRING(piXobjectId) + " Do",
                          "",
                          "").
END PROCEDURE. /* pdf_pattern_use */

FUNCTION pdf_in_pattern RETURNS LOGICAL (pdfStream AS CHARACTER):
    RETURN pdf_get_parameter2(pdfStream, "___inXobject", "0") <> "0".
END FUNCTION.

PROCEDURE pdf_load_xobjects: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE BUFFER TT_pdf_xobject  FOR TT_pdf_xobject.
  DEFINE BUFFER bTT_pdf_xobject FOR TT_pdf_xobject.
  DEFINE BUFFER TT_pdf_font     FOR TT_pdf_font.
  DEFINE BUFFER TT_pdf_image    FOR TT_pdf_image.
  DEFINE BUFFER TT_Object       FOR TT_Object.

  DEFINE VARIABLE cObject   AS CHARACTER   NO-UNDO.
 
  /* in case an XObject uses another one, first assign all the obj */
  FOR EACH TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream:
      ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Xobject", ?, 0, "").
      TT_pdf_xobject.xobject_obj = pdf_inc_ObjectSequence.
  END.
  /* then output each XObject */
  FOR EACH TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream:
      IF TT_pdf_xobject.UseTotalPages OR TT_pdf_xobject.UsePageNo THEN
        RUN ChangePageText
            (pdfStream,
             TT_pdf_xobject.content_file,
             TT_pdf_xobject.UsePageNo OR TT_pdf_xobject.UseTotalPages,
             (IF TT_pdf_xobject.UseTotalPages THEN "@@TotalPages-" + pdfStream + CHR(2) ELSE "") +
              (IF TT_pdf_xobject.UsePageNo THEN "@@PageNo-" + pdfStream /*+ CHR(2) */ ELSE ""),
             (IF TT_pdf_xobject.UseTotalPages THEN STRING(pdf_Page(pdfStream)) + CHR(2) ELSE "") +
              (IF TT_pdf_xobject.UsePageNo THEN STRING(pdf_Page(pdfStream)) /*+ CHR(2) */ ELSE "")).
      /* as ObjectSequence has been called when we were posibly at the wrong place in thhe file, update the object offset */
      setObjectOffset(pdfStream, TT_pdf_xobject.xobject_obj, SEEK(S_pdf_inc) + 1).
      cObject = "/Type/XObject/Subtype/Form/FormType 1"
          + "/Matrix[1 0 0 1 0 0]"
          + "/BBox[0 0 " + dec2string(TT_pdf_xobject.xobject_width) + " " + dec2string(TT_pdf_xobject.xobject_height) + "]"
          + "/Resources<</ProcSet[/PDF/Text/ImageB/ImageC/ImageI]".
      IF CAN-FIND(FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                  AND LOOKUP(STRING(TT_pdf_xobject.xobject_id), TT_pdf_font.used_by_xobject) > 0) THEN DO:
          cObject = cObject + "/Font<<".
          FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
            AND LOOKUP(STRING(TT_pdf_xobject.xobject_id), TT_pdf_font.used_by_xobject) > 0:
              cObject = cObject + TT_pdf_font.font_tag + " " + STRING(TT_pdf_font.font_obj) + " 0 R".
          END.
          cObject = cObject + ">>".
      END.
      IF  CAN-FIND(FIRST TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream
                   AND LOOKUP(STRING(TT_pdf_xobject.xobject_id), TT_pdf_image.used_by_xobject) > 0)
       OR CAN-FIND(FIRST bTT_pdf_xobject WHERE bTT_pdf_xobject.obj_stream = pdfStream
                   AND LOOKUP(STRING(TT_pdf_xobject.xobject_id), bTT_pdf_xobject.used_by_xobject) > 0) THEN DO:
          cObject = cObject + "/XObject<<".
          FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream
              AND LOOKUP(STRING(TT_pdf_xobject.xobject_id), TT_pdf_image.used_by_xobject) > 0:
              cObject = cObject + TT_pdf_image.image_tag + " " + STRING(TT_pdf_image.image_obj) + " 0 R".
          END.
          FOR EACH bTT_pdf_xobject WHERE bTT_pdf_xobject.obj_stream = pdfStream
              AND LOOKUP(STRING(TT_pdf_xobject.xobject_id), bTT_pdf_xobject.used_by_xobject) > 0:
              cObject = cObject + bTT_pdf_xobject.xobject_tag + " " + STRING(bTT_pdf_xobject.xobject_obj) + " 0 R".
          END.
          cObject = cObject + ">>".
      END.
      cObject = cObject + ">>".
      RUN putFileAsStream (pdfStream, TT_pdf_xobject.content_file, TT_pdf_xobject.xobject_obj, cObject, "", pdfEncrypt, YES, YES).
  END. /* FOR EACH tt_pdf_xobject */
END PROCEDURE. /* pdf_load_xobjects */

&UNDEFINE xcTransactionParamBackup

/* 8-AUG-2012 jcc: new management of text state operators for optimization of the output */
PROCEDURE setTextOperator: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER pdfOperator AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER pdfValue    AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lDirty AS LOGICAL   INITIAL TRUE NO-UNDO.

    DEFINE BUFFER tt_state_op  FOR tt_state_op.
    DEFINE BUFFER btt_state_op FOR tt_state_op.

    IF glNoOptimize THEN DO:
        RUN OutputTextContent(pdfStream, "TEXT", pdfValue + " " + pdfOperator {&debugComment}, "", "").
        RETURN.
    END.

    FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "T"
        AND tt_state_op.operator = pdfOperator NO-ERROR.
    IF NOT AVAILABLE tt_state_op THEN DO:
        CREATE tt_state_op.
        ASSIGN
            tt_state_op.obj_stream = pdfStream
            tt_state_op.type       = "T"
            tt_state_op.operator   = pdfOperator.
    END.
    /* 09-AUG-2013 jcc: also exclude "Tm" from this small optimization so that Td & TD gets deleted */
    IF tt_state_op.opvalue = pdfValue AND tt_state_op.operator <> "Tm" AND tt_state_op.operator <> "TD" THEN
        RETURN.
    CASE pdfOperator:
        WHEN "Tm" THEN DO:
            IF tt_state_op.opvalue = "" THEN
                tt_state_op.opvalue = pdfValue.
            ELSE
                tt_state_op.opvalue = multiplyTransformationMatrices(tt_state_op.opvalue, pdfValue).
            /* If there is a TD or Td then delete it, as we just forced the coordinates */
            FIND btt_state_op WHERE btt_state_op.obj_stream = pdfStream AND btt_state_op.type = "T"
                AND btt_state_op.operator = "TD" NO-ERROR.
            IF AVAILABLE btt_state_op THEN
                DELETE btt_state_op.
            FIND btt_state_op WHERE btt_state_op.obj_stream = pdfStream AND btt_state_op.type = "T"
                AND btt_state_op.operator = "Td" NO-ERROR.
            IF AVAILABLE btt_state_op THEN
                DELETE btt_state_op.
        END.
        WHEN "TD" THEN DO:
            IF tt_state_op.opvalue = "" OR tt_state_op.is_dirty = FALSE THEN
                tt_state_op.opvalue = pdfValue.
            ELSE
                tt_state_op.opvalue = dec2string(string2dec(ENTRY(1,tt_state_op.opvalue," ")) + string2dec(ENTRY(1,pdfValue," "))) + " " + dec2string(string2dec(ENTRY(2,tt_state_op.opvalue," ")) + string2dec(ENTRY(2,pdfValue," "))).
        END.
        OTHERWISE
            tt_state_op.opvalue = pdfValue.
    END CASE.
    tt_state_op.is_dirty = lDirty.
END.

PROCEDURE flushTextOperators: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE BUFFER tt_state_op   FOR tt_state_op.
    DEFINE BUFFER bmtt_state_op FOR tt_state_op.

    IF glNoOptimize THEN RETURN.

    FIND bmtt_state_op WHERE bmtt_state_op.obj_stream = pdfStream
                         AND bmtt_state_op.type       = "T"
                         AND bmtt_state_op.operator   = "Tm"
                         AND bmtt_state_op.is_dirty   = TRUE NO-ERROR.
    IF AVAILABLE bmtt_state_op THEN DO:
        PUT STREAM S_pdf_out UNFORMATTED bmtt_state_op.opvalue + " Tm" {&debugComment} {&pdfskip}.
        bmtt_state_op.is_dirty = FALSE.
    END.
    ELSE
        FIND bmtt_state_op WHERE bmtt_state_op.obj_stream = pdfStream
                             AND bmtt_state_op.type       = "T"
                             AND bmtt_state_op.operator   = "Tm" NO-ERROR.
    FOR EACH tt_state_op WHERE tt_state_op.obj_stream = pdfStream
      AND tt_state_op.type     = "T"
      AND tt_state_op.is_dirty = TRUE:
        PUT STREAM S_pdf_out UNFORMATTED tt_state_op.opvalue + " " + tt_state_op.operator {&debugComment} {&pdfskip}.
        tt_state_op.is_dirty = FALSE.
        /* update the matrix with TD value so that we always know the very last position, but do not set the matrix as dirty as we don't want it to be written again */
        /* IF AVAILABLE bmtt_state_op AND tt_state_op.operator = "TD" THEN DO: */
            /* ENTRY(5, bmtt_state_op.opvalue, " ") = dec2string(string2dec(ENTRY(5, bmtt_state_op.opvalue, " ")) + DECIMAL(ENTRY(1, tt_state_op.opvalue, " "))). */
            /* ENTRY(6, bmtt_state_op.opvalue, " ") = dec2string(string2dec(ENTRY(6, bmtt_state_op.opvalue, " ")) + DECIMAL(ENTRY(2, tt_state_op.opvalue, " "))). */
        /* END. */
    END.
END.

PROCEDURE initTextOperators: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE L_Font AS CHARACTER   NO-UNDO.

    DEFINE BUFFER tt_state_op  FOR tt_state_op.
    DEFINE BUFFER btt_state_op FOR tt_state_op.

    IF glNoOptimize THEN DO:
        PUT STREAM S_pdf_out UNFORMATTED "1 0 0 1 " + STRING(pdf_LeftMargin(pdfStream)) + " " + STRING(pdf_TextY(pdfStream)) + " Tm" {&debugComment} {&pdfSKIP}.
        L_font = pdf_Font(pdfStream).
        FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
           AND TT_pdf_font.font_name  = L_Font NO-LOCK NO-ERROR.
        L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".
        PUT STREAM S_pdf_out UNFORMATTED L_Font + " " + dec2string(pdf_PointSize(pdfStream)) + " Tf" {&debugComment} {&pdfSKIP}.
        RETURN.
    END.

    /* Text Matrix */
    FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "T"
        AND tt_state_op.operator = "Tm" NO-ERROR.
    IF NOT AVAILABLE tt_state_op OR NOT tt_state_op.is_dirty THEN
        RUN setTextOperator(pdfStream, "Tm", "1 0 0 1 " + dec2string(pdf_textX(pdfStream)) + " " + dec2string(pdf_textY(pdfStream))).
    IF AVAILABLE tt_state_op AND NOT tt_state_op.is_dirty THEN
        tt_state_op.is_dirty = YES.

    /* Remove displacements (only if the text matrix is not dirty, else they are still relevant) */
    FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "T"
        AND tt_state_op.operator = "Tm".
    IF AVAILABLE tt_state_op AND NOT tt_state_op.is_dirty THEN DO:
        FIND btt_state_op WHERE btt_state_op.obj_stream = pdfStream AND btt_state_op.type = "T"
            AND btt_state_op.operator = "TD" NO-ERROR.
        IF AVAILABLE btt_state_op THEN
            DELETE btt_state_op.
        FIND btt_state_op WHERE btt_state_op.obj_stream = pdfStream AND btt_state_op.type = "T"
            AND btt_state_op.operator = "Td" NO-ERROR.
        IF AVAILABLE btt_state_op THEN
            DELETE btt_state_op.
    END.

    /* Font */
    FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "T"
        AND tt_state_op.operator = "Tf" NO-ERROR.
    IF NOT AVAILABLE tt_state_op THEN DO:
        L_font = pdf_Font(pdfStream).
        FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
            AND TT_pdf_font.font_name  = L_Font NO-LOCK NO-ERROR.
        L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".
        RUN setTextOperator(pdfStream, "Tf", L_Font + " " + dec2string(pdf_PointSize(pdfStream))).
    END.
    /* ELSE IF NOT tt_state_op.is_dirty THEN */
        /* tt_state_op.is_dirty = TRUE. /* the Tf operator must output on the first BT/ET of one page */ */

    /* Set all the operators as dirty so that they will appear in each TEXT context */
    /* (not needed since they remain valid between different BT...ET blocks - in the same page)
    FOR EACH tt_state_op WHERE tt_state_op.obj_stream = pdfStream:
        tt_state_op.is_dirty = TRUE.
    END.*/
END.

PROCEDURE invalidateAllTextOperators: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    /* RUN flushTextOperators (pdfStream). */
/*     DEFINE BUFFER tt_state_op FOR tt_state_op.                    */
/*                                                                   */
/*     FOR EACH tt_state_op WHERE tt_state_op.obj_stream = pdfStream */
/*       AND tt_state_op.type = "T":                                 */
/*         tt_state_op.is_dirty = NO.                                */
/*     END.                                                          */
END.

PROCEDURE setGfxOperator: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER pdfOperator AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER pdfValue    AS CHARACTER  NO-UNDO.

    DEFINE BUFFER tt_state_op  FOR tt_state_op.
    DEFINE BUFFER btt_state_op FOR tt_state_op.

    IF glNoOptimize THEN DO:
        RUN OutputTextContent(pdfStream, "GRAPHIC", pdfValue + " " + pdfOperator {&debugComment}, "", "").
        RETURN.
    END.

    FIND tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type     = "G"
        AND tt_state_op.operator = pdfOperator NO-ERROR.
    IF NOT AVAILABLE tt_state_op THEN DO:
        CASE pdfOperator:
            WHEN "RG" OR
            WHEN "rg" THEN
                IF pdfValue = "0 0 0" THEN RETURN.
            WHEN "J" OR
            WHEN "j" THEN
                IF pdfValue = "0" THEN RETURN.
            WHEN "d" THEN
                IF pdfValue = "[] 0" THEN RETURN.
        END CASE.
        CREATE tt_state_op.
        ASSIGN
            tt_state_op.obj_stream = pdfStream
            tt_state_op.type       = "G"
            tt_state_op.operator   = pdfOperator.
    END.
    IF tt_state_op.opvalue = pdfValue THEN
        RETURN.
    ASSIGN
        tt_state_op.opvalue = pdfValue
        tt_state_op.is_dirty = TRUE.
END.

PROCEDURE flushGfxOperators: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE BUFFER tt_state_op   FOR tt_state_op.
    DEFINE BUFFER bmtt_state_op FOR tt_state_op.

    IF glNoOptimize THEN RETURN.

    FOR EACH tt_state_op WHERE tt_state_op.obj_stream = pdfStream
      AND tt_state_op.type     = "G"
      AND tt_state_op.is_dirty = TRUE:
        PUT STREAM S_pdf_out UNFORMATTED tt_state_op.opvalue + " " + tt_state_op.operator {&debugComment} {&pdfskip}.
        tt_state_op.is_dirty = FALSE.
    END.
END.

PROCEDURE initGfxOperators: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    DEFINE BUFFER tt_state_op FOR tt_state_op.

    IF glNoOptimize THEN RETURN.

    /* set the operators as dirty, except if they have the default value */
    FOR EACH tt_state_op WHERE tt_state_op.obj_stream = pdfStream
        AND tt_state_op.type = "G":
        CASE tt_state_op.operator:
            WHEN "RG" OR
            WHEN "rg" THEN
                tt_state_op.is_dirty = tt_state_op.opvalue <> "0 0 0".
            WHEN "J" OR
            WHEN "j" THEN
                tt_state_op.is_dirty = tt_state_op.opvalue <> "0".
            WHEN "d" THEN
                tt_state_op.is_dirty = tt_state_op.opvalue <> "[] 0".
            OTHERWISE
                tt_state_op.is_dirty = TRUE.
        END CASE.
    END.
END.
/* jcc end */

PROCEDURE OutputTextContent: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfType        AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfPreContent  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfContent     AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfPostContent AS CHARACTER   NO-UNDO.

  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN RETURN.

  IF pdf_Page(pdfStream) > 0 THEN DO:

    /* Determine if the stream name has changed during processing and if so
       close the old stream and open the new stream */
    IF pdfStream <> pdf_CurrentStream THEN DO:
      OUTPUT STREAM S_pdf_out CLOSE.
      OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, TT_pdf_stream.obj_UniqueID, pdf_Page(pdfStream))) BINARY NO-MAP NO-CONVERT NO-ECHO APPEND.
      pdf_CurrentStream = pdfStream.
    END.

    IF pdfType BEGINS "TEXT" AND TT_pdf_Stream.obj_DoingText THEN DO:
      IF pdfPostContent BEGINS ") Tj" THEN /*TODO: should be done with all text showing operators*/
          RUN flushTextOperators(pdfStream).
      RUN PutStreamContent(pdfStream, 
                           pdfPreContent,
                           pdfContent,
                           pdfPostContent).
      RETURN.
    END.

    IF pdfType BEGINS "IMAGE" OR pdfType = "QOPEN" THEN DO:
     
      IF TT_pdf_stream.obj_DoingGraphic THEN DO:
        PUT STREAM S_pdf_out UNFORMATTED 
                       {&pdfSkip}
                   "Q" {&pdfSkip}
                   "q" {&pdfSkip}.
      END.
      ELSE IF NOT TT_pdf_Stream.obj_DoingGraphic THEN DO:
        IF TT_pdf_stream.obj_DoingText THEN
           PUT STREAM S_pdf_out UNFORMATTED 
                           {&pdfSkip}
                      "ET" {&pdfSkip}.

        PUT STREAM S_pdf_out UNFORMATTED 
                       {&pdfSkip}
                   "q" {&pdfSkip}.
      END.

      ASSIGN TT_pdf_stream.obj_DoingText    = FALSE
             TT_pdf_stream.obj_DoingGraphic = TRUE.

      IF pdfType = "QOPEN" THEN
        RUN initGfxOperators(pdfStream).
    END. /* End Image */

    ELSE IF pdfType BEGINS "TEXT" AND NOT TT_pdf_Stream.obj_DoingText THEN DO:
      IF TT_pdf_Stream.obj_DoingGraphic THEN DO:
        PUT STREAM S_pdf_out UNFORMATTED 
                       {&pdfSkip}
                   "Q" {&pdfSkip}.
      END.

      PUT STREAM S_pdf_out UNFORMATTED 
                           "BT" {&pdfSKIP}. 

      RUN initTextOperators(pdfStream).

      ASSIGN TT_pdf_Stream.obj_DoingGraphic = FALSE
             TT_pdf_Stream.obj_DoingText    = TRUE.

    END.

    ELSE IF TT_pdf_Stream.obj_DoingText AND NOT pdfType BEGINS "TEXT" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
               {&pdfSkip}
          "ET" {&pdfskip}
          "q" {&pdfskip}.

      RUN initGfxOperators(pdfStream).

      ASSIGN TT_pdf_Stream.obj_DoingText    = FALSE
             TT_pdf_Stream.obj_DoingGraphic = TRUE.

    END.
    
    ELSE IF TT_pdf_Stream.obj_DoingGraphic AND pdfType BEGINS "TEXT" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
              {&pdfSkip}
          "Q" {&pdfskip}
          "BT" {&pdfskip}.

      RUN initTextOperators(pdfStream).

      ASSIGN TT_pdf_Stream.obj_DoingText    = TRUE
             TT_pdf_Stream.obj_DoingGraphic = FALSE.

    END.

    ELSE IF NOT TT_pdf_Stream.obj_DoingGraphic 
         AND NOT pdfType BEGINS "TEXT" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
              {&pdfSkip}
          "q" {&pdfskip}.

      RUN initGfxOperators(pdfStream).

      ASSIGN TT_pdf_Stream.obj_DoingText    = FALSE
             TT_pdf_Stream.obj_DoingGraphic = TRUE.

    END.

    IF TT_pdf_Stream.obj_DoingText AND pdfPostContent BEGINS ") Tj" THEN /*TODO: should be done with all text showing operators*/
        RUN flushTextOperators(pdfStream).
    ELSE IF TT_pdf_stream.obj_DoingGraphic AND COMPARE(pdfPreContent, "<>", "Q", "CASE-SENSITIVE") THEN /* 14-OCT-2014 jcc: add COMPARE: do not flush gfx operators when closing gfx context */
        RUN flushGfxOperators(pdfStream).

    RUN PutStreamContent(pdfStream, 
                         pdfPreContent,
                         pdfContent,
                         pdfPostContent).

    IF pdfType BEGINS "IMAGE" OR pdfType = "QCLOSE" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
                     {&pdfSkip}
                 "Q" {&pdfSkip}.
      ASSIGN TT_pdf_Stream.obj_DoingText = FALSE
             TT_pdf_Stream.obj_DoingGraphic = FALSE.
    END.

  END. /* Page > 0 */

END. /* OutputTextContent */

FUNCTION StripBoldItal RETURNS CHARACTER
  (INPUT pString  AS CHARACTER):

  ASSIGN pString = REPLACE(REPLACE(REPLACE(REPLACE(
                      pString,{&BoldOnChar},"")
                             ,{&BoldOffChar},"")
                             ,{&ItalicOnChar},"")
                             ,{&ItalicOffChar},"").
  RETURN pString.
END FUNCTION.

/* 25-JUL-2012 jcc: factorize code for PutStreamContent */
/* This one flushes the current buffer to the pdf file */
PROCEDURE _PutStreamContent_flush: /* PRIVATE */
    DEFINE INPUT        PARAMETER pdfStream      AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pdfPreContent  AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcString       AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pdfPostContent AS CHARACTER   NO-UNDO.
    DEFINE              PARAMETER BUFFER TT_pdf_font FOR TT_pdf_font.
    DEFINE INPUT        PARAMETER plTrackPos     AS LOGICAL     NO-UNDO.
    DEFINE INPUT        PARAMETER plDoingText    AS LOGICAL     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pdfColumn      AS DECIMAL     NO-UNDO.

    DEFINE VARIABLE cString     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStringOut  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSize       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iByte       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE mptrContent AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE mZero       AS MEMPTR      NO-UNDO.

    IF plDoingText THEN DO:
        RUN flushTextOperators(pdfStream).
        PUT STREAM S_pdf_out UNFORMATTED pdfPreContent.
        /* 23-FEB-2010 jcc: unicode support + fix post content */
        IF LENGTH(pcString) > 0 THEN DO:
            cString = StripBoldItal(pcString).
            /* 17-FEB-2010 jcc: TODO: unicode support (for the moment, outputs as is) */
            /* 11-FEB-2014 jcc: DONE! ;-) */
            IF AVAILABLE TT_pdf_font AND TT_pdf_font.is_unicode THEN DO:
                RUN utf8_to_utf16be(pdfStream, cString, "streamContent", ?, OUTPUT mptrContent).
                /* 07-JUL-2014 jcc: must replace here chr(13) by "\r" because sometimes it fails in utf8_to_utf16be */
                /* EXPORT STREAM S_pdf_out mptrContent. */
                SET-SIZE(mZero) = 1.
                iSize = GET-SIZE(mptrContent).
                DO i = 1 TO iSize:
                    iByte = GET-BYTE(mptrContent, i).
                    IF iByte = 0 THEN
                        EXPORT STREAM S_pdf_out mZero.
                    ELSE
                        PUT STREAM S_pdf_out UNFORMATTED 
                            IF iByte = 13 THEN "\r" ELSE 
                            IF iByte = 10 THEN "\n" ELSE 
                            CHR(iByte, 'iso8859-1', 'iso8859-1').
                END.
                SET-SIZE(mptrContent) = 0.
            END.
            ELSE DO:
                /* 1-OCT-2014 jcc: pdf_replace_text is now done here (once the string has been cut in pieces and we are sure that the what we are outputting is not a mix of unicode or not - which is possible when using the <font> tag) */
                RUN pdf_replace_text (pdfStream, cString, NO, OUTPUT cStringOut).
                PUT STREAM S_pdf_out UNFORMATTED cStringOut.
            END.

            /* 24-JUL-2012 jcc: track pos when underlining */
            IF plTrackPos THEN ASSIGN
                /* cString  = IF SUBSTRING(cString, 1, 1) = "(" THEN SUBSTRING(cString, 2) ELSE cString */ /* 06-JUL-2014 jcc: WTF? */
                pdfColumn = pdfColumn + pdf_text_widthdec(pdfStream, cString).
            /* 03-JUL-2014 jcc: when subsetting the font, set the string characters as used */
            /* not when plTrackPos is set, 'cause pdf_text_width already sets the characters as used */
            ELSE IF TT_pdf_font.font_subset THEN
                RUN pdf_subset_add_string (pdfStream, pdf_Font(pdfStream), cString, TT_pdf_font.is_unicode).
        END.
        PUT STREAM S_pdf_out UNFORMATTED
            /* StripBoldItal(cString) */
            pdfPostContent {&pdfSKIP}.
        /* 23-FEB-2010 jcc: end */
    END.
    ELSE DO:
        RUN flushGfxOperators(pdfStream).
        PUT STREAM S_pdf_out UNFORMATTED
            pdfPreContent
            pcString
            pdfPostContent {&pdfSKIP}.
    END.
END.

/* buffers a line for underline, strike or link */
PROCEDURE doLine: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfRow    AS DECIMAL     NO-UNDO. /* base line */
    DEFINE INPUT  PARAMETER pdfShift  AS DECIMAL     NO-UNDO. /* shift according to the base line */
    DEFINE INPUT  PARAMETER pdeInit   AS DECIMAL     NO-UNDO. /* initial point from where we eventually rotate to pdf_Angle */
    DEFINE INPUT  PARAMETER pdeStart  AS DECIMAL     NO-UNDO. /* start of the line */
    DEFINE INPUT  PARAMETER pdeEnd    AS DECIMAL     NO-UNDO. /* end of the line */
    DEFINE INPUT  PARAMETER pdeWeight AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pcColor   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER plDoLink  AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pcLink    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pdfPointSize AS DECIMAL     NO-UNDO.

    DEFINE VARIABLE cos      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE pdfAngle AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sin      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE x3       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE x4       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE y3       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE y4       AS DECIMAL   NO-UNDO.

    
    IF pdeStart = pdeEnd THEN RETURN.
    
    pdfAngle = pdf_Angle(pdfStream).
    IF pdfAngle <> 0 THEN DO:
        cos = math_cos(pdfAngle).
        sin = math_sin(pdfAngle).
    END.

    CREATE tt_line.
    ASSIGN
     tt_line.obj_stream = pdfStream
     tt_line.startx     = IF pdfAngle = 0 THEN pdeStart          ELSE pdeInit + (pdeStart - pdeInit) * cos - pdfShift * sin
     tt_line.starty     = IF pdfAngle = 0 THEN pdfRow + pdfShift ELSE pdfRow  + (pdeStart - pdeInit) * sin + pdfShift * cos
     tt_line.endx       = IF pdfAngle = 0 THEN pdeEnd            ELSE pdeInit + (pdeEnd   - pdeInit) * cos - pdfShift * sin
     tt_line.endy       = IF pdfAngle = 0 THEN pdfRow + pdfShift ELSE pdfRow  + (pdeEnd   - pdeInit) * sin + pdfShift * cos
     tt_line.weight     = pdeWeight
     tt_line.red        = IF pcColor = "" THEN pdf_TextRed(pdfStream)   ELSE INTEGER(ENTRY(1, pcColor)) / 255
     tt_line.green      = IF pcColor = "" THEN pdf_TextGreen(pdfStream) ELSE INTEGER(ENTRY(2, pcColor)) / 255
     tt_line.blue       = IF pcColor = "" THEN pdf_TextBlue(pdfStream)  ELSE INTEGER(ENTRY(3, pcColor)) / 255
    .

    /* Create the annotation (link) if told so */
    IF plDoLink THEN DO:
        CREATE TT_pdf_annot.
        ASSIGN TT_pdf_annot.obj_stream    = pdfStream
               TT_pdf_annot.annot_type    = "Link"
               TT_pdf_annot.annot_content = IF pcLink > "" THEN pcLink ELSE pdf_get_parameter(pdfStream, "___TagLink")
               TT_pdf_annot.annot_page    = pdf_Page(pdfStream)
               TT_pdf_annot.in_transaction = pdf_in_transaction(pdfStream).
        ASSIGN TT_pdf_annot.annot_color   = "0 0 0"
               TT_pdf_annot.annot_border  = 0
               TT_pdf_annot.annot_style   = "N".
        IF pdfAngle = 0 THEN
            ASSIGN TT_pdf_annot.annot_rect = dec2string(pdeStart) + " "
                                           + dec2string(pdfRow + pdfPointSize + pdfShift) + " "
                                           + dec2string(pdeEnd) + " "
                                           + dec2string(pdfRow + pdfShift).
        ELSE /* make the rectangle the bounding box */
            ASSIGN
                x3 = pdeInit + (pdeEnd   - pdeInit) * cos - (pdfPointSize + pdfShift) * sin
                y3 = pdfRow  + (pdeEnd   - pdeInit) * sin + (pdfPointSize + pdfShift) * cos
                x4 = pdeInit + (pdeStart - pdeInit) * cos - (pdfPointSize + pdfShift) * sin
                y4 = pdfRow  + (pdeStart - pdeInit) * sin + (pdfPointSize + pdfShift) * cos
                TT_pdf_annot.annot_rect = dec2string(MINIMUM(tt_line.startx, tt_line.endx, x3, x4)) + " "
                                        + dec2string(MINIMUM(tt_line.starty, tt_line.endy, y3, y4)) + " "
                                        + dec2string(MAXIMUM(tt_line.startx, tt_line.endx, x3, x4)) + " "
                                        + dec2string(MAXIMUM(tt_line.starty, tt_line.endy, y3, y4)).
    END. /* IF plDoLink */
END.
/* jcc: end */

PROCEDURE PutStreamContent:  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPreContent  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfContent     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPostContent AS CHARACTER NO-UNDO.

  DEFINE BUFFER TT_pdf_page    FOR TT_pdf_page.
  DEFINE BUFFER TT_pdf_xobject FOR TT_pdf_xobject.
  DEFINE BUFFER TT_pdf_font    FOR TT_pdf_font.

  DEFINE VARIABLE cDefaultFont    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBoldFont       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cItalicFont     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBoldItalicFont AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cColor          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLinkColor      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrevColor      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInitColor      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColors         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColorChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOpenPar        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cChar           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cString         AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iChar           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iColor          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iNumColors      AS INTEGER   NO-UNDO.

  DEFINE VARIABLE lUseTags        AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lHasTags        AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lTrackPos       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lLinesToFlush   AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE pdfColumn        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE pdfColumnInitial AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE pdfRow           AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE pdfPointSize     AS DECIMAL     NO-UNDO.

  DEFINE VARIABLE deStartUnderline AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE deStartStrike    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE deStartLink      AS DECIMAL     NO-UNDO.

  DEFINE VARIABLE iLink     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cLinkChar AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLink     AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE iFont     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cFontChar AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFont     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFonts    AS CHARACTER   NO-UNDO.

  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN RETURN.

  /* PUT STREAM S_pdf_out UNFORMATTED pdfPreContent. */ /* 18-FEB-2014 jcc: moved pdfPreContent to the PUT STREAM below */

  IF pdfContent = "" AND pdfPostContent = "" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED pdfPreContent {&pdfSkip}.
      RETURN.
  END.

  /* Convert tags to single characters - easier to work with */
  IF TT_pdf_stream.obj_DoingText AND pdf_get_parameter(pdfStream,"UseTags") = "TRUE" THEN DO:
    ASSIGN pdfContent = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(
                            pdfContent, "<B>",       {&BoldOnChar})
                                      , "<~/B>",     {&BoldOffChar})
                                      , "<I>",       {&ItalicOnChar})
                                      , "<~/I>",     {&ItalicOffChar}) 
                                      , "<Color=",   {&ColorOnChar})
                                      , "<~/Color>", {&ColorOffChar}) 
                                      , "<U>",       {&UnderlineOnChar})
                                      , "<~/U>",     {&UnderlineOffChar}) 
                                      , "<S>",       {&StrikeOnChar})
                                      , "<~/S>",     {&StrikeOffChar}) 
                                      , "<URL=",     {&LinkOnChar})
                                      , "<~/URL>",   {&LinkOffChar})
                                      , "<font=",    {&FontOnChar})
                                      , "<~/font>",  {&FontOffChar})
           lUseTags  = TRUE
           lTrackPos = INDEX(pdfContent, {&UnderlineOnChar}) > 0
                    OR INDEX(pdfContent, {&UnderlineOffChar}) > 0
                    OR INDEX(pdfContent, {&StrikeOnChar}) > 0
                    OR INDEX(pdfContent, {&StrikeOffChar}) > 0
                    OR INDEX(pdfContent, {&LinkOnChar}) > 0
                    OR INDEX(pdfContent, {&LinkOffChar}) > 0                       
           lHasTags  = INDEX(pdfContent, {&BoldOnChar}) > 0
                    OR INDEX(pdfContent, {&BoldOffChar}) > 0
                    OR INDEX(pdfContent, {&ItalicOnChar}) > 0
                    OR INDEX(pdfContent, {&ItalicOffChar}) > 0
                    OR INDEX(pdfContent, {&ColorOnChar}) > 0
                    OR INDEX(pdfContent, {&ColorOffChar}) > 0
                    OR INDEX(pdfContent, {&FontOnChar}) > 0
                    OR INDEX(pdfContent, {&FontOffChar}) > 0
                    OR lTrackPos
           NO-ERROR.
  END.

  /* 24-JUL-2012 jcc: track X and Y position when underline, strike or link wrapped from previous line */
  IF  TT_pdf_stream.obj_DoingText AND
     (INTEGER(pdf_get_parameter(pdfStream, "UnderlineCount")) > 0
   OR INTEGER(pdf_get_parameter(pdfStream, "StrikeCount")) > 0
   OR INTEGER(pdf_get_parameter(pdfStream, "LinkCount")) > 0) THEN ASSIGN
      lTrackPos = TRUE.
  /* pdfPreContent = REPLACE(pdfPreContent, "~n", " "). */
  IF lTrackPos THEN DO:
      /* Get the initial position */
      pdfPreContent = REPLACE(pdfPreContent, "~n", " "). /* 18-FEB-2014 jcc: moved here */
      IF LOOKUP("Tm", pdfPreContent, " ") > 2 THEN /* force a matrix value via the preContent */
          ASSIGN
           pdfColumn = string2dec(ENTRY(LOOKUP("Tm", pdfPreContent, " ") - 2, pdfPreContent, " "))
           pdfRow    = string2dec(ENTRY(LOOKUP("Tm", pdfPreContent, " ") - 1, pdfPreContent, " ")).
      ELSE /* normal case: get the text position */
          ASSIGN
              pdfColumn = pdf_TextX(pdfStream)
              pdfRow    = pdf_TextY(pdfStream).

      pdfColumnInitial = pdfColumn.
      /* if we were previously underlining, then set the start of the underline */
      IF INTEGER(pdf_get_parameter(pdfStream, "UnderlineCount")) > 0 THEN
          /* deStartUnderline = pdfColumn + IF pdf_WrapText AND pdf_WrapFrom > 1 THEN pdf_text_widthdec(pdfStream, FILL(" ", pdf_WrapFrom - 1)) ELSE 0. */ /* 05-DEC-2016 jcc: see pdf_wrap_text */
          deStartUnderline = pdfColumn.
      IF INTEGER(pdf_get_parameter(pdfStream, "StrikeCount")) > 0 THEN
          /* deStartStrike = pdfColumn + IF pdf_WrapText AND pdf_WrapFrom > 1 THEN pdf_text_widthdec(pdfStream, FILL(" ", pdf_WrapFrom - 1)) ELSE 0. */ /* 05-DEC-2016 jcc: see pdf_wrap_text */
          deStartStrike = pdfColumn.
      IF INTEGER(pdf_get_parameter(pdfStream, "LinkCount")) > 0 THEN
          /* deStartLink = pdfColumn + IF pdf_WrapText AND pdf_WrapFrom > 1 THEN pdf_text_widthdec(pdfStream, FILL(" ", pdf_WrapFrom - 1)) ELSE 0. */ /* 05-DEC-2016 jcc: see pdf_wrap_text */
          deStartLink = pdfColumn.
  END.

  pdfPointSize = pdf_PointSize(pdfStream).

  /* 21-OCT-2014 jcc: get the current tt_pdf_font */
  RUN _pdf_get_font (pdfStream, pdf_Font(pdfStream), BUFFER TT_pdf_font).

  IF lHasTags THEN DO:

    /* Obtain Tag font associations */
    ASSIGN cBoldFont       = pdf_get_parameter(pdfStream,"BoldFont")
           cItalicFont     = pdf_get_parameter(pdfStream,"ItalicFont")
           cBoldItalicFont = pdf_get_parameter(pdfStream,"BoldItalicFont")
           cDefaultFont    = pdf_get_parameter(pdfStream,"DefaultFont").

    /* Loop through the text finding the blocks to change */
    DO iChar = 1 TO LENGTH(pdfContent, "CHARACTER"):
      cChar = SUBSTRING(pdfContent, iChar, 1, "CHARACTER").

      /* Toggle Font */
      CASE cChar:
        /* Toggle Bold ON */
        WHEN {&BoldOnChar} THEN DO:
          /* Write buffer at current position */
          RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

          cString = "".

          /* Increment the Bold level counter */
          RUN pdf_incr_parameter(pdfStream, "BoldCount").

          /* Determine whether we should bold and italicize or just bold */
          IF INT(pdf_get_parameter(pdfStream,"ItalicCount")) > 0 THEN
            RUN _pdf_set_font (pdfStream, cBoldItalicFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
          ELSE
            RUN _pdf_set_font (pdfStream, cBoldFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
        END. /* BoldOn */

        /* Toggle Bold OFF */
        WHEN {&BoldOffChar} THEN DO:
          /* Write buffer at current position */
          RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

          cString = "".

          /* Decrement the Bold level counter */
          RUN pdf_decr_parameter(pdfStream, "BoldCount").

          /* Determine whether we return to Italics or the Default font */
          IF INT(pdf_get_parameter(pdfStream,"BoldCount")) <= 0 THEN DO: 
            IF INT(pdf_get_parameter(pdfStream,"ItalicCount")) > 0 THEN
              RUN _pdf_set_font (pdfStream, cItalicFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
            ELSE
              RUN _pdf_set_font (pdfStream, cDefaultFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
          END.
        END.

        /* Toggle Italics ON */
        WHEN {&ItalicOnChar} THEN DO:
          RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

          cString   = "".

          /* Increment the Italic level counter */
          RUN pdf_incr_parameter(pdfStream, "ItalicCount").

          /* Determine whether we should italicize and bold or just italicize */
          IF INT(pdf_get_parameter(pdfStream,"BoldCount")) > 0 THEN
            RUN _pdf_set_font (pdfStream, cBoldItalicFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
          ELSE
            RUN _pdf_set_font (pdfStream, cItalicFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
        END.

        /* Toggle Italics OFF */
        WHEN {&ItalicOffChar} THEN DO:
          RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

          cString = "".

          /* Decrement the Italic level counter */
          RUN pdf_decr_parameter(pdfStream, "ItalicCount").

          /* Determine whether we return to Italics or the Default font */
          IF INT(pdf_get_parameter(pdfStream,"ItalicCount")) <= 0 THEN DO: 
            IF INT(pdf_get_parameter(pdfStream,"BoldCount")) > 0 THEN
              RUN _pdf_set_font (pdfStream, cBoldFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
            ELSE
              RUN _pdf_set_font (pdfStream, cDefaultFont, pdfPointSize, BUFFER TT_pdf_font) NO-ERROR.
          END.
        END.

        /* Toggle Color ON */
        WHEN {&ColorOnChar} THEN DO:
          RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

          ASSIGN cString   = ""
                 cColor    = "".

          /* Read the colour */
          COLOR-LOOP:
          DO iColor = (iChar + 1) TO LENGTH(pdfContent):
            cColorChar = SUBSTRING(pdfContent, iColor, 1).
            IF cColorChar = ">" THEN
              LEAVE COLOR-LOOP.
            ELSE
              cColor = cColor + cColorChar.
          END.
          
          iChar = iColor.

          /* Store the Color */
          RUN pdf_set_parameter(pdfStream, "ColorLevel",
                                IF pdf_get_parameter(pdfStream, "ColorLevel") <> "" 
                                THEN pdf_get_parameter(pdfStream, "ColorLevel") + "," + cColor
                                ELSE cColor).

          /* Find Color settings based on Color Name */
          cColor = pdf_get_parameter2(pdfStream, "TagColor:" + cColor, "0,0,0").

          cPrevColor = pdf_get_parameter(pdfStream, "___Color").
          RUN _pdf_set_parameter_priv(pdfStream, "___Color", cColor).

          /* Change the color of the ongoing underline (if any) : terminate the current one and restart */
          IF INTEGER(pdf_get_parameter(pdfStream, "UnderlineCount")) > 0 THEN DO:
              RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartUnderline, pdfColumn, 0.5, cPrevColor, NO, "", pdfPointSize).
              ASSIGN
               lLinesToFlush    = TRUE
               deStartUnderline = pdfColumn.
          END.
          IF INTEGER(pdf_get_parameter(pdfStream, "StrikeCount")) > 0 THEN DO:
              RUN doLine(pdfStream, pdfRow, pdfPointSize / 2 - 1, pdfColumnInitial, deStartStrike, pdfColumn, 0.5, cPrevColor, NO, "", pdfPointSize).
              ASSIGN
               lLinesToFlush = TRUE
               deStartStrike = pdfColumn.
          END.

          /* Set Color */
          RUN pdf_rgb (pdfStream, "pdf_text_color", cColor).
        END.

        /* Toggle Color OFF */
        WHEN {&ColorOffChar} THEN DO:
          RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

          ASSIGN cString = ""
                 cColor  = "".

          /* Remove the Last Color */
          cColors    = pdf_get_parameter(pdfStream,"ColorLevel").
          iNumColors = NUM-ENTRIES(cColors) - 1.

          IF iNumColors >= 1 THEN 
          DO iColor = 1 TO iNumColors:
            RUN pdf_set_parameter(pdfStream, "ColorLevel",
                                  IF iColor > 1
                                  THEN pdf_get_parameter(pdfStream, "ColorLevel") + "," + ENTRY(iColor,cColors)
                                  ELSE ENTRY(iColor,cColors)).
          END.

          /* Remove last entry */
          IF iNumColors < 1 THEN DO:
            RUN pdf_set_parameter(pdfStream, "ColorLevel", "").
            cColor = pdf_get_parameter2(pdfStream, "DefaultColor", "Black").
          END. 
          ELSE
            cColor = ENTRY(iNumColors, cColors).

          /* Find Color settings based on Color Name */
          cColor = pdf_get_parameter2(pdfStream, "TagColor:" + cColor, "0,0,0").

          cPrevColor = pdf_get_parameter(pdfStream, "___Color").
          RUN _pdf_set_parameter_priv(pdfStream, "___Color", cColor).

          /* Change the color of the ongoing underline (if any) : terminate the current one and restart */
          IF INTEGER(pdf_get_parameter(pdfStream, "UnderlineCount")) > 0 THEN DO:
              RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartUnderline, pdfColumn, 0.5, cPrevColor, NO, "", pdfPointSize).
              ASSIGN
               lLinesToFlush    = TRUE
               deStartUnderline = pdfColumn.
          END.
          IF INTEGER(pdf_get_parameter(pdfStream, "StrikeCount")) > 0 THEN DO:
              RUN doLine(pdfStream, pdfRow, pdfPointSize / 2 - 1, pdfColumnInitial, deStartStrike, pdfColumn, 0.5, cPrevColor, NO, "", pdfPointSize).
              ASSIGN
               lLinesToFlush = TRUE
               deStartStrike = pdfColumn.
          END.

          /* Set Color */
          RUN pdf_rgb (pdfStream, "pdf_text_color", cColor).
        END.
          
        /* Toggle Underline ON - 24-JUL-2012 jcc */
        WHEN {&UnderlineOnChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

            ASSIGN
             deStartUnderline = pdfColumn
             cString = "".

            /* Increment the Underline level counter */
            RUN pdf_incr_parameter(pdfStream, "UnderlineCount").

        END.

        /* Toggle Underline OFF - 24-JUL-2012 jcc */
        WHEN {&UnderlineOffChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

            cString = "".

            /* Decrement the Underline level counter */
            RUN pdf_decr_parameter(pdfStream, "UnderlineCount").

            /* Define the underline - must be buffered in a temp-table as we are currently doing text */
            IF cColor = "" THEN cColor = pdf_get_parameter(pdfStream, "___Color").
            RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartUnderline, pdfColumn, 0.5, cColor, NO, "", pdfPointSize).
            lLinesToFlush = TRUE.
        END.

        /* Toggle Strike ON - 24-JUL-2012 jcc */
        WHEN {&StrikeOnChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

            ASSIGN
             deStartStrike = pdfColumn
             cString = "".

            /* Increment the Strike level counter */
            RUN pdf_incr_parameter(pdfStream, "StrikeCount").

        END.

        /* Toggle Strike OFF - 24-JUL-2012 jcc */
        WHEN {&StrikeOffChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

            cString = "".

            /* Decrement the Strike level counter */
            RUN pdf_decr_parameter(pdfStream, "StrikeCount").

            /* Define the strike - must be buffered in a temp-table as we are currently doing text */
            IF cColor = "" THEN cColor = pdf_get_parameter(pdfStream, "___Color").
            RUN doLine(pdfStream, pdfRow, pdfPointSize / 2 - 1, pdfColumnInitial, deStartStrike, pdfColumn, 0.5, cColor, NO, "", pdfPointSize).
            lLinesToFlush = TRUE.
        END.

        /* Toggle Link ON - 24-JUL-2012 jcc */
        WHEN {&LinkOnChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

            ASSIGN
             deStartLink = pdfColumn
             cString = "".

            /* Increment the Link level counter */
            RUN pdf_incr_parameter(pdfStream, "LinkCount").

            /* Read the link */
            cLink = "".
            LINK-LOOP:
            DO iLink = (iChar + 1) TO LENGTH(pdfContent):
              cLinkChar = SUBSTRING(pdfContent, iLink, 1).
              IF cLinkChar = ">" THEN LEAVE LINK-LOOP.
              ELSE
                cLink = cLink + cLinkChar.
            END.
            cLink = REPLACE(cLink, "%20", " "). /* 24-MAR-2015 jcc: spaces are not allowed within links */

            iChar = iLink.

            RUN _pdf_set_parameter_priv(pdfStream, "___TagLink", cLink).

            /* Set Color */
            cLinkColor = pdf_get_parameter2(pdfStream, "LinkColor", "0,0,255").
            RUN _pdf_set_parameter_priv(pdfStream, "___Color", cLinkColor).
            RUN pdf_rgb (pdfStream, "pdf_text_color", cLinkColor).
        END.

        /* Toggle Link OFF - 24-JUL-2012 jcc */
        WHEN {&LinkOffChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

            cString = "".

            /* Decrement the Link level counter */
            RUN pdf_decr_parameter(pdfStream, "LinkCount").

            /* Define the link & the underline - must be buffered in a temp-table as we are currently doing text */
            cLinkColor = pdf_get_parameter2(pdfStream, "LinkColor", "0,0,255").
            RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartLink, pdfColumn, 0.5, cLinkColor, YES, cLink, pdfPointSize).
            lLinesToFlush = TRUE.
            /* Decrement the Link level counter */
            RUN _pdf_set_parameter_priv(pdfStream, "___TagLink", "").
            /* Reset Color */
            RUN pdf_rgb (pdfStream, "pdf_text_color", pdf_get_parameter2(pdfStream, "TagColor:" + pdf_get_parameter2(pdfStream, "DefaultColor", "Black"), "0,0,0")).
            RUN _pdf_set_parameter_priv(pdfStream, "___Color", "").
        END.

        /* Toggle Font ON - 30-JUN-2014 jcc */
        WHEN {&FontOnChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).
            cString = "".

            /* Read the font */
            cFont = "".
            FONT-LOOP:
            DO iFont = (iChar + 1) TO LENGTH(pdfContent):
              cFontChar = SUBSTRING(pdfContent, iFont, 1).
              IF cFontChar = ">" THEN LEAVE FONT-LOOP.
              ELSE
                cFont = cFont + cFontChar.
            END.

            iChar = iFont.

            /* Store the Font */
            cFonts = pdf_get_parameter(pdfStream, "___OldFont").
            RUN _pdf_set_parameter_priv(pdfStream, "___OldFont",
                                  IF cFonts <> "" 
                                  THEN cFonts + "," + pdf_Font(pdfStream)
                                  ELSE pdf_Font(pdfStream)).

            /* Set Font */
            RUN _pdf_set_font (pdfStream, cFont, pdfPointSize, BUFFER TT_pdf_font).
        END.

        /* Toggle Font OFF - 30-JUN-2014 jcc */
        WHEN {&FontOffChar} THEN DO:
            RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).
            cString = "".

            cFonts = pdf_get_parameter(pdfStream, "___OldFont").
            cFont  = ENTRY(1, cFonts).
            /* Remove the Last Font */
            ENTRY(1, cFonts) = "".
            IF cFonts > "" THEN
                cFonts = SUBSTRING(cFonts, 2).
            RUN _pdf_set_parameter_priv (pdfStream, "___OldFont", cFonts).

            /* Set old Font */
            RUN _pdf_set_font (pdfStream, cFont, pdfPointSize, BUFFER TT_pdf_font).
        END.

        /* Append to the current block of text */
        OTHERWISE ASSIGN cString = cString + cChar NO-ERROR.
      END CASE.
    END. /* Do iChar */

    /* Write any remaining text at the current position */
    RUN _PutStreamContent_flush(pdfStream, pdfPreContent, cString, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, YES, INPUT-OUTPUT pdfColumn).

    /* if underlining (and/or others) wraps at EOL (we did not find a </u>), then buffer the underline */
    IF INTEGER(pdf_get_parameter(pdfStream, "UnderlineCount")) > 0 THEN DO:
        IF cColor = "" THEN cColor = pdf_get_parameter(pdfStream, "___Color").
        RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartUnderline, pdfColumn, 0.5, cColor, NO, "", pdfPointSize).
        lLinesToFlush = TRUE.
    END.
    IF INTEGER(pdf_get_parameter(pdfStream, "StrikeCount")) > 0 THEN DO:
        IF cColor = "" THEN cColor = pdf_get_parameter(pdfStream, "___Color").
        RUN doLine(pdfStream, pdfRow, pdfPointSize / 2 - 1, pdfColumnInitial, deStartStrike, pdfColumn, 0.5, cColor, NO, "", pdfPointSize).
        lLinesToFlush = TRUE.
    END.
    IF INTEGER(pdf_get_parameter(pdfStream, "LinkCount")) > 0 THEN DO:
        cLinkColor = pdf_get_parameter2(pdfStream, "LinkColor", "0,0,255").
        RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartLink, pdfColumn, 0.5, cLinkColor, YES, cLink, pdfPointSize).
        lLinesToFlush = TRUE.
    END.

  END. /* Using Tags, and Tags were found in the text */

  ELSE DO:
    RUN _PutStreamContent_flush(pdfStream, pdfPreContent, pdfContent, pdfPostContent, BUFFER TT_pdf_font, lTrackPos, TT_pdf_stream.obj_DoingText, INPUT-OUTPUT pdfColumn).

    IF TT_pdf_stream.obj_DoingText THEN DO:
        /* 03-MAY-2014 jcc: using tt_pdf_page worked below because it leaked from another procedure. When I defined the buffer as local to the procedure, it ceased working, so we have to find tt_pdf_page here. */
        DEFINE VARIABLE iPage          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iXobjectId     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lUsePageNo     AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lUseTotalPages AS LOGICAL   NO-UNDO.
        lUsePageNo     = INDEX(pdfContent,"@@PAGEno-" + pdfStream) > 0.
        lUseTotalPages = INDEX(pdfContent,"@@TOTALPages-" + pdfStream) > 0.
        IF lUsePageNo OR lUseTotalPages THEN DO:
            /* 07-MAY-2014 jcc: handle xobjects */
            IF pdf_get_parameter2(pdfStream, "___inXobject", "0") = "0" THEN DO:
                iPage = pdf_Page(pdfStream).
                FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream AND TT_pdf_page.page_nbr = iPage.
                IF lUsePageNo     THEN TT_pdf_page.UsePageNo     = TRUE.
                IF lUseTotalPages THEN TT_pdf_page.UseTotalPages = TRUE.
            END.
            ELSE DO:
                iXobjectId = INTEGER(pdf_get_parameter(pdfStream, "___inXobject")).
                FIND FIRST TT_pdf_xobject WHERE TT_pdf_xobject.obj_stream = pdfStream AND TT_pdf_xobject.xobject_id = iXobjectId.
                IF lUsePageNo     THEN TT_pdf_xobject.UsePageNo     = TRUE.
                IF lUseTotalPages THEN TT_pdf_xobject.UseTotalPages = TRUE.
            END.
        END.

        /* if underlining wraps from previous line, then buffer the underline */
        IF pdfContent > "" AND INTEGER(pdf_get_parameter(pdfStream, "UnderlineCount")) > 0 THEN DO:
            IF cColor = "" THEN cColor = pdf_get_parameter(pdfStream, "___Color").
            RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartUnderline, pdfColumn, 0.5, cColor, NO, "", pdfPointSize).
            lLinesToFlush = TRUE.
        END.
        IF pdfContent > "" AND INTEGER(pdf_get_parameter(pdfStream, "StrikeCount")) > 0 THEN DO:
            IF cColor = "" THEN cColor = pdf_get_parameter(pdfStream, "___Color").
            RUN doLine(pdfStream, pdfRow, pdfPointSize / 2 - 1, pdfColumnInitial, deStartStrike, pdfColumn, 0.5, cColor, NO, "", pdfPointSize).
            lLinesToFlush = TRUE.
        END.
        IF pdfContent > "" AND INTEGER(pdf_get_parameter(pdfStream, "LinkCount")) > 0 THEN DO:
            cLinkColor = pdf_get_parameter2(pdfStream, "LinkColor", "0,0,255").
            RUN doLine(pdfStream, pdfRow, -1, pdfColumnInitial, deStartLink, pdfColumn, 0.5, cLinkColor, YES, cLink, pdfPointSize).
            lLinesToFlush = TRUE.
        END.
    END.

  END.

  /* 25-JUL-2012 jcc: draw lines (if any underline, strike, or link) */
  IF lLinesToFlush THEN
      RUN flushLines(pdfStream).

END. /* PutStreamContent */

/* 1-AUG-2012 jcc : draw all the lines at once: underline, strike, link */
PROCEDURE flushLines: /* PRIVATE */
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.

    /* 14-OCT-2014 jcc: backup old stroke color to restore it afterward */
    DEFINE VARIABLE dBlue  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dGreen AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRed   AS DECIMAL   NO-UNDO.
    dBlue  = pdf_StrokeBlue(pdfStream).
    dGreen = pdf_StrokeGreen(pdfStream).
    dRed   = pdf_StrokeRed(pdfStream).
    
    REPEAT PRESELECT EACH tt_line WHERE tt_line.obj_stream = pdfStream
      BY tt_line.red BY tt_line.green BY tt_line.blue: /* ordered to optimize the pdf size a little: less stroke color changes */
        FIND NEXT tt_line.
        RUN pdf_stroke_color(pdfStream, tt_line.red, tt_line.green, tt_line.blue).
        RUN pdf_line_dec(pdfStream, tt_line.startx, tt_line.starty, tt_line.endx, tt_line.endy, tt_line.weight).
        DELETE tt_line.
    END.

    RUN pdf_stroke_color(pdfStream, dRed, dGreen, dBlue).
END.
/* jcc end */

PROCEDURE ChangePageText: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream        AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfFile          AS CHARACTER   NO-UNDO. /* File to change */
  DEFINE INPUT PARAMETER pdfDynamicJustif AS LOGICAL     NO-UNDO. /* Use dynamic justification (for @@ fields)? */
  DEFINE INPUT PARAMETER pdfFromText      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfToText        AS CHARACTER   NO-UNDO.
  /* 23-APR-2014 jcc: transform pdfFromText & pdfToText into chr(2) delimited lists to perform all the replaces at once */

  DEFINE VARIABLE cFrom          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNewSeek       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE inLine         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iOldSeek       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iXobjectId     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lSpecialFields AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE mLineWithCHR0  AS MEMPTR      NO-UNDO.
  DEFINE VARIABLE vEditor        AS CHARACTER   NO-UNDO.

  DEFINE FRAME f-editor
         veditor VIEW-AS EDITOR SIZE 40 BY 5 LARGE.

  /* 01-MAR-2010 jcc: in batch mode, the editor does not do anything */
  IF SESSION:BATCH-MODE
  OR pdfDynamicJustif /* 23-APR-2014 jcc: handle dynamic justification */
  THEN DO: /* 01-MAR-2010 jcc: as per Jayson Johnson 6/12/2006 */
    OS-RENAME VALUE(pdfFile) VALUE(pdfFile + ".tmp").
    INPUT STREAM S_pdf_inp FROM VALUE(pdfFile + ".tmp") BINARY NO-MAP NO-CONVERT NO-ECHO.
    OUTPUT STREAM S_pdf_out TO VALUE(pdfFile) BINARY NO-MAP NO-CONVERT NO-ECHO.
    blkReadLine:
    REPEAT:
        /* 19-MAR-2015 jcc: manage case when we have some chr(0) due to unicode. ABL does not like chr(0). */
        iOldSeek = SEEK(S_pdf_inp).
        IMPORT STREAM S_pdf_inp UNFORMATTED inLine.
        /* MESSAGE "compare:" SEEK(S_pdf_inp) - iOldSeek - LENGTH(inLine) - 1 "> 1" SKIP "seek:" SEEK(S_pdf_inp) SKIP "oldSeek:" iOldSeek SKIP "read:" LENGTH(inLine) SKIP "line:" inLine VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        IF SEEK(S_pdf_inp) - iOldSeek - LENGTH(inLine) - 1 > 1 THEN DO: /* we had some chr(0) */
            /* 19-MAR-2015 jcc: the naive way: go back and read bytes one by one until next LF or CRLF */
            /* SEEK STREAM s_pdf_inp TO iOldSeek. */
            /* SET-SIZE(mLineWithCHR0) = 1. */
            /* blkLine: */
            /* DO WHILE TRUE: */
                /* IMPORT STREAM s_pdf_inp UNFORMATTED mLineWithCHR0. /* UNFORMATTED is compulsory else dot followed by chr(0) is interpreted as EOF */ */
                /* EXPORT STREAM s_pdf_out mLineWithCHR0. */
                /* IF GET-BYTE(mLineWithCHR0, 1) = 10 OR GET-BYTE(mLineWithCHR0, 1) = 13 THEN DO: */
                    /* IMPORT STREAM s_pdf_inp UNFORMATTED mLineWithCHR0. */
                    /* IF GET-BYTE(mLineWithCHR0, 1) = 10 OR GET-BYTE(mLineWithCHR0, 1) = 13 THEN */
                        /* EXPORT STREAM s_pdf_out mLineWithCHR0. */
                    /* ELSE */
                        /* SEEK STREAM S_pdf_inp TO SEEK(S_pdf_inp) - 1. */
                    /* LEAVE blkLine. */
                /* END. */
            /* END. */
            /* SET-SIZE(mLineWithCHR0) = 0. */

            /* 19-MAR-2015 jcc: the fastest way: go back, read the good nb of bytes at once, output it - based on the fact that because of the CHR(0) ABL reads until the end of next line */
            iNewSeek = SEEK(S_pdf_inp).
            SEEK STREAM S_pdf_inp TO iOldSeek.
            SET-SIZE(mLineWithCHR0) = iNewSeek - iOldSeek - LENGTH(inLine) - 1.
            IMPORT STREAM S_pdf_inp mLineWithCHR0.
            EXPORT STREAM S_pdf_out mLineWithCHR0.
            SET-SIZE(mLineWithCHR0) = 0.
            NEXT blkReadLine.
        END.

        /* 23-APR-2014 jcc: handle dynamic justification */
        IF SUBSTRING(inLine, 1, 1) = CHR(1) THEN DO:
            DEFINE VARIABLE cAlign     AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cFont      AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cMatrix    AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cText      AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cTextOut   AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE dPointSize AS DECIMAL     NO-UNDO.
            DEFINE VARIABLE dWidth     AS DECIMAL     NO-UNDO.
            DEFINE VARIABLE dX         AS DECIMAL     NO-UNDO.
            DEFINE VARIABLE dY         AS DECIMAL     NO-UNDO.

            DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

            ASSIGN
             cAlign     = ENTRY(2, inLine, CHR(1))
             cFont      = ENTRY(3, inLine, CHR(1))
             dPointSize = DECIMAL(ENTRY(4, inLine, CHR(1)))
             dX         = DECIMAL(ENTRY(5, inLine, CHR(1)))
             dY         = DECIMAL(ENTRY(6, inLine, CHR(1)))
             cMatrix    = ENTRY(7, inLine, CHR(1))
             cText      = ENTRY(8, inLine, CHR(1)).
            DO i = NUM-ENTRIES(pdfFromText, CHR(2)) TO 1 BY -1:
                cFrom = ENTRY(i, pdfFromText, CHR(2)).
                IF cFrom > "" THEN DO:
                    cText = REPLACE(cText, cFrom, ENTRY(i, pdfToText, CHR(2))).
                END.
            END.
            dWidth = _pdf_text_width(pdfStream, cText, cFont, "", dPointSize).
            RUN _pdf_get_font (pdfStream, cFont, BUFFER TT_pdf_font). /* 21-OCT-2014 jcc: added, in order to pass the unicode flag to pdf_replace_text */
            RUN pdf_replace_text (pdfStream, cText, TT_pdf_font.is_unicode, OUTPUT cTextOut).
            CASE cAlign:
                WHEN "right" THEN DO:
                    PUT STREAM S_pdf_out UNFORMATTED
                        multiplyTransformationMatrices(cMatrix, "1 0 0 1 " + dec2string(dX - dWidth) + " " + dec2string(dY)) " Tm" SKIP
                        "(" cTextOut ") Tj" SKIP.
                END.
                WHEN "center" THEN DO:
                    PUT STREAM S_pdf_out UNFORMATTED
                        multiplyTransformationMatrices(cMatrix, "1 0 0 1 " + dec2string(dX - dWidth / 2) + " " + dec2string(dY)) " Tm" SKIP
                        "(" cTextOut ") Tj" SKIP.
                END.
            END CASE.
        END. /* IF dynamic justification special line */
        ELSE DO:
            DO i = NUM-ENTRIES(pdfFromText, CHR(2)) TO 1 BY -1:
                cFrom = ENTRY(i, pdfFromText, CHR(2)).
                IF cFrom > "" THEN DO:
                    inLine = REPLACE(inLine, cFrom, ENTRY(i, pdfToText, CHR(2))).
                END.
            END.
            PUT STREAM S_pdf_out UNFORMATTED inLine SKIP.
        END.
    END.
    INPUT STREAM S_pdf_inp CLOSE.
    OUTPUT STREAM S_pdf_out CLOSE.
    OS-DELETE VALUE(pdfFile + ".tmp").      
  END.
  ELSE DO:
      vEditor:INSERT-FILE(pdfFile).
      DO i = NUM-ENTRIES(pdfFromText, CHR(2)) TO 1 BY -1:
          cFrom = ENTRY(i, pdfFromText, CHR(2)).
          IF cFrom > "" THEN DO:
              vEditor:REPLACE(cFrom,
                              ENTRY(i, pdfToText, CHR(2)),
                              8). /* Global */
          END.
      END.
      vEditor:SAVE-FILE(pdfFile).
  END.
END. /* ChangePageText */

PROCEDURE pdf_fill_text:
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFill      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOptions   AS CHARACTER NO-UNDO.

  DEFINE BUFFER TT_pdf_font FOR TT_pdf_font.

  DEFINE VARIABLE cFont AS CHARACTER   NO-UNDO.

  &IF DEFINED(trace) &THEN RUN debugTrace(QUOTER(pdfStream) + "," + QUOTER(pdfFill) + "," + QUOTER(pdfText) + "," + QUOTER(pdfOptions)). &ENDIF

  IF NOT CAN-FIND(FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream) THEN {pdferror.i &msg="'Cannot find Stream!'" &return=YES &error=NO}.

  CREATE TT_pdf_FillTxt.
  ASSIGN TT_pdf_FillTxt.obj_stream   = pdfStream.
  ASSIGN TT_pdf_FillTxt.page_nbr     = pdf_Page(pdfStream)
         TT_pdf_FillTxt.fill_from    = pdfFill
         TT_pdf_FillTxt.fill_to      = pdfText
         TT_pdf_FillTxt.fill_options = pdfOptions
         TT_pdf_FillTxt.in_transaction = pdf_in_transaction(pdfStream).

END. /* pdf_fill_text */

/* 23-AUG-2012 jcc: refactor */
PROCEDURE _ProcessFillText: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream   AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER L_Text      AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER L_LX        AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER L_LY        AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER L_UX        AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER L_UY        AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER L_Font      AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER L_FontSize  AS DECIMAL     NO-UNDO.
    DEFINE INPUT PARAMETER L_Align     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER L_MultiLine AS LOGICAL     NO-UNDO.
    DEFINE INPUT PARAMETER dePageWidth AS DECIMAL     NO-UNDO.
    DEFINE PARAMETER BUFFER TT_Object FOR TT_Object.
    DEFINE PARAMETER BUFFER TT_Widget FOR TT_Widget.

    DEFINE VARIABLE L_RectWidth AS DECIMAL   DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE L_Width     AS DECIMAL   DECIMALS 5 NO-UNDO.

    /* 14-AUG-2012 jcc: fix coordinates when TT_Object.Rotate = 90 */
    IF NOT L_MultiLine THEN DO:
        CASE L_Align:
          WHEN "Right" THEN DO:
            IF L_Font <> "" THEN
              L_Width = pdf_text_fontwidth2(pdfStream, L_Font, L_FontSize, L_Text).
            ELSE
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF L_Width = 0 THEN
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF TT_Object.Rotate = 90 OR TT_Object.Rotate = 270 THEN /* 03-NOV-2014 jcc: 270 */
                RUN pdf_set_TextXY (pdfStream
                                   ,L_UY - L_Width - IF TT_Widget.widget_tpos = "" THEN 2 ELSE 0
                                   ,dePageWidth - (L_UX + L_LX + L_FontSize) / 2 + 1
                                   ,YES).
            ELSE
                RUN pdf_set_TextXY (pdfStream
                                   ,L_UX - L_Width - IF TT_Widget.widget_tpos = "" THEN 2 ELSE 0
                                   ,IF TT_Widget.widget_tpos = "" THEN (L_UY + L_LY + L_FontSize) / 2 - L_FontSize + 1 ELSE L_LY
                                   ,YES).
            RUN OutputTextContent(pdfStream, 
                                  "TEXTXY",
                                  (IF TT_Widget.widget_tpos > "" THEN TT_Widget.widget_tpos + CHR(10) ELSE "")
                                  + "(",
                                  L_Text,
                                  ") Tj" {&debugComment}).
          END. /* Right */

          WHEN "Center" THEN DO:

            IF L_Font <> "" THEN
              L_Width = pdf_text_fontwidth2(pdfStream, L_Font, L_FontSize, L_Text).
            ELSE
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF L_Width = 0 THEN
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF TT_Object.Rotate = 90 OR TT_Object.Rotate = 270 THEN DO: /* 03-NOV-2014 jcc: 270 */
                L_RectWidth = L_UY - L_LY.
                RUN pdf_set_TextXY (pdfStream
                                   ,L_LY + (L_RectWidth - L_Width) / 2
                                   ,dePageWidth - (L_UX + L_LX + L_FontSize) / 2 + 1
                                   ,YES).
            END.
            ELSE DO:
                L_RectWidth = L_UX - L_LX.
                RUN pdf_set_TextXY (pdfStream
                                   ,L_LX + (L_RectWidth - L_Width) / 2
                                   ,IF TT_Widget.widget_tpos = "" THEN (L_UY + L_LY + L_FontSize) / 2 - L_FontSize + 1 ELSE L_LY
                                   ,YES).
            END.
            RUN OutputTextContent(pdfStream, 
                                  "TEXTXY",
                                  (IF TT_Widget.widget_tpos > "" THEN TT_Widget.widget_tpos + CHR(10) ELSE "")
                                  + "(",
                                  L_Text,
                                  ") Tj" {&debugComment}).
          END. /* Center */

          OTHERWISE DO:   /* Assume Left */
              IF TT_Object.Rotate = 90 OR TT_Object.Rotate = 270 THEN /* 03-NOV-2014 jcc: 270 */
                  RUN pdf_set_TextXY (pdfStream
                                     ,L_LY + IF TT_Widget.widget_tpos = "" THEN 2 ELSE 0
                                     ,dePageWidth - (L_UX + L_LX + L_FontSize) / 2 + 1
                                     ,YES).
              ELSE
                  RUN pdf_set_TextXY (pdfStream
                                     ,L_LX + IF TT_Widget.widget_tpos = "" THEN 2 ELSE 0
                                     ,IF TT_Widget.widget_tpos = "" THEN (L_UY + L_LY + L_FontSize) / 2 - L_FontSize + 1 ELSE L_LY
                                     ,YES).
              RUN OutputTextContent(pdfStream, 
                                    "TEXTXY",
                                    (IF TT_Widget.widget_tpos > "" THEN TT_Widget.widget_tpos + CHR(10) ELSE "")
                                    + "(",
                                    L_Text,
                                    ") Tj" {&debugComment}).
          END. /* Otherwise Assume Left */
        END CASE.
    END. /* IF NOT L_MultiLine */
    ELSE IF TT_Object.Rotate = 90 OR TT_Object.Rotate = 270 THEN /* 03-NOV-2014 jcc: 270 */
        RUN pdf_fill_multiline(pdfStream
                              ,L_Text
                              ,L_Font
                              ,L_FontSize
                              ,L_LY
                              ,dePageWidth - L_UX
                              ,L_UY
                              ,dePageWidth - L_LX
                              ,TT_Widget.widget_tpos
                              ,L_Align).
    ELSE
        RUN pdf_fill_multiline(pdfStream
                              ,L_Text
                              ,L_Font
                              ,L_FontSize
                              ,L_LX
                              ,L_LY
                              ,L_UX
                              ,L_UY
                              ,TT_Widget.widget_tpos
                              ,L_Align).
END. /* _ProcessFillText */

PROCEDURE pdf_fill_MultiLine:  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfText      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfFont      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfSize      AS DECIMAL     NO-UNDO.
  DEFINE INPUT PARAMETER pdfLX        AS DECIMAL     DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfLY        AS DECIMAL     DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfUX        AS DECIMAL     DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfUY        AS DECIMAL     DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfTextPos   AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlignment AS CHARACTER   NO-UNDO.

  IF pdfTextPos <> "" THEN ASSIGN
    pdfLX = pdfLX + string2dec(ENTRY(LOOKUP("Td",pdfTextPos," ") - 2, pdfTextPos, " "))
    pdfLY = pdfLY + string2dec(ENTRY(LOOKUP("Td",pdfTextPos," ") - 1, pdfTextPos, " ")).

  RUN pdf_set_font (pdfStream
                   ,IF pdfFont = "" THEN pdf_Font(pdfStream) ELSE pdfFont
                   ,pdfSize).

  /* If no position has been specified in the template (pdfTextPos = ""), then center the text vertically */
  RUN pdf_wrap_text_xy_dec (pdfStream
                           ,pdfText
                           ,pdfLX + IF pdfTextPos = "" THEN 2 ELSE 0
                           ,IF pdfTextPos = "" THEN pdfUY - pdfSize ELSE pdfLY 
                           ,pdfUX - pdfLX - IF pdfTextPos = "" THEN 4 ELSE 0
                           ,pdfUY - pdfLY
                           ,pdfSize
                           ,pdfAlignment).

END. /* pdf_Fill_Multiline */
/* 23-AUG-2012 jcc: end */

PROCEDURE ProcessFillText: /* PRIVATE */
    DEFINE INPUT PARAMETER pdfStream          AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfPage            AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER piTotalPages       AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pdfFormID          AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pdfFormPage        AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER pcFlattenedWidgets AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cFileName        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dOldX            AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dOldY            AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iFileSize        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE L_Align          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE L_Disp           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE L_Font           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE L_FontSize       AS DECIMAL     DECIMALS 2 NO-UNDO.
    DEFINE VARIABLE L_LX             AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE L_LY             AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE L_UX             AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE L_UY             AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE L_MultiLine      AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE L_Rect           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE L_Text           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE L_Width          AS DECIMAL     DECIMALS 5 NO-UNDO.
    DEFINE VARIABLE mPtrFileContents AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE cPdfValue        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cWord            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dTest            AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTfPos           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dePageWidth      AS DECIMAL     NO-UNDO.

    dOldX = pdf_TextX(pdfStream).
    dOldY = pdf_TextY(pdfStream).
    
    DEFINE BUFFER B_TT_stream    FOR TT_pdf_stream.
    DEFINE BUFFER TT_pdf_FillTxt FOR TT_pdf_FillTxt.
    DEFINE BUFFER TT_Widget      FOR TT_Widget.
    DEFINE BUFFER TT_Object      FOR TT_Object.
    
    FIND FIRST B_TT_Stream WHERE B_TT_Stream.obj_Stream = pdfStream 
       NO-ERROR.
    IF NOT AVAIL B_TT_Stream THEN RETURN.
    
    /* 16-OCT-2014 jcc: make use of pdf_set_page instead, in pdf_close */
    /* ASSIGN B_TT_Stream.obj_DoingText    = FALSE */
           /* B_TT_Stream.obj_DoingGraphic = FALSE. */
    /* OUTPUT STREAM S_pdf_out TO VALUE(_get_page_content_file(pdfStream, B_TT_stream.obj_UniqueID, pdfPage)) BINARY NO-MAP NO-CONVERT APPEND. */

    FOR EACH TT_pdf_FillTxt
       WHERE TT_pdf_FillTxt.obj_Stream = pdfStream
         AND TT_pdf_FillTxt.page_nbr   = pdfPage:

      FOR EACH TT_Widget
         WHERE TT_Widget.obj_stream  = pdfStream
           AND TT_Widget.pdf_id > ""
           AND (TT_Widget.widget_page = pdfFormPage OR TT_Widget.widget_page = 0) /* 30-OCT-2014 jcc: added "OR TT_Widget.widget_page = 0": a widget can have no /P (page) defined */
           AND TT_Widget.widget_name = TT_pdf_FillTxt.fill_from
           AND LOOKUP(TT_Widget.widget_name, pcFlattenedWidgets, CHR(1)) > 0 /* 27-MAY-2013 jcc: output only flattened widgets here */
           AND TT_Widget.widget_rect > "":

        FIND FIRST TT_Object
             WHERE TT_Object.obj_stream = pdfStream
               AND TT_Object.pdf_ID     = TT_Widget.pdf_id
               AND TT_Object.obj_type   = "Page"
               AND TT_Object.page_id    = pdfFormPage NO-ERROR.

        /* 15-OCT-2016 jcc: compute the width according to the original form rotation, then use it instead of pdf_pageWidth() */
        dePageWidth = IF TT_Object.Rotate = 90 OR TT_Object.Rotate = 270
                      THEN TT_Object.obj_Media3
                      ELSE TT_Object.obj_Media4.

        /* 13-JAN-2011 jcc: take into account european sessions! do not use 4 times string2dec. */
        L_Rect = REPLACE(TT_Widget.widget_rect, ".", SESSION:NUMERIC-DECIMAL-POINT).
        ASSIGN
         L_LX = DECIMAL(ENTRY(1,L_Rect," "))
         L_LY = DECIMAL(ENTRY(2,L_Rect," "))
         L_UX = DECIMAL(ENTRY(3,L_Rect," "))
         L_UY = DECIMAL(ENTRY(4,L_Rect," ")).

        ASSIGN
         L_Align     = ""
         L_MultiLine = FALSE
         L_Width     = 0.

        /* Define Font & Font size */
        /* 15-OCT-2014 jcc: allow the caller to specify a given font & size */
        L_Font = GetWidgetOption("font", TT_pdf_fillTxt.fill_options).
        IF TT_Widget.widget_disp <> "" THEN DO:
          /* 15-OCT-2014 jcc: rewritten */
          L_Disp = innerTrim(TT_Widget.widget_disp). /* ensure only one space separates each token */
          IF L_Font = ? THEN DO:
              iTfPos = LOOKUP("Tf", L_Disp, " ").
              IF iTfPos > 2 THEN DO: /* "/fontTag fontSize Tf" */
                  ASSIGN
                   L_Font     = ENTRY(iTfPos - 2, L_Disp, " ")
                   L_FontSize = string2dec(ENTRY(iTfPos - 1, L_Disp, " ")).
                  IF L_Font BEGINS "~/" THEN
                      L_Font = SUBSTRING(L_Font, 2). /* strip leading slash */
                  IF L_FontSize = 0 OR L_FontSize = ? THEN
                      L_FontSize = pdf_PointSize(pdfStream).
              END.
              ELSE
                  L_FontSize = pdf_PointSize(pdfStream).
          END.
          ELSE DO:
              L_FontSize = string2dec(GetWidgetOption("fontSize", TT_pdf_fillTxt.fill_options)).
              IF L_FontSize = ? THEN L_FontSize = pdf_PointSize(pdfStream).
          END.

          /* also take into account all other operators */
          DO i = 1 TO NUM-ENTRIES(L_Disp, " "):
            cWord = ENTRY(i,L_Disp," ").
            dTest = DECIMAL(cWord) NO-ERROR.
            IF ERROR-STATUS:ERROR AND SUBSTRING(cWord,1,1) <> "~/" THEN DO:
                IF cWord <> "Tf" THEN
                    RUN setTextOperator(pdfStream, cWord, SUBSTRING(cPdfValue,2)).
                cPdfValue = "".
            END.
            ELSE DO:
                cPdfValue = cPdfValue + " " + cWord.
            END.
          END.
        END. /* IF TT_Widget.widget_disp <> "" */
        ELSE
            L_FontSize = pdf_PointSize(pdfStream).

        RUN pdf_set_font (pdfStream, L_Font, L_FontSize).

        /* 21-AUG-2012 jcc: draw a box around form fields */
        DEFINE VARIABLE cDrawFormFieldRect AS CHARACTER   NO-UNDO.
        cDrawFormFieldRect = pdf_get_parameter(pdfStream, "drawFormFieldRect").
        IF cDrawFormFieldRect > "" THEN DO:
            RUN pdf_stroke_color(pdfStream,ENTRY(1,cDrawFormFieldRect),ENTRY(2,cDrawFormFieldRect),ENTRY(3,cDrawFormFieldRect)).
            RUN pdf_stroke_fill(pdfStream,1,1,1).
            IF TT_Object.Rotate = 90 OR TT_Object.Rotate = 270 THEN RUN pdf_rectdec(pdfStream, L_LY, dePageWidth - L_UX, L_UY - L_LY, L_UX - L_LX, .5). /* 03-NOV-2014 jcc: 270 */
            ELSE RUN pdf_rectdec(pdfStream, L_LX, L_LY, L_UX - L_LX, L_UY - L_LY, .5).
        END.

        CASE TT_Widget.widget_type:
            WHEN "~/Tx" THEN DO:
                L_Text = TT_pdf_FillTxt.Fill_to.
                IF INDEX(L_Text,"@@TotalPages-" + pdfStream) > 0 THEN
                  L_Text = REPLACE(L_Text,"@@TotalPages-" + pdfStream, STRING(piTotalPages)).
                IF INDEX(L_Text,"@@PageNo-" + pdfStream) > 0 THEN
                  L_Text = REPLACE(L_Text,"@@PageNo-" + pdfStream, STRING(pdfPage)).

                /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
                /* RUN pdf_replace_text (pdfStream, L_Text, OUTPUT L_TextOut). */

                L_MultiLine = ?.
                IF TT_pdf_FillTxt.fill_option <> "" THEN DO:
                  L_Align = GetWidgetOption("align", TT_pdf_fillTxt.fill_options).
                  IF GetWidgetOption("multiline", TT_pdf_fillTxt.fill_options) = "YES" THEN
                      L_MultiLine = TRUE.
                  ELSE IF GetWidgetOption("multiline", TT_pdf_fillTxt.fill_options) = "NO" THEN
                      L_MultiLine = FALSE.
                END. /* Options */
                /* 20-AUG-2012 jcc: use the justification (/Q) defined in the template */
                IF L_Align = "" OR L_Align = ? THEN DO:
                    CASE TT_Widget.widget_justif:
                        WHEN "0" THEN L_Align = "LEFT".   /* Will be the default value */
                        WHEN "1" THEN L_Align = "CENTER".
                        WHEN "2" THEN L_Align = "RIGHT".
                    END CASE.
                END.
                /* 21-AUG-2012 jcc: if multiline was not forced by an option, use the value specified in the template */
                IF L_MultiLine = ? THEN
                    L_MultiLine = TT_Widget.widget_multiline.

                RUN _ProcessFillText(pdfStream
                                    ,L_Text
                                    ,L_LX
                                    ,L_LY
                                    ,L_UX
                                    ,L_UY
                                    ,L_Font
                                    ,L_FontSize
                                    ,L_Align
                                    ,L_MultiLine
                                    ,dePageWidth
                                    ,BUFFER TT_Object
                                    ,BUFFER TT_Widget).
            END. /* WHEN "~/Tx" */

            WHEN "~/Btn,checkbox" OR WHEN "~/Btn,radio" THEN DO:
                /* Copy the correct widget appearance to the pdf */
                cFileName = IF TT_pdf_FillTxt.Fill_to = TT_Widget.widget_on_value
                               OR (TT_Widget.widget_type = "~/Btn,checkbox" AND TT_pdf_FillTxt.Fill_to = "YES")
                            THEN TT_Widget.widget_on ELSE TT_Widget.widget_off.
                IF cFileName = "" THEN DO:
                    /* 25-MAY-2013 jcc: if there is no corresponding appeearance, then do nothing */
                    /* {pdferror.i &msg="'Widget' + QUOTER(TT_Widget.widget_name) + ' has no /Yes nor /Off appearance!'" &return=NO &error=NO &returnmsg=NO}. */
                    NEXT.
                END.
                RUN _pdf_close_output_context (pdfStream). /* 04-AUG-2016 jcc */
                /* Save the graphic state */
                PUT STREAM S_pdf_out UNFORMATTED "q" {&pdfSKIP}.
                /* Set the matrix, for correct positionning of the appearance */
                PUT STREAM S_pdf_out UNFORMATTED "1 0 0 1 " + dec2string(L_LX) + " " + dec2string(L_LY) + " cm" {&pdfSKIP}.
                /* Append the appearance stream */
                RUN getFileAsMemptr (pdfStream, cFileName, ?, INPUT-OUTPUT mPtrFileContents, OUTPUT iFileSize).
                EXPORT STREAM S_pdf_out mPtrFileContents.
                /* 03-NOV-2014 jcc: ensure we finish in a carriage return to separate the "Q" from the appearance stream */
                DEFINE VARIABLE iLastByte AS INTEGER     NO-UNDO.
                iLastByte = GET-BYTE(mPtrFileContents, iFileSize).
                SET-SIZE(mPtrFileContents) = 0.
                IF iLastByte <> 10 AND iLastByte <> 13 THEN
                    PUT STREAM S_pdf_out UNFORMATTED {&pdfSKIP}.
                /* Restore the graphic state */
                PUT STREAM S_pdf_out UNFORMATTED "Q" {&pdfSKIP}.
            END.

            WHEN "~/Ch,combo" THEN DO:
                /* Take default value with index, or take the value as free text */
                IF TT_pdf_FillTxt.fill_option <> "" AND GetWidgetOption("text", TT_pdf_fillTxt.fill_options) = "YES" THEN
                    L_Text = TT_pdf_FillTxt.Fill_to.
                ELSE
                    L_Text = ENTRY(INTEGER(TT_pdf_FillTxt.Fill_to), TT_Widget.widget_values, CHR(10)).
                    /* 02-OCT-2014 jcc: now done in _PutStreamContent_flush */
                    /* RUN pdf_replace_text (pdfStream, L_Text, OUTPUT L_TextOut). */

                IF TT_pdf_FillTxt.fill_option <> "" THEN
                    L_Align = GetWidgetOption("align", TT_pdf_fillTxt.fill_options).
                IF L_Align = "" OR L_Align = ? THEN DO:
                    CASE TT_Widget.widget_justif:
                        WHEN "0" THEN L_Align = "LEFT".   /* Will be the default value */
                        WHEN "1" THEN L_Align = "CENTER".
                        WHEN "2" THEN L_Align = "RIGHT".
                    END CASE.
                END.
                RUN _ProcessFillText(pdfStream
                                    ,L_Text
                                    ,L_LX
                                    ,L_LY
                                    ,L_UX
                                    ,L_UY
                                    ,L_Font
                                    ,L_FontSize
                                    ,L_Align
                                    ,L_MultiLine
                                    ,dePageWidth
                                    ,BUFFER TT_Object
                                    ,BUFFER TT_Widget).
            END. /* WHEN "~/Ch,combo" */

            WHEN "~/Ch,list" THEN DO:
                DEFINE VARIABLE cOld       AS CHARACTER   NO-UNDO.
                DEFINE VARIABLE cValues    AS CHARACTER   NO-UNDO.
                DEFINE VARIABLE cValuesIdx AS CHARACTER   NO-UNDO.
                DEFINE VARIABLE iDelta     AS INTEGER     NO-UNDO.
                DEFINE VARIABLE iNbLines   AS INTEGER     NO-UNDO.
                DEFINE VARIABLE iNbVal     AS INTEGER     NO-UNDO.
                DEFINE VARIABLE iVal1      AS INTEGER     NO-UNDO.
                DEFINE VARIABLE iVal2      AS INTEGER     NO-UNDO.

                IF TT_pdf_FillTxt.fill_option <> "" THEN
                    L_Align = GetWidgetOption("align", TT_pdf_fillTxt.fill_options).
                IF L_Align = "" OR L_Align = ? THEN DO:
                    CASE TT_Widget.widget_justif:
                        WHEN "0" THEN L_Align = "LEFT".   /* Will be the default value */
                        WHEN "1" THEN L_Align = "CENTER".
                        WHEN "2" THEN L_Align = "RIGHT".
                    END CASE.
                END.
                /* How many lines are displayed within the list box? */
                iNbLines = TRUNCATE((L_UY - L_LY) / L_FontSize, 0).

                cValuesIdx = TT_pdf_FillTxt.Fill_to.
                IF cValuesIdx > "" THEN DO:
                    /* Order the list of items' indices to select */
                    iNbVal = NUM-ENTRIES(cValuesIdx).
                    IF iNbVal > 1 THEN DO WHILE cOld <> cValuesIdx:
                        cOld = cValuesIdx.
                        DO i = NUM-ENTRIES(cValuesIdx) TO 2 BY -1:
                            iVal1 = INTEGER(ENTRY(i - 1, cValuesIdx)).
                            iVal2 = INTEGER(ENTRY(i, cValuesIdx)).
                            IF iVal1 > iVal2 THEN DO:
                                ENTRY(i - 1, cValuesIdx) = STRING(iVal2).
                                ENTRY(i, cValuesIdx)     = STRING(iVal1).
                            END.
                        END.
                    END.
                    ELSE
                        cOld = cValuesIdx.
                    /* Truncate the list to what will be seen on the pdf */
                    cValuesIdx = "".
                    iVal1 = INTEGER(ENTRY(1, cOld)).
                    cValuesIdx = STRING(iVal1).
                    DO i = 2 TO iNbVal:
                        iVal2 = INTEGER(ENTRY(i, cOld)).
                        IF iVal2 - iVal1 > iNbLines - 1 THEN LEAVE.
                        cValuesIdx = cValuesIdx + "," + STRING(iVal2).
                    END.
                    /* Build the list of values to display */
                    iNbVal = INTEGER(ENTRY(NUM-ENTRIES(cValuesIdx), cValuesIdx)).
                    cValues = "".
                    DO i = iVal1 TO iNbVal:
                        cValues = cValues + CHR(10) + ENTRY(i, TT_Widget.widget_values, CHR(10)).
                    END.
                    cValues = SUBSTRING(cValues, 2).
                    IF NUM-ENTRIES(cValues, CHR(10)) < iNbLines THEN DO:
                        DO i = iNbVal + 1 TO NUM-ENTRIES(TT_Widget.widget_values, CHR(10)):
                            cValues = cValues + CHR(10) + ENTRY(i, TT_Widget.widget_values, CHR(10)).
                        END.
                    END.
                    IF NUM-ENTRIES(cValues, CHR(10)) < iNbLines THEN DO:
                        i = INTEGER(ENTRY(1, cValuesIdx)) - 1.
                        iDelta = 0.
                        DO WHILE i > 0 AND NUM-ENTRIES(cValues, CHR(10)) < iNbLines:
                            cValues = ENTRY(i, TT_Widget.widget_values, CHR(10)) + CHR(10) + cValues.
                            i = i - 1.
                            iDelta = iDelta + 1.
                        END.
                    END.
                    /* Highlight the shown, selected values */
                    RUN pdf_stroke_color(pdfStream, .8, .8, 1).
                    RUN pdf_stroke_fill(pdfStream, .8, .8, 1).
                    iNbVal = NUM-ENTRIES(cValuesIdx).
                    iVal1 = INTEGER(ENTRY(1, cValuesIdx)).
                    DO i = 1 TO iNbVal:
                        iVal2 = INTEGER(ENTRY(i, cValuesIdx)).
                        RUN pdf_rectdec(pdfStream, L_LX + 1, L_UY - L_FontSize * (iVal2 - iVal1 + 1 + iDelta) - 1, L_UX - L_LX - 2, L_FontSize, 0).
                    END.
                END.
                ELSE /* TT_pdf_FillTxt.Fill_to = "" */
                    cValues = TT_Widget.widget_values.

                /* Finally, write the visible values within the list box */
                RUN _ProcessFillText(pdfStream
                                    ,cValues
                                    /* ,cValues */
                                    ,L_LX
                                    ,L_LY
                                    ,L_UX
                                    ,L_UY
                                    ,L_Font
                                    ,L_FontSize
                                    ,L_Align
                                    ,YES
                                    ,dePageWidth
                                    ,BUFFER TT_Object
                                    ,BUFFER TT_Widget).
            END. /* WHEN "~/Ch,list" */
        END CASE. /* CASE TT_Widget.widget_type */
      END. /* Each widget */
    END. /* each TT_pdf_FillTxt */

    RUN pdf_set_TextXY (pdfStream, dOldX, dOldY, NO).

    /* 16-OCT-2014 jcc: commented out, done in pdf_close */
    /* IF L_FoundWidget THEN */
      /* PUT STREAM S_pdf_out {&pdfSKIP} "ET" {&pdfSKIP}. */
    /* OUTPUT STREAM S_pdf_out CLOSE. */
    /* ASSIGN B_TT_Stream.obj_DoingText    = FALSE */
           /* B_TT_Stream.obj_DoingGraphic = FALSE. */

END. /* ProcessFillText */

/* jcc: return information about form widgets (useful for debug) */
PROCEDURE pdf_get_widgets:
    DEFINE INPUT  PARAMETER pdfStream AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcWidgets AS CHARACTER   NO-UNDO.

    DEFINE BUFFER TT_Widget FOR TT_Widget.

    FOR EACH TT_Widget WHERE TT_Widget.obj_stream = pdfStream :
        pcWidgets = pcWidgets + CHR(1) + TT_Widget.widget_name  + CHR(2) + STRING(TT_Widget.widget_page)
                              + CHR(2) + TT_Widget.widget_type  + CHR(2) + TT_Widget.widget_rect
                              + CHR(2) + TT_Widget.widget_value + CHR(2) + TT_Widget.widget_values
                              + CHR(2) + STRING(TT_Widget.widget_page)
                              .
    END.
    pcWidgets = SUBSTRING(pcWidgets, 2).
END PROCEDURE. /* pdf_get_widgets */

/* end of pdf_inc.p */
