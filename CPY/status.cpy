      ***---
       STATUS-BAR-MSG.
           evaluate true
           when STATUSEDIT
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "EDITING"
           when STATUSVIEW
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "VIEWING"
                move 1 to StatusHelp
                perform STATUS-HELP
           when STATUSINS
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "INS"
           when other
                modify form1-st-1-handle, 
                       panel-index  2,
                       panel-text   spaces
           end-evaluate.

      ***---
       STATUS-HELP.
           if StatusHelp = 1
              modify Form1-St-1-Handle, 
                     panel-index = 2, 
                     panel-text "F8 PICK records"
              move BitmapZoomEnabled to BitmapNumZoom
           else
              modify Form1-St-1-Handle, 
                     panel-index = 2, 
                     panel-text "F9 Select other record"
              move BitmapZoomDisabled to BitmapNumZoom
           end-if.
           
           move StatusHelp to e-search.
           modify tool-search, enabled = e-search.
           modify tool-search, bitmap-number = BitmapNumZoom.
