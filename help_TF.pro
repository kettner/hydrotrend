;**********************************************************************
;   help_TF.pro

;   Copyright (c) 2007, Scott D. Peckham 
;   Created:   October 2007

;**********************************************************************

;   Show_HTML_Help

;**********************************************************************
pro Show_HTML_Help, html_file

;---------------------------------------
;Construct possible paths to help files
;---------------------------------------
OS        = strupcase(!version.os)
OS_FAMILY = strupcase(!version.os_family)
if (OS_FAMILY eq 'WINDOWS') then begin
    base  = 'C:\Program Files\'
    paths = ['TopoFlow\Help\', 'TopoFlow\help\', 'topoflow\help\', $
             'RIVIX\RiverTools_3.0\basins\TopoFlow\help\', $
             'RiverTools_3.0\basins\TopoFlow\help\', $
             'RIVIX\RiverTools_3.0\basins\tf\help\', $
             'RiverTools_3.0\basins\tf\help\']
endif else begin
    ;-------------------------------------
    ;Most likely that OS_FAMILY eq 'UNIX'
    ;-------------------------------------
    if (OS eq 'DARWIN') then base = '/Applications/' $
                        else base = '/usr/local/'
    paths = ['TopoFlow/Help/', 'TopoFlow/help/', 'topoflow/help/', $
             'RIVIX/RiverTools_3.0/basins/TopoFlow/help/', $
             'RiverTools_3.0/basins/TopoFlow/help/', $
             'RIVIX/RiverTools_3.0/basins/tf/help/', $
             'RiverTools_3.0/basins/tf/help/']
endelse
paths = (base + paths)

;-------------------------------
;Try to find the help directory
;-------------------------------
dir_found=0b  &  k=0L  &  np=n_elements(paths)
while NOT(dir_found) AND (k le (np-1L)) do begin
    help_path = paths[k]
    dir_found = file_test(help_path, /directory)
    k = (k + 1L)
endwhile

help_file = (help_path + html_file)

;-------------------------------
;This routine doesn't exist yet
;-------------------------------
;Get_Path_Separators, sep
;help_file = (directory + sep + html_file)

;-------------------------------
;Try to find the HTML help file
;-------------------------------
FOUND = File_Test(help_file)
if NOT(FOUND) then begin
    msg = [ $
    'WARNING, ', ' ', $
    'Could not find the TopoFlow help system file: ', $
    ' ', $
    help_file, $
    ' ', $
    'TopoFlow may not be installed correctly. ', $
    ' ']
    result = GUI_Message(msg, /INFO)
    RETURN
endif

;------------------------
;Show the HTML help page
;------------------------
online_help, book=help_file, /full_path

end;  Show_HTML_Help
;**********************************************************************

