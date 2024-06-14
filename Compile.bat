@echo off
setlocal

set js=PlaceNotes.js
set min=PlaceNotes.min.js

call elm make src/Main.elm --optimize --output=%js%

call uglifyjs %js% --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output %min%

REM for %%I in (%js%) do set size=%%~zI
REM echo Initial size: %size% bytes  (%js%)

REM for %%I in (%min%) do set size=%%~zI
REM echo Minified size: %size% bytes  (%min%)

REM powershell.exe -nologo -noprofile -command "& {Compress-Archive -Path '%min%' -DestinationPath '%min%.zip' -CompressionLevel Optimal -Force}"
REM for %%I in (%min%.zip) do set size=%%~zI
REM echo Zipped size: %size% bytes

pause
endlocal

PS C:\My Files\Nux-Git\nbrader.com-place-notes> .\Compile.bat
Success!

    Main ---> PlaceNotes.js

'uglifyjs' is not recognized as an internal or external command,
operable program or batch file.
PS C:\My Files\Nux-Git\nbrader.com-place-notes>