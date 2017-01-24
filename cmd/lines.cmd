@echo off
setlocal enableextensions

set "ods_root=c:/denis/denis_core/odysseus"

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  racket %ods_root%/cmd/lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" -d %ods_root%
) else (
  racket %ods_root%/cmd/lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" -d %1
)

endlocal
