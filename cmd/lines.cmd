@echo off
setlocal enableextensions

set "cmd_root=c:/denis/denis_core/odysseus/cmd"

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  racket %cmd_root%/lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" ..
) else (
  racket %cmd_root%/lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" %1
)



endlocal
