@echo off
setlocal enableextensions

set "ods_root=c:/denis/denis_core/odysseus"

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  echo too few arguments, please, use -q key
) else if %arglen% == 1 (
  racket %ods_root%/cmd/pwd.rkt %1
) else (
  racket %ods_root%/cmd/pwd.rkt %1 %2
)

endlocal
