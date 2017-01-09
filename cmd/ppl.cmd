@echo off
setlocal enableextensions

set "ods_root=c:/denis/denis_core/odysseus"

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  echo please, use -q key
) else (
  racket %ods_root%/cmd/ppl.rkt %1 %2
)

endlocal
