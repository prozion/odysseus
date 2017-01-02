@echo off
setlocal enableextensions

set "ods_root=c:/denis/denis_core/odysseus"

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  echo which resource do you mean?
) else (
  racket %ods_root%/utils/pwd.rkt %1
  echo color 2b hi
  color
)

endlocal
