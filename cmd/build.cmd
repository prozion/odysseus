@echo off
setlocal enableextensions

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  echo build: no arguments
) else if %arglen% == 1 (
  racket ../utils/build.rkt %1
) else if %arglen% == 2 (
  racket ../utils/build.rkt -o %2 %1
) else (
  racket ../utils/build.rkt %*
  REM echo build: wrong number of arguments
)

endlocal
