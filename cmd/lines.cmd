@echo off
setlocal enableextensions

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

if %arglen% == 0 (
  racket lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" ..
) else (
  racket lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" %1
)



endlocal
