@echo off
setlocal enableextensions

REM SET arglen=0
REM FOR %%X IN (%*) DO SET /A arglen+=1
REM
REM if %arglen% == 0 (
REM   echo build: no arguments
REM else if %arglen% == 1 (
REM   racket ../utils/build.rkt %1
REM else if %arglen% == 2 (
REM   racket ../utils/build.rkt -o %2 %1
REM else (
REM   racket ../utils/build.rkt %*
REM   REM echo build: wrong number of arguments
REM

racket %odysseus%/cmd/build-svg.rkt %*

endlocal
