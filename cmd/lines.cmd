@echo off
setlocal enableextensions

racket lines.rkt -e "rkt" -i "^\s*$|^\s*;+.*$" ..

endlocal