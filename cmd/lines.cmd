@echo off
setlocal enableextensions

racket lines.rkt -e "rkt" -F "_test" -i "^\s*$|^\s*;+.*$" ..

endlocal
