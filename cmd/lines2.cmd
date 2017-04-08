@echo off
setlocal enableextensions

set "ods_root=c:/denis/denis_core/odysseus"

REM racket %ods_root%/cmd/lines.rkt -e "(rkt|php|c|cpp|h|js|cmd|java|py|rb|pl|lsp|nl)" -i "^\s*$|^\s*;+.*$|^\s*#+.*$|^\s*(//)+.*$" -d %1

REM lisp:
racket %ods_root%/cmd/lines.rkt -e "(rkt|lsp|nl|ody)" -i "^\s*$|^\s*;+.*$" -d %1

REM racket %ods_root%/cmd/lines.rkt -e %2 -i "^\s*$|^\s*;+.*$" -d %1

endlocal
