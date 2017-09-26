@echo off
setlocal enableextensions

set "ods_root=c:/denis/denis_core/odysseus"

racket %ods_root%/cmd/todaybd.rkt %*

endlocal
