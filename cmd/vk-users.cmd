@echo off
setlocal enableextensions

REM set "ods_root=c:/denis/denis_core/odysseus"
REM racket %ods_root%/cmd/vk.rkt %*

racket %odysseus%/cmd/vk-users.rkt %*

endlocal
