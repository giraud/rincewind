@ECHO OFF
@SETLOCAL
"E:\tools\node-v8.15.1-win-x64\node_modules\esy\_build\default\bin\esy.exe" exec-command --include-npm-bin --include-current-env --include-build-env --project "E:\sources\reason\rincewind" %*
