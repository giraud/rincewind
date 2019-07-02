@ECHO off
@SETLOCAL
@SET ESY__NODE_BIN_PATH=%U:\reason\rincewind\_esy\default\bin%
"T:\opt\nodejs-8.11.3\node.exe" -r "U:\reason\rincewind\_esy\default\pnp.js" %*
            