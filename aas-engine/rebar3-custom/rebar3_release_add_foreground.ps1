$RelFile = Get-ChildItem -Path ".\_build" -Recurse -Include *.cmd -Exclude *-*.*.*.cmd

$Stage1 = (Get-Content($RelFile)) -replace "if `"%1`"==`"console`" goto console", "$&`nif `"%1`"==`"foreground`" goto foreground"
$Stage2 = $Stage1 -replace ":: Ping the running node", @"
:: Start as foreground process
:foreground
set boot=-boot "%boot_script%" -boot_var RELEASE_DIR "%release_root_dir%"
%erl% %boot% %sys_config%  ^
       -args_file "%vm_args%"
goto :eof

$&
"@

$Stage2 | Out-File -FilePath $RelFile -Encoding ASCII

"===> 'Foreground' mode added to release" | Write-Output

