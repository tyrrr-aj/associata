@echo off
setlocal
set rebarscript=%~f0
escript.exe "%rebarscript:.cmd=%" %*

if %* == release (
	powershell -File ^"%~dp0rebar3_release_add_foreground.ps1^"
)
