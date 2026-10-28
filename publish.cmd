@echo off
rem Runs publish.ps1 without changing the PowerShell execution policy.
rem Pass the same options, e.g.  publish.cmd -WhatIf
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0publish.ps1" %*
