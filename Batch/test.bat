@echo off
echo Avant saisie
ping 127.0.0.1 -n 1 >nul
set /p "rep=Entrer une valeur : "
echo Vous avez saisi %rep%
pause
