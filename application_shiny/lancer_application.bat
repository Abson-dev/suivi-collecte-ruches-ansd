@echo off
REM ==========================================================================================
REM  Application Shiny - Suivi de la collecte - Ruches d'abeilles (ANSD)
REM  Double-cliquer sur ce fichier : l'application s'ouvre dans le navigateur.
REM  Garder cette fenetre ouverte pendant l'utilisation ; la fermer pour arreter l'application.
REM ==========================================================================================
setlocal
chcp 65001 >nul
cd /d "%~dp0"

REM --- Recherche de R ---
set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"
if not defined RSCRIPT (
    for /d %%D in ("%ProgramFiles%\R\R-*" "%LOCALAPPDATA%\Programs\R\R-*") do (
        if exist "%%~D\bin\Rscript.exe" set "RSCRIPT=%%~D\bin\Rscript.exe"
    )
)
if not defined RSCRIPT (
    echo.
    echo R est introuvable sur cet ordinateur.
    echo Installez R depuis https://cran.r-project.org/bin/windows/base/ puis relancez ce fichier.
    pause
    exit /b 1
)

REM --- Packages (installes automatiquement au premier lancement) ---
"%RSCRIPT%" installer_packages.R
if errorlevel 1 (
    echo.
    echo ECHEC de l'installation des packages R. Voir la rubrique Depannage de LISEZMOI.txt
    pause
    exit /b 1
)

REM --- Lancement ---
echo.
echo Ouverture de l'application dans le navigateur...
echo Fermez cette fenetre pour arreter l'application.
echo.
"%RSCRIPT%" -e "shiny::runApp('.', launch.browser = TRUE)"
pause
