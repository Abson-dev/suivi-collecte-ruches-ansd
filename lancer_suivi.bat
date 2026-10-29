@echo off
REM ==========================================================================================
REM  Suivi de la collecte - Enquete complementaire et productivite des ruches d'abeilles (ANSD)
REM  Double-cliquer sur ce fichier. Les exports Stata de Survey Solutions doivent se trouver
REM  dans ce meme dossier : PROD_ECPRA_BMD_<n>_STATA_All (dossier ou fichier .zip).
REM  Les resultats sont ecrits dans le sous-dossier "sorties".
REM ==========================================================================================
setlocal
chcp 65001 >nul
cd /d "%~dp0"
set "ENV=ruches-ansd"
set "CONDA="

REM --- Recherche de Miniforge / Miniconda / Anaconda ---
if defined CONDA_EXE for %%I in ("%CONDA_EXE%") do if exist "%%~dpI..\condabin\conda.bat" set "CONDA=%%~dpI..\condabin\conda.bat"
for %%D in ("%USERPROFILE%\miniforge3" "%LOCALAPPDATA%\miniforge3" "%ProgramData%\miniforge3" "%USERPROFILE%\miniconda3" "%LOCALAPPDATA%\miniconda3" "%ProgramData%\miniconda3" "%USERPROFILE%\anaconda3" "%ProgramData%\anaconda3") do (
    if not defined CONDA if exist "%%~D\condabin\conda.bat" set "CONDA=%%~D\condabin\conda.bat"
)
if not defined CONDA (
    echo.
    echo Miniforge est introuvable sur cet ordinateur.
    echo Installez-le depuis https://conda-forge.org/download/ puis relancez ce fichier.
    echo Voir LISEZMOI.txt
    pause
    exit /b 1
)

REM --- Premiere utilisation : creation de l'environnement Python du projet ---
call "%CONDA%" run -n %ENV% python -c "import pandas, pyreadstat, openpyxl" >nul 2>&1
if errorlevel 1 (
    echo.
    echo Premiere utilisation : installation de l'environnement %ENV%.
    echo Cela prend quelques minutes et necessite une connexion internet, une seule fois.
    echo.
    call "%CONDA%" env remove -n %ENV% -y >nul 2>&1
    call "%CONDA%" env create -f environment.yml -n %ENV%
    if errorlevel 1 (
        echo.
        echo ECHEC de l'installation de l'environnement. Voir la rubrique Depannage de LISEZMOI.txt
        pause
        exit /b 1
    )
)

REM --- Traitement ---
echo.
echo Traitement des exports en cours...
echo.
call "%CONDA%" run -n %ENV% --no-capture-output python -X utf8 -W ignore programmes\suivi_collecte.py
if errorlevel 1 (
    echo.
    echo Le traitement a echoue : voir le message ci-dessus et la rubrique Depannage de LISEZMOI.txt
) else (
    echo.
    echo Termine. Les fichiers sont dans le dossier "sorties".
)
pause
