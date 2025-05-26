@echo off
>nul break off

REM # Données
	set "CHEMIN_DOSSIER=%~dp0"
	set "FICHIER_OPTIONS=%CHEMIN_DOSSIER%minmax_options.txt"

REM # Fonctions utilitaires
REM ## Lecture d'un entier utilisateur sécurisé
:lire_entier
	setlocal EnableDelayedExpansion
:le_loop
	set "nonChiffre="
	echo !promptTexte!
	set /p saisie=
	for /f "delims=0123456789" %%C in ("!saisie!") do set nonChiffre=%%C
	if defined nonChiffre goto mauvais
	set /a nombre=!saisie! 2>nul
	if !nombre! GEQ %min% if !nombre! LEQ %max% (
		endlocal & set lireEntierRet=!nombre! & exit /b
	)
:mauvais
	echo.
	echo Veuillez entrer un entier entre !min! et !max!.
	goto le_loop

REM ## Chargement des options sauvegardées
:charger_options
	echo "début de charger_options"
	if exist "%FICHIER_OPTIONS%" (
		for /f "tokens=1-3 delims=," %%A in (%FICHIER_OPTIONS%) do (
			set min=%%A
			set max=%%B
			set tours=%%C
		)
	)
	if not defined min (
		set min=1
		set max=100
		set tours=5
	)
	echo "fin de charger_options"
	exit /b

REM ## Sauvegarde des options
:sauvegarder_options
	(echo %1,%2,%3) > "%FICHIER_OPTIONS%"
	exit /b

REM # Fonctions principales
REM ## Affiche le menu principal
:afficher_menu_principal
	echo.
	echo #### Menu ####
	echo 1 : Jouer
	echo 2 : Options
	echo 3 : Quitter
	exit /b

REM ## Affiche le menu d'options
:afficher_menu_options
	echo.
	echo #### Menu options ####
	echo 1 : Choisir les limites (actuellement : %1 - %2)
	echo 2 : Nombre de tours max (actuellement : %3)
	exit /b

REM ## Joue une partie, indique si plus grand/petit
:jouer_partie
	setlocal EnableDelayedExpansion
	set minLoc=%1 & set maxLoc=%2 & set toursLoc=%3
	set /a intervalle=maxLoc-minLoc+1
	set /a cible=%RANDOM% %% intervalle + minLoc
	echo.
	echo Trouvez entre %minLoc% et %maxLoc% en %toursLoc% tours
	for /l %%N in (1,1,%toursLoc%) do (
		call :lire_entier "Tour %%N : " %minLoc% %maxLoc%
		set choix=!lireEntierRet!
		if !choix! EQU !cible! (echo.& echo Gagné !& endlocal & exit /b)
		if !choix! GTR !cible! (echo -) else (echo +)
	)
	echo.
	echo Perdu ! La réppnse était !cible!.
	endlocal & exit /b

REM ## Gère les options et sauvegarde si modifiées
:gerer_options
	setlocal EnableDelayedExpansion
	call :afficher_menu_options %1 %2 %3
	call :lire_entier "? = " 1 2
	set choixOpt=!lireEntierRet!
	set minOpt=%1 & set maxOpt=%2 & set toursOpt=%3
	if !choixOpt! EQU 1 (
		call :lire_entier "Min = " 1 !maxOpt!
		set minOpt=!lireEntierRet!
		call :lire_entier "Max = " !minOpt! 10000
		set maxOpt=!lireEntierRet!
	) else if !choixOpt! EQU 2 (
		call :lire_entier "Tours max = " 1 100
		set toursOpt=!lireEntierRet!
	)
	call :sauvegarder_options !minOpt! !maxOpt! !toursOpt!
	endlocal & (
		set min=!minOpt!
		set max=!maxOpt!
		set tours=!toursOpt!
	)
	exit /b

REM # Main
:main
	echo "début de main"
	call :charger_options
:boucle_menu
	call :afficher_menu_principal
	call :lire_entier "? = " 1 3

	if "!lireEntierRet!"=="1" (
		call :jouer_partie %min% %max% %tours%
	) else if "!lireEntierRet!"=="2" (
		call :gerer_options %min% %max% %tours%
	) else if "!lireEntierRet!"=="3" (
		goto fin
	)
	goto boucle_menu

REM # Lancement du programme
call :main
:fin
exit /b
