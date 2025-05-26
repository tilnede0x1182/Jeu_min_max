@echo off

ping 127.0.0.1 -n 1 >nul

REM # Données
	set "CHEMIN_DOSSIER=%~dp0"
	set "FICHIER_OPTIONS=%CHEMIN_DOSSIER%minmax_options.txt"

REM # Lancement du programme - DOIT ETRE AU DEBUT
call :main
goto fin

REM # Fonctions utilitaires
REM ## Lecture d'un entier utilisateur sécurisé
:lire_entier
	set "promptTexte=%~1"
	set "minVal=%~2"
	set "maxVal=%~3"
:le_loop
	set "nonChiffre="
	set /p saisie=%promptTexte%
	if "%saisie%"=="" (
		goto mauvais
	)
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre (
		goto mauvais
	)
	set /a nombre=%saisie% 2>nul
	if %nombre% GEQ %minVal% if %nombre% LEQ %maxVal% (
		set lireEntierRet=%nombre%
		exit /b
	)
:mauvais
	echo.
	echo Veuillez entrer un entier entre %minVal% et %maxVal%.
	echo.
	goto le_loop

REM ## Chargement des options sauvegardées
:charger_options
	if exist "%FICHIER_OPTIONS%" (
		for /f "usebackq tokens=1-3 delims=," %%A in ("%FICHIER_OPTIONS%") do (
			set min=%%A
			set max=%%B
			set tours=%%C
		)
	) else (
		echo [DEBUG] Fichier options inexistant
	)
	if not defined min (
		@REM echo [DEBUG] Variables non definies, utilisation des valeurs par defaut
		set min=1
		set max=100
		set tours=5
	)
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
	set /a cible=!RANDOM! %% !intervalle! + !minLoc!
	echo.
	echo Trouvez entre !minLoc! et !maxLoc! en !toursLoc! tours
	for /l %%N in (1,1,!toursLoc!) do (
		call :lire_entier "Tour %%N : " !minLoc! !maxLoc!
		set choix=!lireEntierRet!
		if !choix! EQU !cible! (echo.& echo Gagné^^ ^^!& endlocal & exit /b)
		if !choix! GTR !cible! (echo -) else (echo +)
	)
	echo.
	echo Perdu^^ ^^! La reponse etait !cible!.
	endlocal & exit /b

REM ## Lecture d'un entier avec réaffichage du menu en cas d'erreur
:lire_entier_avec_menu
	set "menuFunction=%~1"
	set "promptTexte=%~2"
	set "minVal=%~3"
	set "maxVal=%~4"
	set "param1=%~5"
	set "param2=%~6"
	set "param3=%~7"
:lemenu_loop
	call :%menuFunction% %param1% %param2% %param3%
	set "nonChiffre="
	set /p saisie=%promptTexte%
	if "%saisie%"=="" goto lemenu_mauvais
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre goto lemenu_mauvais
	set /a nombre=%saisie% 2>nul
	if %nombre% GEQ %minVal% if %nombre% LEQ %maxVal% (
		set lireEntierRet=%nombre%
		exit /b
	)
:lemenu_mauvais
	echo.
	echo Veuillez entrer un entier entre %minVal% et %maxVal%.
	echo.
	goto lemenu_loop

REM ## Gère les options et sauvegarde si modifiées
:gerer_options
	setlocal EnableDelayedExpansion
	set minOpt=%1 & set maxOpt=%2 & set toursOpt=%3
	call :lire_entier_avec_menu afficher_menu_options "? = " 1 2 !minOpt! !maxOpt! !toursOpt!
	set choixOpt=!lireEntierRet!
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
	endlocal & set min=%minOpt% & set max=%maxOpt% & set tours=%toursOpt%
	exit /b

REM # Main
:main
	call :charger_options
:boucle_menu
	call :lire_entier_avec_menu afficher_menu_principal "? = " 1 3
	set choixMenu=%lireEntierRet%

	if "%choixMenu%"=="1" (
		call :jouer_partie %min% %max% %tours%
	) else if "%choixMenu%"=="2" (
		call :gerer_options %min% %max% %tours%
	) else if "%choixMenu%"=="3" (
		goto fin
	)
	goto boucle_menu

:fin
exit /b