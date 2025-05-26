@echo off

@REM echo [DEBUG] Script demarre
ping 127.0.0.1 -n 1 >nul

REM # Données
	set "CHEMIN_DOSSIER=%~dp0"
	set "FICHIER_OPTIONS=%CHEMIN_DOSSIER%minmax_options.txt"
	@REM echo [DEBUG] Variables initiales definies

REM # Lancement du programme - DOIT ETRE AU DEBUT
@REM echo [DEBUG] Lancement du programme
call :main
goto fin

REM # Fonctions utilitaires
REM ## Lecture d'un entier utilisateur sécurisé
:lire_entier
	@REM echo [DEBUG] Entree dans lire_entier avec parametres: "%~1" "%~2" "%~3"
	set "promptTexte=%~1"
	set "minVal=%~2"
	set "maxVal=%~3"
	@REM echo [DEBUG] Variables locales: promptTexte=%promptTexte% minVal=%minVal% maxVal=%maxVal%
:le_loop
	@REM echo [DEBUG] Debut de boucle de saisie
	set "nonChiffre="
	set /p saisie=%promptTexte%
	@REM echo [DEBUG] Saisie recue: "%saisie%"
	if "%saisie%"=="" (
		@REM echo [DEBUG] Saisie vide
		goto mauvais
	)
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre (
		@REM echo [DEBUG] Caractere non numerique detecte: %nonChiffre%
		goto mauvais
	)
	set /a nombre=%saisie% 2>nul
	@REM echo [DEBUG] Nombre converti: %nombre% (entre %minVal% et %maxVal%)
	if %nombre% GEQ %minVal% if %nombre% LEQ %maxVal% (
		@REM echo [DEBUG] Nombre valide, sortie de fonction
		set lireEntierRet=%nombre%
		exit /b
	)
:mauvais
	@REM echo [DEBUG] Saisie incorrecte
	echo.
	echo Veuillez entrer un entier entre %minVal% et %maxVal%.
	echo.
	goto le_loop

REM ## Chargement des options sauvegardées
:charger_options
	@REM echo [DEBUG] Entree dans charger_options
	@REM echo [DEBUG] Chemin fichier: "%FICHIER_OPTIONS%"
	if exist "%FICHIER_OPTIONS%" (
		@REM echo [DEBUG] Fichier options existe
		for /f "usebackq tokens=1-3 delims=," %%A in ("%FICHIER_OPTIONS%") do (
			set min=%%A
			set max=%%B
			set tours=%%C
			@REM echo [DEBUG] Options lues: min=%%A max=%%B tours=%%C
		)
	) else (
		@REM echo [DEBUG] Fichier options inexistant
	)
	if not defined min (
		@REM echo [DEBUG] Variables non definies, utilisation des valeurs par defaut
		set min=1
		set max=100
		set tours=5
	)
	@REM echo [DEBUG] Options finales: min=%min% max=%max% tours=%tours%
	exit /b

REM ## Sauvegarde des options
:sauvegarder_options
	@REM echo [DEBUG] Sauvegarde: %1,%2,%3
	(echo %1,%2,%3) > "%FICHIER_OPTIONS%"
	exit /b

REM # Fonctions principales
REM ## Affiche le menu principal
:afficher_menu_principal
	@REM echo [DEBUG] Affichage menu principal
	echo.
	echo #### Menu ####
	echo 1 : Jouer
	echo 2 : Options
	echo 3 : Quitter
	exit /b

REM ## Affiche le menu d'options
:afficher_menu_options
	@REM echo [DEBUG] Affichage menu options avec %1 %2 %3
	echo.
	echo #### Menu options ####
	echo 1 : Choisir les limites (actuellement : %1 - %2)
	echo 2 : Nombre de tours max (actuellement : %3)
	exit /b

REM ## Joue une partie, indique si plus grand/petit
:jouer_partie
	@REM echo [DEBUG] Debut partie avec %1 %2 %3
	setlocal EnableDelayedExpansion
	set minLoc=%1 & set maxLoc=%2 & set toursLoc=%3
	set /a intervalle=maxLoc-minLoc+1
	set /a cible=!RANDOM! %% !intervalle! + !minLoc!
	@REM echo [DEBUG] Cible generee: !cible!
	echo.
	echo Trouvez entre !minLoc! et !maxLoc! en !toursLoc! tours
	for /l %%N in (1,1,!toursLoc!) do (
		@REM echo [DEBUG] Tour %%N
		call :lire_entier "Tour %%N : " !minLoc! !maxLoc!
		set choix=!lireEntierRet!
		@REM echo [DEBUG] Choix du joueur: !choix!
		if !choix! EQU !cible! (echo.& echo Gagné^^ ^^!& endlocal & exit /b)
		if !choix! GTR !cible! (echo -) else (echo +)
	)
	echo.
	echo Perdu^^ ^^! La reponse etait !cible!.
	endlocal & exit /b

REM ## Lecture d'un entier avec réaffichage du menu en cas d'erreur
:lire_entier_avec_menu
	@REM echo [DEBUG] Entree dans lire_entier_avec_menu
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
	@REM echo [DEBUG] Saisie menu recue: "%saisie%"
	if "%saisie%"=="" goto lemenu_mauvais
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre goto lemenu_mauvais
	set /a nombre=%saisie% 2>nul
	if %nombre% GEQ %minVal% if %nombre% LEQ %maxVal% (
		@REM echo [DEBUG] Choix menu valide: %nombre%
		set lireEntierRet=%nombre%
		exit /b
	)
:lemenu_mauvais
	@REM echo [DEBUG] Choix menu incorrect
	echo.
	echo Veuillez entrer un entier entre %minVal% et %maxVal%.
	echo.
	goto lemenu_loop

REM ## Gère les options et sauvegarde si modifiées
:gerer_options
	@REM echo [DEBUG] Gestion options avec %1 %2 %3
	setlocal EnableDelayedExpansion
	set minOpt=%1 & set maxOpt=%2 & set toursOpt=%3
	call :lire_entier_avec_menu afficher_menu_options "? = " 1 2 !minOpt! !maxOpt! !toursOpt!
	set choixOpt=!lireEntierRet!
	@REM echo [DEBUG] Choix option: !choixOpt!
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
	@REM echo [DEBUG] Entree dans main
	call :charger_options
	@REM echo [DEBUG] Options chargees: min=%min% max=%max% tours=%tours%
:boucle_menu
	@REM echo [DEBUG]  Debut boucle menu
	call :lire_entier_avec_menu afficher_menu_principal "? = " 1 3
	set choixMenu=%lireEntierRet%
	@REM echo [DEBUG]  Choix menu: %choixMenu%

	if "%choixMenu%"=="1" (
		@REM echo [DEBUG]  Lancement partie
		call :jouer_partie %min% %max% %tours%
	) else if "%choixMenu%"=="2" (
		@REM echo [DEBUG]  Gestion options
		call :gerer_options %min% %max% %tours%
	) else if "%choixMenu%"=="3" (
		@REM echo [DEBUG]  Quitter
		goto fin
	)
	goto boucle_menu

:fin
@REM echo [DEBUG] Fin du programme
exit /b