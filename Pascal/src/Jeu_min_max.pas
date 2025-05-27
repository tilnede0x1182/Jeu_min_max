{$mode delphi}

// # Importations
uses
	SysUtils, Classes, StrUtils;

// # Données
const
	fichierOptionsRel = 'data' + DirectorySeparator + 'minmax_options.txt';

type
	TOptions = array[0..2] of Integer;

// # Fonctions utilitaires
// ## Lecture d'un argument de commande
{**
	Lecture sécurisée d'un entier dans une plage
	@message message affiché à l'utilisateur
	@minimum borne inférieure
	@maximum borne supérieure
}
function lireEntier(message: string; minimum, maximum: Integer): Integer;
var
	entree: string;
	valeur: Integer;
begin
	repeat
		Write(message);
		ReadLn(entree);
		entree := Trim(entree);
		if not (TryStrToInt(entree, valeur) and (valeur >= minimum) and (valeur <= maximum)) then
			WriteLn(#10'Veuillez entrer un entier entre ', minimum, ' et ', maximum, '.');
	until TryStrToInt(entree, valeur) and (valeur >= minimum) and (valeur <= maximum);
	Result := valeur;
end;

// # Fonctions utilitaires principales
// ## Lecture du fichier d'options
{**
	Retourne les valeurs par défaut
}
function optionsParDefaut(): TOptions;
begin
	Result[0] := 1;
	Result[1] := 100;
	Result[2] := 5;
end;

{**
	Charge les options sauvegardées depuis le fichier
	@return tableau [minimum, maximum, nombreTours]
}
function chargerOptions(): TOptions;
var
	contenu: TStringList;
	valeurs: TStringArray;
	resultat: TOptions;
begin
	resultat := optionsParDefaut();
	if not FileExists(fichierOptionsRel) then
	begin
		Result := resultat;
		Exit;
	end;
	contenu := TStringList.Create;
	try
		contenu.LoadFromFile(fichierOptionsRel);
		if contenu.Count > 0 then
		begin
			valeurs := SplitString(contenu[0], ',');
			if (Length(valeurs) = 3) and
			   TryStrToInt(valeurs[0], resultat[0]) and
			   TryStrToInt(valeurs[1], resultat[1]) and
			   TryStrToInt(valeurs[2], resultat[2]) then
			begin
				Result := resultat;
				Exit;
			end;
		end;
	finally
		contenu.Free;
	end;
	Result := resultat;
end;

{**
	Sauvegarde les options dans un fichier
	@minimum borne inférieure
	@maximum borne supérieure
	@nombreTours nombre de tours
}
procedure sauvegarderOptions(minimum, maximum, nombreTours: Integer);
var
	lignes: TStringList;
begin
	ForceDirectories('data');
	lignes := TStringList.Create;
	try
		lignes.Add(Format('%d,%d,%d', [minimum, maximum, nombreTours]));
		lignes.SaveToFile(fichierOptionsRel);
	except
		on IOException: Exception do
			WriteLn(StdErr, 'Erreur de sauvegarde : ', IOException.Message);
	end;
	lignes.Free;
end;

// # Fonctions principales
{**
	Affiche le menu principal
}
procedure afficherMenuPrincipal();
begin
	WriteLn(#10'#### Menu ####');
	WriteLn('1 : Jouer');
	WriteLn('2 : Options');
	WriteLn('3 : Quitter');
end;

{**
	Affiche le menu d'options
	@minimum borne inférieure
	@maximum borne supérieure
	@nombreTours nombre de tours max
}
procedure afficherMenuOptions(minimum, maximum, nombreTours: Integer);
begin
	WriteLn(#10'#### Menu options ####');
	WriteLn(Format('1 : Choisir les limites (actuellement : %d - %d)', [minimum, maximum]));
	WriteLn(Format('2 : Nombre de tours max (actuellement : %d)', [nombreTours]));
end;

{**
	Affiche un message de fin de partie
	@cible nombre à trouver
	@choix choix utilisateur
	@trouve variable modifiée à vrai si gagné
}
procedure afficherResultat(cible, choix: Integer; var trouve: Boolean);
begin
	if choix = cible then
	begin
		WriteLn(#10'Gagné !');
		trouve := True;
	end
	else
		WriteLn(IfThen(choix > cible, '-', '+'));
end;

{**
	Lance une partie avec les paramètres actuels
	@minimum borne inférieure
	@maximum borne supérieure
	@nombreTours nombre de tours max
}
procedure jouerPartie(minimum, maximum, nombreTours: Integer);
var
	cible, choix, iterator: Integer;
	trouve: Boolean;
begin
	cible := minimum + Random(maximum - minimum + 1);
	WriteLn(#10'Trouvez entre ', minimum, ' et ', maximum, ' en ', nombreTours, ' tours');
	trouve := False;
	for iterator := 1 to nombreTours do
	begin
		choix := lireEntier('Tour ' + IntToStr(iterator) + ' : ', minimum, maximum);
		afficherResultat(cible, choix, trouve);
		if trouve then Exit;
	end;
	WriteLn(#10'Perdu ! La réppnse était ', cible, '.');
end;

{**
	Gère les modifications des options
	@minimum borne inférieure
	@maximum borne supérieure
	@nombreTours nombre de tours max
	@return tableau [minimum, maximum, nombreTours]
}
function gererOptions(minimum, maximum, nombreTours: Integer): TOptions;
var
	choixOptions: Integer;
	nouvelOptions: TOptions;
begin
	afficherMenuOptions(minimum, maximum, nombreTours);
	choixOptions := lireEntier('? = ', 1, 2);
	if choixOptions = 1 then
	begin
		minimum := lireEntier(#10'Min = ', 1, maximum);
		maximum := lireEntier(#10'Max = ', minimum, 10000);
	end
	else if choixOptions = 2 then
		nombreTours := lireEntier(#10'Tours max = ', 1, 100);
	sauvegarderOptions(minimum, maximum, nombreTours);
	nouvelOptions[0] := minimum;
	nouvelOptions[1] := maximum;
	nouvelOptions[2] := nombreTours;
	Result := nouvelOptions;
end;

// # Main
{**
	Boucle principale du programme
}
procedure main();
var
	options, nouvellesOptions: TOptions;
	minimum, maximum, nombreTours, choixMenu: Integer;
begin
	Randomize;
	options := chargerOptions();
	minimum := options[0];
	maximum := options[1];
	nombreTours := options[2];
	while True do
	begin
		afficherMenuPrincipal();
		choixMenu := lireEntier('? = ', 1, 3);
		case choixMenu of
			1: jouerPartie(minimum, maximum, nombreTours);
			2: begin
				nouvellesOptions := gererOptions(minimum, maximum, nombreTours);
				minimum := nouvellesOptions[0];
				maximum := nouvellesOptions[1];
				nombreTours := nouvellesOptions[2];
			end;
			3: Break;
		end;
	end;
end;

// # Lancement du programme
begin
	main();
end.
