
(* # Importations *)
Random.self_init()

(* # Données *)
let chemin_dossier = Filename.dirname Sys.argv.(0)
let fichier_options = Filename.concat chemin_dossier "minmax_options.txt"

(* # Fonctions utilitaires *)
	(**	Convertit la saisie utilisateur en entier sécurisé
		Valide que l'entier est compris entre min et max
		@message message affiché
		@min borne minimale
		@max borne maximale	*)
	let lire_entier message min max =
		let rec boucle() =
			Printf.printf "%s%!" message;
			let saisie = read_line() in
			match int_of_string_opt saisie with
			| Some valeur when valeur >= min && valeur <= max -> valeur
			| _ ->
				Printf.printf "\nVeuillez entrer un entier entre %d et %d.\n%!" min max;
				boucle()
		in
		boucle()

	(**	Charge les options sauvegardées ou valeurs par défaut
		Retourne (min, max, tours)	*)
	let charger_options() =
		try
			let canal = open_in fichier_options in
			let ligne = input_line canal in
			close_in canal;
			match List.map int_of_string (String.split_on_char ',' ligne) with
			| [min; max; tours] -> (min, max, tours)
			| _ -> (1, 100, 5)
		with Sys_error _ -> (1, 100, 5)

	(**	Sauvegarde les options dans le fichier dédié
		@min borne minimale
		@max borne maximale
		@tours nombre de tentatives	*)
	let sauvegarder_options min max tours =
		let canal = open_out fichier_options in
		Printf.fprintf canal "%d,%d,%d" min max tours;
		close_out canal

(* # Fonctions principales *)
	(**	Affiche le menu principal	*)
	let afficher_menu_principal() =
		Printf.printf "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter\n%!"

	(**	Affiche le menu des options
		@min borne minimale
		@max borne maximale
		@tours nombre de tentatives	*)
	let afficher_menu_options min max tours =
		Printf.printf
			"\n#### Menu options ####\n1 : Choisir les limites (actuellement : %d - %d)\n2 : Nombre de tours max (actuellement : %d)\n%!"
			min max tours

	(**	Lance une partie de devinette
		@min borne minimale
		@max borne maximale
		@tours nombre de tentatives	*)
	let jouer_partie min max tours =
		let cible = Random.int (max - min + 1) + min in
		Printf.printf "\nTrouvez entre %d et %d en %d tours\n%!" min max tours;
		for iterator_numero = 1 to tours do
			let choix = lire_entier (Printf.sprintf "Tour %d : " iterator_numero) min max in
			if choix = cible then (Printf.printf "\nGagné !\n%!"; raise Exit);
			Printf.printf "%s\n%!" (if choix > cible then "-" else "+")
		done;
		Printf.printf "\nPerdu ! La réponse était %d.\n%!" cible

	(**	Gère la modification des options et leur sauvegarde
		@min borne minimale
		@max borne maximale
		@tours nombre de tentatives	*)
	let gerer_options min max tours =
		afficher_menu_options min max tours;
		match lire_entier "? = " 1 2 with
		| 1 ->
			let nouveau_min = lire_entier "\nMin = " 1 max in
			let nouveau_max = lire_entier "\nMax = " nouveau_min 10_000 in
			sauvegarder_options nouveau_min nouveau_max tours;
			(nouveau_min, nouveau_max, tours)
		| 2 ->
			let nouveau_tours = lire_entier "\nTours max = " 1 100 in
			sauvegarder_options min max nouveau_tours;
			(min, max, nouveau_tours)
		| _ -> (min, max, tours)

(* # Main *)
let rec main() =
	let min, max, tours = charger_options() in
	let rec boucle min_bound max_bound tours_bound =
		afficher_menu_principal();
		match lire_entier "? = " 1 3 with
		| 1 -> (try jouer_partie min_bound max_bound tours_bound with Exit -> ()); boucle min_bound max_bound tours_bound
		| 2 ->
			let nouveau_min, nouveau_max, nouveau_tours = gerer_options min_bound max_bound tours_bound in
			boucle nouveau_min nouveau_max nouveau_tours
		| 3 -> ()
		| _ -> boucle min_bound max_bound tours_bound
	in
	boucle min max tours

(* # Lancement du programme *)
let () = main()
