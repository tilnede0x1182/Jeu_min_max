;;; # Importations
	(import (chicken time))

;;; # Données
(define *chemin-dossier*	"")			; dossier courant
(define *fichier-options* "minmax_options.txt")
(random-seed (current-seconds))			; graine LCG

;;; # Fonctions utilitaires
	;;; ## Fonctions chaînes élémentaires
	;;; "
	;;;	Trim d'une chaîne (bords blancs)
	;;;	@texte chaîne cible
	;;; "
(define (trim texte)
	(let recur-start ((i 0))
		(if (or (= i (string-length texte))
				(not (char-whitespace? (string-ref texte i))))
			(let recur-end ((j (- (string-length texte) 1)))
				(if (< j i) ""
					(if (char-whitespace? (string-ref texte j))
						(recur-end (- j 1))
						(substring texte i (+ j 1)))))
			(recur-start (+ i 1)))))

	;;; "
	;;;	Scinde une ligne CSV simple en liste de sous-chaînes
	;;;	@ligne chaîne source
	;;; "
(define (split-csv ligne)
	(let loop ((idx 0) (acc '()))
		(if (= idx (string-length ligne))
			(reverse acc)
			(let find-comma ((j idx))
				(if (or (= j (string-length ligne)) (char=? #\, (string-ref ligne j)))
					(loop (+ j 1) (cons (substring ligne idx j) acc))
					(find-comma (+ j 1)))))))

	;;; "
	;;;	LCG : pseudo-aléatoire entre min et max inclus
	;;; "
(define (rand-between minimum maximum)
	(set! *rand-seed* (modulo (+ (* *rand-seed* 1103515245) 12345) 2147483648))
	(+ minimum (modulo *rand-seed* (+ 1 (- maximum minimum)))))

	;;; ## Lecture d'un entier utilisateur sécurisé
	;;; "
	;;;	Boucle tant que la saisie n'est pas correcte
	;;; "
(define (lire-entier message minimum maximum)
	(let loop ()
		(display message) (flush-output)
		(let* ((entree (trim (read-line)))
			   (valeur (string->number entree)))
			(if (and valeur (integer? valeur) (<= minimum valeur maximum))
				valeur
				(begin
					(print (string-append "\nVeuillez entrer un entier entre "
										  (number->string minimum) " et "
										  (number->string maximum) "."))
					(loop))))))

	;;; ## Chargement des options sauvegardées
(define (charger-options)
	(if (file-exists? *fichier-options*)
		(call-with-input-file *fichier-options*
			(lambda (in)
				(let* ((ligne (read-line in))
					   (elts  (map string->number (split-csv ligne))))
					(if (= (length elts) 3) elts '(1 100 5)))))
		'(1 100 5)))

	;;; ## Sauvegarde des options
(define (sauvegarder-options minimum maximum tours)
	(call-with-output-file *fichier-options*
		(lambda (out) (display (string-append (number->string minimum) ","
											 (number->string maximum) ","
											 (number->string tours)) out))
		#:exists 'truncate))

;;; # Fonctions principales
(define (afficher-menu-principal)
	(print "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter"))

(define (afficher-menu-options minimum maximum tours)
	(print (string-append "\n#### Menu options ####\n1 : Choisir les limites (actuellement : "
						  (number->string minimum) " - " (number->string maximum)
						  ")\n2 : Nombre de tours max (actuellement : "
						  (number->string tours) ")")))

(define (jouer-partie minimum maximum tours)
	(let ((cible (rand-between minimum maximum)))
		(print (string-append "\nTrouvez entre " (number->string minimum) " et "
							  (number->string maximum) " en "
							  (number->string tours) " tours"))
		(let loop ((tour 1))
			(if (> tour tours)
				(print (string-append "\nPerdu ! La réponse était "
									  (number->string cible) "."))
				(let ((choix (lire-entier (string-append "Tour "
														(number->string tour) " : ")
										 minimum maximum)))
					(cond	[(= choix cible) (print "\nGagné !")]
							[(> choix cible) (begin (print "-") (loop (+ tour 1)))]
							[else (begin (print "+") (loop (+ tour 1)))])))))

(define (gerer-options minimum maximum tours)
	(afficher-menu-options minimum maximum tours)
	(case (lire-entier "? = " 1 2)
		[(1) (newline)
			 (set! minimum (lire-entier "Min = " 1 maximum))
			 (newline)
			 (set! maximum (lire-entier "Max = " minimum 10000))]
		[(2) (newline)
			 (set! tours (lire-entier "Tours max = " 1 100))])
	(sauvegarder-options minimum maximum tours)
	(list minimum maximum tours))

;;; # Main
(define (main)
	(let* ((opts (charger-options))
		   (minimum (list-ref opts 0))
		   (maximum (list-ref opts 1))
		   (tours   (list-ref opts 2)))
		(let loop ()
			(afficher-menu-principal)
			(case (lire-entier "? = " 1 3)
				[(1) (jouer-partie minimum maximum tours) (loop)]
				[(2) (letVALUES ((nmin nmax ntours) (apply values (gerer-options minimum maximum tours)))
						(set! minimum nmin) (set! maximum nmax) (set! tours ntours) (loop))]
				[(3) (void)]))))

;;; # Lancement du programme
(main)
