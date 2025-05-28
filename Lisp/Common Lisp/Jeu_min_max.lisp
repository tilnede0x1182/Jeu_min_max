;;; # Données
(defparameter *chemin_dossier*
	(or (when *load-truename*
		(make-pathname :name nil :type nil :defaults *load-truename*))
	*default-pathname-defaults*))

(defparameter *fichier_options*
	(merge-pathnames "minmax_options.txt" *chemin_dossier*))

;;; # Importations
(eval-when (:compile-toplevel :load-toplevel :execute)
	(use-package :cl))

;;; # Fonctions utilitaires
	;;; ## Lecture d'un entier utilisateur sécurisé
	;;; """
	;;;	Lit un entier entre minimum et maximum
	;;;	Boucle tant que la saisie n'est pas correcte
	;;;	@message	texte affiché à l'utilisateur
	;;;	@minimum	valeur minimale
	;;;	@maximum	valeur maximale
	;;; """
(defun lire_entier(message minimum maximum)
	(loop
		(format t "~a" message)
		(force-output)
		(let* ((entree (string-trim " " (read-line)))
			   (valeur (and (every #'digit-char-p entree)
						(parse-integer entree :junk-allowed t))))
			(when (and valeur (<= minimum valeur maximum))
				(return valeur)))
		(format t "~%Veuillez entrer un entier entre ~a et ~a.~%" minimum maximum)))

	;;; ## Chargement des options sauvegardées
	;;; """
	;;;	Charge min, max, tours depuis le fichier
	;;;	Renvoie [1 100 5] si fichier absent ou corrompu
	;;; """
(defun charger_options()
	(when (probe-file *fichier_options*)
		(with-open-file (flux *fichier_options*)
			(let* ((ligne (read-line flux))
				   (elements (loop for position = 0 then (+ next 1)
								   for next = (position #\, ligne :start position)
								   collect (subseq ligne position (or next (length ligne)))
								   while next))
				   (donnees (mapcar #'parse-integer elements)))
				(if (= (length donnees) 3)
					(return-from charger_options donnees)))))
	'(1 100 5))

	;;; ## Sauvegarde des options
	;;; """
	;;;	Enregistre min, max, tours dans le fichier
	;;;	@minimum	valeur minimale
	;;;	@maximum	valeur maximale
	;;;	@tours		nombre de tentatives
	;;; """
(defun sauvegarder_options(minimum maximum tours)
	(with-open-file (flux *fichier_options* :direction :output :if-exists :supersede)
		(format flux "~a,~a,~a" minimum maximum tours)))

;;; # Fonctions principales
	;;; Affiche le menu principal
(defun afficher_menu_principal()
	(format t "~%#### Menu ####~%1 : Jouer~%2 : Options~%3 : Quitter~%"))

	;;; Affiche le menu d'options
(defun afficher_menu_options(minimum maximum tours)
	(format t "~%#### Menu options ####~%1 : Choisir les limites (actuellement : ~a - ~a)~%2 : Nombre de tours max (actuellement : ~a)~%"
		minimum maximum tours))

	;;; Joue une partie, indique si plus grand/petit
(defun jouer_partie(minimum maximum tours)
	(let ((cible (+ minimum (random (+ 1 (- maximum minimum))))))
		(format t "~%Trouvez entre ~a et ~a en ~a tours~%" minimum maximum tours)
		(loop for attempt_number from 1 to tours do
			(let ((choix (lire_entier (format nil "Tour ~a : " attempt_number) minimum maximum)))
				(cond ((= choix cible) 
						(format t "~%Gagné !~%") 
						(return-from jouer_partie nil))
					  ((> choix cible) (format t "-~%"))
					  (t (format t "+~%")))))
		(format t "~%Perdu ! La réponse était ~a.~%" cible)))

	;;; Gère les options et sauvegarde si modifiées
(defun gerer_options(minimum maximum tours)
	(afficher_menu_options minimum maximum tours)
	(case (lire_entier "? = " 1 2)
		(1 (progn
			 (format t "~%")
			 (setf minimum (lire_entier "Min = " 1 maximum))
			 (format t "~%")
			 (setf maximum (lire_entier "Max = " minimum 10000))))
		(2 (progn
			 (format t "~%")
			 (setf tours (lire_entier "Tours max = " 1 100)))))
	(sauvegarder_options minimum maximum tours)
	(list minimum maximum tours))

;;; # Main
;;; """
;;;	Point d'entrée du programme
;;;	Gère la boucle de menu
;;; """
(defun main()
	(destructuring-bind (minimum maximum tours) (charger_options)
		(loop
			(afficher_menu_principal)
			(case (lire_entier "? = " 1 3)
				(1 (jouer_partie minimum maximum tours))
				(2 (destructuring-bind (nmin nmax ntours) (gerer_options minimum maximum tours)
						(setf minimum nmin maximum nmax tours ntours)))
				(3 (return))))))

;;; # Lancement du programme
(main)
