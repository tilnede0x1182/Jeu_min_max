! # Importations
module minmax_mod
	implicit none
	private
	public	:: main

! # Données
	character(len=*), parameter :: fichier_options = 'data/minmax_options.txt'

! # Fonctions utilitaires
	contains

	! ## Lecture d'un entier utilisateur sécurisé
	!**
	! Demande un entier borné jusqu’à obtention d’une entrée valide
	! @message	texte d’invite
	! @borne_min	valeur minimale
	! @borne_max	valeur maximale
	! @resultat	entier valide saisi
	!**
	subroutine lire_entier(message, borne_min, borne_max, resultat)
		implicit none
		character(len=*), intent(in)	:: message
		integer, intent(in)				:: borne_min, borne_max
		integer, intent(out)			:: resultat
		character(len=32)				:: tampon
		integer							:: valeur, erreur
		do
			write(*,'(A)',advance='no') message
			read(*,'(A)') tampon
			read(tampon,*,iostat=erreur) valeur
			if(erreur==0 .and. valeur>=borne_min .and. valeur<=borne_max)then
				resultat = valeur
				exit
			endif
			write(*,'(A)') ''
			write(*,'(A,I0,A,I0,A)') 'Veuillez entrer un entier entre ',borne_min,' et ',borne_max,'.'
		end do
	end subroutine lire_entier

	! ## Conversion entier → chaîne
	function int_to_str(valeur) result(chaine)
		implicit none
		integer, intent(in)		:: valeur
		character(len=32)		:: chaine
		write(chaine,'(I0)') valeur
		chaine = trim(adjustl(chaine))
	end function int_to_str

	! ## Chargement des options sauvegardées
	subroutine charger_options(borne_min, borne_max, limites_tours)
		implicit none
		integer, intent(out)	:: borne_min, borne_max, limites_tours
		integer					:: unite, statut
		logical					:: existe
		inquire(file=fichier_options,exist=existe)
		if(existe)then
			open(newunit=unite,file=fichier_options,status='old',action='read')
			read(unite,*,iostat=statut) borne_min, borne_max, limites_tours
			close(unite)
			if(statut==0) return
		endif
		borne_min = 1;	borne_max = 100;	limites_tours = 5
	end subroutine charger_options

	! ## Sauvegarde des options
	subroutine sauvegarder_options(borne_min, borne_max, limites_tours)
		implicit none
		integer, intent(in)	:: borne_min, borne_max, limites_tours
		integer				:: unite
		open(newunit=unite,file=fichier_options,status='replace',action='write')
		write(unite,'(I0,",",I0,",",I0)') borne_min, borne_max, limites_tours
		close(unite)
	end subroutine sauvegarder_options

! # Fonctions principales
	! Affiche le menu principal
	subroutine afficher_menu_principal()
		implicit none
		write(*,'(A)') ''
		write(*,'(A)') '#### Menu ####'
		write(*,'(A)') '1 : Jouer'
		write(*,'(A)') '2 : Options'
		write(*,'(A)') '3 : Quitter'
	end subroutine afficher_menu_principal

	! Affiche le menu d’options
	subroutine afficher_menu_options(borne_min, borne_max, limites_tours)
		implicit none
		integer, intent(in)	:: borne_min, borne_max, limites_tours
		write(*,'(A)') ''
		write(*,'(A)') '#### Menu options ####'
		write(*,'(A,I0,A,I0,A)') '1 : Choisir les limites (actuellement : ',borne_min,' - ',borne_max,')'
		write(*,'(A,I0,A)') '2 : Nombre de tours max (actuellement : ',limites_tours,')'
	end subroutine afficher_menu_options

	! Joue une partie, indique si plus grand / plus petit
	subroutine jouer_partie(borne_min, borne_max, limites_tours)
		implicit none
		integer, intent(in)	:: borne_min, borne_max, limites_tours
		integer				:: cible, essai, choix
		real				:: alea
		call random_number(alea);	cible = int(alea*(borne_max-borne_min+1))+borne_min
		write(*,'(A,A,I0,A,I0,A,I0,A)') '', 'Trouvez entre ', borne_min, ' et ', borne_max, ' en ', limites_tours, ' tours'
		do essai = 1, limites_tours
			call lire_entier('Tour '//trim(int_to_str(essai))//' : ', borne_min, borne_max, choix)
			if(choix==cible)then
				write(*,'(A)') '';	write(*,'(A)') 'Gagné !';	return
			elseif(choix>cible)then
				write(*,'(A)') '-'
			else
				write(*,'(A)') '+'
			endif
		end do
		write(*,'(A,A,I0,A)') '', 'Perdu ! La réponse était ', cible, '.'
	end subroutine jouer_partie

	! Gère les options et sauvegarde si modifiées
	subroutine gerer_options(borne_min, borne_max, limites_tours)
		implicit none
		integer, intent(inout)	:: borne_min, borne_max, limites_tours
		integer					:: choix_options
		call afficher_menu_options(borne_min, borne_max, limites_tours)
		call lire_entier('? = ',1,2,choix_options)
		if(choix_options==1)then
			call lire_entier('Min = ',1,borne_max,borne_min)
			call lire_entier('Max = ',borne_min,10000,borne_max)
		elseif(choix_options==2)then
			call lire_entier('Tours max = ',1,100,limites_tours)
		endif
		call sauvegarder_options(borne_min,borne_max,limites_tours)
	end subroutine gerer_options

! # Main
	subroutine main()
		implicit none
		integer :: borne_min, borne_max, limites_tours, choix_menu
		call charger_options(borne_min,borne_max,limites_tours)
		do
			call afficher_menu_principal()
			call lire_entier('? = ',1,3,choix_menu)
			select case(choix_menu)
				case(1);	call jouer_partie(borne_min,borne_max,limites_tours)
				case(2);	call gerer_options(borne_min,borne_max,limites_tours)
				case(3);	exit
			end select
		end do
	end subroutine main
end module minmax_mod

! # Lancement du programme
program run
	use minmax_mod
	call main()
end program run
