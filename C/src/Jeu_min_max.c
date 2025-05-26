
// # Importations
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <limits.h>
#include <libgen.h>

// # Données
#define OPTIONS_FILENAME "../data/minmax_options.txt"

// # Fonctions utilitaires
// ## Construction du chemin du fichier d'options
/**
	Construit le chemin vers le fichier d'options situé
	dans le même répertoire que l'exécutable.
	@dest buffer de sortie
	@size taille du buffer
	@argv0 chemin de l'exécutable
*/
void construire_chemin_options(char *dest,size_t size,const char *argv0){
	char chemin[PATH_MAX];strncpy(chemin,argv0,PATH_MAX-1);chemin[PATH_MAX-1]='\0';
	char *dossier=dirname(chemin);snprintf(dest,size,"%s/%s",dossier,OPTIONS_FILENAME);
}

// ## Lecture d'un entier utilisateur sécurisé
/**
	Lit un entier valide compris dans [min,max]
	@message invite affichée
	@min limite basse
	@max limite haute
*/
int lire_entier(const char *message,int min,int max){
	char buffer[64];
	while(1){
		printf("%s",message);
		if(!fgets(buffer,sizeof(buffer),stdin))exit(EXIT_FAILURE);
		buffer[strcspn(buffer,"\n")]='\0';
		char *endptr;long valeur=strtol(buffer,&endptr,10);
		if(*endptr=='\0'&&valeur>=min&&valeur<=max)return(int)valeur;
		printf("\nVeuillez entrer un entier entre %d et %d.\n",min,max);
	}
}

// ## Chargement des options sauvegardées
/**
	Charge min, max, tours depuis le fichier ou définit des valeurs par défaut
*/
void charger_options(int *min,int *max,int *tours,const char *path){
	FILE *fichier=fopen(path,"r");
	if(fichier&&fscanf(fichier,"%d,%d,%d",min,max,tours)==3){fclose(fichier);return;}
	if(fichier)fclose(fichier);
	*min=1;*max=100;*tours=5;
}

// ## Sauvegarde des options
/**
	Sauvegarde min, max, tours dans le fichier, crée data/ si nécessaire
*/
void sauvegarder_options(int min,int max,int tours,const char *path){
	char dossier[PATH_MAX];
	strncpy(dossier, path, PATH_MAX-1);
	dossier[PATH_MAX-1] = '\0';

	// Retire le nom de fichier pour n'avoir que le dossier
	char *slash = strrchr(dossier, '/');
	if(slash){
		*slash = '\0';
		mkdir(dossier, 0755); // crée ./data si manquant
	}

	FILE *fichier = fopen(path, "w");
	if(fichier){
		fprintf(fichier, "%d,%d,%d", min, max, tours);
		fclose(fichier);
	}
}


// # Fonctions principales
/**
	Affiche le menu principal
*/
void afficher_menu_principal(){puts("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter");}

/**
	Affiche le menu d'options
*/
void afficher_menu_options(int min,int max,int tours){
	printf("\n#### Menu options ####\n1 : Choisir les limites (actuellement : %d - %d)\n2 : Nombre de tours max (actuellement : %d)\n",min,max,tours);
}

/**
	Lance une partie
*/
void jouer_partie(int min,int max,int tours){
	int cible=rand()%(max-min+1)+min;
	printf("\nTrouvez entre %d et %d en %d tours\n",min,max,tours);
	for(int attempt_number=1;attempt_number<=tours;++attempt_number){
		char message[32];snprintf(message,sizeof(message),"Tour %d : ",attempt_number);
		int choix=lire_entier(message,min,max);
		if(choix==cible){puts("\nGagné !");return;}
		puts(choix>cible?"-":"+");
	}
	printf("\nPerdu ! La réppnse était %d.\n",cible);
}

/**
	Gère les options et les sauvegarde
*/
void gerer_options(int *min,int *max,int *tours,const char *path){
	afficher_menu_options(*min,*max,*tours);
	int choix_options=lire_entier("? = ",1,2);
	if(choix_options==1){
		*min=lire_entier("\nMin = ",1,*max);
		*max=lire_entier("\nMax = ",*min,10000);
	}else{
		*tours=lire_entier("\nTours max = ",1,100);
	}
	sauvegarder_options(*min,*max,*tours,path);
}

// # Main
int main(int argc,char **argv){
	char options_path[PATH_MAX];construire_chemin_options(options_path,sizeof(options_path),argv[0]);
	int min,max,tours;charger_options(&min,&max,&tours,options_path);
	srand((unsigned)time(NULL));
	while(1){
		afficher_menu_principal();
		int choix_menu=lire_entier("? = ",1,3);
		if(choix_menu==1)jouer_partie(min,max,tours);
		else if(choix_menu==2)gerer_options(&min,&max,&tours,options_path);
		else break;
	}
	return 0;
}
