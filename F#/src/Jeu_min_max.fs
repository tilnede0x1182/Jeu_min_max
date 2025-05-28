// # Importations
open System
open System.IO
open System.Text.RegularExpressions

// # Données
let motifEntier = Regex(@"\d+")
let fichierOptions = Path.Combine(Environment.CurrentDirectory, "data", "minmax_options.txt")
let generateurAleatoire = Random()

// # Fonctions utilitaires
// ## Lecture d'un argument de commande
/// """
/// Lecture sécurisée d'un entier depuis l'utilisateur
/// Contrôle l'appartenance à un intervalle [minimum, maximum]
/// @param message message à afficher
/// @param minimum borne inférieure
/// @param maximum borne supérieure
/// @return   entier validé
/// """
let rec lireEntier(message:string, minimum:int, maximum:int) : int =
    Console.Write(message)
    let entree = Console.ReadLine().Trim()
    if motifEntier.IsMatch(entree) then
        let valeur = int entree
        if valeur >= minimum && valeur <= maximum then valeur
        else
            printfn "\nVeuillez entrer un entier entre %d et %d." minimum maximum
            lireEntier(message, minimum, maximum)
    else
        printfn "\nVeuillez entrer un entier entre %d et %d." minimum maximum
        lireEntier(message, minimum, maximum)

// # Fonctions utilitaires principales
// ## Création d'une tâche
/// """
/// Charge les options sauvegardées depuis le fichier
/// @return tableau [minimum, maximum, nombreTours]
/// """
let chargerOptions() : int[] =
    if File.Exists(fichierOptions) then
        try
            File.ReadAllText(fichierOptions).Split(',')
            |> Array.filter motifEntier.IsMatch
            |> Array.map int
            |> fun valeurs -> if valeurs.Length = 3 then valeurs else [|1;100;5|]
        with
        | :? IOException -> [|1;100;5|]
    else
        [|1;100;5|]

/// """
/// Sauvegarde les options dans le fichier
/// @param minimum borne inférieure
/// @param maximum borne supérieure
/// @param nombreTours nombre de tours
/// """
let sauvegarderOptions(minimum:int, maximum:int, nombreTours:int) =
    try
        Directory.CreateDirectory(Path.GetDirectoryName(fichierOptions)) |> ignore
        File.WriteAllText(fichierOptions, $"{minimum},{maximum},{nombreTours}")
    with
    | :? IOException as IOExceptionEcriture ->
        Console.Error.WriteLine($"Erreur de sauvegarde : {IOExceptionEcriture.Message}")

// # Fonctions principales
/// """
/// Affiche le menu principal
/// """
let afficherMenuPrincipal() =
    printfn "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter"

/// """
/// Affiche le menu d'options
/// """
let afficherMenuOptions(minimum:int, maximum:int, nombreTours:int) =
    printfn "\n#### Menu options ####\n1 : Choisir les limites (actuellement : %d - %d)\n2 : Nombre de tours max (actuellement : %d)" minimum maximum nombreTours

/// """
/// Lance une partie et affiche + ou -
/// """
let jouerPartie(minimum:int, maximum:int, nombreTours:int) =
    let cible = generateurAleatoire.Next(minimum, maximum + 1)
    printfn "\nTrouvez entre %d et %d en %d tours" minimum maximum nombreTours
    let rec tour numeroEssai =
        if numeroEssai > nombreTours then
            printfn "\nPerdu ! La réponse était %d." cible
        else
            let choix = lireEntier($"Tour {numeroEssai} : ", minimum, maximum)
            if choix = cible then
                printfn "\nGagné !"
            else
                printfn "%s" (if choix > cible then "-" else "+")
                tour (numeroEssai + 1)
    tour 1

/// """
/// Gère le menu Options et sauvegarde
/// """
let gererOptions(minimum:int, maximum:int, nombreTours:int) : int[] =
    afficherMenuOptions(minimum, maximum, nombreTours)
    let choixOptions = lireEntier("? = ", 1, 2)
    let mutable min  = minimum
    let mutable max  = maximum
    let mutable tours = nombreTours
    if choixOptions = 1 then
        min <- lireEntier("\nMin = ", 1, maximum)
        max <- lireEntier("\nMax = ", min, 10000)
    elif choixOptions = 2 then
        tours <- lireEntier("\nTours max = ", 1, 100)
    sauvegarderOptions(min, max, tours)
    [|min; max; tours|]

/// """
/// Boucle principale interactive
/// """
let rec boucleProgramme(minimum:int, maximum:int, nombreTours:int) =
    afficherMenuPrincipal()
    match lireEntier("? = ", 1, 3) with
    | 1 ->
        jouerPartie(minimum, maximum, nombreTours)
        boucleProgramme(minimum, maximum, nombreTours)
    | 2 ->
        let options = gererOptions(minimum, maximum, nombreTours)
        boucleProgramme(options.[0], options.[1], options.[2])
    | 3 -> ()
    | _ -> boucleProgramme(minimum, maximum, nombreTours)

// # Main
/// """
/// Point d'entrée du programme
/// """
let main() =
    let options = chargerOptions()
    boucleProgramme(options.[0], options.[1], options.[2])

// # Lancement du programme
main()
