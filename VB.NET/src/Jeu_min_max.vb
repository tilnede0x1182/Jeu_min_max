' # Importations
Imports System
Imports System.IO
Imports System.Linq
Imports System.Text.RegularExpressions

' # Données
Module MinMax
	Private ReadOnly scanner As New Func(Of String)(AddressOf Console.ReadLine)
	Private ReadOnly motifEntier As New Regex("^\d+$", RegexOptions.Compiled)
	Private ReadOnly fichierOptions As String = Path.Combine(Environment.CurrentDirectory, "data", "minmax_options.txt")
	Private ReadOnly generateurAleatoire As New Random()

	' # Fonctions utilitaires
	' ## Lecture d'un argument de commande
	''' <summary>
	''' Lecture sécurisée d'un entier dans [minimum, maximum]
	''' </summary>
	''' <param name="message">Message d’invite</param>
	''' <param name="minimum">Borne inférieure</param>
	''' <param name="maximum">Borne supérieure</param>
	''' <returns>Entier validé</returns>
	Private Function lireEntier(message As String, minimum As Integer, maximum As Integer) As Integer
		Do
			Console.Write(message)
			Dim entree = scanner().Trim()
			If motifEntier.IsMatch(entree) Then
				Dim valeur = Integer.Parse(entree)
				If valeur >= minimum AndAlso valeur <= maximum Then Return valeur
			End If
			Console.WriteLine(vbLf & "Veuillez entrer un entier entre " & minimum & " et " & maximum & ".")
		Loop
	End Function

	' # Fonctions utilitaires principales
	' ## Création d'une tâche
	''' <summary>
	''' Charge les options sauvegardées depuis le fichier
	''' </summary>
	''' <returns>Tableau [minimum, maximum, nombreTours]</returns>
	Private Function chargerOptions() As Integer()
		If File.Exists(fichierOptions) Then
			Try
				Dim ligne = File.ReadLines(fichierOptions).FirstOrDefault()
				Dim valeurs = ligne?.Split(","c).Where(Function(segment) motifEntier.IsMatch(segment)).Select(AddressOf Integer.Parse).ToList()
				If valeurs IsNot Nothing AndAlso valeurs.Count = 3 Then Return valeurs.ToArray()
			Catch exceptionLecture As IOException
				' Ignore → valeurs par défaut
			End Try
		End If
		Return {1, 100, 5}
	End Function

	''' <summary>
	''' Sauvegarde les options dans le fichier
	''' </summary>
	''' <param name="minimum">Borne inférieure</param>
	''' <param name="maximum">Borne supérieure</param>
	''' <param name="nombreTours">Nombre de tours</param>
	Private Sub sauvegarderOptions(minimum As Integer, maximum As Integer, nombreTours As Integer)
		Try
			File.WriteAllText(fichierOptions, $"{minimum},{maximum},{nombreTours}")
		Catch exceptionEcriture As IOException
			Console.Error.WriteLine("Erreur de sauvegarde : " & exceptionEcriture.Message)
		End Try
	End Sub

	' # Fonctions principales
	''' <summary>
	''' Affiche le menu principal
	''' </summary>
	Private Sub afficherMenuPrincipal()
		Console.WriteLine(vbLf & "#### Menu ####" & vbLf & "1 : Jouer" & vbLf & "2 : Options" & vbLf & "3 : Quitter")
	End Sub

	''' <summary>
	''' Affiche le menu d’options
	''' </summary>
	Private Sub afficherMenuOptions(minimum As Integer, maximum As Integer, nombreTours As Integer)
		Console.WriteLine(vbLf & "#### Menu options ####" & vbLf &
			"1 : Choisir les limites (actuellement : " & minimum & " - " & maximum & ")" & vbLf &
			"2 : Nombre de tours max (actuellement : " & nombreTours & ")")
	End Sub

	''' <summary>
	''' Lance une partie et affiche + ou -
	''' </summary>
	Private Sub jouerPartie(minimum As Integer, maximum As Integer, nombreTours As Integer)
		Dim cible = minimum + generateurAleatoire.Next(maximum - minimum + 1)
		Console.WriteLine(vbLf & "Trouvez entre " & minimum & " et " & maximum & " en " & nombreTours & " tours")
		For numeroEssai As Integer = 1 To nombreTours
			Dim choix = lireEntier("Tour " & numeroEssai & " : ", minimum, maximum)
			If choix = cible Then Console.WriteLine(vbLf & "Gagné !") : Return
			Console.WriteLine(If(choix > cible, "-", "+"))
		Next
		Console.WriteLine(vbLf & "Perdu ! La réppnse était " & cible & ".")
	End Sub

	''' <summary>
	''' Gère le menu Options et sauvegarde
	''' </summary>
	Private Function gererOptions(minimum As Integer, maximum As Integer, nombreTours As Integer) As Integer()
		afficherMenuOptions(minimum, maximum, nombreTours)
		Select Case lireEntier("? = ", 1, 2)
			Case 1
				minimum = lireEntier(vbLf & "Min = ", 1, maximum)
				maximum = lireEntier(vbLf & "Max = ", minimum, 10000)
			Case 2
				nombreTours = lireEntier(vbLf & "Tours max = ", 1, 100)
		End Select
		sauvegarderOptions(minimum, maximum, nombreTours)
		Return {minimum, maximum, nombreTours}
	End Function

	' # Main
	''' <summary>
	''' Boucle principale du programme
	''' </summary>
	Sub main()
		Dim optionsArray = chargerOptions()
		Dim minimum = optionsArray(0), maximum = optionsArray(1), nombreTours = optionsArray(2)
		Do
			afficherMenuPrincipal()
			Select Case lireEntier("? = ", 1, 3)
				Case 1 : jouerPartie(minimum, maximum, nombreTours)
				Case 2
					Dim nouvellesOptions = gererOptions(minimum, maximum, nombreTours)
					minimum = nouvellesOptions(0) : maximum = nouvellesOptions(1) : nombreTours = nouvellesOptions(2)
				Case 3 : Exit Do
			End Select
		Loop
	End Sub
End Module
