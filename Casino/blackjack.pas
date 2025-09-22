unit BlackJack;

{$mode objfpc}{$H+}

interface

uses
  SDL2, SDL2_ttf, SDL2_image, SysUtils;

procedure LancerBlackjack(renderer: PSDL_Renderer; font: PTTF_Font);

implementation

type
  // -------- Type pour une carte --------
  TCarte = record
    Valeur: string;
    Couleur: string;
  end;

  // -------- Type pour un joueur --------
  TJoueur = record
    Score: Integer;
    Jetons: Integer;
    NombreCartes: Integer;
  end;

  // -------- Type pour le croupier --------
  TCroupier = record
    Score: Integer;
    NombreCartes: Integer;
  end;

  // -------- Type pour la partie --------
  TPartie = record
    MiseTotale: Integer;
  end;

// Tableaux pour les valeurs et couleurs des cartes
const
  ValeursCartes: array[0..12] of string = ('As','2','3','4','5','6','7','8','9','10','Valet','Dame','Roi');
  CouleursCartes: array[0..3] of string = ('Pique','Trèfle','Carreau','Cœur');

var
  JeuCartes: array of TCarte;
  IndexCarte: Integer;

// Fonction pour créer une carte
function CreerCarte(Valeur, Couleur: string): TCarte;
begin
  Result.Valeur := Valeur;
  Result.Couleur := Couleur;
end;

// Fonction pour obtenir la valeur numérique d'une carte
function ValeurNumeriqueCarte(Carte: TCarte): Integer;
begin
  if Carte.Valeur = 'As' then
    Result := 11
  else if (Carte.Valeur = 'Valet') or (Carte.Valeur = 'Dame') or (Carte.Valeur = 'Roi') or (Carte.Valeur = '10') then
    Result := 10
  else
    Result := StrToInt(Carte.Valeur);
end;

// Fonction pour afficher une carte
function AfficherCarte(Carte: TCarte): string;
begin
  Result := Carte.Valeur + ' de ' + Carte.Couleur;
end;

// Procédure pour mélanger les cartes
procedure MelangerCartes(var Cartes: array of TCarte);
var
  i, j: Integer;
  Temp: TCarte;
begin
  for i := High(Cartes) downto Low(Cartes) do
  begin
    j := Random(Length(Cartes));
    Temp := Cartes[i];
    Cartes[i] := Cartes[j];
    Cartes[j] := Temp;
  end;
end;

// Procédure pour initialiser le jeu
procedure InitialiserJeu(var Joueur: TJoueur; var Croupier: TCroupier; var Partie: TPartie);
var
  i, j, k: Integer;
begin
  // Créer le joueur avec 200 jetons
  Joueur.Score := 0;
  Joueur.Jetons := 200;
  Joueur.NombreCartes := 0;
  
  // Créer le croupier
  Croupier.Score := 0;
  Croupier.NombreCartes := 0;
  
  // Initialiser la partie
  Partie.MiseTotale := 0;
  
  // Créer le jeu de cartes (6 paquets)
  SetLength(JeuCartes, 6 * 13 * 4);
  for i := 0 to 5 do
    for j := 0 to 12 do
      for k := 0 to 3 do
        JeuCartes[i*52 + j*4 + k] := CreerCarte(ValeursCartes[j], CouleursCartes[k]);
  
  // Mélanger les cartes
  MelangerCartes(JeuCartes);
  IndexCarte := 0;
end;

// Procédure pour afficher l'état du jeu
procedure AfficherEtat(Joueur: TJoueur; Partie: TPartie);
begin
  writeln('Votre score: ', Joueur.Score);
  writeln('Vos jetons: ', Joueur.Jetons);
  writeln('Mise actuelle: ', Partie.MiseTotale);
end;

procedure LancerBlackjack(renderer: PSDL_Renderer; font: PTTF_Font);
var
  Joueur: TJoueur;
  Croupier: TCroupier;
  Partie: TPartie;
  ChoixMenu, Mise, Choix: Integer;
  ContinuerPartie: Boolean;
begin
  Randomize;
  InitialiserJeu(Joueur, Croupier, Partie);
  
  writeln('Bienvenue au jeu de BlackJack!');
  writeln('------------------------------');
  
  ContinuerPartie := True;
  
  while ContinuerPartie and (Joueur.Jetons > 0) do
  begin
    writeln;
    writeln('Que voulez-vous faire?');
    writeln('1: Jouer une main');
    writeln('2: Quitter le jeu');
    write('Votre choix: ');
    readln(ChoixMenu);
    
    case ChoixMenu of
      1:
        begin
          // Demander la mise
          writeln;
          write('Combien voulez-vous miser? ');
          readln(Mise);
          
          // Vérifier que la mise est valide
          while Mise > Joueur.Jetons do
          begin
            writeln('Vous n''avez pas assez de jetons!');
            write('Combien voulez-vous miser? ');
            readln(Mise);
          end;
          
          // Déduire la mise des jetons du joueur
          Joueur.Jetons := Joueur.Jetons - Mise;
          Partie.MiseTotale := Mise;
          
          // Distribuer les cartes
          writeln;
          writeln('Distribution des cartes...');
          
          // Cartes du joueur
          Joueur.Score := ValeurNumeriqueCarte(JeuCartes[IndexCarte]) +
                          ValeurNumeriqueCarte(JeuCartes[IndexCarte+1]);
          writeln('Vos cartes: ', AfficherCarte(JeuCartes[IndexCarte]), ' et ',
                  AfficherCarte(JeuCartes[IndexCarte+1]));
          writeln('Votre score: ', Joueur.Score);
          
          // Carte visible du croupier
          Croupier.Score := ValeurNumeriqueCarte(JeuCartes[IndexCarte+2]);
          writeln('Carte visible du croupier: ', AfficherCarte(JeuCartes[IndexCarte+2]));
          
          IndexCarte := IndexCarte + 3;
          
          // Tour du joueur
          writeln;
          writeln('--- Votre tour ---');
          while True do
          begin
            writeln;
            writeln('Que voulez-vous faire?');
            writeln('1: Prendre une carte');
            writeln('2: Rester');
            write('Votre choix: ');
            readln(Choix);
            
            if Choix = 1 then
            begin
              // Donner une carte supplémentaire au joueur
              Joueur.Score := Joueur.Score + ValeurNumeriqueCarte(JeuCartes[IndexCarte]);
              writeln('Vous recevez: ', AfficherCarte(JeuCartes[IndexCarte]));
              writeln('Nouveau score: ', Joueur.Score);
              
              IndexCarte := IndexCarte + 1;
              
              // Vérifier si le joueur a dépassé 21
              if Joueur.Score > 21 then
              begin
                writeln('Vous avez dépassé 21! Vous perdez.');
                Break;
              end;
            end
            else
            begin
              Break;
            end;
          end;
          
          // Tour du croupier si le joueur n'a pas perdu
          if Joueur.Score <= 21 then
          begin
            writeln;
            writeln('--- Tour du croupier ---');
            
            // Le croupier prend sa deuxième carte
            Croupier.Score := Croupier.Score + ValeurNumeriqueCarte(JeuCartes[IndexCarte]);
            writeln('Le croupier prend sa deuxième carte: ', AfficherCarte(JeuCartes[IndexCarte]));
            writeln('Score du croupier: ', Croupier.Score);
            
            IndexCarte := IndexCarte + 1;
            
            // Le croupier prend des cartes supplémentaires jusqu'à avoir au moins 17
            while Croupier.Score < 17 do
            begin
              Croupier.Score := Croupier.Score + ValeurNumeriqueCarte(JeuCartes[IndexCarte]);
              writeln('Le croupier prend une carte: ', AfficherCarte(JeuCartes[IndexCarte]));
              writeln('Nouveau score du croupier: ', Croupier.Score);
              
              IndexCarte := IndexCarte + 1;
            end;
            
            // Déterminer le gagnant
            writeln;
            if (Croupier.Score > 21) or (Joueur.Score > Croupier.Score) then
            begin
              writeln('Vous gagnez!');
              Joueur.Jetons := Joueur.Jetons + 2 * Mise;
            end
            else if Joueur.Score < Croupier.Score then
            begin
              writeln('Le croupier gagne!');
            end
            else
            begin
              writeln('Égalité! Vous récupérez votre mise.');
              Joueur.Jetons := Joueur.Jetons + Mise;
            end;
          end;
          
          // Réinitialiser pour la prochaine main
          Joueur.Score := 0;
          Croupier.Score := 0;
        end;
      
      2:
        begin
          ContinuerPartie := False;
        end;
    end;
  end;
  
  if Joueur.Jetons <= 0 then
    writeln('Vous n''avez plus de jetons! Game over.');
  
  writeln;
  writeln('Merci d''avoir joué!');
  writeln('Jetons finaux: ', Joueur.Jetons);
  readln;
end;

end.
