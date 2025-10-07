unit Machineasous;

interface

uses
  SDL2, SDL2_ttf, SDL2_image, SysUtils;
  

procedure LancerMachineaSous(renderer: PSDL_Renderer; font: PTTF_Font);

implementation

type
// -------- Type pour un symbole --------
 TSymbole = record
    symbole: array[0..2] of string 
  end;

  // -------- Type pour un joueur --------
  TJoueur = record
    Victoire: Boolean;
    Jetons: Integer;
    nombresymboles:integer;
  end;

  // -------- Type pour la partie --------
  TPartie = record
    MiseTotale: Integer;
  end;
  
Const
 ValeurSymbole: array[0..2] of TSymbole = ('☠','€','★')
 
 var
  JeuSymboles: array of TSymbole;
  IndexSymbole: Integer;
  
// Fonction pour crÃ©er un symbole
function CreerSymbole(Symbole: string): TSymbole;
begin
  Result.Symbole := Symbole;
end;

 
 
 // Fonction pour afficher une carte
function AfficherSymbole(Symbole: TSymbole): string;
var
Symbole1, Symbole2, Symbole3: TSymbole;
begin
  Result := Symbole1 + ' et ' + Symbole2 + 'et' + Symbole3 ;
end;

// ProcÃ©dure pour DONNER LES SYMBOLES
procedure DonneSymbole (var Symboles: array of TSymbole);
var
  i, j: Integer;
  Temp: TSymbole;
begin
  for i := High(Symboles) downto Low(Symboles) do
  begin
    j := Random(Length(Symboles));
    Temp := Symboles[i];
    Symboles[i] := Symboles[j];
    Symboles[j] := Temp;
  end;
end;

// ProcÃ©dure pour initialiser le jeu
procedure InitialiserJeu(var Joueur: TJoueur; var Partie: TPartie);
var
  i, j, k: Integer;
begin
  // CrÃ©er le joueur avec 200 jetons
  Joueur.Score := 0;
  Joueur.Jetons := 200;
  Joueur.NombreSymboles := 0;
  
  // Initialiser la partie
  Partie.MiseTotale := 0;
  
  // MÃ©langer les symboles
  DonneSymbole(JeuSymboles);
  IndexSymbole := 0;
end;

// ProcÃ©dure pour afficher l'Ã©tat du jeu
procedure AfficherEtat(Joueur: TJoueur; Partie: TPartie);
begin
  writeln('Votre score: ', Joueur.Score);
  writeln('Vos jetons: ', Joueur.Jetons);
  writeln('Mise actuelle: ', Partie.MiseTotale);
end;

procedure LancerMachineaSous(renderer: PSDL_Renderer; font: PTTF_Font);
var
  Joueur: TJoueur;
  Partie: TPartie;
  ChoixMenu, Mise, Choix: Integer;
  ContinuerPartie: Boolean;
begin
  Randomize;
  InitialiserJeu(Joueur, Partie);
  
  writeln('Bienvenue au jeu de la Machine Ã  Sous!');
  writeln('------------------------------');
  
  ContinuerPartie := True;
  
  while ContinuerPartie and (Joueur.Jetons > 0) do
  begin
    writeln;
    writeln('Que voulez-vous faire?');
    writeln('1: Jouer une manche');
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
          
          // VÃ©rifier que la mise est valide
          while Mise > Joueur.Jetons do
          begin
            writeln('Vous n''avez pas assez de jetons!');
            write('Combien voulez-vous miser? ');
            readln(Mise);
          end;
          
          // DÃ©duire la mise des jetons du joueur
          Joueur.Jetons := Joueur.Jetons - Mise;
          Partie.MiseTotale := Mise;
          
          // Lancer le jeu
          writeln;
          writeln('Lancer le jeu...');
          
          
          
          
         // Symbole du joueur
          Joueur.Score := ValeurNumeriquesymbole(JeuSymbole[IndexSymbole]) +
                          ValeurNumeriqueSymbole(JeuSymbole[IndexSymbole+1]);
          writeln('Vos cartes: ', AfficherSymbole(JeuSymbole[IndexSymbole]), ' et ',
                  AfficherSymbole(JeuSymboles[IndexSymbole+1]));
          writeln('Votre score: ', Joueur.Score);
          
          
          // Tour du joueur
          writeln;
          writeln('--- Votre tour ---');
          while True do
          begin
            writeln;
            writeln('Que voulez-vous faire?');
            writeln('1: Lancer la machine');
            writeln('2: Arrêter');
            write('Votre choix: ');
            readln(Choix);
            
            if Choix = 1 then
            begin
              // Lancer la machine 
              Joueur.Score := Joueur.Score + ValeurNumeriqueSymbole(JeuSymbole[Indexsymbole]);
              writeln('Vous recevez: ', AfficherSymbole(JeuSymbole[indexSmbole]));
              writeln('Nouveau score: ', Joueur.Score);
              
              IndexSymbole := IndexSymbole + 1;
              
            
            // Détermine gagnant
            writeln;
            if (Joueur.Symbole = '€') then
            begin
              writeln('Vous gagnez!');
              Joueur.Jetons := Joueur.Jetons + 2 * Mise;
            end
            else if Joueur.Symbole = '★' or Joueur.Symbole = '☠' then
            begin
              writeln('Vous avez perdu!');
            end
          end;
          
          // Réiniialisaion pour la prochaine main
          Joueur.Score := 0;
         
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
  writeln('Merci d''avoir jouer!');
  writeln('Jetons finaux: ', Joueur.Jetons);
  readln;
end;

end.




