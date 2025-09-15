{$mode objfpc}
{$H+}
program BlackJack;

uses
  SysUtils;

type
  TCarte = class
  private
    FValeur: string;
    FCouleur: string;
  public
    constructor Create(const AValeur, ACouleur: string);
    function ValeurNumerique: Integer;
    function AsString: string;
  end;

  TJoueur = class
  private
    FScore: Integer;
    FJetons: Integer;
    FCartes: Integer;
  public
    constructor Create(AScore, AJetons, ACartes: Integer);
    property Score: Integer read FScore write FScore;
    property Jetons: Integer read FJetons write FJetons;
    property Cartes: Integer read FCartes write FCartes;
    function AsString: string;
  end;

  TCroupier = class
  private
    FScore: Integer;
    FCartes: Integer;
  public
    constructor Create(AScore, ACartes: Integer);
    property Score: Integer read FScore write FScore;
    property Cartes: Integer read FCartes write FCartes;
    function AsString: string;
  end;

  TGame = class
  private
    FPot: Integer;
  public
    constructor Create;
    property Pot: Integer read FPot write FPot;
    function AsString: string;
  end;

  TDeck = array of TCarte;

{ --- Implémentations --- }

constructor TCarte.Create(const AValeur, ACouleur: string);
begin
  FValeur := AValeur;
  FCouleur := ACouleur;
end;

function TCarte.ValeurNumerique: Integer;
begin
  if FValeur = 'As' then
    Result := 11
  else if (FValeur = 'Valet') or (FValeur = 'Dame') or (FValeur = 'Roi') or (FValeur = '10') then
    Result := 10
  else
    Result := StrToIntDef(FValeur, 0);
end;

function TCarte.AsString: string;
begin
  Result := FValeur + ' de ' + FCouleur;
end;

constructor TJoueur.Create(AScore, AJetons, ACartes: Integer);
begin
  FScore := AScore;
  FJetons := AJetons;
  FCartes := ACartes;
end;

function TJoueur.AsString: string;
begin
  Result := Format('Joueur [score = %d, jetons restants = %d, cartes = %d]', [FScore, FJetons, FCartes]);
end;

constructor TCroupier.Create(AScore, ACartes: Integer);
begin
  FScore := AScore;
  FCartes := ACartes;
end;

function TCroupier.AsString: string;
begin
  Result := Format('Croupier [score = %d, carte = %d]', [FScore, FCartes]);
end;

constructor TGame.Create;
begin
  FPot := 0;
end;

function TGame.AsString: string;
begin
  Result := Format('Mise de la partie [%d]', [FPot]);
end;

{ Shuffle Fisher–Yates }
procedure Shuffle(var D: TDeck);
var
  i, j: Integer;
  tmp: TCarte;
begin
  if Length(D) <= 1 then Exit;
  for i := High(D) downto 1 do
  begin
    j := Random(i + 1);
    tmp := D[i];
    D[i] := D[j];
    D[j] := tmp;
  end;
end;

{ Récupère la carte suivante (remélange si besoin) }
function NextCard(var Deck: TDeck; var idx: Integer): TCarte;
begin
  if Length(Deck) = 0 then
    raise Exception.Create('Paquet vide');
  if idx >= Length(Deck) then
  begin
    Shuffle(Deck);
    idx := 0;
  end;
  Result := Deck[idx];
  Inc(idx);
end;

{ Lecture sûre d'un entier }
function ReadIntPrompt(const Prompt: string): Integer;
var
  s: string;
begin
  repeat
    Write(Prompt);
    ReadLn(s);
    if TryStrToInt(Trim(s), Result) then Exit
    else Writeln('Entrée invalide, entrez un nombre entier.');
  until False;
end;

{ --- Programme principal --- }
var
  Valeurs: array[0..12] of string = ('As','2','3','4','5','6','7','8','9','10','Valet','Dame','Roi');
  Couleurs: array[0..3] of string = ('Pique','Trefle','Carreau','Coeur');
  Cartes: TDeck;
  Joueur: TJoueur;
  Croupier: TCroupier;
  Game: TGame;
  i, j, k, Index, MenuChoice, Mise, Choice: Integer;
  JoueurCard1, JoueurCard2, CroupierCard1, CroupierCard2, NewCard, Card: TCarte;
  Continuer, Play: Boolean;
begin
  Randomize;

  Joueur := TJoueur.Create(0, 200, 0);
  Croupier := TCroupier.Create(0, 0);
  Game := TGame.Create;

  // Générer le paquet (6 jeux)
  SetLength(Cartes, 0);
  for i := 1 to 6 do
    for j := 0 to High(Valeurs) do
      for k := 0 to High(Couleurs) do
      begin
        SetLength(Cartes, Length(Cartes) + 1);
        Cartes[High(Cartes)] := TCarte.Create(Valeurs[j], Couleurs[k]);
      end;

  Shuffle(Cartes);

  writeln('Welcome to the BlackJack game');
  writeln('------------------------------');

  if Joueur.Jetons = 0 then
    writeln('Le joueur n''a pas assez de jetons pour jouer')
  else
  begin
    Index := 0;
    Play := True;
    while Play do
    begin
      writeln;
      writeln('1: Voulez-vous jouer ?');
      writeln('2: Quitter le jeu !');
      MenuChoice := ReadIntPrompt('Choix (1/2) : ');

      case MenuChoice of
        1:
          begin
            writeln(Joueur.AsString);
            writeln(Croupier.AsString);

            Mise := ReadIntPrompt('Quel est votre mise ? ');
            while Mise > Joueur.Jetons do
            begin
              writeln('Mise trop haute');
              Mise := ReadIntPrompt('Quel est votre mise ? ');
            end;

            Game.Pot := Mise;
            Joueur.Jetons := Joueur.Jetons - Mise;

            writeln('Distribution des cartes');

            JoueurCard1 := NextCard(Cartes, Index);
            CroupierCard1 := NextCard(Cartes, Index);
            JoueurCard2 := NextCard(Cartes, Index);
            CroupierCard2 := NextCard(Cartes, Index);

            Joueur.Score := JoueurCard1.ValeurNumerique + JoueurCard2.ValeurNumerique;
            Croupier.Score := CroupierCard1.ValeurNumerique;

            writeln('Vos cartes : ', JoueurCard1.AsString, ' et ', JoueurCard2.AsString);
            writeln('Score joueur : ', Joueur.Score);
            writeln('Carte visible du croupier : ', CroupierCard1.AsString);

            // Tour du joueur
            Continuer := True;
            while Continuer do
            begin
              writeln;
              writeln('Que voulez-vous faire ?');
              writeln('1: Tirer une autre carte');
              writeln('2: Rester');
              Choice := ReadIntPrompt('Choix (1/2) : ');

              case Choice of
                1:
                  begin
                    NewCard := NextCard(Cartes, Index);
                    Joueur.Score := Joueur.Score + NewCard.ValeurNumerique;
                    writeln('Vous tirez : ', NewCard.AsString);
                    writeln('Nouveau score joueur : ', Joueur.Score);

                    if Joueur.Score > 21 then
                    begin
                      writeln('Vous perdez d''office !');
                      Joueur.Score := 0;
                      Croupier.Score := 0;
                      Continuer := False;
                    end;
                  end;
                2:
                  begin
                    Continuer := False;
                    Croupier.Score := Croupier.Score + CroupierCard2.ValeurNumerique;
                    writeln('Le croupier révèle sa carte cachée : ', CroupierCard2.AsString);
                    writeln('Score croupier : ', Croupier.Score);
                  end;
              else
                writeln('Choix invalide.');
              end;
            end;

            // Tour du croupier si le joueur n'a pas bust
            if Joueur.Score > 0 then
            begin
              while Croupier.Score < 17 do
              begin
                Card := NextCard(Cartes, Index);
                Croupier.Score := Croupier.Score + Card.ValeurNumerique;
                writeln('Le croupier tire : ', Card.AsString);
                writeln('Score croupier : ', Croupier.Score);
              end;

              if (Croupier.Score > 21) or (Joueur.Score > Croupier.Score) then
              begin
                writeln('Vous gagnez !');
                Joueur.Jetons := Joueur.Jetons + Mise * 2;
              end
              else if Joueur.Score < Croupier.Score then
                writeln('Le croupier gagne !')
              else
              begin
                writeln('Egalite !');
                Joueur.Jetons := Joueur.Jetons + Mise;
              end;
            end;

            Joueur.Score := 0;
            Croupier.Score := 0;
          end;
        2: Play := False;
      else
        writeln('Choix invalide.');
      end;
    end;
  end;

  writeln('Merci d''avoir joué !');

  { Libération mémoire }
  for i := 0 to High(Cartes) do
    Cartes[i].Free;
  SetLength(Cartes, 0);
  Joueur.Free;
  Croupier.Free;
  Game.Free;

  readln;
end.
