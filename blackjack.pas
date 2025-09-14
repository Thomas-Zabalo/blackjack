program BlackJack;

uses
  SysUtils;

type
  // -------- Classe Carte --------
  TCarte = class
  private
    FValeur: string;
    FCouleur: string;
  public
    constructor Create(AValeur, ACouleur: string);
    function GetValeur: string;
    function GetCouleur: string;
    function GetValeurNumerique: Integer;
    function ToString: string; override;
  end;

  // -------- Classe Joueur --------
  TJoueur = class
  private
    FScore: Integer;
    FJeton: Integer;
    FCarte: Integer;
  public
    constructor Create(AScore, AJeton, ACarte: Integer);
    property Score: Integer read FScore write FScore;
    property Jeton: Integer read FJeton write FJeton;
    property Carte: Integer read FCarte write FCarte;
    function ToString: string; override;
  end;

  // -------- Classe Croupier --------
  TCroupier = class
  private
    FScore: Integer;
    FCarte: Integer;
  public
    constructor Create(AScore, ACarte: Integer);
    property Score: Integer read FScore write FScore;
    property Carte: Integer read FCarte write FCarte;
    function ToString: string; override;
  end;

  // -------- Classe Game --------
  TGame = class
  private
    FPot: Integer;
  public
    constructor Create;
    property Pot: Integer read FPot write FPot;
    function ToString: string; override;
  end;

  // -------- Tableau de cartes --------
  TCardArray = array of TCarte;

{ ==== Implémentations ==== }

constructor TCarte.Create(AValeur, ACouleur: string);
begin
  FValeur := AValeur;
  FCouleur := ACouleur;
end;

function TCarte.GetValeur: string;
begin
  Result := FValeur;
end;

function TCarte.GetCouleur: string;
begin
  Result := FCouleur;
end;

function TCarte.GetValeurNumerique: Integer;
begin
  if FValeur = 'As' then
    Result := 11
  else if (FValeur = 'Valet') or (FValeur = 'Dame') or (FValeur = 'Roi') or (FValeur = '10') then
    Result := 10
  else
    Result := StrToInt(FValeur);
end;

function TCarte.ToString: string;
begin
  Result := FValeur + ' de ' + FCouleur;
end;

constructor TJoueur.Create(AScore, AJeton, ACarte: Integer);
begin
  FScore := AScore;
  FJeton := AJeton;
  FCarte := ACarte;
end;

function TJoueur.ToString: string;
begin
  Result := Format('Joueur [score = %d, jetons restants = %d, cartes = %d]', [FScore, FJeton, FCarte]);
end;

constructor TCroupier.Create(AScore, ACarte: Integer);
begin
  FScore := AScore;
  FCarte := ACarte;
end;

function TCroupier.ToString: string;
begin
  Result := Format('Croupier [score = %d, carte = %d]', [FScore, FCarte]);
end;

constructor TGame.Create;
begin
  FPot := 0;
end;

function TGame.ToString: string;
begin
  Result := Format('Mise de la partie [%d]', [FPot]);
end;

{ ==== Shuffle helper ==== }
procedure Shuffle(var Cards: TCardArray);
var
  i, j: Integer;
  temp: TCarte;
begin
  for i := High(Cards) downto Low(Cards) do
  begin
    j := Random(Length(Cards));
    temp := Cards[i];
    Cards[i] := Cards[j];
    Cards[j] := temp;
  end;
end;

{ ==== Programme Principal ==== }

var
  Joueur: TJoueur;
  Croupier: TCroupier;
  Game: TGame;
  Valeurs: array[0..12] of string = ('As','2','3','4','5','6','7','8','9','10','Valet','Dame','Roi');
  Couleurs: array[0..3] of string = ('Pique','Trèfle','Carreau','Cœur');
  Cartes: TCardArray;
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

  if Joueur.Jeton = 0 then
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
      readln(MenuChoice);

      case MenuChoice of
        1:
          begin
            writeln(Joueur.ToString);
            writeln(Croupier.ToString);

            writeln('Quel est votre mise ?');
            readln(Mise);

            while Mise > Joueur.Jeton do
            begin
              writeln('Mise trop haute');
              writeln('Quel est votre mise ?');
              readln(Mise);
            end;

            Game.Pot := Mise;
            Joueur.Jeton := Joueur.Jeton - Mise;

            writeln('Distribution des cartes');

            JoueurCard1 := Cartes[Index]; Inc(Index);
            CroupierCard1 := Cartes[Index]; Inc(Index);
            JoueurCard2 := Cartes[Index]; Inc(Index);
            CroupierCard2 := Cartes[Index]; Inc(Index);

            Joueur.Score := JoueurCard1.GetValeurNumerique + JoueurCard2.GetValeurNumerique;
            Croupier.Score := CroupierCard1.GetValeurNumerique;

            writeln('Vos cartes : ', JoueurCard1.ToString, ' et ', JoueurCard2.ToString);
            writeln('Score joueur : ', Joueur.Score);
            writeln('Carte visible du croupier : ', CroupierCard1.ToString);

            // Tour du joueur
            Continuer := True;
            while Continuer do
            begin
              writeln;
              writeln('Que voulez-vous faire ?');
              writeln('1: Tirer une autre carte');
              writeln('2: Rester');
              readln(Choice);

              case Choice of
                1:
                  begin
                    NewCard := Cartes[Index]; Inc(Index);
                    Joueur.Score := Joueur.Score + NewCard.GetValeurNumerique;
                    writeln('Vous tirez : ', NewCard.ToString);
                    writeln('Nouveau score joueur : ', Joueur.Score);

                    if Joueur.Score > 21 then
                    begin
                      writeln('💀 Vous perdez d''office !');
                      Joueur.Score := 0;
                      Croupier.Score := 0;
                      Continuer := False;
                    end;
                  end;
                2:
                  begin
                    Continuer := False;
                    Croupier.Score := Croupier.Score + CroupierCard2.GetValeurNumerique;
                    writeln('Le croupier révèle sa carte cachée : ', CroupierCard2.ToString);
                    writeln('Score croupier : ', Croupier.Score);
                  end;
              end;
            end;

            // Tour du croupier si le joueur n’a pas dépassé
            if Joueur.Score > 0 then
            begin
              while Croupier.Score < 17 do
              begin
                Card := Cartes[Index]; Inc(Index);
                Croupier.Score := Croupier.Score + Card.GetValeurNumerique;
                writeln('Le croupier tire : ', Card.ToString);
                writeln('Score croupier : ', Croupier.Score);
              end;

              if (Croupier.Score > 21) or (Joueur.Score > Croupier.Score) then
              begin
                writeln('🎉 Vous gagnez !');
                Joueur.Jeton := Joueur.Jeton + Mise * 2;
              end
              else if Joueur.Score < Croupier.Score then
              begin
                writeln('Le croupier gagne !');
              end
              else
              begin
                writeln('Égalité !');
                Joueur.Jeton := Joueur.Jeton + Mise; // On rend la mise
              end;
            end;

            Joueur.Score := 0;
            Croupier.Score := 0;
          end;

        2:
          Play := False;
      end;
    end;
  end;

  writeln('Merci d''avoir joué !');
  readln;
end.
