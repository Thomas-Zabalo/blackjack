unit BlackJack;

{$mode objfpc}{$H+}

interface

uses
  SDL2, SDL2_ttf, SysUtils;

procedure LancerBlackjack(renderer: PSDL_Renderer; font: PTTF_Font);

implementation

type
  TCarte = record
    Valeur: string;
    Couleur: string;
  end;

  TJoueur = record
    Score: Integer;
    Jetons: Integer;
    Cartes: array of TCarte;
  end;

  TCroupier = record
    Score: Integer;
    Cartes: array of TCarte;
    PremiereCarteCachee: Boolean;
  end;

  TPartieEtat = (ETAT_MISE, ETAT_JEU, ETAT_RESULTAT);

const
  ValeursCartes: array[0..12] of string = ('As','2','3','4','5','6','7','8','9','10','Valet','Dame','Roi');
  CouleursCartes: array[0..3] of string = ('Pique','Trèfle','Carreau','Cœur');

var
  JeuCartes: array of TCarte;
  IndexCarte: Integer;
  Joueur: TJoueur;
  Croupier: TCroupier;
  PartieEtat: TPartieEtat;
  MiseActuelle: Integer;

// Fonctions de base du jeu
function CreerCarte(Valeur, Couleur: string): TCarte;
begin
  Result.Valeur := Valeur;
  Result.Couleur := Couleur;
end;

function ValeurNumeriqueCarte(Carte: TCarte): Integer;
begin
  if Carte.Valeur = 'As' then
    Result := 11
  else if (Carte.Valeur = 'Valet') or (Carte.Valeur = 'Dame') or (Carte.Valeur = 'Roi') or (Carte.Valeur = '10') then
    Result := 10
  else
    Result := StrToInt(Carte.Valeur);
end;

procedure MelangerCartes;
var
  i, j: Integer;
  Temp: TCarte;
begin
  for i := High(JeuCartes) downto Low(JeuCartes) do
  begin
    j := Random(Length(JeuCartes));
    Temp := JeuCartes[i];
    JeuCartes[i] := JeuCartes[j];
    JeuCartes[j] := Temp;
  end;
end;

procedure InitialiserJeu;
var
  i, j: Integer;
begin
  // Créer le jeu de cartes (1 paquet pour simplifier)
  SetLength(JeuCartes, 52);
  for i := 0 to 12 do
    for j := 0 to 3 do
      JeuCartes[i*4 + j] := CreerCarte(ValeursCartes[i], CouleursCartes[j]);
  
  MelangerCartes;
  IndexCarte := 0;
  
  // Initialiser joueur
  SetLength(Joueur.Cartes, 0);
  Joueur.Score := 0;
  Joueur.Jetons := 200;
  
  // Initialiser croupier
  SetLength(Croupier.Cartes, 0);
  Croupier.Score := 0;
  Croupier.PremiereCarteCachee := True;
  
  // Initialiser partie
  MiseActuelle := 0;
  PartieEtat := ETAT_MISE;
end;

function CalculerScore(Cartes: array of TCarte): Integer;
var
  i, score, asCount: Integer;
begin
  score := 0;
  asCount := 0;
  
  for i := 0 to High(Cartes) do
  begin
    score := score + ValeurNumeriqueCarte(Cartes[i]);
    if Cartes[i].Valeur = 'As' then
      asCount := asCount + 1;
  end;
  
  // Gérer les As (11 ou 1 point)
  while (score > 21) and (asCount > 0) do
  begin
    score := score - 10;
    asCount := asCount - 1;
  end;
  
  Result := score;
end;

procedure DistribuerCartesInitiales;
begin
  // Distribuer 2 cartes au joueur
  SetLength(Joueur.Cartes, 2);
  Joueur.Cartes[0] := JeuCartes[IndexCarte]; Inc(IndexCarte);
  Joueur.Cartes[1] := JeuCartes[IndexCarte]; Inc(IndexCarte);
  Joueur.Score := CalculerScore(Joueur.Cartes);
  
  // Distribuer 2 cartes au croupier
  SetLength(Croupier.Cartes, 2);
  Croupier.Cartes[0] := JeuCartes[IndexCarte]; Inc(IndexCarte);
  Croupier.Cartes[1] := JeuCartes[IndexCarte]; Inc(IndexCarte);
  Croupier.Score := CalculerScore(Croupier.Cartes);
  
  Croupier.PremiereCarteCachee := True;
  PartieEtat := ETAT_JEU;
end;

procedure JoueurTirerCarte;
begin
  SetLength(Joueur.Cartes, Length(Joueur.Cartes) + 1);
  Joueur.Cartes[High(Joueur.Cartes)] := JeuCartes[IndexCarte];
  Inc(IndexCarte);
  Joueur.Score := CalculerScore(Joueur.Cartes);
  
  if Joueur.Score > 21 then
  begin
    PartieEtat := ETAT_RESULTAT;
    Croupier.PremiereCarteCachee := False;
  end;
end;

procedure CroupierJouer;
begin
  Croupier.PremiereCarteCachee := False;
  
  // Le croupier tire jusqu'à 17 ou plus
  while Croupier.Score < 17 do
  begin
    SetLength(Croupier.Cartes, Length(Croupier.Cartes) + 1);
    Croupier.Cartes[High(Croupier.Cartes)] := JeuCartes[IndexCarte];
    Inc(IndexCarte);
    Croupier.Score := CalculerScore(Croupier.Cartes);
  end;
  
  PartieEtat := ETAT_RESULTAT;
end;

procedure DeterminerResultat;
begin
  if Joueur.Score > 21 then
    // Joueur a dépassé 21, il perd
  else if Croupier.Score > 21 then
  begin
    // Croupier dépasse 21, joueur gagne
    Joueur.Jetons := Joueur.Jetons + MiseActuelle * 2;
  end
  else if Joueur.Score > Croupier.Score then
  begin
    // Joueur a un meilleur score
    Joueur.Jetons := Joueur.Jetons + MiseActuelle * 2;
  end
  else if Croupier.Score > Joueur.Score then
  begin
    // Croupier a un meilleur score, joueur perd sa mise
  end
  else
  begin
    // Égalité, joueur récupère sa mise
    Joueur.Jetons := Joueur.Jetons + MiseActuelle;
  end;
end;

procedure NouvelleMain;
begin
  SetLength(Joueur.Cartes, 0);
  SetLength(Croupier.Cartes, 0);
  Joueur.Score := 0;
  Croupier.Score := 0;
  MiseActuelle := 0;
  PartieEtat := ETAT_MISE;
  
  // Si on arrive à la fin du paquet, on remélange
  if IndexCarte > 40 then
  begin
    MelangerCartes;
    IndexCarte := 0;
  end;
end;

// Fonctions d'affichage SDL2
procedure DessinerTexte(renderer: PSDL_Renderer; font: PTTF_Font; texte: string; x, y: Integer; alignCenter: Boolean);
var
  surface: PSDL_Surface;
  texture: PSDL_Texture;
  rect: TSDL_Rect;
  color: TSDL_Color;
begin
  color.r := 255; color.g := 255; color.b := 255; color.a := 255;
  
  surface := TTF_RenderText_Blended(font, PChar(texte), color);
  if surface <> nil then
  begin
    texture := SDL_CreateTextureFromSurface(renderer, surface);
    rect.w := surface^.w;
    rect.h := surface^.h;
    if alignCenter then
      rect.x := x - rect.w div 2
    else
      rect.x := x;
    rect.y := y;
    SDL_RenderCopy(renderer, texture, nil, @rect);
    SDL_FreeSurface(surface);
    SDL_DestroyTexture(texture);
  end;
end;

procedure DessinerCarte(renderer: PSDL_Renderer; font: PTTF_Font; carte: TCarte; x, y: Integer; cachee: Boolean);
var
  rect: TSDL_Rect;
  texte: string;
begin
  // Rectangle de la carte
  rect.x := x;
  rect.y := y;
  rect.w := 80;
  rect.h := 120;
  
  if cachee then
  begin
    SDL_SetRenderDrawColor(renderer, 0, 0, 139, 255); // Bleu foncé pour carte cachée
    SDL_RenderFillRect(renderer, @rect);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    SDL_RenderDrawRect(renderer, @rect);
    DessinerTexte(renderer, font, '?', x + 40, y + 60, True);
  end
  else
  begin
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255); // Blanc pour carte visible
    SDL_RenderFillRect(renderer, @rect);
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderDrawRect(renderer, @rect);
    
    texte := carte.Valeur + ' ' + Copy(carte.Couleur, 1, 1);
    DessinerTexte(renderer, font, texte, x + 40, y + 60, True);
  end;
end;

procedure AfficherJeu(renderer: PSDL_Renderer; font: PTTF_Font);
var
  i: Integer;
  message, scoreCroupier: string;
begin
  // Fond vert de la table
  SDL_SetRenderDrawColor(renderer, 0, 100, 0, 255);
  SDL_RenderClear(renderer);
  
  // Titre
  DessinerTexte(renderer, font, 'BLACKJACK', 400, 20, True);
  
  // Informations joueur
  DessinerTexte(renderer, font, 'Jetons: ' + IntToStr(Joueur.Jetons), 20, 20, False);
  DessinerTexte(renderer, font, 'Mise: ' + IntToStr(MiseActuelle), 20, 50, False);
  
  // Score du croupier
  if Croupier.PremiereCarteCachee then
    scoreCroupier := '?'
  else
    scoreCroupier := IntToStr(Croupier.Score);
  
  DessinerTexte(renderer, font, 'Croupier - Score: ' + scoreCroupier, 400, 100, True);
  
  // Cartes du croupier
  for i := 0 to High(Croupier.Cartes) do
  begin
    if (i = 0) and Croupier.PremiereCarteCachee then
      DessinerCarte(renderer, font, Croupier.Cartes[i], 300 + i * 90, 130, True)
    else
      DessinerCarte(renderer, font, Croupier.Cartes[i], 300 + i * 90, 130, False);
  end;
  
  // Cartes du joueur
  DessinerTexte(renderer, font, 'Votre main - Score: ' + IntToStr(Joueur.Score), 400, 300, True);
  
  for i := 0 to High(Joueur.Cartes) do
    DessinerCarte(renderer, font, Joueur.Cartes[i], 300 + i * 90, 330, False);
  
  // Messages selon l'état du jeu
  case PartieEtat of
    ETAT_MISE:
      begin
        DessinerTexte(renderer, font, 'MISEZ: 1=10, 2=25, 3=50, 4=100 - ESPACE pour valider', 400, 500, True);
        DessinerTexte(renderer, font, 'ECHAP pour quitter', 400, 530, True);
      end;
    
    ETAT_JEU:
      begin
        DessinerTexte(renderer, font, 'H: Hit (Prendre une carte)  S: Stand (Rester)', 400, 500, True);
        DessinerTexte(renderer, font, 'ECHAP pour quitter', 400, 530, True);
        
        if Joueur.Score = 21 then
          DessinerTexte(renderer, font, 'BLACKJACK!', 400, 470, True);
      end;
    
    ETAT_RESULTAT:
      begin
        if Joueur.Score > 21 then
          message := 'VOUS AVEZ DEPASSE 21! VOUS PERDEZ.'
        else if Croupier.Score > 21 then
          message := 'CROUPIER DEPASSE 21! VOUS GAGNEZ!'
        else if Joueur.Score > Croupier.Score then
          message := 'VOUS GAGNEZ!'
        else if Croupier.Score > Joueur.Score then
          message := 'CROUPIER GAGNE!'
        else
          message := 'EGALITE!';
        
        DessinerTexte(renderer, font, message, 400, 470, True);
        DessinerTexte(renderer, font, 'ESPACE pour nouvelle main - ECHAP pour quitter', 400, 500, True);
      end;
  end;
end;

procedure LancerBlackjack(renderer: PSDL_Renderer; font: PTTF_Font);
var
  event: TSDL_Event;
  quit: Boolean;
begin
  Randomize;
  InitialiserJeu;
  quit := False;
  
  while not quit do
  begin
    while SDL_PollEvent(@event) <> 0 do
    begin
      case event.type_ of
        SDL_QUITEV: 
          quit := True;
        
        SDL_KEYDOWN:
          begin
            case event.key.keysym.sym of
              SDLK_ESCAPE:
                quit := True;
              
              SDLK_SPACE:
                begin
                  case PartieEtat of
                    ETAT_MISE:
                      if MiseActuelle > 0 then
                        DistribuerCartesInitiales;
                    ETAT_RESULTAT:
                      NouvelleMain;
                  end;
                end;
              
              // Mises
              SDLK_1: if PartieEtat = ETAT_MISE then MiseActuelle := 10;
              SDLK_2: if PartieEtat = ETAT_MISE then MiseActuelle := 25;
              SDLK_3: if PartieEtat = ETAT_MISE then MiseActuelle := 50;
              SDLK_4: if PartieEtat = ETAT_MISE then MiseActuelle := 100;
              
              // Actions de jeu
              SDLK_h: if PartieEtat = ETAT_JEU then JoueurTirerCarte;
              SDLK_s: if PartieEtat = ETAT_JEU then
                      begin
                        CroupierJouer;
                        DeterminerResultat;
                      end;
            end;
          end;
      end;
    end;
    
    AfficherJeu(renderer, font);
    SDL_RenderPresent(renderer);
    SDL_Delay(16);
  end;
end;

end.
