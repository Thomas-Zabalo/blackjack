program BlackJackSDL;
{$mode objfpc}{$H+}

uses
  SysUtils,
  SDL2,
  SDL2_image,
  SDL2_ttf;

var
  fenetre: PSDL_Window;
  rendu: PSDL_Renderer;
  tableTexture: PSDL_Texture;
  font: PTTF_Font;
  event: TSDL_Event;
  running: Boolean;
  screenW, screenH: Integer;

procedure InitialiserSDL;
var
  modeDM: TSDL_DisplayMode;
begin
  // Initialisation SDL
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    WriteLn('Erreur SDL_Init: ', SDL_GetError);
    Halt(1);
  end;

  // Initialisation SDL_image
  if (IMG_Init(IMG_INIT_PNG) and IMG_INIT_PNG) = 0 then
  begin
    WriteLn('Erreur IMG_Init: ', IMG_GetError);
    SDL_Quit;
    Halt(1);
  end;

  // Initialisation SDL_ttf
  if TTF_Init < 0 then
  begin
    WriteLn('Erreur TTF_Init: ', TTF_GetError);
    IMG_Quit;
    SDL_Quit;
    Halt(1);
  end;

  // Obtenir la résolution de l'écran
  if SDL_GetDesktopDisplayMode(0, @modeDM) <> 0 then
  begin
    WriteLn('Erreur SDL_GetDesktopDisplayMode: ', SDL_GetError);
    TTF_Quit;
    IMG_Quit;
    SDL_Quit;
    Halt(1);
  end;

  screenW := modeDM.w;
  screenH := modeDM.h;

  // Créer la fenêtre en plein écran
  fenetre := SDL_CreateWindow('BlackJack',
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    screenW, screenH,
    SDL_WINDOW_FULLSCREEN_DESKTOP);
    
  if fenetre = nil then
  begin
    WriteLn('Erreur SDL_CreateWindow: ', SDL_GetError);
    TTF_Quit;
    IMG_Quit;
    SDL_Quit;
    Halt(1);
  end;

  // Créer le renderer
  rendu := SDL_CreateRenderer(fenetre, -1, 
    SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
    
  if rendu = nil then
  begin
    WriteLn('Erreur SDL_CreateRenderer: ', SDL_GetError);
    SDL_DestroyWindow(fenetre);
    TTF_Quit;
    IMG_Quit;
    SDL_Quit;
    Halt(1);
  end;
end;

procedure ChargerRessources;
var
  surface: PSDL_Surface;
begin
  // Charger l'image de la table
  surface := IMG_Load('table.png');
  if surface = nil then
  begin
    WriteLn('Erreur IMG_Load: ', IMG_GetError);
    WriteLn('Assurez-vous que table.png est dans le même dossier');
    // Continuer sans l'image
    tableTexture := nil;
  end
  else
  begin
    tableTexture := SDL_CreateTextureFromSurface(rendu, surface);
    SDL_FreeSurface(surface);
    if tableTexture = nil then
      WriteLn('Erreur création texture: ', SDL_GetError);
  end;

  // Charger la police
  font := TTF_OpenFont('arial.ttf', 28);
  if font = nil then
  begin
    // Essayer d'autres polices courantes
    font := TTF_OpenFont('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 28);
    if font = nil then
      font := TTF_OpenFont('/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf', 28);
    if font = nil then
      WriteLn('Warning: Police non chargée - ', TTF_GetError);
  end;
end;

procedure AfficherTable;
begin
  // Effacer l'écran
  SDL_SetRenderDrawColor(rendu, 0, 100, 0, 255); // Vert foncé pour la table
  SDL_RenderClear(rendu);

  // Afficher l'image de fond si elle existe
  if tableTexture <> nil then
  begin
    // Redimensionner l'image pour qu'elle remplisse l'écran
    SDL_RenderCopy(rendu, tableTexture, nil, nil);
  end
  else
  begin
    // Dessiner une table basique si l'image n'est pas chargée
    SDL_SetRenderDrawColor(rendu, 0, 100, 0, 255);
    SDL_RenderClear(rendu);
    
    // Dessiner le contour de la table
    SDL_SetRenderDrawColor(rendu, 139, 69, 19, 255); // Marron
    SDL_RenderDrawRect(rendu, nil);
  end;
end;

procedure AfficherMessage(message: string; x, y: Integer);
var
  surface: PSDL_Surface;
  texture: PSDL_Texture;
  rect: TSDL_Rect;
  color: TSDL_Color;
begin
  if font = nil then Exit;

  color.r := 255;
  color.g := 255;
  color.b := 255;
  color.a := 255;

  surface := TTF_RenderText_Solid(font, PChar(message), color);
  if surface <> nil then
  begin
    texture := SDL_CreateTextureFromSurface(rendu, surface);
    if texture <> nil then
    begin
      rect.x := x;
      rect.y := y;
      rect.w := surface^.w;
      rect.h := surface^.h;
      
      SDL_RenderCopy(rendu, texture, nil, @rect);
      SDL_DestroyTexture(texture);
    end;
    SDL_FreeSurface(surface);
  end;
end;

procedure LancerJeuBlackjack;
var
  quitGame: Boolean;
  gameRunning: Boolean;
begin
  quitGame := False;
  
  while not quitGame do
  begin
    // Afficher l'interface de base
    AfficherTable;
    
    // Afficher les messages de bienvenue
    AfficherMessage('BLACKJACK', screenW div 2 - 50, 50);
    AfficherMessage('Appuyez sur:', screenW div 2 - 80, 150);
    AfficherMessage('J - Jouer une partie', screenW div 2 - 100, 200);
    AfficherMessage('Q - Quitter', screenW div 2 - 60, 250);
    
    SDL_RenderPresent(rendu);

    // Attendre l'entrée du joueur
    while SDL_PollEvent(@event) <> 0 do
    begin
      case event.type_ of
        SDL_QUITEV: 
          begin
            quitGame := True;
            running := False;
          end;
          
        SDL_KEYDOWN:
          begin
            // Utiliser seulement les minuscules pour éviter les doublons
            case event.key.keysym.sym of
              SDLK_j: // J pour jouer
                begin
                  // ICI VOUS INTEGREREZ VOTRE CODE BLACKJACK EXISTANT
                  gameRunning := True;
                  while gameRunning do
                  begin
                    AfficherTable;
                    AfficherMessage('Partie en cours...', screenW div 2 - 80, screenH div 2);
                    AfficherMessage('Appuyez sur ECHAP pour retourner au menu', screenW div 2 - 180, screenH div 2 + 50);
                    SDL_RenderPresent(rendu);
                    
                    // Gérer les événements de la partie
                    if SDL_PollEvent(@event) <> 0 then
                    begin
                      case event.type_ of
                        SDL_QUITEV:
                          begin
                            gameRunning := False;
                            quitGame := True;
                            running := False;
                          end;
                        SDL_KEYDOWN:
                          begin
                            if event.key.keysym.sym = SDLK_ESCAPE then
                              gameRunning := False;
                            // Ajouter d'autres touches pour le jeu ici
                          end;
                      end;
                    end;
                    SDL_Delay(16);
                  end;
                end;
                
              SDLK_q: // Q pour quitter
                begin
                  quitGame := True;
                  running := False;
                end;
                
              SDLK_ESCAPE: // Echap pour quitter
                begin
                  quitGame := True;
                  running := False;
                end;
            end;
          end;
      end;
    end;
    
    SDL_Delay(16);
  end;
end;

procedure LibererRessources;
begin
  if tableTexture <> nil then
    SDL_DestroyTexture(tableTexture);
  if font <> nil then
    TTF_CloseFont(font);
  if rendu <> nil then
    SDL_DestroyRenderer(rendu);
  if fenetre <> nil then
    SDL_DestroyWindow(fenetre);
    
  TTF_Quit;
  IMG_Quit;
  SDL_Quit;
end;

begin
  // Initialisation
  InitialiserSDL;
  ChargerRessources;
  
  // Boucle principale
  running := True;
  LancerJeuBlackjack;
  
  // Nettoyage
  LibererRessources;
end.
