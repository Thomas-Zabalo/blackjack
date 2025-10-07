program MenuCasino;

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  SDL2, SDL2_image, SDL2_ttf, SysUtils, blackJack;

type
  TGameState = (MENU, JEU_BLACKJACK, JEU_ROULETTE, JEU_SLOTS);

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  event: TSDL_Event;
  running: Boolean;

  font: PTTF_Font;
  textColor: TSDL_Color;

  // Rectangles des boutons
  btnBlackjackRect, btnRouletteRect, btnSlotsRect, btnQuitRect: TSDL_Rect;

  // Textes
  texteBlackjack: string = 'Blackjack';
  texteRoulette: string = 'Roulette';
  texteSlots: string = 'Machine à sous';
  texteQuit: string = 'Quitter';

  etatJeu: TGameState;
  mouseX, mouseY: Integer;
  screenW, screenH: Integer;

  // Pour le texte SDL
  textSurface: PSDL_Surface;
  textTexture: PSDL_Texture;
  textRect: TSDL_Rect;

procedure DessinerTexte(s: AnsiString; x, y: Integer; alignCenter: Boolean);
begin
  textSurface := TTF_RenderText_Blended(font, PAnsiChar(s), textColor);
  if textSurface <> nil then
  begin
    textTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
    textRect.w := textSurface^.w;
    textRect.h := textSurface^.h;
    if alignCenter then
      textRect.x := x - textRect.w div 2
    else
      textRect.x := x;
    textRect.y := y;
    SDL_RenderCopy(renderer, textTexture, nil, @textRect);
    SDL_FreeSurface(textSurface);
    SDL_DestroyTexture(textTexture);
  end;
end;

// Surcharge pour l'alignement par défaut à gauche
procedure DessinerTexte(s: AnsiString; x, y: Integer);
begin
  DessinerTexte(s, x, y, False);
end;

procedure DessinerBouton(rect: TSDL_Rect; texte: string);
begin
  SDL_SetRenderDrawColor(renderer, 70, 70, 150, 255);
  SDL_RenderFillRect(renderer, @rect);
  SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
  SDL_RenderDrawRect(renderer, @rect);
  DessinerTexte(texte, rect.x + rect.w div 2, rect.y + rect.h div 2 - 12, True);
end;

procedure LancerBlackjackPleinEcran;
var
  fenetreBJ: PSDL_Window;
  renduBJ: PSDL_Renderer;
  fontBJ: PTTF_Font;
  modeDM: TSDL_DisplayMode;
begin
  // Obtenir la résolution de l'écran
  if SDL_GetDesktopDisplayMode(0, @modeDM) <> 0 then
  begin
    WriteLn('Erreur SDL_GetDesktopDisplayMode: ', SDL_GetError);
    Exit;
  end;

  screenW := modeDM.w;
  screenH := modeDM.h;

  // Créer une nouvelle fenêtre en plein écran pour le Blackjack
  fenetreBJ := SDL_CreateWindow('BlackJack - Plein Ecran',
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    screenW, screenH,
    SDL_WINDOW_FULLSCREEN_DESKTOP);
    
  if fenetreBJ = nil then
  begin
    WriteLn('Erreur création fenêtre Blackjack: ', SDL_GetError);
    Exit;
  end;

  // Créer le renderer pour le Blackjack
  renduBJ := SDL_CreateRenderer(fenetreBJ, -1, 
    SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
    
  if renduBJ = nil then
  begin
    WriteLn('Erreur création renderer Blackjack: ', SDL_GetError);
    SDL_DestroyWindow(fenetreBJ);
    Exit;
  end;

  // Charger une police pour le Blackjack
  fontBJ := TTF_OpenFont('arial.ttf', 24);
  if fontBJ = nil then
    fontBJ := TTF_OpenFont('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 24);

  // Lancer le vrai jeu Blackjack
  LancerBlackjack(renduBJ, fontBJ);

  // Nettoyage
  if fontBJ <> nil then
    TTF_CloseFont(fontBJ);
  if renduBJ <> nil then
    SDL_DestroyRenderer(renduBJ);
  if fenetreBJ <> nil then
    SDL_DestroyWindow(fenetreBJ);
end;

procedure AfficherMenuPrincipal;
begin
  // Fond bleu nuit
  SDL_SetRenderDrawColor(renderer, 30, 30, 60, 255);
  SDL_RenderClear(renderer);

  // Titre du casino
  DessinerTexte('CASINO VIRTUEL', screenW div 2, 100, True);

  // Bouton Blackjack
  DessinerBouton(btnBlackjackRect, texteBlackjack);

  // Bouton Roulette
  DessinerBouton(btnRouletteRect, texteRoulette);

  // Bouton Machine à sous
  DessinerBouton(btnSlotsRect, texteSlots);

  // Bouton Quitter
  DessinerBouton(btnQuitRect, texteQuit);
end;

var
  modeDM: TSDL_DisplayMode;
  btnWidth, btnHeight, btnSpacing, totalHeight, startY: Integer;

begin
  // --- Initialisation SDL ---
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then 
  begin
    Writeln('Erreur SDL_Init: ', SDL_GetError());
    Halt(1);
  end;
  
  if (IMG_Init(IMG_INIT_PNG) and IMG_INIT_PNG) = 0 then 
  begin
    Writeln('Erreur IMG_Init: ', IMG_GetError());
    Halt(1);
  end;
  
  if TTF_Init() <> 0 then 
  begin
    Writeln('Erreur TTF_Init: ', TTF_GetError());
    Halt(1);
  end;

  // Obtenir la résolution de l'écran pour le plein écran
  if SDL_GetDesktopDisplayMode(0, @modeDM) = 0 then
  begin
    screenW := modeDM.w;
    screenH := modeDM.h;
  end
  else
  begin
    screenW := 800;
    screenH := 600;
  end;

  // Créer la fenêtre en plein écran
  window := SDL_CreateWindow('Casino Virtuel', 
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    screenW, screenH, SDL_WINDOW_FULLSCREEN_DESKTOP);
    
  if window = nil then
  begin
    Writeln('Erreur création fenêtre: ', SDL_GetError());
    Halt(1);
  end;

  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if renderer = nil then
  begin
    Writeln('Erreur création renderer: ', SDL_GetError());
    Halt(1);
  end;

  // Chargement de la police
  font := TTF_OpenFont('arial.ttf', 24);
  if font = nil then
  begin
    font := TTF_OpenFont('/usr/share/fonts/truetype/freefont/FreeSans.ttf', 24);
    if font = nil then
    begin
      Writeln('Erreur chargement police: ', TTF_GetError());
    end;
  end;

  textColor.r := 255; textColor.g := 255; textColor.b := 255; textColor.a := 255;

  // Position des boutons du menu (centrés)
  btnWidth := 300;
  btnHeight := 80;
  btnSpacing := 20;
  totalHeight := 4 * btnHeight + 3 * btnSpacing;
  startY := (screenH - totalHeight) div 2;
  
  btnBlackjackRect.x := (screenW - btnWidth) div 2;
  btnBlackjackRect.y := startY;
  btnBlackjackRect.w := btnWidth;
  btnBlackjackRect.h := btnHeight;
  
  btnRouletteRect.x := (screenW - btnWidth) div 2;
  btnRouletteRect.y := startY + btnHeight + btnSpacing;
  btnRouletteRect.w := btnWidth;
  btnRouletteRect.h := btnHeight;
  
  btnSlotsRect.x := (screenW - btnWidth) div 2;
  btnSlotsRect.y := startY + 2 * (btnHeight + btnSpacing);
  btnSlotsRect.w := btnWidth;
  btnSlotsRect.h := btnHeight;
  
  btnQuitRect.x := (screenW - btnWidth) div 2;
  btnQuitRect.y := startY + 3 * (btnHeight + btnSpacing);
  btnQuitRect.w := btnWidth;
  btnQuitRect.h := btnHeight;

  etatJeu := MENU;
  running := True;

  while running do
  begin
    while SDL_PollEvent(@event) <> 0 do
    begin
      case event.type_ of
        SDL_QUITEV: running := False;

        SDL_KEYDOWN:
          if event.key.keysym.sym = SDLK_ESCAPE then
            running := False;

        SDL_MOUSEBUTTONDOWN:
          if event.button.button = SDL_BUTTON_LEFT then
          begin
            mouseX := event.button.x;
            mouseY := event.button.y;

            if etatJeu = MENU then
            begin
              // Blackjack - Lance votre jeu existant
              if (mouseX >= btnBlackjackRect.x) and (mouseX <= btnBlackjackRect.x + btnBlackjackRect.w) and
                 (mouseY >= btnBlackjackRect.y) and (mouseY <= btnBlackjackRect.y + btnBlackjackRect.h) then
              begin
                LancerBlackjackPleinEcran;
              end;

              // Roulette
              if (mouseX >= btnRouletteRect.x) and (mouseX <= btnRouletteRect.x + btnRouletteRect.w) and
                 (mouseY >= btnRouletteRect.y) and (mouseY <= btnRouletteRect.y + btnRouletteRect.h) then
              begin
                etatJeu := JEU_ROULETTE;
              end;

              // Machine à sous
              if (mouseX >= btnSlotsRect.x) and (mouseX <= btnSlotsRect.x + btnSlotsRect.w) and
                 (mouseY >= btnSlotsRect.y) and (mouseY <= btnSlotsRect.y + btnSlotsRect.h) then
              begin
                etatJeu := JEU_SLOTS;
              end;

              // Quitter
              if (mouseX >= btnQuitRect.x) and (mouseX <= btnQuitRect.x + btnQuitRect.w) and
                 (mouseY >= btnQuitRect.y) and (mouseY <= btnQuitRect.y + btnQuitRect.h) then
                running := False;
            end;
          end;
      end;
    end;

    // --- Dessin ---
    case etatJeu of
      MENU:
        begin
          AfficherMenuPrincipal;
        end;

      JEU_ROULETTE:
        begin
          SDL_SetRenderDrawColor(renderer, 50, 50, 50, 255);
          SDL_RenderClear(renderer);
          DessinerTexte('ROULETTE - EN DEVELOPPEMENT', screenW div 2, screenH div 2, True);
          SDL_RenderPresent(renderer);
          SDL_Delay(2000);
          etatJeu := MENU;
        end;

      JEU_SLOTS:
        begin
          SDL_SetRenderDrawColor(renderer, 80, 0, 80, 255);
          SDL_RenderClear(renderer);
          DessinerTexte('MACHINE A SOUS - EN DEVELOPPEMENT', screenW div 2, screenH div 2, True);
          SDL_RenderPresent(renderer);
          SDL_Delay(2000);
          etatJeu := MENU;
        end;
    end;

    SDL_RenderPresent(renderer);
    SDL_Delay(16);
  end;

  // Nettoyage
  if font <> nil then TTF_CloseFont(font);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  TTF_Quit();
  IMG_Quit();
  SDL_Quit();
end.
