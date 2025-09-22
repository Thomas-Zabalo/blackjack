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
  btnBlackjackRect, btnRouletteRect, btnSlotsRect, btnQuitRect, titleRect: TSDL_Rect;
  tempRect: TSDL_Rect; // Rectangle temporaire pour les messages

  // Textes
  texteBlackjack: string = 'Blackjack';
  texteRoulette: string = 'Roulette';
  texteSlots: string = 'Machine à sous';
  texteQuit: string = 'Quitter';

  etatJeu: TGameState;
  mouseX, mouseY: Integer;

  // Pour le texte SDL
  textSurface: PSDL_Surface;
  textTexture: PSDL_Texture;
  textRect: TSDL_Rect;

procedure DessinerTexte(s: AnsiString; rect: TSDL_Rect);
begin
  textSurface := TTF_RenderText_Blended(font, PAnsiChar(s), textColor);
  if textSurface <> nil then
  begin
    textTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
    textRect.w := textSurface^.w;
    textRect.h := textSurface^.h;
    textRect.x := rect.x + (rect.w - textRect.w) div 2;
    textRect.y := rect.y + (rect.h - textRect.h) div 2;
    SDL_RenderCopy(renderer, textTexture, nil, @textRect);
    SDL_FreeSurface(textSurface);
    SDL_DestroyTexture(textTexture);
  end;
end;

begin
  // --- Initialisation SDL ---
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then 
  begin
    Writeln('Erreur SDL_Init: ', SDL_GetError());
    Halt(1);
  end;
  
  if (IMG_Init(IMG_INIT_PNG) and IMG_INIT_PNG) = 0 then 
  begin
    Writeln('Erreur IMG_Init: ', SDL_GetError());
    Halt(1);
  end;
  
  if TTF_Init() <> 0 then 
  begin
    Writeln('Erreur TTF_Init: ', SDL_GetError());
    Halt(1);
  end;

  window := SDL_CreateWindow('Menu Casino', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                             800, 600, SDL_WINDOW_SHOWN);
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
      Writeln('Erreur chargement police: ', SDL_GetError());
      Writeln('Essayez d''installer les polices: sudo apt-get install ttf-mscorefonts-installer');
    end;
  end;

  textColor.r := 255; textColor.g := 255; textColor.b := 255; textColor.a := 255;

  // Position des boutons (alignés verticalement)
  btnBlackjackRect.x := 300; btnBlackjackRect.y := 150; btnBlackjackRect.w := 200; btnBlackjackRect.h := 60;
  btnRouletteRect.x := 300; btnRouletteRect.y := 230; btnRouletteRect.w := 200; btnRouletteRect.h := 60;
  btnSlotsRect.x := 300; btnSlotsRect.y := 310; btnSlotsRect.w := 200; btnSlotsRect.h := 60;
  btnQuitRect.x := 300; btnQuitRect.y := 390; btnQuitRect.w := 200; btnQuitRect.h := 60;
  titleRect.x := 250; titleRect.y := 50; titleRect.w := 300; titleRect.h := 80;

  etatJeu := MENU;
  running := True;

  while running do
  begin
    while SDL_PollEvent(@event) <> 0 do
    begin
      case event.type_ of
        SDL_QUITEV: running := False;

        SDL_MOUSEBUTTONDOWN:
          if event.button.button = SDL_BUTTON_LEFT then
          begin
            mouseX := event.button.x;
            mouseY := event.button.y;

            if etatJeu = MENU then
            begin
              // Blackjack
              if (mouseX >= btnBlackjackRect.x) and (mouseX <= btnBlackjackRect.x + btnBlackjackRect.w) and
                 (mouseY >= btnBlackjackRect.y) and (mouseY <= btnBlackjackRect.y + btnBlackjackRect.h) then
                etatJeu := JEU_BLACKJACK;

              // Roulette
              if (mouseX >= btnRouletteRect.x) and (mouseX <= btnRouletteRect.x + btnRouletteRect.w) and
                 (mouseY >= btnRouletteRect.y) and (mouseY <= btnRouletteRect.y + btnRouletteRect.h) then
              begin
                etatJeu := JEU_ROULETTE;
                Writeln('Roulette (pas encore codé)');
              end;

              // Machine à sous
              if (mouseX >= btnSlotsRect.x) and (mouseX <= btnSlotsRect.x + btnSlotsRect.w) and
                 (mouseY >= btnSlotsRect.y) and (mouseY <= btnSlotsRect.y + btnSlotsRect.h) then
              begin
                etatJeu := JEU_SLOTS;
                Writeln('Machine à sous (pas encore codé)');
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
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    case etatJeu of
      MENU:
        begin
          // Dessiner un fond pour le menu
          SDL_SetRenderDrawColor(renderer, 30, 30, 60, 255);
          SDL_RenderClear(renderer);

          // Titre du casino
          SDL_SetRenderDrawColor(renderer, 100, 0, 0, 255);
          SDL_RenderFillRect(renderer, @titleRect);
          DessinerTexte('CASINO VIRTUEL', titleRect);

          // Bouton Blackjack
          SDL_SetRenderDrawColor(renderer, 0, 100, 200, 255);
          SDL_RenderFillRect(renderer, @btnBlackjackRect);
          DessinerTexte(texteBlackjack, btnBlackjackRect);

          // Bouton Roulette
          SDL_SetRenderDrawColor(renderer, 0, 200, 0, 255);
          SDL_RenderFillRect(renderer, @btnRouletteRect);
          DessinerTexte(texteRoulette, btnRouletteRect);

          // Bouton Machine à sous
          SDL_SetRenderDrawColor(renderer, 200, 100, 0, 255);
          SDL_RenderFillRect(renderer, @btnSlotsRect);
          DessinerTexte(texteSlots, btnSlotsRect);

          // Bouton Quitter
          SDL_SetRenderDrawColor(renderer, 200, 0, 0, 255);
          SDL_RenderFillRect(renderer, @btnQuitRect);
          DessinerTexte(texteQuit, btnQuitRect);
        end;

      JEU_BLACKJACK:
        begin
          LancerBlackjack(renderer, font);
          etatJeu := MENU;
        end;

      JEU_ROULETTE:
        begin
          // écran placeholder
          SDL_SetRenderDrawColor(renderer, 50, 50, 50, 255);
          SDL_RenderClear(renderer);
          
          // Utiliser le rectangle temporaire pour le message
          tempRect.x := 200; tempRect.y := 250; tempRect.w := 400; tempRect.h := 100;
          DessinerTexte('ROULETTE - EN DEVELOPPEMENT', tempRect);
          
          SDL_RenderPresent(renderer);
          SDL_Delay(2000);
          etatJeu := MENU;
        end;

      JEU_SLOTS:
        begin
          // écran placeholder
          SDL_SetRenderDrawColor(renderer, 80, 0, 80, 255);
          SDL_RenderClear(renderer);
          
          // Utiliser le rectangle temporaire pour le message
          tempRect.x := 150; tempRect.y := 250; tempRect.w := 500; tempRect.h := 100;
          DessinerTexte('MACHINE A SOUS - EN DEVELOPPEMENT', tempRect);
          
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
