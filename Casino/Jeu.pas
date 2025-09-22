program MenuRectangles;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SDL2,
  SDL2_ttf,
  SysUtils;
  
  


const
  WINDOW_WIDTH  = 1800;
  WINDOW_HEIGHT = 1600;
  BUTTON_W      = 300;
  BUTTON_H      = 70;
  BUTTON_SPACING = 20;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  event: TSDL_Event;
  running: Boolean;

  font: PTTF_Font;
  textSurface: PSDL_Surface;
  textTexture: PSDL_Texture;
  textColor: TSDL_Color;
  
 
  mouseX, mouseY: Integer;

  // rectangles boutons
  btnBlackjackRect, btnRouletteRect, btnSlotsRect, btnQuitRect: TSDL_Rect;
  textBlackjackRect, textRouletteRect, textSlotsRect, textQuitRect: TSDL_Rect;

  // textes
  texteBlackjack: string = 'Blackjack';
  texteRoulette:  string = 'Roulette';
  texteSlots:     string = 'Machine à sous';
  texteQuit:      string = 'Quitter';

  centerX, startY: Integer;

begin
  // --- Initialisation SDL2 et TTF ---
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
  begin
    Writeln('Erreur SDL_Init : ', SDL_GetError);
    Halt(1);
  end;

  if TTF_Init() <> 0 then
  begin
    Writeln('Erreur TTF_Init : ', TTF_GetError);
    SDL_Quit();
    Halt(1);
  end;

  window := SDL_CreateWindow('Menu Principal', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                             WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN);
  if window = nil then
  begin
    Writeln('Erreur SDL_CreateWindow : ', SDL_GetError);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;

  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if renderer = nil then
  begin
    Writeln('Erreur SDL_CreateRenderer : ', SDL_GetError);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;

  // --- Ouvrir une police (modifie le chemin si besoin) ---
  // Note : sur Ubuntu la police DejaVu est souvent disponible à ce chemin
  font := TTF_OpenFont('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 24);
  if font = nil then
  begin
    Writeln('Erreur TTF_OpenFont : ', TTF_GetError);
    // Nettoyage
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;

  // --- Calcul positions centrées ---
  centerX := (WINDOW_WIDTH - BUTTON_W) div 2;
  startY := 120;

  btnBlackjackRect.x := centerX;
  btnBlackjackRect.y := startY;
  btnBlackjackRect.w := BUTTON_W;
  btnBlackjackRect.h := BUTTON_H;

  btnRouletteRect.x := centerX;
  btnRouletteRect.y := startY + (BUTTON_H + BUTTON_SPACING) * 1;
  btnRouletteRect.w := BUTTON_W;
  btnRouletteRect.h := BUTTON_H;

  btnSlotsRect.x := centerX;
  btnSlotsRect.y := startY + (BUTTON_H + BUTTON_SPACING) * 2;
  btnSlotsRect.w := BUTTON_W;
  btnSlotsRect.h := BUTTON_H;

  btnQuitRect.x := centerX;
  btnQuitRect.y := startY + (BUTTON_H + BUTTON_SPACING) * 3;
  btnQuitRect.w := BUTTON_W;
  btnQuitRect.h := BUTTON_H;

  // couleur texte blanc (utilisée pour tous)
  textColor.r := 255;
  textColor.g := 255;
  textColor.b := 255;
  textColor.a := 255;

  running := True;
  while running do
  begin
    // --- Gestion des événements ---
    while SDL_PollEvent(@event) <> 0 do
    begin
      case event.type_ of
        SDL_QUITEV:
          running := False;

        SDL_MOUSEBUTTONDOWN:
          begin
            if event.button.button = SDL_BUTTON_LEFT then
            begin
              mouseX := event.button.x;
              mouseY := event.button.y;

              // Blackjack
              if (mouseX >= btnBlackjackRect.x) and (mouseX <= btnBlackjackRect.x + btnBlackjackRect.w) and
                 (mouseY >= btnBlackjackRect.y) and (mouseY <= btnBlackjackRect.y + btnBlackjackRect.h) then
              begin
                Writeln('Tu as choisi : Blackjack');
                // >>> lancer Blackjack ici
              end;

              // Roulette
              if (mouseX >= btnRouletteRect.x) and (mouseX <= btnRouletteRect.x + btnRouletteRect.w) and
                 (mouseY >= btnRouletteRect.y) and (mouseY <= btnRouletteRect.y + btnRouletteRect.h) then
              begin
                Writeln('Tu as choisi : Roulette');
                // >>> lancer Roulette ici
              end;

              // Machine à sous
              if (mouseX >= btnSlotsRect.x) and (mouseX <= btnSlotsRect.x + btnSlotsRect.w) and
                 (mouseY >= btnSlotsRect.y) and (mouseY <= btnSlotsRect.y + btnSlotsRect.h) then
              begin
                Writeln('Tu as choisi : Machine à sous');
                // >>> lancer Slots ici
              end;

              // Quitter
              if (mouseX >= btnQuitRect.x) and (mouseX <= btnQuitRect.x + btnQuitRect.w) and
                 (mouseY >= btnQuitRect.y) and (mouseY <= btnQuitRect.y + btnQuitRect.h) then
              begin
                Writeln('Quitter le programme');
                running := False;
              end;
            end;
          end;
      end;
    end;

    // --- Récupérer position souris pour hover (optionnel) ---
    SDL_GetMouseState(@mouseX, @mouseY);

    // --- Dessin ---
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // fond noir
    SDL_RenderClear(renderer);

    // Bouton Blackjack (rouge), met en évidence si hover
    if (mouseX >= btnBlackjackRect.x) and (mouseX <= btnBlackjackRect.x + btnBlackjackRect.w) and
       (mouseY >= btnBlackjackRect.y) and (mouseY <= btnBlackjackRect.y + btnBlackjackRect.h) then
      SDL_SetRenderDrawColor(renderer, 230, 60, 60, 255) // plus clair quand hover
    else
      SDL_SetRenderDrawColor(renderer, 200, 0, 0, 255);
    SDL_RenderFillRect(renderer, @btnBlackjackRect);

    // Bouton Roulette (vert)
    if (mouseX >= btnRouletteRect.x) and (mouseX <= btnRouletteRect.x + btnRouletteRect.w) and
       (mouseY >= btnRouletteRect.y) and (mouseY <= btnRouletteRect.y + btnRouletteRect.h) then
      SDL_SetRenderDrawColor(renderer, 60, 230, 60, 255)
    else
      SDL_SetRenderDrawColor(renderer, 0, 180, 0, 255);
    SDL_RenderFillRect(renderer, @btnRouletteRect);

    // Bouton Machine à sous (bleu)
    if (mouseX >= btnSlotsRect.x) and (mouseX <= btnSlotsRect.x + btnSlotsRect.w) and
       (mouseY >= btnSlotsRect.y) and (mouseY <= btnSlotsRect.y + btnSlotsRect.h) then
      SDL_SetRenderDrawColor(renderer, 80, 80, 255, 255)
    else
      SDL_SetRenderDrawColor(renderer, 0, 0, 200, 255);
    SDL_RenderFillRect(renderer, @btnSlotsRect);

    // Bouton Quitter (gris)
    if (mouseX >= btnQuitRect.x) and (mouseX <= btnQuitRect.x + btnQuitRect.w) and
       (mouseY >= btnQuitRect.y) and (mouseY <= btnQuitRect.y + btnQuitRect.h) then
      SDL_SetRenderDrawColor(renderer, 180, 180, 180, 255)
    else
      SDL_SetRenderDrawColor(renderer, 120, 120, 120, 255);
    SDL_RenderFillRect(renderer, @btnQuitRect);

    // --- Afficher textes (centrés sur chaque bouton) ---
    // Blackjack
    textSurface := TTF_RenderText_Blended(font, PChar(AnsiString(texteBlackjack)), textColor);
    if textSurface <> nil then
    begin
      textTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
      textBlackjackRect.w := textSurface^.w;
      textBlackjackRect.h := textSurface^.h;
      textBlackjackRect.x := btnBlackjackRect.x + (btnBlackjackRect.w - textBlackjackRect.w) div 2;
      textBlackjackRect.y := btnBlackjackRect.y + (btnBlackjackRect.h - textBlackjackRect.h) div 2;
      SDL_RenderCopy(renderer, textTexture, nil, @textBlackjackRect);
      SDL_FreeSurface(textSurface);
      SDL_DestroyTexture(textTexture);
    end;

    // Roulette
    textSurface := TTF_RenderText_Blended(font, PChar(AnsiString(texteRoulette)), textColor);
    if textSurface <> nil then
    begin
      textTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
      textRouletteRect.w := textSurface^.w;
      textRouletteRect.h := textSurface^.h;
      textRouletteRect.x := btnRouletteRect.x + (btnRouletteRect.w - textRouletteRect.w) div 2;
      textRouletteRect.y := btnRouletteRect.y + (btnRouletteRect.h - textRouletteRect.h) div 2;
      SDL_RenderCopy(renderer, textTexture, nil, @textRouletteRect);
      SDL_FreeSurface(textSurface);
      SDL_DestroyTexture(textTexture);
    end;

    // Machine à sous
    textSurface := TTF_RenderText_Blended(font, PChar(AnsiString(texteSlots)), textColor);
    if textSurface <> nil then
    begin
      textTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
      textSlotsRect.w := textSurface^.w;
      textSlotsRect.h := textSurface^.h;
      textSlotsRect.x := btnSlotsRect.x + (btnSlotsRect.w - textSlotsRect.w) div 2;
      textSlotsRect.y := btnSlotsRect.y + (btnSlotsRect.h - textSlotsRect.h) div 2;
      SDL_RenderCopy(renderer, textTexture, nil, @textSlotsRect);
      SDL_FreeSurface(textSurface);
      SDL_DestroyTexture(textTexture);
    end;

    // Quitter
    textSurface := TTF_RenderText_Blended(font, PChar(AnsiString(texteQuit)), textColor);
    if textSurface <> nil then
    begin
      textTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
      textQuitRect.w := textSurface^.w;
      textQuitRect.h := textSurface^.h;
      textQuitRect.x := btnQuitRect.x + (btnQuitRect.w - textQuitRect.w) div 2;
      textQuitRect.y := btnQuitRect.y + (btnQuitRect.h - textQuitRect.h) div 2;
      SDL_RenderCopy(renderer, textTexture, nil, @textQuitRect);
      SDL_FreeSurface(textSurface);
      SDL_DestroyTexture(textTexture);
    end;

    // Afficher l'écran
    SDL_RenderPresent(renderer);

    // Limiter la boucle (≈60 FPS)
    SDL_Delay(16);
  end;

  // --- Nettoyage ---
  TTF_CloseFont(font);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  TTF_Quit();
  SDL_Quit();
end.+

