program MenuRectangles;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SDL2,
  SDL2_ttf,
  SDL2_image,
  SysUtils,
  Blackjack;
  
const
  WINDOW_WIDTH  = 1920;
  WINDOW_HEIGHT = 1080;
  BUTTON_SIZE = 300;
  BUTTON_SPACING = 20;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  event: TSDL_Event;
  running: Boolean;
  
  backgroundTexture: PSDL_Texture;
  backgroundSurface: PSDL_Surface;

  btnBlackjackTex, btnBlackjackHoverTex,
  btnRouletteTex, btnRouletteHoverTex,
  btnMachineTex, btnMachineHoverTex: PSDL_Texture;
  surf: PSDL_Surface;

  font: PTTF_Font;
  textSurface: PSDL_Surface;
  textTexture: PSDL_Texture;
  textColor: TSDL_Color;
  
  mouseX, mouseY: Integer;

  // rectangles boutons
  btnBlackjackRect, btnRouletteRect, btnSlotsRect, btnQuitRect: TSDL_Rect;
  textRouletteRect, textSlotsRect, textQuitRect: TSDL_Rect;

  // textes
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
  font := TTF_OpenFont('C:\Windows\Fonts\arial.ttf', 24);
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
  
  // --- Ouvre le background image --- 
  backgroundSurface := IMG_Load('background_casino.png');
  if backgroundSurface = nil then
  begin
    Writeln('Erreur lors du chargement de image :', IMG_GetError);
    // Nettoyage
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  
  // --- Chargement du background image ---
  backgroundTexture := SDL_CreateTextureFromSurface(renderer, backgroundSurface);
  SDL_FreeSurface(backgroundSurface);
  
 // --- Chargement bouton Blackjack ---
  surf := IMG_Load('blackjack_button.png');
  if surf = nil then
  begin
    Writeln('Erreur chargement blackjack_button.png : ', IMG_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  btnBlackjackTex := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_FreeSurface(surf);

  surf := IMG_Load('blackjack_button_hover.png');
  if surf = nil then
  begin
    Writeln('Erreur chargement blackjack_button_hover.png : ', IMG_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  btnBlackjackHoverTex := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_FreeSurface(surf);
  
   // --- Chargement bouton Roulette ---
  surf := IMG_Load('roulette_button.png');
  if surf = nil then
  begin
    Writeln('Erreur chargement roulette_button.png : ', IMG_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  btnRouletteTex := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_FreeSurface(surf);

  surf := IMG_Load('roulette_button_hover.png');
  if surf = nil then
  begin
    Writeln('Erreur chargement roulette_button_hover.png : ', IMG_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  btnRouletteHoverTex := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_FreeSurface(surf);
  
   // --- Chargement bouton Machine à sous ---
  surf := IMG_Load('machine_button.png');
  if surf = nil then
  begin
    Writeln('Erreur chargement machine_button.png : ', IMG_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  btnMachineTex := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_FreeSurface(surf);

  surf := IMG_Load('machine_button_hover.png');
  if surf = nil then
  begin
    Writeln('Erreur chargement machine_button_hover.png : ', IMG_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    Halt(1);
  end;
  btnMachineHoverTex := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_FreeSurface(surf);

  // --- Calcul positions centrées ---
  centerX := (WINDOW_WIDTH - (3 * BUTTON_SIZE + 2 * BUTTON_SPACING)) div 2;
  startY := (WINDOW_HEIGHT - (2 * BUTTON_SIZE + BUTTON_SPACING)) div 2;

  btnBlackjackRect.x := centerX;
  btnBlackjackRect.y := startY;
  btnBlackjackRect.w := BUTTON_SIZE;
  btnBlackjackRect.h := BUTTON_SIZE;

  btnRouletteRect.x := centerX + BUTTON_SIZE + BUTTON_SPACING;
  btnRouletteRect.y := startY;
  btnRouletteRect.w := BUTTON_SIZE;
  btnRouletteRect.h := BUTTON_SIZE;

  btnSlotsRect.x := centerX + 2 * (BUTTON_SIZE + BUTTON_SPACING);
  btnSlotsRect.y := startY;
  btnSlotsRect.w := BUTTON_SIZE;
  btnSlotsRect.h := BUTTON_SIZE;

  btnQuitRect.x := centerX + BUTTON_SIZE + BUTTON_SPACING;
  btnQuitRect.y := startY + BUTTON_SIZE + BUTTON_SPACING;
  btnQuitRect.w := BUTTON_SIZE;
  btnQuitRect.h := BUTTON_SIZE;

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
                LancerBlackjack(renderer,font);
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

	// --- Background ---
	SDL_RenderCopy(renderer, backgroundTexture, nil, nil);
	
    // Bouton Blackjack (rouge), met en évidence si hover
    if (mouseX >= btnBlackjackRect.x) and (mouseX <= btnBlackjackRect.x + btnBlackjackRect.w) and
       (mouseY >= btnBlackjackRect.y) and (mouseY <= btnBlackjackRect.y + btnBlackjackRect.h) then
      SDL_RenderCopy(renderer, btnBlackjackHoverTex, nil, @btnBlackjackRect)
    else
      SDL_RenderCopy(renderer, btnBlackjackTex, nil, @btnBlackjackRect);

    // Bouton Roulette (vert)
    if (mouseX >= btnRouletteRect.x) and (mouseX <= btnRouletteRect.x + btnRouletteRect.w) and
       (mouseY >= btnRouletteRect.y) and (mouseY <= btnRouletteRect.y + btnRouletteRect.h) then
      SDL_RenderCopy(renderer, btnRouletteHoverTex, nil, @btnRouletteRect)
    else
      SDL_RenderCopy(renderer, btnRouletteTex, nil, @btnRouletteRect);

    // Bouton Machine à sous (bleu)
    if (mouseX >= btnSlotsRect.x) and (mouseX <= btnSlotsRect.x + btnSlotsRect.w) and
       (mouseY >= btnSlotsRect.y) and (mouseY <= btnSlotsRect.y + btnSlotsRect.h) then
      SDL_RenderCopy(renderer, btnMachineHoverTex, nil, @btnSlotsRect)
    else
      SDL_RenderCopy(renderer, btnMachineTex, nil, @btnSlotsRect);

    // Bouton Quitter (gris)
    if (mouseX >= btnQuitRect.x) and (mouseX <= btnQuitRect.x + btnQuitRect.w) and
       (mouseY >= btnQuitRect.y) and (mouseY <= btnQuitRect.y + btnQuitRect.h) then
      SDL_SetRenderDrawColor(renderer, 180, 180, 180, 255)
    else
      SDL_SetRenderDrawColor(renderer, 120, 120, 120, 255);
    SDL_RenderFillRect(renderer, @btnQuitRect);

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
  SDL_DestroyTexture(backgroundTexture);
  SDL_DestroyTexture(btnBlackjackTex);
	
  TTF_Quit();
  SDL_Quit();
end.+

