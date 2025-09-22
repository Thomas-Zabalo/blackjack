program MenuAvecImagesEtTexte;

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  SDL2,
  SDL2_image,
  SDL2_ttf,
  SysUtils;

const
  WINDOW_WIDTH = 1800;
  WINDOW_HEIGHT = 1200;

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

  
  btnBlackjackRect, btnRouletteRect, btnSlotsRect,btnQuitRect: TSDL_Rect;
  textBlackjackRect, textRouletteRect, textSlotsRect, textQuitRect: TSDL_Rect;

  texteBlackjack: string = 'Blackjack';
  texteRoulette: string = 'Roulette';
  texteSlots: string = 'Machine à sous';

 

begin
  // --- Initialisation SDL2, SDL_image, SDL_ttf ---
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
  begin
    Writeln('Erreur SDL_Init : ', SDL_GetError);
    Halt(1);
  end;

  if (IMG_Init(IMG_INIT_PNG) and IMG_INIT_PNG) = 0 then
  begin
    Writeln('Erreur IMG_Init : ', IMG_GetError);
    Halt(1);
  end;

  if TTF_Init() <> 0 then
  begin
    Writeln('Erreur TTF_Init : ', TTF_GetError);
    Halt(1);
  end;

  window := SDL_CreateWindow('Menu Principal', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                             WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN);
  if window = nil then
  begin
    Writeln('Erreur SDL_CreateWindow : ', SDL_GetError);
    Halt(1);
  end;

  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if renderer = nil then
  begin
    Writeln('Erreur SDL_CreateRenderer : ', SDL_GetError);
    SDL_DestroyWindow(window);
    Halt(1);
  end;

  // --- Charger la police ---
  font := TTF_OpenFont('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 24);
  if font = nil then
  begin
    Writeln('Erreur TTF_OpenFont : ', TTF_GetError);
    // nettoyage
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
    Halt(1);
  end;

  // --- Définir la couleur du texte (blanc) ---
  textColor.r := 255;
  textColor.g := 255;
  textColor.b := 255;
  textColor.a := 255;

  // --- Charger les images des boutons ---
  imgPlaySurface := IMG_Load('btn_play.png');
  if imgPlaySurface = nil then
  begin
    Writeln('Erreur IMG_Load btn_play.png : ', IMG_GetError);
    // nettoyage
    TTF_CloseFont(font);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
    Halt(1);
  end;
  imgPlayTexture := SDL_CreateTextureFromSurface(renderer, imgPlaySurface);
  SDL_FreeSurface(imgPlaySurface);

  imgQuitSurface := IMG_Load('btn_quit.png');
  if imgQuitSurface = nil then
  begin
    Writeln('Erreur IMG_Load btn_quit.png : ', IMG_GetError);
    // nettoyage
    SDL_DestroyTexture(imgPlayTexture);
    TTF_CloseFont(font);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
    Halt(1);
  end;
  imgQuitTexture := SDL_CreateTextureFromSurface(renderer, imgQuitSurface);
  SDL_FreeSurface(imgQuitSurface);

  // --- Définir les rectangles des boutons (position + taille) ---
  btnBlackjackRect.x := 300;
  btnBlackjackRect.y := 150;
  btnBlackjackRect.w := 200;
  btnBlackjackRect.h := 60;
  SDL_QueryTexture(imgBlackjackTexture, nil, nil, @btnBlackjackRect.w, @btnBlackjackRect.h);

  btnRouletteRect.x := 300;
  btnRouletteRect.y := 250;
  btnRouletteRect.w := 200;
  btnRouletteRect.h := 60;
  SDL_QueryTexture(imgRouletteTexture, nil, nil, @btnRouletteRect.w, @btnRouletteRect.h);

  btnSlotsRect.x := 300;
  btnSlotsRect.y := 350;
  btnSlotsRect.w := 200;
  btnSlotsRect.h := 60;
  SDL_QueryTexture(imgSlotsTexture, nil, nil, @btnSlotsRect.w, @btnSlotsRect.h);

  btnQuitRect.x := 300;
  btnQuitRect.y := 450;
  btnQuitRect.w := 200;
  btnQuitRect.h := 60;
  SDL_QueryTexture(imgQuitTexture, nil, nil, @btnQuitRect.w, @btnQuitRect.h);

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

            // Si clic dans bouton Blackjack
		if (mouseX >= btnBlackjackRect.x) and (mouseX <= btnBlackjackRect.x + btnBlackjackRect.w) and
   		(mouseY >= btnBlackjackRect.y) and (mouseY <= btnBlackjackRect.y + btnBlackjackRect.h) then
		begin
		  Writeln('Tu as choisi Blackjack');
 		 // lancer le jeu Blackjack ici
		end;

		// Si clic dans bouton Roulette
		if (mouseX >= btnRouletteRect.x) and (mouseX <= btnRouletteRect.x + btnRouletteRect.w) and
		   (mouseY >= btnRouletteRect.y) and (mouseY <= btnRouletteRect.y + btnRouletteRect.h) then
		begin
 		 Writeln('Tu as choisi Roulette');
 		 // lancer le jeu Roulette ici
		end;

		// Si clic dans bouton Machine à sous
		if (mouseX >= btnSlotsRect.x) and (mouseX <= btnSlotsRect.x + btnSlotsRect.w) and
		   (mouseY >= btnSlotsRect.y) and (mouseY <= btnSlotsRect.y + btnSlotsRect.h) then
		begin
 		 Writeln('Tu as choisi Machine à sous');
 		 // lancer le jeu Machine à sous ici
		end;

		// Si clic dans bouton Quitter
		if (mouseX >= btnQuitRect.x) and (mouseX <= btnQuitRect.x + btnQuitRect.w) and
		   (mouseY >= btnQuitRect.y) and (mouseY <= btnQuitRect.y + btnQuitRect.h) then
		begin
 		 Writeln('Quitter le jeu');
 		 running := False;
		end;
            end;
          end;
      end;
    end;

    // --- Dessiner tout ---

    // Effacer l’écran en noir
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

     // --- Bouton Blackjack (rouge) ---
    SDL_SetRenderDrawColor(renderer, 200, 0, 0, 255);
    SDL_RenderFillRect(renderer, @btnBlackjackRect);
   
   // --- Bouton Roulette (vert) ---
    SDL_SetRenderDrawColor(renderer, 0, 200, 0, 255);
    SDL_RenderFillRect(renderer, @btnRouletteRect);
   
   // --- Bouton Machine à sous (bleu) ---
    SDL_SetRenderDrawColor(renderer, 0, 0, 200, 255);
    SDL_RenderFillRect(renderer, @btnSlotsRect);

    // --- Bouton Quitter (gris) ---
    SDL_SetRenderDrawColor(renderer, 100, 100, 100, 255);
    SDL_RenderFillRect(renderer, @btnQuitRect);
    
     // Texte blanc
    textColor.r := 255; textColor.g := 255; textColor.b := 255; textColor.a := 255;


    // --- Texte sur le bouton Jouer ---
    // Créer une surface texte
   // Texte Blackjack
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

// Texte Roulette
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



// Texte Machine à sous
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

    // --- Texte sur le bouton Quitter ---
    textSurface := TTF_RenderText_Blended(font, PChar(AnsiString(textePlay)), textColor);
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

    // Présenter le résultat
    SDL_RenderPresent(renderer);

    // Limiter la boucle (par exemple 60 FPS)
    SDL_Delay(16);
  end;

  // --- Nettoyage ---
  SDL_DestroyTexture(imgPlayTexture);
  SDL_DestroyTexture(imgQuitTexture);
  TTF_CloseFont(font);

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  TTF_Quit();
  IMG_Quit();
  SDL_Quit();
end.
