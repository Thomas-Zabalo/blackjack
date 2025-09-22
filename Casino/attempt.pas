program image ;

uses sdl2 , sdl2_image ;
const
SURFACEWIDTH =640; { largeur en pixels de la surface de jeu }
SURFACEHEIGHT =480; { hauteur en pixels de la surface de jeu }
IMAGEWIDTH =600; { largeur en pixels de l ’ image }
IMAGEHEIGHT =400; { hauteur en pixels de l ’ image }

 procedure initialise ( var sdlwindow : PSDL_Window ; var sdlRenderer : PSDL_Renderer ;
var table : PSDL_TEXTURE );
begin
	{ charger la b i b l i o t h e q u e }
	SDL_Init ( SDL_INIT_VIDEO );
	{ i n i t i a l i s e r la fenetre et le moteur de rendu : largeur , hauteur ,
	* type de fenetre }
	SDL_CreateWindowAndRenderer( SURFACEWIDTH , SURFACEHEIGHT , SDL_WINDOW_SHOWN ,
	@sdlwindow , @sdlRenderer );
	{ chargement de l’image comme texture }
	table := IMG_LoadTexture ( sdlRenderer , 'table.gif' );
end ;

	{ Procedure pour quitter p rop rem e n t la SDL }
procedure termine ( var sdlwindow : PSDL_WINDOW ; var sdlRenderer : PSDL_Renderer ;
var table : PSDL_TEXTURE );
begin
	{ vider la memoire c o r r e s p o n d a n t a l ’ image et a la fenetre }
	SDL_DestroyTexture ( table );
	SDL_DestroyRenderer ( sdlRenderer );
	SDL_DestroyWindow ( sdlwindow );
	{ decharger la b i b l i o t h e q u e }
	SDL_Quit ();
end ;

{ Procedure d ’ affichage }
procedure affiche ( sdlRenderer : PSDL_RENDERER ; table : PSDL_TEXTURE );
var destination_rect : TSDL_RECT ;
begin
	SDL_RenderClear(sdlrenderer);
	{ Choix de la position et taille de l ’ element a afficher }
	destination_rect . x :=( SURFACEWIDTH - IMAGEWIDTH ) div 2;
	destination_rect . y :=( SURFACEHEIGHT - IMAGEHEIGHT ) div 2;
	destination_rect . w := IMAGEWIDTH ;
	destination_rect . h := IMAGEHEIGHT ;
	{ Coller l ’ element sword avec les c a r a c t e r i s t i q u e s d e s t i n a t i o n _ r e c t }
	SDL_RenderCopy ( sdlRenderer , table , nil , @destination_rect );
	{ Afficher la nouvelle image }
	SDL_RenderPresent ( sdlRenderer );
end ;
var fenetre : PSDL_WINDOW ;
rendu : PSDL_Renderer ;
table : PSDL_TEXTURE ;
begin
	initialise ( fenetre , rendu , table );
	affiche ( rendu , table );
	{ Attendre 400 secondes }
	SDL_delay (400000);
	termine ( fenetre , rendu , table );
end .
