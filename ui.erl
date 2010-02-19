-module(ui).
-export([go/2, demo/0]).
-author("Nicolas Favre-Felix - n.favrefelix#gmail.com").
-license("Public Domain").

-include_lib("/usr/lib/esdl/include/sdl.hrl").
-include_lib("/usr/lib/esdl/include/sdl_events.hrl").
-include_lib("/usr/lib/esdl/include/sdl_video.hrl").

demo() ->
	go(30,30).

go(W,H) ->
	_Server = sdl:init(?SDL_INIT_VIDEO),
	Flags = ?SDL_ANYFORMAT band ?SDL_RESIZABLE,
	ScreenRef = sdl_video:setVideoMode(6*W, 6*H, 8, Flags),
	Screen = sdl_video:getSurface(ScreenRef),

	Black = sdl_video:mapRGB(Screen, 0, 0, 0),
	White = sdl_video:mapRGB(Screen, 255, 255, 255),

	Walls = maze_gen:maze(W, H),
	loop({W,H,Walls}, Screen, {Black, White}, []).
	   
loop(_, _, _, {keyboard,0,1,9,27,4096,0}) -> sdl:quit(); % escape key
loop({W, H, Walls}, Screen, {Black, White}, _) -> 
	true = sdl_video:fillRect(Screen, null, Black),
	true = sdl_video:fillRect(Screen, #sdl_rect{x=1,y=1,w=W*6-2,h=H*6-2}, White),

	lists:map(fun(Wall) -> draw_wall(Wall, Screen, Black) end, Walls),

	DB = (Screen#sdl_surface.flags band ?SDL_DOUBLEBUF),
	if DB == ?SDL_DOUBLEBUF -> sdl_video:flip(Screen);
	true -> sdl_video:updateRect(Screen, 0,0,W*6, H*6) %Updates)
	end,

	E = sdl_events: waitEvent(),
	loop({W, H, Walls}, Screen, {Black, White}, E).

draw_wall({X, Y, h}, Screen, Black) ->
	R = #sdl_rect{x = X * 6, y = Y * 6, w = 7, h = 1},
	sdl_video:fillRect(Screen, R, Black);

draw_wall({X, Y, v}, Screen, Black) ->
	R = #sdl_rect{x = X * 6, y = Y * 6, w = 1, h = 7},
	sdl_video:fillRect(Screen, R, Black).
	

