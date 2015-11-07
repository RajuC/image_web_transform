-module(image_web_transform_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    PrivDir = code:priv_dir(image_web_transform),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, image_web_transform, "index.html"}},   
            {"/upload", upload_handler, []},
            {"/resizeimg", resize_img_handler, []},          
            {"/cropimg", crop_img_handler, []},
            {"/imgdetails", img_transform_handler, []},
            {"/[...]", cowboy_static, {priv_dir, image_web_transform, ""
                }},
            {"/[...]", cowboy_static, {PrivDir++"/images/", image_web_transform, ""
                }}  
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100,
        [{port, 8088}],
        [{env, [{dispatch, Dispatch}]}
    ]),
    image_web_transform_sup:start_link().

stop(_State) ->
	ok.