-module(resize_img_handler).

-include("image_transform.hrl"). 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    io:format("~nresize_img_handler:: PostVals : ~p~n",[PostVals]),  
    [{<<"resize">>,Trans}] = PostVals,
    [ResizeTrans,Image,Extn] = string:tokens(binary_to_list(Trans),"_"),  
    ImageName = Image++"."++Extn,
    ImageRec = image_transform:identify_file(ImageName),
    Url = "http://localhost:8088/images/"++Image++"_orig."++Extn,
    HtmlBody = trans_html:provide_html(ImageRec,ResizeTrans,Url),
    {ok, Req3} = cowboy_req:reply(200, [], HtmlBody, Req2),
    {ok, Req3, State}.


 
terminate(_Reason, _Req, _State) ->
    ok.
