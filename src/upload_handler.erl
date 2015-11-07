%% upload handler 

-module(upload_handler).
-include("image_transform.hrl").
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-define(MAX_FILE_SIZE_LIMIT, 8307200). % 8M


init(_Type, Req, []) ->
	{ok, Req, undefined}.


%%resize=noScale&width=200&height=100&rotation=90_clock&imageFormat=jpg
handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            {B, ReqU} = 
            case multipart(Req2) of
                {ok,Body, ReqM} ->
                    {Body, ReqM};
                {rejected, file_size_limit, ReqM} ->
                    {<<"POST: File Size Limit">>, ReqM};
                {error,Reason} ->
                     {list_to_binary(Reason), Req}
                end,
            {ok, Req3} = cowboy_req:reply(200, [], B, ReqU),
            {ok, Req3, State};
        <<"GET">> ->
            Body = <<"<h1>This is a response for GET</h1>">>,
            {ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
            {ok, Req3, State};
        _ ->
            Body = <<"<h1>This is a response for other methods</h1>">>,
            {ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
            {ok, Req3, State}
        end.



multipart(Req) ->
    multipart(Req, ?MAX_FILE_SIZE_LIMIT).
multipart(Req, MaxFileSizeLimit) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    {ok, Url, _Req3} = cowboy_req:part_body(Req2),
                    io:format("~nupload_handler ::Url ::::::: ~p~n",[Url]),
                     ImageRec = image_transform:get_img_details_from_url(Url),
                    io:format("Original Image Record : ~p~n",[ImageRec]),
                    {ok,trans_html:provide_html(ImageRec,"resize",binary_to_list(Url)),Req2};

                {file, FieldName, Filename, CType, CTransferEncoding} ->
                 io:format("~nupload_handler::FieldName: ~p|| Filename:~p ||CType ~p ||CTransferEncoding: ~p ~n",
                    [FieldName, Filename, CType, CTransferEncoding]),
                    Extn = filename:extension(binary_to_list(Filename)),
                    if (Extn == ".jpg") or (Extn == ".jpeg") or (Extn == ".png") ->
                        Reply = stream_file(Req2,Extn,binary_to_list(CType), MaxFileSizeLimit),
                        case Reply of
                            {error,Reason} ->
                                {error,Reason};
                            ImageRec ->
                                io:format("~nupload_handler::Original Image Record : ~p~n",[ImageRec]),
                                ImgShortName = ImageRec#image.short_name,
                                Extn1 = ImageRec#image.extn,

                                Url = "http://localhost:8088/images/"++ImgShortName++"_orig"++Extn1,
                                {ok,trans_html:provide_html(ImageRec,"resize",Url),Req2}
                        end;
                    true ->
                        {error,"Invalid Image File"}
                    end
            end;
        {done, Req2} ->
            {ok, Req2}
    end.

stream_file(Req,Extn,Type,MaxFileSizeLimit) ->
    {_Control, Data, _Req2} = cowboy_req:part_body(Req),
    NewFileSize = byte_size(Data),
    case NewFileSize > MaxFileSizeLimit of
        true -> {error,"larger_file_size"};
        false ->
            image_transform:get_img_details_from_upload(Data,Extn,Type)   
    end.

terminate(_Reason, _Req, _State) ->
    ok.