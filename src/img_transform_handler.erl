%%% main handler which handles all the transforms

-module(img_transform_handler).

-include("image_transform.hrl"). 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.
-define(IMG_DIR,code:priv_dir(image_web_transform)++"/images/").
handle(Req, State) ->
	io:format("Req:: ~p~n",[Req]),
	{ok, PostVals, _Req2} = cowboy_req:body_qs(Req),
	TransformValues= get_post_values_list(PostVals,[]),
	io:format("img_transform_handler::TransformValues:: ~p~n",[TransformValues]),
	Response = get_transform_data(TransformValues),
	case Response of
		{error,Reason} ->
			io:format("~nError :: ~p~n",[Reason]),
			cowboy_req:reply(400,[],list_to_binary(Reason), Req);
		{ImgRec,HtmlBody} ->
			io:format("~nimg_transform_handler::Transformed_Image_Details : ~p~n",[ImgRec]),
			cowboy_req:reply(200,[], HtmlBody, Req);
		{error,ResponseCode,Reason} ->
			io:format("~nError: ~p : ~p~n",[ResponseCode,Reason]),
			cowboy_req:reply(ResponseCode,[],list_to_binary(Reason), Req)
	end,
	{ok,Req,State}.



get_transform_data(TransformValues) ->
	{_,ImageName} = lists:keyfind("image",1,TransformValues),
	Scale = lists:keyfind("scale",1,TransformValues),
	Rotation = lists:keyfind("rotation",1,TransformValues),
	ConvertFormat = lists:keyfind("finalFormat",1,TransformValues),
	{_,FinalTransform} = lists:keyfind("transform",1,TransformValues),
	FinalTransformParams =lists:keyfind(FinalTransform,1,TransformValues),
	ImageRec=image_transform:identify_file(ImageName),
	FinalImageRec =
	case check_format(ImageRec,ConvertFormat) of
		{ok,convert} ->
			get_transform_data(ImageRec,[Scale,Rotation,FinalTransformParams,ConvertFormat]);
		not_needed ->
			get_transform_data(ImageRec,[Scale,Rotation,FinalTransformParams])
	end,
	FinalImgName = FinalImageRec#image_tr.name,
	Width = FinalImageRec#image_tr.width,
	Height = FinalImageRec#image_tr.height,
	Url = "http://localhost:8088/images/"++FinalImgName,
	TransformedImgData = trans_html:get_output_snippet(Url,Width,Height),
	{FinalImageRec,TransformedImgData}.



get_transform_data(ImageRec,[]) ->
	ImageRec;
get_transform_data(ImageRec,[{"scale","0_Scale"}|Rest]) ->
	get_transform_data(ImageRec,Rest);

get_transform_data(ImageRec,[{"finalFormat",""}|Rest]) ->
	get_transform_data(ImageRec,Rest);

get_transform_data(ImageRec,[{"finalFormat",Format}|Rest]) ->
	TransImageRec=image_transform:transform_image_extended(ImageRec,{Format,[]}),
	get_transform_data(TransImageRec,Rest);

get_transform_data(ImageRec,[{"scale",ScalePer}|Rest]) ->
	case ScalePer of 
		"25_scale" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"scale",["25"]}),
			get_transform_data(TransImageRec,Rest);
		"50_scale" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"scale",["50"]}),
			get_transform_data(TransImageRec,Rest);
		"75_scale" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"scale",["75"]}),
			get_transform_data(TransImageRec,Rest)
	end;
get_transform_data(ImageRec,[{"rotation",RotationDeg}|Rest]) -> 
	case RotationDeg of 
		"0_clock" ->
			get_transform_data(ImageRec,Rest);
		"90_clock" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"rotation",["90"]}),
			get_transform_data(TransImageRec,Rest);
		"180_clock" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"rotation",["180"]}),
			get_transform_data(TransImageRec,Rest);
		"90_antiClock" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"rotation",["-90"]}),
			get_transform_data(TransImageRec,Rest);
		"180_antiClock" ->
			TransImageRec=image_transform:transform_image_extended(ImageRec,{"rotation",["-180"]}),
			get_transform_data(TransImageRec,Rest)	
	end;

%%{"resize",[{"width","400"},{"height","300"}]}
get_transform_data(ImageRec,[{"resize",[{"width",""},{"height",""}]}|Rest]) ->
	get_transform_data(ImageRec,Rest);
get_transform_data(ImageRec,[{"resize",[{"width",Width},{"height",Height}]}|Rest]) ->
	TransImageRec=image_transform:transform_image_extended(ImageRec,{"resize",[Width,Height]}),
	get_transform_data(TransImageRec,Rest);

get_transform_data(ImageRec,[{"crop",[{"width",""},{"height",""},{"left",""},{"top",""}]}|Rest]) ->
	get_transform_data(ImageRec,Rest);
get_transform_data(ImageRec,[{"crop",[{"width",Width},{"height",Height},{"left",Left},{"top",Top}]}|Rest]) ->
		TransImageRec=image_transform:transform_image_extended(ImageRec,{"crop",[Width,Height,Left,Top]}),
	get_transform_data(TransImageRec,Rest);	

get_transform_data(ImageRec,[{"cropToCircle",[{"width",""},{"height",""}]}|Rest]) ->
	get_transform_data(ImageRec,Rest);	

get_transform_data(ImageRec,[{"cropToCircle",[{"width",Width},{"height",Height}]}|_Rest]) ->
	TransImageRec=image_transform:crop_image_2_circle(ImageRec,list_to_integer( Width),list_to_integer( Height)),
	get_transform_data(TransImageRec,[]).



check_format(#image{extn = ".jpg"},{"finalFormat","to_jpg"}) ->
	not_needed;
check_format(#image{extn = ".jpeg"},{"finalFormat","to_jpg"}) ->
	not_needed;	
check_format(#image{extn = ".png"},{"finalFormat","to_png"}) ->
	not_needed;
check_format(_,_) ->
	{ok,convert}.


get_post_values_list([],KeyValueList) ->
	get_prop_trans(KeyValueList,[],[]);

get_post_values_list([{Transform,Value}|Rest],KeyValueList) ->
	[Trans,ImageName,Extn]=string:tokens(binary_to_list(Transform),"_"),
	NewTupleList= lists:keystore("image",1,KeyValueList,{"image",ImageName++"."++Extn}),
	NewTupleList1= lists:keystore(Trans,1,NewTupleList,{Trans,binary_to_list(Value)}),
	get_post_values_list(Rest,NewTupleList1). 


get_prop_trans([],FinalValues,OtherTrans) ->
	FinalValues ++get_othr_trans(OtherTrans,[],length(OtherTrans));
get_prop_trans([{Transform,Value}|Rest],FinalValues,OtherTrans)  ->	
	case string:tokens(Transform,"-") of
		[Trans,TransParam] ->
			get_prop_trans(Rest,FinalValues,[{Trans,[{TransParam,Value}]}]++ OtherTrans);
		[Trans] ->
			get_prop_trans(Rest,[{Trans,Value}]++ FinalValues,OtherTrans)
	end.

get_othr_trans([{Trans,Param}],Values,1) ->
	[{Trans,Param++Values}]++[{"transform",Trans}];
get_othr_trans([{_Trans,Param}|Rest],Values,LengthOfList) ->
	get_othr_trans(Rest,Param++Values,LengthOfList-1).



terminate(_Reason, _Req, _State) ->
	ok.
