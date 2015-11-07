%%@Image_Transform_Service

-module(image_transform).

-include("image_transform.hrl").

-export([start_apps/0]).
-export([resize_img/3,
	crop_img/5,
	rotate_exif/1,
	convert_to_jpg/1,
	scale_img/2,
	crop_img_to_circle/3,
	crop_image_2_circle/3,
	get_img_details_from_url/1,
	get_img_details_from_upload/3,
	transform_image_extended/2,
	identify_file/1,
	adjust_image_size/2]).
-export([to_int/1]).
-define(IMG_DIR,code:priv_dir(image_web_transform)++"/images/").
-define(STR_LIST(A),[[L]||L<-A]).
-define(DSTN_DIR,"/home/raju/Desktop/Storage/").




%%===========================================================================
%% function: start_apps/0
%%@doc
%%make sure to start this function when using this module seperately before doing any transforamtion
%%end
-spec start_apps() -> Result when
	Result :: ok | {error,Reason},
	Reason :: term().
start_apps() ->
	application:start(inets),
	application:start(asn1),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%							MAIN API TRANSFORM FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%===========================================================================


-spec resize_img(ImageUrl :: binary(),ReqWidth :: integer(),
	ReqHeight :: integer()) -> Result when 
	Result :: {FinalImageRec,TransformedImgData}|
			{error,Reason}|{error,ResponseCode,Reason},
			FinalImageRec :: #image_tr{},
			TransformedImgData :: binary(),
			ResponseCode :: integer(),
			Reason :: term().
resize_img(ImageUrl,ReqWidth,ReqHeight) ->
	case get_img_details_from_url(ImageUrl) of
		{error,Reason} ->
			{error,Reason};
		{error,ResponseCode,Reason} ->
			{error,ResponseCode,Reason};
		ImageRec ->
			if (ReqWidth =< 0) or (ReqHeight =< 0) ->
				lager:error("Width_And_Height_Cannot_Be_Less_Than_Or_Equal_To_Zero"),
				{error,"Width_And_Height_Cannot_Be_Less_Than_Or_Equal_To_Zero"};
			true ->
				NewWidth = check_width(ImageRec#image.width,
								to_int(ReqWidth),"initial"),
				NewHeight = check_height(ImageRec#image.height,
							to_int(ReqHeight),"initial"),
				FinalImageRec = transform_image_extended(ImageRec,
									{"resize",[integer_to_list(NewWidth),
									integer_to_list(NewHeight)]}),
				ImageName = FinalImageRec#image_tr.name,
				{ok,TransformedImgData} = file:read_file(
												?IMG_DIR++ ImageName),
%%				file:write_file(?DSTN_DIR++ImageName,TransformedImgData),
				delete_temp_files(ImageName),
				{FinalImageRec,TransformedImgData}
			end
	end.

%%===========================================================================
%% function: scale_img/2
%%@doc
%% Scale the image according to the given input Scale Percentage  and
%% returns the transformed image details with Transformed image binary Data
%%end
-spec scale_img(ImageUrl :: binary(),ScaleFactor :: integer()) -> Result when 
	Result :: {FinalImageRec,TransformedImgData}|
			{error,Reason}|{error,ResponseCode,Reason},
			FinalImageRec :: #image_tr{},
			TransformedImgData :: binary(),
			ResponseCode :: integer(),
			Reason :: term().
scale_img(ImageUrl,ScaleFactor) ->
	if (ScaleFactor =< 0) or (ScaleFactor == undefined) ->
		{error,"Invalid_ScaleFactor"};
	true ->
		case get_img_details_from_url(ImageUrl) of
			{error,Reason} ->
				{error,Reason};
			{error,ResponseCode,Reason} ->
				{error,ResponseCode,Reason};
			ImageRec ->
				FinalImageRec = transform_image_extended(ImageRec,
									{"scale",[integer_to_list(
									ScaleFactor)]}),
				ImageName = FinalImageRec#image_tr.name,
				{ok,TransformedImgData} = file:read_file(
												?IMG_DIR++ ImageName),
%%				file:write_file(?DSTN_DIR++ImageName,TransformedImgData),
				delete_temp_files(ImageName),
				{FinalImageRec,TransformedImgData}
		end
	end.

%%===========================================================================
%% function: crop_img/5
%%@doc
%% Crop the image according to the given input width,height, Left and Top values  and
%% returns the transformed image details with Transformed image binary Data
%%end
-spec crop_img(ImageUrl :: binary(),ReqWidth :: integer(),
				ReqHeight :: integer(),XOffset :: integer(),
				YOffset :: integer()) -> Result when 
	Result :: {FinalImageRec,TransformedImgData}|
			{error,Reason}|{error,ResponseCode,Reason},
			FinalImageRec :: #image_tr{},
			TransformedImgData :: binary(),
			ResponseCode :: integer(),
			Reason :: term().
crop_img(ImageUrl,ReqWidth,ReqHeight,XOffset,YOffset) ->
	case get_img_details_from_url(ImageUrl) of
		{error,Reason} ->
			{error,Reason};
		{error,ResponseCode,Reason} ->
			{error,ResponseCode,Reason};
		ImageRec ->
			if (ReqWidth =< 0) or (ReqHeight =< 0) or 
			(XOffset == undefined) or (YOffset == undefined) ->
				{error,"Invalid_Width_OR_Height_OR_Left_Top_Details"};
			true ->
				NewWidth = check_width(ImageRec#image.width,
								to_int(ReqWidth),"initial"),
				NewHeight = check_height(ImageRec#image.height,
								to_int(ReqHeight),"initial"),
				CropWidth = check_width(ImageRec#image.width,NewWidth,""),
				CropHeight = check_height(ImageRec#image.height,NewHeight,""),
				FinalImageRec = transform_image_extended(ImageRec,{"crop",
					[integer_to_list(CropWidth),integer_to_list(CropHeight),
					integer_to_list(to_int(XOffset)),integer_to_list(
					to_int(YOffset))]}),
				ImageName = FinalImageRec#image_tr.name,
				{ok,TransformedImgData} = file:read_file(
												?IMG_DIR++ ImageName),
%%				file:write_file(?DSTN_DIR++ImageName,TransformedImgData),
				delete_temp_files(ImageName),
				{FinalImageRec,TransformedImgData}
			end
	end.

%%===========================================================================
%% function: crop_img_to_circle/3
%%@doc
%% Crop the image to circle according to the given input width,height values  and
%% returns the transformed image details with Transformed image binary Data
%%end
-spec crop_img_to_circle(ImageUrl :: binary(),ReqWidth :: integer(),
				ReqHeight :: integer()) -> Result when 
	Result :: {FinalImageRec,TransformedImgData}|
			{error,Reason}|{error,ResponseCode,Reason},
			FinalImageRec :: #image_tr{},
			TransformedImgData :: binary(),
			ResponseCode :: integer(),
			Reason :: term().
crop_img_to_circle(ImageUrl,ReqWidth,ReqHeight) ->
	case get_img_details_from_url(ImageUrl) of
		{error,Reason} ->
			{error,Reason};
		{error,ResponseCode,Reason} ->
			{error,ResponseCode,Reason};
		ImageRec ->
			if (ReqWidth =< 0) or (ReqHeight =< 0) ->
				{error,"Width_And_Height_Cannot_Be_Less_Than_Or_Equal_To_Zero"};
			true ->
				NewWidth = check_width(ImageRec#image.width,
								to_int(ReqWidth),"initial"),
				NewHeight = check_height(ImageRec#image.height,
								to_int(ReqHeight),"initial"),
				FinalImageRec = crop_image_2_circle(ImageRec,NewWidth,NewHeight),
				ImageName = FinalImageRec#image_tr.name,
				{ok,TransformedImgData} = file:read_file(
												?IMG_DIR++ ImageName),
				file:write_file(?DSTN_DIR++ImageName,TransformedImgData),
				delete_temp_files(ImageName),
				{FinalImageRec,TransformedImgData}
			end
	end.

%%===========================================================================
%% function: convert_to_jpg/1
%%@doc
%% Convert the image to Jpg format and
%% returns the transformed image details with Transformed image binary Data
%%end
-spec convert_to_jpg(ImageUrl :: binary()) -> Result when 
	Result :: {FinalImageRec,TransformedImgData}|
			{error,Reason}|{error,ResponseCode,Reason},
			FinalImageRec :: #image_tr{},
			TransformedImgData :: binary(),
			ResponseCode :: integer(),
			Reason :: term().
convert_to_jpg(ImageUrl) ->
	case get_img_details_from_url(ImageUrl) of
		{error,Reason} ->
			{error,Reason};
		{error,ResponseCode,Reason} ->
			{error,ResponseCode,Reason};
		ImageRec ->
			FinalImageRec = transform_image_extended(ImageRec,{"to_jpg",[]}),
			ImageName = FinalImageRec#image_tr.name,
			{ok,TransformedImgData} = file:read_file(
											?IMG_DIR++ ImageName),
%%				file:write_file(?DSTN_DIR++ImageName,TransformedImgData),
			delete_temp_files(ImageName),
			{FinalImageRec,TransformedImgData}
	end.


%%===========================================================================
%%rotate_exif/1
%%@doc
%% auto orients the image 
%% and returns the imageRecord of the transformed image.
%%end

-spec rotate_exif(ImageRec :: #image_tr{}|#image{})-> Result when
	Result :: ImageRec | {error,Reason},
	ImageRec :: #image_tr{},
	Reason :: term().
rotate_exif(ImageRec) ->
	transform_image_extended(ImageRec,{"rotate_exif",[]}).

%%===========================================================================
%%crop_image_2_circle/3
%%@doc
%% Crop the image to circle 
%% and returns the imageRecord of the transformed image.
%%end	

-spec crop_image_2_circle(ImageRec :: #image{},Width::integer(),
		Height :: integer())-> Result when
	Result :: ImageRec | {error,Reason},
	ImageRec :: #image_tr{},
	Reason :: term().
crop_image_2_circle(ImageRec,Width,Height) ->
	case resize_image(ImageRec,Width,Height,"ScaleContainBoth") of
		{error,Reason} ->
			{error,Reason};
		TImageRec ->
			{WidthCenter,HeightCenter,XEdge,YEdge} = 
				get_circle_coordinates(									
					to_int(TImageRec#image_tr.width),
					to_int(TImageRec#image_tr.height)),  
			transform_image_extended(ImageRec,{"CropImageToCircle",
				[TImageRec#image_tr.width,
				TImageRec#image_tr.height|
				[integer_to_list(X)||X<-[WidthCenter,HeightCenter,XEdge,YEdge]]]})
	end.

%%===========================================================================
%%scale_image/2
%%@doc
%% Scale the image to given Percentage
%% and returns the imageRecord of the transformed image.
%%end

-spec scale_image(ImageRec :: #image_tr{}|#image{},ScaleFactor::integer())-> Result when
	Result :: ImageRec | {error,Reason},
	ImageRec :: #image_tr{},
	Reason :: term().
scale_image(ImageRec,ScaleFactor)	->
	transform_image_extended(ImageRec,{"scale",
		[integer_to_list(ScaleFactor)]}).

%%===========================================================================
%%resize_image/3
%%@doc
%% Resize the image to given width and height with respective scalemode
%% and returns the imageRecord of the transformed image.
%%end
-spec resize_image(ImageRec :: #image_tr{}|#image{},Width::integer(),
		Height :: integer(),ScaleMode :: string())-> Result when
	Result :: ImageRec | {error,Reason},
	ImageRec :: #image_tr{},
	Reason :: term().
resize_image(ImageRec,Width,Height,ScaleMode) when is_record(ImageRec,image) ->
	WxH = ImageRec#image.width++"x"++ImageRec#image.height,
	{SWidth,SHeight} = get_scaled_width_height(WxH,to_int(Width),
													to_int(Height),
													ScaleMode),
	transform_image_extended(ImageRec,{"resize",[integer_to_list(
								round(SWidth)),integer_to_list(
								round(SHeight))]});
resize_image(ImageRec,Width,Height,ScaleMode)->
	WxH = ImageRec#image_tr.width++"x"++ImageRec#image_tr.height,
	{SWidth,SHeight} = get_scaled_width_height(WxH,to_int(Width),
												to_int(Height),
												ScaleMode),
	transform_image_extended(ImageRec,{"resize",[integer_to_list(
								round(SWidth)),integer_to_list(
								round(SHeight))]}).



%%===========================================================================
%%transform_image_extended/2
%%@doc
%% does the actual tranforms and store the tranformed image to /tmp/ directory 
%% and returns the imageRecord of the final transformed image.
%%end
-spec transform_image_extended(ImageRec :: #image_tr{}|#image{},{OpName :: string(),
			OpData :: list()}) -> Result when 
	Result :: ImageRec | {error, Reason},
	ImageRec :: #image_tr{},
	Reason :: term().
transform_image_extended(ImageRec,{OpName,OpData}) ->
	io:format("~ntransform_image_extended :: ~p,~p,~p,~n",[ImageRec,OpName,OpData]),
	ImageName = case is_record(ImageRec,image) of
					true ->
						ImageRec#image.name;
					false ->
						ImageRec#image_tr.name
				end,
	FileName = get_filename(OpName,ImageName),
	case OpName of
		"resize"  ->
			[W,H] = OpData,	
			Cmd = "convert " ++ ?IMG_DIR++ImageName ++ " -resize "++W++"x"++H++ " "++?IMG_DIR++ FileName,
			os:cmd(Cmd);
		"crop"	->
			[W,H,X,Y] = OpData,
			Cmd = "convert -crop "++W++"x"++H++"+"++X++"+"++Y++" "++?IMG_DIR++ImageName++" "++?IMG_DIR++ FileName,
			os:cmd(Cmd);			
		"scale"	->
			[ScaleFactor] = OpData,
			Cmd = "convert " ++ ?IMG_DIR++ImageName ++ " -resize "++ScaleFactor++"% " ++?IMG_DIR++ FileName,
			os:cmd(Cmd);	
		"rotation" ->
			[Degrees] = OpData,	
				%%convert cup.jpg -rotate -90 cup_a90.jpg
			Cmd = "convert " ++ ?IMG_DIR++ImageName ++ " -rotate "++Degrees++" "++?IMG_DIR++ FileName,
			os:cmd(Cmd);			
		"to_jpg" ->
			Cmd = "convert " ++ ?IMG_DIR++ImageName++" "++?IMG_DIR++ FileName,
			os:cmd(Cmd);	
		"to_png" ->
			Cmd = "convert " ++ ?IMG_DIR++ImageName++" "++?IMG_DIR++ FileName,
			os:cmd(Cmd);			
		"rotate_exif" ->
			Cmd = "convert " ++ ?IMG_DIR++ImageName++" -auto-orient "++?IMG_DIR++ FileName,
			os:cmd(Cmd);
		"CropImageToCircle" ->
			[ImgWidth,ImgHeight,WidthCenter,HeightCenter,XEdge,YEdge] = OpData,
			WdxHt = ImgWidth++"x"++ImgHeight,
			Cmd ="convert "++?IMG_DIR++ImageName++" \\( -size "++WdxHt++" xc:none -fill white -draw 'circle "++WidthCenter++","++HeightCenter++" "++XEdge++","++YEdge++"' \\) -compose copy_opacity -composite "++?IMG_DIR++FileName,
			os:cmd(Cmd)
	end,
	os:cmd("convert -strip -interlace Plane -quality 80 "++FileName++" "++FileName),
	case identify_img(FileName,?IMG_DIR) of
		{WxH,Size,Typ} ->
			[Wd,Ht]=string:tokens(WxH,"x"),
      		Shortname =filename:rootname(FileName),
			TransformImageRec = #image_tr{short_name = Shortname, 
			name = FileName,width = Wd,height=Ht,type = Typ, size = Size},
			TransformImageRec;
		{error,Reason} ->
			{error,Reason}
	end.




%%===========================================================================
%% get_scaled_width_height/4
%%@doc
%% returns the image width and height according to the ScaleMode
%%@end

-spec get_scaled_width_height(WxH :: string(),Width:: integer(),
		Height :: integer(),ScaleMode :: string()) -> Result when 
		Result :: {integer()|float(),integer()|float()}.
get_scaled_width_height(WxH,Width,Height,ScaleMode) ->
	[ImageWidth,ImageHeight]=string:tokens(WxH,"x"),
	AspectRatio =  to_int(ImageWidth)/to_int(ImageHeight),
	WidthScaledToHeight = AspectRatio* Height,
	HeightScaledToWidth = Width/AspectRatio,
	case ScaleMode of 
		"ScaleContainBoth" when (WidthScaledToHeight < Width) ->
			{Width,HeightScaledToWidth};
		"ScaleContainBoth" when (HeightScaledToWidth < Height) ->
			{WidthScaledToHeight,Height};
		"ScaleContainBoth" ->
			{Width,HeightScaledToWidth};
		"ScaleFitBoth" when (WidthScaledToHeight > Width) ->
			{Width,HeightScaledToWidth};
		"ScaleFitBoth" when (HeightScaledToWidth > Height) ->
			{WidthScaledToHeight,Height};
		"ScaleFitBoth" ->
			{Width,HeightScaledToWidth};
		"ScaleFitHeight" ->
			{WidthScaledToHeight,Height};
		"ScaleFitWidth" ->
			{Width,HeightScaledToWidth}
	end.


%%===========================================================================
%% get_img_details_from_url/4
%%@doc
%% returns the Original image Record and stores it in /tmp/ dir
%%@end
-spec get_img_details_from_url(ImageUrl :: binary()|undefined) -> Result
	when Result :: ImageRec | {error,Reason}|{error,ResponseCode,Reason},
	ImageRec :: #image{},
	ResponseCode :: integer(),
	Reason :: term().

get_img_details_from_url(ImageUrl) when ImageUrl == undefined ->
	{error,"Missing_ImageUrl_Details."};
get_img_details_from_url(ImageUrl) when ImageUrl /= undefined ->
	case lists:member(" ",?STR_LIST(binary_to_list(ImageUrl))) of
		false ->
			case httpc:request(binary_to_list(ImageUrl)) of
				{ok,{{"HTTP/1.1",200,"OK"},Headers,ImgData}} ->
					{_,Type}=lists:keyfind("content-type",1,Headers),
					[_,Extn]=string:tokens(Type,"/"),
					ImgExtn = 
					case Extn of
						"jpeg" ->
							".jpg";
						Other ->
							"."++Other
					end,
					get_img_details_from_upload(ImgData,ImgExtn,Type);
				{ok,{{"HTTP/1.1",ResponseCode,_Reason},_,_}} ->
					{error,ResponseCode,"Invalid_ImageUrl_Parameter"};
				{error,_Reason} ->
					{error,"Invalid_ImageUrl_Parameter"}
			end;
		true ->
			{error,"Invalid_ImageUrl_Parameter"}
	end.

get_img_details_from_upload(ImgData,Extn,Type) ->
	ShortName = get_image_name(),
	FileName = ShortName++Extn,
	OrigFileName = ShortName++"_orig"++Extn,
	PrivDir = code:priv_dir(image_web_transform),
	io:format("Private Dir~p~n",[PrivDir]), 
	file:write_file(PrivDir++"/images/"++FileName,ImgData),
	file:write_file(PrivDir++"/images/"++OrigFileName,ImgData),
	io:format("File name :: ~p~n",[PrivDir++"/images/"++FileName]),
	{WxH,Size,_Typ}= identify_img(FileName,PrivDir++"/images/"),
	[W,H]=string:tokens(WxH,"x"),
	ImageRec = #image{name = FileName, 
				short_name = ShortName,width = W,height=H,
				type = Type, size = Size,extn =Extn},

	ImageRec.


%%===========================================================================
get_image_name() ->
    {L,M,N} = erlang:timestamp(),
    string:join([integer_to_list(L),ran_alpha(),ran_alpha(),
        integer_to_list(M),ran_alpha(),ran_alpha(),integer_to_list(N)],"").

ran_alpha() ->
    List ="ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Index = random:uniform(length(List)),
    [lists:nth(Index,List)].


%%===========================================================================
%%adjust_image_size/2
%%@Doc
%% decreases the size of the image if the tranformed image size is 
%%more than the requested and returns the final record
%%@end
-spec adjust_image_size(ImageRec :: #image_tr{},MaxSizeInKB :: string()) -> 
		#image_tr{}.
adjust_image_size(ImageRec,MaxSizeInKB) ->
	case check_image_size(ImageRec#image_tr.size,MaxSizeInKB) of
		{ok,"less"}->
			ImageRec;
		{error,"more"}	->
			NewImageRec = scale_image(ImageRec,75),
			adjust_image_size(NewImageRec,MaxSizeInKB)
	end.

%%===========================================================================
%%check_image_size/2
%%@Doc
%% checks the size of the Tranformed image  with the given input size
%%@end
-spec check_image_size(Size :: string(),MaxSize :: string())-> 
		Result when Result :: {ok,Output}|{error,Output},
		Output :: string().
check_image_size(Size,MaxSize) ->
	Siz = convert_to_bytes(Size),
	MaxSiz = convert_to_bytes(MaxSize),
	case Siz =< MaxSiz of
		true ->
			{ok,"less"};
		false ->
			{error,"more"}
	end.

%%===========================================================================
%%convert_to_bytes/2
%%@Doc
%% Converts the size of the Tranformed image  to bytes.
%%@end
-spec convert_to_bytes(Size :: string())-> integer().
convert_to_bytes(Size) ->
	SizeL = ?STR_LIST(Size),
	Len = length(SizeL),
	KbOrMbSize = lists:flatten(lists:nthtail(Len-2,SizeL)),
	case  KbOrMbSize of
		"MB" ->
			(to_int(lists:flatten(lists:sublist(SizeL,length(SizeL)-2))))*1000000;
		"KB" ->
			to_int(lists:flatten(lists:sublist(SizeL,length(SizeL)-2)))*1000
	end.

%%===========================================================================
%%get_circle_coordinates/2
%%@Doc
%% gets the circle cordinates so as to crop the image to circle.
%%@end
-spec get_circle_coordinates(ImageWidth :: integer(),ImageHeight :: integer()) -> 
	{integer(),integer(),integer(),integer()}.
get_circle_coordinates(ImageWidth,ImageHeight)->
	WidthCenter = to_int(ImageWidth/2.0),
	HeightCenter = to_int(ImageHeight/2.0),
	if (ImageWidth>ImageHeight) ->
		{WidthCenter,HeightCenter,WidthCenter,0};
	true ->
		{WidthCenter,HeightCenter,0,HeightCenter}
	end.


%%===========================================================================
%%to_int/1
%%@Doc
%%converts to integer leaving atom behind
%%@end
-spec to_int(Val :: integer()|string()|atom()|binary()|float()) -> 
			Val :: integer() |atom().
to_int(Val) when is_integer(Val) ->
	Val;
to_int(Val) when is_atom(Val) ->
	Val;
to_int(Val) when is_binary(Val) ->
	to_int(binary_to_list(Val));
to_int(Val) when is_float(Val) ->
	to_int(float_to_list(Val,[{decimals,0}]));
to_int(Val) when is_list(Val) ->
	case lists:member(".",?STR_LIST(Val)) of
		true ->
			to_int(list_to_float(Val));
		false ->
			to_int(list_to_integer(Val))
	end.

%%===========================================================================
%%check_width/3
%%@Doc
%%check the width values and returns the required changed width value
%%@end
-spec check_width(ImageWidth :: integer(), Width :: integer()|atom(),
			Option :: string())-> integer().
check_width(ImageWidth,Width,Option ) ->
	if Option == "initial" ->
		if Width == undefined ->
			to_int(ImageWidth);
		true ->
			to_int(Width)
		end;
	true ->
		if ImageWidth < Width ->
			to_int(ImageWidth);
		true ->
			to_int(Width)
		end
	end.

%%===========================================================================
%%check_height/3
%%@Doc
%%check the height values and returns the required changed height value
%%@end
-spec check_height(ImageHeight :: integer(),Height :: integer()|atom(),
		Option :: string())-> integer().
check_height(ImageHeight,Height,Option) ->
	if Option == "initial" ->
		if Height == undefined ->
			to_int(ImageHeight);
		true ->
			to_int(Height)
		end;
	true ->
		if ImageHeight < Height ->
			to_int(ImageHeight);
		true ->
			to_int(Height)
		end
	end.

%%===========================================================================
%%get_filename/3
%%@Doc
%%get the filename with the extension attached according to the option given as input
%%@end
-spec get_filename(OpName :: string(),ImgName :: string())-> string().
get_filename(OpName,ImgName)->
	Img = filename:rootname(ImgName),
	case OpName of 
		"to_jpg" ->
			Img ++ ".jpg";
		"to_png" ->
			Img ++ ".png";
		"CropImageToCircle" ->
			Img ++ ".png";
		_Other ->
			ImgName
	end.

%%===========================================================================
%%identify_img/2
%%@Doc
%%get the properties of the image which is present in the dir given
%%@end
-spec identify_img(Img :: string(),Dir :: string()) -> 
			{WxH :: string(),Typ :: string(),Size ::string()}| 
			{error,Reason :: term()}.
identify_img(Img,Dir) ->
	case string:tokens(os:cmd("identify "++Dir++Img)," ") of
		[_Src,Typ,WxH,_,_,_,Size,_,_]->
			 {WxH,Size,Typ};
		Reason ->
			{error,string:join(Reason," ")}
	end.




identify_file(ImageName) ->
    Extn = filename:extension(ImageName),
    PrivDir = code:priv_dir(image_web_transform),
    {WxH,Size,Type}= identify_img(ImageName,PrivDir++"/images/"),
    [W,H]=string:tokens(WxH,"x"),
    ShortName = filename:rootname(ImageName),
    ImageRec = #image{name = ImageName, 
                short_name = ShortName,width = W,height=H,
                type = "image/"++Type, size = Size,extn =Extn},
    ImageRec.

%%===========================================================================
%%@doc
%% deletes the given image png and jpg files from temp directory
%%make sure to do this operation at end.
%%end
-spec delete_temp_files(ImageName :: string()) -> ok.
delete_temp_files(ImageName) ->
	Img =filename:rootname(ImageName),
	os:cmd("rm -rf /tmp/"++Img++".jpg /tmp/"++Img++".png"),
	ok.



