%% all the html at the front end are handled in this module

-module(trans_html).
-include("image_transform.hrl").
-export([provide_html/3,get_output_snippet/3]).



provide_html(ImageRec,CropTrans,Url) ->
    io:format("provide_html :: ImageRec: ~p,CropTrans: ~p,Url: ~p ~n",[ImageRec,CropTrans,Url]),
    {image,_FulImgName,ImgName,Width,Height,_,Size,Format} = ImageRec,
    [Extn] = string:tokens(Format,"."),
	{Title,ExtraSnippet,TransSnippet} = 
	case  CropTrans of
		"crop" ->
			get_crop_snippets(<<"crop">> ,ImgName,Extn);
		"cropToCircle" ->
			get_crop_snippets(<<"cropToCircle">>,ImgName,Extn);
		"resize" ->
			get_crop_snippets(<<"resize">>,ImgName,Extn)
	end,
	CropHtml = "<html>
        <head>
        <style type=\"text/css\">
        body    {
            margin: auto;
            }
        #tableform  {   
            position: absolute; 
            top: 47%;
            left: 20%;
            margin:10px auto;
            border: medium groove gray;
            background:white;
            padding:10px;   
            width:800px;
            height: 310px;  
                }

        .imgDetails{
            position: absolute; 
            top: 3%;
            left: 20%;
            margin:5px auto;
            border: medium groove gray;
            color:white;
            background:#808080;
            padding:10px;
            width:800px;
            height: 270px;
            float:left;         
            list-style-type: disc;
            padding-top: 5px;
        }
        
        #load_screen{
        background: white;
        z-index:10;
        position: fixed;
        top:0px;
        width:100%;
        height:100%;
        min-height: 480px;
        }
        #load_screen .loading {
        color:#FFF;
        width:100%;
        height:20px;
        margin:250px auto;
        text-align: center;
        }      
        </style>
        <script language=\"javascript\" type=\"text/javascript\">
            window.addEventListener(\"load\",function(){
                var load_screen = document.getElementById(\"load_screen\");
                document.body.removeChild(load_screen);
                });
        </script>
        </head> 
        <body>
		<div id = \"load_screen\"><div class = \"loading\"> <img src= \"http://localhost:8088/loading.gif\"> </div></div>
            <div class =\"imgDetails\">
                <p> 
                <img src="++Url++" alt=\"Original Image\" height=\"250\" width=\"500\" align = \"right\" >
                <ul>
                    <li> <b> Original Image Details : </b> </li><br>
                  <li>Image size: "++Size ++" </li>
                  <li>Image WidthxHeight: "++Width++"x"++Height++"</li>
                  <li>format: "++Format++"</li>
                </ul>   
                </p>"++ExtraSnippet++"</div>      
            <div id = \"tableform\">
                <h2 align= \"center\">"++Title++" </h2>
                <form action=\"/imgdetails\" type = \"text\" method=\"post\">
                    Rotation : <select name=\"rotation_"++ImgName++"_"++Extn++"\">
                            <option value=\"0_clock\">No Rotation</option>
                            <option value=\"90_clock\">90 deg clockwise</option>
                            <option value=\"90_antiClock\">90 deg anti-clockwise</option>
                            <option value=\"180_clock\">180 deg clockwise</option>
                            <option value=\"180_antiClock\">180 deg anti-clockwise</option>
                        </select><br><br/>
                    Scale to %: <select name=\"scale_"++ImgName++"_"++Extn++"\">
                                <option value=\"0_Scale\">No Scale</option>
                                <option value=\"25_scale\">Scale to 25%</option>
                                <option value=\"50_scale\">Scale to 50%</option>
                                <option value=\"75_scale\">Scale to 75%</option>
                            </select>"++TransSnippet++"<br><br>
                    Image Format: <select name=\"finalFormat_"++ImgName++"_"++Extn++"\">
                                    <option value=\"\">Format</option>
                                    <option value=\"to_jpg\">JPEG</option>
                                    <option value=\"to_png\">PNG</option>
                                </select>
                    <br><br>
                    <input type=\"submit\" value=\"submit\" />
                    <input type=\"reset\" value=\"reset\" />

                </form>     
            </div>
        </body>",
list_to_binary(CropHtml).



get_crop_snippets(<<"crop">>,ImgName,Extn) ->
	Title = "Choose Details to Crop the image",
	ExtraSnippet = 
		"<form action=\"/resizeimg\" method=\"post\">
	      <button name=\"resize\" type=\"submit\" value=\"resize_"++ImgName++"_"++Extn++"\">Resize Image</button>
	    </form> <br> 

	    <form action=\"/cropimg\" method=\"post\">
	      <button name=\"crop\" type=\"submit\" value=\"cropToCircle_"++ImgName++"_"++Extn++"\">Crop Image to Circle</button>
	    </form>",
	CropSnippet = 
		"<br><br>Crop Image by Width, Height, Left and Top Values <br><br>
	    Width: <input type=\"text\" name=\"crop-width_"++ImgName++"_"++Extn++"\" size=\"7\"/>
	    Height: <input type=\"text\" name=\"crop-height_"++ImgName++"_"++Extn++"\" size=\"7\"/>
	    Left: <input type=\"text\" name=\"crop-left_"++ImgName++"_"++Extn++"\" size=\"7\"/>
	    Top: <input type=\"text\" name=\"crop-top_"++ImgName++"_"++Extn++"\" size=\"7\"/>",
	{Title,ExtraSnippet,CropSnippet};

get_crop_snippets(<<"cropToCircle">>,ImgName,Extn) ->
	Title = "Choose Details to Crop the image to Circle",
	ExtraSnippet = 
		"<form action=\"/resizeimg\" method=\"post\">
	      <button name=\"resize\" type=\"submit\" value=\"resize_"++ImgName++"_"++Extn++"\">Resize Image</button>
	    </form> <br>
	     <form action=\"/cropimg\" method=\"post\">
	      <button name=\"crop\" type=\"submit\" value=\"crop_"++ImgName++"_"++Extn++"\">Crop Image</button> <br>
	    </form> ",
	CropSnippet = 
		"<br><br>Crop Image To Circle by Width and Height <br><br>
	    Width: <input type=\"text\" name=\"cropToCircle-width_"++ImgName++"_"++Extn++"\" size=\"7\"/>
	    Height: <input type=\"text\" name=\"cropToCircle-height_"++ImgName++"_"++Extn++"\" size=\"7\"/>",
    {Title,ExtraSnippet,CropSnippet};


get_crop_snippets(<<"resize">>,ImgName,Extn) ->
	Title = "Choose Details to Resize the image",
	ExtraSnippet =
        "<form action=\"/cropimg\" method=\"post\">
          <button name=\"crop\" type=\"submit\" value=\"crop_"++ImgName++"_"++Extn++"\">Crop Image</button> <br><br>
          <button name=\"crop\" type=\"submit\" value=\"cropToCircle_"++ImgName++"_"++Extn++"\">Crop Image to Circle</button>
        </form>",    
    ResizeSnippet =            
        "<h3>OR</h3> Resize Image by entering Width and Height <br><br>
        Width: <input type=\"text\" name=\"resize-width_"++ImgName++"_"++Extn++"\" size=\"7\"/>
        Height: <input type=\"text\" name=\"resize-height_"++ImgName++"_"++Extn++"\" size=\"7\"/>",
       {Title,ExtraSnippet,ResizeSnippet}.

get_output_snippet(Url,Width,Height) ->
	OutputSnippet = 
		"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
		<html xmlns=\"http://www.w3.org/1999/xhtml\">
		<head>
		<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
		<title>Transformation of Images</title>
		<style>
		body,p {
		    margin: 0;
		    padding: 0;
		    text-align: left;
		    font-family:\"Adobe Garamond Pro Bold\", Georgia, \"Times New Roman\", Times, serif;
		    color: white;
		    background: #25060B;
		    background-repeat:repeat-x;
		}
		*
		{
		  margin: 0 auto 0 auto;
		 text-align:left;
		}
		</style>
		</head>

		<body>
		<br><br>
		<form>
		    <input Type=\"BUTTON\" value=\"Transform Another Image\" Onclick=\"window.location.href='http://localhost:8088/index.html'\"> 
		</form>
		<br>
		<h4> Click on the image to download to save to your local drive </h4><br><br>   
		<p>
        <a href ="++ Url++" download>
		    <img src="++Url++" alt=\"Transformed Image\" height="++Height++" width="++Width++" align = \"left\" download>
		</a>
        </p>
		</body>
		</html>",
	OutputSnippet.       