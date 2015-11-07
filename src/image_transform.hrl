%%@doc
%% record fields which have the Original image details
%%end

-record(image,{
				name,
			  	short_name,
				width,
				height,
				type,
				size,
				extn
				}).

%%@doc
%% record fields which have the tranformed image details
%%end
-record(image_tr,{
					name,
				  	short_name,
					width,
					height,
					type,
					size,
				  	transform_str
					}). 
