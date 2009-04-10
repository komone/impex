%%
%% Tuple Importer/Exporter
%% Copyright (c) 2009 Steve Davis <steven.charles.davis@gmail.com>. All rights reserved.
%% LICENSE: Creative Commons Non-Commercial License V 3.0 
%% http://creativecommons.org/licenses/by-nc/3.0/us/
%%

-record(form, {datatype, root, mods=[], defs=[]}).

-record(def, {type, patterns=[], transforms=[]}).
