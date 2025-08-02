%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
surf(peaks);
%=============================================================================
f = figure();
[X,Y] = meshgrid(1:0.5:10,1:20);
Z = sin(X) + cos(Y);
S = surf(X, Y, Z);
%=============================================================================
P = properties(S);
REF = {'AlphaData';
'AlphaDataMapping';
'AmbientStrength';
'BackFaceLighting';
'BeingDeleted'; 
'BusyAction';   
'CData';    
'CDataMapping'; 
'CDataMode';
'Children'; 
'CreateFcn';
'DeleteFcn';
'DiffuseStrength';
'EdgeAlpha';
'EdgeColor';
'EdgeLighting'; 
'FaceAlpha';
'FaceColor';
'FaceLighting'; 
'Interruptible';
'LineStyle';
'LineWidth';
'Marker';   
'MarkerEdgeColor';         
'MarkerFaceColor';
'MarkerSize';   
'MeshStyle';
'Parent';   
'SpecularColorReflectance';
'SpecularExponent';        
'SpecularStrength';     
'Tag';      
'Type';     
'UserData'; 
'VertexNormals';
'Visible';  
'XData';    
'XDataMode';
'YData';    
'YDataMode';
'ZData'};
assert_isequal(P, REF);
%=============================================================================
assert_isequal(S.Type, 'surface');
%=============================================================================
