%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f  = figure();
t = 0:pi/50:10*pi;
L = plot3(sin(t), cos(t), t);
axis square
assert_istrue(isgraphics(L, 'line'));
P = properties(L);
REF = {'BeingDeleted';
'BusyAction';
'Children';       
'Color';
'CreateFcn';
'DeleteFcn';          
'DisplayName';    
'Interruptible';
'LineStyle';      
'LineWidth';      
'Marker';         
'MarkerEdgeColor';
'MarkerFaceColor';
'MarkerSize';     
'Parent';         
'Tag';            
'Type';           
'UserData';       
'Visible';        
'XData';          
'XDataMode';      
'YData';          
'ZData'};
assert_isequal(P, REF);
%=============================================================================
f  = figure();
t = 0:0.1:10*pi;
r = linspace (0, 1, length(t));
z = linspace (0, 1, length(t));
h = plot3 (r .* cos (t), r .* sin (t), z);
ylabel ('r .* sin (t)');
xlabel ('r .* cos (t)');
zlabel ('z');
title (_('plot3 display of 3-D helix'));
axis square
%=============================================================================
