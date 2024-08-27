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
f = figure();
x = linspace(-5,5);
y = x.^3-12*x;
plot(x,y);
xt = [-2 2];
yt = [16 -16];
str = 'dy/dx = 0';
T = text(xt, yt, str);
T(1).Color = 'red';
T(1).FontSize = 14;
%=============================================================================
assert_isequal(size(T), [1 2]);
%=============================================================================
assert_isequal(T(1).Color, [1 0 0]);
%=============================================================================
assert_isequal(T(1).FontSize, 14);
%=============================================================================
P = properties(T(1));
REF = {'BackgroundColor';  
'BeingDeleted';
'BoundingBox';        
'BusyAction';  
'Children';    
'Color';
'CreateFcn';
'DeleteFcn';        
'EdgeColor';          
'Extent';             
'FontAngle';          
'FontName';           
'FontSize';
'FontSmoothing';           
'FontUnits';          
'FontWeight';         
'HorizontalAlignment';
'Interpreter';
'Interruptible';
'LineStyle';          
'LineWidth';          
'Margin';             
'Parent';             
'Position';           
'Rotation';           
'String';             
'Tag';                
'Type';               
'Units';              
'UserData';           
'VerticalAlignment';  
'Visible'};
assert_isequal(P, REF);
%=============================================================================
