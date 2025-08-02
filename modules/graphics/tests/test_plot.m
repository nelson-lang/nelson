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
x = 0:pi/100:2*pi;
y = sin(x);
L = plot(x, y);
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
assert_istrue(isempty(L.Children));
assert_isapprox(L.Color, [0 0.4470 0.7410], 1e-4);
assert_isequal(L.DisplayName, '');
assert_isequal(L.LineStyle, '-');
assert_isapprox(L.LineWidth, 0.5000, 1e-4);
assert_isequal(L.Marker, 'none')
assert_isequal(L.MarkerEdgeColor, [0 0.447000 0.741000]);
assert_isequal(L.MarkerFaceColor, 'none');
assert_isequal(L.MarkerSize, 6);
assert_isequal(L.Parent, gca());
assert_isequal(L.Tag, '');
assert_isequal(L.Type, 'line');
assert_isequal(L.UserData, []);
assert_isequal(L.Visible, 'on');
assert_isapprox(L.XData, x, 1e-4);
assert_isequal(L.XDataMode,'manual');
assert_isapprox(L.YData, y, 1e-4);
assert_isequal(L.ZData, zeros(1, 201));
%=============================================================================
x = 0:pi/100:2*pi;
y1 = sin(x);
y2 = sin(x-0.25);
y3 = sin(x-0.5);
%=============================================================================
f = figure();
L = plot(x,y1,x,y2,'--',x,y3,':');
assert_isequal(size(L), [3 1]);
assert_isequal(L(1).LineStyle, '-');
assert_isequal(L(2).LineStyle, '--');
assert_isequal(L(3).LineStyle, ':');
%=============================================================================
f = figure();
L = plot(x,y1,'g',x,y2,'b--o',x,y3,'c*');
assert_isequal(size(L), [3 1]);
%=============================================================================
f = figure();
x = linspace(0,10);
y = sin(x);
L = plot(x, y, '-o');
assert_isequal(size(L), [1 1]);
assert_isequal(L(1).LineStyle, '-');
%=============================================================================
f = figure();
x = -pi:pi/10:pi;
y = tan(sin(x)) - sin(tan(x));
L = plot(x, y, '--gs', 'LineWidth', 2,  'MarkerSize', 10, 'MarkerEdgeColor','b',  'MarkerFaceColor',[0.5,0.5,0.5]);
%=============================================================================
f = figure();
x = linspace(-2*pi,2*pi);
y1 = sin(x);
y2 = cos(x);
L = plot(x, y1, x, y2);
L(1).LineWidth = 2;
L(2).Marker = '*';
%=============================================================================
f = figure();
r = 2;
xc = 4;
yc = 3;
theta = linspace(0, 2*pi);
x = r * cos(theta) + xc;
y = r * sin(theta) + yc;
plot(x, y);
axis equal
%=============================================================================
