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
x = linspace(0,10);
y = sin(x);
R = line(x, y);
assert_isequal(R.Type, 'line');
%=============================================================================
P = properties(R);
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
figure();
t = linspace(0,10*pi,200);
x = sin(t);
y = cos(t);
z = t;
R = line(x, y, z);
view(3)
%=============================================================================
figure();
x = linspace(0,10);
y = sin(x);
line('XData',x,'YData',y)
%=============================================================================
figure();
x = [1 9];
y = [2 12];
line(x,y,'Color','red','LineStyle','--')
%=============================================================================
figure();
x = [3 2];
y = [15 12];
pl = line(x, y);
pl.Color = 'green';
pl.LineStyle = '--';
%=============================================================================