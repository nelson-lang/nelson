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
close all
for i=1:10
  figure(i);
end
%=============================================================================
current = gcf();
assert_isequal(current.Number, 10)
%=============================================================================
f1 = figure(1);
current = gcf();
assert_isequal(f1, current);
assert_isequal(current.Number, 1)
%=============================================================================
f11 = figure();
current = gcf();
assert_isequal(f11, current);
assert_isequal(current.Number, 11)
%=============================================================================
f33 = figure(33);
current = gcf();
assert_isequal(f33, current);
assert_isequal(current.Number, 33)
%=============================================================================
f12 = figure();
current = gcf();
assert_isequal(f12, current);
assert_isequal(current.Number, 12)
%=============================================================================
f1 = figure(33);
assert_isequal(f1, gcf());
f35 = figure(35);
assert_isequal(f35, gcf());
%=============================================================================
f = figure();
p = properties(f);
REF = {'AlphaMap';         
'BeingDeleted';     
'BusyAction';       
'ButtonDownFcn';    
'Children';    
'CloseRequestFcn';  
'Color';  
'Colormap';
'CreateFcn';
'CurrentAxes';
'DeleteFcn';
'DevicePixelRatio';
'DrawLater';        
'GraphicsSmoothing';
'Interruptible';
'KeyPressFcn';    
'KeyReleaseFcn';    
'MenuBar';    
'Name';          
'NextPlot';         
'Number';         
'NumberTitle';      
'Parent';      
'Position';
'Resize';         
'SizeChangedFcn';   
'Tag';   
'ToolBar';          
'Type';          
'UserData';         
'Visible';
'WindowState'};
assert_isequal(p, REF);
%=============================================================================
f = figure('Resize', 'off');
assert_isequal(f.Resize, 'off');
f.Resize = 'on';
assert_isequal(f.Resize, 'on');
%=============================================================================
f.WindowState = 'normal';
assert_isequal(f.WindowState, 'normal');
f.WindowState = 'maximized';
assert_isequal(f.WindowState, 'maximized');
f.WindowState = 'minimized';
assert_isequal(f.WindowState, 'minimized');
f.WindowState = 'normal';
assert_isequal(f.WindowState, 'normal');
%=============================================================================
close all
%=============================================================================
