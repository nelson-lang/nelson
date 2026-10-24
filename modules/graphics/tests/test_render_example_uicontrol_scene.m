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

close all;
%=============================================================================
f = figure('Visible', 'on', 'Position', [100 100 500 360], 'Color', [1 1 1]);
axes('Parent', f, 'Position', [0.12 0.28 0.82 0.65]);
plot(1:5, [1 4 2 5 3], 'LineWidth', 1.5);
uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'Apply', 'Position', [30 20 90 30]);
uicontrol('Parent', f, 'Style', 'text', 'String', 'status ready', 'Position', [140 22 130 24]);
drawnow();
%=============================================================================
asserts.istrue(isgraphics(f, 'figure'));
asserts.isequal(f.Type, 'figure');
asserts.isequal(char(f.Visible), 'on');
asserts.isequal(f.Color, [1 1 1]);
axesHandles = findobj(f, 'Type', 'axes');
asserts.isequal(numel(axesHandles), 1);
for k = 1:numel(axesHandles)
  asserts.isequal(axesHandles(k).Type, 'axes');
  asserts.isequal(axesHandles(k).Parent, f);
end
lineObjects = findobj(f, 'Type', 'line');
asserts.isequal(numel(lineObjects), 1);
asserts.istrue(all(isgraphics(lineObjects, 'line')));
for k = 1:numel(lineObjects)
  asserts.isequal(lineObjects(k).Type, 'line');
end
uicontrolObjects = findobj(f, 'Type', 'uicontrol');
asserts.isequal(numel(uicontrolObjects), 2);
asserts.istrue(all(isgraphics(uicontrolObjects, 'uicontrol')));
for k = 1:numel(uicontrolObjects)
  asserts.isequal(uicontrolObjects(k).Type, 'uicontrol');
end
close(f);
close all;
%=============================================================================
