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
f = figure('Visible', 'on', 'Position', [100 100 1075 760], 'Color', [1 1 1]);
x = -pi:pi / 10:pi;
y = tan(sin(x)) - sin(tan(x));
plot(x, y, '--rs', 'LineWidth', 2, 'MarkerEdgeColor', 'k', 'MarkerFaceColor', 'g', 'MarkerSize', 11);
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
h = findobj(f, 'Type', 'line'); asserts.isequal(h.Marker, 'square');
close(f);
close all;
%=============================================================================
