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
f = figure('Visible', 'on', 'Position', [100 100 560 420], 'Color', [1 1 1]);
axes('Parent', f);
plot(1:6, [1 3 2 5 4 6], 'r-', 'LineWidth', 1.5);
hold('on');
plot(1:6, [6 4 5 2 3 1], 'b--', 'LineWidth', 1.5);
legend({'up', 'down'}, 'Location', 'northwest');
title('legend scene');
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
asserts.isequal(numel(lineObjects), 2);
asserts.istrue(all(isgraphics(lineObjects, 'line')));
for k = 1:numel(lineObjects)
  asserts.isequal(lineObjects(k).Type, 'line');
end
legendObjects = findobj(f, 'Type', 'legend');
asserts.isequal(numel(legendObjects), 1);
asserts.istrue(all(isgraphics(legendObjects, 'legend')));
for k = 1:numel(legendObjects)
  asserts.isequal(legendObjects(k).Type, 'legend');
end
lgd = findobj(f, 'Type', 'legend'); asserts.istrue(isgraphics(lgd, 'legend'));
close(f);
close all;
%=============================================================================
