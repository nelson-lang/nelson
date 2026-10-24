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
x = 1:8;
y = [3 7 4 6 2 8 5 9];
scatter(x, y, 80, 'filled', 'MarkerFaceColor', [0.1 0.6 0.8]);
title('scatter scene');
grid('on');
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
scatterObjects = findobj(f, 'Type', 'scatter');
asserts.isequal(numel(scatterObjects), 1);
asserts.istrue(all(isgraphics(scatterObjects, 'scatter')));
for k = 1:numel(scatterObjects)
  asserts.isequal(scatterObjects(k).Type, 'scatter');
end
close(f);
close all;
%=============================================================================
