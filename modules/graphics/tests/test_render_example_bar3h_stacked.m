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
f = figure('Visible', 'on', 'Position', [100 100 620 460], 'Color', [1 1 1]);
axes('Parent', f);
bar3h([1 -2; -3 4], 'stacked');
colormap(parula(64));
title('horizontal stacked 3d bars');
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
surfaceObjects = findobj(f, 'Type', 'surface');
asserts.isequal(numel(surfaceObjects), 2);
asserts.istrue(all(isgraphics(surfaceObjects, 'surface')));
for k = 1:numel(surfaceObjects)
  asserts.isequal(surfaceObjects(k).Type, 'surface');
end
close(f);
close all;
%=============================================================================
