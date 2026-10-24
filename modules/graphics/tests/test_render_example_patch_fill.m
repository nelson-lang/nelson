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
fill([0 1 0.5], [0 0 1], [0.2 0.7 0.3], 'LineWidth', 2);
axis([-0.2 1.2 -0.2 1.2]);
title('patch scene');
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
patchObjects = findobj(f, 'Type', 'patch');
asserts.isequal(numel(patchObjects), 1);
asserts.istrue(all(isgraphics(patchObjects, 'patch')));
for k = 1:numel(patchObjects)
  asserts.isequal(patchObjects(k).Type, 'patch');
end
close(f);
close all;
%=============================================================================
