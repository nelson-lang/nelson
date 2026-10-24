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
f = figure('Visible', 'on', 'Position', [100 100 760 300], 'Color', [1 1 1]);
[x, y, z] = peaks(16);
subplot(1, 3, 1); surf(x, y, z); shading('faceted'); view(35, 28);
subplot(1, 3, 2); surf(x, y, z); shading('flat'); view(35, 28);
subplot(1, 3, 3); surf(x, y, z); shading('interp'); view(35, 28);
drawnow();
%=============================================================================
asserts.istrue(isgraphics(f, 'figure'));
asserts.isequal(f.Type, 'figure');
asserts.isequal(char(f.Visible), 'on');
asserts.isequal(f.Color, [1 1 1]);
axesHandles = findobj(f, 'Type', 'axes');
asserts.isequal(numel(axesHandles), 3);
for k = 1:numel(axesHandles)
  asserts.isequal(axesHandles(k).Type, 'axes');
  asserts.isequal(axesHandles(k).Parent, f);
end
close(f);
close all;
%=============================================================================
