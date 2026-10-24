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
f = figure('Visible', 'on', 'Position', [100 100 640 420], 'Color', [1 1 1]);
subplot(1, 2, 1);
plot(1:5, (1:5) .^ 2, 'k-o');
title('left');
subplot(1, 2, 2);
bar([2 6 4 8]);
title('right');
drawnow();
%=============================================================================
asserts.istrue(isgraphics(f, 'figure'));
asserts.isequal(f.Type, 'figure');
asserts.isequal(char(f.Visible), 'on');
asserts.isequal(f.Color, [1 1 1]);
axesHandles = findobj(f, 'Type', 'axes');
asserts.isequal(numel(axesHandles), 2);
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
barObjects = findobj(f, 'Type', 'bar');
asserts.isequal(numel(barObjects), 1);
asserts.istrue(all(isgraphics(barObjects, 'bar')));
for k = 1:numel(barObjects)
  asserts.isequal(barObjects(k).Type, 'bar');
end
close(f);
close all;
%=============================================================================
