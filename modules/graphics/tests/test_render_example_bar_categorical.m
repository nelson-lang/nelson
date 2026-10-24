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
X = categorical({'Small', 'Medium', 'Large', 'Extra Large'});
X = reordercats(X, {'Small', 'Medium', 'Large', 'Extra Large'});
Y = [10 21 33 52];
bar(X, Y);
title('categorical bars');
xlabel('size');
ylabel('value');
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
barObjects = findobj(f, 'Type', 'bar');
asserts.isequal(numel(barObjects), 1);
asserts.istrue(all(isgraphics(barObjects, 'bar')));
for k = 1:numel(barObjects)
  asserts.isequal(barObjects(k).Type, 'bar');
end
close(f);
close all;
%=============================================================================
