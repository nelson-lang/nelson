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
f = figure('Visible', 'on', 'Position', [100 100 520 420], 'Color', [1 1 1]);
axes('Parent', f);
imagesc(magic(12));
axis('image');
colormap(parula(64));
colorbar();
title('image scene');
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
imageObjects = findobj(f, 'Type', 'image');
asserts.isequal(numel(imageObjects), 1);
asserts.istrue(all(isgraphics(imageObjects, 'image')));
for k = 1:numel(imageObjects)
  asserts.isequal(imageObjects(k).Type, 'image');
end
close(f);
close all;
%=============================================================================
