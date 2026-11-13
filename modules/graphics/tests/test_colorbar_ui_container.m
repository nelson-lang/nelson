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
% A colorbar of an axes held by a uitab or a uipanel belongs to that container
% and sits next to the axes, in the container units.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 700 500]);
tg = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
tb = uitab(tg, 'Title', 'T1');
ax = axes('Parent', tb);
imagesc(ax, magic(5));
cb = colorbar(ax);
assert_istrue(isequal(cb.Parent, tb));
assert_isequal(cb.Location, 'eastoutside');
assert_istrue(any(tb.Children == cb));
assert_isfalse(any(f.Children == cb));
cbPos = cb.Position;
axPos = ax.Position;
assert_istrue(cbPos(1) > axPos(1) + axPos(3));
assert_istrue(cbPos(1) + cbPos(3) < 1);
assert_isapprox(cbPos(2), axPos(2), 1e-2);
assert_isapprox(cbPos(4), axPos(4), 1e-2);
% colorbar(ax) again reuses the same object.
cb2 = colorbar(ax);
assert_istrue(isequal(cb2, cb));
assert_isequal(numel(findall(tb, 'Type', 'colorbar')), 1);
% The frame shows the colorbar: its center pixel is a colormap color, not the gray tab background.
img = getframe(f);
[h, w, ~] = size(img.cdata);
x = round((cbPos(1) + cbPos(3) / 2) * w);
y = round((1 - (cbPos(2) + cbPos(4) / 2)) * h);
rgb = double(squeeze(img.cdata(y, x, :)));
assert_istrue(max(rgb) - min(rgb) > 60);
% 'off' removes it from the tab.
colorbar(ax, 'off');
assert_istrue(isempty(findall(tb, 'Type', 'colorbar')));
cb = colorbar(ax);
% Deleting the axes deletes its colorbar.
delete(ax);
assert_isfalse(isgraphics(cb));
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 700 500]);
p = uipanel(f, 'Units', 'pixels', 'Position', [50 50 400 300]);
ax = axes('Parent', p);
cb = colorbar(ax);
assert_istrue(isequal(cb.Parent, p));
cbPos = cb.Position;
axPos = ax.Position;
assert_istrue(cbPos(1) > axPos(1) + axPos(3));
assert_istrue(cbPos(1) + cbPos(3) < 1);
assert_isapprox(cbPos(2), axPos(2), 1e-2);
% The strip width follows the panel width (400 px), not the figure width.
assert_isapprox(cbPos(3), 0.0537, 2e-3);
close(f);
%=============================================================================
