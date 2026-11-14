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
% axes(t) places a cartesian axes in a tiled layout
f = figure('Visible', 'off');
t = tiledlayout(2, 2);
ax = axes(t);
assert_isequal(ax.Type, 'axes');
assert_isequal(ax.Parent.Type, 'tiledlayout');
assert_isequal(numel(t.Children), 1);
assert_isequal(ax.Layout.Tile, 1);
close(f);
%=============================================================================
% polaraxes(t) places a polaraxes in a tiled layout
f = figure('Visible', 'off');
t = tiledlayout(2, 2);
pax = polaraxes(t);
assert_isequal(pax.Type, 'polaraxes');
assert_isequal(pax.Parent.Type, 'tiledlayout');
assert_isequal(numel(t.Children), 1);
assert_isequal(pax.Layout.Tile, 1);
% a second axes stacks on tile 1 (axes(t)/polaraxes(t) never advance the cursor)
ax = axes(t);
assert_isequal(numel(t.Children), 2);
assert_isequal(ax.Layout.Tile, 1);
close(f);
%=============================================================================
