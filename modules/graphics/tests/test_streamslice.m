%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
[x, y] = meshgrid(-2:2, -2:2);
u = -y;
v = x;
f = figure('Visible', 'off');
h = streamslice(x, y, u, v);
ax = gca();
asserts.istrue(isgraphics(h(1), 'line'));
asserts.istrue(numel(h) >= 40);
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [-2 2.5]);
asserts.isequal(char(ax.XGrid), 'off');
asserts.isequal(char(ax.YGrid), 'off');
close(f);
%=============================================================================
[verts, averts] = streamslice(x, y, u, v);
asserts.istrue(iscell(verts));
asserts.istrue(iscell(averts));
% The automatic seeding must fill the slice densely, not a sparse handful.
asserts.istrue(numel(verts) >= 50);
asserts.istrue(numel(averts) >= 10);
asserts.istrue(all(cellfun(@(pts) size(pts, 1) == 3, averts)));
%=============================================================================
% A higher density draws at least as many streamlines as the default.
[vertsDefault] = streamslice(x, y, u, v);
[vertsDense] = streamslice(x, y, u, v, 2);
asserts.istrue(numel(vertsDense) >= numel(vertsDefault));
%=============================================================================
[verts, averts] = streamslice(x, y, u, v, 'noarrows');
asserts.istrue(numel(verts) >= 20);
asserts.istrue(isempty(averts));
%=============================================================================
[x3, y3, z3] = meshgrid(-1:1, -1:1, -1:1);
u3 = -y3;
v3 = x3;
w3 = ones(size(x3));
f = figure('Visible', 'off');
ax = axes();
h = streamslice(ax, x3, y3, z3, u3, v3, w3, 0, 0, 0);
asserts.istrue(isgraphics(h(1), 'line'));
asserts.isequal(h(1).Parent, ax);
close(f);
%=============================================================================
asserts.checkerror('streamslice(1, 2, 3, 4, 5);', getString(message('Nelson:dimagree:vectorComponentSizesMatch')));
%=============================================================================
