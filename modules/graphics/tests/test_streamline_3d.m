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
[x3, y3, z3] = meshgrid(-1:1, -1:1, -1:1);
u3 = -y3;
v3 = x3;
w3 = ones(size(x3));
f = figure('Visible', 'off');
ax = axes();
h = streamline(ax, x3, y3, z3, u3, v3, w3, 0, 0, 0, 'LineWidth', 2);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).LineWidth, 2);
asserts.isfalse(isempty(h(1).ZData));
asserts.isequal(ax.View, [0 90]);
close(f);
%=============================================================================
