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
[x, y, u, v] = graphics_streamline_fixture();
f = figure('Visible', 'off');
h = streamline(x, y, u, v, 0, 0);
ax = gca();
asserts.isequal(h(1).XData, 0);
asserts.isequal(h(1).YData, 0);
asserts.istrue(isempty(h(1).ZData));
asserts.isapprox(ax.XLim, [-1 1], 1e-12);
asserts.isapprox(ax.YLim, [-1 1], 1e-12);
asserts.isapprox(ax.XTick, -1:0.2:1, 1e-12);
asserts.isapprox(ax.YTick, -1:0.2:1, 1e-12);
asserts.isapprox(ax.ZTick, [-1 0 1], 1e-12);
close(f);
%=============================================================================
