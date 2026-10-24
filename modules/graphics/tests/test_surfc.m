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
f = figure('Visible', 'off');
Z = peaks(30);
h = surfc(Z);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.ZLim, [min(Z(:)) max(Z(:))], 1e-12);
asserts.isequal(ax.ZTick, -6:2:8);
asserts.isequal(ax.XTick, 5:5:30);
asserts.isequal(ax.YTick, 5:5:30);
close(f);
%=============================================================================