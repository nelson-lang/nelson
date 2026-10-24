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
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fsurf(ax, @(x, y) x + y);
asserts.isequal(h.XRange, [-5 5]);
asserts.isequal(h.YRange, [-5 5]);
asserts.isequal(ax.XLim, [-5 5]);
asserts.isequal(ax.YLim, [-5 5]);
asserts.isequal(h.XRangeMode, 'auto');
asserts.isequal(h.YRangeMode, 'auto');
%=============================================================================
