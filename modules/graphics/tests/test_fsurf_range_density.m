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
h = fsurf(@(x, y) x .* y, [-1 1], 'MeshDensity', 9);
ax = gca();
asserts.isequal(h.XRange, [-1 1]);
asserts.isequal(h.YRange, [-1 1]);
asserts.isequal(ax.XLim, [-1 1]);
asserts.isequal(ax.YLim, [-1 1]);
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.YRangeMode, 'manual');
asserts.isequal(size(h.ZData), [9 9]);
%=============================================================================
