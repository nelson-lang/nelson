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
t = tiledlayout(2, 3);
% tilenum(t, row, col)
n = tilenum(t, 1, 1);
assert_isequal(n, 1);
n2 = tilenum(t, 2, 3);
assert_isequal(n2, 6);
% out-of-range returns NaN
n3 = tilenum(t, 3, 1);
assert_istrue(isnan(n3));
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile;
ax2 = nexttile;
n1 = tilenum(ax1);
n2 = tilenum(ax2);
assert_isequal(n1, 1);
assert_isequal(n2, 2);
close all;
%=============================================================================
