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
% tilerowcol(t, tnum)
[r, c] = tilerowcol(t, 1);
assert_isequal(r, 1);
assert_isequal(c, 1);
[r2, c2] = tilerowcol(t, 4);
assert_isequal(r2, 2);
assert_isequal(c2, 1);
% out-of-range returns NaN
[r3, c3] = tilerowcol(t, 10);
assert_istrue(isnan(r3));
assert_istrue(isnan(c3));
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile;
ax2 = nexttile;
[r1, c1] = tilerowcol(ax1);
assert_isequal(r1, 1);
assert_isequal(c1, 1);
[r2, c2] = tilerowcol(ax2);
assert_isequal(r2, 1);
assert_isequal(c2, 2);
close all;
%=============================================================================
