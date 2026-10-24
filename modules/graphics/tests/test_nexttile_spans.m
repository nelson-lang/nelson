%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile;
ax3 = nexttile([1 2]);
asserts.istrue(isgraphics(ax3, 'axes'));
close all;
%=============================================================================
t = tiledlayout(2, 3);
nexttile;
nexttile;
ax_ref = nexttile(1);
asserts.istrue(isgraphics(ax_ref, 'axes'));
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile([2 1]);
ax1b = nexttile(1);
asserts.isequal(ax1, ax1b);
ax3 = nexttile(3);
asserts.isfalse(ax1 == ax3);
asserts.isequal(tilenum(ax3), 3);
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile(1, [1 2]);
ax1b = nexttile(1, [1 2]);
asserts.isequal(ax1, ax1b);
ax2 = nexttile(2);
asserts.isfalse(ax1 == ax2);
asserts.isequal(tilenum(ax2), 2);
close all;
%=============================================================================
