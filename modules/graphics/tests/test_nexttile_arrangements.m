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
t = tiledlayout('vertical');
ax1 = nexttile;
ax2 = nexttile;
ax3 = nexttile;
asserts.isequal(t.GridSize, [3, 1]);
asserts.isequal(tilenum(ax1), 1);
asserts.isequal(tilenum(ax3), 3);
close all;
%=============================================================================
t = tiledlayout('horizontal');
ax1 = nexttile;
ax2 = nexttile;
ax3 = nexttile;
asserts.isequal(t.GridSize, [1, 3]);
asserts.isequal(tilenum(ax2), 2);
asserts.isequal(tilenum(ax3), 3);
close all;
%=============================================================================
t = tiledlayout(2, 2);
east = nexttile('east');
east2 = nexttile('EAST');
asserts.isequal(east, east2);
asserts.istrue(isnan(tilenum(east)));
close all;
%=============================================================================
