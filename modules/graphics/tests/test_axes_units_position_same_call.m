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
% properties apply in argument order: a Position given after Units is in
% those units, a Position given before Units is converted
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
ax = axes(f, 'Units', 'pixels', 'Position', [50 50 300 200]);
asserts.isequal(ax.Units, 'pixels');
asserts.isequal(ax.Position, [50 50 300 200]);
asserts.isequal(ax.InnerPosition, [50 50 300 200]);
%=============================================================================
ax2 = axes(f, 'Position', [50 50 300 200], 'Units', 'pixels');
asserts.isequal(ax2.Units, 'pixels');
asserts.isapprox(ax2.Position, [20001 15001 120000 60000], 1e-9);
%=============================================================================
p = uipanel(f, 'Units', 'pixels', 'Position', [20 20 360 260]);
ax3 = axes('Parent', p, 'Units', 'pixels', 'Position', [30 30 300 200]);
asserts.isequal(ax3.Position, [30 30 300 200]);
%=============================================================================
ax4 = axes(f);
set(ax4, 'Units', 'pixels', 'Position', [10 20 100 80]);
asserts.isequal(ax4.Position, [10 20 100 80]);
%=============================================================================
% Units alone still converts the existing Position (pixels start at 1)
ax5 = axes(f, 'Position', [0.25 0.5 0.5 0.25]);
ax5.Units = 'pixels';
asserts.isequal(ax5.Position, [101 151 200 75]);
close(f);
%=============================================================================
