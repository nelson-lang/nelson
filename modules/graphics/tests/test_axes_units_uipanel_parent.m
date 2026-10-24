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
% the Units of an axes held by a uipanel are relative to the panel content
% area: the panel size minus its border and its title band
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
p = uipanel(f, 'Units', 'pixels', 'Position', [20 20 200 100], 'BorderType', 'none');
ax = axes(p, 'Position', [0.25 0.5 0.5 0.25]);
ax.Units = 'pixels';
asserts.isapprox(ax.Position, [50 50 100 25], 1e-12);
ax.Units = 'normalized';
asserts.isapprox(ax.Position, [0.25 0.5 0.5 0.25], 1e-12);
%=============================================================================
ax2 = axes(p, 'Units', 'pixels', 'Position', [20 10 100 50]);
ax2.Units = 'normalized';
asserts.isapprox(ax2.Position, [0.1 0.1 0.5 0.5], 1e-12);
%=============================================================================
% panel sized in normalized units: 0.5 x 0.5 of the 400x300 figure
p2 = uipanel(f, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], 'BorderType', 'none');
ax3 = axes(p2, 'Position', [0 0 1 1]);
ax3.Units = 'pixels';
asserts.isapprox(ax3.Position, [0 0 200 150], 1e-12);
%=============================================================================
% the border is deducted on each side (default: 'line', 1 pixel)
p3 = uipanel(f, 'Units', 'pixels', 'Position', [20 20 200 100]);
ax5 = axes(p3, 'Position', [0 0 1 1]);
ax5.Units = 'pixels';
asserts.isapprox(ax5.Position, [0 0 198 98], 1e-12);
p3.BorderWidth = 5;
ax6 = axes(p3, 'Position', [0 0 1 1]);
ax6.Units = 'pixels';
asserts.isapprox(ax6.Position, [0 0 190 90], 1e-12);
%=============================================================================
% a title adds a band above the content: the height only shrinks
p4 = uipanel(f, 'Units', 'pixels', 'Position', [20 20 200 100], ...
  'BorderType', 'none', 'Title', 'Panel');
ax7 = axes(p4, 'Position', [0 0 1 1]);
ax7.Units = 'pixels';
asserts.isapprox(ax7.Position(1:3), [0 0 200], 1e-12);
asserts.istrue(ax7.Position(4) < 100 && ax7.Position(4) > 80);
ax7.Units = 'normalized';
asserts.isapprox(ax7.Position, [0 0 1 1], 1e-12);
%=============================================================================
% axes held by the figure keep the figure as reference
ax4 = axes(f, 'Position', [0.25 0.5 0.5 0.25]);
ax4.Units = 'pixels';
asserts.isapprox(ax4.Position, [100 150 200 75], 1e-12);
close(f);
%=============================================================================
