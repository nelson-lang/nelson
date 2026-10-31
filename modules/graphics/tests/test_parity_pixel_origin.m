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
% Parity with the reference: the first pixel of an axes parent is at 1 (pixel units start at 1,
% the other units at 0), so the full parent is [1 1 W H] in pixels. A Position set by the user
% grows OuterPosition by the larger of the loose and the tight inset.
%=============================================================================
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 560 420]);
ax = axes(f);
ax.Units = 'pixels';
assert_istrue(close_to(ax.Position, [73.8 47.2 434 342.3]));
assert_istrue(close_to(ax.OuterPosition, [1 1 560 420]));
assert_istrue(close_to(ax.InnerPosition, [73.8 47.2 434 342.3]));
ax.Units = 'normalized';
assert_istrue(close_to(ax.Position, [0.13 0.11 0.775 0.815]));
assert_istrue(close_to(ax.OuterPosition, [0 0 1 1]));
%=============================================================================
% a pixel position set by the user reads back unchanged and converts with the origin at 1
ax.Units = 'pixels';
ax.Position = [50 60 300 200];
assert_istrue(close_to(ax.Position, [50 60 300 200]));
assert_isequal(ax.PositionConstraint, 'innerposition');
assert_istrue(close_to(ax.OuterPosition, [-22.8 13.8 426 277.7]));
ax.Units = 'normalized';
assert_istrue(close_to(ax.Position, [49 59 300 200] ./ [560 420 560 420]));
ax.Units = 'points';
assert_istrue(close_to(ax.Position, [36.75 44.25 225 150]));
ax.Units = 'inches';
assert_istrue(close_to(ax.Position, [49 59 300 200] / 96));
ax.Units = 'centimeters';
assert_istrue(close_to(ax.Position, [49 59 300 200] / 96 * 2.54));
ax.Units = 'pixels';
assert_istrue(close_to(ax.Position, [50 60 300 200]));
%=============================================================================
ax2 = axes(f, 'Units', 'pixels', 'Position', [1 1 100 100]);
ax2.Units = 'normalized';
assert_istrue(close_to(ax2.Position, [0 0 100 100] ./ [560 420 560 420]));
ax3 = axes(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
ax3.Units = 'pixels';
assert_istrue(close_to(ax3.Position, [1 1 560 420]));
%=============================================================================
% the rendering follows the same origin: an axes covering the parent has its box inside it
ax4 = axes(f, 'Units', 'pixels', 'Position', [1 1 560 420]);
plot(ax4, 1:3);
drawnow();
assert_istrue(close_to(ax4.Position, [1 1 560 420]));
close(f);
%=============================================================================
function ok = close_to(actual, expected)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) < 1e-6);
end
%=============================================================================
