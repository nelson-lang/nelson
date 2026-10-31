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
% Parity with the reference: TightInset is the space the tick labels, the axis labels and the
% titles take around the axes box. Reference values (pixels, 10 pt font, 560x420 figure) were read
% in the reference; the tolerance covers the font metric differences between the two engines.
%=============================================================================
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 560 420]);
ax = axes(f);
ax.Units = 'pixels';
assert_istrue(close_to(ax.TightInset, [24.53 20.53 4 7.6], 3));
%=============================================================================
plot(ax, 1:10);
drawnow();
assert_istrue(close_to(ax.TightInset, [20.53 20.53 7.6 7.6], 3));
%=============================================================================
% the value follows Units
ax.Units = 'normalized';
assert_istrue(close_to(ax.TightInset .* [560 420 560 420], [20.53 20.53 7.6 7.6], 3));
ax.Units = 'pixels';
%=============================================================================
xlabel(ax, 'X');
ylabel(ax, 'Y');
title(ax, 'T');
drawnow();
assert_istrue(close_to(ax.TightInset, [41.33 38.67 7.6 20.47], 3.5));
%=============================================================================
% two-line label grows the bottom, empty ticks give the minimum
xlabel(ax, {'X', 'XX'});
tight = ax.TightInset;
assert_istrue(close_to(tight(2), 55.47, 4));
xlabel(ax, 'X');
ax.XTick = [];
drawnow();
tight = ax.TightInset;
assert_istrue(tight(2) < 38.67 - 10);
close(f);
%=============================================================================
% wide tick labels grow the left inset
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 560 420]);
ax = axes(f, 'Units', 'pixels');
plot(ax, 1:10);
ax.YTick = 5;
ax.YTickLabel = {'aaaaaaaaaa'};
drawnow();
tight = ax.TightInset;
assert_istrue(tight(1) > 60);
close(f);
%=============================================================================
function ok = close_to(actual, expected, tolerance)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) <= tolerance);
end
%=============================================================================
