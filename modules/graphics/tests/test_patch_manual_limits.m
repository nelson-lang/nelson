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
% Manual limits stay as set when patches are added (datenum-size values).
t0 = 738451.0384;
w = 60 / 86400;
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, t0 + (0:100) * w / 100, 1:101);
set(ax, 'XLim', t0 + [0 w]);
xl = get(ax, 'XLim');
p = patch(ax, t0 + w / 3 + [0 0 0 0], [2 2 5 5], 'r');
asserts.isequal(get(ax, 'XLim'), xl);
asserts.isequal(get(ax, 'XLimMode'), 'manual');
patch(ax, t0 + w / 3 + [0 0.1 0.1 0], [2 2 5 5], 'r');
asserts.isequal(get(ax, 'XLim'), xl);
%=============================================================================
% Manual YLim is kept too, and the auto YLim is not frozen.
asserts.isequal(get(ax, 'YLimMode'), 'auto');
set(ax, 'YLim', [0 10]);
patch(ax, t0 + [1 2 2 1], [50 50 60 60], 'g');
asserts.isequal(get(ax, 'YLim'), [0 10]);
asserts.isequal(get(ax, 'XLim'), xl);
%=============================================================================
% Changing patch data later does not move manual limits.
set(p, 'XData', t0 + [5 6 6 5]');
asserts.isequal(get(ax, 'XLim'), xl);
asserts.isequal(get(ax, 'YLim'), [0 10]);
delete(f);
%=============================================================================
% Plain values: zero-width patch inside and outside a manual range.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:10);
set(ax, 'XLim', [1000 1001]);
patch(ax, [1000.5 1000.5 1000.5 1000.5], [2 2 5 5], 'r');
asserts.isequal(get(ax, 'XLim'), [1000 1001]);
patch(ax, [1 2 2 1], [1 1 2 2], 'r');
asserts.isequal(get(ax, 'XLim'), [1000 1001]);
%=============================================================================
% Manual ZLim is kept for a 3-D patch.
set(ax, 'ZLim', [-1 1]);
patch(ax, [1 2 2], [1 1 2], [5 6 7], 'r');
asserts.isequal(get(ax, 'ZLim'), [-1 1]);
%=============================================================================
% Other primitives keep manual limits too.
line(ax, [0 5000], [0 1]);
surface(ax, [0 3000; 0 3000], [0 0; 1 1], zeros(2));
text(ax, 9000, 1, 'x');
asserts.isequal(get(ax, 'XLim'), [1000 1001]);
asserts.isequal(get(ax, 'ZLim'), [-1 1]);
delete(f);
%=============================================================================
% Patch property form (Faces/Vertices) also keeps manual limits.
f = figure('Visible', 'off');
ax = axes(f);
set(ax, 'XLim', [0 1], 'YLim', [0 1]);
patch(ax, 'Faces', [1 2 3], 'Vertices', [5 5 0; 6 5 0; 6 6 0], 'FaceColor', 'r');
asserts.isequal(get(ax, 'XLim'), [0 1]);
asserts.isequal(get(ax, 'YLim'), [0 1]);
delete(f);
%=============================================================================
% Auto limits still include a new zero-width patch.
f = figure('Visible', 'off');
ax = axes(f);
patch(ax, 1000.5 + [0 0 0 0], [2 2 5 5], 'r');
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isapprox(get(ax, 'XLim'), [999.5 1001.5], 1e-12);
asserts.isapprox(get(ax, 'YLim'), [2 5], 1e-12);
delete(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:10);
patch(ax, [20 20 20 20], [2 2 5 5], 'r');
asserts.isapprox(get(ax, 'XLim'), [0 20], 1e-12);
asserts.isapprox(get(ax, 'YLim'), [1 10], 1e-12);
delete(f);
%=============================================================================
% Large abscissas with a tiny range keep the limits and ticks of the same range at zero.
f = figure('Visible', 'off', 'Position', [50 50 560 420]);
ax = axes(f);
plot(ax, t0 + (0:100) * w / 100, 1:101);
patch(ax, t0 + w / 3 + [0 0 0 0], [2 2 5 5], 'r');
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.istrue(max(abs(get(ax, 'XLim') - t0 - [0 0.0007])) < 1e-8);
asserts.isequal(numel(get(ax, 'XTick')), 8);
asserts.istrue(max(abs(get(ax, 'XTick') - t0 - (0:7) * 1e-4)) < 1e-8);
delete(f);
%=============================================================================
% A narrow ruler takes the coarser step.
f = figure('Visible', 'off', 'Position', [50 50 400 420]);
ax = axes(f);
plot(ax, t0 + (0:100) * w / 100, 1:101);
asserts.istrue(max(abs(get(ax, 'XLim') - t0 - [0 0.0008])) < 1e-8);
asserts.isequal(numel(get(ax, 'XTick')), 5);
asserts.istrue(max(abs(get(ax, 'XTick') - t0 - (0:4) * 2e-4)) < 1e-8);
delete(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [50 50 560 420]);
ax = axes(f);
plot(ax, (0:100) * 0.694 / 100, 1:101);
asserts.isapprox(get(ax, 'XLim'), [0 0.7], 1e-12);
delete(f);
%=============================================================================
