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
% Data-unit text (including the axes title) must follow the axis scale and
% render on log-scaled axes. Regression test for titles/text disappearing on
% semilogx / semilogy / loglog, and for the title centering on a log X axis.
%=============================================================================
% Data-unit text renders on a log X axis (was dropped off the projection).
f = figure('Visible', 'off');
c1 = onCleanup(@() close(f));
semilogx([1 10 100], [1 2 3]);
text(10, 2, 'MARKER', 'Color', [1 0 0], 'FontSize', 20);
title('Log Title');
drawnow();
frame = getframe(f);
cdata = frame.cdata;
redMask = (cdata(:, :, 1) > 150) & (cdata(:, :, 2) < 100) & (cdata(:, :, 3) < 100);
asserts.istrue(sum(redMask(:)) > 50);
%=============================================================================
% Title is centered on the geometric mean of the limits on a log X axis.
t = get(gca(), 'Title');
xl = get(gca(), 'XLim');
p = get(t, 'Position');
asserts.istrue(abs(p(1) - sqrt(xl(1) * xl(2))) <= 1e-6 * sqrt(xl(1) * xl(2)));
%=============================================================================
% A linear axis keeps the arithmetic center (unchanged behavior).
g = figure('Visible', 'off');
c2 = onCleanup(@() close(g));
plot([0 100], [0 1]);
tt = title('Linear Title');
xll = get(gca(), 'XLim');
pp = get(tt, 'Position');
asserts.istrue(abs(pp(1) - (xll(1) + xll(2)) / 2) <= 1e-3 * (xll(2) - xll(1)) + 1e-6);
%=============================================================================
