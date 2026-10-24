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
% Pin the calibration geometry for 573x460 on a 1024x768 CI runner.
set(groot, 'DefaultFigurePosition', [423 180 1074 646]);
%=============================================================================
f = figure();
x = linspace(0, 2 * pi);
y = sin(x);
plot(x, y, '-o');
axis([0 2 * pi -1.5 1.5]);
lim = axis();
REF = [0 6.2832 -1.5000 1.5000];
asserts.isapprox(lim(1), REF(1), 1e-4);
asserts.isapprox(lim(2), REF(2), 1e-4);
asserts.isapprox(lim(3), REF(3), 1e-4);
asserts.isapprox(lim(4), REF(4), 1e-4);
%=============================================================================
f = figure();
x = linspace(-10, 10, 200);
y = sin(4 * x) ./ exp(0.1 * x);
plot(x, y);
axis([-10 10 0 inf]);
lim = axis();
REF = [-10 10 0 inf];
asserts.isapprox(lim(1), REF(1), 1e-4);
asserts.isapprox(lim(2), REF(2), 1e-4);
asserts.isapprox(lim(3), REF(3), 1e-4);
asserts.istrue(isinf(lim(4)));
%=============================================================================
f = figure('Visible', 'off');
t = 0:0.01:2 * pi;
x = cos(t);
ax1 = subplot(2, 2, 1);
plot(t, x);
title('normal plot');
ax2 = subplot(2, 2, 2);
plot(t, x);
title('axis square');
axis('square');
ax3 = subplot(2, 2, 3);
plot(t, x);
title('axis equal');
axis('equal');
ax4 = subplot(2, 2, 4);
plot(t, x);
title('normal plot again');
axis('normal');
asserts.isequal(f.Position(3:4), groot().DefaultFigurePosition(3:4));
asserts.isapprox(ax1.XLim, [0 6.28], 1e-12);
asserts.isapprox(ax1.YLim, [-1 1], 1e-12);
asserts.isapprox(ax1.Title.Position, [3.1400054516965978 1.0236425339366511 0], 2.5e-3);
asserts.isapprox(ax2.XLim, [0 6.28], 1e-12);
asserts.isapprox(ax2.YLim, [-1 1], 1e-5);
asserts.isequal(ax2.PlotBoxAspectRatio, [1 1 1]);
asserts.isequal(ax2.PlotBoxAspectRatioMode, 'manual');
asserts.isapprox(ax2.DataAspectRatio, [3.14 1 1], 1e-5);
asserts.isequal(ax2.DataAspectRatioMode, 'auto');
asserts.isapprox(ax2.Title.Position, [3.1400216086634636 1.0236425339366511 0], 2.5e-3);
asserts.isapprox(ax3.XLim, [0 6.28], 1e-12);
asserts.istrue(ax3.YLim(1) < -1.9 && ax3.YLim(1) > -2);
asserts.istrue(ax3.YLim(2) > 1.9 && ax3.YLim(2) < 2);
axisEqualZHalfSpan = (ax3.YLim(2) - ax3.YLim(1)) / 2;
asserts.isapprox(ax3.ZLim, [-axisEqualZHalfSpan axisEqualZHalfSpan], 1e-12);
asserts.isequal(ax3.ZTick, [-1 0 1]);
asserts.isequal(ax3.DataAspectRatio, [1 1 1]);
asserts.isequal(ax3.DataAspectRatioMode, 'manual');
asserts.isequal(ax3.PlotBoxAspectRatioMode, 'manual');
asserts.isapprox(ax3.Title.Position, [3.14000545820319 1.9723324275642153 0], 2.5e-3);
asserts.isapprox(ax4.XLim, [0 6.28], 1e-12);
asserts.isapprox(ax4.YLim, [-1 1], 1e-12);
asserts.isequal(ax1.XTick, [0 1 2 3 4 5 6]);
asserts.isequal(ax2.XTick, [0 1 2 3 4 5 6]);
asserts.isequal(ax3.XTick, [0 1 2 3 4 5 6]);
asserts.isequal(ax4.XTick, [0 1 2 3 4 5 6]);
asserts.isequal(ax4.PlotBoxAspectRatioMode, 'auto');
asserts.isequal(ax4.DataAspectRatioMode, 'auto');
asserts.isapprox(ax4.Title.Position, [3.1400133425944716 1.0237500000000004 0], 2.5e-3);
close(f);
%=============================================================================
% multiple options in a single call must all be applied (command form)
f = figure('Visible', 'off');
[x, y, z] = sphere(30);
surf(x, y, z);
ax = gca();
axis equal off
asserts.isequal(char(ax.Visible), 'off');
asserts.isequal(ax.DataAspectRatio, [1 1 1]);
asserts.isequal(ax.DataAspectRatioMode, 'manual');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
close(f);
%=============================================================================
% multiple options in a single call must all be applied (paren form)
f = figure('Visible', 'off');
[x, y, z] = sphere(30);
surf(x, y, z);
ax = gca();
axis('equal', 'off');
asserts.isequal(char(ax.Visible), 'off');
asserts.isequal(ax.DataAspectRatio, [1 1 1]);
asserts.isequal(ax.DataAspectRatioMode, 'manual');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
close(f);
%=============================================================================
% options are applied in order: 'on' after 'off' leaves axes visible
f = figure('Visible', 'off');
plot(1:10);
ax = gca();
axis off on
asserts.isequal(char(ax.Visible), 'on');
close(f);
%=============================================================================
% numeric limits combined with a keyword option in a single call
f = figure('Visible', 'off');
plot(1:10);
ax = gca();
axis([0 5 0 5], 'off');
asserts.isapprox(ax.XLim, [0 5], 1e-12);
asserts.isapprox(ax.YLim, [0 5], 1e-12);
asserts.isequal(char(ax.Visible), 'off');
close(f);
%=============================================================================
