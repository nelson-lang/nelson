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
close all
reset(groot());
addpath([modulepath('graphics', 'tests')]);
%=============================================================================
f = figure('Visible', 'on', 'Position', [100 100 960 520], 'Color', [1 1 1]);
buildStart = tic();
ax1 = axes('position', [.05 .6 .2 .325]);
ax2 = axes('position', [.3 .6 .2 .325]);
ax3 = axes('position', [.05 .075 .45 .35]);
ax4 = axes('position', [.525 -.20 .475 1.4]);
set(gcf(), 'Position', [100 100 960 520]);
n = 64;
C = magic(n);
x2 = 1:16;
y2 = x2;
r1 = 2;
r2 = 1;
[T1, T2] = meshgrid(0:2 * pi / n:2 * pi, 0:2 * pi / n:2 * pi);
R = r1 + r2 * cos(T2);
Z = r2 * sin(T2);
X = cos(T1) .* R;
Y = sin(T1) .* R;
set(gcf(), 'CurrentAxes', ax1);
imagesc(C, [-1 1]), box on;
axis xy
rectangle('Position', [1 1 16 16], 'LineWidth', 2);
set(gcf(), 'CurrentAxes', ax2);
imagesc(x2, y2, C(x2, y2), [-1 1]), box on;
axis xy
set(gcf(), 'CurrentAxes', ax3);
imagesc(C), box on;
axis xy
set(gcf(), 'CurrentAxes', ax4);
surf(X, Y, Z, C, 'EdgeColor', 'none');
axis equal
view(100, 32);
buildElapsed = toc(buildStart);
asserts.istrue(buildElapsed < 45);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
