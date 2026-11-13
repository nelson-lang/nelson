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
% 2x4 polar axes grid: FontSize shrinks, the title follows.
f = figure('Position', [100 100 900 500], 'Visible', 'off');
p = gobjects(2, 4);
for l = 1:4
  for d = 1:2
    p(d, l) = polaraxes(f, 'Position', [0.03 + (l - 1) * 0.245, 0.56 - (d - 1) * 0.5, 0.19, 0.34]);
    title(p(d, l), 'T');
  end
end
drawnow();
assert_isequal(p(1, 1).FontSize, 8.5);
assert_isequal(p(1, 1).FontSizeMode, 'auto');
assert_isapprox(p(1, 1).Title.FontSize, 9.35, 1e-12);
%=============================================================================
% Same grid with cartesian axes: labels follow too.
clf(f);
a = gobjects(2, 4);
for l = 1:4
  for d = 1:2
    a(d, l) = axes(f, 'Position', [0.03 + (l - 1) * 0.245, 0.56 - (d - 1) * 0.5, 0.19, 0.34]);
    title(a(d, l), 'T');
  end
end
drawnow();
assert_isequal(a(2, 4).FontSize, 8.5);
assert_isapprox(a(2, 4).Title.FontSize, 9.35, 1e-12);
assert_isapprox(a(2, 4).XLabel.FontSize, 9.35, 1e-12);
assert_isapprox(a(2, 4).YLabel.FontSize, 9.35, 1e-12);
%=============================================================================
% Width and height thresholds (pixels), half point steps down to 8.
clf(f);
widths = [100 130 131 178 179 227 228 275 276 400];
expectedW = [8 8 8.5 8.5 9 9 9.5 9.5 10 10];
heights = [100 104 105 143 144 181 182 220 221 400];
expectedH = [8 8 8.5 8.5 9 9 9.5 9.5 10 10];
fw = zeros(size(widths));
fh = zeros(size(heights));
for k = 1:numel(widths)
  ax = axes(f, 'Units', 'pixels', 'Position', [20 20 widths(k) 450]);
  drawnow();
  fw(k) = ax.FontSize;
  delete(ax);
  ax = axes(f, 'Units', 'pixels', 'Position', [20 20 800 heights(k)]);
  drawnow();
  fh(k) = ax.FontSize;
  delete(ax);
end
assert_isequal(fw, expectedW);
assert_isequal(fh, expectedH);
% The smaller side drives the shrink.
ax = axes(f, 'Units', 'pixels', 'Position', [20 20 250 150]);
drawnow();
assert_isequal(ax.FontSize, 9);
%=============================================================================
% Auto again after a manual size, and the size follows Position.
clf(f);
ax = axes(f, 'Units', 'pixels', 'Position', [20 20 150 150]);
drawnow();
assert_isequal(ax.FontSize, 8.5);
ax.FontSize = 12;
drawnow();
assert_isequal(ax.FontSize, 12);
assert_isequal(ax.FontSizeMode, 'manual');
ax.Position = [20 20 100 100];
drawnow();
assert_isequal(ax.FontSize, 12);
ax.FontSizeMode = 'auto';
drawnow();
assert_isequal(ax.FontSize, 8);
ax.Position = [20 20 400 400];
drawnow();
assert_isequal(ax.FontSize, 10);
assert_isequal(ax.FontSizeMode, 'auto');
%=============================================================================
% Normalized axes follow the figure size.
clf(f);
ax = axes(f, 'Position', [0.1 0.1 0.19 0.34]);
title(ax, 'T');
drawnow();
assert_isequal(ax.FontSize, 8.5);
f.Position = [100 100 1800 1000];
drawnow();
assert_isequal(ax.FontSize, 10);
assert_isapprox(ax.Title.FontSize, 11, 1e-12);
f.Position = [100 100 900 500];
drawnow();
assert_isequal(ax.FontSize, 8.5);
assert_isapprox(ax.Title.FontSize, 9.35, 1e-12);
%=============================================================================
% A title size set by the user is kept.
title(ax, 'T', 'FontSize', 20);
xlabel(ax, 'x');
set(ax.XLabel, 'FontSize', 15);
f.Position = [100 100 1800 1000];
drawnow();
assert_isequal(ax.FontSize, 10);
assert_isequal(ax.Title.FontSize, 20);
assert_isequal(ax.XLabel.FontSize, 15);
f.Position = [100 100 900 500];
drawnow();
assert_isequal(ax.FontSize, 8.5);
assert_isequal(ax.Title.FontSize, 20);
assert_isequal(ax.XLabel.FontSize, 15);
%=============================================================================
% The shrink starts from the user default.
set(groot, 'DefaultAxesFontSize', 20);
clf(f);
ax = axes(f, 'Units', 'pixels', 'Position', [20 20 180 400]);
drawnow();
assert_isequal(ax.FontSize, 17.5);
assert_isequal(ax.FontSizeMode, 'auto');
set(groot, 'DefaultAxesFontSize', 'remove');
%=============================================================================
% subplot: the grid cell sizes the font, not the inner box.
clf(f);
s = gobjects(1, 16);
for k = 1:16
  s(k) = subplot(4, 4, k);
end
drawnow();
assert_isequal(s(1).FontSize, 8);
f.Position = [100 100 859 517];
drawnow();
assert_isequal(s(16).FontSize, 8.5);
f.Position = [100 100 1400 800];
drawnow();
assert_isequal(s(1).FontSize, 9);
close(f);
f = figure('Position', [100 100 859 517], 'Visible', 'off');
s = subplot(2, 2, 1);
drawnow();
assert_isequal(s.FontSize, 9.5);
% a cell of another grid shape and a span use their inner box
s2 = subplot(4, 4, 16);
drawnow();
assert_isequal(s2.FontSize, 8);
clf(f);
s = subplot(4, 4, [1 2]);
drawnow();
assert_isequal(s.FontSize, 8);
clf(f);
s = subplot(1, 1, 1);
drawnow();
assert_isequal(s.FontSize, 10);
f.Position = [100 100 900 500];
drawnow();
%=============================================================================
% tiledlayout tiles follow the grid too.
clf(f);
f.Position = [100 100 560 420];
drawnow();
t = tiledlayout(f, 1, 2);
ax1 = nexttile(t);
ax2 = nexttile(t);
drawnow();
assert_isequal(ax1.FontSize, 9);
assert_isequal(ax2.FontSize, 9);
f.Position = [100 100 900 500];
drawnow();
%=============================================================================
% An axes laid out from its OuterPosition uses the default plot box inside it.
clf(f);
f.Position = [100 100 400 300];
drawnow();
ax = axes(f);
plot(ax, 1:10);
colorbar(ax, 'westoutside');
drawnow();
assert_isequal(ax.FontSize, 10);
f.Position = [100 100 859 517];
drawnow();
clf(f);
ax = axes(f, 'OuterPosition', [0 0.75 0.25 0.25]);
drawnow();
assert_isequal(ax.FontSize, 8.5);
f.Position = [100 100 900 500];
drawnow();
clf(f);
pn = uipanel(f, 'Position', [0 0 0.3 0.3]);
ax = axes(pn);
drawnow();
assert_isequal(ax.FontSize, 8.5);
%=============================================================================
% uiaxes keep their size.
clf(f);
ua = uiaxes(f, 'Position', [20 20 150 150]);
drawnow();
fs = ua.FontSize;
ua.Position = [20 20 600 400];
drawnow();
assert_isequal(ua.FontSize, fs);
close(f);
%=============================================================================
