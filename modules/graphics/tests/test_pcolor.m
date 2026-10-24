%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
x1 = [1.0 0.9 0.8 0.7];
x2 = [0.6 0.5 0.4 0.3];
x3 = [0.2 0.1 0.0 0.5];
x4 = [0.5 0.5 0.5 0.5];
xd = [1.0 1.0 1.0 1.0];
v = [[x1 0]; [x2 0]; [x3 0]; [x4 0]; [xd 0]];
h = pcolor(v);
asserts.isequal(h.ZData, zeros(size(v)));
asserts.isequal(h.CData, v);
colormap(gray)
%=============================================================================
f = figure('Visible', 'on', 'Position', [100 100 500 500], 'Color', [1 1 1]);
C = [1 2 3; 4 5 6; 7 8 9];
h = pcolor(C);
h.EdgeColor = 'none';
colormap([1 0 0; 0 1 0; 0 0 1; 1 1 0; 1 0 1; 0 1 1; 0.5 0.5 0.5; 0 0 0; 1 1 1]);
clim([1 9]);
ax = gca();
ax.Position = [0 0 1 1];
ax.Visible = 'off';
drawnow();
F = getframe(f);
assert_pixel_color(F.cdata, 0.25, 0.75, [255 0 0]);
assert_pixel_color(F.cdata, 0.75, 0.75, [0 255 0]);
assert_pixel_color(F.cdata, 0.25, 0.25, [255 255 0]);
assert_pixel_color(F.cdata, 0.75, 0.25, [255 0 255]);
close(f);
%=============================================================================
f = figure();
[X, Y] = meshgrid(1:20);
LY = log(Y);
C = repmat([1:20; 20:-1:1], 10, 1);
s = pcolor(X, LY, C);
%=============================================================================
ax1 = subplot(1, 2, 1);
C1 = rand(20, 10);
pcolor(ax1, C1)
ax2 = subplot(1, 2, 2);
C2 = rand(50, 10);
pcolor(ax2, C2)
%=============================================================================
x = linspace(0, 2 * pi, 100);
y = linspace(0, 2 * pi, 100);
Z = sin(x' * y);
f = figure();
h = pcolor(x, y, Z);
asserts.isequal(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882]);
%=============================================================================
function assert_pixel_color(img, xFraction, yFraction, expected)
  [height, width, ~] = size(img);
  x = max(1, min(width, round(width * xFraction)));
  y = max(1, min(height, round(height * yFraction)));
  pixel = double(squeeze(img(y, x, 1:3))).';
  asserts.istrue(max(abs(pixel - expected)) <= 12);
end
%=============================================================================
