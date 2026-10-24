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
%=============================================================================
f = figure();
x = 0:pi / 100:2 * pi;
y = sin(x);
plot(x, y);
xlabel('X label');
ax = gca();
asserts.istrue(isgraphics(ax.XLabel, 'text'));
asserts.isequal(ax.XLabel.String, 'X label');
%=============================================================================
close all
%=============================================================================
f = figure();
x = 0:pi / 100:2 * pi;
y = sin(x);
plot(x, y);
ylabel('Y label');
ax = gca();
asserts.istrue(isgraphics(ax.YLabel, 'text'));
asserts.isequal(ax.YLabel.String, 'Y label');
%=============================================================================
f = figure();
x = 0:pi / 100:2 * pi;
y = sin(x);
plot(x, y);
zlabel('Z label');
ax = gca();
asserts.istrue(isgraphics(ax.ZLabel, 'text'));
asserts.isequal(ax.ZLabel.String, 'Z label');
drawnow();
assert_values_isclose(ax.ZLabel.FontSize, 11, 1e-12);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
t = 0:pi / 50:10 * pi;
plot3(sin(t), cos(t), t);
axis square;
zlim([0 10 * pi]);
zlabel('Z label');
zlabelSvgFile = [tempname(), '.svg'];
saveas(f, zlabelSvgFile);
ax = gca();
assert_values_isclose(ax.ZLim, [0 10 * pi], 1e-12);
assert_values_isclose(ax.XTick, [-1 0 1], 1e-12);
assert_values_isclose(ax.YTick, [-1 0 1], 1e-12);
if numel(ax.ZTick) == 7
  assert_values_isclose(ax.ZTick, [0 5 10 15 20 25 30], 1e-12);
else
  assert_values_isclose(ax.ZTick, [0 20], 1e-12);
end
assert_values_isclose(ax.ZLabel.FontSize, 11, 1e-12);
asserts.isequal(ax.ZLabel.Rotation, 90);
asserts.isequal(ax.ZLabel.VerticalAlignment, 'bottom');
asserts.isequal(ax.ZLabel.Units, 'data');
assert_values_isclose(ax.ZLabel.Position, [-1.2125134400534989 1.1630678183772858 15.707974225017779], 1e-12);
close(f);
rmfile(zlabelSvgFile);
%=============================================================================
f = figure();
x = 0:pi / 100:2 * pi;
y = sin(x);
plot(x, y);
ax = gca();
xlabel(ax, 'X label');
asserts.istrue(isgraphics(ax.XLabel, 'text'));
asserts.isequal(ax.XLabel.String, 'X label');
%=============================================================================
close all
%=============================================================================
function assert_values_isclose(actual, expected, tolerance)
  asserts.isequal(numel(actual), numel(expected));
  actual = reshape(actual, 1, []);
  expected = reshape(expected, 1, []);
  asserts.istrue(all(abs(actual - expected) <= tolerance));
end
%=============================================================================
