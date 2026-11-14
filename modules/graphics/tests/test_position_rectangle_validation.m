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
negativeSize = getString(message('Nelson:graphics:positionNegativeSize'));
notFinite = getString(message('Nelson:graphics:positionMustBeFinite'));
notNumeric = getString(message('Nelson:graphics:positionMustBeNumeric'));
notFourElements = getString(message('Nelson:graphics:positionFourElements'));
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
makers = {@() uicontrol(f), @() uipanel(f), @() uibuttongroup(f), @() uitabgroup(f), @() uitable(f)};
for k = 1:numel(makers)
  h = makers{k}();
  h.Units = 'pixels';
  h.Position = [10 10 50 50];
  % a negative width or height is refused and the value is kept
  assertRefused(h, 'Position', [10 10 -5 20], negativeSize);
  assertRefused(h, 'Position', [10 10 20 -5], negativeSize);
  assertRefused(h, 'Position', [10 10 NaN 20], notFinite);
  assertRefused(h, 'Position', [NaN 10 20 20], notFinite);
  assertRefused(h, 'Position', [10 10 Inf 20], notFinite);
  assertRefused(h, 'Position', [-Inf 10 20 20], notFinite);
  % only four numeric values are a position
  assertRefused(h, 'Position', 'abcd', notNumeric);
  assertRefused(h, 'Position', true(1, 4), notNumeric);
  assertRefused(h, 'Position', {10, 10, 20, 20}, notNumeric);
  assertRefused(h, 'Position', [10 10 20], notFourElements);
  assertRefused(h, 'Position', [10 10 20 20 5], notFourElements);
  assertRefused(h, 'Position', [], notFourElements);
  asserts.isequal(h.Position, [10 10 50 50]);
  % zero sizes and negative origins are valid
  h.Position = [10 10 0 20];
  asserts.isequal(h.Position, [10 10 0 20]);
  h.Position = [10 10 20 0];
  asserts.isequal(h.Position, [10 10 20 0]);
  h.Position = [-10 -10 20 20];
  asserts.isequal(h.Position, [-10 -10 20 20]);
  h.Position = int8([10; 10; 20; 20]);
  asserts.isequal(h.Position, [10 10 20 20]);
  h.Position = single([11 12 21 22]);
  asserts.isequal(h.Position, [11 12 21 22]);
  delete(h);
end
%=============================================================================
% any units, set(), the constructor and the other position properties
c = uicontrol(f, 'Units', 'normalized', 'Position', [0.1 0.1 0.2 0.2]);
assertRefused(c, 'Position', [0.1 0.1 -0.5 0.2], negativeSize);
assertRefused(c, 'InnerPosition', [0.1 0.1 -0.5 0.2], negativeSize);
assertRefused(c, 'OuterPosition', [0.1 0.1 0.5 -0.2], negativeSize);
c.Position = [0.1 0.1 0 0.2];
asserts.isapprox(c.Position, [0.1 0.1 0 0.2], 1e-12);
assertCodeRefused(@() uicontrol(f, 'Position', [10 10 -5 20]), negativeSize);
assertCodeRefused(@() uipanel(f, 'Position', [0 0 -1 1]), negativeSize);
%=============================================================================
% setpixelposition restores Units when the position is refused
assertCodeRefused(@() setpixelposition(c, [1 1 -10 10]), negativeSize);
asserts.isequal(c.Units, 'normalized');
asserts.isapprox(c.Position, [0.1 0.1 0 0.2], 1e-12);
%=============================================================================
% axes, legend and colorbar follow the same rule, an annotation does not
ax = axes(f, 'Units', 'normalized', 'Position', [0.1 0.1 0.5 0.5]);
plot(ax, 1:3);
assertRefused(ax, 'Position', [0.1 0.1 -0.5 0.5], negativeSize);
assertRefused(ax, 'Position', [0.1 0.1 0.5 -0.5], negativeSize);
assertRefused(ax, 'Position', [0.1 0.1 NaN 0.5], notFinite);
assertRefused(ax, 'Position', 'abcd', notNumeric);
assertRefused(ax, 'Position', true(1, 4), notNumeric);
assertRefused(ax, 'Position', [0.1 0.1 0.5], notFourElements);
assertRefused(ax, 'OuterPosition', [], notFourElements);
assertRefused(ax, 'InnerPosition', [0.1 0.1 -0.5 0.5], negativeSize);
assertRefused(ax, 'OuterPosition', [0.1 0.1 0.5 -0.5], negativeSize);
assertCodeRefused(@() axes(f, 'Position', [0.1 0.1 -0.5 0.5]), negativeSize);
ax.Position = [0.1 0.1 0 0.5];
asserts.isapprox(ax.Position, [0.1 0.1 0 0.5], 1e-12);
ax.Position = [-0.1 -0.1 0.5 0.5];
asserts.isapprox(ax.Position, [-0.1 -0.1 0.5 0.5], 1e-12);
ax.Position = [0.1 0.1 0.5 0.5];
lg = legend(ax, 'a');
assertRefused(lg, 'Position', [0.1 0.1 -0.5 0.2], negativeSize);
assertRefused(lg, 'Position', [0.1 0.1 0.5 Inf], notFinite);
cb = colorbar(ax);
assertRefused(cb, 'Position', [0.1 0.1 -0.5 0.2], negativeSize);
assertRefused(cb, 'Position', [NaN 0.1 0.5 0.2], notFinite);
an = annotation(f, 'rectangle', [0.1 0.1 0.2 0.2]);
an.Position = [0.1 0.1 -0.5 0.2];
asserts.isapprox(an.Position, [0.1 0.1 -0.5 0.2], 1e-12);
%=============================================================================
assertRefused(f, 'Position', [100 100 -5 400], negativeSize);
assertRefused(f, 'Position', [100 100 500 NaN], notFinite);
assertRefused(f, 'Position', 'abcd', notNumeric);
assertRefused(f, 'Position', true(1, 4), notNumeric);
assertRefused(f, 'Position', [100 100 500], notFourElements);
assertRefused(f, 'InnerPosition', [100 100 -5 400], negativeSize);
assertRefused(f, 'OuterPosition', [100 100 500 -5], negativeSize);
asserts.isequal(f.Position, [100 100 500 400]);
close(f);
%=============================================================================
% charts, layouts, the bubble legend and the rectangle primitive follow the same rule
T = table([1; 2; 3; 4], [4; 3; 2; 1], 'VariableNames', {'x', 'y'});
charts = {@(f) heatmap(magic(4)), @(f) piechart(f, [1 2 3]), @(f) donutchart(f, [1 2 3]), ...
  @(f) bubblecloud(f, [1 2 3]), @(f) parallelplot(f, magic(4)), @(f) scatterhistogram(f, T, 'x', 'y'), ...
  @(f) wordcloud(f, {'a', 'b', 'c'}, [3 2 1]), @(f) stackedplot(f, magic(4)), @(f) tiledlayout(f, 2, 2), ...
  @(f) polaraxes(f)};
names = {'Position', 'InnerPosition', 'OuterPosition'};
for k = 1:numel(charts)
  f = figure('Visible', 'off');
  h = charts{k}(f);
  for n = 1:numel(names)
    assertRefused(h, names{n}, [0.1 0.1 -0.5 0.5], negativeSize);
    assertRefused(h, names{n}, [0.1 0.1 0.5 -0.5], negativeSize);
    assertRefused(h, names{n}, [0.1 0.1 NaN 0.5], notFinite);
    assertRefused(h, names{n}, [0.1 0.1 0.5], notFourElements);
    assertRefused(h, names{n}, 'abcd', notNumeric);
    assertRefused(h, names{n}, true(1, 4), notNumeric);
  end
  h.Position = [0.1 0.1 0.5 0.5];
  close(f);
end
f = figure('Visible', 'off');
ax = axes(f);
bubblechart(ax, 1:3, 1:3, 1:3);
bl = bubblelegend(ax);
assertRefused(bl, 'Position', [0.1 0.1 -0.5 0.5], negativeSize);
assertRefused(bl, 'Position', [0.1 0.1 Inf 0.5], notFinite);
r = rectangle(ax, 'Position', [0 0 1 1]);
assertRefused(r, 'Position', [0 0 -1 1], negativeSize);
assertRefused(r, 'Position', [0 0 1 -1], negativeSize);
assertRefused(r, 'Position', [0 0 NaN 1], notFinite);
assertRefused(r, 'Position', [0 0 1], notFourElements);
r.Position = [-1 -2 0 3];
asserts.isequal(r.Position, [-1 -2 0 3]);
close(f);
%=============================================================================
f = uifigure('Visible', 'off', 'Position', [100 100 500 400]);
makers = {@() uibutton(f), @() uipanel(f), @() uilabel(f)};
for k = 1:numel(makers)
  h = makers{k}();
  h.Position = [10 10 50 50];
  assertRefused(h, 'Position', [10 10 -5 20], negativeSize);
  assertRefused(h, 'Position', [10 10 NaN 20], notFinite);
  h.Position = [10 10 0 20];
  asserts.isequal(h.Position, [10 10 0 20]);
end
uax = uiaxes(f, 'Position', [10 10 200 150]);
assertRefused(uax, 'Position', [10 10 -5 20], negativeSize);
assertRefused(uax, 'Position', [10 10 NaN 20], notFinite);
assertRefused(f, 'Position', [100 100 -5 400], negativeSize);
close(f);
%=============================================================================
function assertRefused(h, name, value, expected)
  before = get(h, name);
  msg = '';
  try
    set(h, name, value);
  catch
    e = lasterror();
    msg = e.message;
  end
  asserts.istrue(contains(msg, expected), msg);
  asserts.isequal(get(h, name), before);
end
%=============================================================================
function assertCodeRefused(fn, expected)
  msg = '';
  try
    fn();
  catch
    e = lasterror();
    msg = e.message;
  end
  asserts.istrue(contains(msg, expected), msg);
end
%=============================================================================
