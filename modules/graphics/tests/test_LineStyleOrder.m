%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
rng('default');
ax = gca();
ax.ColorOrder = [0 0 0; 0.5 0.5 0.5];
ax.LineStyleOrder = {'-', ':', '--', '-.', '--'};
hold all
expectedLineStyle = {'-',  '-',  ':',  ':',  '--',  '--'};
expectedColor = {   [0     0     0],...
[0.5000    0.5000    0.5000],...
[0     0     0], ...
[0.5000    0.5000    0.5000], ...
[0     0     0], ...
[0.5000    0.5000    0.5000]};
expectedColorIndex =  [2, 1, 2, 1, 2, 1];
expectedLineStyleIndex = [1, 2, 2, 3, 3, 4];

for i = 1:6
  H = plot( rand( 1, 10 ) );
  assert_isequal(ax.ColorOrderIndex, expectedColorIndex(i));
  assert_isequal(ax.LineStyleOrderIndex, expectedLineStyleIndex(i));
  assert_isequal(H.LineStyle, expectedLineStyle{i})
  assert_isapprox(H.Color, expectedColor{i}, 1e-2);
end
%=============================================================================
f = figure();
ax = gca();
ax.LineStyleOrder = {'-', '--', ':', '-.', '-', '--', '-', '--'};
hold on
expected = [1, 1, 1, 1, 1, 1, 2, 2, 2];

for r = 1:9
  x = linspace(0,r,500);
  y = sqrt(r.^2-x.^2);
  plot(x, y, 'LineWidth', 2);
  assert_isequal(ax.LineStyleOrderIndex, expected(r));
end
%=============================================================================
f = figure();
ax = axes;
ax.ColorOrder = [1 0 0; 0 0 1];
ax.LineStyleOrder = {'-','--'};
hold on
expected = [1, 2, 2, 3, 3];
for i = 1:5
  plot([i i+2])
  assert_isequal(ax.LineStyleOrderIndex, expected(i));
end
hold off
%=============================================================================
f = figure();
ax = axes;
ax.ColorOrder = [1 0 0; 0 0 1];
ax.LineStyleOrder = {'-','--'};
hold on
X1 = 1:5;
X2=  16: 20;
X = [X1;X2];
Y1 = sin(X1);
Y2 = cos(X2);
Y = [Y1;Y2];
H = plot(X, Y);
assert_isequal(H(1).LineStyle, '-');
assert_isequal(H(2).LineStyle, '-');
assert_isequal(H(3).LineStyle, '--');
assert_isequal(H(4).LineStyle, '--');
assert_isequal(H(4).LineStyle, '--');
%=============================================================================