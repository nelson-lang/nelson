%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
f = figure('Visible', 'off');
x = [1 2 3; 2 4 6; 3 6 9; 100 8 12];
h = boxplot(x, 'Labels', {'A', 'B', 'C'});
asserts.istrue(isgraphics(h, 'hggroup'));
asserts.isequal(h.Type, 'hggroup');
s = h.UserData;
asserts.isequal(s.ChartType, 'boxplot');
asserts.isequal(s.Labels, {'A'; 'B'; 'C'});
asserts.isequal(numel(s.Groups), 3);
asserts.istrue(numel(h.Children) >= 18);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XTick, 1:3);
asserts.isequal(ax.XTickLabel, {'A'; 'B'; 'C'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
values = [1 2 3 4 10 11 12 13 NaN];
groups = {'left', 'left', 'left', 'left', 'right', 'right', 'right', 'right', 'right'};
h = boxplot(values, groups, 'Orientation', 'horizontal', 'Widths', 0.4, 'Symbol', 'rx');
s = h.UserData;
asserts.isequal(s.Orientation, 'horizontal');
asserts.isequal(s.Labels, {'left'; 'right'});
ax = ancestor(h, 'axes');
asserts.isequal(ax.YTick, 1:2);
asserts.isequal(ax.YTickLabel, {'left'; 'right'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = boxplot(ax, [1; 2; 3; 20], 'Whisker', 0, 'ShowOutliers', 'off', 'LineWidth', 2);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.UserData.ChartType, 'boxplot');
close(f);
%=============================================================================
f = figure('Visible', 'off');
girls = (randn(10, 1) * 5 + 140);
boys = (randn(13, 1) * 8 + 135);
heights = [girls; boys];
sex = [ones(numel(girls), 1); 2 * ones(numel(boys), 1)];
h = boxplot(heights, sex, 'Labels', {'girls', 'boys'});
xlim([0 3]);
title('Grade 3 heights');
asserts.istrue(isgraphics(h, 'hggroup'));
asserts.isequal(numel(h.UserData.Groups), 2);
asserts.isequal(h.UserData.Labels, {'girls'; 'boys'});
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
data = [(randn(10, 1) * 5 + 140); (randn(25, 1) * 8 + 135); ...
  (randn(20, 1) * 6 + 165)];
groups = [(ones(10, 1)); (ones(25, 1) * 2); (ones(20, 1) * 3)];
labels = {'Team A', 'Team B', 'Team C'};
pos = [2, 1, 3];
h = boxplot(data, groups, 'Notch', 'on', 'Labels', labels, 'Positions', pos, ...
  'BoxStyle', 'filled', 'Widths', 0.45);
title('Group splitting with paired vectors');
asserts.isequal(h.UserData.Labels, labels(:));
asserts.isequal(h.UserData.Positions, pos);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XTick, pos);
asserts.isequal(ax.XTickLabel, labels(:));
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = boxplot(randn(100, 9), 'Notch', 'on', 'BoxStyle', 'filled', ...
  'Colors', 'ygcwkmb', 'Whisker', 1.2);
title('Different colors specified with characters');
asserts.isequal(numel(h.UserData.Groups), 9);
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
colors = [0.7 0.7 0.7; 0.0 0.4 0.9; 0.7 0.4 0.3; 0.7 0.1 0.7; ...
  0.8 0.7 0.4; 0.1 0.8 0.5; 0.9 0.9 0.2];
h = boxplot(randn(100, 13), 'Notch', 'on', 'BoxStyle', 'filled', ...
  'Colors', colors, 'Whisker', 1.3, 'Widths', 0.45);
title('Different colors specified as RGB values');
asserts.isequal(numel(h.UserData.Groups), 13);
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
X = [1 2 3; 2 4 6; 3 6 9; 8 7 12];
h = boxplot(X, 'Labels', {'A', 'B', 'C'});
patches = findobj(h, 'Type', 'patch');
asserts.isequal(numel(patches), 3);
asserts.isequal(get(patches(1), 'EdgeColor'), [0 0.447 0.741]);
asserts.isequal(get(patches(1), 'FaceColor'), 'none');
lines = findobj(h, 'Type', 'line');
medianColors = [];
dashedCount = 0;
for k = 1:numel(lines)
  if strcmp(get(lines(k), 'LineStyle'), '--')
    dashedCount = dashedCount + 1;
    asserts.isequal(get(lines(k), 'Color'), [0 0.447 0.741]);
  end
  if isequal(get(lines(k), 'Color'), [0.85 0.325 0.098])
    medianColors = [medianColors; get(lines(k), 'Color')];
  end
end
asserts.isequal(size(medianColors, 1), 3);
asserts.isequal(dashedCount, 6);
close(f);
%=============================================================================
asserts.checkerror('boxplot();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('boxplot({1, 2});', getString(message('Nelson:validators:mustBeNumeric', 'Value')));
asserts.checkerror('boxplot([1 2], [1 2 3]);', getString(message('Nelson:graphics:invalidGroupData')));
asserts.checkerror('boxplot([1 2], ''Labels'', {''A'', ''B''});', getString(message('Nelson:boxplot:invalidNumberOfLabels')));
asserts.checkerror('boxplot([1 2], ''BoxWidth'', 0.5);', getString(message('Nelson:graphics:unsupportedProperty', 'boxplot')));
asserts.checkerror('boxplot([1 2], ''OutlierTags'', ''on'');', getString(message('Nelson:graphics:unsupportedProperty', 'boxplot')));
%=============================================================================
