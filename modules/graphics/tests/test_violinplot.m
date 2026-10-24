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
f = figure('Visible', 'off');
h = violinplot([1 2 2 3 4]);
asserts.istrue(isgraphics(h, 'violinplot'));
asserts.isequal(h.Type, 'violinplot');
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.XData, ones(5, 1));
asserts.isequal(h.YData, [1; 2; 2; 3; 4]);
asserts.isequal(h.Orientation, 'vertical');
asserts.isapprox(h.DensityWidth, 0.9, 1e-12);
asserts.isapprox(h.FaceAlpha, 0.2, 1e-12);
asserts.isapprox(h.FaceColor, [0.066 0.443 0.745], 1e-12);
asserts.isapprox(h.EdgeColor, [0.066 0.443 0.745], 1e-12);
asserts.isequal(h.LineStyle, '-');
asserts.isapprox(h.LineWidth, 0.5, 1e-12);
asserts.isequal(h.DensityDirection, 'both');
asserts.isequal(h.DensityScale, 'area');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = violinplot([1 2 3; 4 5 6], 'Orientation', 'horizontal', 'DensityWidth', 0.4);
asserts.isequal(size(h), [1 3]);
asserts.isequal(h(1).Type, 'violinplot');
asserts.isequal(h(1).YData, [1; 4]);
asserts.isequal(h(1).XData, [1; 1]);
asserts.isequal(h(1).Orientation, 'horizontal');
asserts.isapprox(h(1).DensityWidth, 0.4, 1e-12);
asserts.isequal(h(2).YData, [2; 5]);
asserts.isequal(h(2).XData, [2; 2]);
asserts.isequal(h(3).YData, [3; 6]);
asserts.isequal(h(3).XData, [3; 3]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = violinplot([1 1 2 2], [10 11 20 21]);
asserts.istrue(isgraphics(h, 'violinplot'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.XData, [1; 1; 2; 2]);
asserts.isequal(h.YData, [10; 11; 20; 21]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = violinplot([1 1 1 2 2 2], [1 2 2 3 4 5]);
drawnow();
ax = gca();
asserts.isequal(numel(ax.Children), 1);
asserts.isequal(ax.Children(1), h);
asserts.isequal(char(ax.Box), 'off');
asserts.isapprox(ax.XLim, [0.5 2.5], 1e-12);
asserts.isapprox(ax.XTick, 0.5:0.5:2.5, 1e-12);
asserts.isapprox(ax.YLim, [-4 10], 1e-12);
asserts.isapprox(ax.YTick, -4:2:10, 1e-12);
asserts.isapprox(ax.ZLim, [0 1], 1e-12);
asserts.isapprox(ax.ZTick, [0 0.5 1], 1e-12);
close(f);
%=============================================================================
% Kernel density support: [min - 3h, max + 3h], h = MAD / 0.6745 * (4 / (3 n))^(1/5).
f = figure('Visible', 'off');
h = violinplot([1 2 2 3 4]);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [-2.414546624 7.414546624], 1e-9);
close(f);
% Zero MAD: the data range is the scale.
f = figure('Visible', 'off');
h = violinplot([0 0 0 1 7]);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [-16.12178188 23.12178188], 1e-9);
close(f);
% Constant data and a single sample: unit bandwidth.
f = figure('Visible', 'off');
h = violinplot([5 5 5 5]);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [2 8], 1e-12);
close(f);
f = figure('Visible', 'off');
h = violinplot(2);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [-1 5], 1e-12);
close(f);
% Several violins: data range padded by the widest kernel support.
f = figure('Visible', 'off');
h = violinplot([1 1 1 2 2 2], [1 2 2 3 4 5]);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [-2.781836918 8.781836918], 1e-9);
close(f);
f = figure('Visible', 'off');
h = violinplot([1 2 3; 4 5 6], 'Orientation', 'horizontal');
drawnow();
asserts.isapprox(gca().XLim, [-6 14], 1e-12);
close(f);
%=============================================================================
% Large data: no oversmoothing, and repaints stay cheap.
rng(0);
y = randn(1e5, 1);
f = figure('Visible', 'off');
h = violinplot(y);
drawnow();
asserts.isapprox(gca().YLim, [-5 5], 1e-12);
axis tight;
drawnow();
asserts.istrue(gca().YLim(1) > min(y) - 0.5);
asserts.istrue(gca().YLim(2) < max(y) + 0.5);
close(f);
%=============================================================================
% The cached density follows data changes.
f = figure('Visible', 'off');
violinplot([1 2 2 3 8]);
ylim([-10 20]);
drawnow();
expected = regexprep(__web_display_list__(f), '"displaySignature":"\d+"', '');
close(f);
f = figure('Visible', 'off');
h = violinplot([1 2 2 3 4]);
ylim([-10 20]);
drawnow();
h.YData = [1; 2; 2; 3; 8];
drawnow();
asserts.isequal(regexprep(__web_display_list__(f), '"displaySignature":"\d+"', ''), expected);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = violinplot(ax, [1 1], [2 3], 'FaceAlpha', 0.25, 'EdgeColor', [1 0 0]);
asserts.isequal(h.Parent, ax);
asserts.isapprox(h.FaceAlpha, 0.25, 1e-12);
asserts.isapprox(h.EdgeColor, [1 0 0], 1e-12);
close(f);
%=============================================================================
% EvaluationPoints and DensityValues: computed in auto mode, one column per violin.
f = figure('Visible', 'off');
h = violinplot([1 2 2 3 4]);
asserts.isequal(h.EvaluationPointsMode, 'auto');
asserts.isequal(h.DensityValuesMode, 'auto');
asserts.isequal(class(h.EvaluationPoints), 'double');
asserts.isequal(size(h.EvaluationPoints), [100 1]);
asserts.isequal(size(h.DensityValues), [100 1]);
asserts.isapprox(h.EvaluationPoints(1), -2.414546624, 1e-9);
asserts.isapprox(h.EvaluationPoints(100), 7.414546624, 1e-9);
asserts.isapprox(h.DensityValues(1), 0.000855498237928, 1e-9);
asserts.isapprox(max(h.DensityValues), 0.253131642593, 1e-9);
hold on
h2 = violinplot([1 1 1 2 2 2], [1 2 2 3 4 5]);
hold off
asserts.isequal(size(h2.EvaluationPoints), [100 2]);
asserts.isequal(size(h2.DensityValues), [100 2]);
asserts.isapprox(h2.DensityValues(1:3, :), [0.001788485501 0.001253926228; 0.002222733073 0.001580090029; 0.002748541346 0.001979961435], 1e-9);
% Manual evaluation points: the density is evaluated there, a row is stored as a column.
h.EvaluationPoints = linspace(0, 5, 7);
asserts.isequal(h.EvaluationPointsMode, 'manual');
asserts.isequal(h.DensityValuesMode, 'auto');
asserts.isequal(size(h.EvaluationPoints), [7 1]);
asserts.isapprox(h.DensityValues', [0.07991523896 0.1651764713 0.2372385644 0.2497921006 0.2053763334 0.1351723558 0.06711794502], 1e-9);
h.EvaluationPointsMode = 'auto';
asserts.isequal(size(h.EvaluationPoints), [100 1]);
% Manual density values survive a data change, the evaluation points follow it.
h.DensityValues = (1:100)';
asserts.isequal(h.DensityValuesMode, 'manual');
h.YData = [1; 2; 2; 3; 8];
asserts.isapprox(h.EvaluationPoints(100), 11.41454662, 1e-9);
asserts.isequal(h.DensityValues, (1:100)');
h.DensityValuesMode = 'auto';
asserts.isapprox(h.DensityValues(1), 0.0008554893523, 1e-9);
h.EvaluationPoints = single([1; 2; 3]);
asserts.isequal(class(h.EvaluationPoints), 'single');
close(f);
%=============================================================================
% Manual evaluation points bound the value axis; mismatched sizes draw nothing.
f = figure('Visible', 'off');
h = violinplot([1 2 2 3 4]);
h.EvaluationPoints = (0:0.5:5)';
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [0 5], 1e-12);
close(f);
f = figure('Visible', 'off');
h = violinplot([1 1 2 2], [1 2 3 4]);
h.EvaluationPoints = [(0:5)', (1:6)'];
asserts.isapprox(h.DensityValues(1, :), [0.1041218093 0.004056825624], 1e-9);
drawnow();
withViolins = count(__web_display_list__(f), '"kind":"polygon"');
h.EvaluationPoints = (0:5)';
drawnow();
asserts.isequal(withViolins - count(__web_display_list__(f), '"kind":"polygon"'), 2);
close(f);
%=============================================================================
% Precomputed densities without sample data: one violin per column.
e = (0:0.5:5)';
d = exp(-(e - 2.5) .^ 2);
f = figure('Visible', 'off');
h = violinplot('EvaluationPoints', e, 'DensityValues', d);
asserts.isequal(size(h), [1 1]);
asserts.isequal(size(h.XData), [1 0]);
asserts.isequal(h.XDataMode, 'auto');
asserts.isequal(h.EvaluationPointsMode, 'manual');
asserts.isequal(h.DensityValuesMode, 'manual');
asserts.isequal(h.DensityValues, d);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [0 5], 1e-12);
close(f);
f = figure('Visible', 'off');
h = violinplot('EvaluationPoints', [e, e + 1], 'DensityValues', [d, 2 * d]);
axis tight;
drawnow();
asserts.isapprox(gca().YLim, [0 6], 1e-12);
asserts.isapprox(gca().XLim, [0.55 2.45], 1e-12);
close(f);
f = figure('Visible', 'off');
h = violinplot(zeros(0, 1));
asserts.istrue(isempty(h.EvaluationPoints));
asserts.istrue(isempty(h.DensityValues));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = violinplot([1 2 2 3 4]);
asserts.checkerror('h.EvaluationPoints = ''a'';', graphicsPropertyErrorMessage('EvaluationPoints', 'Nelson:graphics:floatingPointExpected'));
asserts.checkerror('h.EvaluationPoints = int8([1; 2]);', graphicsPropertyErrorMessage('EvaluationPoints', 'Nelson:graphics:floatingPointExpected'));
asserts.checkerror('h.EvaluationPoints = [1; NaN];', graphicsPropertyErrorMessage('EvaluationPoints', 'Nelson:expectedFinite', 'Value'));
asserts.isequal(h.EvaluationPointsMode, 'auto');
close(f);
asserts.checkerror('violinplot([1 2 2 3 4], ''EvaluationPoints'', (0:5)'');', getString(message('Nelson:violinplot:densityWithSampleData')), 'Nelson:violinplot:densityWithSampleData');
asserts.checkerror('violinplot(''EvaluationPoints'', (0:5)'');', getString(message('Nelson:notEnoughInputs')));
close all
%=============================================================================
asserts.checkerror('violinplot();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('violinplot({1});', getString(message('Nelson:validators:mustBeNumeric', 'Value')));
asserts.checkerror('violinplot([1 2], [1 2 3]);', getString(message('Nelson:graphics:invalidGroupData')));
asserts.checkerror('violinplot([1], ''BadName'', 1);', getString(message('Nelson:graphics:unsupportedProperty', 'violinplot')));
%=============================================================================
