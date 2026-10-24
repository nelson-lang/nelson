%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
close all
%=============================================================================
f = figure('Visible', 'off');
h = histogram2([1 1 2 3], [1 2 2 3], [0 2 4], [0 2 4]);
asserts.istrue(isgraphics(h, 'histogram2'));
asserts.isequal(h.Type, 'histogram2');
assert_required_properties(h, reference_histogram2_properties());
asserts.isequal(h.XBinEdges, [0 2 4]);
asserts.isequal(h.YBinEdges, [0 2 4]);
asserts.isequal(h.BinCounts, [1 1; 0 2]);
asserts.isequal(h.Values, [1 1; 0 2]);
asserts.isequal(h.NumBins, [2 2]);
asserts.isequal(h.BinWidth, [2 2]);
asserts.isequal(h.BinMethod, 'manual');
asserts.isequal(h.XBinLimits, [0 4]);
asserts.isequal(h.YBinLimits, [0 4]);
asserts.isequal(h.XBinLimitsMode, 'manual');
asserts.isequal(h.YBinLimitsMode, 'manual');
asserts.isequal(h.FaceColor, 'auto');
asserts.isequal(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882]);
asserts.isequal(h.FaceLighting, 'lit');
% Bars default to solid opaque faces with thin edges: FaceAlpha and EdgeAlpha
% are fully opaque so no bar reads through another, and the edge is a thin
% solid line.
asserts.isequal(h.FaceAlpha, 1);
asserts.isequal(h.EdgeAlpha, 1);
asserts.isequal(h.LineWidth, 0.5);
asserts.isequal(h.LineStyle, '-');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram2([1 1 2 3 4 4], [1 2 2 3 3 4], [0 2 4], [0 2 4]);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [0 4]);
asserts.isequal(ax.YLim, [0 4]);
asserts.isequal(ax.XTick, 0:1:4);
asserts.isequal(ax.YTick, 0:1:4);
asserts.isequal(ax.ZTick, 0:1:4);
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(char(ax.Box), 'on');
asserts.isapprox(ax.DataAspectRatio, [1 1 1], 1e-12);
asserts.isapprox(ax.CameraViewAngle, 10.339584907201973, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [-3.0291773414041456; 3.5783969397257605];
y = [-3.9463612282560554; 3.7280138578918525];
h = histogram2(x, y, [12 10], 'Normalization', 'probability');
ax = ancestor(h, 'axes');
asserts.isapprox(h.XBinEdges, ...
  [-3.5 -2.91 -2.32 -1.73 -1.14 -0.55 0.04 0.63 1.22 1.81 2.40 2.99 3.58], 1e-12);
asserts.isapprox(h.YBinEdges, ...
  [-4.2 -3.4 -2.6 -1.8 -1 -0.2 0.6 1.4 2.2 3 3.8], 1e-12);
asserts.isapprox(ax.XLim, [-3.5 3.58], 1e-12);
asserts.isapprox(ax.YLim, [-4.2 3.8], 1e-12);
asserts.isequal(ax.XTick, -2:2:2);
asserts.isequal(ax.YTick, -4:2:2);
asserts.isequal(ax.ZTick, 0:0.1:0.5);
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isapprox(ax.DataAspectRatio, [7.08 / 0.5 8 / 0.5 1], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram2([1 1 2 3], [1 2 2 3], [0 2 4], [0 2 4], ...
  'Normalization', 'probability');
asserts.isapprox(h.Values, [0.25 0.25; 0 0.5], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram2('XBinEdges', [0 1 2], 'YBinEdges', [0 1 2 3], ...
  'BinCounts', [1 2 3; 4 5 6]);
asserts.isequal(h.BinCountsMode, 'manual');
asserts.isequal(h.BinCounts, [1 2 3; 4 5 6]);
asserts.isequal(h.Values, [1 2 3; 4 5 6]);
asserts.isequal(h.NumBins, [2 3]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram2([1 2 3 4], [1 1 3 4], [0 2 4], [0 2 4], ...
  'DisplayStyle', 'tile', 'ShowEmptyBins', 'on', 'LineWidth', 1.5);
asserts.isequal(h.DisplayStyle, 'tile');
asserts.isequal(char(h.ShowEmptyBins), 'on');
asserts.isequal(h.LineWidth, 1.5);
asserts.isequal(h.FaceLighting, 'none');
ax = ancestor(h, 'axes');
asserts.isequal(ax.View, [0 90]);
asserts.isequal(ax.CLim, [0 2]);
didError = false;
try
  h.FaceLighting = 'flat';
catch
  didError = true;
end
asserts.istrue(didError);
h.DisplayStyle = 'bar3';
asserts.isequal(h.FaceLighting, 'lit');
h.FaceLighting = 'none';
asserts.isequal(h.FaceLighting, 'none');
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
asserts.checkerror('histogram2([1 2], [1])', getString(message('Nelson:dimagree:valueValueNumberElements', 'X', 'Y')));
f = figure('Visible', 'off');
h = histogram2([1 2], [1 2], [0 0 1], [0 1]);
asserts.isequal(h.XBinEdges, [0 0 1]);
asserts.isequal(h.YBinEdges, [0 1]);
asserts.isequal(h.Values, [0; 1]);
close(f);
%=============================================================================
% Automatic two-dimensional binning (BinMethod 'auto'): a coarse 'nice' bin
% width per dimension (bivariate Scott's rule), not one bin per integer.
f = figure('Visible', 'off');
h = histogram2(cos(1:500)', sin(1:500)');
asserts.isequal(h.NumBins, [4 4]);
asserts.isequal(h.BinWidth, [0.5 0.5]);
asserts.isequal(h.XBinEdges, [-1 -0.5 0 0.5 1]);
asserts.isequal(h.YBinEdges, [-1 -0.5 0 0.5 1]);
asserts.isequal(h.XBinLimits, [-1 1]);
asserts.isequal(h.YBinLimits, [-1 1]);
asserts.isequal(h.BinMethod, 'auto');
asserts.isequal(max(h.Values(:)), 44);
close(f);
%=============================================================================
% Integer data with a large range: Scott's rule, not one bin per integer.
f = figure('Visible', 'off');
h = histogram2((1:100)', (1:100)');
asserts.isequal(h.NumBins, [4 4]);
asserts.isequal(h.BinWidth, [30 30]);
asserts.isequal(h.XBinLimits, [0 120]);
asserts.isequal(h.YBinLimits, [0 120]);
close(f);
%=============================================================================
% Integer data at the small-range threshold (range <= 50): integer rule.
f = figure('Visible', 'off');
h = histogram2((1:51)', (1:51)');
asserts.isequal(h.NumBins, [51 51]);
asserts.isequal(h.BinWidth, [1 1]);
asserts.isequal(h.XBinLimits, [0.5 51.5]);
close(f);
%=============================================================================
% Just past the threshold: switches to Scott's rule.
f = figure('Visible', 'off');
h = histogram2((1:55)', (1:55)');
asserts.isequal(h.NumBins, [3 3]);
asserts.isequal(h.BinWidth, [20 20]);
asserts.isequal(h.XBinLimits, [0 60]);
close(f);
%=============================================================================
% Different spread per dimension yields a different bin width per dimension.
f = figure('Visible', 'off');
h = histogram2(linspace(-1, 1, 500)', linspace(0, 10, 500)');
asserts.isequal(h.NumBins, [4 5]);
asserts.isequal(h.BinWidth, [0.5 2]);
asserts.isequal(h.XBinLimits, [-1 1]);
asserts.isequal(h.YBinLimits, [0 10]);
close(f);
%=============================================================================
% The rule is applied independently per dimension: integer rule on the integer
% column, Scott's rule on the continuous column.
f = figure('Visible', 'off');
xMixed = mod((0:299), 10)' + 1;
h = histogram2(xMixed, cos(1:300)');
asserts.isequal(h.NumBins, [10 4]);
asserts.isequal(h.BinWidth, [1 0.5]);
asserts.isequal(h.XBinLimits, [0.5 10.5]);
asserts.isequal(h.YBinLimits, [-1 1]);
close(f);
%=============================================================================
% Explicit BinMethod values.
f = figure('Visible', 'off');
h = histogram2(cos(1:500)', sin(1:500)', 'BinMethod', 'scott');
asserts.isequal(h.NumBins, [4 4]);
asserts.isequal(h.BinWidth, [0.5 0.5]);
h = histogram2(cos(1:500)', sin(1:500)', 'BinMethod', 'integers');
asserts.isequal(h.NumBins, [3 3]);
asserts.isequal(h.BinWidth, [1 1]);
asserts.isequal(h.XBinLimits, [-1.5 1.5]);
close(f);
%=============================================================================
close all
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
  for k = 1:numel(required)
    name = required{k};
    value = get(h, name);
    if ~ismember(name, readonly_histogram2_properties())
      set(h, name, value);
    end
  end
end
%=============================================================================
function names = readonly_histogram2_properties()
  names = {'Values'; 'Children'; 'BeingDeleted'; 'Parent'; 'Type'; ...
    'Annotation'; 'DataTipTemplate'};
end
%=============================================================================
function names = reference_histogram2_properties()
  names = {'Data'; 'BinCounts'; 'NumBins'; 'XBinEdges'; 'YBinEdges'; ...
    'BinWidth'; 'BinMethod'; 'XBinLimits'; 'YBinLimits'; 'Normalization'; ...
    'FaceColor'; 'EdgeColor'; 'LineWidth'; 'LineStyle'; 'FaceLighting'; ...
    'BinCountsMode'; 'XBinLimitsMode'; 'YBinLimitsMode'; 'DisplayStyle'; ...
    'FaceAlpha'; 'EdgeAlpha'; 'ShowEmptyBins'; 'Values'; 'Children'; ...
    'Parent'; 'Visible'; 'HandleVisibility'; 'DisplayName'; 'Annotation'; ...
    'DataTipTemplate'; 'Selected'; 'SelectionHighlight'; 'HitTest'; ...
    'PickableParts'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; ...
    'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; ...
    'Tag'; 'UserData'; 'SeriesIndex'};
end
%=============================================================================
