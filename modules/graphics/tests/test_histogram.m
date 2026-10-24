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
h = histogram([1 1 2 3], [0 2 4]);
asserts.istrue(isgraphics(h, 'histogram'));
asserts.isequal(h.Type, 'histogram');
assert_required_properties(h, reference_histogram_properties());
asserts.isequal(h.BinEdges, [0 2 4]);
asserts.isequal(h.BinCounts, [2 2]);
asserts.isequal(h.Values, [2 2]);
asserts.isequal(h.BinLimits, [0 4]);
asserts.isequal(h.BinMethod, 'manual');
asserts.isequal(h.BinLimitsMode, 'manual');
asserts.isequal(h.FaceColor, 'auto');
asserts.isequal(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram([1 1 2 3], [0 2 4], 'Normalization', 'probability');
asserts.isapprox(h.Values, [0.5 0.5], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = histogram(ax, [1 1 2 3 4 5 5 6], [0 2 4 6], ...
  'Normalization', 'probability', 'FaceAlpha', 0.5);
drawnow();
asserts.isapprox(ax.XLim, [-0.3 6.3], 1e-12);
asserts.isapprox(ax.YLim, [0 0.5], 1e-12);
asserts.isapprox(ax.XTick, [0 1 2 3 4 5 6], 1e-12);
asserts.isapprox(ax.YTick, [0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5], 1e-12);
asserts.isapprox(h.Values, [0.25 0.25 0.5], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram([1 1 2 2 2 3 4 4 5]);
asserts.isequal(h.BinEdges, [0.5 1.5 2.5 3.5 4.5 5.5]);
asserts.isequal(h.BinCounts, [2 3 1 2 1]);
asserts.isequal(h.NumBins, 5);
asserts.isequal(h.BinWidth, 1);
asserts.isequal(h.BinLimits, [0.5 5.5]);
close(f);
%=============================================================================
% Auto BinMethod: integer rule only for a small data range (<= 50). At the
% boundary the integer rule still applies (one bin per integer value).
f = figure('Visible', 'off');
h = histogram(1:51);
asserts.isequal(h.NumBins, 51);
asserts.isequal(h.BinWidth, 1);
asserts.isequal(h.BinLimits, [0.5 51.5]);
asserts.isequal(h.BinMethod, 'auto');
asserts.isequal(h.BinLimitsMode, 'auto');
close(f);
%=============================================================================
% Beyond the integer range 'auto' switches to Scott's rule with a 'nice' bin
% width, giving a handful of wide bins instead of one bin per value.
f = figure('Visible', 'off');
h = histogram(1:52);
asserts.isequal(h.NumBins, 6);
asserts.isequal(h.BinWidth, 10);
asserts.isequal(h.BinEdges, [0 10 20 30 40 50 60]);
asserts.isequal(h.BinLimits, [0 60]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram(1:100);
asserts.isequal(h.NumBins, 5);
asserts.isequal(h.BinWidth, 20);
asserts.isequal(h.BinEdges, [0 20 40 60 80 100]);
asserts.isequal(h.BinLimits, [0 100]);
asserts.isequal(h.BinMethod, 'auto');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram(1:1000);
asserts.isequal(h.NumBins, 10);
asserts.isequal(h.BinWidth, 100);
asserts.isequal(h.BinLimits, [0 1000]);
close(f);
%=============================================================================
% Negative range crossing zero: 'nice' edges anchored on multiples of width.
f = figure('Visible', 'off');
h = histogram(-30:30);
asserts.isequal(h.NumBins, 4);
asserts.isequal(h.BinWidth, 20);
asserts.isequal(h.BinEdges, [-40 -20 0 20 40]);
close(f);
%=============================================================================
% Constant data: a single bin bracketing the value (nearly-constant rule).
f = figure('Visible', 'off');
h = histogram(7 * ones(1, 20));
asserts.isequal(h.NumBins, 1);
asserts.isequal(h.BinWidth, 1);
asserts.isequal(h.BinEdges, [6.5 7.5]);
asserts.isequal(h.BinCounts, 20);
close(f);
%=============================================================================
% Non-integer data uses Scott's rule regardless of range.
f = figure('Visible', 'off');
h = histogram([1.1 2.2 3.3 4.4]);
asserts.isequal(h.NumBins, 2);
asserts.isequal(h.BinWidth, 3);
asserts.isequal(h.BinEdges, [0 3 6]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram('BinEdges', [0 1 2 3], 'BinCounts', [3 4 5]);
asserts.isequal(h.BinCountsMode, 'manual');
asserts.isequal(h.BinCounts, [3 4 5]);
asserts.isequal(h.Values, [3 4 5]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram([1 2 3 4], 2, 'BinLimits', [0 4], 'Orientation', 'horizontal');
asserts.isequal(h.BinLimitsMode, 'manual');
asserts.isequal(h.Orientation, 'horizontal');
asserts.isequal(h.BinCounts, [1 3]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram([1 2 3 4], 'BinWidth', 2, 'DisplayStyle', 'stairs', 'LineWidth', 1.5);
asserts.isequal(h.BinEdges, [0 2 4]);
asserts.isequal(h.BinLimits, [0 4]);
asserts.isequal(h.BinCounts, [1 3]);
asserts.isequal(h.Values, [1 3]);
asserts.isequal(h.DisplayStyle, 'stairs');
asserts.isequal(h.LineWidth, 1.5);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histogram([1 2 3], [0 0 1]);
asserts.isequal(h.BinEdges, [0 0 1]);
asserts.isequal(h.Values, [0 1]);
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
    if ~strcmp(name, 'Values') && ~strcmp(name, 'Children') && ~strcmp(name, 'BeingDeleted') ...
        && ~strcmp(name, 'Parent') && ~strcmp(name, 'Type')
      set(h, name, value);
    end
  end
end
%=============================================================================
function names = reference_histogram_properties()
  names = {'Data'; 'BinCounts'; 'NumBins'; 'BinEdges'; 'BinWidth'; 'BinMethod';
    'BinLimits'; 'Normalization'; 'FaceColor'; 'EdgeColor'; 'LineWidth'; 'LineStyle';
    'BinCountsMode'; 'BinLimitsMode'; 'Orientation'; 'DisplayStyle'; 'FaceAlpha';
    'EdgeAlpha'; 'Values'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility';
    'DisplayName'; 'Annotation'; 'DataTipTemplate'; 'Selected'; 'SelectionHighlight';
    'HitTest'; 'PickableParts'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction';
    'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag';
    'UserData'; 'SeriesIndex'};
end
%=============================================================================
