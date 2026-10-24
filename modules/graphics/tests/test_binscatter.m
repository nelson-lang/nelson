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
x = [0.1 0.2 0.8 1.2 1.8 1.9];
y = [0.1 0.9 0.8 1.2 1.1 1.9];
f = figure('Visible', 'off');
h = binscatter(x, y, [2 2], 'XLimits', [0 2], 'YLimits', [0 2]);
asserts.istrue(isgraphics(h, 'binscatter'));
asserts.isequal(h.Type, 'binscatter');
assert_required_properties(h, reference_binscatter_properties());
asserts.isequal(h.NumBins, [2 2]);
asserts.isequal(h.NumBinsMode, 'manual');
asserts.isequal(h.XLimits, [0 2]);
asserts.isequal(h.XLimitsMode, 'manual');
asserts.isequal(h.YLimits, [0 2]);
asserts.isequal(h.YLimitsMode, 'manual');
asserts.isequal(h.XBinEdges, [0 1 2]);
asserts.isequal(h.YBinEdges, [0 1 2]);
asserts.isequal(h.Values, [3 0; 0 3]);
asserts.isequal(sum(h.Values(:)), 6);
ax = ancestor(h, 'axes');
asserts.isequal(ax.YDir, 'normal');
asserts.isequal(char(ax.Box), 'on');
asserts.isequal(ax.ZLimMode, 'auto');
asserts.isequal(ax.ZLim, [-1 1]);
asserts.isequal(ax.ZTickMode, 'auto');
asserts.isequal(ax.ZTick, [-1 0 1]);
asserts.isequal(ax.CLim, [0 3]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = binscatter(ax, x, y, 'NumBins', [2 2], 'ShowEmptyBins', 'on', 'FaceAlpha', 0.5);
asserts.isequal(h.Parent, ax);
asserts.isequal(char(h.ShowEmptyBins), 'on');
asserts.isequal(h.FaceAlpha, 0.5);
set(h, 'XLimits', [0 2], 'YLimits', [0 2]);
asserts.isequal(h.XLimitsMode, 'manual');
asserts.isequal(h.YLimitsMode, 'manual');
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x2 = randn(1000, 1);
y2 = randn(1000, 1);
h = binscatter(x2, y2, [15 10]);
asserts.istrue(isgraphics(h, 'binscatter'));
asserts.isequal(size(h.Values), [15 10]);
asserts.isequal(sum(h.Values(:)), 1000);
close(f);
%=============================================================================
f = figure('Visible', 'off');
xz = randn(2000, 1);
yz = 2 * xz + randn(2000, 1);
h = binscatter(xz, yz, [12 10]);
initialSize = size(h.Values);
ax = ancestor(h, 'axes');
initialXLim = ax.XLim;
initialYLim = ax.YLim;
zoom(f, 2);
drawnow();
zoomSize = size(h.Values);
asserts.istrue(diff(ax.XLim) < diff(initialXLim));
asserts.istrue(diff(ax.YLim) < diff(initialYLim));
asserts.istrue(zoomSize(1) > initialSize(1));
asserts.istrue(zoomSize(2) > initialSize(2));
asserts.isequal(ax.CLim(2), max(h.Values(:)));
xlim(ax, [-0.5 0.5]);
ylim(ax, [-1.5 1.5]);
drawnow();
manualZoomSize = size(h.Values);
asserts.istrue(manualZoomSize(1) > initialSize(1));
asserts.istrue(manualZoomSize(2) > initialSize(2));
close(f);
%=============================================================================
f = figure('Visible', 'off');
x3 = [1 2 NaN Inf 3];
y3 = [1 2 3 4 3];
h = binscatter(x3, y3);
asserts.isequal(h.NumBins, [1 1]);
asserts.isequal(sum(h.Values(:)), 3);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = binscatter([1 2 3], [1 2 3]);
asserts.throws('set(h, ''Values'', 1);', 'Values', 'Nelson:graphics:propertyError');
asserts.throws('set(h, ''XBinEdges'', [0 1]);', 'XBinEdges', 'Nelson:graphics:propertyError');
asserts.throws('set(h, ''YBinEdges'', [0 1]);', 'YBinEdges', 'Nelson:graphics:propertyError');
close(f);
%=============================================================================
asserts.throws('binscatter(1);', 'argument', 'Nelson:narginchk:notEnoughInputs');
asserts.throws('binscatter([1 2], [1 2 3]);', 'X', 'Nelson:dimagree:valueValueNumberElements');
asserts.throws('binscatter([1 2], [1 2], ''NumBins'', 0);', 'NumBins', 'Nelson:graphics:invalidNumBins');
asserts.throws('binscatter([1 2], [1 2], ''NumBins'', [2 2], ''NumBinsMode'', ''auto'');', ...
  'NumBins', 'Nelson:binscatter:numBinsCannotBeSpecified');
asserts.throws('binscatter([1 2], [1 2], ''XLimitsMode'', ''manual'');', ...
  'XLimits', 'Nelson:binscatter:invalidXLimits');
asserts.throws('binscatter([1 2], [1 2], ''XBinEdges'', [0 1]);', ...
  'binscatter', 'Nelson:graphics:unsupportedProperty');
%=============================================================================
close all
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
  for k = 1:numel(required)
    name = required{k};
    value = get(h, name);
    if ~ismember(name, readonly_binscatter_properties())
      set(h, name, value);
    end
  end
end
%=============================================================================
function names = readonly_binscatter_properties()
  names = {'Values'; 'XBinEdges'; 'YBinEdges'; 'Children'; 'BeingDeleted'; ...
    'Parent'; 'Type'; 'Annotation'; 'DataTipTemplate'};
end
%=============================================================================
function names = reference_binscatter_properties()
  names = {'XData'; 'YData'; 'NumBins'; 'NumBinsMode'; 'ShowEmptyBins'; ...
    'XBinEdges'; 'YBinEdges'; 'XLimits'; 'XLimitsMode'; 'YLimits'; ...
    'YLimitsMode'; 'Values'; 'FaceAlpha'; 'Children'; 'Parent'; ...
    'Visible'; 'HandleVisibility'; 'DisplayName'; 'Annotation'; ...
    'DataTipTemplate'; 'Selected'; 'SelectionHighlight'; 'HitTest'; ...
    'PickableParts'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; ...
    'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; ...
    'Tag'; 'UserData'};
end
%=============================================================================
