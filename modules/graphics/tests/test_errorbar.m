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
h = errorbar([1 2 3], [2 4 3], [0.2 0.3 0.4], [0.5 0.6 0.7]);
asserts.istrue(isgraphics(h, 'errorbar'));
asserts.isequal(h.Type, 'errorbar');
assert_required_properties(h, reference_errorbar_properties());
asserts.isequal(h.XData, [1 2 3]);
asserts.isequal(h.YData, [2 4 3]);
asserts.isapprox(h.YNegativeDelta, [0.2 0.3 0.4], 1e-12);
asserts.isapprox(h.YPositiveDelta, [0.5 0.6 0.7], 1e-12);
asserts.istrue(isempty(h.XNegativeDelta));
asserts.istrue(isempty(h.XPositiveDelta));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar([2 4 3], [0.1 0.2 0.3], '-or', 'LineWidth', 1.5);
asserts.isequal(h.XData, [1 2 3]);
asserts.isequal(h.XDataMode, 'auto');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineWidth, 1.5);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar([1 2 3], [2 4 3], [0.1 0.2 0.3]);
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.XDataMode, 'manual');
asserts.isapprox(h.YNegativeDelta, [0.1 0.2 0.3], 1e-12);
asserts.istrue(isempty(h.XNegativeDelta));
close(f);
%=============================================================================
f = figure('Visible', 'off');
Y = [1 2; 3 4; 5 6];
E = [0.1 0.2; 0.3 0.4; 0.5 0.6];
h = errorbar(Y, E);
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).XData, [1 2 3]);
asserts.isequal(h(1).YData, [1 3 5]);
asserts.isequal(h(2).YData, [2 4 6]);
asserts.isapprox(h(2).YNegativeDelta, [0.2 0.4 0.6], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar(1:3, [2 4 3], [0.1 0.2 0.3], 'horizontal');
asserts.isapprox(h.XNegativeDelta, [0.1 0.2 0.3], 1e-12);
asserts.istrue(isempty(h.YNegativeDelta));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar(1:3, [2 4 3], [0.1 0.2 0.3], 'both', '-or');
asserts.isapprox(h.XNegativeDelta, [0.1 0.2 0.3], 1e-12);
asserts.isapprox(h.YNegativeDelta, [0.1 0.2 0.3], 1e-12);
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineStyle, '-');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar(1:3, [2 4 3], [0.1 0.2 0.3], 'both', 'o');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.LineStyle, 'none');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar([1 2 3], [2 4 3], [0.2 0.2 0.2], [0.4 0.4 0.4], ...
  [0.1 0.2 0.3], [0.3 0.2 0.1]);
asserts.isequal(size(h), [1 1]);
asserts.isapprox(h.XNegativeDelta, [0.1 0.2 0.3], 1e-12);
asserts.isapprox(h.XPositiveDelta, [0.3 0.2 0.1], 1e-12);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = errorbar(1:3, [2 4 3], 0.2);
asserts.isequal(size(h), [1 3]);
asserts.isequal(h(1).XData, 1);
asserts.isequal(h(2).YData, 4);
asserts.isequal(h(3).YNegativeDelta, 0.2);
close(f);
%=============================================================================
asserts.istrue(isValidGraphicsProperty('errorbar', 'CapSize'));
asserts.checkerror('errorbar([1 2], [1], [0.1 0.2])', getString(message('Nelson:dimagree:valueValueNumberElements', 'XData', 'YData')));
%=============================================================================
close all
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
  for k = 1:numel(required)
    name = required{k};
    value = get(h, name);
    if ~ismember(name, readonly_errorbar_properties())
      set(h, name, value);
    end
  end
end
%=============================================================================
function names = readonly_errorbar_properties()
  names = {'Annotation'; 'BeingDeleted'; 'Children'; 'DataTipTemplate'; 'Parent'; 'Type'};
end
%=============================================================================
function names = reference_errorbar_properties()
  names = {'AlignVertexCenters'; 'CapSize'; 'Clipping'; 'Color'; 'ColorMode';
    'LineWidth'; 'LineStyle'; 'LineStyleMode'; 'Marker'; 'MarkerMode';
    'MarkerEdgeColor'; 'MarkerFaceColor'; 'MarkerSize'; 'XNegativeDelta';
    'XNegativeDeltaSource'; 'XPositiveDelta'; 'XPositiveDeltaSource';
    'YNegativeDelta'; 'YNegativeDeltaSource'; 'YPositiveDelta';
    'YPositiveDeltaSource'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility';
    'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible';
    'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected';
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'DisplayName'; 'Annotation';
    'SeriesIndex'; 'XData'; 'XDataMode'; 'XDataSource'; 'YData'; 'YDataSource';
    'DataTipTemplate'};
end
%=============================================================================
