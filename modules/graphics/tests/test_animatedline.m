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
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
an = animatedline(ax);
asserts.istrue(isgraphics(an, 'animatedline'));
asserts.isequal(an.Type, 'animatedline');
asserts.isequal(an.MaximumNumPoints, 1000000);
asserts.isapprox(an.Color, [33 33 33] / 255, 1e-12);
asserts.isequal(an.ColorMode, 'auto');
asserts.isequal(an.LineStyle, '-');
asserts.isequal(an.LineStyleMode, 'auto');
asserts.isequal(an.Marker, 'none');
asserts.isequal(an.MarkerMode, 'auto');
asserts.isequal(an.MarkerEdgeColor, 'auto');
asserts.isequal(an.MarkerFaceColor, 'none');
asserts.isequal(an.SeriesIndex, 'none');
assert_required_properties(an, reference_animatedline_properties());
[x, y, z] = getpoints(an);
asserts.istrue(isempty(x));
asserts.istrue(isempty(y));
asserts.istrue(isempty(z));
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
  asserts.isfalse(any(strcmp(p, 'XData')));
  asserts.isfalse(any(strcmp(p, 'YData')));
  asserts.isfalse(any(strcmp(p, 'ZData')));
end
%=============================================================================
function names = reference_animatedline_properties()
  names = {'MaximumNumPoints'; 'Color'; 'ColorMode'; 'LineStyle'; 'LineStyleMode';
    'LineWidth'; 'AlignVertexCenters'; 'Marker'; 'MarkerMode'; 'MarkerSize';
    'MarkerEdgeColor'; 'MarkerFaceColor'; 'Clipping'; 'Children'; 'Parent'; 'Visible';
    'HandleVisibility'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted';
    'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected';
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'DisplayName'; 'Annotation';
    'SeriesIndex'};
end
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
