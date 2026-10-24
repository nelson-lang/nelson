%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
[f2, ax2, rendered] = create_animatedline_render_axes();
drawnow();
redFrame = getframe(f2).cdata;
rendered.Color = [0 0 1];
drawnow();
blueFrame = getframe(f2).cdata;
asserts.istrue(frame_delta(redFrame, blueFrame) > 0);
close(f2);
%=============================================================================
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
