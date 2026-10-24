%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
[f2, ax2, rendered] = create_animatedline_render_axes();
clipLine = animatedline(ax2, [-1 0.5 2], [0.5 0.7 0.5], 'Color', [0 0 0], 'LineWidth', 3);
clipLine.Clipping = 'on';
drawnow();
clippedFrame = getframe(f2).cdata;
clipLine.Clipping = 'off';
drawnow();
unclippedFrame = getframe(f2).cdata;
asserts.istrue(frame_delta(clippedFrame, unclippedFrame) > 0);
clipLine.AlignVertexCenters = 'on';
drawnow();
alignedFrame = getframe(f2).cdata;
asserts.isequal(char(clipLine.AlignVertexCenters), 'on');
asserts.istrue(frame_delta(unclippedFrame, alignedFrame) >= 0);
close(f2);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
