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
[f3, markerLine, tail, clipLine, sceneFrame, x] = create_animatedline_render_scene();
markerLine.Marker = 'none';
drawnow();
withoutMarkersFrame = getframe(f3).cdata;
asserts.istrue(frame_delta(sceneFrame, withoutMarkersFrame) > 0);
close(f3);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
