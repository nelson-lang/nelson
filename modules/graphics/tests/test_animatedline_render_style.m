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
rendered.Visible = 'off';
drawnow();
hiddenFrame = getframe(f2).cdata;
rendered.Visible = 'on';
rendered.LineStyle = ':';
rendered.LineWidth = 8;
rendered.Marker = 'o';
rendered.MarkerSize = 14;
rendered.MarkerFaceColor = [0 1 0];
drawnow();
styledFrame = getframe(f2).cdata;
asserts.istrue(frame_delta(hiddenFrame, styledFrame) > 0);
close(f2);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
