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
f = figure('Visible', 'off', 'InnerPosition', [100 100 320 240], 'Color', [1 1 1]);
ax = axes('Parent', f, 'Position', [0.15 0.15 0.75 0.75]);
p = patch('Parent', ax, 'XData', [0 1 1 0], 'YData', [0 0 1 1], 'FaceColor', [1 0 0]);
drawnow();
opaqueFrame = getframe(f).cdata;
p.FaceAlpha = 0.2;
p.EdgeColor = [0 0 1];
drawnow();
transparentFrame = getframe(f).cdata;
asserts.istrue(frame_delta(opaqueFrame, transparentFrame) > 0);
asserts.isequal(p.FaceAlpha, 0.2);
asserts.isequal(p.EdgeColor, [0 0 1]);
close(f);
%=============================================================================
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
