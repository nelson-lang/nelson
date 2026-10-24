%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
close all
f = figure('Visible', 'off', 'Units', 'pixels', 'InnerPosition', [100 100 360 280], ...
  'Color', [1 1 1]);
d = donutchart([4 3 2], {'A', 'B', 'C'}, 'CenterLabel', '9', ...
  'InnerRadius', 0.55, 'FaceColor', [0.1 0.5 0.8], 'EdgeColor', [0 0 0]);
drawnow();
filledFrame = getframe(f).cdata;
asserts.istrue(any(filledFrame(:) < 245));
d.InnerRadius = 0.2;
d.CenterLabel = 'sum';
drawnow();
smallerHoleFrame = getframe(f).cdata;
asserts.istrue(frame_delta(filledFrame, smallerHoleFrame) > 0);
d.Visible = 'off';
drawnow();
hiddenFrame = getframe(f).cdata;
asserts.istrue(frame_delta(smallerHoleFrame, hiddenFrame) > 0);
close(f);
close all
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
