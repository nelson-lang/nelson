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
p = piechart([1 2 3], 'FaceColor', [1 0 0], 'EdgeColor', [0 0 0], ...
  'LineWidth', 1, 'LegendVisible', 'off');
drawnow();
filledFrame = getframe(f).cdata;
asserts.istrue(any(filledFrame(:) < 245));
p.FaceColor = 'none';
p.EdgeColor = [0 0 0];
p.LineWidth = 3;
drawnow();
outlineFrame = getframe(f).cdata;
asserts.istrue(frame_delta(filledFrame, outlineFrame) > 0);
p.Visible = 'off';
drawnow();
hiddenFrame = getframe(f).cdata;
asserts.istrue(frame_delta(outlineFrame, hiddenFrame) > 0);
close(f);
close all
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
