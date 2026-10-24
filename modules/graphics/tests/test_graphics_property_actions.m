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
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 320 240], 'Color', [1 1 1]);
ax = axes('Parent', f, 'Position', [0.15 0.15 0.75 0.75]);
h = line('Parent', ax, 'XData', [0 1], 'YData', [0 1], 'Color', [1 0 0], 'LineWidth', 2);
drawnow();
redFrame = getframe(f).cdata;
h.Color = [0 0 1];
drawnow();
blueFrame = getframe(f).cdata;
asserts.istrue(frame_delta(redFrame, blueFrame) > 0);
h.Visible = 'off';
drawnow();
hiddenFrame = getframe(f).cdata;
asserts.istrue(frame_delta(blueFrame, hiddenFrame) > 0);
h.Visible = 'on';
h.XData = [0 1 2];
h.YData = [0 1 0];
asserts.isequal(h.XData, [0 1 2]);
asserts.isequal(h.YData, [0 1 0]);
asserts.isequal(h.XDataMode, 'manual');
close(f);
%=============================================================================
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
