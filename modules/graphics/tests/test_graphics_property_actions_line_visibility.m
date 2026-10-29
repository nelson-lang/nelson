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
s = scatter(ax, 1:5, [1 3 2 4 2], 80, [1 0 0], 'filled');
drawnow();
smallFrame = getframe(f).cdata;
s.SizeData = 260;
s.CData = [0 0 1];
drawnow();
largeFrame = getframe(f).cdata;
asserts.istrue(frame_delta(smallFrame, largeFrame) > 0);
asserts.isequal(s.SizeData, 260);
asserts.isequal(s.CDataMode, 'manual');
close(f);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
