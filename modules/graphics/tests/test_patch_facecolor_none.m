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
f = figure('Visible', 'off', 'Color', [1 1 1], 'Position', [100 100 240 240]);
ax = axes('Parent', f);
p = patch('Parent', ax, 'XData', [0 1 0], 'YData', [0 0 1], ...
  'FaceColor', 'none', 'EdgeColor', 'green', 'LineWidth', 2);
axis(ax, 'off');
axis(ax, [0 1 0 1]);
drawnow();
F = getframe(ax);
blackPixels = F.cdata(:, :, 1) < 8 & F.cdata(:, :, 2) < 8 & F.cdata(:, :, 3) < 8;
asserts.isequal(nnz(blackPixels), 0);
asserts.isequal(p.FaceColor, 'none');
