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
close all
f = figure('Visible', 'off', 'Color', [1 1 1], 'Position', [100 100 240 240]);
ax = axes('Parent', f);
colormap(ax, [1 0 0; 0 1 0; 0 0 1; 1 1 0]);
clim(ax, [1 4]);
patch('Parent', ax, 'Faces', [1 2 3 4], ...
  'Vertices', [0 0 0; 1 0 0; 1 1 0; 0 1 0], ...
  'FaceVertexCData', 2, 'FaceColor', 'flat', 'EdgeColor', 'none');
axis(ax, [0 1 0 1]);
axis(ax, 'off');
view(ax, [0 0 1]);
drawnow();
F = getframe(ax);
greenPixels = F.cdata(:, :, 1) < 40 & F.cdata(:, :, 2) > 210 & F.cdata(:, :, 3) < 40;
asserts.istrue(nnz(greenPixels) > numel(greenPixels) * 0.1);
%=============================================================================
