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
f = figure('Position', [120 120 640 480], 'Color', [1 1 1]);
ax = axes('Parent', f);
patch('Parent', ax, ...
  'Faces', [1 2 3; 1 3 4; 1 4 5; 1 5 2], ...
  'Vertices', [0 0 1; -1 -1 0; 1 -1 0; 1 1 0; -1 1 0], ...
  'FaceColor', 'none', 'EdgeColor', [0 0.45 0.8], 'LineWidth', 2);
axis(ax, [-1.2 1.2 -1.2 1.2 0 1.2]);
axis(ax, 'square');
for angle = [20 55 90 125]
  view(ax, [angle 25]);
  drawnow();
end
frame = assert_opengl_patch_frame(f);
coloredPixels = frame(:, :, 3) > 100 & frame(:, :, 1) < 180 & frame(:, :, 2) < 180;
asserts.istrue(nnz(coloredPixels) > numel(coloredPixels) * 0.001);
close(f);
%=============================================================================
