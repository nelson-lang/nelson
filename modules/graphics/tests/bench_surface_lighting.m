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
% Animated two-sided gouraud lighting on a large surface: measures the
% per-vertex lighting cost as the geometry changes each frame.
tic()
f = figure('Visible', 'off', 'Position', [100 100 640 480], 'Color', [1 1 1]);
[x, y, z] = sphere(80);
ax = axes('Parent', f);
s = surf(ax, x, y, z, 'EdgeColor', 'none', 'FaceColor', [0.8 0.5 0.2], ...
  'FaceLighting', 'gouraud');
light(ax, 'Position', [-3 -4 6], 'Style', 'local');
material(ax, 'shiny');
axis(ax, 'equal');
axis(ax, 'off');
for k = 1:30
  a = k * pi / 30;
  set(s, 'ZData', z + 0.15 * sin(4 * a) * x);
  drawnow();
end
close(f);
toc()
%=============================================================================
