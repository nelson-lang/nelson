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
% A filled scatter marker's on-screen area depends only on its shape and on the
% SizeData: for a given SizeData each marker shape covers a fixed fraction of the
% circle's area. These fractions are fixed expected values (the square is
% smaller than its bounding box, the diamond is a tall rhombus, the triangles are
% equilateral, the stars are regular). The check is the ratio of each shape's
% filled area to the circle's, so it is independent of figure size and device
% pixel ratio.
%=============================================================================
close all;
%=============================================================================
markers = {'o', 's', 'd', '^', 'v', '<', '>', 'p', 'h'};
% Expected filled-area / circle-filled-area for each marker shape.
expected = [1.0000, 0.7696, 1.0329, 0.7371, 0.7373, 0.7361, 0.7365, 0.3569, 0.5524];
tol = 0.06;
%=============================================================================
areas = zeros(1, numel(markers));
for k = 1:numel(markers)
  areas(k) = marker_filled_area(markers{k});
end
ratios = areas ./ areas(1);
for k = 1:numel(markers)
  assert_istrue(abs(ratios(k) - expected(k)) < tol);
end
%=============================================================================
function a = marker_filled_area(mk)
  f = figure('Visible', 'off', 'Position', [100 100 400 400], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Position', [0.1 0.1 0.8 0.8]);
  axis(ax, [0 1 0 1]);
  hold(ax, 'on');
  scatter(ax, 0.5, 0.5, 3600, 'filled', 'Marker', mk, ...
    'MarkerFaceColor', [1 0 0], 'MarkerEdgeColor', 'none');
  drawnow();
  frame = getframe(f);
  close(f);
  image = frame.cdata;
  red = (image(:, :, 1) > 150) & (image(:, :, 2) < 100) & (image(:, :, 3) < 100);
  a = sum(red(:));
  assert_istrue(a > 0);
end
%=============================================================================
