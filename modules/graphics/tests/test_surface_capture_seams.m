%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A captured flat surface shows no seam along its face edges: every pixel inside
% the silhouette keeps the face colour, with antialiasing on and off.
%=============================================================================
for smoothing = {'on', 'off'}
  f = figure('Visible', 'off', 'Position', [100 100 800 600], 'Color', [1 1 1], 'GraphicsSmoothing', smoothing{1});
  ax = axes(f);
  surf(ax, peaks(20), ones(20), 'EdgeColor', 'none');
  colormap(ax, [0.5 0.5 0.5]);
  axis(ax, 'off');
  drawnow;
  fr = getframe(f);
  img = double(fr.cdata);
  grey = all(abs(img - 128) <= 2, 3);
  covered = ~all(img >= 250, 3);
  % stay away from the antialiased silhouette: erode the covered area by 3 pixels
  inside = covered;
  for dx = -3:3
    for dy = -3:3
      inside = inside & circshift(covered, [dy dx]);
    end
  end
  assert_istrue(nnz(inside) > 50000);
  assert_isequal(nnz(inside & ~grey), 0);
  close(f);
end
%=============================================================================
