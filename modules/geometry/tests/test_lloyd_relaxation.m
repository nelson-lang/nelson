%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
lloydFigureVisible = 'off';
lloydAnimationEnabled = true;
lloydAnimationFrames = 40;
lloydFramePause = 0;
lloydAnimationContinuous = false;
lloydPointCount = 48;
run([modulepath('geometry', 'root'), '/examples/lloyd_relaxation.m']);

assert_istrue(isgraphics(lloydFigure));
assert_isequal(size(lloydSeeds), [lloydPointCount 2]);
% Every seed stays inside the unit square it is relaxed within.
assert_istrue(all(lloydSeeds(:) >= 0 & lloydSeeds(:) <= 1));
% Relaxation spreads the seeds out: no two sit on top of each other.
pairwise = min(pdist2Compat(lloydSeeds));
assert_istrue(min(pairwise) > 0);
delete(lloydFigure);
drawnow();
%=============================================================================
function distances = pdist2Compat(points)
  count = size(points, 1);
  distances = inf(count, count);
  for i = 1:count
    for j = 1:count
      if i ~= j
        distances(i, j) = norm(points(i, :) - points(j, :));
      end
    end
  end
end
%=============================================================================
