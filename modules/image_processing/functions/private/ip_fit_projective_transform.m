%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = ip_fit_projective_transform(movingPoints, fixedPoints)
  n = size(movingPoints, 1);
  if n < 4
    error('Nelson:image_processing:leastFourPointPairsRequiredProjective', 'At least four point pairs are required for a projective transform.');
  end
  x = movingPoints(:, 1);
  y = movingPoints(:, 2);
  u = fixedPoints(:, 1);
  v = fixedPoints(:, 2);
  A = zeros(2 * n, 8);
  b = zeros(2 * n, 1);
  A(1:2:end, :) = [x zeros(n, 1) -u .* x y zeros(n, 1) -u .* y ones(n, 1) zeros(n, 1)];
  A(2:2:end, :) = [zeros(n, 1) x -v .* x zeros(n, 1) y -v .* y zeros(n, 1) ones(n, 1)];
  b(1:2:end) = u;
  b(2:2:end) = v;
  if rank(A) < 8
    error('Nelson:image_processing:controlPointsNotDefineUniqueTransformation', 'Control points do not define a unique transformation.');
  end
  p = A \ b;
  T = [p(1) p(2) p(3); p(4) p(5) p(6); p(7) p(8) 1];
end
%=============================================================================
