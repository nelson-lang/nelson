%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = ip_fit_affine_transform(movingPoints, fixedPoints)
  if size(movingPoints, 1) < 3
    error('Nelson:image_processing:leastThreePointPairsRequiredAffine', 'At least three point pairs are required for an affine transform.');
  end
  x = movingPoints(:, 1);
  y = movingPoints(:, 2);
  u = fixedPoints(:, 1);
  v = fixedPoints(:, 2);
  A = [x y ones(size(x))];
  if rank(A) < 3
    error('Nelson:image_processing:controlPointsNotDefineUniqueTransformation', 'Control points do not define a unique transformation.');
  end
  paramsU = A \ u;
  paramsV = A \ v;
  T = [paramsU(1) paramsV(1) 0; paramsU(2) paramsV(2) 0; paramsU(3) paramsV(3) 1];
end
%=============================================================================
