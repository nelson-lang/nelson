%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Apply a geometric transformation matrix M to points, using the row-vector
% convention [x ... 1] = [u ... 1] * M. coords is a cell array holding either a
% single packed N-by-dim matrix or dim separate coordinate arrays of equal size.
% Returns a cell array with one packed matrix or dim reshaped coordinate arrays.
%=============================================================================
function out = ip_transform_points(M, dim, isProjective, coords)
  nCoord = numel(coords);
  if nCoord == 1
    P = coords{1};
    if ~isnumeric(P) || ~isreal(P)
      error('Nelson:image_processing:pointsRealNumericArray', 'Points must be a real numeric array.');
    end
    if ~ismatrix(P) || size(P, 2) ~= dim
      error('Nelson:image_processing:packedPointsMatrixOneColumnPer', 'Packed points must be a matrix with one column per dimension.');
    end
    out = {ip_transform_points_apply(double(P), M, dim, isProjective)};
  elseif nCoord == dim
    sz = size(coords{1});
    n = numel(coords{1});
    P = zeros(n, dim);
    for k = 1:dim
      c = coords{k};
      if ~isnumeric(c) || ~isreal(c)
        error('Nelson:image_processing:pointCoordinatesRealNumericArrays', 'Point coordinates must be real numeric arrays.');
      end
      if ~isequal(size(c), sz)
        error('Nelson:image_processing:pointCoordinateArraysAllSameSize', 'Point coordinate arrays must all be the same size.');
      end
      P(:, k) = double(c(:));
    end
    R = ip_transform_points_apply(P, M, dim, isProjective);
    out = cell(1, dim);
    for k = 1:dim
      out{k} = reshape(R(:, k), sz);
    end
  else
    error('Nelson:image_processing:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function R = ip_transform_points_apply(P, M, dim, isProjective)
  H = [P, ones(size(P, 1), 1)] * M;
  if isProjective
    R = H(:, 1:dim) ./ H(:, dim + 1);
  else
    R = H(:, 1:dim);
  end
end
%=============================================================================
