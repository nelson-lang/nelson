%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = pagelsqminnorm(A, B, varargin)
  % minimum norm least-squares solution of every page with expansion of the page dimensions
  narginchk(2, 4);
  [Ar, Br, aMap, bMap, pageSize] = pageBroadcast(A, B);
  if size(Ar, 1) ~= size(Br, 1)
    error('Nelson:linalg:pageRowsMismatch', 'The two inputs must have the same number of rows.');
  end
  count = numel(aMap);
  proto = zeros(1, 'like', A) + zeros(1, 'like', B);
  X = zeros([size(Ar, 2), size(Br, 2), count], 'like', proto);
  for k = 1:count
    X(:, :, k) = lsqminnorm(Ar(:, :, aMap(k)), Br(:, :, bMap(k)), varargin{:});
  end
  X = reshape(X, [size(Ar, 2), size(Br, 2), pageSize]);
end
%=============================================================================
