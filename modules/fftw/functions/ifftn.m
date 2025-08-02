%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = ifftn(varargin)
  narginchk(1, 2);
  X = varargin{1};
  if nargin() == 2
    dims = varargin{2};
  else
    dims = size(X);
  end
  dims = dims(:)';
  if (numel(dims) < ndims(X))
    points = size(X);
    dims = [dims, points((numel(dims) + 1):end)];
end
Y = X;
if ~issingle(Y)
  Y = double(Y);
end
if (numel(Y) ~= 0)
  for n=1:numel(dims)
    Y = ifft(Y, dims(n), n);
  end
end
end
%=============================================================================
