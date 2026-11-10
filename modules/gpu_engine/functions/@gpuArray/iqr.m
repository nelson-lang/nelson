%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = iqr(X, varargin)
  % distance between the upper and lower quartiles on the device; other forms run on the host
  ok = false;
  if nargin == 1 || (nargin == 2 && isnumeric(varargin{1}) && isscalar(varargin{1}) && ~isgpuarray(varargin{1}))
    dim = [];
    if nargin == 2
      dim = varargin{1};
    elseif ~isempty(X)
      dim = find(size(X) ~= 1, 1);
      if isempty(dim)
        dim = 1;
      end
    end
    [Q, ok] = __gpuArray_quantile__(X, [0.25 0.75], dim);
    if ok
      Y = diff(Q, 1, dim);
    end
  end
  if ~ok
    Y = __gpuArray_host__('iqr', X, varargin{:});
  end
end
%=============================================================================
