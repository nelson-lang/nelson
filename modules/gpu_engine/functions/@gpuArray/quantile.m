%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = quantile(X, p, varargin)
  % quantiles on the device for data without NaN; other forms run on the host
  ok = false;
  if nargin == 2 || (nargin == 3 && isnumeric(varargin{1}) && isscalar(varargin{1}) && ~isgpuarray(varargin{1}))
    dim = [];
    if nargin == 3
      dim = varargin{1};
    end
    [Y, ok] = __gpuArray_quantile__(X, p, dim);
  end
  if ~ok
    Y = __gpuArray_host__('quantile', X, p, varargin{:});
  end
end
%=============================================================================
