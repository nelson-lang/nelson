%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = qr(varargin)
  % CPU fallback: computed on host in double precision (results may be complex,
  % so they are returned as host arrays; gpuArray stores real single/logical).
  args = varargin;
  for k = 1:numel(args)
    if isgpuarray(args{k})
      args{k} = double(gather(args{k}));
    end
  end
  n = max(nargout, 1);
  varargout = cell(1, n);
  [varargout{1:n}] = qr(args{:});
end
%=============================================================================
