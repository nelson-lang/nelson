%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = median(varargin)
  % Host fallback: gpuArray arguments are gathered, the host function runs, and
  % numeric/logical results are re-wrapped as gpuArray to stay device-resident.
  args = varargin;
  for k = 1:numel(args)
    if isgpuarray(args{k})
      args{k} = gather(args{k});
    end
  end
  n = max(nargout, 1);
  outputs = cell(1, n);
  [outputs{1:n}] = median(args{:});
  for k = 1:numel(outputs)
    o = outputs{k};
    if isnumeric(o) || islogical(o)
      outputs{k} = gpuArray(o);
    end
  end
  varargout = outputs;
end
%=============================================================================
