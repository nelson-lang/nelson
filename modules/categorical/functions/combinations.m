%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = combinations(varargin)
  narginchk(1, Inf);
  names = cell(1, nargin);
  values = cell(1, nargin);
  for k = 1:nargin
    values{k} = varargin{k}(:);
    names{k} = inputname(k);
    if isempty(names{k})
      names{k} = ['Var', int2str(k)];
    end
  end
  counts = cellfun(@numel, values);
  total = prod(counts);
  columns = cell(1, nargin);
  block = total;
  for k = 1:nargin
    block = block / counts(k);
    idx = repmat(kron((1:counts(k))', ones(block, 1)), total / (block * counts(k)), 1);
    columns{k} = values{k}(idx);
  end
  T = table(columns{:}, 'VariableNames', names);
end
%=============================================================================
