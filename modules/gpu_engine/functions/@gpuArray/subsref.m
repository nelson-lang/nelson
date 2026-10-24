%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = subsref(a, s)
  if ~strcmp(s(1).type, '()')
    error('Nelson:gpuArray:invalidIndexing', 'gpuArray: only () indexing is supported.');
  end
  subs = s(1).subs;
  n = numel(subs);
  for k = 1:n
    if ischar(subs{k}) && isequal(subs{k}, ':')
      if n == 1
        subs{k} = (1:numel(a))';
      else
        subs{k} = (1:size(a, k))';
      end
    end
  end
  r = __gpuArray_index__(a, subs{:});
  if numel(s) > 1
    r = subsref(r, s(2:end));
  end
end
%=============================================================================
