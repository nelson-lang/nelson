%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function a = subsasgn(a, s, b)
  if numel(s) > 1 || ~strcmp(s(1).type, '()')
    error('Nelson:gpuArray:invalidIndexedAssignment', 'gpuArray: only () indexed assignment is supported.');
  end
  if ~isgpuarray(a)
    % a host array receiving device values becomes a device array
    a = gpuArray(a);
  end
  subs = s(1).subs;
  original = subs;
  n = numel(subs);
  % deletion and growth change the shape: the host applies its rules, the result comes back
  reshaping = isnumeric(b) && ~isgpuarray(b) && isequal(size(b), [0 0]);
  for k = 1:n
    if ischar(subs{k}) && isequal(subs{k}, ':')
      subs{k} = (1:__gpuArray_end__(a, k, n))';
    elseif islogical(subs{k})
      % a mask selects the positions of its true values
      subs{k} = find(subs{k});
    end
    if ~isempty(subs{k}) && gather(max(subs{k}(:))) > __gpuArray_end__(a, k, n)
      reshaping = true;
    end
  end
  if reshaping
    x = gather(a);
    for k = 1:n
      if isgpuarray(original{k})
        original{k} = gather(original{k});
      end
    end
    x(original{:}) = gather(b);
    a = gpuArray(x);
    return
  end
  a = __gpuArray_asgn__(a, b, subs{:});
end
%=============================================================================
