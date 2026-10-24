%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = spones(S)
  narginchk(1, 1);

  if ~ismatrix(S)
    error('Nelson:sparse:ndSparseNotSupported', 'ND-sparse arrays are not supported.');
  end
  if isa(S, 'single')
    oneValue = single(1);
  else
    oneValue = 1;
  end
  if ~issparse(S)
    if isa(S, 'single')
      s = sparse(single(S ~= 0));
    else
      s = sparse(double(S ~= 0));
    end
  else
    [I, J] = find(S);
    [m, n] = size(S);
    s = sparse(I, J, oneValue, m, n);
  end
end
%=============================================================================
