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
    error(_('ND-sparse arrays are not supported.'));
  end
  if ~issparse(S)
    s = sparse(double(S ~= 0));
  else
    [I, J, V] = IJV(S);
    [m, n] = size(S);
    s = sparse(I, J, 1, m, n);
  end
end
%=============================================================================
