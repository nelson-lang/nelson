%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = and(A, B)
  % internal function (overload)
  A = sparse(logical(A));
  B = sparse(logical(B));
  sA = size(A);
  sB = size(B);
  [IA, JA, VA] = IJV(A);
  [IB, JB, VB] = IJV(B);
  if sA == sB
    if isscalar(A)
      R = sparse(full(A) & full(B));  
    else
      VC = VA & VB;
      R = sparse(IA, JA, VC, sA(1), sA(2));
    end
    return
  end
  if isscalar(A) || isscalar(B)
    VC = VA & VB;
    if isscalar(A)
      R = sparse(IB, JB, VC, sB(1), sB(2));
    else
      R = sparse(IA, JA, VC, sA(1), sA(2));
    end
    return
  end
  if ((isrow(A) && iscolumn(B)) || (isrow(B) && iscolumn(A)))
    R = sparse(full(A) & full(B));
    return
  end
  error(_('Arrays have incompatible sizes for this operation.'));
end
%=============================================================================
