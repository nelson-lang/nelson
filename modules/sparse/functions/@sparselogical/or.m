%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = or(A, B)
  % internal function (overload)
  asSparse = issparse(A) && issparse(B);
  A = sparse(logical(A));
  B = sparse(logical(B));
  sA = size(A);
  sB = size(B);
  [IA, JA, VA] = IJV(A);
  [IB, JB, VB] = IJV(B);
  if sA == sB
    if isscalar(A)
      R = sparse(full(A) | full(B));
    else
      VC = VA | VB;
      R = sparse(IA, JA, VC, sA(1), sA(2));
    end
    if (~asSparse && issparse(R))
      R = full(R);
    end
    return
  end
  if isscalar(A) || isscalar(B)
    R = full(A) | full(B);
    R = sparse(R);
    if (~asSparse && issparse(R))
      R = full(R);
    end
    return
  end
  if ((isrow(A) && iscolumn(B)) || (isrow(B) && iscolumn(A)))
    R = sparse(full(A) | full(B));
    if (~asSparse && issparse(R))
      R = full(R);
    end
    return
  end
  error(_('Arrays have incompatible sizes for this operation.'));
end
%=============================================================================
