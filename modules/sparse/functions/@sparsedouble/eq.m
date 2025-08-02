%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = eq(A, B)
  if ~issparse(A)
    A = sparse(A);
  end
  if ~issparse(B)
    B = sparse(B);
  end
  R = sparse(full(A) == full(B));
end
%=============================================================================
