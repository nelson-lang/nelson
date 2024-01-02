%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = and(A, B)
  if ~issparse(A) || ~islogical(A)
    A = sparse(logical(A));
  end
  if ~issparse(B) || ~islogical(B)
    B = sparse(logical(B));
  end
  R = and(A, B);
end
%=============================================================================
