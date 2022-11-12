%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = generic_and_sparselogical(A, B)
  % internal function (overload)
  if (issparse(A) || isnumeric(A) || islogical(A))
    R = sparselogical_and_sparselogical(sparse(logical(A)), B);
  else
    BB = full(B);
    R = A & BB;
  end
end
%=============================================================================
