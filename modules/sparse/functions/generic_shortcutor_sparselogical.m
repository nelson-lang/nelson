%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = generic_shortcutor_sparselogical(A, B)
  % internal function (overload)
  if (issparse(A))
    R = sparselogical_shortcutor_sparselogical(logical(A), B);
  else
    BB = full(B);
    R = A || BB;
  end
end
%=============================================================================
