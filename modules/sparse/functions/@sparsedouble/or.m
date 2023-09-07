%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = or(A, B)
  if ~islogical(A)
    A = logical(A);
  end
  if ~islogical(B)
    B = logical(B);
  end
  R = or(A, B);
end
%=============================================================================
