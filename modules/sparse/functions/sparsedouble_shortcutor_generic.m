%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = sparsedouble_shortcutor_generic(A, B)
  % internal function (overload)
  R = sparselogical_shortcutor_sparselogical(logical(A), sparse(logical(B)));
end
%=============================================================================
