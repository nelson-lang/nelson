%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = generic_shortcutand_sparselogical(A, B)
  % internal function (overload)
  R = sparselogical_shortcutand_sparselogical(sparse(logical(A)), logical(B));
end
%=============================================================================
