%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = ris(nSize, className)
  paramSeq = -2*(1:nSize) + (nSize + 1.5);
  % pass empty ySeq so cauchy treats y as x and className is used correctly
  R = cauchy(paramSeq, [], className);
end
%=============================================================================
