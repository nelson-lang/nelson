%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = grcar(n, upperBand, className)
  if isempty(upperBand)
    upperBand = 3;
  end
  R = tril(triu(ones(n,className)), upperBand) - diag(ones(n-1,1,className), -1);
end
%=============================================================================
