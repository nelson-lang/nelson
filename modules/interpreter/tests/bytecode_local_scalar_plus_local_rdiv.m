%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = bytecode_local_scalar_plus_local_rdiv(n)
  s = 0;
  numerator = 3;
  denominator = 2;
  for k = 1:n
    s = s + numerator / denominator;
    numerator = numerator + 1;
    denominator = denominator + 1;
  end
end
%=============================================================================
