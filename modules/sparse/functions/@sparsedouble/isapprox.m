%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = isapprox(a, b, precision)
  if nargin() == 2
    precision = 0;
  end
  r = false;
  b = sparse(b);
  if ~isequal(size(a), size(b))
    r = false;
  else
    [IA, JA, VA] = IJV(a);
    [IB, JB, VB] = IJV(b);
    r = isequal(IA, IB) && isequal(JA, JB) && isapprox(VA, VB, precision);
  end
end