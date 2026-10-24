%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function q = integral_zero_result(fun, a, arrayValued)
  if arrayValued
    v = fun(a);
    q = zeros(size(v));
    if ~isreal(v)
      q = complex(q, q) - complex(q, q);
    end
  else
    q = 0;
  end
end
%=============================================================================
