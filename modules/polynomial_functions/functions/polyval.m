%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = polyval(p, x)
  y = p(1);
  n = prod(size(p));
  for i = 2:n
    y = y.*x + p(i);
  end
end
%=============================================================================
