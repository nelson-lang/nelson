%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pp = pchip(x, y, xq)
  narginchk(2, 3);
  if nargin == 3
    pp = interp1(x, y, xq, 'pchip');
  else
    pp = interp1(x, y, 'pchip', 'pp');
  end
end
%=============================================================================
