%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function yq = makima(x, y, xq)
  narginchk(2, 3);
  nargoutchk(0, 1);
  if nargin == 2
    yq = interp1(x, y, 'makima', 'pp');
  else
    yq = interp1(x, y, xq, 'makima');
  end
end
%=============================================================================
