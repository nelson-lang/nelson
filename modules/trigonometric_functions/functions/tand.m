%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = tand(x)
  % sind and cosd perform an exact reduction on degrees:
  % zeros and infinities (with the right sign) come out exactly.
  r = sind(x) ./ cosd(x);
  if isreal(x)
    % multiples of 45 degrees are exactly +/-1
    q = mod(x, 180);
    r(q == 45) = 1;
    r(q == 135) = -1;
  end
end
%=============================================================================
