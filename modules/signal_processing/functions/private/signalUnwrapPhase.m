%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalUnwrapPhase(x)
  y = x;
  if isempty(x)
    return;
  end
  offset = 0;
  for k = 2:length(x)
    d = x(k) - x(k - 1);
    if d > pi
      offset = offset - 2 * pi;
    elseif d < -pi
      offset = offset + 2 * pi;
    end
    y(k) = x(k) + offset;
  end
end
%=============================================================================
