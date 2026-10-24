%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalTrimTrailingZeros(x)
  last = find(x ~= 0, 1, 'last');
  if isempty(last)
    y = 0;
  else
    y = x(1:last);
  end
end
%=============================================================================
