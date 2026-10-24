%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movsum(x, window, varargin)
  y = movingWindowApply(x, window, 'sum', @sumAlongFirst, varargin{:});
end
%=============================================================================
function y = sumAlongFirst(x, nanflag)
  if strcmp(nanflag, 'omitnan') && isfloat(x)
    xn = x;
    xn(isnan(xn)) = 0;
    y = sum(xn, 1);
  else
    y = sum(x, 1);
  end
end
%=============================================================================
