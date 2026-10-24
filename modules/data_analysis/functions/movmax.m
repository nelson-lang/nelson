%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movmax(x, window, varargin)
  y = movingWindowApply(x, window, 'max', @maxAlongFirst, varargin{:});
end
%=============================================================================
function y = maxAlongFirst(x, nanflag)
  y = max(x, [], 1);
  if strcmp(nanflag, 'includenan') && isfloat(x)
    y(any(isnan(x), 1)) = NaN;
  end
end
%=============================================================================
