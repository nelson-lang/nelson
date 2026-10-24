%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movmean(x, window, varargin)
  y = movingWindowApply(x, window, 'mean', @meanAlongFirst, varargin{:});
end
%=============================================================================
function y = meanAlongFirst(x, nanflag)
  if strcmp(nanflag, 'omitnan') && isfloat(x)
    m = isnan(x);
    xn = x;
    xn(m) = 0;
    y = sum(xn, 1) ./ sum(~m, 1);
  else
    y = mean(x, 1);
  end
end
%=============================================================================
