%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movprod(x, window, varargin)
  y = movingWindowApply(x, window, '', @prodAlongFirst, varargin{:});
end
%=============================================================================
function y = prodAlongFirst(x, nanflag)
  if strcmp(nanflag, 'omitnan') && isfloat(x)
    xn = x;
    xn(isnan(xn)) = 1;
    y = prod(xn, 1);
  else
    y = prod(x, 1);
  end
end
%=============================================================================
