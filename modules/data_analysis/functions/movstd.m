%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, m] = movstd(x, window, varargin)
  y = movingWindowApply(x, window, '', @stdAlongFirst, varargin{:});
  if nargout > 1
    % moving mean over the same windows
    m = movmean(x, window, varargin{:});
  end
end
%=============================================================================
function y = stdAlongFirst(x, nanflag)
  y = std(x, 0, 1);
  if strcmp(nanflag, 'omitnan') && isfloat(x)
    % only the columns holding a NaN are reduced without it
    for c = find(any(isnan(x), 1))
      v = x(:, c);
      v = v(~isnan(v));
      if isempty(v)
        y(c) = NaN;
      else
        y(c) = std(v, 0);
      end
    end
  end
end
%=============================================================================
