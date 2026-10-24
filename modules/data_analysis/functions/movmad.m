%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movmad(x, window, varargin)
  y = movingWindowApply(x, window, 'mad', @madLocal, varargin{:});
end
%=============================================================================
function y = madLocal(x, nanflag)
  if strcmp(nanflag, 'omitnan') && isfloat(x)
    y = zeros(1, size(x, 2));
    for c = 1:size(x, 2)
      v = x(:, c);
      v = v(~isnan(v));
      if isempty(v)
        y(c) = NaN;
      else
        y(c) = median(abs(v - median(v)));
      end
    end
  else
    m = median(x, 1);
    y = median(abs(x - repmat(m, size(x, 1), 1)), 1);
  end
end
%=============================================================================
