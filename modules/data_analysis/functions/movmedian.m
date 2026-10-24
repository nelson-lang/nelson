%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movmedian(x, window, varargin)
  y = movingWindowApply(x, window, 'median', @medianAlongFirst, varargin{:});
end
%=============================================================================
function y = medianAlongFirst(x, nanflag)
  if strcmp(nanflag, 'omitnan') && isfloat(x)
    y = median(x, 1, 'omitnan');
  else
    y = median(x, 1);
  end
end
%=============================================================================
