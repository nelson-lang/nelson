%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function windowSize = validateSmooth3WindowSize(windowSize)
  if ~isnumeric(windowSize) || ~isreal(windowSize) || isempty(windowSize)
    error('Nelson:validateSmooth3WindowSize:invalidWindowSize', 'Window size must be a positive odd integer scalar or three-element vector.');
  end
  if isscalar(windowSize)
    windowSize = repmat(double(windowSize), 1, 3);
  else
    windowSize = double(windowSize(:).');
  end
  if ~isValidSmooth3WindowSize(windowSize)
    error('Nelson:validateSmooth3WindowSize:invalidWindowSize', 'Window size must be a positive odd integer scalar or three-element vector.');
  end
end
%=============================================================================
function tf = isValidSmooth3WindowSize(windowSize)
  tf = numel(windowSize) == 3 && all(isfinite(windowSize)) && all(windowSize > 0) ...
    && all(fix(windowSize) == windowSize) && all(rem(windowSize, 2) == 1);
end
%=============================================================================
