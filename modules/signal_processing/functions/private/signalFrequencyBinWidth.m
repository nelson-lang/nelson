%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function width = signalFrequencyBinWidth(f)
  f = f(:);
  if length(f) < 2
    error('Nelson:signal:invalidFrequencyVector', 'Frequency vector must contain at least two elements.');
  end
  missingWidth = (f(end) - f(1)) / (length(f) - 1);
  if f(1) ~= 0
    width = [missingWidth; diff(f)];
  else
    width = [diff(f); missingWidth];
  end
end
%=============================================================================
