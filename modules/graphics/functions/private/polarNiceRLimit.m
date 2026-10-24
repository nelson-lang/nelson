%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function limits = polarNiceRLimit(maxRadius)
  if isempty(maxRadius) || ~isfinite(maxRadius) || maxRadius <= 0
    limits = [0 1];
    return
  end
  if abs(maxRadius - round(maxRadius)) < 1e-12
    limits = [0 maxRadius];
    return
  end
  rawStep = maxRadius / 5;
  magnitude = 10 ^ floor(log10(rawStep));
  normalized = rawStep / magnitude;
  if normalized <= 1
    step = magnitude;
  elseif normalized <= 2
    step = 2 * magnitude;
  elseif normalized <= 5
    step = 5 * magnitude;
  else
    step = 10 * magnitude;
  end
  limits = [0 ceil(maxRadius / step) * step];
end
%=============================================================================
