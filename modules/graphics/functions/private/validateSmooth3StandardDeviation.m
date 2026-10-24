%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sd = validateSmooth3StandardDeviation(sd)
  if ~isnumeric(sd) || ~isreal(sd) || ~isscalar(sd) || ~isfinite(sd) || sd <= 0
    error('Nelson:validateSmooth3StandardDeviation:invalidStandardDeviation', 'Standard deviation must be a positive finite real numeric scalar.');
  end
  sd = double(sd);
end
%=============================================================================
