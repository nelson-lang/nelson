%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sf = validateShrinkFactor(sf)
  if ~isnumeric(sf) || ~isreal(sf) || ~isscalar(sf) || ~isfinite(sf) || sf < 0
    error('Nelson:validateShrinkFactor:invalidShrinkFactor', 'Shrink factor must be a nonnegative finite real numeric scalar.');
  end
  sf = double(sf);
end
%=============================================================================
