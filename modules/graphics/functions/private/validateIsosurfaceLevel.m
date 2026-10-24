%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function isovalue = validateIsosurfaceLevel(value)
  if ~isnumeric(value) || ~isreal(value) || ~isscalar(value) || ~isfinite(value)
    error('Nelson:validateIsosurfaceLevel:invalidIsovalue', 'Isovalue must be a finite real numeric scalar.');
  end
  isovalue = double(value);
end
%=============================================================================
