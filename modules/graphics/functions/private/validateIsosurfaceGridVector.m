%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateIsosurfaceGridVector(values)
  if any(~isfinite(values)) || numel(values) < 2
    error('Nelson:validateIsosurfaceGridVector:invalidGridVectors', 'Grid vectors must be finite and contain at least two values.');
  end
  steps = diff(values);
  if ~(all(steps > 0) || all(steps < 0))
    error('Nelson:validateIsosurfaceGridVector:gridVectorsNotMonotonic', 'Grid vectors must be strictly monotonic.');
  end
end
%=============================================================================
