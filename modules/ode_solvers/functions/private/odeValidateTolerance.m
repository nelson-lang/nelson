%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function odeValidateTolerance(value, name)
  if isempty(value)
    error('Nelson:ode_solvers:invalidValue:valueNonEmptyToleranceExpected', '%s: non-empty tolerance expected.', name);
  end
  if ~isnumeric(value) || any(~isfinite(value(:))) || any(value(:) < 0)
    error('Nelson:ode_solvers:invalidValue:valueFiniteNonNegativeNumericValue', '%s: finite non-negative numeric value expected.', name);
  end
end
%=============================================================================
