%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function odeValidateVector(value, name)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ~isvector(value) || any(~isfinite(value(:)))
    error('Nelson:ode_solvers:mustBeFiniteVector:valueFiniteNumericVectorExpected', '%s: finite numeric vector expected.', name);
  end
end
%=============================================================================
