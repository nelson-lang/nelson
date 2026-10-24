%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = odeValidatePositiveIntegerOrEmpty(value, name)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value <= 0 ...
      || value ~= fix(value)
    error('Nelson:ode_solvers:mustBeIntegerScalar', '%s must be an integer scalar.', name);
  end
end
%=============================================================================
