%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stop = odeStopFlag(value, name)
  if isempty(value)
    stop = false;
    return
  end
  if ~(isnumeric(value) || islogical(value)) || ~isscalar(value) || ~isfinite(double(value))
    error('Nelson:ode_solvers:invalidValue:valueStopFlagAScalarNumeric', '%s stop flag must be a scalar numeric or logical value.', name);
  end
  stop = logical(value);
end
%=============================================================================
