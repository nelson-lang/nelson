%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = odeValidateSundialsChoice(value, name, choices)
  if isempty(value)
    value = 'auto';
    return
  end
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
  end
  for k = 1:length(choices)
    if strcmpi(value, choices{k})
      value = choices{k};
      return
    end
  end
  error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
end
%=============================================================================
