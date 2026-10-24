%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = odeValidateOnOff(value, name)
  if isempty(value)
    value = [];
    return
  end
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value) || ~(strcmpi(value, 'on') || strcmpi(value, 'off'))
    error('Nelson:ode_solvers:invalidValue:valueOff', '%s must be ''on'' or ''off''.', name);
  end
  if strcmpi(value, 'on')
    value = 'on';
  else
    value = 'off';
  end
end
%=============================================================================
