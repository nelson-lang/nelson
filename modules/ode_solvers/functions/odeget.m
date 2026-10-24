%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = odeget(options, name, defaultValue)
  narginchk(2, 3);
  if nargin < 3
    defaultValue = [];
  end
  if isempty(options)
    value = defaultValue;
    return
  end
  if odeIsSolverOptions(options)
    options = struct(options);
  end
  if ~isstruct(options)
    error('Nelson:ode_solvers:optionsStructureExpected', 'Options structure expected.');
  end
  fieldName = odeOptionName(name);
  if isfield(options, fieldName) && ~isempty(options.(fieldName))
    value = options.(fieldName);
  else
    value = defaultValue;
  end
end
%=============================================================================
