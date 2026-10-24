%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = odeMergeOptions(options, extra)
  if isempty(extra)
    return
  end
  if odeIsSolverOptions(extra)
    extra = struct(extra);
  end
  if ~isstruct(extra)
    error('Nelson:ode_solvers:optionsStructureExpected', 'Options structure expected.');
  end
  names = fieldnames(extra);
  for k = 1:length(names)
    if strcmp(names{k}, 'Sensitivity')
      options.Sensitivity = extra.(names{k});
      continue
    end
    if strcmp(names{k}, 'ID') || strcmp(names{k}, 'ComputeConsistentInitialConditions')
      continue
    end
    name = odeOptionName(names{k});
    options.(name) = extra.(names{k});
  end
end
%=============================================================================
