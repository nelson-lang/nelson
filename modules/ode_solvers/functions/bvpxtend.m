%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solinit = bvpxtend(sol, xnew, ynew, parameters)
  narginchk(2, 4);
  data = odeSolutionData(sol);
  if nargin < 4
    parameters = bvpSolutionParameters(data);
  end
  x = sort(unique([data.x(:); xnew(:)])).';
  if nargin >= 3 && ~isempty(ynew)
    solinit = bvpinit(x, ynew, parameters);
    return
  end
  if x(1) < data.x(1) || x(length(x)) > data.x(length(data.x))
    error('Nelson:ode_solvers:extendedMeshOutsideSolutionIntervalRequires', 'Extended mesh outside the solution interval requires an initial guess.');
  end
  y = deval(data, x);
  solinit = struct('x', x, 'y', y, 'parameters', parameters(:).');
end
%=============================================================================
function parameters = bvpSolutionParameters(sol)
  if isfield(sol, 'parameters')
    parameters = sol.parameters;
  else
    parameters = [];
  end
end
%=============================================================================
