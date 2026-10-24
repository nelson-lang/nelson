%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = odeset(varargin)
  if nargin == 0
    options = odeDefaultOptions();
    return
  end
  options = odeDefaultOptions();
  startIndex = 1;
  if isstruct(varargin{1})
    options = odeMergeOptions(options, varargin{1});
    startIndex = 2;
  elseif odeIsSolverOptions(varargin{1})
    options = odeMergeOptions(options, struct(varargin{1}));
    startIndex = 2;
  end
  if startIndex <= nargin
    if mod(nargin - startIndex + 1, 2) ~= 0
      error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
    end
    for k = startIndex:2:nargin
      name = odeOptionName(varargin{k});
      options.(name) = varargin{k + 1};
    end
  end
end
%=============================================================================
