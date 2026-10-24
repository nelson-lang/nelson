%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = optimoptions(solver, varargin)
  if nargin == 0
    error('Nelson:optimoptions:solverNameExpected', 'Solver name expected.');
  end
  if isobject(solver)
    solver = class(solver);
  end
  if isstring(solver)
    solver = char(solver);
  end
  if ~ischar(solver) || size(solver, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Solver name');
  end
  if nargin == 2 && (isstruct(varargin{1}) || isobject(varargin{1}))
    opts = optim.options.SolverOptions(solver, varargin{1});
    return
  end
  if mod(length(varargin), 2) ~= 0
    error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
  end
  opts = optim.options.SolverOptions(solver, varargin{:});
end
%=============================================================================
