%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function constr = optimconstr(varargin)
  if nargin == 0
    constr = optim.problemdef.OptimizationConstraint([], '<=', []);
  elseif nargin == 3
    constr = optim.problemdef.OptimizationConstraint(varargin{1}, varargin{2}, varargin{3});
  elseif nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  else
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
end
%=============================================================================
