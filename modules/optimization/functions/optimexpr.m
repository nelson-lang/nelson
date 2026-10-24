%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function expr = optimexpr(varargin)
  if nargin == 0
    expr = optim.problemdef.OptimizationExpression(0, '0');
  elseif nargin == 1
    expr = fromAnyExpression(varargin{1});
  else
    expr = optim.problemdef.OptimizationExpression(zeros(varargin{:}), 'zeros');
  end
end
%=============================================================================
function expr = fromAnyExpression(value)
  if isa(value, 'optim.problemdef.OptimizationExpression')
    expr = value;
  elseif isa(value, 'optim.problemdef.OptimizationVariable')
    expr = toExpression(value);
  else
    expr = optim.problemdef.OptimizationExpression(value, 'constant');
  end
end
%=============================================================================
