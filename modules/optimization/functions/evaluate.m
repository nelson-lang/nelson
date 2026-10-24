%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = evaluate(expr, values)
  if nargin < 2
    values = struct();
  end
  if isa(expr, 'optim.problemdef.OptimizationExpression')
    value = expr.EvalFcn(values);
  elseif isa(expr, 'optim.problemdef.OptimizationConstraint')
    lhs = evaluate(expr.Left, values);
    rhs = evaluate(expr.Right, values);
    switch expr.Relation
      case '<='
        value = lhs <= rhs;
      case '>='
        value = lhs >= rhs;
      otherwise
        value = lhs == rhs;
    end
  elseif isa(expr, 'optim.problemdef.OptimizationVariable')
    if ~isstruct(values) || ~isfield(values, expr.Name)
      error('Nelson:optimization:variableValueMissing', 'Value for optimization variable is missing.');
    end
    value = values.(expr.Name);
  else
    value = expr;
  end
end
%=============================================================================
