%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef OptimizationConstraint
  properties
    Left = []
    Relation = '<='
    Right = []
  end

  methods
    function obj = OptimizationConstraint(left, relation, right)
      if nargin == 0
        return
      end
      obj.Left = optim.problemdef.OptimizationExpression.fromAny(left);
      obj.Relation = relation;
      obj.Right = optim.problemdef.OptimizationExpression.fromAny(right);
    end

    function value = evaluate(obj, values)
      lhs = evaluate(obj.Left, values);
      rhs = evaluate(obj.Right, values);
      if strcmp(obj.Relation, '<=')
        value = lhs <= rhs;
      elseif strcmp(obj.Relation, '>=')
        value = lhs >= rhs;
      else
        value = lhs == rhs;
      end
    end

    function residual = residual(obj, values)
      residual = evaluate(obj.Left, values) - evaluate(obj.Right, values);
      if strcmp(obj.Relation, '>=')
        residual = -residual;
      end
    end

    function disp(obj)
      builtin('disp', ['OptimizationConstraint: ', obj.Relation])
    end
  end
end
%=============================================================================
