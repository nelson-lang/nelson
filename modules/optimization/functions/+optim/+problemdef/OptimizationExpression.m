%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef OptimizationExpression
  properties
    EvalFcn = []
    Display = ''
    Variables = {}
    VarMap = struct()
    VarLowerBound = struct()
    VarUpperBound = struct()
    VarType = struct()
  end

  methods
    function obj = OptimizationExpression(value, displayText, variables, varMap)
      if nargin == 0
        value = 0;
        displayText = '0';
        variables = {};
      end
      if nargin < 2
        displayText = 'expression';
      end
      if nargin < 3
        variables = {};
      end
      if nargin < 4
        varMap = struct();
      end
      if isa(value, 'function_handle')
        obj.EvalFcn = value;
      else
        obj.EvalFcn = @(values) value;
      end
      obj.Display = displayText;
      obj.Variables = variables(:)';
      obj.VarMap = varMap;
      obj.VarLowerBound = struct();
      obj.VarUpperBound = struct();
      obj.VarType = struct();
    end

    function value = evaluate(obj, values)
      if nargin < 2
        values = struct();
      end
      evalFcn = obj.EvalFcn;
      value = evalFcn(values);
    end

    function c = plus(a, b)
      c = binary(a, b, '+', @(x, y) x + y);
    end

    function c = minus(a, b)
      c = binary(a, b, '-', @(x, y) x - y);
    end

    function c = times(a, b)
      c = binary(a, b, '.*', @(x, y) x .* y);
    end

    function c = rdivide(a, b)
      c = binary(a, b, './', @(x, y) x ./ y);
    end

    function c = mtimes(a, b)
      c = binary(a, b, mtimesDisplayOperator(a, b), @(x, y) x * y);
    end

    function c = power(a, b)
      c = powerExpression(a, b, @(x, y) x .^ y);
    end

    function c = mpower(a, b)
      c = powerExpression(a, b, @(x, y) x ^ y);
    end

    function c = uminus(a)
      a = optim.problemdef.OptimizationExpression.fromAny(a);
      c = optim.problemdef.OptimizationExpression(@(v) - evaluate(a, v), ['-', a.Display], a.Variables);
      c.VarMap = a.VarMap;
      c.VarLowerBound = a.VarLowerBound;
      c.VarUpperBound = a.VarUpperBound;
      c.VarType = a.VarType;
    end

    function c = sum(a, varargin)
      a = optim.problemdef.OptimizationExpression.fromAny(a);
      c = optim.problemdef.OptimizationExpression(@(v) sum(evaluate(a, v), varargin{:}), ...
        sumDisplay(a, varargin), a.Variables);
      c.VarMap = a.VarMap;
      c.VarLowerBound = a.VarLowerBound;
      c.VarUpperBound = a.VarUpperBound;
      c.VarType = a.VarType;
    end

    function c = norm(a)
      a = optim.problemdef.OptimizationExpression.fromAny(a);
      c = optim.problemdef.OptimizationExpression(@(v) norm(evaluate(a, v)), ['norm(', a.Display, ')'], a.Variables);
      c.VarMap = a.VarMap;
      c.VarLowerBound = a.VarLowerBound;
      c.VarUpperBound = a.VarUpperBound;
      c.VarType = a.VarType;
    end

    function c = log(a)
      a = optim.problemdef.OptimizationExpression.fromAny(a);
      c = optim.problemdef.OptimizationExpression(@(v) log(evaluate(a, v)), ['log(', a.Display, ')'], a.Variables);
      c.VarMap = a.VarMap;
      c.VarLowerBound = a.VarLowerBound;
      c.VarUpperBound = a.VarUpperBound;
      c.VarType = a.VarType;
    end

    function c = le(a, b)
      c = optim.problemdef.OptimizationConstraint(a, '<=', b);
    end

    function c = ge(a, b)
      c = optim.problemdef.OptimizationConstraint(a, '>=', b);
    end

    function c = eq(a, b)
      c = optim.problemdef.OptimizationConstraint(a, '==', b);
    end

    function disp(obj)
      builtin('disp', obj.Display)
    end

    function value = subsref(obj, s)
      if strcmp(s(1).type, '.')
        value = builtin('subsref', obj, s);
        return
      end
      if ~strcmp(s(1).type, '()')
        error('Nelson:OptimizationExpression:unsupportedIndexing', 'Unsupported expression indexing.');
      end
      subs = s(1).subs;
      value = optim.problemdef.OptimizationExpression(@(values) indexValue(evaluate(obj, values), subs), ...
        [obj.Display, subscriptText(subs)], obj.Variables, obj.VarMap);
      value.VarLowerBound = obj.VarLowerBound;
      value.VarUpperBound = obj.VarUpperBound;
      value.VarType = obj.VarType;
      if length(s) > 1
        value = subsref(value, s(2:end));
      end
    end
  end

  methods (Static)
    function obj = fromAny(value)
      if isa(value, 'optim.problemdef.OptimizationExpression')
        obj = value;
      elseif isa(value, 'optim.problemdef.OptimizationVariable')
        obj = toExpression(value);
      else
        obj = optim.problemdef.OptimizationExpression(value, valueDisplay(value), {});
      end
    end
  end
end
%=============================================================================
function c = binary(a, b, op, f)
  a = optim.problemdef.OptimizationExpression.fromAny(a);
  b = optim.problemdef.OptimizationExpression.fromAny(b);
  vars = unique([a.Variables, b.Variables], 'stable');
  varMap = mergeVarMaps(a.VarMap, b.VarMap);
  lowerMap = mergeVarMaps(a.VarLowerBound, b.VarLowerBound);
  upperMap = mergeVarMaps(a.VarUpperBound, b.VarUpperBound);
  typeMap = mergeVarMaps(a.VarType, b.VarType);
  c = optim.problemdef.OptimizationExpression(@(v) f(evaluate(a, v), evaluate(b, v)), ...
    ['(', a.Display, ' ', op, ' ', b.Display, ')'], vars, varMap);
  c.VarLowerBound = lowerMap;
  c.VarUpperBound = upperMap;
  c.VarType = typeMap;
end
%=============================================================================
function c = powerExpression(a, b, f)
  a = optim.problemdef.OptimizationExpression.fromAny(a);
  b = optim.problemdef.OptimizationExpression.fromAny(b);
  vars = unique([a.Variables, b.Variables], 'stable');
  varMap = mergeVarMaps(a.VarMap, b.VarMap);
  lowerMap = mergeVarMaps(a.VarLowerBound, b.VarLowerBound);
  upperMap = mergeVarMaps(a.VarUpperBound, b.VarUpperBound);
  typeMap = mergeVarMaps(a.VarType, b.VarType);
  c = optim.problemdef.OptimizationExpression(@(v) f(evaluate(a, v), evaluate(b, v)), ...
    [a.Display, '.^', b.Display], vars, varMap);
  c.VarLowerBound = lowerMap;
  c.VarUpperBound = upperMap;
  c.VarType = typeMap;
end
%=============================================================================
function varMap = mergeVarMaps(a, b)
  varMap = a;
  names = fieldnames(b);
  for k = 1:length(names)
    varMap.(names{k}) = b.(names{k});
  end
end
%=============================================================================
function txt = valueDisplay(value)
  if isnumeric(value) && isscalar(value)
    txt = num2str(value);
  else
    txt = 'constant';
  end
end
%=============================================================================
function op = mtimesDisplayOperator(a, b)
  if (isnumeric(a) && isscalar(a)) || (isnumeric(b) && isscalar(b))
    op = '.*';
  else
    op = '*';
  end
end
%=============================================================================
function txt = sumDisplay(a, args)
  if isempty(args)
    txt = ['sum(', a.Display, ')'];
  elseif length(args) == 1 && isnumeric(args{1}) && isscalar(args{1})
    txt = ['sum(', a.Display, ',', int2str(args{1}), ')'];
  elseif length(args) == 1 && (ischar(args{1}) || isstring(args{1}))
    txt = ['sum(', a.Display, ',', char(args{1}), ')'];
  else
    txt = ['sum(', a.Display, ',...)'];
  end
end
%=============================================================================
function value = indexValue(data, subs)
  value = data(subs{:});
end
%=============================================================================
function txt = subscriptText(subs)
  parts = cell(1, length(subs));
  for k = 1:length(subs)
    value = subs{k};
    if isnumeric(value) && isscalar(value)
      parts{k} = int2str(value);
    elseif ischar(value)
      parts{k} = value;
    else
      parts{k} = '...';
    end
  end
  body = parts{1};
  for k = 2:length(parts)
    body = [body, ',', parts{k}];
  end
  txt = ['(', body, ')'];
end
%=============================================================================
