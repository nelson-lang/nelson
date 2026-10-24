%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef OptimizationVariable
  properties
    Name = ''
    Size = [1 1]
    Type = 'continuous'
    LowerBound = -Inf
    UpperBound = Inf
  end

  methods
    function obj = OptimizationVariable(name, varargin)
      if nargin == 0
        return
      end
      obj.Name = char(name);
      dims = [];
      while ~isempty(varargin) && isnumeric(varargin{1})
        value = double(varargin{1});
        if isscalar(value)
          dims = [dims, value];
        else
          dims = value(:).';
        end
        varargin(1) = [];
      end
      if ~isempty(dims)
        if numel(dims) == 1
          dims = [dims, 1];
        end
        validateDimensions(dims);
        obj.Size = dims;
      end
      if mod(length(varargin), 2) ~= 0
        error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:length(varargin)
        pname = lower(char(varargin{k}));
        value = varargin{k + 1};
        switch pname
          case 'lowerbound'
            obj.LowerBound = value;
          case 'upperbound'
            obj.UpperBound = value;
          case 'type'
            obj.Type = char(value);
          otherwise
            error('Nelson:optimization:unsupportedOption', 'Unsupported "%s" option.', pname);
        end
      end
      obj.Type = validateType(obj.Type);
      validateBounds(obj.LowerBound, obj.UpperBound, obj.Size);
      if strcmpi(obj.Type, 'binary')
        if isequal(obj.LowerBound, -Inf)
          obj.LowerBound = 0;
        end
        if isequal(obj.UpperBound, Inf)
          obj.UpperBound = 1;
        end
      end
    end

    function value = evaluate(obj, values)
      if ~isstruct(values) || ~isfield(values, obj.Name)
        error('Nelson:optimization:variableValueMissing', 'Value for optimization variable is missing.');
      end
      value = values.(obj.Name);
    end

    function expr = toExpression(obj)
      varMap = struct();
      varMap.(obj.Name) = obj.Size;
      expr = optim.problemdef.OptimizationExpression(@(values) evaluate(obj, values), obj.Name, {obj.Name}, varMap);
      expr.VarLowerBound.(obj.Name) = obj.LowerBound;
      expr.VarUpperBound.(obj.Name) = obj.UpperBound;
      expr.VarType.(obj.Name) = obj.Type;
    end

    function c = plus(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) + b;
    end

    function c = minus(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) - b;
    end

    function c = times(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) .* b;
    end

    function c = rdivide(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) ./ b;
    end

    function c = mtimes(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) * b;
    end

    function c = power(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) .^ b;
    end

    function c = mpower(a, b)
      c = optim.problemdef.OptimizationExpression.fromAny(a) ^ b;
    end

    function c = uminus(a)
      c = -toExpression(a);
    end

    function c = sum(a, varargin)
      c = sum(toExpression(a), varargin{:});
    end

    function c = norm(a)
      c = norm(toExpression(a));
    end

    function c = log(a)
      c = log(toExpression(a));
    end

    function c = le(a, b)
      c = toExpression(a) <= b;
    end

    function c = ge(a, b)
      c = toExpression(a) >= b;
    end

    function c = eq(a, b)
      c = toExpression(a) == b;
    end

    function value = subsref(obj, s)
      if strcmp(s(1).type, '.')
        value = builtin('subsref', obj, s);
        return
      end
      if ~strcmp(s(1).type, '()')
        error('Nelson:OptimizationVariable:unsupportedIndexing', 'Unsupported variable indexing.');
      end
      base = toExpression(obj);
      subs = s(1).subs;
      displayText = [obj.Name, subscriptText(subs)];
      value = optim.problemdef.OptimizationExpression(@(values) indexValue(evaluate(base, values), subs), ...
        displayText, base.Variables, base.VarMap);
      value.VarLowerBound = base.VarLowerBound;
      value.VarUpperBound = base.VarUpperBound;
      value.VarType = base.VarType;
      if length(s) > 1
        value = subsref(value, s(2:end));
      end
    end

    function index = end(obj, k, n)
      if k < n
        index = obj.Size(k);
      else
        index = prod(obj.Size(k:end));
      end
    end

    function disp(obj)
      builtin('disp', ['OptimizationVariable: ', obj.Name])
    end
  end
end
%=============================================================================
function validateDimensions(dims)
  if any(~isfinite(dims)) || any(dims < 1) || any(abs(dims - round(dims)) > eps)
    error('Nelson:OptimizationVariable:invalidDimensions', 'Optimization variable dimensions must be positive integers.');
  end
end
%=============================================================================
function type = validateType(type)
  valid = {'continuous', 'integer', 'binary'};
  type = lower(char(type));
  if ~any(strcmp(type, valid))
    error('Nelson:optim:invalidOptionValue', 'Invalid value for "%s".', 'Type');
  end
end
%=============================================================================
function validateBounds(lb, ub, sz)
  count = prod(sz);
  validateBoundSize(lb, count);
  validateBoundSize(ub, count);
  if isscalar(lb) && isscalar(ub)
    if lb > ub
      error('Nelson:optimization:boundsInconsistent', 'Lower bounds must not exceed upper bounds.');
    end
  elseif numel(lb) == numel(ub)
    if any(lb(:) > ub(:))
      error('Nelson:optimization:boundsInconsistent', 'Lower bounds must not exceed upper bounds.');
    end
  end
end
%=============================================================================
function validateBoundSize(bound, count)
  if ~(isscalar(bound) || numel(bound) == count)
    error('Nelson:OptimizationVariable:invalidBoundsSize', 'Variable bounds must be scalar or match the variable size.');
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
