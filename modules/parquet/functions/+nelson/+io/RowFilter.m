%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef RowFilter
  properties
    VariableNames = {};
    Expression = [];
  end
  methods
    function obj = RowFilter(variableNames, expression)
      if nargin >= 1
        obj.VariableNames = variableNames;
      end
      if nargin >= 2
        obj.Expression = expression;
      end
    end
    function R = subsref(obj, s)
      if strcmp(s(1).type, '.')
        name = s(1).subs;
        st = struct(obj);
        if strcmp(name, 'VariableNames')
          R = st.VariableNames;
          return
        elseif strcmp(name, 'Expression')
          R = st.Expression;
          return
        elseif strcmp(name, 'apply') && length(s) > 1 && strcmp(s(2).type, '()')
          R = apply(obj, s(2).subs{:});
          return
        elseif strcmp(name, 'variables')
          R = variables(obj);
          return
        end
        if any(strcmp(st.VariableNames, name))
          R = nelson.io.RowFilter(st.VariableNames, struct('kind', 'var', 'name', name));
          if length(s) > 1
            R = subsref(R, s(2:end));
          end
          return
        end
      end
      R = builtin('subsref', obj, s);
    end
    function R = gt(A, B), R = compare(A, B, 'gt'); end
    function R = ge(A, B), R = compare(A, B, 'ge'); end
    function R = lt(A, B), R = compare(A, B, 'lt'); end
    function R = le(A, B), R = compare(A, B, 'le'); end
    function R = eq(A, B), R = compare(A, B, 'eq'); end
    function R = ne(A, B), R = compare(A, B, 'ne'); end
    function R = and(A, B), R = logicalOp(A, B, 'and'); end
    function R = or(A, B), R = logicalOp(A, B, 'or'); end
    function R = not(A)
      st = struct(A);
      R = nelson.io.RowFilter(st.VariableNames, struct('kind', 'not', 'left', st.Expression));
    end
    function T = apply(obj, T)
      st = struct(obj);
      if isempty(st.Expression)
        return
      end
      mask = evaluateExpression(st.Expression, T);
      T = T(mask, :);
    end
    function names = variables(obj)
      st = struct(obj);
      names = collectVariables(st.Expression);
    end
  end
end
%=============================================================================
function R = compare(A, B, op)
  [names, left] = normalizeFilterSide(A);
  [names, right] = mergeFilterSide(names, B);
  R = nelson.io.RowFilter(names, struct('kind', op, 'left', left, 'right', right));
end
%=============================================================================
function R = logicalOp(A, B, op)
  [names, left] = normalizeFilterSide(A);
  [names, right] = mergeFilterSide(names, B);
  R = nelson.io.RowFilter(names, struct('kind', op, 'left', left, 'right', right));
end
%=============================================================================
function [names, expr] = normalizeFilterSide(value)
  if isa(value, 'nelson.io.RowFilter')
    st = struct(value);
    names = st.VariableNames;
    expr = st.Expression;
  else
    names = {};
    expr = struct('kind', 'const', 'value', value);
  end
end
%=============================================================================
function [names, expr] = mergeFilterSide(names, value)
  if isa(value, 'nelson.io.RowFilter')
    st = struct(value);
    names = unique([names, st.VariableNames], 'stable');
    expr = st.Expression;
  else
    expr = struct('kind', 'const', 'value', value);
  end
end
%=============================================================================
function value = evaluateExpression(expr, T)
  switch expr.kind
    case 'var'
      value = T.(expr.name);
    case 'const'
      value = expr.value;
    case 'gt'
      value = evaluateExpression(expr.left, T) > evaluateExpression(expr.right, T);
    case 'ge'
      value = evaluateExpression(expr.left, T) >= evaluateExpression(expr.right, T);
    case 'lt'
      value = evaluateExpression(expr.left, T) < evaluateExpression(expr.right, T);
    case 'le'
      value = evaluateExpression(expr.left, T) <= evaluateExpression(expr.right, T);
    case 'eq'
      value = evaluateExpression(expr.left, T) == evaluateExpression(expr.right, T);
    case 'ne'
      value = evaluateExpression(expr.left, T) ~= evaluateExpression(expr.right, T);
    case 'and'
      value = evaluateExpression(expr.left, T) & evaluateExpression(expr.right, T);
    case 'or'
      value = evaluateExpression(expr.left, T) | evaluateExpression(expr.right, T);
    case 'not'
      value = ~evaluateExpression(expr.left, T);
    otherwise
      error('Nelson:RowFilter:unsupportedExpression', 'Unsupported row filter expression.');
  end
end
%=============================================================================
function names = collectVariables(expr)
  names = {};
  if isempty(expr)
    return
  end
  switch expr.kind
    case 'var'
      names = {expr.name};
    case 'const'
      names = {};
    case 'not'
      names = collectVariables(expr.left);
    otherwise
      names = unique([collectVariables(expr.left), collectVariables(expr.right)], 'stable');
  end
end
%=============================================================================
