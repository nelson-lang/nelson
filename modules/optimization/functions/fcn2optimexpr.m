%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function expr = fcn2optimexpr(fcn, varargin)
  if nargin < 1 || ~isa(fcn, 'function_handle')
    error('Nelson:fcn2optimexpr:functionHandleExpected', '%s must be a function handle.', 'First argument');
  end
  [inputs, ~] = splitInputsAndOptions(varargin);
  argExprs = cell(1, numel(inputs));
  vars = {};
  varMap = struct();
  lowerMap = struct();
  upperMap = struct();
  typeMap = struct();
  displays = cell(1, numel(inputs));
  for k = 1:numel(inputs)
    e = optim.problemdef.OptimizationExpression.fromAny(inputs{k});
    argExprs{k} = e;
    vars = unique([vars, e.Variables], 'stable');
    varMap = mergeVarMaps(varMap, e.VarMap);
    lowerMap = mergeVarMaps(lowerMap, e.VarLowerBound);
    upperMap = mergeVarMaps(upperMap, e.VarUpperBound);
    typeMap = mergeVarMaps(typeMap, e.VarType);
    displays{k} = e.Display;
  end
  displayText = [functionName(fcn), '(', joinDisplays(displays), ')'];
  expr = optim.problemdef.OptimizationExpression(@(values) applyFcn(fcn, argExprs, values), ...
    displayText, vars, varMap);
  expr.VarLowerBound = lowerMap;
  expr.VarUpperBound = upperMap;
  expr.VarType = typeMap;
end
%=============================================================================
function value = applyFcn(fcn, argExprs, values)
  args = cell(1, numel(argExprs));
  for k = 1:numel(argExprs)
    args{k} = evaluate(argExprs{k}, values);
  end
  value = fcn(args{:});
end
%=============================================================================
function [inputs, options] = splitInputsAndOptions(args)
  knownOptions = {'outputsize', 'reuseevaluation', 'analysis'};
  splitIndex = numel(args) + 1;
  for k = 1:numel(args)
    item = args{k};
    if (ischar(item) || (isstring(item) && isscalar(item))) ...
        && any(strcmp(lower(char(item)), knownOptions))
      splitIndex = k;
      break
    end
  end
  inputs = args(1:splitIndex - 1);
  options = args(splitIndex:end);
  if mod(numel(options), 2) ~= 0
    error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
  end
end
%=============================================================================
function name = functionName(fcn)
  name = func2str(fcn);
  if ~isempty(name) && name(1) == '@'
    name = 'fcn';
  end
end
%=============================================================================
function txt = joinDisplays(displays)
  if isempty(displays)
    txt = '';
    return
  end
  txt = displays{1};
  for k = 2:numel(displays)
    txt = [txt, ', ', displays{k}];
  end
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
