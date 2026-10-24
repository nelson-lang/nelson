%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = mergeOptions(solver, varargin)
  opts = defaultOptions(solver);
  for k = 1:nargin - 1
    arg = varargin{k};
    if isempty(arg)
      continue
    end
    alreadyValidated = false;
    if isobject(arg)
      arg = struct(arg);
      alreadyValidated = true;
    end
    if ~isstruct(arg) || ~isscalar(arg)
      error('Nelson:optimization:invalidOptions', 'Options must be a scalar structure or optimization options object.');
    end
    fields = fieldnames(arg);
    for i = 1:length(fields)
      value = arg.(fields{i});
      if isempty(value)
        continue
      end
      if alreadyValidated && isfield(opts, fields{i})
        opts.(fields{i}) = value;
      else
        name = fieldName(fields{i}, opts);
        opts.(name) = validateOption(name, value);
      end
    end
  end
  opts = normalizeAliases(opts);
end
%=============================================================================
function name = fieldName(inputName, opts)
  if isfield(opts, inputName)
    name = inputName;
  else
    name = canonicalName(inputName);
  end
end
%=============================================================================
function opts = normalizeAliases(opts)
  if ~isempty(opts.MaxFunctionEvaluations)
    opts.MaxFunEvals = opts.MaxFunctionEvaluations;
  elseif ~isempty(opts.MaxFunEvals)
    opts.MaxFunctionEvaluations = opts.MaxFunEvals;
  end
  if ~isempty(opts.MaxIterations)
    opts.MaxIter = opts.MaxIterations;
  elseif ~isempty(opts.MaxIter)
    opts.MaxIterations = opts.MaxIter;
  end
  if ~isempty(opts.FunctionTolerance)
    opts.TolFun = opts.FunctionTolerance;
  end
  if ~isempty(opts.StepTolerance)
    opts.TolX = opts.StepTolerance;
  end
  if isfield(opts, 'TolCon') && ~isempty(opts.TolCon)
    opts.ConstraintTolerance = opts.TolCon;
  end
  if isfield(opts, 'PlotFcn') && ~isempty(opts.PlotFcn)
    opts.PlotFcns = opts.PlotFcn;
  end
  if isfield(opts, 'HessFcn') && ~isempty(opts.HessFcn)
    opts.HessianFcn = opts.HessFcn;
  end
  if isfield(opts, 'HessMult') && ~isempty(opts.HessMult)
    opts.HessianMultiplyFcn = opts.HessMult;
  end
  if isfield(opts, 'Hessian') && (ischar(opts.Hessian) || isstring(opts.Hessian)) && ~isempty(opts.Hessian)
    opts.HessianApproximation = opts.Hessian;
  end
end
%=============================================================================
function value = validateOption(name, value)
  if strcmp(name, 'EnableFeasibilityMode')
    value = logicalScalar(value, name);
    return
  end
  if strcmp(name, 'HessianApproximation') || strcmp(name, 'HessUpdate')
    value = hessianApproximationValue(value, name);
    return
  end
  switch name
    case {'Display', 'FiniteDifferenceType', 'FunValCheck', 'Jacobian', ...
        'LargeScale', 'ScaleProblem', 'Diagnostics', 'Algorithm', ...
        'SubproblemAlgorithm', 'GradObj', 'GradConstr', 'HessUpdate', ...
        'CutGeneration', 'Heuristics', 'LPPreprocess', 'NodeSelection', 'RootLPAlgorithm', ...
        'BarrierParamUpdate', 'HessianApproximation'}
      value = textValue(value, name);
    case 'Hessian'
      if isnumeric(value) || ischar(value) || isstring(value)
        return
      end
      error('Nelson:optimization:mustBeNumericOrText', '%s must be a numeric matrix or text scalar.', name);
    case {'OutputFcn', 'PlotFcn', 'PlotFcns', 'HessianFcn', 'HessianMultiplyFcn', ...
        'HessFcn', 'HessMult', 'JacobMult', 'JacobPattern', 'HessPattern', 'TypicalX'}
      return
    case {'MaxFunEvals', 'MaxFunctionEvaluations'}
      if ischar(value) || isstring(value)
        value = char(value);
        return
      end
      if ~(isnumeric(value) && isscalar(value)) || isnan(value)
        error('Nelson:optimization:mustBeNumericScalar', '%s must be a numeric scalar.', name);
      end
      value = double(value);
    case {'UseParallel', 'SpecifyObjectiveGradient', 'SpecifyConstraintGradient', ...
        'CheckGradients', 'HonorBounds', 'UseCodegenSolver'}
      value = logicalScalar(value, name);
    otherwise
      if ~(isnumeric(value) && isscalar(value)) || isnan(value)
        error('Nelson:optimization:mustBeNumericScalar', '%s must be a numeric scalar.', name);
      end
      value = double(value);
  end
end
%=============================================================================
function value = hessianApproximationValue(value, name)
  if iscell(value)
    if isempty(value) || ~ischar(value{1}) || ~strcmpi(value{1}, 'lbfgs')
      error('Nelson:optimization:invalidValue', '%s must be a valid Hessian approximation.', name);
    end
    if numel(value) > 1 && (~isnumeric(value{2}) || ~isscalar(value{2}) || value{2} < 1)
      error('Nelson:optimization:mustBePositive', '%s memory value must be a positive scalar.', name);
    end
    return
  end
  value = textValue(value, name);
end
%=============================================================================
function value = logicalScalar(value, name)
  if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
  value = logical(value);
end
%=============================================================================
function txt = textValue(value, name)
  if isstring(value)
    if ~isscalar(value)
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
    end
    value = char(value);
  end
  if ~ischar(value) || size(value, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  txt = value;
end
%=============================================================================
