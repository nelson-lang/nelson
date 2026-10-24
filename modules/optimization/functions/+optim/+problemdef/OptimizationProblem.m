%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef OptimizationProblem
  properties
    Objective = []
    Constraints = struct()
    Variables = struct()
    ObjectiveSense = 'minimize'
    Description = ''
  end

  methods
    function obj = OptimizationProblem(varargin)
      if mod(length(varargin), 2) ~= 0
        error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:length(varargin)
        optionName = lower(char(varargin{k}));
        optionValue = varargin{k + 1};
        if strcmp(optionName, 'objective')
          objective = optim.problemdef.OptimizationExpression.fromAny(optionValue);
          obj.Objective = objective;
        elseif strcmp(optionName, 'constraints')
          obj.Constraints = optionValue;
        elseif strcmp(optionName, 'objectivesense')
          obj.ObjectiveSense = char(optionValue);
        elseif strcmp(optionName, 'description')
          obj.Description = char(optionValue);
        else
          error('Nelson:optimization:unsupportedOption', 'Unsupported "%s" option.', optionName);
        end
      end
    end

    function st = prob2struct(obj, varargin)
      st = struct();
      st.solver = 'fminsearch';
      st.objective = @(x) evaluateWithVector(obj, x);
      st.x0 = zeros(variableCount(obj), 1);
      st.options = [];
      if nargin >= 2 && isstruct(varargin{1})
        st.x0 = packValues(obj, varargin{1});
      end
      st = addLinearFields(st, obj);
    end

    function [sol, fval, exitflag, output] = solve(obj, x0, varargin)
      if nargin < 2
        x0 = struct();
      end
      st = prob2struct(obj, x0);
      if isempty(varargin)
        options = [];
      else
        options = varargin{1};
      end
      st.options = options;
      if strcmp(st.solver, 'linprog')
        [x, fval, exitflag, output] = linprog(st);
      elseif strcmp(st.solver, 'intlinprog')
        [x, fval, exitflag, output] = intlinprog(st);
      elseif strcmp(st.solver, 'quadprog')
        [x, fval, exitflag, output] = quadprog(st);
      elseif strcmp(st.solver, 'fmincon')
        [x, fval, exitflag, output] = fmincon(st);
      else
        objective = st.objective;
        [x, fval, exitflag, output] = fminsearch(objective, st.x0, options);
      end
      addOffset = isfield(st, 'objectiveOffset') ...
        && (strcmp(st.solver, 'linprog') || strcmp(st.solver, 'intlinprog') ...
        || strcmp(st.solver, 'quadprog'));
      if strcmpi(obj.ObjectiveSense, 'maximize') && ~isempty(fval)
        if addOffset
          fval = fval + st.objectiveOffset;
        end
        fval = -fval;
      elseif addOffset && ~isempty(fval)
        fval = fval + st.objectiveOffset;
      end
      if isempty(x)
        sol = emptySolution(obj);
      else
        sol = unpackValues(obj, x);
      end
    end

    function value = evaluateWithVector(obj, x)
      values = unpackValues(obj, x);
      value = evaluate(obj.Objective, values);
      if strcmpi(obj.ObjectiveSense, 'maximize')
        value = -value;
      end
      value = double(value) + 1e8 * (constraintPenalty(obj, values) + variablePenalty(obj, values));
    end

    function n = variableCount(obj)
      sizes = variableSizes(obj);
      names = variableNames(obj, sizes);
      n = 0;
      for k = 1:length(names)
        n = n + prod(sizes.(names{k}));
      end
    end

    function names = objectiveVariables(obj)
      if isempty(obj.Objective)
        names = {};
      else
        names = obj.Objective.Variables;
      end
      names = unique([names, constraintVariables(obj)], 'stable');
    end

    function names = constraintVariables(obj)
      names = {};
      constraints = constraintList(obj);
      for k = 1:length(constraints)
        c = constraints{k};
        names = unique([names, c.Left.Variables, c.Right.Variables], 'stable');
      end
    end

    function sizes = variableSizes(obj)
      sizes = struct();
      if ~isempty(obj.Objective) && isfield(struct(obj.Objective), 'VarMap')
        sizes = obj.Objective.VarMap;
      end
      constraints = constraintList(obj);
      for k = 1:length(constraints)
        c = constraints{k};
        if isfield(struct(c.Left), 'VarMap')
          sizes = mergeVarMaps(sizes, c.Left.VarMap);
        end
        if isfield(struct(c.Right), 'VarMap')
          sizes = mergeVarMaps(sizes, c.Right.VarMap);
        end
      end
      names = objectiveVariables(obj);
      for k = 1:length(names)
        if ~isfield(sizes, names{k})
          sizes.(names{k}) = [1 1];
        end
      end
    end

    function x = packValues(obj, values)
      sizes = variableSizes(obj);
      names = variableNames(obj, sizes);
      x = zeros(variableCount(obj), 1);
      offset = 0;
      for k = 1:length(names)
        if isstruct(values) && isfield(values, names{k})
          value = values.(names{k});
        else
          value = zeros(sizes.(names{k}));
        end
        count = prod(sizes.(names{k}));
        x(offset + 1:offset + count) = value(:);
        offset = offset + count;
      end
    end

    function values = unpackValues(obj, x)
      sizes = variableSizes(obj);
      names = variableNames(obj, sizes);
      values = struct();
      offset = 0;
      for k = 1:length(names)
        count = prod(sizes.(names{k}));
        values.(names{k}) = reshape(x(offset + 1:offset + count), sizes.(names{k}));
        offset = offset + count;
      end
    end

    function penalty = constraintPenalty(obj, values)
      penalty = 0;
      constraints = constraintList(obj);
      for k = 1:length(constraints)
        c = constraints{k};
        r = residual(c, values);
        if strcmp(c.Relation, '==')
          penalty = penalty + sum(r(:) .^ 2);
        else
          penalty = penalty + sum(max(0, r(:)) .^ 2);
        end
      end
    end

    function penalty = variablePenalty(obj, values)
      penalty = 0;
      if isempty(obj.Objective)
        return
      end
      expr = obj.Objective;
      names = objectiveVariables(obj);
      for k = 1:length(names)
        name = names{k};
        if ~isfield(values, name)
          continue
        end
        x = values.(name);
        if isfield(expr.VarLowerBound, name)
          lb = expr.VarLowerBound.(name);
          penalty = penalty + sum(max(0, lb - x) .^ 2);
        end
        if isfield(expr.VarUpperBound, name)
          ub = expr.VarUpperBound.(name);
          penalty = penalty + sum(max(0, x - ub) .^ 2);
        end
        if isfield(expr.VarType, name)
          type = lower(expr.VarType.(name));
          if strcmp(type, 'integer')
            penalty = penalty + sum((x - round(x)) .^ 2);
          elseif strcmp(type, 'binary')
            penalty = penalty + sum(max(0, -x) .^ 2 + max(0, x - 1) .^ 2 + (x .* (1 - x)) .^ 2);
          end
        end
      end
    end

    function constraints = constraintList(obj)
      constraints = flattenConstraints(obj.Constraints);
    end

    function disp(obj)
      builtin('disp', ['OptimizationProblem: ', obj.ObjectiveSense])
    end
  end
end
%=============================================================================
function constraints = flattenConstraints(value)
  constraints = {};
  if isempty(value)
    return
  end
  if isa(value, 'optim.problemdef.OptimizationConstraint')
    for k = 1:numel(value)
      item = value(k);
      if ~isempty(item.Left) && ~isempty(item.Right)
        constraints{end + 1} = item;
      end
    end
  elseif isstruct(value)
    fields = fieldnames(value);
    for n = 1:numel(value)
      for k = 1:length(fields)
        constraints = appendConstraints(constraints, flattenConstraints(value(n).(fields{k})));
      end
    end
  elseif iscell(value)
    for k = 1:numel(value)
      constraints = appendConstraints(constraints, flattenConstraints(value{k}));
    end
  end
end
%=============================================================================
function constraints = appendConstraints(constraints, extra)
  for k = 1:length(extra)
    constraints{end + 1} = extra{k};
  end
end
%=============================================================================
function sol = emptySolution(prob)
  sizes = variableSizes(prob);
  names = variableNames(prob, sizes);
  sol = struct();
  for k = 1:length(names)
    sol.(names{k}) = [];
  end
end
%=============================================================================
function problem = addLinearFields(problem, prob)
  n = variableCount(prob);
  zeroValues = unpackValues(prob, zeros(n, 1));
  isLinear = true;
  if ~isempty(prob.Objective)
    [f, constant, isObjectiveLinear] = linearCoefficients(@(values) evaluate(prob.Objective, values), prob, zeroValues);
    if strcmpi(prob.ObjectiveSense, 'maximize')
      f = -f;
      constant = -constant;
    end
    problem.f = f(:);
    problem.objectiveOffset = constant;
    isLinear = isLinear && isObjectiveLinear;
  end
  [A, b, Aeq, beq, areConstraintsLinear] = linearConstraints(prob, zeroValues);
  isLinear = isLinear && areConstraintsLinear;
  problem.A = sparse(A);
  problem.b = b;
  problem.Aeq = sparse(Aeq);
  problem.beq = beq;
  [lb, ub, intcon] = variableBoundsAndTypes(prob);
  problem.lb = lb;
  problem.ub = ub;
  problem.intcon = intcon;
  if isLinear && isfield(problem, 'f')
    if isempty(intcon)
      problem.solver = 'linprog';
    else
      problem.solver = 'intlinprog';
    end
  elseif ~isempty(intcon)
    error('Nelson:OptimizationProblem:integerNonlinearNotSupported', 'Nonlinear problems with integer variables are not supported.');
  elseif isempty(intcon) && areConstraintsLinear && ~isempty(prob.Objective)
    [H, qf, qconstant, isQuadratic] = quadraticCoefficients(prob);
    if isQuadratic
      problem.solver = 'quadprog';
      problem.H = sparse(H);
      problem.f = qf(:);
      problem.objectiveOffset = qconstant;
    end
  end
  if strcmp(problem.solver, 'fminsearch') && isempty(intcon) && ~isempty(prob.Objective) ...
      && hasContinuousConstraints(prob, lb, ub)
    problem.solver = 'fmincon';
    problem.objective = @(x) evaluateObjectiveOnly(prob, x);
    if areConstraintsLinear
      problem.nonlcon = [];
    else
      problem.A = sparse([]);
      problem.b = [];
      problem.Aeq = sparse([]);
      problem.beq = [];
      problem.nonlcon = @(x) nonlinearConstraints(prob, x);
    end
  end
end
%=============================================================================
function [H, f, constant, isQuadratic] = quadraticCoefficients(prob)
  n = variableCount(prob);
  objective = @(x) double(evaluate(prob.Objective, unpackValues(prob, x)));
  constant = objective(zeros(n, 1));
  if ~isscalar(constant)
    H = []; f = []; isQuadratic = false; return
  end
  H = zeros(n, n);
  f = zeros(n, 1);
  for i = 1:n
    ei = zeros(n, 1); ei(i) = 1;
    v1 = objective(ei);
    v2 = objective(2 * ei);
    H(i, i) = v2 - 2 * v1 + constant;
    f(i) = v1 - constant - 0.5 * H(i, i);
  end
  for i = 1:n
    ei = zeros(n, 1); ei(i) = 1;
    for j = i + 1:n
      ej = zeros(n, 1); ej(j) = 1;
      vij = objective(ei + ej);
      H(i, j) = vij - constant - f(i) - f(j) - 0.5 * H(i, i) - 0.5 * H(j, j);
      H(j, i) = H(i, j);
    end
  end
  isQuadratic = validateQuadratic(objective, H, f, constant);
  if strcmpi(prob.ObjectiveSense, 'maximize')
    H = -H; f = -f; constant = -constant;
  end
  H = (H + H') / 2;
end
%=============================================================================
function tf = validateQuadratic(objective, H, f, constant)
  n = length(f);
  points = [2 * ones(n, 1), -ones(n, 1), (1:n)'];
  tf = true;
  for k = 1:size(points, 2)
    x = points(:, k);
    actual = objective(x);
    expected = 0.5 * x' * H * x + f' * x + constant;
    if ~isscalar(actual) || abs(actual - expected) > 1e-8 * max(1, abs(actual))
      tf = false; return
    end
  end
end
%=============================================================================
function [coefficients, constant, isLinear] = linearCoefficients(fun, prob, zeroValues)
  n = variableCount(prob);
  constant = double(fun(zeroValues));
  m = numel(constant);
  coefficients = zeros(m, n);
  for k = 1:n
    x = zeros(n, 1);
    x(k) = 1;
    values = unpackValues(prob, x);
    value = double(fun(values));
    coefficients(:, k) = value(:) - constant(:);
  end
  isLinear = isLinearApproximation(fun, prob, coefficients, constant);
end
%=============================================================================
function [A, b, Aeq, beq, isLinear] = linearConstraints(prob, zeroValues)
  A = [];
  b = [];
  Aeq = [];
  beq = [];
  isLinear = true;
  constraints = constraintList(prob);
  for k = 1:length(constraints)
    c = constraints{k};
    residualFcn = @(values) evaluate(c.Left, values) - evaluate(c.Right, values);
    [row, constant, isConstraintLinear] = linearCoefficients(residualFcn, prob, zeroValues);
    isLinear = isLinear && isConstraintLinear;
    if strcmp(c.Relation, '==')
      Aeq = [Aeq; row];
      beq = [beq; -constant(:)];
    elseif strcmp(c.Relation, '<=')
      A = [A; row];
      b = [b; -constant(:)];
    else
      A = [A; -row];
      b = [b; constant(:)];
    end
  end
end
%=============================================================================
function tf = isLinearApproximation(fun, prob, coefficients, constant)
  n = variableCount(prob);
  points = [2 * ones(n, 1), -ones(n, 1)];
  tf = true;
  for k = 1:size(points, 2)
    x = points(:, k);
    values = unpackValues(prob, x);
    actual = double(fun(values));
    expected = constant(:) + coefficients * x;
    if numel(actual) ~= numel(expected) || max(abs(actual(:) - expected(:))) > 1e-10
      tf = false;
      return
    end
  end
end
%=============================================================================
function [lb, ub, intcon] = variableBoundsAndTypes(prob)
  sizes = variableSizes(prob);
  names = variableNames(prob, sizes);
  n = variableCount(prob);
  lb = -Inf(n, 1);
  ub = Inf(n, 1);
  intcon = [];
  metadata = variableMetadata(prob);
  offset = 0;
  for k = 1:length(names)
    name = names{k};
    count = prod(sizes.(name));
    range = offset + (1:count);
    if isfield(metadata.lower, name)
      lb(range) = expandBound(metadata.lower.(name), count);
    end
    if isfield(metadata.upper, name)
      ub(range) = expandBound(metadata.upper.(name), count);
    end
    if isfield(metadata.type, name)
      type = lower(metadata.type.(name));
      if strcmp(type, 'integer') || strcmp(type, 'binary')
        intcon = [intcon, range];
      end
    end
    offset = offset + count;
  end
end
%=============================================================================
function metadata = variableMetadata(prob)
  metadata.lower = struct();
  metadata.upper = struct();
  metadata.type = struct();
  expressions = {};
  if ~isempty(prob.Objective)
    expressions{end + 1} = prob.Objective;
  end
  constraints = constraintList(prob);
  for k = 1:length(constraints)
    expressions{end + 1} = constraints{k}.Left;
    expressions{end + 1} = constraints{k}.Right;
  end
  for k = 1:length(expressions)
    metadata.lower = mergeVarMaps(metadata.lower, expressions{k}.VarLowerBound);
    metadata.upper = mergeVarMaps(metadata.upper, expressions{k}.VarUpperBound);
    metadata.type = mergeVarMaps(metadata.type, expressions{k}.VarType);
  end
end
%=============================================================================
function names = variableNames(prob, sizes)
  names = objectiveVariables(prob);
  extraNames = fieldnames(sizes);
  names = unique([names, extraNames(:)'], 'stable');
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
function value = expandBound(bound, count)
  if isscalar(bound)
    value = repmat(bound, count, 1);
  else
    value = bound(:);
  end
end
%=============================================================================
function tf = hasContinuousConstraints(prob, lb, ub)
  tf = ~isempty(constraintList(prob)) || any(isfinite(lb)) || any(isfinite(ub));
end
%=============================================================================
function value = evaluateObjectiveOnly(prob, x)
  values = unpackValues(prob, x);
  value = evaluate(prob.Objective, values);
  if strcmpi(prob.ObjectiveSense, 'maximize')
    value = -value;
  end
  value = double(value);
end
%=============================================================================
function [c, ceq] = nonlinearConstraints(prob, x)
  values = unpackValues(prob, x);
  c = [];
  ceq = [];
  constraints = constraintList(prob);
  for k = 1:length(constraints)
    item = constraints{k};
    residualValue = double(residual(item, values));
    if strcmp(item.Relation, '==')
      ceq = [ceq; residualValue(:)];
    elseif strcmp(item.Relation, '<=')
      c = [c; residualValue(:)];
    else
      c = [c; -residualValue(:)];
    end
  end
end
%=============================================================================
