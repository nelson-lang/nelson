%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = odeApplySolverOptionPairs(obj, varargin)
  if isempty(varargin)
    return
  end
  if length(varargin) == 1 && isstruct(varargin{1})
    names = fieldnames(varargin{1});
    for k = 1:length(names)
      obj = assignSolverOption(obj, names{k}, varargin{1}.(names{k}), false);
    end
    return
  end
  if mod(length(varargin), 2) ~= 0
    error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(varargin)
    obj = assignSolverOption(obj, varargin{k}, varargin{k + 1}, true);
  end
end
%=============================================================================
function obj = assignSolverOption(obj, name, value, strict)
  if isstring(name)
    name = char(name);
  end
  switch lower(name)
    case 'id'
      return
    case 'outputsel'
      name = 'OutputSelection';
    case 'vectorized'
      name = 'Vectorization';
    otherwise
      name = canonicalPropertyName(obj, name);
  end
  if ~isprop(obj, name)
    if ~strict && isempty(value)
      return
    end
    error('Nelson:ode_solvers:unknownSolverOptionProperty', 'Unknown %s property.', 'solver option');
  end
  if isempty(value)
    return
  end
  value = normalizeSolverOptionValue(obj, name, value);
  obj.(name) = value;
end
%=============================================================================
function value = normalizeSolverOptionValue(obj, name, value)
  if isempty(value)
    return
  end
  switch name
    case {'InitialStep', 'MaxStep', 'MinStep'}
      value = odeValidatePositiveScalarOrEmpty(value, name);
    case 'Refine'
      value = odeValidatePositiveIntegerOrEmpty(value, name);
    case {'NormControl', 'BDF'}
      value = odeValidateOnOff(value, name);
    case 'Vectorization'
      value = normalizeVectorization(obj, value);
    case 'OutputFcn'
      if ~(isempty(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
    case 'OutputSelection'
      if ~(isempty(value) || (isnumeric(value) && isvector(value) && all(value(:) >= 1)))
        error('Nelson:ode_solvers:outputselectionIndicesInvalid', '%s indices are invalid.', 'OutputSelection');
      end
    case 'MaxOrder'
      maxOrder = 5;
      if strcmp(obj.ID, 'cvodesnonstiff')
        maxOrder = 12;
      end
      if ~isnumeric(value) || ~isscalar(value) || value ~= fix(value) || value < 1 ...
          || value > maxOrder
        error('Nelson:ode_solvers:maxorderIntegerScalarBetween', 'MaxOrder must be an integer scalar between %d and %d.', 1, maxOrder);
      end
    case 'ComputeConsistentInitialConditions'
      if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
        error('Nelson:ode_solvers:computeconsistentinitialconditionsScalarLogicalValue', 'ComputeConsistentInitialConditions must be a scalar logical value.');
      end
      value = logical(value);
  end
end
%=============================================================================
function value = normalizeVectorization(obj, value)
  if strcmp(obj.ID, 'ode15i') || strcmp(obj.ID, 'idas')
    if iscell(value)
      if numel(value) ~= 2
        error('Nelson:ode_solvers:invalidValue:valueOff', '%s must be ''on'' or ''off''.', 'Vectorized');
      end
      value = {odeValidateOnOff(value{1}, 'Vectorized'), ...
        odeValidateOnOff(value{2}, 'Vectorized')};
    else
      value = odeValidateOnOff(value, 'Vectorized');
      value = {value, value};
    end
  else
    value = odeValidateOnOff(value, 'Vectorized');
  end
end
%=============================================================================
function name = canonicalPropertyName(obj, name)
  names = properties(obj);
  for k = 1:length(names)
    if strcmpi(name, names{k})
      name = names{k};
      return
    end
  end
end
%=============================================================================
