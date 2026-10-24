%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef odeSensitivity
  properties
    Method = 'direct'
    ParameterIndices = []
    Jacobian = []
    InitialValue = []
    InitialSlope = []
    ObjectiveFcn = []
    QuadratureFcn = []
    ObjectiveTime = []
    AdjointRelativeTolerance = []
    AdjointAbsoluteTolerance = []
  end

  methods
    function obj = odeSensitivity(varargin)
      if mod(nargin, 2) ~= 0
        error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:nargin
        name = varargin{k};
        if isstring(name)
          name = char(name);
        end
        obj = odeAssignSensitivityProperty(obj, name, varargin{k + 1});
      end
    end

    function obj = set.Method(obj, value)
      obj.Method = odeNormalizeSensitivityMethod(value);
    end

    function obj = set.ParameterIndices(obj, value)
      obj.ParameterIndices = odeNormalizeParameterIndices(value);
    end

    function obj = set.Jacobian(obj, value)
      if isempty(value) || isa(value, 'odeJacobian')
        obj.Jacobian = value;
      else
        obj.Jacobian = odeJacobian(value);
      end
    end

    function obj = set.InitialValue(obj, value)
      obj.InitialValue = odeNormalizeSensitivityInitial(value, 'InitialValue');
    end

    function obj = set.InitialSlope(obj, value)
      obj.InitialSlope = odeNormalizeSensitivityInitial(value, 'InitialSlope');
    end

    function obj = set.ObjectiveFcn(obj, value)
      obj.ObjectiveFcn = odeNormalizeSensitivityFunction(value, 'ObjectiveFcn');
    end

    function obj = set.QuadratureFcn(obj, value)
      obj.QuadratureFcn = odeNormalizeSensitivityFunction(value, 'QuadratureFcn');
    end

    function obj = set.ObjectiveTime(obj, value)
      obj.ObjectiveTime = odeNormalizeObjectiveTime(value);
    end

    function obj = set.AdjointRelativeTolerance(obj, value)
      obj.AdjointRelativeTolerance = odeNormalizeAdjointTolerance(value, 'AdjointRelativeTolerance');
    end

    function obj = set.AdjointAbsoluteTolerance(obj, value)
      obj.AdjointAbsoluteTolerance = odeNormalizeAdjointTolerance(value, 'AdjointAbsoluteTolerance');
    end
  end
end

function obj = odeAssignSensitivityProperty(obj, name, value)
  switch lower(name)
    case 'method'
      obj.Method = odeNormalizeSensitivityMethod(value);
    case 'parameterindices'
      obj.ParameterIndices = odeNormalizeParameterIndices(value);
    case 'jacobian'
      obj.Jacobian = value;
    case 'initialvalue'
      obj.InitialValue = odeNormalizeSensitivityInitial(value, 'InitialValue');
    case 'initialslope'
      obj.InitialSlope = odeNormalizeSensitivityInitial(value, 'InitialSlope');
    case 'objectivefcn'
      obj.ObjectiveFcn = odeNormalizeSensitivityFunction(value, 'ObjectiveFcn');
    case 'quadraturefcn'
      obj.QuadratureFcn = odeNormalizeSensitivityFunction(value, 'QuadratureFcn');
    case 'objectivetime'
      obj.ObjectiveTime = odeNormalizeObjectiveTime(value);
    case 'adjointrelativetolerance'
      obj.AdjointRelativeTolerance = odeNormalizeAdjointTolerance(value, 'AdjointRelativeTolerance');
    case 'adjointabsolutetolerance'
      obj.AdjointAbsoluteTolerance = odeNormalizeAdjointTolerance(value, 'AdjointAbsoluteTolerance');
    otherwise
      error('Nelson:ode_solvers:unknownSensitivityProperty', 'Unknown %s property.', 'sensitivity');
  end
end

function value = odeNormalizeSensitivityMethod(value)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:ode_solvers:sensitivityMethodDirectAdjoint', 'Sensitivity method must be direct or adjoint.');
  end
  switch lower(value)
    case {'direct', 'forward'}
      value = 'direct';
    case 'adjoint'
      value = 'adjoint';
    otherwise
      error('Nelson:ode_solvers:sensitivityMethodDirectAdjoint', 'Sensitivity method must be direct or adjoint.');
  end
end

function value = odeNormalizeParameterIndices(value)
  if isempty(value)
    value = [];
    return
  end
  if ~isnumeric(value) || ~isreal(value) || any(~isfinite(value(:))) ...
      || any(value(:) ~= fix(value(:))) || any(value(:) < 1)
    error('Nelson:ode_solvers:parameterindicesContainPositiveIntegerValues', 'ParameterIndices must contain positive integer values.');
  end
  value = value(:);
end

function value = odeNormalizeSensitivityInitial(value, name)
  if ~isempty(value) && (~isnumeric(value) || any(~isfinite(value(:))))
    error('Nelson:ode_solvers:mustBeFiniteNumeric', '%s must contain finite numeric values.', name);
  end
end
%=============================================================================
function value = odeNormalizeSensitivityFunction(value, name)
  if ~isempty(value) && ~isa(value, 'function_handle')
    error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', name);
  end
end
%=============================================================================
function value = odeNormalizeObjectiveTime(value)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ~isreal(value) || ~isscalar(value) || ~isfinite(value)
    error('Nelson:ode_solvers:mustBeFiniteScalar', '%s must be a finite scalar.', 'ObjectiveTime');
  end
end
%=============================================================================
function value = odeNormalizeAdjointTolerance(value, name)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ~isreal(value) || ~isscalar(value) || ~isfinite(value) || value <= 0
    error('Nelson:ode_solvers:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
end
%=============================================================================
