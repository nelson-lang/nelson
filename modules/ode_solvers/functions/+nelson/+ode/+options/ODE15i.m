%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ODE15i < nelson.ode.Options
  properties
    InitialStep = []
    MaxStep = []
    MinStep = []
    NormControl = 'off'
    OutputFcn = []
    OutputSelection = []
    Vectorization = {'off', 'off'}
    MaxOrder = 5
    ComputeConsistentInitialConditions = true
  end

  methods
    function obj = ODE15i(varargin)
      obj.ID = 'ode15i';
      obj.DefaultRefine = 1;
      obj = odeApplySolverOptionPairs(obj, varargin{:});
    end

    function s = struct(obj)
      s = odeSolverOptionsToStruct(obj);
    end

    function tf = isDefault(obj)
      tf = odeIsDefaultSolverOptions(obj);
    end

    function obj = set.InitialStep(obj, value)
      obj.InitialStep = odeValidatePositiveScalarOrEmpty(value, 'InitialStep');
    end

    function obj = set.MaxStep(obj, value)
      obj.MaxStep = odeValidatePositiveScalarOrEmpty(value, 'MaxStep');
    end

    function obj = set.MinStep(obj, value)
      obj.MinStep = odeValidatePositiveScalarOrEmpty(value, 'MinStep');
    end

    function obj = set.Refine(obj, value)
      obj.Refine = odeValidatePositiveIntegerOrEmpty(value, 'Refine');
    end

    function obj = set.NormControl(obj, value)
      obj.NormControl = odeValidateOnOff(value, 'NormControl');
    end

    function obj = set.Vectorization(obj, value)
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
      obj.Vectorization = value;
    end

    function obj = set.MaxOrder(obj, value)
      if ~isnumeric(value) || ~isscalar(value) || value ~= fix(value) || value < 1 || value > 5
        error('Nelson:ode_solvers:maxorderIntegerScalarBetween', 'MaxOrder must be an integer scalar between %d and %d.', 1, 5);
      end
      obj.MaxOrder = value;
    end

    function obj = set.ComputeConsistentInitialConditions(obj, value)
      if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
        error('Nelson:ode_solvers:computeconsistentinitialconditionsScalarLogicalValue', 'ComputeConsistentInitialConditions must be a scalar logical value.');
      end
      obj.ComputeConsistentInitialConditions = logical(value);
    end

    function obj = set.OutputFcn(obj, value)
      if ~(isempty(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
      obj.OutputFcn = value;
    end

    function obj = set.OutputSelection(obj, value)
      if ~(isempty(value) || (isnumeric(value) && isvector(value) && all(value(:) >= 1)))
        error('Nelson:ode_solvers:outputselectionIndicesInvalid', '%s indices are invalid.', 'OutputSelection');
      end
      obj.OutputSelection = value;
    end
  end
end
%=============================================================================
