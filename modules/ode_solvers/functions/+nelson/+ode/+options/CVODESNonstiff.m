%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef CVODESNonstiff < nelson.ode.Options
  properties
    InitialStep = []
    MaxStep = []
    MinStep = []
    NormControl = 'off'
    OutputFcn = []
    OutputSelection = []
    Vectorization = 'off'
    MaxOrder = 12
    LinearSolver = 'auto'
    Preconditioner = 'auto'
  end

  methods
    function obj = CVODESNonstiff(varargin)
      obj.ID = 'cvodesnonstiff';
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
      obj.Vectorization = odeValidateOnOff(value, 'Vectorized');
    end

    function obj = set.MaxOrder(obj, value)
      if ~isnumeric(value) || ~isscalar(value) || value ~= fix(value) || value < 1 || value > 12
        error('Nelson:ode_solvers:maxorderIntegerScalarBetween', 'MaxOrder must be an integer scalar between %d and %d.', 1, 12);
      end
      obj.MaxOrder = value;
    end

    function obj = set.LinearSolver(obj, value)
      obj.LinearSolver = odeValidateSundialsLinearSolver(value);
    end

    function obj = set.Preconditioner(obj, value)
      obj.Preconditioner = odeValidateSundialsPreconditioner(value);
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
