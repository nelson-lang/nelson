%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ODE23s < nelson.ode.Options
  properties
    InitialStep = []
    MaxStep = []
    MinStep = []
    NormControl = 'off'
    OutputFcn = []
    OutputSelection = []
    Vectorization = 'off'
  end

  methods
    function obj = ODE23s(varargin)
      obj.ID = 'ode23s';
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
