%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ODEResults
  properties
    Time = []
    Solution = []
    Sensitivity = []
    EventTime = []
    EventSolution = []
    EventIndex = []
    EventSensitivity = []
    AdjointGradient = []
  end

  properties (Hidden)
    RawSolution = []
    SolutionValues = []
    Stats = []
  end

  methods
    function obj = ODEResults(sol)
      if nargin == 0
        return
      end
      if ~isstruct(sol) || ~isfield(sol, 'x') || ~isfield(sol, 'y')
        error('Nelson:ode_solvers:odeSolutionStructureExpected', 'ODE solution structure expected.');
      end
      obj.RawSolution = sol;
      obj.Time = sol.x;
      obj.Solution = sol.y;
      obj.SolutionValues = sol.y.';
      if isfield(sol, 'xe')
        obj.EventTime = sol.xe;
      end
      if isfield(sol, 'ye')
        obj.EventSolution = sol.ye;
      end
      if isfield(sol, 'ie')
        obj.EventIndex = sol.ie;
      end
      if isfield(sol, 'sensitivity')
        obj.Sensitivity = sol.sensitivity;
      else
        obj.Sensitivity = [];
      end
      if isfield(sol, 'eventSensitivity')
        obj.EventSensitivity = sol.eventSensitivity;
      else
        obj.EventSensitivity = [];
      end
      if isfield(sol, 'adjointGradient')
        obj.AdjointGradient = sol.adjointGradient;
      else
        obj.AdjointGradient = [];
      end
      if isfield(sol, 'stats')
        obj.Stats = sol.stats;
      end
    end
  end
end
%=============================================================================
