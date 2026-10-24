%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% result = fmiModelExchange(fmu, tStop)
% result = fmiModelExchange(fmu, tStop, dt)
%
% Imports an FMI 3.0 Model Exchange FMU and integrates it with Nelson's own
% fixed-step RK4 solver (with step-boundary state-event handling). Unlike
% Co-Simulation, the FMU exposes only its equations and Nelson owns the solver.
% Records every Float64 output at each step. Returns a struct:
%   .time        (N x 1)      time points
%   .outputNames (1 x nOut)   output variable names
%   .outputs     (N x nOut)   output values over time
%
% fmu may be a path to a .fmu archive or to an already-extracted FMU
% directory. dt defaults to tStop/1000 when omitted (Model Exchange needs a
% finer step than Co-Simulation, since Nelson integrates the states).
%=============================================================================
function result = fmiModelExchange(fmu, tStop, dt, relTol)
  narginchk(2, 4);
  mustBeTextScalar(fmu);
  mustBeReal(tStop);
  mustBeScalarOrEmpty(tStop);
  [fmuDir, tempToClean] = fmiEnsureUnzipped(fmu);
  if ~isempty(tempToClean)
    cleaner = onCleanup(@() rmdir(tempToClean, 's')); %#ok<NASGU>
  end
  haveDt = (nargin >= 3) && ~isempty(dt);
  if haveDt
    mustBeReal(dt);
    mustBeScalarOrEmpty(dt);
  end
  % Optional relative tolerance passed to the FMU at initialization.
  haveRelTol = (nargin >= 4) && ~isempty(relTol);
  if haveRelTol
    mustBeReal(relTol);
    mustBeScalarOrEmpty(relTol);
    step = dt;
    if ~haveDt
      step = tStop / 100;
    end
    result = __nflow_fmi_modelexchange__(fmuDir, tStop, step, relTol);
  elseif haveDt
    result = __nflow_fmi_modelexchange__(fmuDir, tStop, dt);
  else
    result = __nflow_fmi_modelexchange__(fmuDir, tStop);
  end
end
%=============================================================================
