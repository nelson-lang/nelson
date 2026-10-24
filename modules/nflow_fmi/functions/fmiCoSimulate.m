%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% result = fmiCoSimulate(fmu, tStop)
% result = fmiCoSimulate(fmu, tStop, dt)
% result = fmiCoSimulate(fmu, tStop, dt, inputs)
%
% Runs a fixed-step Co-Simulation of an FMI 3.0 FMU and records every Float64
% output at each communication point. Returns a struct:
%   .time        (N x 1)      communication points
%   .outputNames (1 x nOut)   output variable names
%   .outputs     (N x nOut)   output values over time
%
% fmu may be a path to a .fmu archive or to an already-extracted FMU
% directory. dt defaults to tStop/100 when omitted or empty.
%
% inputs is an optional scalar struct whose field names are FMU Float64 input
% variables and whose values are the constant values held on those inputs for
% the whole run, for example struct('u', 2.5). Inputs left out keep their FMU
% start values.
%=============================================================================
function result = fmiCoSimulate(fmu, tStop, dt, inputs, relTol)
  narginchk(2, 5);
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
  % Optional relative tolerance for the FMU's internal solver.
  haveRelTol = (nargin >= 5) && ~isempty(relTol);
  if haveRelTol
    mustBeReal(relTol);
    mustBeScalarOrEmpty(relTol);
  end
  step = dt;
  if ~haveDt
    step = tStop / 100;
  end
  names = {};
  values = [];
  if nargin >= 4 && ~isempty(inputs)
    if ~isstruct(inputs) || ~isscalar(inputs)
      error('Nelson:nflow_fmi:badInputs:inputsStruct', 'inputs must be a scalar struct (field = FMU input name, value = constant).');
    end
    names = fieldnames(inputs);
    values = zeros(1, numel(names));
    for i = 1:numel(names)
      values(i) = double(inputs.(names{i}));
    end
  end
  if haveRelTol
    result = __nflow_fmi_cosim__(fmuDir, tStop, step, names, values, relTol);
  elseif ~isempty(names)
    result = __nflow_fmi_cosim__(fmuDir, tStop, step, names, values);
  elseif haveDt
    result = __nflow_fmi_cosim__(fmuDir, tStop, dt);
  else
    result = __nflow_fmi_cosim__(fmuDir, tStop);
  end
end
%=============================================================================
