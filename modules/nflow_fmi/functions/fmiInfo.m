%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% info = fmiInfo(fmu)
%
% Reads the model description of an FMI 3.0 FMU and returns its metadata:
%   .modelIdentifier, .fmiVersion, .coSimulation, .modelExchange,
%   .scheduledExecution, and the variable list (.names, .valueReferences,
%   .causalities, .dataTypes).
%
% fmu may be a path to a .fmu archive or to an already-extracted FMU
% directory. A .fmu archive is extracted with the ZIP-slip-hardened unzip
% into a temporary directory that is removed before returning.
%=============================================================================
function info = fmiInfo(fmu)
  narginchk(1, 1);
  mustBeTextScalar(fmu);
  [fmuDir, tempToClean] = fmiEnsureUnzipped(fmu);
  if ~isempty(tempToClean)
    cleaner = onCleanup(@() rmdir(tempToClean, 's')); %#ok<NASGU>
  end
  raw = __nflow_fmi_inspect__(fmuDir);
  % Present a clean, documented public API (consistent with fmiCoSimulate),
  % decoupled from the internal builtin field naming.
  info = struct();
  info.modelIdentifier = raw.modelIdentifier;
  info.fmiVersion = raw.fmiVersion;
  info.coSimulation = raw.coSimulation;
  info.modelExchange = raw.modelExchange;
  info.scheduledExecution = raw.scheduledExecution;
  info.names = raw.variableNames;
  info.valueReferences = raw.variableValueReferences;
  info.causalities = raw.variableCausality;
  info.dataTypes = raw.variableDataType;
  % Per-variable descriptions and Float64 start values (NaN when the variable
  % declares no start value). Aligned element by element with the fields above.
  if isfield(raw, 'variableDescriptions')
    info.descriptions = raw.variableDescriptions;
  end
  if isfield(raw, 'variableStartValues')
    info.startValues = raw.variableStartValues;
  end
end
%=============================================================================
