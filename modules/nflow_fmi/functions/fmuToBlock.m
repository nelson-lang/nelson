%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% block = fmuToBlock(fmu)
% block = fmuToBlock(fmu, 'Name', value, ...)
%
% Turn an FMU into a native nflow block manifest entry: it reads the FMU model
% description and produces a block (same schema as a libraries/*/library.json
% block) with one input port per FMU input, one output port per FMU output, the
% FMU parameters, its icon (when the FMU ships a model.png), and a default `path`
% parameter pointing at the .fmu. Placing the block yields a working `fmu` block
% that simulates through the existing FMI path -- no engine change.
%
%   fmu   a path to a .fmu archive or an already-extracted FMU directory.
%
% Name/value options:
%   'Name'         the block label / library title base (default: the FMU model
%                  identifier).
%   'IconDir'      directory to extract the FMU icon (model.png) into (default:
%                  the directory of 'LibraryFile', else tempdir()).
%   'LibraryFile'  when given, also write a one-block library.json to this path.
%   'LibraryId'    the library id (default: 'user.<name>').
%
% Returns the block manifest as a struct. With 'LibraryFile', the library JSON is
% written and its path is echoed.
%
% See also: fmiInfo, modelicaToFmu.
%=============================================================================
function block = fmuToBlock(fmu, varargin)
  narginchk(1, inf);
  mustBeTextScalar(fmu);
  opts = fmuToBlock_options(varargin);

  info = fmiInfo(fmu);
  inspectInfo = fmuToBlock_inspect(fmu);
  name = opts.Name;
  if isempty(name)
    name = info.modelIdentifier;
  end
  if isempty(name)
    name = 'FMU';
  end

  inputs = fmuToBlock_bycausality(info, 'input');
  outputs = fmuToBlock_bycausality(info, 'output');
  params = fmuToBlock_bycausality(info, 'parameter');
  paramDefaults = fmuToBlock_param_defaults(info, params);

  % Geometry: height grows with the busier side; ports spread evenly.
  width = 120;
  nRows = max([numel(inputs), numel(outputs), 1]);
  height = max(64, 24 + 20 * nRows);

  block = struct();
  block.type = 'fmu'; % the engine handler
  block.label = name;
  block.fmiVersion = info.fmiVersion;
  block.inputs = fmuToBlock_ports(inputs, 0, height, 'left');
  block.outputs = fmuToBlock_ports(outputs, width, height, 'right');
  block.parameters = params; % FMU parameter names
  block.parameterDefaults = paramDefaults; % struct name -> start value
  block.width = width;
  block.height = height;
  block.defaultParams = struct('path', strrep(fmuToBlock_pathstr(fmu), '\', '/'));
  block.defaultParams.numInputs = numel(inputs);
  block.defaultParams.inputNames = inputs;
  block.defaultParams.inputWidths = fmuToBlock_widths(inspectInfo, 'input');
  block.defaultParams.numOutputs = numel(outputs);
  block.defaultParams.outputNames = outputs;
  block.defaultParams.outputWidths = fmuToBlock_widths(inspectInfo, 'output');
  block.defaultParams.parameterNames = params;
  block.defaultParams.parameterValues = paramDefaults;
  if ~isempty(inspectInfo) && isfield(inspectInfo, 'numContinuousStates')
    block.defaultParams.numContinuousStates = inspectInfo.numContinuousStates;
  end
  if ~isempty(inspectInfo) && isfield(inspectInfo, 'numEventIndicators')
    block.defaultParams.numEventIndicators = inspectInfo.numEventIndicators;
  end

  % Icon: extract the FMU's model.png when present, else a labelled fallback.
  iconDir = opts.IconDir;
  if isempty(iconDir)
    if ~isempty(opts.LibraryFile)
      iconDir = fileparts(opts.LibraryFile);
    else
      iconDir = tempdir();
    end
  end
  iconFile = fmuToBlock_extract_icon(fmu, name, iconDir);
  if ~isempty(iconFile)
    block.icon = fmuToBlock_basename(iconFile);
    block.render = struct('type', 'image', 'src', fmuToBlock_basename(iconFile));
  else
    block.render = struct('type', 'label', 'text', name);
  end

  if ~isempty(opts.LibraryFile)
    fmuToBlock_write_library(opts.LibraryFile, opts.LibraryId, name, block);
  end
end
%=============================================================================
function inspectInfo = fmuToBlock_inspect(fmu)
  inspectInfo = [];
  try
    j = jsondecode(__nflow_fmi_inspect_json__(fmu));
    if isfield(j, 'success') && isequal(j.success, true)
      inspectInfo = j;
    end
  catch
    inspectInfo = [];
  end
end
%=============================================================================
function widths = fmuToBlock_widths(inspectInfo, causality)
  widths = [];
  if isempty(inspectInfo) || ~isfield(inspectInfo, 'variables')
    return
  end
  vars = inspectInfo.variables;
  if ~iscell(vars)
    vars = num2cell(vars);
  end
  widths = [];
  for k = 1:numel(vars)
    v = vars{k};
    if isfield(v, 'causality') && strcmp(v.causality, causality)
      w = 1;
      if isfield(v, 'arraySize') && isnumeric(v.arraySize) && isscalar(v.arraySize)
        w = max(1, double(v.arraySize));
      end
      widths(end + 1) = w; %#ok<AGROW>
    end
  end
end
%=============================================================================
function opts = fmuToBlock_options(args)
  opts = struct('Name', '', 'IconDir', '', 'LibraryFile', '', 'LibraryId', '');
  if mod(numel(args), 2) ~= 0
    error('Nelson:nflow_fmi:fmuToBlockArg', 'Options must be given as name/value pairs.');
  end
  for k = 1:2:numel(args)
    nm = char(args{k});
    switch nm
      case {'Name', 'IconDir', 'LibraryFile', 'LibraryId'}
        opts.(nm) = char(args{k + 1});
      otherwise
        error('Nelson:nflow_fmi:unknownOption', 'Unknown option ''%s''.', nm);
    end
  end
  if isempty(opts.LibraryId) && ~isempty(opts.Name)
    opts.LibraryId = ['user.', lower(opts.Name)];
  end
end
%=============================================================================
function names = fmuToBlock_bycausality(info, causality)
  names = {};
  if ~isfield(info, 'causalities') || ~isfield(info, 'names')
    return
  end
  for k = 1:numel(info.causalities)
    if strcmp(info.causalities{k}, causality)
      names{end + 1} = info.names{k}; %#ok<AGROW>
    end
  end
end
%=============================================================================
function defaults = fmuToBlock_param_defaults(info, paramNames)
  % Map each parameter name to its Float64 start value (0 when the FMU declares
  % none, i.e. NaN in startValues). Returns a scalar struct name -> value.
  defaults = struct();
  if ~isfield(info, 'startValues') || ~isfield(info, 'names')
    return
  end
  for k = 1:numel(paramNames)
    idx = find(strcmp(info.names, paramNames{k}), 1);
    value = 0;
    if ~isempty(idx) && idx <= numel(info.startValues) && ~isnan(info.startValues(idx))
      value = info.startValues(idx);
    end
    defaults.(fmuToBlock_valid_field(paramNames{k})) = value;
  end
end
%=============================================================================
function f = fmuToBlock_valid_field(name)
  % A struct-field-safe key from an FMU variable name (which may be dotted).
  f = regexprep(char(name), '\W', '_');
  if isempty(f) || ~isempty(regexp(f(1), '[^A-Za-z]', 'once'))
    f = ['p_', f];
  end
end
%=============================================================================
function ports = fmuToBlock_ports(names, x, height, side)
  ports = {};
  n = numel(names);
  for k = 1:n
    y = round(height * k / (n + 1));
    ports{end + 1} = struct('x', x, 'y', y, 'side', side, 'name', names{k}); %#ok<AGROW>
  end
end
%=============================================================================
function iconFile = fmuToBlock_extract_icon(fmu, name, iconDir)
  % Best-effort: FMI ships an optional icon at the archive root (model.png).
  iconFile = '';
  [fmuDir, tempToClean] = fmiEnsureUnzipped(fmu);
  cleaner = onCleanup(@() fmuToBlock_cleanup(tempToClean));
  candidates = {[fmuDir, '/model.png'], [fmuDir, '/icons/model.png']};
  for k = 1:numel(candidates)
    if isfile(candidates{k})
      if ~isfolder(iconDir)
        mkdir(iconDir);
      end
      iconFile = [iconDir, '/', name, '.png'];
      copyfile(candidates{k}, iconFile);
      return
    end
  end
end
%=============================================================================
function fmuToBlock_cleanup(tempToClean)
  if ~isempty(tempToClean) && isfolder(tempToClean)
    rmdir(tempToClean, 's');
  end
end
%=============================================================================
function fmuToBlock_write_library(libFile, libId, name, block)
  if isempty(libId)
    libId = ['user.', lower(name)];
  end
  lib = struct();
  lib.id = libId;
  lib.title = name;
  lib.version = '1.0.0';
  lib.format = 'nflow-2';
  lib.comment = ['Imported FMU block: ', name, '.'];
  lib.blocks = {block};
  fid = fopen(libFile, 'w');
  if fid < 0
    error('Nelson:nflow_fmi:fmuToBlockWrite', 'Could not write the library to ''%s''.', libFile);
  end
  fwrite(fid, jsonencode(lib));
  fclose(fid);
end
%=============================================================================
function s = fmuToBlock_pathstr(fmu)
  s = char(fmu);
end
%=============================================================================
function b = fmuToBlock_basename(p)
  [~, n, e] = fileparts(p);
  b = [n, e];
end
%=============================================================================
