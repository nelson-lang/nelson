%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fmu = modelicaToFmu(model)
% fmu = modelicaToFmu(model, modelName)
% fmu = modelicaToFmu(model, modelName, 'Name', value, ...)
%
% Compile a Modelica model to an FMU with OpenModelica and return the path to the
% produced .fmu. This is the compilation half of the nflow Modelica bridge: a
% `modelica` block calls it to turn Modelica into an FMU that nflow then imports
% through its FMI path.
%
%   model      a path to a .mo file, or inline Modelica source text.
%   modelName  the Modelica class to build. Optional: when omitted it is the
%              .mo base name (file input) or parsed from the first
%              model/block/class/package declaration (inline source).
%
% Name/value options:
%   'FmiVersion'  '2.0' (default) | '3.0'
%   'FmuType'     'cs'  (default, Co-Simulation) | 'me' | 'me_cs'
%   'Libraries'   cellstr of extra .mo files to loadFile before building
%   'Rebuild'     false (default) | true  -- ignore the cache and recompile
%
% The result is cached under tempdir() keyed by a hash of the source, model name,
% OpenModelica version and FMU options, so an unchanged model is compiled once.
% The cache is shared by every Nelson process on the machine, so a build never
% happens inside it: it runs in a private directory and is published as a whole
% entry once it succeeded. An entry is never written into nor removed, because
% another process may be reading it -- including under 'Rebuild', which
% recompiles and keeps its fresh FMU private when an entry already exists.
%
% Raises:
%   Nelson:nflow_fmi:modelicaUnavailable    OpenModelica is missing or cannot export
%                                        FMUs (the simulation is blocked).
%   Nelson:nflow_fmi:modelicaCompileFailed  omc ran but did not produce the FMU;
%                                        the message carries the omc diagnostics.
%
% See also: modelicaInfo, modelicaConfigure.
%=============================================================================
function fmu = modelicaToFmu(model, modelName, varargin)
  narginchk(1, inf);
  nargoutchk(0, 1);
  mustBeTextScalar(model);
  if nargin < 2
    modelName = '';
  end
  opts = modelica_parse_options(varargin);

  % Resolve the source: an existing file, or inline Modelica text written out.
  model = char(model);
  isFile = isfile(model);

  % Fail fast on a model given as a file path that does not exist. Without this
  % a missing .mo path (e.g. an example not deployed, or a mistyped path) would
  % fall through to the inline-source branch and be written verbatim into a .mo
  % file, surfacing only as a cryptic OpenModelica parser error such as
  % "Unexpected token near: D (IDENT)" (the Windows drive letter of the path).
  if ~isFile && modelica_looks_like_file_path(model)
    error('Nelson:nflow_fmi:modelicaToFmuFileNotFound', ...
      'The Modelica model file ''%s'' does not exist.', model);
  end

  info = modelicaInfo();
  if ~info.capable
    error('Nelson:nflow_fmi:modelicaUnavailable', ...
      'The Modelica block needs OpenModelica to compile a model to an FMU, but it is not usable: %s', ...
      info.reason);
  end

  if isFile
    moPath = modelica_abspath(model);
    source = fileread(moPath);
    if isempty(modelName)
      [~, modelName] = fileparts(moPath);
    end
  else
    source = model;
    if isempty(modelName)
      modelName = modelica_parse_model_name(source);
    end
  end
  if isempty(modelName)
    error('Nelson:nflow_fmi:modelicaToFmuArg', ...
      'Could not determine the Modelica model name; pass it explicitly as the second argument.');
  end

  % Cache key: source + name + omc version + FMU options.
  keyText = [source, '|', modelName, '|', info.version, '|', opts.FmiVersion, '|', opts.FmuType];
  for k = 1:numel(opts.Libraries)
    keyText = [keyText, '|lib:', opts.Libraries{k}]; %#ok<AGROW>
  end
  key = sha256(keyText);
  cacheRoot = [tempdir(), '/nflow_modelica_cache'];
  cacheDir = [cacheRoot, '/', key];
  fmu = [cacheDir, '/', modelName, '.fmu'];
  if isfile(fmu) && ~opts.Rebuild
    return
  end

  % Build in a PRIVATE directory, never in the cache entry. The key hashes the
  % model only, so two Nelson processes compiling the same model aim at the same
  % entry: building in place had one of them wipe the directory the other was
  % compiling into, or reading its FMU out of.
  buildDir = tempname();
  [made, madeMessage] = mkdir(buildDir);
  if ~made
    error('Nelson:nflow_fmi:modelicaToFmuMkdir', ...
      'Could not create the build directory ''%s'': %s', buildDir, madeMessage);
  end
  if ~isFile
    moPath = [buildDir, '/', modelName, '.mo'];
    fid = fopen(moPath, 'w');
    if fid < 0
      modelica_remove_dir(buildDir);
      error('Nelson:nflow_fmi:modelicaToFmuWrite', 'Could not write the Modelica source to ''%s''.', moPath);
    end
    fwrite(fid, source);
    fclose(fid);
  end

  buildFmu = [buildDir, '/', modelName, '.fmu'];
  [ok, out] = modelica_run_omc(info.omc, buildDir, moPath, modelName, opts);
  if ~ok || ~isfile(buildFmu)
    modelica_remove_dir(buildDir); % our own scratch, never a shared cache entry
    error('Nelson:nflow_fmi:modelicaCompileFailed', ...
      'OpenModelica failed to build an FMU for model ''%s''.\n%s', modelName, out);
  end
  fmu = modelica_publish_entry(cacheRoot, cacheDir, fmu, buildDir, buildFmu, opts.Rebuild);
end
%=============================================================================
function opts = modelica_parse_options(args)
  opts = struct('FmiVersion', '2.0', 'FmuType', 'cs', 'Libraries', {{}}, 'Rebuild', false);
  if mod(numel(args), 2) ~= 0
    error('Nelson:nflow_fmi:modelicaToFmuArg:optionsGivenNameValuePairs', 'Options must be given as name/value pairs.');
  end
  for k = 1:2:numel(args)
    name = args{k};
    if ~(ischar(name) || (isstring(name) && isscalar(name)))
      error('Nelson:nflow_fmi:modelicaToFmuArg:optionNamesStrings', 'Option names must be strings.');
    end
    value = args{k + 1};
    switch char(name)
      case 'FmiVersion'
        opts.FmiVersion = char(value);
      case 'FmuType'
        opts.FmuType = char(value);
      case 'Libraries'
        if ischar(value) || (isstring(value) && isscalar(value))
          opts.Libraries = {char(value)};
        elseif iscell(value)
          opts.Libraries = cellfun(@char, value, 'UniformOutput', false);
        elseif isstring(value)
          opts.Libraries = cellstr(value);
        else
          error('Nelson:nflow_fmi:modelicaToFmuArg:librariesAStringACellStrings', '''Libraries'' must be a string or a cell of strings.');
        end
      case 'Rebuild'
        opts.Rebuild = logical(value);
      otherwise
        error('Nelson:nflow_fmi:unknownOption', 'Unknown option ''%s''.', char(name));
    end
  end
end
%=============================================================================
function [ok, out] = modelica_run_omc(omc, workDir, moPath, modelName, opts)
  % Build a .mos script that loads the model (plus any extra libraries) and asks
  % OpenModelica to export an FMU into workDir. getErrorString() after each step
  % surfaces diagnostics in the captured output.
  wd = strrep(workDir, '\', '/');
  mo = strrep(modelica_abspath(moPath), '\', '/');
  lines = {};
  lines{end + 1} = ['cd("', wd, '"); getErrorString();'];
  for k = 1:numel(opts.Libraries)
    lib = strrep(modelica_abspath(opts.Libraries{k}), '\', '/');
    lines{end + 1} = ['loadFile("', lib, '"); getErrorString();']; %#ok<AGROW>
  end
  lines{end + 1} = ['loadFile("', mo, '"); getErrorString();'];
  lines{end + 1} = ['buildModelFMU(', modelName, ', version="', opts.FmiVersion, ...
    '", fmuType="', opts.FmuType, '"); getErrorString();'];
  mos = [wd, '/build.mos'];
  fid = fopen(mos, 'w');
  if fid < 0
    ok = false; out = ['Could not write the OpenModelica script ''', mos, '''.'];
    return
  end
  fwrite(fid, strjoin(lines, sprintf('\n')));
  fclose(fid);
  [st, out] = system(['"', omc, '" "', mos, '"']);
  ok = (st == 0);
end
%=============================================================================
function fmu = modelica_publish_entry(cacheRoot, cacheDir, cachedFmu, buildDir, buildFmu, rebuild)
  % Publish a finished private build as the shared cache entry, and return the
  % path to use.
  %
  % Publishing is a directory rename: buildDir comes from tempname() and the
  % cache lives under tempdir(), so the two share a volume and movefile renames
  % instead of copying -- the entry becomes visible complete, or not at all.
  % Concurrency is handled by losing gracefully rather than by locking: an entry
  % that already exists was published by another process from a build of exactly
  % this input (the key hashes the source, the model name, the OpenModelica
  % version and the FMU options), so it is adopted instead of overwritten.
  fmu = buildFmu; % fallback: hand back our own private build
  if isfolder(cacheDir)
    if rebuild
      % 'Rebuild' wanted a fresh artifact and must not disturb an entry other
      % processes may be reading: keep the private build and return that path.
      return
    end
  else
    modelica_make_dir(cacheRoot); % the rename needs its parent to exist
    published = movefile(buildDir, cacheDir);
    if ~published && ~isfolder(cacheDir)
      return
    end
  end
  if ~isfile(cachedFmu)
    return % unusable entry: keep the private build
  end
  fmu = cachedFmu;
  modelica_remove_dir(buildDir); % still there when another process won
  modelica_remove_dir(modelica_nested_dir(cacheDir, buildDir));
end
%=============================================================================
function p = modelica_nested_dir(cacheDir, buildDir)
  % Where movefile puts the build directory when another process created the
  % cache entry between the isfolder() test and the move: inside it, under the
  % build directory's own name. That copy is ours alone, so it can be removed.
  [~, name, ext] = fileparts(buildDir);
  p = [cacheDir, '/', name, ext];
end
%=============================================================================
function tf = modelica_make_dir(d)
  % mkdir reports a failure when another process creates the same directory
  % first, so what counts is the directory existing afterwards, not the status.
  tf = isfolder(d);
  if ~tf
    tf = mkdir(d) || isfolder(d);
  end
end
%=============================================================================
function tf = modelica_remove_dir(d)
  % Best effort, and only ever on a private directory under tempdir(): a cache
  % entry belongs to whoever is reading it.
  tf = true;
  if isfolder(d)
    tf = rmdir(d, 's');
  end
end
%=============================================================================
function tf = modelica_looks_like_file_path(s)
  % A model argument is meant as a file path -- rather than inline Modelica
  % source -- when it is a single line ending in the Modelica ".mo" extension.
  % Inline source ends with "end <Name>;", never with ".mo", so a real model
  % body is never misread as a (missing) path.
  s = strtrim(char(s));
  tf = ~contains(s, sprintf('\n')) && ~contains(s, sprintf('\r')) && ...
    endsWith(lower(s), '.mo');
end
%=============================================================================
function name = modelica_parse_model_name(source)
  name = '';
  tok = regexp(source, '(?:^|\n)\s*(?:model|block|class|package|connector|record)\s+([A-Za-z_]\w*)', ...
    'tokens', 'once');
  if ~isempty(tok)
    name = tok{1};
  end
end
%=============================================================================
function p = modelica_abspath(p)
  p = char(p);
  % Already absolute? (drive-letter or leading slash)
  if ~isempty(regexp(p, '^[A-Za-z]:', 'once')) || startsWith(p, '/') || startsWith(p, '\')
    p = strrep(p, '\', '/');
    return
  end
  p = strrep([pwd(), '/', p], '\', '/');
end
%=============================================================================
