%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_run__()
  arguments = argv();
  executable = arguments{1};
  originalDirectory = pwd();
  originalPath = path();
  directory = getenv('NELSONC_EXTRACTION_DIRECTORY');
  if isempty(directory)
    error('Nelson:nelsonc:environment', 'Missing native application extraction directory.');
  end
  [currentDirectorySearch, userPath] = __nelsonc_environment__(false, '');
  cleanup = onCleanup(@() restore_environment(originalDirectory, originalPath, currentDirectorySearch, userPath));
  __application_archive__('extract', executable, directory);
  encodedManifest = fileread(fullfile(directory, 'application.json'));
  __bytecode_artifact__('verify', encodedManifest);
  manifest = jsondecode(encodedManifest);
  if ~isstruct(manifest) || ~isscalar(manifest) || ~isfield(manifest, 'format') ...
    || (~isequal(manifest.format, 1) && ~isequal(manifest.format, 2))
    error('Nelson:nelsonc:manifest:incompatibleApplicationManifest', 'Incompatible application manifest.');
  end
  numericInputs = false;
  if isequal(manifest.format, 2)
    if ~isfield(manifest, 'treatInputsAsNumeric') ...
      || ~islogical(manifest.treatInputsAsNumeric) || ~isscalar(manifest.treatInputsAsNumeric)
      error('Nelson:nelsonc:manifest:invalidApplicationInputConversion', 'Invalid application input conversion.');
    end
    numericInputs = manifest.treatInputsAsNumeric;
  elseif isfield(manifest, 'treatInputsAsNumeric')
    error('Nelson:nelsonc:manifest:inputConversionRequiresManifestVersion2', 'Input conversion requires application manifest version 2.');
  end
  required = {'entry', 'entryKind', 'entryName', 'gui', 'paths'};
  for k = 1:numel(required)
    if ~isfield(manifest, required{k})
      error('Nelson:nelsonc:manifest:incompleteApplicationManifest', 'Incomplete application manifest.');
    end
  end
  validate_relative(manifest.entry);
  if ~iscell(manifest.paths) || isempty(manifest.paths) || ~islogical(manifest.gui) || ~isscalar(manifest.gui)
    error('Nelson:nelsonc:manifest:invalidApplicationManifest', 'Invalid application manifest.');
  end
  for k = 1:numel(manifest.paths)
    validate_relative(manifest.paths{k});
  end
  if ~any(strcmp(manifest.entryKind, {'script', 'function'})) || ~ischar(manifest.entryName) ...
    || isempty(regexp(manifest.entryName, '^[A-Za-z_][A-Za-z_0-9]*(\.[A-Za-z_][A-Za-z_0-9]*)*$', 'once'))
    error('Nelson:nelsonc:manifest:invalidApplicationEntry', 'Invalid application entry.');
  end
  for k = numel(manifest.paths):-1:1
    addpath(fullfile(directory, manifest.paths{k}));
  end
  entry = fullfile(directory, manifest.entry);
  if isfield(manifest, 'nativePaths') && ~isempty(manifest.nativePaths)
    if ~iscell(manifest.nativePaths) || numel(manifest.nativePaths) > 256
      error('Nelson:nelsonc:manifest:invalidNativeApplicationPaths', 'Invalid native application paths.');
    end
    nativePaths = {};
    for k = 1:numel(manifest.nativePaths)
      validate_relative(manifest.nativePaths{k});
      nativePaths{k} = fullfile(directory, manifest.nativePaths{k});
    end
    previousNativePaths = __library_search_paths__(nativePaths);
    nativeCleanup = onCleanup(@() __library_search_paths__(previousNativePaths));
  end
  cd(getenv('NELSONC_INVOKING_DIRECTORY'));
  previousDeployment = __nelsonc_environment__('deployment', directory);
  deploymentCleanup = onCleanup(@() __nelsonc_environment__('deployment', previousDeployment));
  inputs = argv('user');
  if __nelsonc_application_help__(manifest, inputs)
    return;
  end
  if strcmp(manifest.entryKind, 'script')
    execute_script(entry);
  else
    if numericInputs
      for k = 1:numel(inputs)
        inputs{k} = builtin('str2double', inputs{k});
      end
    end
    feval(manifest.entryName, inputs{:});
  end
  if manifest.gui
    __nelsonc_wait_for_windows__();
  end
  clear deploymentCleanup;
  clear nativeCleanup;
  clear cleanup;
end

function execute_script(entry)
  run(entry, false);
end

function validate_relative(name)
  if ~ischar(name) || ~isrow(name) || ~startsWith(name, 'roots/') ...
    || any(name == '\') || any(name == ':') || any(name == char(0))
    error('Nelson:nelsonc:manifest', 'Invalid application path.');
  end
  components = strsplit(name, '/');
  if any(strcmp(components, '..')) || any(strcmp(components, '.')) || any(cellfun('isempty', components))
    error('Nelson:nelsonc:manifest', 'Invalid application path.');
  end
end

function restore_environment(originalDirectory, searchPath, currentDirectorySearch, userPath)
  cd(originalDirectory);
  path(searchPath);
  __nelsonc_environment__(currentDirectorySearch, userPath);
end
