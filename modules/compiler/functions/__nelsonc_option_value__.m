%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = __nelsonc_option_value__(name, value)
  switch name
    case {'AutoDetectDataFiles', 'Verbose', 'NoConsole', 'EmbedArchive', 'TreatInputsAsNumeric'}
      value = boolean_value(value, name);
      if strcmp(name, 'NoConsole') && value && ~ispc()
        error('Nelson:nelsonc:platform', 'No-console applications require Windows.');
      end
    case 'AdditionalFiles'
      value = additional_files(value);
    case 'SupportPackages'
      value = support_packages(value);
    case 'ExecutableVersion'
      value = text_value(value, name, false);
      __nelsonc_executable__('validate-version', value);
    case 'RuntimeLogFile'
      value = text_value(value, name, true);
      __nelsonc_executable__('validate-log-file', value);
    case {'ExecutableIcon', 'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}
      value = text_value(value, name, true);
      if isempty(value); return; end
      % Validate the requested value before gating on platform support, so an
      % invalid image is always an options error regardless of the host.
      value = absolute_path(value);
      [parent, stem, extension] = fileparts(value);
      if ~any(strcmpi(extension, {'.jpg', '.jpeg', '.png', '.bmp', '.gif'})) || ~isfile(value)
        error('Nelson:nelsonc:options:imageFileExpected', '%s must name an existing JPG, JPEG, PNG, BMP or GIF image.', name);
      end
      if ~ispc()
        error('Nelson:nelsonc:platform:resourcesCurrentlyRequireWindows', '%s resources currently require Windows.', name);
      end
    case {'Mode', 'RuntimeMode'}
      value = lower(text_value(value, name, false));
      choices = {'auto', 'cli', 'gui'};
      if strcmp(name, 'RuntimeMode')
        choices = {'bundled', 'installed'};
      end
      if ~any(strcmp(value, choices))
        error('Nelson:nelsonc:options:invalidValue', 'Invalid %s: %s.', name, value);
      end
    case 'ExecutableName'
      value = text_value(value, name, false);
      if isempty(regexp(value, '^[A-Za-z_][A-Za-z_0-9]*$', 'once')) ...
        || (ispc() && ~isempty(regexpi(value, '^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$', 'once')))
        error('Nelson:nelsonc:options:invalidExecutableName', 'Invalid executable name: %s.', value);
      end
    case {'AppFile', 'OutputDir', 'CustomHelpTextFile'}
      value = text_value(value, name, strcmp(name, 'CustomHelpTextFile'));
      if isempty(value)
        return;
      end
      value = absolute_path(value);
      if strcmp(name, 'AppFile')
        [parent, stem, extension] = fileparts(value);
        if ~strcmp(extension, '.m') || ~isfile(value)
          error('Nelson:nelsonc:options:appFileNameExistingMSource', 'AppFile must name an existing .m source file.');
        end
      elseif strcmp(name, 'CustomHelpTextFile') && ~isfile(value)
        error('Nelson:nelsonc:options:customHelpTextFileNameExistingFile', 'CustomHelpTextFile must name an existing file.');
      elseif strcmp(name, 'OutputDir') && isfile(value)
        error('Nelson:nelsonc:options:outputDirDirectory', 'OutputDir must be a directory.');
      end
    otherwise
      error('Nelson:nelsonc:options:unknownBuildOption', 'Unknown build option: %s.', name);
  end
end

function value = text_value(value, name, allowEmpty)
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ~ischar(value) || (~isempty(value) && ~isrow(value)) ...
    || (~allowEmpty && isempty(value)) || any(value == char(0))
    error('Nelson:nelsonc:options:characterVectorScalarString', '%s must be a character vector or scalar string.', name);
  end
end

function value = boolean_value(value, name)
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ischar(value) && isrow(value) && any(strcmpi(value, {'on', 'off'}))
    value = strcmpi(value, 'on');
  elseif (islogical(value) || isnumeric(value)) && isscalar(value) ...
    && isreal(value) && (value == 0 || value == 1)
    value = logical(value);
  else
    error('Nelson:nelsonc:options:onOffValueExpected', '%s must be on, off, true, false, zero or one.', name);
  end
end

function files = additional_files(value)
  if ischar(value)
    if isempty(value)
      value = {};
    else
      value = {value};
    end
  elseif isstring(value)
    value = cellstr(value);
  end
  if ~iscell(value) || (~isempty(value) && ~isvector(value)) || numel(value) > 10000
    error('Nelson:nelsonc:options:additionalFilesFilenameVectorFilenames', 'AdditionalFiles must be a filename or a vector of filenames.');
  end
  files = {};
  for k = 1:numel(value)
    files{k} = absolute_path(text_value(value{k}, 'AdditionalFiles', false));
  end
end

function name = absolute_path(name)
  % Keep links visible to the input selector; fullpath would resolve them here.
  absolute = startsWith(name, '/');
  if ispc()
    absolute = absolute || startsWith(name, '\');
    if length(name) >= 2 && name(2) == ':'
      if length(name) < 3 || ~any(name(3) == '/\')
        error('Nelson:nelsonc:options:driveRelativePathsNotSupported', 'Drive-relative paths are not supported.');
      end
      absolute = true;
    elseif absolute && ~startsWith(name, '//') && ~startsWith(name, '\\')
      directory = pwd();
      if length(directory) < 2 || directory(2) ~= ':'
        error('Nelson:nelsonc:options:driveQualifiedPathRequired', 'A drive-qualified path is required.');
      end
      name = [directory(1:2), name];
    end
  end
  if ~absolute
    name = fullfile(pwd(), name);
  end
  if ispc()
    name = strrep(name, '\', '/');
  end
end

function names = support_packages(value)
  if ischar(value); value = {value}; elseif isstring(value); value = cellstr(value); end
  if ~iscell(value) || isempty(value) || ~isvector(value) || numel(value) > 10000
    error('Nelson:nelsonc:options:supportPackagesAutodetectNoneNonemptyVector', 'SupportPackages must be autodetect, none or a nonempty vector of package names.');
  end
  names = cell(1, numel(value));
  for k = 1:numel(value)
    names{k} = text_value(value{k}, 'SupportPackages', false);
    if ~isvarname(names{k})
      error('Nelson:nelsonc:options:invalidRegisteredPackageName', 'Invalid registered package name: %s.', names{k});
    end
  end
  if numel(names) > 1 && any(ismember(names, {'autodetect', 'none'}))
    error('Nelson:nelsonc:options:autodetectNoneCannotCombinedPackage', 'autodetect and none cannot be combined with package names.');
  end
  names = unique(names);
end
