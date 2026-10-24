%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = __nelsonc_installer_option__(name, value)
  if any(strcmp(name, {'AdditionalFiles', 'OutputDir', 'Verbose', ...
    'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}))
    value = __nelsonc_option_value__(name, value);
    return;
  end
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  multiline = any(strcmp(name, {'Description', 'InstallationNotes'}));
  if ~ischar(value) || (~isempty(value) && ~isrow(value)) || numel(value) > 32768
    error('Nelson:nelsonc:options:characterVectorScalarString', '%s must be a character vector or scalar string.', name);
  end
  controls = value < char(32);
  if multiline
    controls = controls & value ~= char(9) & value ~= char(10) & value ~= char(13);
  end
  if any(controls)
    error('Nelson:nelsonc:options:containsUnsupportedControlCharacters', '%s contains unsupported control characters.', name);
  end
  switch name
    case {'ApplicationName', 'InstallerName'}
      if isempty(value) || any(ismember(value, '/\:*?"<>|')) || any(value(end) == '. ') ...
        || ~isempty(regexpi(value, '^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\.|$)', 'once'))
        error('Nelson:nelsonc:options:validFilenameComponent', '%s must be a valid filename component.', name);
      end
    case {'RuntimeDelivery', 'PackageType', 'OptionalDependencies', 'Compression'}
      choices = {'all', 'none'};
      if strcmp(name, 'Compression')
        choices = {'none', 'fast', 'normal', 'max'};
      elseif strcmp(name, 'RuntimeDelivery')
        choices = {'web', 'installer', 'none'};
      elseif strcmp(name, 'PackageType')
        choices = {'auto', 'zip'};
      end
      if ~any(strcmp(value, choices))
        error('Nelson:nelsonc:options:invalidValue', 'Invalid %s: %s.', name, value);
      end
    case 'Version'
      __nelsonc_executable__('validate-version', value);
    case 'Shortcut'
      if ~isempty(value)
        paths = __nelsonc_option_value__('AdditionalFiles', value);
        value = paths{1};
      end
    case 'DefaultInstallationDir'
      if isempty(value)
        return;
      end
      if ispc()
        value = strrep(value, '\', '/');
        if isempty(regexp(value, '^([A-Za-z]:/|//|%ProgramFiles%/)', 'once')) ...
          || contains(value, '"')
          error('Nelson:nelsonc:options:defaultInstallationDirAbsoluteTargetPath', 'DefaultInstallationDir must be an absolute target path.');
        end
      elseif ~startsWith(value, '/')
        error('Nelson:nelsonc:options:defaultInstallationDirAbsolute', 'DefaultInstallationDir must be absolute.');
      end
    case {'AuthorCompany', 'AuthorEmail', 'AuthorName', 'Summary', 'Description', 'InstallationNotes'}
    otherwise
      error('Nelson:nelsonc:options:unknownInstallerOption', 'Unknown installer option: %s.', name);
  end
end
