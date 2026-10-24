%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Single source of truth for module.json manifest field validation.
% Validates the schema fields only (no source-tree structure checks).
% Reused by nmm_validate (structural validation) and nmm_init (manifest
% generation) so a generated manifest is always nmm-valid.
%=============================================================================
function nmm_validate_module_json(data, arch)
  if nargin < 2
    arch = computer('arch');
  end
  validateRequiredFields(data);
  validateModuleName(data.module);
  validateTextField(data.title, 'title');
  validateTextField(data.summary, 'summary');
  validateSemverVersion(data.version, 'version');
  validatePlatforms(data.platforms, arch);
  validateNelsonCompatibility(data.nelson);
  validateBuiltin(data.builtin);
  validateLicense(data.license);
  validateDependencies(data.dependencies);
  validateFilesField(data);
end
%=============================================================================
% Optional 'files' packaging allowlist: an array of non-empty glob strings
% (see nmm_pack_select). Absent is valid; malformed is a clear error.
function validateFilesField(data)
  if ~isfield(data, 'files')
    return
  end
  value = data.files;
  if isstring(value)
    value = cellstr(value);
  end
  if ischar(value)
    value = {value};
  end
  if ~iscell(value) || isempty(value)
    nmm_error('ModuleJsonFilesInvalid', 'module.json field must be a non-empty array of strings: files');
  end
  for k = 1:numel(value)
    entry = value{k};
    if isstring(entry) && isscalar(entry)
      entry = char(entry);
    end
    if ~ischar(entry) || isempty(entry)
      nmm_error('ModuleJsonFilesInvalid', 'module.json field must be a non-empty array of strings: files');
    end
  end
end
%=============================================================================
function validateRequiredFields(data)
  required = {'module', 'title', 'summary', 'version', 'platforms', 'nelson', 'builtin', 'license', 'dependencies'};
  for k = required
    if ~isfield(data, k{1})
      nmm_error('ModuleJsonFieldMissing', 'module.json field is missing: %s', k{1});
    end
  end
end
%=============================================================================
function validateModuleName(module_name)
  validateTextField(module_name, 'module');
  if isempty(regexp(module_name, '^[A-Za-z][A-Za-z0-9_]*$', 'once'))
    nmm_error('InvalidModuleName', 'Invalid module name.');
  end
end
%=============================================================================
function validateTextField(value, fieldname)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value) || isempty(value)
    nmm_error('ModuleJsonTextFieldInvalid', 'module.json field must be a non-empty string: %s', fieldname);
  end
end
%=============================================================================
function validateSemverVersion(value, fieldname)
  validateTextField(value, fieldname);
  semver(value, value);
end
%=============================================================================
function validatePlatforms(platforms, arch)
  [r, msg] = nmm_is_supported_platform(platforms, arch);
  if ~r
    nmm_error('PlatformNotSupported', msg);
  end
end
%=============================================================================
function validateNelsonCompatibility(range)
  validateTextField(range, 'nelson');
  v = version('-number');
  current = sprintf('%d.%d.%d', v(1), v(2), v(3));
  if ~semverConstraintSatisfied(current, range)
    nmm_error('NelsonVersionNotSupported', 'Nelson version not supported: %s', range);
  end
end
%=============================================================================
function validateBuiltin(value)
  if ~(islogical(value) && isscalar(value))
    nmm_error('ModuleJsonBuiltinInvalid', 'module.json field must be a logical scalar: builtin');
  end
end
%=============================================================================
function validateLicense(value)
  validateTextField(value, 'license');
  if ~isSpdxExpression(value)
    nmm_error('ModuleJsonLicenseInvalid', 'module.json license must be a SPDX expression.');
  end
end
%=============================================================================
function tf = isSpdxExpression(value)
  tf = false;
  tokens = strsplit(strtrim(value), ' ');
  expectIdentifier = true;
  for k = 1:length(tokens)
    token = tokens{k};
    if isempty(token)
      continue
    end
    if expectIdentifier
      if ~isSpdxIdentifier(token)
        return
      end
      expectIdentifier = false;
    else
      if ~strcmp(token, 'AND') && ~strcmp(token, 'OR') && ~strcmp(token, 'WITH')
        return
      end
      expectIdentifier = true;
    end
  end
  tf = ~expectIdentifier;
end
%=============================================================================
function tf = isSpdxIdentifier(token)
  tf = ~isempty(regexp(token, '^[A-Za-z0-9][A-Za-z0-9\.\-\+]*$', 'once')) ...
      && (contains(token, '-') || any(strcmp(token, {'MIT', 'ISC', '0BSD'})));
end
%=============================================================================
function validateDependencies(dependencies)
  if ~isstruct(dependencies)
    nmm_error('ModuleJsonDependenciesInvalid', 'module.json field must be a struct: dependencies');
  end
  for name = fieldnames(dependencies)'
    validateModuleName(name{1});
    value = dependencies.(name{1});
    validateTextField(value, ['dependencies.', name{1}]);
    [isRepository, url, tag_branch] = nmm_is_http_repository(value);
    if ~isRepository && ~isdir(value) && ~isfile(value)
      validateSemverConstraint(value);
    end
  end
end
%=============================================================================
function validateSemverConstraint(value)
  if ~isVersionConstraint(value)
    semver(value, value);
    return
  end
  constraints = strsplit(strtrim(value), ' ');
  for k = 1:length(constraints)
    item = constraints{k};
    if isempty(item)
      continue
    end
    semver('1.0.0', item);
  end
end
%=============================================================================
function tf = semverConstraintSatisfied(version, constraint)
  if ~isVersionConstraint(constraint)
    tf = semver(version, constraint) == 0;
    return
  end
  tf = true;
  constraints = strsplit(strtrim(constraint), ' ');
  for k = 1:length(constraints)
    item = constraints{k};
    if isempty(item)
      continue
    end
    try
      if semver(version, item) ~= 1
        tf = false;
        return
      end
    catch
      tf = false;
      return
    end
  end
end
%=============================================================================
function tf = isVersionConstraint(value)
  tf = startsWith(value, '>') || startsWith(value, '<') ...
      || startsWith(value, '=') || startsWith(value, '^') ...
      || startsWith(value, '~') || contains(value, ' ');
end
%=============================================================================
