%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = nmm_validate(varargin)
  if length(varargin) < 1 || length(varargin) > 6
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_path = varargin{1};
  if ~ischar(module_path)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  strict = false;
  asJson = false;
  includeWarnings = false;
  arch = computer('arch');
  k = 2;
  while k <= length(varargin)
    option = varargin{k};
    if ~ischar(option)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 1);
    end
    if strcmp(option, '-strict')
      strict = true;
    elseif strcmp(option, '-json')
      asJson = true;
    elseif strcmp(option, '-warnings')
      includeWarnings = true;
    elseif strcmp(option, '-target') && k < length(varargin) && ischar(varargin{k + 1})
      % platform the package is built for (cross build), see nmm('pack')
      arch = varargin{k + 1};
      k = k + 1;
    else
      nmm_error('UnknownArgument', '#%d argument: unknown option.', k + 1);
    end
    k = k + 1;
  end
  if asJson
    r = validateJsonReport(module_path, strict, includeWarnings);
    return
  end
  if includeWarnings
    r = validateWarningReport(module_path, strict);
    return
  end
  validateModule(module_path, strict, arch);
  r = true;
end
%=============================================================================
function report = validateJsonReport(module_path, strict, includeWarnings)
  report = struct();
  report.ok = true;
  report.issues = {};
  report.warnings = {};
  try
    validateModule(module_path, strict);
    if includeWarnings
      report.warnings = validationWarnings(module_path);
    end
  catch
    e = lasterror();
    report.ok = false;
    report.issues = {e.message};
  end
  report = jsonprettyprint(jsonencode(report));
end
%=============================================================================
function report = validateWarningReport(module_path, strict)
  validateModule(module_path, strict);
  report = struct();
  report.ok = true;
  report.warnings = validationWarnings(module_path);
end
%=============================================================================
function validateModule(module_path, strict, arch)
  if nargin < 3
    arch = computer('arch');
  end
  [data, module_dir] = resolveManifest(module_path);
  nmm_validate_module_json(data, arch);
  if isempty(module_dir)
    % lone module.json file: manifest fields only, no source-tree checks
    if strict
      validateStrictMetadata(data);
    end
    return
  end
  validateSourceStructure(module_dir);
  if strict
    validateStrictMetadata(data);
    validateStrictSourceStructure(module_dir);
  end
end
%=============================================================================
% Resolves a validate/warnings target to a manifest struct.
% module_path may be a module directory (module_dir is returned with a
% trailing slash) or a lone module.json file / any *.json (module_dir is '').
function [data, module_dir] = resolveManifest(module_path)
  if isManifestFile(module_path)
    module_dir = '';
    data = jsondecode(module_path, '-file');
    return
  end
  module_dir = ensureTrailingSlash(module_path);
  if ~isdir(module_dir)
    nmm_error('DirectoryExpectedExistingDirectoryExpected', 'an existing directory expected.');
  end
  module_json = [module_dir, 'module.json'];
  if ~isfile(module_json)
    nmm_error('ModuleJsonMissing', 'module.json is missing.');
  end
  data = jsondecode(module_json, '-file');
end
%=============================================================================
function tf = isManifestFile(module_path)
  tf = isfile(module_path) && endsWith(lower(module_path), '.json');
end
%=============================================================================
function warnings = validationWarnings(module_path)
  [data, module_dir] = resolveManifest(module_path);
  warnings = nmm_module_json_warnings(data);
  if isempty(module_dir)
    return
  end
  [xmlCount, chapterCount] = xmlHelpFileCount(module_dir);
  if xmlCount == 0
    warnings = appendWarning(warnings, _('XML help file is missing.'));
  elseif xmlCount == chapterCount
    warnings = appendWarning(warnings, 'module help pages are missing.');
  end
end
%=============================================================================
function warnings = appendWarning(warnings, message)
  warnings{end + 1} = message;
end
%=============================================================================
function [xmlCount, chapterCount] = xmlHelpFileCount(module_path)
  xmlCount = 0;
  chapterCount = 0;
  entries = dir([module_path, 'help']);
  for k = 1:length(entries)
    if ~entries(k).isdir || strcmp(entries(k).name, '.') || strcmp(entries(k).name, '..')
      continue
    end
    files = dir([module_path, 'help/', entries(k).name, '/xml/*.xml']);
    xmlCount = xmlCount + length(files);
    for j = 1:length(files)
      if strcmp(files(j).name, 'chapter.xml')
        chapterCount = chapterCount + 1;
      end
    end
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
function validateSourceStructure(module_path)
  if ~isfile([module_path, 'builder.m']) && ~isfile([module_path, 'loader.m'])
    nmm_error('LoaderMissingBuilderMLoader', 'builder.m or loader.m is missing.');
  end
  if ~isdir([module_path, 'etc'])
    nmm_error('EtcDirectoryMissing', 'etc directory is missing.');
  end
  if ~isfile([module_path, 'etc/startup.m'])
    nmm_error('StartupMissing', 'etc/startup.m is missing.');
  end
  if ~isfile([module_path, 'etc/finish.m'])
    nmm_error('FinishMissing', 'etc/finish.m is missing.');
  end
  if ~isdir([module_path, 'help'])
    nmm_error('HelpDirectoryMissing', 'help directory is missing.');
  end
  if ~isdir([module_path, 'tests'])
    nmm_error('TestsDirectoryMissing', 'tests directory is missing.');
  end
end
%=============================================================================
function validateStrictMetadata(data)
  validateOptionalTextField(data, 'repository');
  validateOptionalTextField(data, 'homepage');
  validateKeywords(data);
  % issues and documentation are truly optional: validate only when present.
  validatePresentUrlField(data, 'issues');
  validatePresentUrlField(data, 'documentation');
end
%=============================================================================
function validateOptionalTextField(data, fieldname)
  if ~isfield(data, fieldname)
    nmm_error('ModuleJsonFieldMissing', 'module.json field is missing: %s', fieldname);
  end
  validateTextField(data.(fieldname), fieldname);
  if strcmp(fieldname, 'repository') || strcmp(fieldname, 'homepage')
    validateHttpUrl(data.(fieldname), fieldname);
  end
end
%=============================================================================
% Validates an optional HTTP-URL field only when it is present and non-empty.
% Absence is never an error (the field is optional); an empty value is ignored.
function validatePresentUrlField(data, fieldname)
  if ~isfield(data, fieldname)
    return
  end
  value = data.(fieldname);
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value) || isempty(value)
    return
  end
  validateHttpUrl(value, fieldname);
end
%=============================================================================
function validateHttpUrl(value, fieldname)
  if ~(startsWith(value, 'http://') || startsWith(value, 'https://'))
    nmm_error('ModuleJsonUrlInvalid', 'module.json field must be an HTTP URL: %s', fieldname);
  end
end
%=============================================================================
function validateKeywords(data)
  if ~isfield(data, 'keywords')
    nmm_error('ModuleJsonFieldMissingModuleJsonField', 'module.json field is missing: keywords');
  end
  keywords = data.keywords;
  if isstring(keywords)
    keywords = cellstr(keywords);
  end
  if ~iscellstr(keywords) || isempty(keywords)
    nmm_error('ModuleJsonKeywordsInvalid', 'module.json field must be a non-empty cell of strings: keywords');
  end
end
%=============================================================================
function validateStrictSourceStructure(module_path)
  testFiles = dir([module_path, 'tests/*.m']);
  if isempty(testFiles)
    nmm_error('PackageTestsMissing', 'No package tests found.');
  end
  if ~hasXmlHelpFile(module_path)
    nmm_error('XmlHelpMissing', 'XML help file is missing.');
  end
end
%=============================================================================
function tf = hasXmlHelpFile(module_path)
  tf = false;
  entries = dir([module_path, 'help']);
  for k = 1:length(entries)
    if ~entries(k).isdir || strcmp(entries(k).name, '.') || strcmp(entries(k).name, '..')
      continue
    end
    files = dir([module_path, 'help/', entries(k).name, '/xml/*.xml']);
    if ~isempty(files)
      tf = true;
      return
    end
  end
end
%=============================================================================
function validateBuiltPackageStructure(module_path)
  if ~isfile([module_path, 'loader.m'])
    nmm_error('LoaderMissing', 'loader.m is missing.');
  end
  if ~isfile([module_path, 'module-lock.json'])
    nmm_error('ModuleLockMissing', 'module-lock.json is missing.');
  end
end
%=============================================================================
function path = ensureTrailingSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
