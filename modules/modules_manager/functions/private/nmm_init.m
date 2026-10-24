%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nmm('init', DESTINATION, ...) generates a valid module.json manifest.
% The manifest is validated with nmm's own schema validator
% (nmm_validate_module_json) so the produced file is always nmm-valid.
%=============================================================================
function result = nmm_init(varargin)
  if isempty(varargin)
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  destination = varargin{1};
  if ~ischar(destination) || isempty(destination)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  options = parseOptions(varargin(2:end));
  destination = ensureTrailingSlash(destination);

  if options.interactive
    options = promptMissingFields(options, destination);
  end

  data = buildManifest(options, destination);
  nmm_validate_module_json(data);

  json = jsonprettyprint(jsonencode(data));

  % best-practice warnings reuse nmm's own warnings linter (non-fatal)
  warnings = nmm_module_json_warnings(data);

  if options.dryrun
    echoWarnings(warnings);
    result = data;
    result.warnings = warnings;
    return
  end

  if ~isdir(destination)
    mkdir(destination);
  end
  module_json_path = [destination, 'module.json'];
  if isfile(module_json_path) && ~options.force
    nmm_error('ModuleJsonAlreadyExists', 'module.json already exists (use ''Force'', true to overwrite): %s', module_json_path);
  end
  filewrite(module_json_path, json);

  if options.skeleton
    writeSkeleton(destination, data.module);
  end

  echoWarnings(warnings);
  result = fullpath(module_json_path);
end
%=============================================================================
function echoWarnings(warnings)
  if isempty(warnings)
    return
  end
  disp(_('module.json best-practice suggestions:'));
  for k = 1:length(warnings)
    disp(['  - ', warnings{k}]);
  end
end
%=============================================================================
function options = parseOptions(args)
  options = struct();
  options.module = '';
  options.title = '';
  options.summary = '';
  options.version = '';
  options.license = '';
  options.builtin = [];
  options.platforms = {};
  options.nelson = '';
  options.dependencies = [];
  options.keywords = {};
  options.authors = {};
  options.description = '';
  options.repository = '';
  options.homepage = '';
  options.issues = '';
  options.documentation = '';
  options.force = false;
  options.dryrun = false;
  options.interactive = false;
  options.skeleton = false;

  if mod(length(args), 2) ~= 0
    nmm_error('NameValuePairsExpected', 'name/value pairs expected.');
  end
  for k = 1:2:length(args)
    key = args{k};
    value = args{k + 1};
    if ~ischar(key)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 2);
    end
    switch lower(key)
      case 'module'
        options.module = asChar(value, 'module');
      case 'title'
        options.title = asChar(value, 'title');
      case 'summary'
        options.summary = asChar(value, 'summary');
      case 'version'
        options.version = asChar(value, 'version');
      case 'license'
        options.license = asChar(value, 'license');
      case 'nelson'
        options.nelson = asChar(value, 'nelson');
      case 'description'
        options.description = asChar(value, 'description');
      case 'repository'
        options.repository = asChar(value, 'repository');
      case 'homepage'
        options.homepage = asChar(value, 'homepage');
      case 'issues'
        options.issues = asChar(value, 'issues');
      case 'documentation'
        options.documentation = asChar(value, 'documentation');
      case 'builtin'
        options.builtin = asLogical(value, 'builtin');
      case 'platforms'
        options.platforms = asCellstr(value, 'platforms');
      case 'keywords'
        options.keywords = asCellstr(value, 'keywords');
      case 'authors'
        options.authors = asCellstr(value, 'authors');
      case 'author'
        options.authors = asCellstr(value, 'author');
      case 'dependencies'
        options.dependencies = asDependencies(value);
      case 'force'
        options.force = asLogical(value, 'Force');
      case 'dryrun'
        options.dryrun = asLogical(value, 'DryRun');
      case 'interactive'
        options.interactive = asLogical(value, 'Interactive');
      case 'skeleton'
        options.skeleton = asLogical(value, 'Skeleton');
      otherwise
        nmm_error('UnknownOption', 'unknown option: %s', key);
    end
  end
end
%=============================================================================
function data = buildManifest(options, destination)
  module_name = options.module;
  if isempty(module_name)
    module_name = defaultModuleName(destination, options.title);
  end

  version = options.version;
  if isempty(version)
    version = '1.0.0';
  end
  platforms = options.platforms;
  if isempty(platforms)
    platforms = {'all'};
  end
  nelson = options.nelson;
  if isempty(nelson)
    nelson = '>=2.0.0';
  end
  builtin = options.builtin;
  if isempty(builtin)
    builtin = false;
  end
  dependencies = options.dependencies;
  if isempty(dependencies)
    dependencies = struct();
  end
  title = options.title;
  if isempty(title)
    title = module_name;
  end
  summary = options.summary;
  if isempty(summary)
    summary = title;
  end

  data = struct();
  data.module = module_name;
  data.title = title;
  data.summary = summary;
  data.version = version;
  if ~isempty(options.description)
    data.description = options.description;
  end
  if ~isempty(options.authors)
    data.authors = options.authors;
  end
  if ~isempty(options.keywords)
    data.keywords = options.keywords;
  end
  data.license = options.license;
  if ~isempty(options.repository)
    data.repository = options.repository;
  end
  if ~isempty(options.homepage)
    data.homepage = options.homepage;
  end
  if ~isempty(options.issues)
    data.issues = options.issues;
  end
  if ~isempty(options.documentation)
    data.documentation = options.documentation;
  end
  data.builtin = builtin;
  data.platforms = platforms;
  data.nelson = nelson;
  data.dependencies = dependencies;
end
%=============================================================================
function name = defaultModuleName(destination, title)
  name = folderBaseName(destination);
  if isValidModuleName(name)
    return
  end
  name = sanitizeModuleName(name);
  if isValidModuleName(name)
    return
  end
  name = sanitizeModuleName(title);
  if isValidModuleName(name)
    return
  end
  nmm_error('ModuleNameNotDerivable', 'Unable to derive a valid module name, please provide ''module''.');
end
%=============================================================================
function base = folderBaseName(destination)
  base = destination;
  while endsWith(base, '/') || endsWith(base, '\')
    base = base(1:end - 1);
  end
  slash = max([strfind(base, '/'), strfind(base, '\')]);
  if ~isempty(slash)
    base = base(slash + 1:end);
  end
end
%=============================================================================
function name = sanitizeModuleName(candidate)
  if isempty(candidate)
    name = '';
    return
  end
  name = regexprep(candidate, '[^A-Za-z0-9_]', '_');
  name = regexprep(name, '^[^A-Za-z]+', '');
end
%=============================================================================
function tf = isValidModuleName(name)
  tf = ischar(name) && ~isempty(name) && ~isempty(regexp(name, '^[A-Za-z][A-Za-z0-9_]*$', 'once'));
end
%=============================================================================
function value = asChar(value, fieldname)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    nmm_error('OptionCharactersExpected', 'option ''%s'': characters expected.', fieldname);
  end
end
%=============================================================================
function value = asLogical(value, fieldname)
  if ~(islogical(value) && isscalar(value))
    if isnumeric(value) && isscalar(value)
      value = logical(value);
    else
      nmm_error('LogicalExpected', 'option ''%s'': logical scalar expected.', fieldname);
    end
  end
end
%=============================================================================
function value = asCellstr(value, fieldname)
  if ischar(value)
    value = {value};
  elseif isstring(value)
    value = cellstr(value);
  elseif iscell(value)
    for k = 1:numel(value)
      if isstring(value{k})
        value{k} = char(value{k});
      end
      if ~ischar(value{k})
        nmm_error('OptionCellOfStringsExpected', 'option ''%s'': cell of strings expected.', fieldname);
      end
    end
    value = value(:)';
  else
    nmm_error('OptionCellOfStringsExpected', 'option ''%s'': cell of strings expected.', fieldname);
  end
end
%=============================================================================
function value = asDependencies(value)
  if isempty(value)
    value = struct();
    return
  end
  if ~isstruct(value)
    nmm_error('InvalidDependenciesOption', 'option ''dependencies'': struct expected.');
  end
end
%=============================================================================
function options = promptMissingFields(options, destination)
  if isempty(options.module)
    default_module = '';
    try
      default_module = defaultModuleName(destination, options.title);
    catch
      default_module = '';
    end
    options.module = promptValidated('module name', default_module, @isValidModuleName, 'Invalid module name.');
  end
  if isempty(options.title)
    options.title = promptText('title', options.module);
  end
  if isempty(options.summary)
    options.summary = promptText('summary', options.title);
  end
  if isempty(options.version)
    options.version = promptValidated('version', '1.0.0', @isSemverOk, 'Invalid semantic version.');
  end
  if isempty(options.license)
    options.license = promptText('license (SPDX)', 'MIT');
  end
  if isempty(options.nelson)
    options.nelson = promptText('nelson version range', '>=2.0.0');
  end
  if isempty(options.platforms)
    options.platforms = promptValidated('platforms (comma separated)', 'all', @isPlatformsOk, 'Platform not supported.', @splitList);
  end
  if isempty(options.keywords)
    answer = promptText('keywords (comma separated)', '');
    if ~isempty(strtrim(answer))
      options.keywords = splitList(answer);
    end
  end
  if isempty(options.dependencies)
    answer = promptText('dependencies (name=constraint, comma separated)', '');
    if ~isempty(strtrim(answer))
      options.dependencies = parseDependencyList(answer);
    end
  end
end
%=============================================================================
function value = promptText(label, default_value)
  prompt = sprintf('%s [%s]: ', label, default_value);
  answer = input(prompt, 's');
  if isempty(strtrim(answer))
    value = default_value;
  else
    value = strtrim(answer);
  end
end
%=============================================================================
function value = promptValidated(label, default_value, checkFcn, errorMessage, varargin)
  transform = @(x) x;
  if ~isempty(varargin)
    transform = varargin{1};
  end
  while true
    answer = promptText(label, default_value);
    candidate = transform(answer);
    try
      ok = checkFcn(candidate);
    catch
      ok = false;
    end
    if ok
      value = candidate;
      return
    end
    disp(errorMessage);
  end
end
%=============================================================================
function tf = isSemverOk(value)
  tf = false;
  if ~ischar(value) || isempty(value)
    return
  end
  try
    semver(value, value);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function tf = isPlatformsOk(value)
  tf = nmm_is_supported_platform(value);
end
%=============================================================================
function items = splitList(text)
  items = {};
  tokens = strsplit(text, ',');
  for k = 1:length(tokens)
    token = strtrim(tokens{k});
    if ~isempty(token)
      items{end + 1} = token;
    end
  end
end
%=============================================================================
function deps = parseDependencyList(text)
  deps = struct();
  tokens = strsplit(text, ',');
  for k = 1:length(tokens)
    token = strtrim(tokens{k});
    if isempty(token)
      continue
    end
    pair = strsplit(token, '=');
    name = strtrim(pair{1});
    if length(pair) > 1
      constraint = strtrim(pair{2});
    else
      constraint = '>=0.0.0';
    end
    deps.(name) = constraint;
  end
end
%=============================================================================
function writeSkeleton(destination, module_name)
  header = ["%=============================================================================";
    "% generated by nmm('init')";
    "%============================================================================="];
  filewrite([destination, 'builder.m'], header);
  nmm_build_loader(module_name, destination(1:end - 1));
  if ~isdir([destination, 'etc'])
    mkdir([destination, 'etc']);
  end
  filewrite([destination, 'etc/startup.m'], header);
  filewrite([destination, 'etc/finish.m'], header);
  if ~isdir([destination, 'help'])
    mkdir([destination, 'help']);
  end
  if ~isdir([destination, 'help/en_US'])
    mkdir([destination, 'help/en_US']);
  end
  if ~isdir([destination, 'help/en_US/xml'])
    mkdir([destination, 'help/en_US/xml']);
  end
  filewrite([destination, 'help/en_US/xml/chapter.xml'], ...
    ['<?xml version="1.0" encoding="UTF-8" ?>', char(10), '<chapter></chapter>']);
  if ~isdir([destination, 'tests'])
    mkdir([destination, 'tests']);
  end
  test_lines = [header; ""; "assert_istrue(true);"];
  filewrite([destination, 'tests/test_', module_name, '.m'], test_lines);
end
%=============================================================================
function path = ensureTrailingSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
