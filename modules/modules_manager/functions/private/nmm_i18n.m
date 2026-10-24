%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% result = nmm_i18n(MODULE)
% result = nmm_i18n(MODULE, LOCALE1, LOCALE2, ...)
%   Generate (or refresh) the locale catalogs of a module so its _()/_W() user
%   interface strings and its own "Nelson:<module>:*" error templates can be
%   translated. MODULE is a module root directory or an installed module name.
%
%   The work is delegated to ngen.i18n when the ngen module is available;
%   otherwise the same pipeline is run inline on the native i18nHelpers builtin
%   (extractUI / extractErrors / namespace / merge / sort). Catalogs are written
%   under MODULE/locale as <module>-ui-<locale>.json and
%   <module>-errors-<locale>.json (an empty catalog is still written as {}).
%=============================================================================
function result = nmm_i18n(varargin)
  if nargin < 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  moduleDir = nmm_i18n_resolve_dir(varargin{1});
  if nargin > 1
    locales = varargin(2:end);
  else
    locales = getavailablelanguages();
    locales = locales(:)';
  end
  name = nmm_i18n_module_name(moduleDir);

  if ismodule('ngen')
    result = ngen.i18n(moduleDir, 'Locales', locales, 'Name', name);
    return;
  end

  % --- Inline fallback (no ngen): core i18nHelpers only. ----------------------
  if ~any(strcmp(locales, 'en_US'))
    locales = [{'en_US'}, locales];
  end
  localeDir = [moduleDir, '/locale'];
  if ~isdir(localeDir)
    mkdir(localeDir);
  end
  uiEn = [localeDir, '/', name, '-ui-en_US.json'];
  errEn = [localeDir, '/', name, '-errors-en_US.json'];

  i18nHelpers('extractUI', moduleDir, uiEn);
  nmm_i18n_finalize(uiEn);
  i18nHelpers('extractErrors', moduleDir, errEn);
  i18nHelpers('namespace', errEn, [name, ':']);
  nmm_i18n_finalize(errEn);

  files = {uiEn, errEn};
  for k = 1:numel(locales)
    loc = locales{k};
    if strcmp(loc, 'en_US')
      continue;
    end
    uiLoc = [localeDir, '/', name, '-ui-', loc, '.json'];
    errLoc = [localeDir, '/', name, '-errors-', loc, '.json'];
    nmm_i18n_ensure_json(uiLoc);
    nmm_i18n_ensure_json(errLoc);
    i18nHelpers('merge', uiEn, uiLoc);
    i18nHelpers('merge', errEn, errLoc);
    nmm_i18n_finalize(uiLoc);
    nmm_i18n_finalize(errLoc);
    files{end + 1} = uiLoc; %#ok<AGROW>
    files{end + 1} = errLoc; %#ok<AGROW>
  end
  result = struct('name', name, 'localeDir', localeDir, 'files', {files});
end
%=============================================================================
function moduleDir = nmm_i18n_resolve_dir(value)
  if ~ischar(value) && ~(isstring(value) && isscalar(value))
    nmm_error('InvalidArgument', 'A module directory or name is expected.');
  end
  value = char(value);
  if isdir(value)
    moduleDir = strrep(value, '\', '/');
    while length(moduleDir) > 1 && strcmp(moduleDir(end), '/')
      moduleDir = moduleDir(1:end - 1);
    end
    return;
  end
  if ismodule(value)
    moduleDir = strrep(modulepath(value), '\', '/');
    return;
  end
  nmm_error('ModuleNotFound', 'Module directory or name not found.');
end
%=============================================================================
function name = nmm_i18n_module_name(moduleDir)
  name = '';
  jsonPath = [moduleDir, '/module.json'];
  if isfile(jsonPath)
    try
      info = jsondecode(jsonPath, '-file');
      if isstruct(info) && isfield(info, 'module') && ~isempty(info.module)
        name = char(info.module);
      end
    catch
    end
  end
  if isempty(name)
    parts = strsplit(moduleDir, '/');
    name = parts{end};
  end
end
%=============================================================================
function nmm_i18n_ensure_json(jsonFile)
  if ~isfile(jsonFile)
    filewrite(jsonFile, sprintf('{}\n'));
  end
end
%=============================================================================
function nmm_i18n_finalize(jsonFile)
  hasEntries = false;
  if isfile(jsonFile)
    try
      data = jsondecode(jsonFile, '-file');
      hasEntries = isstruct(data) && ~isempty(fieldnames(data));
    catch
    end
  end
  if hasEntries
    i18nHelpers('sort', jsonFile, jsonFile);
  else
    filewrite(jsonFile, sprintf('{}\n'));
  end
end
%=============================================================================
