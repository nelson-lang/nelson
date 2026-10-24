%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = configuremsvc()
  % currently only VS 2017, VS 2019 and VS 2022
  status = false;
  message = '';
  clear('havecompiler'); % clear persistent variable
  if ~ispc()
    varargout{1} = false;
    varargout{2} = _('Not implemented on this platform.');
    return
  end
  try
    vsjson = vswhere();
    varargout{1} = true;
  catch
    e = lasterror();
    varargout{1} = false;
    varargout{2} = e.message;
    return
  end

  if isempty(vsjson)
    varargout{1} = false;
    varargout{2} = 'No Visual Studio installation found.';
    return
  end

  if startsWith(vsjson.installationVersion, '18.')
    vsconfig = [modulepath('dynamic_link'), '/resources/msvc2026.json'];
  elseif startsWith(vsjson.installationVersion, '17.')
    vsconfig = [modulepath('dynamic_link'), '/resources/msvc2022.json'];
  elseif startsWith(vsjson.installationVersion, '16.')
    vsconfig = [modulepath('dynamic_link'), '/resources/msvc2019.json'];
  else
    vsconfig = [modulepath('dynamic_link'), '/resources/msvc2017.json'];
  end
  [status, message] = checkExistFile(vsconfig);
  if ~status
    varargout{1} = status;
    varargout{2} = message;
    return
  end
  msvc201X = jsondecode(vsconfig, '-file');

  vsinfo_batch = [modulepath('dynamic_link'), '/resources/vcinfo.bat'];
  arch = computer('arch');
  vcvarsbatpath = [vsjson.installationPath, '/VC/Auxiliary/Build'];
  if strcmp(arch, 'woa64')
    vcvarsbat = 'vcvarsarm64.bat';
  else
    if strcmp(arch, 'win64')
      vcvarsbat = 'vcvars64.bat';
    else
      vcvarsbat = 'vcvars32.bat';
    end
  end
  vcvarsbatfullfilename = [vcvarsbatpath, '/', vcvarsbat];
  vcinfo_batchpath = [modulepath('dynamic_link'), '/resources'];
  vcinfo_batch = 'vcinfo.bat';
  vcinfo_batchfullfilename = [vcinfo_batchpath, '/', vcinfo_batch];
  [status, message] = checkExistFile(vcinfo_batchfullfilename);
  if ~status
    varargout{1} = status;
    varargout{2} = message;
    return
  end
  [status, message] = checkExistFile(vcvarsbatfullfilename);
  if ~status
    varargout{1} = status;
    varargout{2} = message;
    return
  end
  names = {};
  values = {};
  if allCompilerEnvironmentVariablesExist(msvc201X.ENVIRONMENT_VARIABLES)
    for k = msvc201X.ENVIRONMENT_VARIABLES(:)'
      names{end + 1} = k{1};
      values{end + 1, 1} = currentCompilerEnvironmentValue(k{1}, getenv(k{1}), vsjson.installationPath);
    end
  else
    current_PATH = getenv('PATH');
    if ~contains(current_PATH, vcvarsbatpath)
      setenv('PATH', [current_PATH, ';', vcvarsbatpath]);
    end
    if ~contains(getenv('PATH'), vcinfo_batchpath)
      setenv('PATH', [getenv('PATH'), ';', vcinfo_batchpath]);
    end
    for k = msvc201X.ENVIRONMENT_VARIABLES(:)'
      cmd = ['"', vcinfo_batchfullfilename, '" "', vcvarsbatfullfilename, '" ', k{1}];
      [s, m] = unix(cmd);
      if (s ~= 0)
        varargout{1} = false;
        varargout{2} = sprintf('%s environment variable does not exist.', k{1});
        return
      end
      % ack to remove '\n' at the end
      m(m == 10) = '';
      if strcmp(k{1}, 'PATH') == true
        m = replace(m, current_PATH, '');
      end
      names{end + 1} = k{1};
      values{end + 1, 1} = m;
    end
  end
  config = cell2struct(values, names);
  config.COMPILER_CHOICE = 'msvc';
  json = jsonencode(config);
  json = jsonprettyprint(json);
  filewrite([prefdir(), '/compiler_', arch, '.json'], json);
  loadcompilerconf();
  if nargout == 0
    disp(_('msvc compiler detected and configured.'))
  end
  varargout{1} = true;
  varargout{2} = _('msvc compiler detected and configured.');
end
%=============================================================================
function [status, message] = checkExistFile(filename, errormsg)
  message = '';
  status = isfile(filename);
  if ~status
    message = sprintf('%s does not exist.', filename);
  end
end
%=============================================================================
function tf = allCompilerEnvironmentVariablesExist(names)
  tf = true;
  for k = names(:)'
    if isempty(getenv(k{1}))
      tf = false;
      return
    end
  end
end
%=============================================================================
function value = currentCompilerEnvironmentValue(name, value, installationPath)
  if strcmp(name, 'PATH')
    value = compilerPathEntries(value, installationPath);
  else
    value = uniqueEnvironmentEntries(value);
  end
end
%=============================================================================
function value = compilerPathEntries(value, installationPath)
  entries = strsplit(value, ';');
  selected = {};
  installationPath = lower(replace(installationPath, '/', '\'));
  for k = 1:length(entries)
    entry = entries{k};
    normalized = lower(replace(entry, '/', '\'));
    if contains(normalized, installationPath) || contains(normalized, '\windows kits\')
      selected = [selected, entry];
    end
  end
  value = strjoin(uniquePreserveOrder(selected), ';');
end
%=============================================================================
function value = uniqueEnvironmentEntries(value)
  value = strjoin(uniquePreserveOrder(strsplit(value, ';')), ';');
end
%=============================================================================
function entries = uniquePreserveOrder(entries)
  result = {};
  seen = {};
  for k = 1:length(entries)
    entry = entries{k};
    normalized = lower(replace(entry, '/', '\'));
    if isempty(entry) || any(strcmp(seen, normalized))
      continue
    end
    result = [result, entry];
    seen = [seen, normalized];
  end
  entries = result;
end
%=============================================================================
