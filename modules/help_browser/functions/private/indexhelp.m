%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = indexhelp(varargin)
  % private function used to index help files
  % indexhelp(true) forces indexing
  force = false;
  if nargin > 1
    error(_('Wrong number of input arguments.'));
  end
  if nargin == 1
    force = varargin{1};
    if ~islogical(force)
      error(_('Wrong type for argument #1: logical expected.'));
    end
    if ~isscalar(force)
      error(_('Wrong size for argument #1: scalar expected.'));
    end
  end
  pref_help = [prefdir(), '/help_index.nh5'];
  attributes = helpbrowser('-attributes');
  needToIndex = false;
  if ~isfile(pref_help) || force || isempty(attributes)
    needToIndex = true;
  else
    load(pref_help);
    if ~isvar('version_help')
      needToIndex = true;
    else
      current_version = version('-number');
      if (compareVersion(version_help, current_version) ~= 0)
        needToIndex = true;
      end
    end
  end
  if needToIndex
    r = reindexhelp(pref_help);
  else
    r = false;
  end
end
%=============================================================================
function modules_help_list = getModulesHelpList()
  run([nelsonroot(), '/modules/modules.m']);
  funcList = @(x) x{1};
  modules_help_list = [cellfun(funcList, modules_list, 'UniformOutput', false); getExternalModules()];
end
%=============================================================================
function external_modules = getExternalModules()
  [m, p, v, pm] = getmodules();
  external_modules = m(pm == false);
end
%=============================================================================
function r = reindexhelp(pref_help)
  helpbrowser('-clearcache');
  homepage = string([nelsonroot(), '/modules/main/help/', getlanguage(), '/', nelsonappid(), '.help.qch']);
  if ~isfile(homepage)
    homepage = string([nelsonroot(), '/modules/main/help/', getdefaultlanguage(), '/', nelsonappid(), '.help.qch']);
  end
  filesToRegister = string([]);
  filesToRegister = [filesToRegister; homepage];
  modules_help_list = getModulesHelpList();
  len = length(modules_help_list);
  lastDisplayedPercent = dispPercentLevel(0, '');    
  for idx = [1:len]
    module_name = modules_help_list{idx};
    help_file = getHelpFile(module_name);
    if isfile(help_file)
      filesToRegister = [filesToRegister; string(help_file)];
    end
    percent = (idx * 100) * inv(len);
    lastDisplayedPercent = dispPercentLevel(percent, lastDisplayedPercent);     
  end
  helpbrowser('-register', filesToRegister);
  dispPercentLevel(100, lastDisplayedPercent);    
  fprintf(stdout, char(13));
  helpbrowser();
  helpbrowser('-setsource', ['qthelp://', nelsonappid(), '.help/help/homepage.html'])
  version_help = version('-number');
  save(pref_help, 'version_help');
  r = true;
end
%=============================================================================
function newPrevious = dispPercentLevel(percent, previous)
  if ~isempty(previous)
    backspace = repmat(sprintf('\b'), 1, length(previous));
    newPrevious = sprintf('%d %', percent);
    fprintf(stdout, '%s%s', backspace, newPrevious);
  else
    newPrevious = sprintf('%d %', percent);
    fprintf(stdout, '\n%s %s', _('Indexing help files:'), newPrevious);
  end
end
%=============================================================================
function help_file = getHelpFile(module_name)
  help_file = '';
  module_path = [nelsonroot() , '/modules/', module_name];
  if ismodule(module_name)
    module_path = modulepath(module_name);
    help_file = [module_path, '/help/', getlanguage(), '/', nelsonappid(), '.modules.', module_name, '.help.qch'];
  end
  if ~isfile(help_file)
    help_file = [module_path, '/help/', getdefaultlanguage(), '/', nelsonappid(), '.modules.', module_name, '.help.qch'];
  end
end
%=============================================================================
function r = compareVersion(previous, current)
  currentAsNumber = 1000 * current(1) + 100 * current(2) + 10 * current(3);
  previousAsNumber = 1000 * previous(1) + 100 * previous(2) + 10 * current(3);
  if (currentAsNumber == previousAsNumber)
    r = 0;
  else
    if (previousAsNumber > currentAsNumber)
      r = -1;
    else
      r = 1;
    end
  end
end
%=============================================================================
