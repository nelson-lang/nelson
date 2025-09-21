%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function install_help(verbose)
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  destination_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  makeSureDestinationDirExists(destination_dir);
  if ~isdir(destination_dir)
    mkdir(destination_dir);
  end
  for lang = getavailablelanguages()'
    lang_dir = [destination_dir, '/', lang{1}];
  end

  modules_help_list = get_modules_help_list();

  len = length(modules_help_list);
  if verbose
    lastDisplayedPercent = dispPercentLevel(0, '');
  end    

  for idx = [1:len]
    module_name = modules_help_list{idx};
    module_path = get_module_path(module_name);
    if isempty(module_path)
      continue;
    end
    for lang = getavailablelanguages()'
      destination_dir = [userdir(), '/Nelson/', version_string, '/help/', lang{1}];
      help_archive_name = [module_path, '/help/', nelsonappid(), '.modules.', module_name, '.help.', lang{1}, '.nhz'];
      if ~isfile(help_archive_name)
        continue;
      end
     unzip(help_archive_name, destination_dir);
    end
    percent = (idx * 100) * inv(len);
    if verbose
      lastDisplayedPercent = dispPercentLevel(percent, lastDisplayedPercent);
    end
  end
  if verbose
    fprintf(stdout, '\n');
  end

  main_module_path = [nelsonroot(), '/modules/main'];
  for lang = getavailablelanguages()'
    destination_dir = [userdir(), 'Nelson/', version_string, '/help/', lang{1}];
    help_archive_name = [main_module_path, '/help/', nelsonappid(), '.modules.', 'main', '.help.', lang{1}, '.nhz'];
    if ~isfile(help_archive_name)
      continue;
    end
    unzip(help_archive_name, destination_dir);
    files = dir([destination_dir, '/main/*.*']);
    for k = 1:length(files)
      if files(k).isdir
        continue;
      end
      copyfile([destination_dir, '/main/', files(k).name], [destination_dir, '/', files(k).name]);
    end 
  end  
end
%=============================================================================
function newPrevious = dispPercentLevel(percent, previous)
  if ~isempty(previous)
    backspace = repmat(sprintf('\b'), 1, length(previous));
    newPrevious = sprintf('%d %', percent);
    fprintf(stdout, '%s%s', backspace, newPrevious);
  else
    newPrevious = sprintf('%d %', percent);
    fprintf(stdout, '\n%s %s', _('Extracting help files:'), newPrevious);
  end
end
%=============================================================================
function makeSureDestinationDirExists(destination_dir)
  if ~isdir(destination_dir)
    mkdir(destination_dir);
  end
  for lang = getavailablelanguages()'
    lang_dir = [destination_dir, '/', lang{1}];
    if ~isdir(lang_dir)
      mkdir(lang_dir);
    end
  end
end
%=============================================================================
function module_path = get_module_path(module_name)
  module_path = '';
  if ismodule(module_name)
    module_path = modulepath(module_name);
  else
    path = [nelsonroot(), '/modules/', module_name, '/etc/startup.m'];
    if isfile(path)
      module_path = [nelsonroot(), '/modules/', module_name, '/'];
    end
  end
end
%=============================================================================
