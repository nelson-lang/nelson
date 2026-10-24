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
  destination_dir = get_help_cache_dir(version_string);
  makeSureDestinationDirExists(destination_dir);
  languages = get_deploy_languages();
  modules_help_list = get_modules_help_list();

  len = length(modules_help_list);
  progress_count = 0;
  progress_total = len + 1;
  lastDisplayedPercent = '';
  if verbose
    lastDisplayedPercent = dispPercentLevel(0, '');
  end

  for idx = [1:len]
    module_name = modules_help_list{idx};
    module_path = get_module_path(module_name);
    if ~isempty(module_path)
      install_module_help_archives(module_name, module_path, languages, destination_dir);
    end
    progress_count = progress_count + 1;
    if verbose
      percent = (progress_count * 100) * inv(progress_total);
      lastDisplayedPercent = dispPercentLevel(percent, lastDisplayedPercent);
    end
  end

  main_module_path = [nelsonroot(), '/modules/main'];
  install_main_help_archives(main_module_path, languages, destination_dir);
  progress_count = progress_count + 1;
  if verbose
    percent = (progress_count * 100) * inv(progress_total);
    lastDisplayedPercent = dispPercentLevel(percent, lastDisplayedPercent);
  end
  if verbose
    fprintf(stdout, '\n');
  end
end
%=============================================================================
function install_module_help_archives(module_name, module_path, languages, help_dir)
  for lang = languages'
    destination_dir = [help_dir, lang{1}];
    help_archive_name = [module_path, '/help/', nelsonappid(), '.modules.', module_name, '.help.', lang{1}, '.nhz'];
    install_help_archive(help_archive_name, destination_dir, module_name);
  end
end
%=============================================================================
function install_main_help_archives(main_module_path, languages, help_dir)
  for lang = languages'
    destination_dir = [help_dir, lang{1}];
    help_archive_name = [main_module_path, '/help/', nelsonappid(), '.modules.', 'main', '.help.', lang{1}, '.nhz'];
    archive_exists = isfile(help_archive_name);
    if install_help_archive(help_archive_name, destination_dir, 'main') || archive_exists
      copy_main_help_files(destination_dir);
      copy_viewer_files(destination_dir);
    end
  end
end
%=============================================================================
function copy_main_help_files(destination_dir)
  files = dir([destination_dir, '/main/*.*']);
  for k = 1:length(files)
    if ~files(k).isdir
      copyfile([destination_dir, '/main/', files(k).name], [destination_dir, '/', files(k).name]);
    end
  end
end
%=============================================================================
function copy_viewer_files(destination_dir)
  resources_dir = [modulepath('help_tools'), '/resources'];
  viewer_file = [resources_dir, '/help_viewer.html'];
  search_file = [resources_dir, '/search_results.html'];
  if isfile(viewer_file)
    copyfile(viewer_file, [destination_dir, '/index.html']);
  end
  if isfile(search_file)
    copyfile(search_file, [destination_dir, '/search_results.html']);
  end
end
%=============================================================================
function installed = install_help_archive(help_archive_name, destination_dir, module_name)
  installed = false;
  if ~isfile(help_archive_name)
    return;
  end
  if is_help_archive_current(help_archive_name, destination_dir, module_name)
    return;
  end
  unzip(help_archive_name, destination_dir);
  write_help_archive_marker(help_archive_name, destination_dir, module_name);
  installed = true;
end
%=============================================================================
function tf = is_help_archive_current(help_archive_name, destination_dir, module_name)
  tf = false;
  module_destination_dir = [destination_dir, '/', module_name];
  if ~isdir(module_destination_dir)
    return;
  end
  if strcmp(module_name, 'main') && ~isfile([destination_dir, '/index.html'])
    return;
  end
  marker_filename = get_help_archive_marker_filename(destination_dir, module_name);
  if ~isfile(marker_filename)
    return;
  end
  archive_info = dir(help_archive_name);
  marker_info = dir(marker_filename);
  if isempty(archive_info.datenum) || isempty(marker_info.datenum)
    return;
  end
  tf = marker_info.datenum >= archive_info.datenum;
end
%=============================================================================
function write_help_archive_marker(help_archive_name, destination_dir, module_name)
  marker_dir = get_help_archive_marker_dir(destination_dir);
  if ~isdir(marker_dir)
    mkdir(marker_dir);
  end
  marker_filename = get_help_archive_marker_filename(destination_dir, module_name);
  filewrite(marker_filename, help_archive_name);
end
%=============================================================================
function marker_filename = get_help_archive_marker_filename(destination_dir, module_name)
  marker_filename = [get_help_archive_marker_dir(destination_dir), '/', module_name, '.stamp'];
end
%=============================================================================
function marker_dir = get_help_archive_marker_dir(destination_dir)
  marker_dir = [destination_dir, '/.deployhelp'];
end
%=============================================================================
function newPrevious = dispPercentLevel(percent, previous)
  percent = round(percent);
  if ~isempty(previous)
    backspace = repmat(sprintf('\b'), 1, length(previous));
    newPrevious = sprintf('%d %%', percent);
    fprintf(stdout, '%s%s', backspace, newPrevious);
  else
    newPrevious = sprintf('%d %%', percent);
    fprintf(stdout, '\n%s %s', _('Extracting help files:'), newPrevious);
  end
end
%=============================================================================
function makeSureDestinationDirExists(destination_dir)
  if ~isdir(destination_dir)
    mkdir(destination_dir);
  end
  for lang = get_deploy_languages()'
    lang_dir = [destination_dir, '/', lang{1}];
    if ~isdir(lang_dir)
      mkdir(lang_dir);
    end
  end
end
%=============================================================================
function languages = get_deploy_languages()
  % Internal knob: NELSON_HELP_DEPLOY_LANGUAGES restricts the deployed
  % languages to the requested subset (comma/semicolon separated). Unset (the
  % default) deploys every available language. Used to keep the help_tools
  % test suite fast without dropping module coverage; not a public contract.
  languages = getavailablelanguages();
  requested = getenv('NELSON_HELP_DEPLOY_LANGUAGES');
  if isempty(requested)
    return
  end
  wanted = strtrim(strsplit(requested, {',', ';'}));
  keep = false(size(languages));
  for k = 1:numel(languages)
    keep(k) = any(strcmp(languages{k}, wanted));
  end
  if any(keep)
    languages = languages(keep);
  end
end
%=============================================================================
function help_dir = get_help_cache_dir(version_string)
  help_dir = getenv('NELSON_HELP_CACHE_DIR');
  if ~isempty(help_dir)
    help_dir = ensureTrailingFileSeparator(help_dir);
  elseif ~isempty(getenv('NELSON_PREFERENCES_PATH'))
    help_dir = [prefdir(), '/help/'];
  else
    help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  end
end
%=============================================================================
function path = ensureTrailingFileSeparator(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
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
