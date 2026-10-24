%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function xmldocrenderimages(varargin)
  % xmldocrenderimages() build images for Nelson help files
  % xmldocrenderimages(module_name) build images for a module loaded in Nelson
  % xmldocrenderimages([], []) build images for all modules in all languages
  % xmldocrenderimages([], [], force) force image generation for all modules in all languages
  % xmldocrenderimages(module_name, []) build images for a module in all languages
  % xmldocrenderimages(module_name, language) build images for a module in a specific language
  % xmldocrenderimages(module_name, language, force) force image generation
  narginchk(0, 3);
   currentLang = getlanguage();
   force = false;

   if (nargin() == 0)
    render_all_modules([], force);
    setlanguage(currentLang);
    return;
  end
  if (nargin() == 2 || nargin() == 3)
    module = varargin{1};
    language = varargin{2};
    if (nargin() == 3)
      force = varargin{3};
      if ~islogical(force) || ~isscalar(force)
        error('Nelson:xmldocrenderimages:invalidInputType', 'Wrong type for argument #%d: %s expected.', 3, 'logical');
      end
    end
    if is_all_modules_request(module)
      render_all_modules(language, force);
      setlanguage(currentLang);
      return;
    end
    if ~ismodule(module)
      error('Nelson:xmldocrenderimages:invalidModuleName', 'Invalid module name.');
    end
    if isempty(language)
      render_module_all_languages(module, force);
      setlanguage(currentLang);
      return;
    end
    if ~is_valid_language(language)
      error('Nelson:xmldocrenderimages:invalidLanguage', 'Invalid language');
    end
    xmldocrenderimages_module(module, language, force);
    setlanguage(currentLang);
    return;
  end
  module = varargin{1};
  if ~ismodule(module)
    error('Nelson:xmldocrenderimages:invalidModuleName', 'Invalid module name.');
  end
  render_module_all_languages(module, force);
  setlanguage(currentLang);
end
%=============================================================================
function render_all_modules(language, force)
  if isempty(language)
    modules_help_list = get_modules_help_list();
    for lang = getavailablelanguages()'
      setlanguage(lang{1});
      for m = modules_help_list'
        xmldocrenderimages_module(m{1}, lang{1}, force);
      end
    end
    return;
  end
  if ~is_valid_language(language)
    error('Nelson:xmldocrenderimages:invalidLanguage', 'Invalid language');
  end
  modules_help_list = get_modules_help_list();
  setlanguage(language);
  for m = modules_help_list'
    xmldocrenderimages_module(m{1}, language, force);
  end
end
%=============================================================================
function tf = is_all_modules_request(module)
  tf = isnumeric(module) && isempty(module);
end
%=============================================================================
function tf = is_valid_language(language)
  tf = ischar(language) && ~isempty(language) && any(strcmp(language, getavailablelanguages()));
end
%=============================================================================
function render_module_all_languages(module, force)
  for lang = getavailablelanguages()'
    xmldocrenderimages_module(module, lang{1}, force);
  end
end
%=============================================================================
function xmldocrenderimages_module(module, language, force)
  fprintf('Rendering images for module %s language %s\n', module, language);
  if ~ismodule(module)
    return;
  end
  module_path = modulepath(module);
  xml_module_path = fullfile(module_path, 'help', language, 'xml');
  if ~isdir(xml_module_path)
    return;
  end
  TMP = [tempname() '_xmldocrenderimages'];
  mkdir(TMP);
  [status, scriptFiles, msg] = generate_image_scripts(xml_module_path, TMP, force);
  if ~status
    error('Nelson:xmldocrenderimages:scriptGenerationFailed', msg);
  end
  TIMEOUT = 120;
  if isempty(scriptFiles)
    rmdir(TMP, 's');
    return;
  end
  commands = build_render_commands(scriptFiles, TIMEOUT);
  nbWorkers = get_render_workers();
  fprintf('Rendering %d images on %d workers.\n', numel(scriptFiles), nbWorkers);
  for firstIndex = 1:nbWorkers:numel(scriptFiles)
    lastIndex = min(firstIndex + nbWorkers - 1, numel(scriptFiles));
    batchCommands = commands(firstIndex:lastIndex);
    batchScriptFiles = scriptFiles(firstIndex:lastIndex);
    [status, output] = system(batchCommands, ones(1, numel(batchCommands)) * (TIMEOUT + 10));
    failedIndex = find(status ~= 0);
    for k = 1:numel(failedIndex)
      i = failedIndex(k);
      retry_render_command(batchCommands(i), batchScriptFiles{i}, TIMEOUT);
    end
  end
  rmdir(TMP, 's');
end
%=============================================================================
function commands = build_render_commands(scriptFiles, timeout)
  nelson_bin_path = modulepath('nelson', 'bin');
  nelson_exe_name = 'nelson-adv-cli';
  if ispc()
    nelson_exe_name = 'nelson-adv-cli.exe';
  end
  nelson_exe_path = ['"', nelson_bin_path, '/', nelson_exe_name, '"'];
  commands = string({});
  for i = 1:numel(scriptFiles)
    cmd = [nelson_exe_path, ' --timeout ', num2str(timeout), ...
      ' --quiet --no-user-startup --no-user-modules --no-ipc --file "', scriptFiles{i}, '"'];
    commands = [commands, string(cmd)];
  end
end
%=============================================================================
function nbWorkers = get_render_workers()
  nbWorkers = min(maxNumCompThreads(), 4);
  if nbWorkers < 1
    nbWorkers = 1;
  end
end
%=============================================================================
function retry_render_command(command, scriptFile, timeout)
  maxAttempts = 5;
  lastOutput = '';
  for attempt = 1:maxAttempts
    [status, output] = system(command, 2 * timeout + 10);
    if (status == 0)
      return;
    end
    lastOutput = output;
  end
  msg = sprintf('Error while rendering image: %s', scriptFile);
  if ~isempty(lastOutput)
    msg = [msg, newline, char(lastOutput)];
  end
  error('Nelson:xmldocrenderimages:renderFailed', msg);
end
%=============================================================================
function [status, scriptFiles, msg] = generate_image_scripts(xml_module_path, tmp_dir, force)
  [status, scriptFiles, msg] = __xmldocgenerateimages__(xml_module_path, tmp_dir, force);
  if ~status
    return;
  end
  entries = dir(xml_module_path);
  for k = 1:numel(entries)
    entry = entries(k);
    if entry.isdir && ~strcmp(entry.name, '.') && ~strcmp(entry.name, '..')
      child_path = [xml_module_path, '/', entry.name];
      [child_status, child_scriptFiles, child_msg] = generate_image_scripts(child_path, tmp_dir, force);
      if ~child_status
        status = child_status;
        msg = child_msg;
        return;
      end
      scriptFiles = [scriptFiles(:); child_scriptFiles(:)];
    end
  end
end
%=============================================================================
