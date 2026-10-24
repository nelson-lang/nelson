%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function state = __nelsonc_editor_files__(state, action, section, key)
  if strcmp(action, 'browse')
    if strcmp(key, 'OutputDir')
      value = uigetdir(pwd(), 'Select output directory');
      if isequal(value, 0); return; end
    else
      filter = '*.*';
      if strcmp(key, 'AppFile'); filter = '*.m'; end
      if any(strcmp(key, {'ExecutableIcon', 'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}))
        filter = '*.png;*.jpg;*.jpeg;*.bmp;*.gif';
      end
      [name, folder] = uigetfile(filter, 'Select file');
      if isequal(name, 0); return; end
      value = fullfile(folder, name);
    end
    state.Controls.(section).(key).Value = value;
    if strcmp(key, 'AppFile')
      [folder, name] = fileparts(value);
      if isempty(state.Controls.Build.ExecutableName.Value); state.Controls.Build.ExecutableName.Value = name; end
      if isempty(state.Controls.Build.OutputDir.Value)
        state.Controls.Build.OutputDir.Value = fullfile(folder, [name, 'standaloneApplication']);
      end
    end
    return;
  end
  list = state.Controls.(section).AdditionalFiles;
  if strcmp(action, 'remove-file')
    list.Items = list.Items(~ismember(list.Items, list.Value));
    return;
  end
  if strcmp(action, 'add-folder')
    value = uigetdir(pwd(), 'Add resource folder');
    if isequal(value, 0); return; end
    files = {value};
  else
    [names, folder] = uigetfile('*.*', 'Add resource files', 'MultiSelect', 'on');
    if isequal(names, 0); return; end
    if ischar(names); names = {names}; end
    files = cell(size(names));
    for k = 1:numel(names); files{k} = fullfile(folder, names{k}); end
  end
  for k = 1:numel(files)
    if ~any(strcmp(list.Items, files{k})); list.Items = [list.Items, files(k)]; end
  end
end
