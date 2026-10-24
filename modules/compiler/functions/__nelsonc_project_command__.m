%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function text = __nelsonc_project_command__(project)
  options = __nelsonc_project__('options', project);
  values = struct(options);
  names = fieldnames(values);
  lines = {['options = compiler.build.StandaloneApplicationOptions(', literal(options.AppFile), ');']};
  for k = 1:numel(names)
    name = names{k};
    if strcmp(name, 'AppFile'); continue; end
    lines{end + 1} = ['options.', name, ' = ', literal(values.(name)), ';'];
  end
  lines{end + 1} = ['results = compiler.build.', project.Target, '(options);'];
  text = strjoin(lines, char(10));
end

function text = literal(value)
  if islogical(value)
    text = 'false';
    if value; text = 'true'; end
  elseif iscell(value)
    entries = cell(size(value));
    for k = 1:numel(value); entries{k} = literal(value{k}); end
    text = ['{', strjoin(entries, ', '), '}'];
  elseif ischar(value) && ~any(value < char(32))
    text = ['''', strrep(value, '''', ''''''), ''''];
  else
    error('Nelson:compiler:project:optionControlCharacters', 'Cannot export an option containing control characters.');
  end
end
