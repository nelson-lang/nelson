%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
LANGUAGE = 'en_US';
fd = fopen([nelsonroot(), '/tools/missing_help/help_ignore.txt'], 'rt');
ignored = {};
line = fgets(fd);
if ischar(line)
  r = strfind(line,' ');
  line(r) = [];
  if strcmp(line, char([13, 10])) == true
    eol = 2;
  else
    eol = 1;
  end
  ignored = vertcat(ignored, {line(1:end - eol)});
end
while ischar(line)
  line = fgets(fd);
  if ischar(line)
    r = strfind(line,' ');
    line(r) = [];
    if strcmp(line, char([13, 10])) == true
      eol = 2;
    else
      eol = 1;
    end
    ignored = vertcat(ignored, {line(1:end - eol)});
  end
end
fclose(fd);


all_help_keys = iskeyword();
all_help_keys = [all_help_keys; what()];

for m = getmodules()(:)'
  path_functions = [modulepath(m{1}), '/functions'];
  if isdir(path_functions)
    functions_info = dir([path_functions,'/*.m']);
    for f = functions_info(:)'
      all_help_keys {end + 1} = f.name(1:end-2);
    end    
  end
end

keys_need_documented = {};
for k = all_help_keys(:)'
  r = strfind(ignored, k{1});
  if all(cellfun('isempty', r))
    keys_need_documented = vertcat(keys_need_documented, k);
  end
end

helps = {};
for m = getmodules()(:)'
  path_help = [modulepath(m{1}), '/help/', LANGUAGE, '/xml/'];
  files_info = dir([path_help,'*.xml']);
  for f = files_info(:)'
    name = fileparts(f.name, 'filename');
    if ~strcmp(name, 'chapter')
      helps = vertcat(helps, {name});
    end
  end
end

missing ={};
for k = keys_need_documented(:)'
  r = strfind(helps, k{1});
  if all(cellfun('isempty', r))
    missing = vertcat(missing, k);
  end
end

fprintf(stdout, ['', char(10)]);
fprintf(stdout, ['%%=============================================================================', char(10)]);
fprintf(stdout, ['%% ', _('Number of undocumented functions'), _(' : '), int2str(length(missing)), char(10)]);
fprintf(stdout, ['%% ', _('Number of existing help files'), _(' : '), int2str(length(helps)), char(10)]);
fprintf(stdout, ['%%=============================================================================', char(10)]);
if length(missing) == 0
  fprintf(stdout, ['%% ', _('Congratulations all functions are documented.'), char(10)]);
end
for m = missing(:)'
  if iskeyword(m{1})
    fprintf(stdout, ['%% ', m{1}, char(10)]);
  else
    fprintf(stdout, ['%% ', m{1}, ' ', which(m{1}), char(10)]);
  end
end
fprintf(stdout, ['%%=============================================================================', char(10)]);
exit();
