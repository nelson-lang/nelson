%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function index_dirs = help_xml_index_directories(language, extra_dirs)
  % XML help roots ("<module>/help/<language>/xml" with a chapter.xml) of every
  % builtin module, plus extra_dirs. Used by xmldocbuild to resolve
  % <link linkend="${module}name"> across modules.
  if nargin < 2
    extra_dirs = {};
  end
  if ischar(extra_dirs)
    extra_dirs = {extra_dirs};
  end
  index_dirs = {};
  modules_path = [nelsonroot(), '/modules/'];
  module_list = dir(modules_path);
  for i = 1:length(module_list)
    if ~module_list(i).isdir || any(strcmp(module_list(i).name, {'.', '..'}))
      continue
    end
    help_path = [modules_path, module_list(i).name, '/help/', language, '/xml'];
    if isdir(help_path) && isfile([help_path, '/chapter.xml'])
      index_dirs{end + 1} = help_path; %#ok<AGROW>
    end
  end
  for k = 1:length(extra_dirs)
    extra = extra_dirs{k};
    if isdir(extra) && ~any(strcmp(index_dirs, extra))
      index_dirs{end + 1} = extra; %#ok<AGROW>
    end
  end
end
%=============================================================================
