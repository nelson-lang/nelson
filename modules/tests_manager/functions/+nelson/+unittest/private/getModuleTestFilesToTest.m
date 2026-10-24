%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function files = getModuleTestFilesToTest(modules_list, test_names) %#ok<NLS0026>
  files = [];
  if length(modules_list) ~= 1
    return
  end
  if ischar(test_names)
    test_names = {test_names};
  elseif ~iscellstr(test_names)
    return
  end
  module_name = modules_list{1};
  try
    tests_dir = [modulepath(module_name), '/tests/'];
  catch
    tests_dir = [nelsonroot(), '/modules/', module_name, '/tests/'];
  end
  for k = test_names(:)'
    test_file = resolveModuleTest(tests_dir, k{1});
    if isfile(test_file)
      if isempty(files)
        files = {test_file};
      else
        files = [files; test_file];
      end
    end
  end
end
