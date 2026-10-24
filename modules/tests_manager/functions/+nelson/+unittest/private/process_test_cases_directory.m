%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suite = process_test_cases_directory(...
    tests_dir, option, classname, module_name, nbWorkers, runtimeOptions, stoponfail)
  if isdir(tests_dir)
    test_files_list = test_run_get_files_list_by_option(tests_dir, option);
    test_suite = process_files_to_test(...
      test_files_list, classname, module_name, nbWorkers, runtimeOptions, stoponfail);
  else
    fprintf(stdout, ['  ', _('No directory tests available.'), newline]);
    test_suite = create_test_suite();
  end
end
