%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function struct_res = runfile(varargin)
  % internal function
  % not documented
  filename = runfile_parseInput(varargin);

  feature('currentAxesOnClick', false);
  feature('currentFigureOnClick', false);

  timing = 0;
  output_test = '';
  test_run_options = parsetags(filename);
  dia_ref = runfile_referenceFile(filename, test_run_options);

  current_path_test_runfile = path();
  current_dir_test_runfile = pwd();
  % 'test_run_options' variable protected of clear('all') in test files
  varlock('local', 'test_run_options')
  varlock('local', 'current_path_test_runfile')
  varlock('local', 'current_dir_test_runfile')
  ticBeginRunFile = feval('time', 'ns');
  varlock('local', 'ticBeginRunFile');

  try
    cd(fileparts(filename));
    output_test = evalc('run_status = run(filename, ''errcatch'');');
    if run_status
      struct_res = struct();
      struct_res.r = 0;
      struct_res.msg = '';
    else
      ex = lasterror();
      if strcmp(ex.identifier, 'Nelson:tests_manager:testsuite_skipped')
        struct_res = runfile_testSkipped(ex.message);
      else
        struct_res = runfile_testFailed(ex.message);
      end
    end
  catch ex
    if strcmp(ex.identifier, 'Nelson:tests_manager:testsuite_skipped')
      struct_res = runfile_testSkipped(ex.message);
    else
      struct_res = runfile_testFailed(ex.message);
    end
  end

  ticEndRunFile = feval('time', 'ns');
  timing = (double(ticEndRunFile) - double(ticBeginRunFile)) * 1e-9;

  varunlock('local', 'ticBeginRunFile');
  varunlock('local', 'test_run_options');
  varunlock('local', 'current_path_test_runfile')
  varunlock('local', 'current_dir_test_runfile')
  path(current_path_test_runfile);
  cd(current_dir_test_runfile);

  if (test_run_options.check_ref)
    msg = runfile_referenceError(filename, dia_ref, output_test);
    if ~isempty(msg)
      struct_res = runfile_testFailed(msg);
    end
  end
  struct_res.stdout = output_test;
  struct_res.timing = timing;
end
%=============================================================================
