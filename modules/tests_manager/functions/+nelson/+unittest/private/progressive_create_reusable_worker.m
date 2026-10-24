%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function worker = progressive_create_reusable_worker(testCase)
  worker = worker_pool_empty_worker();
  privateDir = fileparts(nfilename('fullpathext'), 'path');
  worker.command_filename = [unittest_tempdir(), ...
    'nelson_unittest_worker_', createGUID(), '.m'];
  content = {
    'setenv(''NELSON_UNITTEST_RUNFILE_WORKER'', ''TRUE'');'
    ['addpath(', nelsonStringLiteral(privateDir), ');']
    'while true'
    '  worker_file = __nelson_unittest_worker_receive__();'
    '  if isempty(worker_file)'
    '    break'
    '  end'
    '  res_struct = worker_pool_safe_runfile(worker_file, false);'
    ['  __nelson_unittest_worker_emit__(', ...
      'jsonencode(encode_runfile_payload(res_struct)));']
    'end'
    'exit(0, ''force'');'
  };
  filewrite(worker.command_filename, content);
  worker.executable = getNelsonExecutablePath('nelson-cli');
  options = test_case_process_options(testCase);
  userArguments = test_case_process_user_arguments(testCase);
  worker.process_arguments = [options, ' --quiet --no-user-startup --file ', ...
    quoteProcessArgument(worker.command_filename)];
  worker.process_arguments = append_process_user_arguments(worker.process_arguments, userArguments);
end
%=============================================================================
