%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [cmd, command_filename, outputfile, redirect_err] = buildFileRunCommand(cmd, test_case, private_dir, userArguments)
  testfile = test_case.filename;
  outputfile = '';
  if test_case.options.gui_mode
    [~, name, ~] = fileparts(testfile); %#ok<NLS0004>
    outputfile = [unittest_tempdir(), name, '_', createGUID(), '.output'];
  end
  redirect_err = '';
  command_filename = [unittest_tempdir(), 'test_', createGUID(), '.m'];
  % Resolve the name and root together, including nested tests, versioned
  % packages and temporary package-manager staging directories.
  [moduleName, moduleRootPath] = discover_moduleNameFromFile(testfile);
  content = {['current_runfile_path = path();'];
    ['cleanup_runfile_path = onCleanup(@() path(current_runfile_path));'];
    ['setenv(''NELSON_UNITTEST_RUNFILE_WORKER'', ''TRUE'');'];
    ['addpath(', nelsonStringLiteral(private_dir), ');'];
    ['encode_runfile_payload(struct(''r'', 0, ''msg'', '''', ''stdout'', '''', ''timing'', 0));']};
  if ~isempty(moduleName)
    % Optional modules (declared {name, false} in modules.m) are not auto-loaded
    % at startup, so the child process would not find their functions. Load the
    % module on demand; the guard keeps this a no-op for auto-loaded modules.
    content = [content; ['if ~ismodule(', nelsonStringLiteral(moduleName), '); try; ', ...
      'addmodule(', nelsonStringLiteral(moduleRootPath), ', ', ...
      nelsonStringLiteral(moduleName), '); catch; end; end']];
  end
  content = [content; ['res_struct = runfile(', nelsonStringLiteral(testfile), ',', ...
    int2str(test_case.options.mpi_mode), ');']];
  content = [content; ['payload_json = jsonencode(encode_runfile_payload(res_struct));']];
  if ~isempty(outputfile)
    content = [content; ['filewrite(', nelsonStringLiteral(outputfile), ...
      ', [payload_json, char(10)]);']];
  end
  content = [content; '__nelson_unittest_worker_emit__(payload_json);'];
  if test_case.options.mpi_mode
    content = [content; 'exit();'];
  else
    content = [content; 'exit(double(res_struct.r), ''force'');'];
  end
  filewrite(command_filename, content);
  cmd = [cmd, ' --quiet', ' ', '--no-user-startup', ' ', '--timeout', ' ', ...
    int2str(test_case.timeout), ' ', '--file', ' "', command_filename, ...
    '"'];
  cmd = append_process_user_arguments(cmd, userArguments);
end
