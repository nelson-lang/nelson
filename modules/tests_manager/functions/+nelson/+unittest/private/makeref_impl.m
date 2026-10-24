%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = makeref_impl(filename)
  if ~isfile(filename)
    error('Nelson:tests_manager:fileDoesNotExist', 'An existing filename expected.');
  end

  options = parsetags(filename);
  if (options.check_ref == false)
    error('Nelson:tests_manager:checkRefTagExpected', '<--CHECK REF--> tag expected.');
  end
  [p, f, e] = fileparts(filename); %#ok<NLS0004>
  if strcmp(p, '') == true;
    p = pwd();
    filename = [p, '/', f, e];
  end
  dia_ref = [p, '/', f, '.ref'];
  % delete previous .ref
  if isfile(dia_ref)
    rmfile(dia_ref);
  end
  r = test_makerefile(filename, options, dia_ref);
  r = (r == 1);
end
%=============================================================================
function r = test_makerefile(filename, options, ref_dest)
  cmd = '--no-user-modules --no-ipc --without-file-watcher';
  userArguments = '--no-audio --without-python --without-julia';
  if (options.english_imposed)
    cmd = [cmd, ' --language en_US'];
  end

  code = ['filename = ', nelsonStringLiteral(filename), '; ', ...
    'ref_dest = ', nelsonStringLiteral(ref_dest), '; ', ...
    'output = evalc(''r = run(filename, ''''errcatch'''');''); ', ...
    'filewrite(ref_dest, output); ', ...
    'exit(double(r), ''force'');'];

  cmd = [cmd, ' --quiet --no-user-startup --execute ', quoteProcessArgument(code)];
  cmd = append_process_user_arguments(cmd, userArguments);

  backend = test_launcher_backend();
  if options.cli_mode
    [executable, processArguments] = build_native_nelson_cli(cmd, false);
  elseif options.gui_mode
    [executable, processArguments] = build_native_nelson_gui(cmd, false);
  elseif options.adv_cli_mode
    if strcmp(backend, 'webview')
      [executable, processArguments] = build_native_nelson_adv_cli_webview(cmd, false);
    else
      [executable, processArguments] = build_native_nelson_adv_cli(cmd, false);
    end
  else
    [executable, processArguments] = build_native_nelson_cli(cmd, false);
  end
  jobs = run_process_jobs({executable}, {processArguments}, uint64(defaultTimeout(false, struct('Timeout', []))), ...
    {'makeref'}, 1, {filename});
  result = run_processes(jobs, 1);
  r = result.exit_code;
end
%=============================================================================
