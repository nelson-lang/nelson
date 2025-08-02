%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = test_run(varargin)
  nbWorkers = maxNumCompThreads();
  __totalTimeBegin = time();
  [modules_list, files_to_test, result_outputfile, option, stoponfail] = parseInputArguments(nargin, varargin);
  nelson_version = version('-number');
  nelson_version_str = sprintf('Nelson-%d.%d.%d.%d', nelson_version(1), nelson_version(2), nelson_version(3), nelson_version(4));
  nbTestSuites = length(modules_list);
  if isempty(files_to_test)
    modules_idx = 1;
    if isempty(modules_list)
      error(_('No valid module.'));
    end
    fprintf(stdout, [newline, _('Tests running on %d workers.'), newline], nbWorkers);
    test_suites = create_test_suites();
    for m = modules_list(:)'
      if test_suites.errors ~= 0 && stoponfail
        status = false;
        break
      end
      fmt = [newline, '%0', int2str(length(int2str(nbTestSuites))), 'd/%0', int2str(length(int2str(nbTestSuites))), 'd [%s]:', newline];
      fprintf(stdout, fmt, modules_idx, nbTestSuites, m{1});
      modules_idx = modules_idx + 1;
      classname = [nelson_version_str, '.', m{1}];
      try
        tests_dir = [modulepath(m{1}), '/tests/'];
      catch
        tests_dir = [nelsonroot(), '/modules/', m{1}, '/tests/'];
      end
      test_suite = process_test_cases_directory(tests_dir, option, classname, m{1}, nbWorkers);
      for t = test_suite
        test_suites.disabled = test_suites.disabled + t.disabled;
        test_suites.errors = test_suites.errors + t.errors;
        test_suites.passed = test_suites.passed + t.passed;
        test_suites.tests = test_suites.tests + t.tests;
        test_suites.bench = test_suites.bench + t.bench;
        test_suites.time = test_suites.time + t.time;
        test_suites.tests_list = [test_suites.tests_list; t];
      end
    end
  else
    classname = nelson_version_str;
    test_suite = process_files_to_test(files_to_test, option, classname, '', nbWorkers);
    test_suites.disabled = test_suite.disabled;
    test_suites.errors = test_suite.errors;
    test_suites.passed = test_suite.passed;
    test_suites.tests = test_suite.tests;
    test_suites.bench = test_suite.bench;
    test_suites.time = test_suite.time;
    test_suites.tests_list = test_suite;
  end
  __totalTimeEnd = time();
  status = test_suites.errors == 0;
  test_suites.total_time = __totalTimeEnd - __totalTimeBegin;
  disp_summary(test_suites);
  res = save_results(test_suites, result_outputfile);
  if ~res
    error(_('Impossible to save results file.'));
  end
end
%=============================================================================
function r = getStatusCharacter(status)
  if ~isunicodesupported()
    r = status;
    return
  end
  switch status
    case 'Fail'
      r = ' 🔴 ';
    case 'Pass'
      r = ' ✅ ';
    case {'Skip', 'Interactive', 'No display'}
      r = ' ⭕ ';
    otherwise
      r = status;
    end
  end
  %=============================================================================
function test_cases_updated = process_test_cases(test_cases, nbWorkers, nbTotalTests, initialIndex)
  accumulator = [];
  test_cases_updated = [];
  nbTests = length(test_cases);
  count = 0;
  lenCharsNbTests = length(int2str(nbTotalTests));
  if lenCharsNbTests < 2
    lenCharsNbTests = lenCharsNbTests + 1;
  end
  fmtStart = ['  ', '%0', int2str(lenCharsNbTests), 'd/', '%0', int2str(lenCharsNbTests),'d - %s'];
  indexTest = initialIndex;
  if ispc()
    abortedCode = 258;
  else
    abortedCode = 134;
  end
  for test_case = test_cases
    accumulator = [accumulator; test_case];
    if (length(accumulator) == nbWorkers || count == nbTests - 1)
      commands = {accumulator(1:length(accumulator)).command};
      timeout = [];
      for i = 1:length(accumulator)
        if (accumulator(i).isbench)
          timeout = [timeout, 360];
        else
          timeout = [timeout, 120];
        end
      end
      [s, m] = unix(commands, timeout);
      for i = 1:length(accumulator)
        aborted = false;
        if (s(i) == abortedCode) && strcmp(m{i}, 'ABORTED')
          aborted = true;
        end
        if strcmp(commands(i), 'echo skip')
          accumulator(i).msg = '';
        else
          accumulator(i).msg = m{i};
        end
        accumulator(i).res_cmd = s(i);
        accumulator(i) = post_run_test_case(accumulator(i), aborted);
        test_cases_updated = [test_cases_updated; accumulator(i)];
        startChars = sprintf(fmtStart, indexTest, nbTotalTests,  accumulator(i).name);
        statusChars = getStatusCharacter(accumulator(i).status);
        nb_spaces = max(0, 80 - length(startChars) - length(statusChars) + 1);
        fprintf(stdout, '%s%s%s%s', startChars, blanks(nb_spaces), statusChars, newline);
        displayTestCaseFail(accumulator(i))
        if ((accumulator(i).skip) && (~isempty(accumulator(i).msg)))
          msg = sprintf('    Reason: %s\n', accumulator(i).msg);
          if length(msg) > 75
            msg = [msg(1:75), '...', '\n'];
          end
          fprintf(stdout, msg);
        end 
        indexTest = indexTest + 1;        
      end
      accumulator = [];
    end
    count = count + 1;
  end
  if ~isempty(test_cases_updated)
    test_cases_updated = rmfield(test_cases_updated, 'command');
    test_cases_updated = rmfield(test_cases_updated, 'outputfile');
    test_cases_updated = rmfield(test_cases_updated, 'redirect_err');
    test_cases_updated = rmfield(test_cases_updated, 'command_filename');
    test_cases_updated = rmfield(test_cases_updated, 'res_cmd');
    test_cases_updated = rmfield(test_cases_updated, 'skip');
  end
end
%=============================================================================
function displayFilenameAndLine(msg)
  if (length(msg.stack) > 0)
    pos = 1;
    if (msg.stack(1).line == 0)
      pos = 2;
      if ~((length(msg.stack) > 1) && (msg.stack(pos).line ~= 0))
        pos = -1;
      end
    end
    if pos > 0
      disp(['      ', _('File:'), ' ', msg.stack(pos).file]);
      disp(['      ', _('Line:'), ' ', num2str(msg.stack(pos).line)]);
    end
  end
end
%=============================================================================
function displayTestCaseFail(test_case)
  if strcmp(test_case.status, 'Fail') == true
    disp(['    run(''', test_case.filename, ''')']);
    disp(_('    Error:'));
    if ~isempty(test_case.msg)
      if strcmp(class(test_case.msg), 'cell') == 1
        msg = test_case.msg(:)';
        for k = msg(1:3)
          disp(['      ', k{1}]);
        end
      else
        if strcmp(class(test_case.msg), 'struct') == true
          disp(['      ', test_case.msg.message]);
          displayFilenameAndLine(test_case.msg);
        else
          disp(['      ', test_case.msg]);
        end
      end
    else
      disp(test_case)
    end
    disp(' ');
  end
end
%=============================================================================
function test_suite = process_files_to_test(test_files_list, option, classname, module_name, nbWorkers)
  test_suite = create_test_suite();
  test_suite.name = module_name;
  test_cases = [];
  time_test = 0;
  nbTests = length(test_files_list);
  if nbTests == 0
    fprintf(stdout, ['  ', _('No tests available.'), newline]);
  end
  lenCharsNbTests = length(int2str(nbTests));
  if lenCharsNbTests < 2
    lenCharsNbTests = lenCharsNbTests + 1;
  end
  fmtStart = ['  ', '%0', int2str(lenCharsNbTests), 'd/', '%0', int2str(lenCharsNbTests),'d - %s'];
  parallel_tests = [];
  sequential_tests = [];
  for file = test_files_list(:)'
    test_case = create_test_case(file{1}, classname);
    test_case.classname = classname;
    if test_case.options.sequential_test_required || test_case.isbench
      sequential_tests = [sequential_tests, test_case];
    else
      parallel_tests = [parallel_tests, test_case];
    end
  end
  nbTotalTests = length(sequential_tests) + length(parallel_tests);
  test_cases_parallel = process_test_cases(parallel_tests, nbWorkers, nbTotalTests, 1);
  test_cases_sequential = process_test_cases(sequential_tests, 1, nbTotalTests, length(test_cases_parallel) + 1);
  test_cases = [test_cases_sequential; test_cases_parallel];
  time_test = 0;
  for test_case = test_cases(:)'
    if test_case.isbench
      test_suite.bench = test_suite.bench + 1;
    else
      test_suite.tests = test_suite.tests + 1;
      switch test_case.status
        case 'Fail'
          test_suite.errors = test_suite.errors + 1;
        case 'Pass'
          test_suite.passed = test_suite.passed + 1;
        case {'Skip', 'Interactive', 'No display'}
          test_suite.disabled = test_suite.disabled + 1;
        end
      end
      time_test = time_test + test_case.time;
    end 
    
    test_suite.test_cases_list = test_cases;
    test_suite.time = test_suite.time + time_test;
  end
  %=============================================================================
function test_suite = process_test_cases_directory(tests_dir, option, classname, module_name, nbWorkers)
  if isdir(tests_dir)
    test_files_list = getFilesListByOption(tests_dir, option);
    test_suite = process_files_to_test(test_files_list, option, classname, module_name, nbWorkers);
  else
    fprintf(stdout, ['  ', _('No directory tests available.'), newline]);
    test_suite = create_test_suite();
  end
end
%=============================================================================
function test_case_cleaned = post_run_test_case(test_case_res, aborted)
  if isfile(test_case_res.command_filename)
    try
      rmfile(test_case_res.command_filename);
    end
  end
  if test_case_res.options.mpi_mode
    test_case_cleaned = post_run_test_case_mpi(test_case_res);
  else
    if (aborted)
      test_case_cleaned = post_run_test_case_aborted(test_case_res);
    elseif test_case_res.skip
      test_case_cleaned = deep_copy_test_case(test_case_res);
    elseif test_case_res.res_cmd ~= 1
      test_case_cleaned = post_run_test_case_skip_or_fail(test_case_res);
    else
      test_case_cleaned = post_run_test_case_skip_or_pass(test_case_res);
    end
  end
end
%=============================================================================
function test_case_cleaned = deep_copy_test_case(test_case_res)
  test_case_cleaned.filename = test_case_res.filename;
  test_case_cleaned.options = test_case_res.options;
  test_case_cleaned.msg = test_case_res.msg;
  test_case_cleaned.name = test_case_res.name;
  test_case_cleaned.classname = test_case_res.classname;
  test_case_cleaned.status = test_case_res.status;
  test_case_cleaned.time = test_case_res.time;
  test_case_cleaned.isbench = test_case_res.isbench;
  test_case_cleaned.skip = test_case_res.skip;
end
%=============================================================================
function test_case_res = post_run_test_case_mpi(test_case_res)
  if isfile(test_case_res.outputfile)
    s = jsondecode(fileread(test_case_res.outputfile));
    test_case_res.time = s.timing;
    switch (s.r)
      case 0
        test_case_res.status = 'Pass';
        test_case_res.skip = false;
      case 1
        test_case_res.status = 'Fail';
        test_case_res.msg = s.msg;
        test_case_res.skip = false;
      case 2
        test_case_res.status = 'Skip';
        test_case_res.skip = true;
        test_case_res.msg = s.msg;
      otherwise
        error(_('Case not managed.'));
      end
  else
    test_case_res.status = 'Fail';
  end
end
%=============================================================================
function test_case_res = post_run_test_case_aborted(test_case_res)
  test_case_res.status = 'Fail';
  test_case_res.msg = [_('Test execution was terminated after timeout.')];
end
%=============================================================================
function test_case_res = post_run_test_case_skip_or_fail(test_case_res)
  if isfile(test_case_res.outputfile)
    s = jsondecode(fileread(test_case_res.outputfile));
    test_case_res.time = s.timing;
    switch (s.r)
      case 0
        if test_case_res.isbench
          if s.timing > 100
            test_case_res.status = [mat2str(s.timing * inv(60), 2), ' min'];
          else
            if s.timing < 1e-3
              test_case_res.status = [mat2str(s.timing * 1e3, 2), ' ms'];
            else
              test_case_res.status = [mat2str(s.timing, 2), ' s'];
            end
          end
        else
          test_case_res.status = 'Pass';
          test_case_res.msg = '';
        end
        test_case_res.skip = false;
      case 1
        test_case_res.status = 'Fail';
        test_case_res.msg = s.msg;
        test_case_res.skip = false;
      case 2
        test_case_res.status = 'Skip';
        test_case_res.skip = true;
        test_case_res.msg = s.msg;
      otherwise
        error(_('Case not managed.'));       
    end
  elseif isfile(test_case_res.redirect_err)
    s = jsondecode(fileread(test_case_res.redirect_err));
    if ~isempty(s)
      test_case_res.msg = s.msg;
    else
      test_case_res.status = 'Fail';
      test_case_res.msg = [_('Error code not catched: '), num2str(test_case_res.res_cmd)];
    end
  else
    test_case_res.status = 'Fail';
    test_case_res.msg = [_('Error code not catched: '), num2str(test_case_res.res_cmd)];
  end
end
%=============================================================================
function test_case_res = post_run_test_case_skip_or_pass(test_case_res)
  if ~isfile(test_case_res.outputfile)
    return
  end
  s = jsondecode(fileread(test_case_res.outputfile));
  r = rmfile(test_case_res.outputfile);
  test_case_res.time = s.timing;
  switch (s.r)
    case 0
      test_case_res.skip = false;      
      if test_case_res.isbench
        if s.timing > 100
          test_case_res.status = [mat2str(s.timing * inv(60), 2), ' min'];
        else
          if s.timing < 1e-3
            test_case_res.status = [mat2str(s.timing * 1e3, 2), ' ms'];
          else
            test_case_res.status = [mat2str(s.timing, 2), ' s'];
          end
        end
      else
        test_case_res.status = 'Pass';
        test_case_res.msg = '';
      end
    case 1
      test_case_res.skip = false;
      test_case_res.status = 'Fail';
      test_case_res.msg = s.msg;
    case 2
      test_case_res.skip = true;
      test_case_res.status = 'Skip';
      if (strcmp(s.msg, _('Testsuite skipped')) == false)
        test_case_res.msg = s.msg;
      else
        test_case_res.msg = '';
      end
    otherwise
      error(_('Case not managed.'));
  end
  if isfile(test_case_res.redirect_err)
    r = rmfile(test_case_res.redirect_err);
  end
end
%=============================================================================
function r = isSupportedPlatform(test_case)
  is_all_platforms =  ~test_case.options.windows_only && ~test_case.options.mac_only && ~test_case.options.unix_only;
  if is_all_platforms
    r = true;
  else
    r = (ispc() && test_case.options.windows_only) || (ismac() && test_case.options.mac_only) || (isunix() && ~ismac() && test_case.options.unix_only);
  end
end
%=============================================================================
function test_case = create_test_case(filename, classname)
  test_case.filename = filename;
  test_case.options = test_parsetags(filename);
  test_case.msg = '';
  
  [p, f, e] = fileparts(filename);
  test_case.name = f;
  test_case.classname = classname;
  test_case.status = ''; % 'Pass', 'Fail', or 'Skip'. '' not evaluated.
  test_case.time = 0;
  test_case.isbench = isbench(filename);
  test_case.skip = false;
  
  if test_case.isbench
    timeout = int2str(380);
  else
    timeout = int2str(140);
  end
  
  if (test_case.options.mpi_mode && ~have_mpi())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.audio_input_required && ~have_audio_input())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.audio_output_required && ~have_audio_output())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.index_64_bit_required && ~have_64_bit_index_supported())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.c_cpp_compiler_required && ~have_c_cpp_compiler())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.excel_required && ~have_excel())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.native_architecture_required && ~is_native_platform())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.release_only && ~is_release())
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (~haveDisplay() && test_case.options.with_display)
    test_case.status = 'No display';
    test_case.skip = true;
  end
  if (test_case.options.not_fixed)
    test_case.status = 'Skip';
    test_case.skip = true;
  end
  if (test_case.options.interactive_test)
    test_case.status = 'Interactive';
    test_case.skip = true;
  end
  if ismodule('python_engine')
    pe = pyenv();
    if (test_case.options.python_environment_required && (pe.Version == ""))
      test_case.status = 'Skip';
      test_case.skip = true;
    end
  else
    if (test_case.options.python_environment_required)
      test_case.status = 'Skip';
      test_case.skip = true;
    end
  end
  if ismodule('julia_engine')
    je = jlenv();
    if (test_case.options.julia_environment_required && (je.Version == ""))
      test_case.status = 'Skip';
      test_case.skip = true;
    end
  else
    if (test_case.options.julia_environment_required)
      test_case.status = 'Skip';
      test_case.skip = true;
    end
  end
  
  test_case.command = 'echo skip';
  test_case.outputfile = '';
  test_case.redirect_err = '';
  test_case.command_filename = '';
  if ~test_case.skip
    if isSupportedPlatform(test_case)
      testfile = test_case.filename;
      [p, f, e] = fileparts(testfile);
      outputfile = [tempdir(), '' , f, '.output'];
      if isfile(outputfile)
        r = rmfile(outputfile);
      end
      redirect_err = [tempdir(), '' , f, '.err'];
      r = rmfile(redirect_err);
      cmd = '';
      
      command_filename = [tempdir(), 'test_', createGUID(), '.m'];
      content = {['res_struct = test_runfile(''', testfile, ''',', int2str(test_case.options.mpi_mode),');'];
      ['filewrite(''', outputfile, ''', jsonencode(res_struct));']};
      if test_case.options.mpi_mode
        content = [content; 'exit();'];
      else
        content = [content; 'exit(double(res_struct.r), ''force'');'];
      end
      filewrite(command_filename, content);
      
      if test_case.options.english_imposed
        cmd = [' --language', ' ', 'en_US'];
      end
      if test_case.options.no_user_modules
        cmd = [cmd, ' ', '--nousermodules'];
      end
      if ~test_case.options.ipc_required
        cmd = [cmd, ' ', '--noipc'];
      end
      if ~test_case.options.file_watcher_required
        cmd = [cmd, ' ', '--withoutfilewatcher'];
      end
      
      if ispc()
        redirect_to_file = [' 2>&1 "' , redirect_err, '"'];
      else
        redirect_to_file = [' 2>&1 ''' , redirect_err, ''''];
      end
      without_audio = ~(test_case.options.audio_input_required || test_case.options.audio_output_required) && ~endsWith(test_case.classname, '.audio');
      if without_audio
        cmd = [cmd, ' ', '--noaudio'];
      end
      
      without_python = ~test_case.options.python_environment_required;
      if without_python
        cmd = [cmd, ' ', '--without_python'];
      end

      without_julia = ~test_case.options.julia_environment_required;
      if without_julia
        cmd = [cmd, ' ', '--without_julia'];
      end

      
      cmd = [cmd, ' --quiet', ' ', '--nouserstartup', ' ', '--timeout', ' ', timeout, ' ', '--file', ' "', command_filename, '" ', redirect_to_file];
      if test_case.options.gui_mode
        test_case.command = build_command_nelson_gui(cmd, test_case.options.mpi_mode);
      elseif test_case.options.adv_cli_mode
        test_case.command = build_command_nelson_adv_cli(cmd, test_case.options.mpi_mode);
      else
        test_case.command = build_command_nelson_cli(cmd, test_case.options.mpi_mode);
      end
      test_case.outputfile = outputfile;
      test_case.redirect_err = redirect_err;
      test_case.command_filename = command_filename;
    else
      test_case.status = 'Skip';
      test_case.skip = true;
    end
  end
end
%=============================================================================
function test_suite = create_test_suite()
  test_suite.name = '';
  test_suite.tests = 0;
  test_suite.disabled = 0;
  test_suite.errors = 0;
  test_suite.passed = 0;
  test_suite.bench = 0;
  test_suite.hostname = hostname();
  test_suite.id = createGUID();
  test_suite.package = '';
  test_suite.skipped = 0;
  test_suite.time = 0;
  test_suite.timestamp = timestamp(); % 2017-01-21T16:17:18
  test_suite.environment = get_environment_test();
  test_suite.test_cases_list = struct([]);
end
%=============================================================================
function test_suites = create_test_suites()
  test_suites.disabled = 0;
  test_suites.errors = 0;
  test_suites.name = '';
  test_suites.passed = 0;
  test_suites.tests = 0;
  test_suites.bench = 0;
  test_suites.time = 0;
  test_suites.tests_list = struct([]);
end
%=============================================================================
function t = timestamp()
  n = now();
  v = datevec(n);
  t = sprintf('%d-%02d-%02dT%02d:%02d:%02d', v(1), v(2), v(3), v(4), v(5), v(6));
end
%=============================================================================
function info = get_environment_test()
  platform = '';
  if ismac()
    platform = 'macos';
  else
    if isunix()
      platform = 'unix';
    else
      if ispc()
        platform = 'windows';
      else
        platform = 'unknow';
      end
    end
  end
  compiler = version('-compiler');
  compiler_str = [];
  for c = compiler
    compiler_str = [compiler_str, ' ', c{1}];
  end
  info = ['Nelson', ' ', version(), ' ', platform, ' ', compiler_str];
end
%=============================================================================
function tests_list = getFilesListByOption(tests_dir, option)
  tests_list = [];
  switch option
    case 'all'
      unitary_tests = dir([tests_dir, 'test_*.m']);
      if ~isempty(unitary_tests)
        unitary_tests = sort(string(strcat(tests_dir, {unitary_tests.name})));
      else
        unitary_tests = string({});
      end
      
      nonreg_tests = dir([tests_dir, 'bug_*.m']);
      if ~isempty(nonreg_tests)
        nonreg_tests = sort(string(strcat(tests_dir, {nonreg_tests.name})));
      else
        nonreg_tests = string({});
      end
      
      bench_tests = dir([tests_dir, 'bench_*.m']);
      if ~isempty(bench_tests)
        bench_tests = sort(string(strcat(tests_dir, {bench_tests.name})));
      else
        bench_tests = string({});
      end
      
      all_tests = [unitary_tests, nonreg_tests, bench_tests];
      if ~isempty(all_tests)
        all_tests(all_tests == "") = [];
        tests_list = cell(all_tests);
      else
        tests_list = {};
      end
      
    case 'all_tests'
      unitary_tests = dir([tests_dir, 'test_*.m']);
      if ~isempty(unitary_tests)
        unitary_tests = sort(string(strcat(tests_dir, {unitary_tests.name})));
      else
        unitary_tests = string({});
      end
      
      nonreg_tests = dir([tests_dir, 'bug_*.m']);
      if ~isempty(nonreg_tests)
        nonreg_tests = sort(string(strcat(tests_dir, {nonreg_tests.name})));
      else
        nonreg_tests = string({});
      end
      
      all_tests = [unitary_tests, nonreg_tests];
      if ~isempty(all_tests)
        all_tests(all_tests == "") = [];
        tests_list = cell(all_tests);
      else
        tests_list = {};
      end
      
    case 'unitary_tests'
      unitary_tests = dir([tests_dir, 'test_*.m']);
      if ~isempty(unitary_tests)
        unitary_tests = sort(string(strcat(tests_dir, {unitary_tests.name})));
        unitary_tests(unitary_tests == "") = [];
        tests_list = cell(unitary_tests);
      else
        tests_list = {};
      end
      
    case 'nonreg_tests'
      nonreg_tests = dir([tests_dir, 'bug_*.m']);
      if ~isempty(nonreg_tests)
        nonreg_tests = sort(string(strcat(tests_dir, {nonreg_tests.name})));
        nonreg_tests(nonreg_tests == "") = [];
        tests_list = cell(nonreg_tests);
      else
        tests_list = {};
      end
      
    case 'benchs'
      bench_tests = dir([tests_dir, 'bench_*.m']);
      if ~isempty(bench_tests)
        bench_tests = sort(string(strcat(tests_dir, {bench_tests.name})));
        bench_tests(bench_tests == "") = [];
        tests_list = cell(bench_tests);
      else
        tests_list = {};
      end
      
    otherwise
      error(_('Argument not managed.'));
    end
  end
  %=============================================================================
function test_suites = create_test_suites()
  test_suites.disabled = 0;
  test_suites.errors = 0;
  test_suites.name = '';
  test_suites.passed = 0;
  test_suites.tests = 0;
  test_suites.bench = 0;
  test_suites.time = 0;
  test_suites.tests_list = struct([]);
end
%=============================================================================
function [modules_list, files_to_test, result_outputfile, option, stoponfail] = parseInputArguments(nargin, args)
  stoponfail = false;
  modules_list = [];
  files_to_test = [];
  result_outputfile = [];
  option = 'all';
  supported_options = {'all', 'all_tests', 'unitary_tests', 'nonreg_tests', 'benchs'};
  switch nargin
    case 0
      % status = test_run()
      modules_list = getAllModulesList();
    case 1
      % status = test_run([])
      % status = test_run('minimal_tests')
      % status = test_run('-stoponfail')
      % status = test_run(modules)
      % status = test_run(file_to_test)
      param1 = args{1};
      if isequal(size(param1), [0 0]) && isdouble(param1)
        if isdouble(param1)
          modules_list = getAllModulesList();
        else
          error(_('[] expected.'));
        end
      end
      
      modules_list = getModulesToTest(param1);
      files_to_test = getFilesToTest(param1);
      if ~isempty(modules_list) || ~isempty(files_to_test)
        option = 'all';
      else
        stoponfail = haveStopOnFailOption(param1);
        if ~stoponfail
          option = getOption(param1);
        end
      end
    case 2
      % status = test_run(modules, '-stoponfail')
      % status = test_run(file_to_test, '-stoponfail')
      % status = test_run(modules, option)
      % status = test_run(file_to_test, option)
      % status = test_run('minimal_tests', '-stoponfail')
      % status = test_run('minimal_tests', option)
      % status = test_run([], '-stoponfail')
      % status = test_run([], option)
      % status = test_run(modules, file_output)
      % status = test_run(file_to_test, file_output)
      % status = test_run([], file_output)
      param1 = args{1};
      if isequal(size(param1), [0 0]) && isdouble(param1)
        if isdouble(param1)
          modules_list = getAllModulesList();
        else
          error(_('[] expected.'));
        end
      else
        modules_list = getModulesToTest(param1);
        files_to_test = getFilesToTest(param1);
        if ~isempty(modules_list) && ~isempty(files_to_test)
          error(_('#1 argument: cell of modules or filenames expected.'));
        end
        if ischar(param1) && strcmp(param1, 'minimal_tests')
          option = 'unitary_tests';
        end
      end
      param2 = args{2};
      stoponfail = haveStopOnFailOption(param2);
      if ~stoponfail
        option = getOption(param2);
        if isempty(option)
          option = 'all';
          if ischar(param2)
            result_outputfile = param2;
          end
        end
      end
      
    case 3
      % status = test_run(modules, option, output_file)
      % status = test_run(modules, '-stoponfail', output_file)
      param1 = args{1};
      if isequal(size(param1), [0 0]) && isdouble(param1)
        if isdouble(param1)
          modules_list = getAllModulesList();
        else
          error(_('[] expected.'));
        end
      else
        modules_list = getModulesToTest(param1);
        files_to_test = getFilesToTest(param1);
        if ~isempty(modules_list) && ~isempty(files_to_test)
          error(_('#1 argument: cell of modules or filenames expected.'));
        end
      end
      param2 = args{2};
      stoponfail = haveStopOnFailOption(param2);
      if ~stoponfail
        option = getOption(param2);
        if isequal(size(option), [0 0]) && isdouble(option)
          if isdouble(option)
            if ischar(param1) && strcmp(param1, 'minimal_tests')
              option = 'unitary_tests';
            else
              option = 'all';
            end
          else
            error(_('option expected.'));
          end
        end
      end
      param3 = args{3};
      if ischar(param3)
        result_outputfile = param3;
      else
        error(_('filename expected.'));
      end
      
    case 4
      % status = test_run(modules, option, output_file, '-stoponfail')
      % status = test_run(modules, option, output_file, [])
      param1 = args{1};
      param2 = args{2};
      param3 = args{3};
      param4 = args{4};
      if isequal(size(param1), [0 0]) && isdouble(param1)
        if isdouble(param1)
          modules_list = getAllModulesList();
        else
          error(_('[] expected.'));
        end
      else
        modules_list = getModulesToTest(param1);
        files_to_test = getFilesToTest(param1);
        if ~isempty(modules_list) && ~isempty(files_to_test)
          error(_('#1 argument: cell of modules or filenames expected.'));
        end
      end
      option = getOption(param2);
      if isempty(option)
        error(_('#2 argument: option expected.'));
      end
      if ischar(param3)
        result_outputfile = param3;
      else
        error(_('#3 argument: filename expected.'));
      end
      if isequal(size(param4), [0 0]) && isdouble(param4)
        if ~isdouble(param4)
          error(_('#4 argument: ''-stoponfail'' or [] expected'));
        end
      else
        stoponfail = haveStopOnFailOption(param4);
        if ~stoponfail
          error(_('#4 argument: ''-stoponfail'' or [] expected'));
        end
      end
      
    otherwise
      error(_('Wrong number of input arguments.'));
    end
  end
  %=============================================================================
function modules = getModulesToTest(param)
  modules = [];
  if iscellstr(param)
    for k = param
      if ismodule(k{1})
        if isempty(modules)
          modules = k;
        else
          modules = [modules; k{1}];
        end
      end
    end
  elseif ischar(param)
    if ismodule(param)
      modules = {param};
    elseif strcmp(param, 'minimal_tests')
      run([nelsonroot(), '/modules/tests_manager/minimal_tests.m']);
      modules = minimal_tests;
    end
  end
end
%=============================================================================
function list = readAvailableModulesFromFile()
  run([nelsonroot(), '/modules/modules.m']);
  funcList = @(x) x{1};
  list = string(cellfun(funcList, modules_list, 'UniformOutput', false));
  funcAvailable = @(x) x{2};
  available = cellfun(funcAvailable, modules_list, 'UniformOutput', true);
  list(~available) = [];
end
%=============================================================================
function modules_list = getAllModulesList()
  modules_list = readAvailableModulesFromFile();
  current_modules = getmodules();
  if isempty(modules_list)
    modules_list = current_modules;
  end
  if ~isequal(modules_list, current_modules)
    for m = current_modules'
      if ~any(contains(modules_list, m{1}))
        modules_list = [modules_list; m{1}];
      end
    end
  end
  withAudioModule = ~strcmp(getenv('AUDIODEV'), 'null');
  if strcmp(getenv('AUDIODEV'), 'null')
    modules_list(modules_list == "audio") = [];
  end
  withoutIPC = strcmp(getenv('NELSON_WITHOUT_IPC'), 'TRUE');
  if withoutIPC
    modules_list(modules_list == "ipc") = [];
  end
  withoutMPI = strcmp(getenv('NELSON_WITHOUT_MPI'), 'TRUE');  
  if withoutMPI  
    modules_list(modules_list == "mpi") = [];  
  end
  withoutSlicot = ~ismodule('slicot');
  if withoutSlicot
    modules_list(modules_list == "slicot") = [];  
  end
end
%=============================================================================
function files = getFilesToTest(param)
  files = [];
  if iscellstr(param)
    for k = param
      if isfile(k{1})
        if isempty(files)
          files = k;
        else
          files = [files; k{1}];
        end
      end
    end
  elseif ischar(param)
    if isfile(param)
      files = {param};
    end
  end
end
%=============================================================================
function r = isbench(filename)
  [p, fwext] = fileparts(filename);
  r = startsWith(fwext, 'bench_');
end
%=============================================================================
function r = have_mpi()
  persistent mpi_required
  if isempty(mpi_required)
    if ispc()
      mpi_required = (unix('mpiexec -help') == 0);
    else
      mpi_required = (unix('mpiexec --help') == 0);
    end
  end
  r = mpi_required;
end
%=============================================================================
function r = have_audio_input()
  persistent audio_input_required
  if isempty(audio_input_required)
    audio_input_required = false;
    if ismodule('audio')
      if strcmp(getenv('AUDIODEV'), 'null') == true
        audio_input_required = false;
      else
        devices = audiodevinfo('default');
        audio_input_required = ~isempty(devices.input);
      end
    end
  end
  r = audio_input_required;
end
%=============================================================================
function r = have_audio_output()
  persistent audio_output_required;
  if isempty(audio_output_required)
    audio_output_required = false;
    if ismodule('audio')
      if strcmp(getenv('AUDIODEV'), 'null') == true
        audio_output_required = false;
      else
        devices = audiodevinfo('default');
        audio_output_required = ~isempty(devices.output);
      end
    end
  end
  r = audio_output_required;
end
%=============================================================================
function r = have_excel()
  persistent excel_detected;
  if isempty(excel_detected)
    excel_detected = false;
    if ismodule('com_engine')
      if ispc()
        try
          excelApplication = actxserver('Excel.Application');
          excelApplication.Quit;
          delete(excelApplication);
          excel_detected = true;
        catch
          excel_detected = false;
        end
      end
    end
  end
  r = excel_detected;
end
%=============================================================================
function r = have_c_cpp_compiler()
  persistent compiler_detected;
  if isempty(compiler_detected)
    compiler_detected = havecompiler();
  end
  r = compiler_detected;
end
%=============================================================================
function r = have_64_bit_index_supported()
  persistent have_64_bit_index;
  if isempty(have_64_bit_index)
    compiler_info = version('-compiler');
    have_64_bit_index = strcmp(compiler_info{3}, '64');
  end
  r = have_64_bit_index;
end
%=============================================================================
function r = is_release()
  persistent is_release_version;
  if isempty(is_release_version)
    ver_comp = version('-compiler');
    is_release_version = startsWith(ver_comp{2}, 'release');
  end
  r = is_release_version;
end
%=============================================================================
function r = is_native_platform()
  persistent is_native;
  if isempty(is_native)
    if ispc()
      ver_comp = version('-compiler');
      targetAMD64 = startsWith(ver_comp{3}, '64');
      targetX86 = startsWith(ver_comp{3}, '32');
      arch = getenv('PROCESSOR_ARCHITECTURE');
      archW6432 = getenv('PROCESSOR_ARCHITEW6432');
      if targetX86
        is_native = strcmp(arch, 'x86') && strcmp(archW6432, '');
      elseif targetAMD64
        is_native = strcmp(arch, 'AMD64') && strcmp(archW6432, '');
      else
        is_native = false;
      end
    else
      is_native = true;
    end
  end
  r = is_native;
end
%=============================================================================
function res = haveDisplay()
  persistent withDisplay;
  if isempty(withDisplay)
    withDisplay = true;
    if ~ispc()
      if ~ismac()
        DISPLAY_ENV = getenv('DISPLAY');
        withDisplay = ~strcmp(DISPLAY_ENV, '');
      end
    end
  end
  res = withDisplay;
end
%=============================================================================
function cmd = build_command_nelson_cli(command, withMPI)
  cmd = build_command_nelson_mode('nelson-cli', command, withMPI);
end
%=============================================================================
function cmd = build_command_nelson_adv_cli(command, withMPI)
  cmd = build_command_nelson_mode('nelson-adv-cli', command, withMPI);
end
%=============================================================================
function cmd = build_command_nelson_gui(command, withMPI)
  cmd = build_command_nelson_mode('nelson-gui', command, withMPI);
end
%=============================================================================
function cmd = build_command_nelson_mode(exe, command, withMPI)
  persistent nelson_bin_path
  if isempty(nelson_bin_path)
    nelson_bin_path = modulepath('nelson', 'bin');
  end
  nelson_exe_path = ['"', nelson_bin_path, '/', exe, '"'];
  cmd = [prefixCommand(withMPI), nelson_exe_path, ' ' , command];
end
%=============================================================================
function pc = prefixCommand(withMPI)
  pc = '';
  if withMPI
    pc = ['mpiexec -n 2 '];
  end
end
%=============================================================================
function disp_summary(test_suites)
  fprintf(stdout, ['  ===============================================================================', newline]);
  fprintf(stdout, ['  ', _('Summary:'), newline]);
  fprintf(stdout, ['  ', _('Tests:'),   ' ', int2str(test_suites.tests), newline]);
  if isunicodesupported()
    fprintf(stdout, ['  ✅ ', _('Passed:'),  ' ', int2str(test_suites.passed), newline]);
    fprintf(stdout, ['  🔴 ', _('Failed:'),  ' ', int2str(test_suites.errors), newline]);
    fprintf(stdout, ['  ⭕ ', _('Skipped:'), ' ',  int2str(test_suites.disabled), newline]);
  else
    fprintf(stdout, ['  ', _('Passed:'), ' ', int2str(test_suites.passed), newline]);
    fprintf(stdout, ['  ', _('Failed:'), ' ', int2str(test_suites.errors), newline]);
    fprintf(stdout, ['  ', _('Skipped:'), ' ',  int2str(test_suites.disabled), newline]);
  end
  fprintf(stdout, ['  ', _('Benchs:'), ' ', int2str(test_suites.bench), newline]);
  if test_suites.time > 100
    fprintf(stdout, ['  ', _('Tests time:'), ' ', mat2str(test_suites.time * inv(60), 2), ' min', newline]);
  else
    fprintf(stdout, ['  ', _('Tests time:'), ' ', mat2str(test_suites.time, 2), ' s ', newline]);
  end
  if test_suites.total_time > 100
    fprintf(stdout, ['  ', _('Total time:'), ' ', mat2str(test_suites.total_time * inv(60), 2), ' min', newline]);
  else
    fprintf(stdout, ['  ', _('Total time:'), ' ', mat2str(test_suites.total_time, 2), ' s ', newline]);
  end
  fprintf(stdout, ['  ================================================================================', newline]);
end
%=============================================================================
function res = save_results(test_suites, result_outputfile)
  res = true;
  if ~isempty(result_outputfile) && ischar(result_outputfile)
    [p, f, e] = fileparts(result_outputfile);
    if strcmp(e, '.*')
      res = save_as_xml(test_suites, [p, filesep, f, '.xml']);
      res = res && save_as_json(test_suites, [p, filesep, f, '.json']);
    elseif strcmp(e, '.xml')
      res = save_as_xml(test_suites, result_outputfile);
    elseif strcmp(e, '.json')
      res = save_as_json(test_suites, result_outputfile);
    else
      res = save_as_xml(test_suites, result_outputfile);
    end
  end
end
%=============================================================================
function res = save_as_json(test_suites, jsonFileDestination)
  filewrite(jsonFileDestination, jsonprettyprint(jsonencode(test_suites)))
  res = isfile(jsonFileDestination);
end
%=============================================================================
function res = save_as_xml(test_suites, xmlFileDestination)
  res = false;
  eol = newline;
  fp = fopen(xmlFileDestination, 'wt');
  if fp ~= -1
    fprintf(fp, ['<?xml version="1.0"?>', eol]);
    fprintf(fp, ['<testsuites tests="', int2str(test_suites.tests), '" disabled="', int2str(test_suites.disabled), '" time="', mat2str(test_suites.time, 2), '" errors="', int2str(test_suites.errors),'">', eol]);
    
    for test_suite = test_suites.tests_list'
      fprintf(fp, ['  <testsuite name="', test_suite.name, '" id="', test_suite.id, '" time="', mat2str(test_suite.time, 2), '" tests="', int2str(test_suite.tests), '" skipped="', int2str(test_suite.disabled),'" errors="', int2str(test_suite.errors), '" hostname="', test_suite.hostname, '" timestamp="', test_suite.timestamp, '" ', '>', eol]);
      fprintf(fp, ['    <properties>', eol]);
      fprintf(fp, ['      <property name="', 'environment', '" value="', test_suite.environment,'"/>', eol]);
      fprintf(fp, ['    </properties>', eol]);
      
      for tc = test_suite.test_cases_list
        for t = tc'
          if t.isbench && strcmp(t.status, 'Fail') == false
            t.status = "Pass";
          end
          if strcmp(t.status, 'Pass') == true
            fprintf(fp, ['    <testcase name="', t.name, '" time="', mat2str(t.time, 2), '" classname="', t.classname, '">', eol]);
            if isstruct(t.msg)
              msg = t.msg.message;
            else
              msg = t.msg;
            end
            fprintf(fp, ['    </testcase>', eol]);
          elseif strcmp(t.status, 'Fail') == true
            fprintf(fp, ['    <testcase name="', t.name, '" time="', mat2str(t.time, 2), '" classname="', t.classname, '">', eol]);
            if isstruct(t.msg)
              msg = t.msg.message;
            else
              msg = t.msg;
            end
            if length(msg) > 0
              if iscellstr(msg)
                fprintf(fp, ['      <error type="error" message="', _('test failed'), '">', '<![CDATA[', eol]);
                for m = msg(:)'
                  fprintf(fp, [m{1}, eol]);
                end
                fprintf(fp, [']]>', '</error>', eol]);
              else
                fprintf(fp, ['      <error type="error" message="', _('test failed'), '">', '<![CDATA[', msg, ']]>', '</error>', eol]);
              end
            end
            fprintf(fp, ['    </testcase>', eol]);
          elseif strcmp(t.status, 'Skip') == true || strcmp(t.status, 'Interactive') == true || strcmp(t.status, 'No display') == true
            fprintf(fp, ['    <testcase name="', t.name, '" time="', mat2str(t.time, 2), '" classname="', t.classname, '"/>', eol]);
            fprintf(fp, ['      <skipped/>', eol]);
          end
        end
        fprintf(fp, ['  </testsuite>', eol]);
      end
      fprintf(fp, ['</testsuites>', eol]);
    end
    fclose(fp);
    res = true;
  end
end
%=============================================================================
function res = haveStopOnFailOption(param)
  res = false;
  if ischar(param)
    res = strcmp(param, '-stoponfail');
  end
end
%=============================================================================
function option = getOption(param)
  supported_options = {'all', 'all_tests', 'unitary_tests', 'nonreg_tests', 'benchs'};
  option = [];
  if ischar(param)
    if strcmp(param, 'minimal_tests')
      option = 'unitary_tests';
    else
      found_option = any(contains(supported_options, param));
      if found_option
        option = param;
      end
    end
  end
end
%=============================================================================
