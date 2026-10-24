%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
% <--TIMEOUT 300-->
ncc('--help');
benchmark_logging();

function benchmark_logging()
  work = tempname(); mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'bench_log.m');
  code = {'function bench_log()', 'maxNumCompThreads(1);', ...
    'value = compute(); tic; value = compute(); elapsed = toc;', ...
    'fprintf(''WARMED_SECONDS=%.9f\nRESULT=%.17g\n'', elapsed, value);', 'end', ...
    'function value = compute()', 'value = 0;', ...
    'for k = 1:1000000; value = value + k; end', 'end'};
  filewrite(entry, strjoin(code, newline()));
  options = compiler.build.StandaloneApplicationOptions(entry, 'OutputDir', fullfile(work, 'plain'));
  plain = compiler.build.standaloneApplication(options);
  options.RuntimeLogFile = '';
  options.OutputDir = fullfile(work, 'explicit_empty');
  same = compiler.build.standaloneApplication(options);
  asserts.isequal(sha256(plain.Files{1}, '-file'), sha256(same.Files{1}, '-file'));
  options.RuntimeLogFile = 'runtime.log';
  options.OutputDir = fullfile(work, 'logged');
  logged = compiler.build.standaloneApplication(options);
  applications = {plain, logged};
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  rmfile(entry);
  wall = zeros(2, 9); warmed = wall; sizes = zeros(1, 2);
  for sample = 1:9
    order = [1, 2]; if mod(sample, 2) == 0; order = [2, 1]; end
    for index = order
      executable = applications{index}.Files{1};
      timer = tic(); [status, output] = system(['"', executable, '"'], 60);
      wall(index, sample) = toc(timer);
      asserts.isequal(status, 0, output);
      values = regexp(output, 'WARMED_SECONDS=([0-9.]+)\s+RESULT=([0-9.eE+-]+)', 'tokens', 'once');
      asserts.isequal(numel(values), 2, output);
      warmed(index, sample) = str2double(values{1});
      asserts.isequal(str2double(values{2}), 500000500000);
      info = dir(executable); sizes(index) = info.bytes;
    end
  end
  text = fileread(fullfile(logged.Options.OutputDir, 'runtime.log'));
  asserts.isequal(numel(strfind(text, 'RESULT=500000500000')), 9);
  asserts.isfalse(isfile(fullfile(plain.Options.OutputDir, 'runtime.log')));
  a = plain.RuntimeDependencies.__nelsonc_snapshot__();
  b = logged.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.isequal({a.files.relative}, {b.files.relative});
  asserts.isequal({a.files.sha256}, {b.files.sha256});
  fprintf('RUNTIME_LOG_BENCH=%s\n', jsonencode(struct('ExecutableBytes', sizes, ...
    'FirstProcessSeconds', wall(:, 1)', 'MedianLaterProcessSeconds', median(wall(:, 2:end), 2)', ...
    'MedianWarmedSeconds', median(warmed, 2)', 'ProcessSeconds', wall, 'WarmedSeconds', warmed)));
  clear restore cleanup;
end
