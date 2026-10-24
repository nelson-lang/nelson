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
benchmark_inputs();

function benchmark_inputs()
  work = tempname(); mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'bench_entry.m');
  code = {'function bench_entry(n)', ...
    'if ischar(n); n = str2double(n); end', ...
    'maxNumCompThreads(1); x = linspace(0, pi, 8192);', ...
    'for k = 1:20; y = sum(sin(x)); end', ...
    'tic; for k = 1:n; y = sum(sin(x)); end; elapsed = toc;', ...
    'fprintf(''WARMED_SECONDS=%.9f\nRESULT=%.17g\n'', elapsed, y);', 'end'};
  filewrite(entry, strjoin(code, newline()));
  options = compiler.build.StandaloneApplicationOptions(entry, 'OutputDir', fullfile(work, 'plain'));
  plain = compiler.build.standaloneApplication(options);
  options.TreatInputsAsNumeric = false;
  options.OutputDir = fullfile(work, 'explicit_false');
  same = compiler.build.standaloneApplication(options);
  asserts.isequal(sha256(plain.Files{1}, '-file'), sha256(same.Files{1}, '-file'));
  options.TreatInputsAsNumeric = true;
  options.OutputDir = fullfile(work, 'numeric');
  numeric = compiler.build.standaloneApplication(options);
  applications = {plain, numeric};
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  rmfile(entry);
  wall = zeros(2, 7); warmed = wall; results = wall; sizes = zeros(1, 2);
  for sample = 1:7
    order = [1, 2]; if mod(sample, 2) == 0; order = [2, 1]; end
    for index = order
      executable = applications{index}.Files{1};
      timer = tic();
      [status, output] = system(['"', executable, '" 500'], 60);
      wall(index, sample) = toc(timer);
      asserts.isequal(status, 0, output);
      values = regexp(output, 'WARMED_SECONDS=([0-9.]+)\s+RESULT=([0-9.eE+-]+)', 'tokens', 'once');
      asserts.isequal(numel(values), 2, output);
      warmed(index, sample) = str2double(values{1});
      results(index, sample) = str2double(values{2});
      info = dir(executable); sizes(index) = info.bytes;
    end
  end
  asserts.isequal(results, repmat(results(1, 1), 2, 7));
  a = plain.RuntimeDependencies.__nelsonc_snapshot__();
  b = numeric.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.isequal({a.files.relative}, {b.files.relative});
  fprintf('NUMERIC_INPUT_BENCH=%s\n', jsonencode(struct('ExecutableBytes', sizes, ...
    'FirstProcessSeconds', wall(:, 1)', 'MedianLaterProcessSeconds', median(wall(:, 2:end), 2)', ...
    'MedianWarmedSeconds', median(warmed, 2)', 'ProcessSeconds', wall, 'WarmedSeconds', warmed)));
  clear restore cleanup;
end
