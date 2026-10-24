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
if ispc() || ismac(); return; end
ncc('--help');
benchmark_registry();

function benchmark_registry()
  work = tempname(); mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'bench_registry.m');
  code = {'function bench_registry()', ...
    'maxNumCompThreads(1); x = linspace(0, pi, 8192);', ...
    'for k = 1:20; y = sum(sin(x)); end', ...
    'tic; for k = 1:2000; y = sum(sin(x)); end; elapsed = toc;', ...
    'fprintf(''WARMED_SECONDS=%.9f\nRESULT=%.17g\n'', elapsed, y);', 'end'};
  filewrite(entry, strjoin(code, newline()));
  built = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'build'));
  plan = __nelsonc_distribution_plan__(built);
  runtime = fullfile(work, 'shared');
  __nelsonc_copy_runtime__(plan.runtime, runtime);
  rmfile(entry);
  config = fullfile(work, 'configuration');
  directory = fullfile(config, 'nelson', 'runtimes', plan.architecture); mkdir(directory);
  filewrite(fullfile(directory, [plan.engineFingerprint, '.root']), ...
    ['NELSON_RUNTIME_ROOT_1', newline(), runtime, newline()]);
  names = {'NELSONC_RUNTIME_ROOT', 'NELSON_RUNTIME_PATH', 'LD_LIBRARY_PATH', ...
    'PATH', 'HOME', 'XDG_CONFIG_HOME', 'XDG_CONFIG_DIRS'};
  values = cellfun(@getenv, names, 'UniformOutput', false);
  restore = onCleanup(@() restore_environment(names, values));
  setenv('NELSON_RUNTIME_PATH', ''); setenv('LD_LIBRARY_PATH', '');
  setenv('PATH', '/usr/bin:/bin'); setenv('HOME', work);
  setenv('XDG_CONFIG_HOME', config); setenv('XDG_CONFIG_DIRS', fullfile(work, 'empty-system'));
  quote = char(39);
  command = [quote, strrep(built.Files{1}, quote, char([39 34 39 34 39])), quote];
  wall = zeros(2, 9); warmed = wall; results = wall;
  for sample = 1:9
    order = [1, 2]; if mod(sample, 2) == 0; order = [2, 1]; end
    for index = order
      if index == 1; setenv('NELSONC_RUNTIME_ROOT', runtime); else; setenv('NELSONC_RUNTIME_ROOT', ''); end
      timer = tic(); [status, output] = system(command, 45); wall(index, sample) = toc(timer);
      asserts.isequal(status, 0, output);
      value = regexp(output, 'WARMED_SECONDS=([0-9.]+)\s+RESULT=([0-9.eE+-]+)', 'tokens', 'once');
      asserts.isequal(numel(value), 2, output);
      warmed(index, sample) = str2double(value{1});
      results(index, sample) = str2double(value{2});
    end
  end
  asserts.isequal(results, repmat(results(1, 1), 2, 9));
  info = dir(built.Files{1});
  fprintf('RUNTIME_REGISTRY_BENCH=%s\n', jsonencode(struct(...
    'Modes', {{'explicit', 'registered'}}, 'ExecutableBytes', info.bytes, ...
    'FirstProcessSeconds', wall(:, 1)', 'MedianLaterProcessSeconds', median(wall(:, 2:end), 2)', ...
    'MedianWarmedSeconds', median(warmed, 2)', 'ProcessSeconds', wall, 'WarmedSeconds', warmed)));
  clear restore cleanup;
end

function restore_environment(names, values)
  for k = 1:numel(names); setenv(names{k}, values{k}); end
end
