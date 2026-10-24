%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
ncc('--help');
check_external();

function check_external()
  directory = [tempname(), ' archive ', char([28450, 23383])];
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  entry = fullfile(directory, 'external_entry.m');
  data = fullfile(directory, 'payload.txt');
  filewrite(data, repmat('EXTERNAL_DATA', 1, 4096));
  filewrite(entry, ['function external_entry(); ', ...
    'root = fileparts(mfilename(''fullpath'')); ', ...
    'text = fileread(fullfile(root, ''payload.txt'')); assert(numel(text) == 53248); ', ...
    'disp(''EXTERNAL_ARCHIVE_OK''); end']);
  options = compiler.build.StandaloneApplicationOptions(entry, 'EmbedArchive', 'off', ...
    'AdditionalFiles', data, 'OutputDir', fullfile(directory, 'first'), 'RuntimeLogFile', 'external.log');
  first = compiler.build.standaloneApplication(options);
  asserts.isfalse(first.Options.EmbedArchive);
  asserts.isequal(numel(first.Files), 3);
  asserts.istrue(endsWith(first.Files{2}, 'external_entry.nca'));
  asserts.istrue(contains(fileread(first.Files{3}), 'external_entry.nca'));
  __application_archive__('verify', first.Files{1});
  options.OutputDir = fullfile(directory, 'repeat');
  again = compiler.build.standaloneApplication(options);
  for k = 1:2
    asserts.isequal(sha256(first.Files{k}, '-file'), sha256(again.Files{k}, '-file'));
  end
  options.OutputDir = fullfile(directory, 'embedded');
  options.EmbedArchive = true;
  embedded = compiler.build.standaloneApplication(options);
  small = dir(first.Files{1});
  external = dir(first.Files{2});
  large = dir(embedded.Files{1});
  asserts.isequal(small.bytes + external.bytes, large.bytes);
  __application_archive__('verify', embedded.Files{1});
  options.EmbedArchive = false;
  applications = {first, embedded};
  if ispc()
    options.OutputDir = fullfile(directory, 'windowed');
    applications{end + 1} = compiler.build.standaloneWindowsApplication(options);
  end
  check_collision(options, directory);
  previous = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previous));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  rmfile(entry);
  rmfile(data);
  for k = 1:numel(applications)
    run_application(applications{k}.Files{1}, true);
  end
  relocated = fullfile(directory, 'relocated');
  mkdir(relocated);
  extension = '';
  if ispc(); extension = '.exe'; end
  executable = fullfile(relocated, ['renamed', extension]);
  archive = fullfile(relocated, 'renamed.nca');
  copyfile(first.Files{1}, executable);
  copyfile(first.Files{2}, archive);
  run_application(executable, true);
  rmfile(archive);
  run_application(executable, false);
  copyfile(first.Files{2}, archive);
  fid = fopen(archive, 'r+b');
  fwrite(fid, uint8(0), 'uint8');
  fclose(fid);
  run_application(executable, false);
  clear restore;
  clear cleanup;
end

function check_collision(options, directory)
  options.OutputDir = fullfile(directory, 'collision');
  mkdir(options.OutputDir);
  occupied = fullfile(options.OutputDir, 'external_entry.nca');
  filewrite(occupied, 'KEEP');
  failed = false;
  try; compiler.build.standaloneApplication(options); catch; failed = true; end
  asserts.istrue(failed);
  asserts.isequal(fileread(occupied), 'KEEP');
  asserts.isfalse(isfile(fullfile(options.OutputDir, 'buildresult.json')));
end

function run_application(executable, success)
  [status, output] = system(['"', executable, '"'], 60);
  if success
    asserts.isequal(status, 0, [executable, newline(), output]);
    asserts.istrue(contains(output, 'EXTERNAL_ARCHIVE_OK'), output);
  else
    asserts.istrue(status ~= 0, output);
    asserts.isfalse(contains(output, 'EXTERNAL_ARCHIVE_OK'), output);
  end
end
