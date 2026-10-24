%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'windowed_entry.m');
filewrite(entry, 'function windowed_entry(); disp(''WINDOWED_ENTRY_OK''); end');
opts = ncc('options', entry, 'OutputDir', directory, 'RuntimeMode', 'installed', 'Mode', 'cli');
asserts.isfalse(opts.NoConsole);
if ~ispc()
  asserts.checkerror('opts.NoConsole = true;', 'No-console applications require Windows.');
  asserts.checkerror('ncc(entry, ''--no-console'', ''--explain-link'')', 'No-console applications require Windows.');
  options = struct('noConsole', true);
  output = fullfile(directory, 'rejected');
  asserts.checkerror('__nelsonc_build__(entry, output, {}, false, ''installed'', options)', 'No-console applications require Windows.');
  asserts.isfalse(isfile(output));
else
  console = ncc(opts);
  asserts.isequal(pe_subsystem(console.Executable), 3);
  opts.NoConsole = true;
  opts.ExecutableName = 'windowed';
  plan = nelson.compiler.analyze(opts);
  asserts.istrue(plan.noConsole);
  asserts.isfalse(plan.runtime.gui);
  result = nelson.compiler.build(opts);
  asserts.isequal(pe_subsystem(result.Executable), 2);
  asserts.isequal(result.BuildType, 'standaloneWindowsApplication');
  asserts.istrue(result.Options.NoConsole);
  asserts.istrue(result.DependencyPlan.noConsole);
  asserts.isfalse(console.DependencyPlan.noConsole);
  asserts.isequal(result.RuntimePlan, console.RuntimePlan);
  asserts.isequal(result.Manifest, console.Manifest);
  asserts.isequal(archive_digest(result.Executable), archive_digest(console.Executable));
  opts.OutputDir = fullfile(directory, 'repeat');
  repeated = ncc(opts);
  asserts.isequal(repeated.SHA256, result.SHA256);
  legacy = ncc(entry, '-o', fullfile(directory, 'legacy.exe'), '--runtime', 'installed', '--cli', '--no-console');
  asserts.isequal(sha256(legacy, '-file'), result.SHA256);
  asserts.isequal(result.Files, {result.Executable});
  asserts.isequal(result.RuntimeDirectory, '');
end
clear cleanup;

function subsystem = pe_subsystem(filename)
  fid = fopen(filename, 'rb');
  closer = onCleanup(@() fclose(fid));
  fseek(fid, 60, 'bof');
  header = fread(fid, 1, 'uint32');
  fseek(fid, header + 24 + 68, 'bof');
  subsystem = double(fread(fid, 1, 'uint16'));
end

function digest = archive_digest(filename)
  fid = fopen(filename, 'rb');
  closer = onCleanup(@() fclose(fid));
  fseek(fid, -352 + 32, 'eof');
  digest = char(fread(fid, 64, '*uint8'))';
end
