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
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
mkdir(source);
entry = fullfile(source, 'custom_help_entry.m');
helpfile = fullfile(source, 'help.txt');
expected = ['APPLICATION_HELP 100% %s', char(10), char([233, 955, 20013]), char(10)];
fid = fopen(helpfile, 'wb');
fwrite(fid, unicode2native(expected, 'UTF-8'));
fclose(fid);
asserts.isequal(__nelsonc_help_text__(helpfile), expected);
filewrite(entry, 'function custom_help_entry(varargin); disp(''ENTRY_EXECUTED''); disp(jsonencode(varargin)); end');
extension = '';
if ispc()
  extension = '.exe';
end
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreEnvironment = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
applications = {};
for mode = {'bundled', 'installed'}
  applications{end + 1} = fullfile(directory, [mode{1}, extension]);
  ncc(entry, '--cli', '--runtime', mode{1}, '--help-file', helpfile, '-o', applications{end});
end
plain = fullfile(directory, ['plain', extension]);
ncc(entry, '--cli', '--runtime', 'installed', '-o', plain);
bad = fullfile(source, 'bad.txt');
fid = fopen(bad, 'wb');
fwrite(fid, uint8([195, 40]));
fclose(fid);
asserts.checkerror('__nelsonc_help_text__(bad)', 'Application help must contain valid UTF-8 text.');
fid = fopen(bad, 'wb');
fwrite(fid, uint8([65, 0, 66]));
fclose(fid);
asserts.checkerror('__nelsonc_help_text__(bad)', 'Application help must not exceed 1 MiB or contain null characters.');
fid = fopen(bad, 'wb');
fwrite(fid, repmat(uint8(65), 1, 1048577));
fclose(fid);
asserts.checkerror('__nelsonc_help_text__(bad)', 'Application help must not exceed 1 MiB or contain null characters.');
filewrite(bad, '');
asserts.isequal(__nelsonc_help_text__(bad), '');
emptyApplication = fullfile(directory, ['empty', extension]);
ncc(entry, '--cli', '--runtime', 'installed', '--help-file', bad, '-o', emptyApplication);
fid = fopen(bad, 'wb');
fwrite(fid, [uint8([239, 187, 191]), unicode2native(expected, 'UTF-8')]);
fclose(fid);
asserts.isequal(__nelsonc_help_text__(bad), expected);
missing = fullfile(source, 'missing.txt');
asserts.checkerror('__nelsonc_help_text__(missing)', 'Application help must be an existing UTF-8 file.');
asserts.checkerror('ncc(entry, ''--help-file'')', 'Missing value after --help-file.');
rejected = fullfile(directory, 'not-created', ['bad', extension]);
asserts.checkerror('ncc(entry, ''--help-file'', missing, ''-o'', rejected)', 'Application help must be an existing UTF-8 file.');
asserts.isfalse(isfolder(fileparts(rejected)));
rmdir(source, 's');
captured = fullfile(directory, 'help-output.txt');
for k = 1:numel(applications)
  for flag = {'-?', '/?', '--help'}
    [status, message] = system(['"', applications{k}, '" ', flag{1}, ' > "', captured, '" 2>&1'], 60);
    output = fileread(captured);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(strrep(output, char(13), ''), expected), output);
    asserts.isfalse(contains(output, 'ENTRY_EXECUTED'), output);
  end
  [status, output] = system(['"', applications{k}, '" caller --help 2>&1'], 60);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, 'ENTRY_EXECUTED'), output);
  asserts.istrue(contains(output, '["caller","--help"]'), output);
end
[status, output] = system(['"', plain, '" --help 2>&1'], 60);
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, 'ENTRY_EXECUTED'), output);
[status, output] = system(['"', emptyApplication, '" --help 2>&1'], 60);
asserts.isequal(status, 0, output);
asserts.isequal(output, '');
clear restoreEnvironment cleanup;
