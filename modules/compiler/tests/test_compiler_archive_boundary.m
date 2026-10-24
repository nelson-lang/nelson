%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 300-->
ncc('--help');
check_boundary();

function check_boundary()
  work = [tempname(), ' compiler boundary'];
  mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'boundary_entry.m');
  filewrite(entry, ['function boundary_entry(); ', ...
    'assert(~ismodule(''compiler'')); assert(isempty(which(''__nelsonc_archive__''))); ', ...
    'assert(~isfolder(fullfile(nelsonroot(), ''modules'', ''compiler''))); ', ...
    'disp(''COMPILER_FREE_RUNTIME_OK''); end']);
  outputs = {};
  extension = '';
  if ispc(); extension = '.exe'; end
  for embedded = [true, false]
    folder = fullfile(work, int2str(double(embedded)));
    output = fullfile(folder, ['boundary', extension]);
    args = {};
    if ~embedded; args = {'--external-archive'}; end
    outputs{end + 1} = ncc(entry, '-o', output, '--runtime', 'bundled', args{:});
    runtime = fullfile(folder, 'boundary.runtime');
    asserts.isfalse(isfolder(fullfile(runtime, 'modules', 'compiler')));
    asserts.istrue(isfolder(fullfile(runtime, 'modules', 'file_archiver')));
    inventory = jsondecode(fileread(fullfile(runtime, 'runtime.json')));
    names = string({inventory.files.relative});
    asserts.isfalse(any(contains(lower(names), 'nlscompiler')));
    asserts.isfalse(any(contains(names, 'nelsonc.exe')));
  end
  rmfile(entry);
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', fullfile(work, 'missing-runtime'));
  for k = 1:numel(outputs)
    [status, text] = system(['"', outputs{k}, '"'], 60);
    asserts.isequal(status, 0, text);
    asserts.istrue(contains(text, 'COMPILER_FREE_RUNTIME_OK'), text);
  end
  clear restore;
  clear cleanup;
end
