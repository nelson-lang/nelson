%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--TIMEOUT 600-->
% The FMU cache under tempdir() is shared by every Nelson process on the
% machine and is keyed by a hash of the model, so processes compiling the same
% model target the same entry. Building inside that entry made concurrent
% builds delete or overwrite each other ("the process cannot access the file
% because it is being used by another process"), and only on a COLD entry --
% hence the unique model name below, which no earlier run can have cached.
%=============================================================================
if ~modelicaInfo().capable
  return
end
%=============================================================================
% A model no previous run can have compiled: the cache entry is cold for sure.
modelName = ['MTFC', strrep(createGUID(), '-', '')];
src = ['model ', modelName, ' Real x(start=1); equation der(x) = -x; end ', modelName, ';'];
%=============================================================================
% Three processes compile it at the same time.
worker = [tempname(), '.m'];
filewrite(worker, {...
  ['modelName = ''', modelName, ''';'], ...
  ['src = ''', src, ''';'], ...
  'try', ...
  '  disp([''RESULT:'', modelicaToFmu(src, modelName)]);', ...
  'catch e', ...
  '  disp([''FAILED:'', e.identifier, '' '', e.message]);', ...
  'end'});
nelson_cli = [modulepath('nelson', 'bin'), '/nelson-cli'];
command = ['"', nelson_cli, '" --quiet --no-user-startup --no-ipc -f "', worker, '"'];
[status, output] = system({command, command, command}, 240);
rmfile(worker);
%=============================================================================
marker = 'RESULT:';
paths = cell(1, 3);
for k = 1:3
  assert_isequal(status(k), 0);
  lines = strsplit(char(output(k)), newline());
  found = '';
  for n = 1:numel(lines)
    one = strtrim(lines{n});
    if startsWith(one, marker)
      found = one((numel(marker) + 1):end);
    end
  end
  assert_istrue(~isempty(found)); % no build lost the race
  paths{k} = found;
end
% All three agree on one shared cache entry, and it holds a usable FMU.
assert_isequal(paths{2}, paths{1});
assert_isequal(paths{3}, paths{1});
fmu = paths{1};
assert_istrue(isfile(fmu));
assert_istrue(endsWith(fmu, [modelName, '.fmu']));
fi = fmiInfo(fmu);
assert_isequal(fi.fmiVersion, '2.0');
%=============================================================================
% 'Rebuild' recompiles without disturbing an entry other processes may read.
entry = dir(fmu);
rebuilt = modelicaToFmu(src, modelName, 'Rebuild', true);
assert_istrue(isfile(fmu)); % entry still there
kept = dir(fmu);
assert_isequal(kept(1).bytes, entry(1).bytes); % and untouched
assert_isequal(kept(1).datenum, entry(1).datenum);
assert_istrue(isfile(rebuilt));
assert_istrue(~strcmp(rebuilt, fmu)); % kept private
fir = fmiInfo(rebuilt);
assert_isequal(fir.fmiVersion, '2.0');
rmdir(fileparts(rebuilt), 's');
%=============================================================================
rmdir(fileparts(fmu), 's'); % this key is unique to this run
%=============================================================================
