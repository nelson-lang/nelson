%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
package = fullfile(source, '+NcByteAccess', '+nested');
mkdir(package);
fixture = fullfile(nelsonroot(), 'modules', 'interpreter', 'tests', 'bytecode_classdef_access', '+NcByteAccess', '+nested', 'Item.m');
copyfile(fixture, fullfile(package, 'Item.m'));
entry = fullfile(source, 'access_entry.m');
fid = fopen(entry, 'wt');
closer = onCleanup(@() fclose(fid));
lines = {
  'function access_entry()'
  'object = NcByteAccess.nested.Item();'
  'if object.Value ~= 8 || NcByteAccess.nested.Item.visible() ~= 17 || NcByteAccess.nested.Item.reveal() ~= 999'
  'error(''Valid class access failed.'');'
  'end'
  'privateRejected = false;'
  'try; value = NcByteAccess.nested.Item.secret(); catch ex; privateRejected = strcmp(ex.identifier, ''Nelson:interpreter:undefinedMember''); end'
  'protectedRejected = false;'
  'try; value = NcByteAccess.nested.Item.guarded(); catch ex; protectedRejected = strcmp(ex.identifier, ''Nelson:interpreter:undefinedMember''); end'
  'if ~privateRejected || ~protectedRejected; error(''Restricted static method was exposed.''); end'
  'fprintf(''PACKAGED_CLASS_ACCESS_OK\n'');'
  'end'};
for k = 1:numel(lines)
  fprintf(fid, '%s\n', lines{k});
end
clear closer;
previous = getenv('NELSONC_RUNTIME_ROOT');
restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previous));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
applications = {};
for mode = {'bundled', 'installed'}
  output = fullfile(directory, mode{1});
  if ispc()
    output = [output, '.exe'];
  end
  ncc(entry, '-o', output, '--runtime', mode{1});
  applications{end + 1} = output;
end
rmdir(source, 's');
for k = 1:numel(applications)
  [status, output] = system(['"', applications{k}, '" 2>&1'], 60);
  asserts.isequal(status, 0);
  asserts.istrue(contains(output, 'PACKAGED_CLASS_ACCESS_OK'));
end
clear restore;
clear cleanup;
