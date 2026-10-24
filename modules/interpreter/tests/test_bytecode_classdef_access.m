%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if iswasm()
  skip_testsuite('bytecode artifact engine is not available in the portable WebAssembly runtime.');
end
%=============================================================================
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
callers = fullfile(directory, 'callers');
classes = fullfile(directory, 'classes');
mkdir(callers);
package = fullfile(classes, '+NcByteAccess', '+nested');
mkdir(package);
fixture = fullfile(modulepath('interpreter', 'tests'), 'bytecode_classdef_access', '+NcByteAccess', '+nested', 'Item.m');
copyfile(fixture, fullfile(package, 'Item.m'));
names = {'nc_byte_private', 'nc_byte_protected', 'nc_byte_public', 'nc_byte_internal'};
members = {'secret', 'guarded', 'visible', 'reveal'};
for k = 1:numel(names)
  write_lines(fullfile(callers, [names{k}, '.m']), {
    ['function value = ', names{k}, '()']
    ['value = NcByteAccess.nested.Item.', members{k}, '();']
    'end'});
end
write_lines(fullfile(callers, 'nc_byte_discarded.m'), {
  'function nc_byte_discarded()'
  'NcByteAccess.nested.Item.secret();'
  'end'});
names{end + 1} = 'nc_byte_discarded';
% The classes are deliberately absent from the path during bytecode compilation.
for k = 1:numel(names)
  source = fullfile(callers, [names{k}, '.m']);
  __bytecode_artifact__('compile', source, fullfile(callers, [names{k}, '.nbc']));
  delete(source);
end
addpath(callers);
addpath(classes);
restore = onCleanup(@() restore_paths(callers, classes));
assert_restricted(@() nc_byte_private());
assert_restricted(@() nc_byte_protected());
nc_byte_object = NcByteAccess.nested.Item();
asserts.isequal(nc_byte_object.Value, 8);
for k = 1:3
  assert_restricted(@() nc_byte_private());
  assert_restricted(@() nc_byte_protected());
  identifier = '';
  try
    nc_byte_discarded();
  catch ex
    identifier = ex.identifier;
  end
  asserts.istrue(~isempty(identifier));
  asserts.isequal(nc_byte_public(), 17);
  asserts.isequal(nc_byte_internal(), 999);
end
clear nc_byte_object;
clear restore;
clear cleanup;

function assert_restricted(callback)
  identifier = '';
  try
    value = callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.istrue(any(strcmp(identifier, {'Nelson:interpreter:undefinedMember', 'Nelson:UndefinedFunction'})));
end

function restore_paths(callers, classes)
  rmpath(callers);
  rmpath(classes);
end

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  closer = onCleanup(@() fclose(fid));
  for k = 1:numel(lines)
    fprintf(fid, '%s\n', lines{k});
  end
  clear closer;
end
