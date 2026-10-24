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
previousPath = path();
restore = onCleanup(@() path(previousPath));
callers = fullfile(directory, 'callers');
packageRoot = fullfile(directory, 'packages');
package = fullfile(packageRoot, '+NcLookupPkg', '+nested');
guardPackage = fullfile(packageRoot, '+nc_lookup_guard_pkg', '+nested');
mkdir(callers);
mkdir(package);
mkdir(guardPackage);
leafSource = {
  'function value = leaf(x)'
  'value = 17;'
  'if nargin() > 0; value = value + x; end'
  'end'};
filewrite(fullfile(package, 'leaf.m'), leafSource);
filewrite(fullfile(guardPackage, 'leaf.m'), leafSource);
filewrite(fullfile(package, 'Item.m'), {
  'classdef Item'
  'properties'
  'Value'
  'end'
  'methods'
  'function obj = Item(); obj.Value = 8; end'
  'end'
  'end'});
filewrite(fullfile(callers, 'nc_lookup_guard.m'), {
  'function value = nc_lookup_guard(useVariable)'
  'if useVariable'
  'eval(''nc_lookup_guard_pkg.nested.leaf = @(x) x + 100;'');'
  'end'
  'value = nc_lookup_guard_pkg.nested.leaf(4);'
  'end'});
filewrite(fullfile(callers, 'nc_lookup_bare.m'), {
  'function [value, objectValue] = nc_lookup_bare()'
  'value = NcLookupPkg.nested.leaf;'
  'object = NcLookupPkg.nested.Item;'
  'objectValue = object.Value;'
  'end'});
for name = {'nc_lookup_guard', 'nc_lookup_bare'}
  source = fullfile(callers, [name{1}, '.m']);
  destination = fullfile(callers, [name{1}, '.nbc']);
  __bytecode_artifact__('compile', source, destination);
  delete(source);
end
guardInfo = __bytecode_artifact__('inspect', fullfile(callers, 'nc_lookup_guard.nbc'));
bareInfo = __bytecode_artifact__('inspect', fullfile(callers, 'nc_lookup_bare.nbc'));
asserts.istrue(contains(guardInfo, 'ENSURE_DOTTED_FUNCTION'));
asserts.istrue(contains(bareInfo, 'CALL_CLASSDEF_MEMBER'));
addpath(callers, '-frozen');
addpath(packageRoot, '-frozen');
for format = {'source', 'bytecode'}
  if strcmp(format{1}, 'bytecode')
    for folder = {package, guardPackage}
      source = fullfile(folder{1}, 'leaf.m');
      __bytecode_artifact__('compile', source, fullfile(folder{1}, 'leaf.nbc'));
      delete(source);
    end
    rehash();
    asserts.istrue(endsWith(which('NcLookupPkg.nested.leaf'), '.nbc'));
    asserts.istrue(endsWith(which('nc_lookup_guard_pkg.nested.leaf'), '.nbc'));
  end
  for k = 1:3
    asserts.isequal(nc_lookup_guard(false), 21);
    asserts.isequal(nc_lookup_guard(true), 104);
    [value, objectValue] = nc_lookup_bare();
    asserts.isequal(value, 17);
    asserts.isequal(objectValue, 8);
  end
end
rmpath(packageRoot);
asserts.isequal(nc_lookup_guard(true), 104);
assert_missing_package();
clear restore;
clear cleanup;

function assert_missing_package()
  try
    nc_lookup_guard(false);
  catch ex
    asserts.isequal(ex.identifier, 'Nelson:UndefinedVariable');
    return;
  end
  error('A removed package must not resolve through a cached function.');
end
