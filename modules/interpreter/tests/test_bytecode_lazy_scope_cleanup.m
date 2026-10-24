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
package = fullfile(packageRoot, '+NcLazyScope', '+nested');
mkdir(callers);
mkdir(package);
filewrite(fullfile(package, 'leaf.m'), {
  'function value = leaf(x)'
  'value = 17;'
  'if nargin() > 0; value = value + x; end'
  'end'});
filewrite(fullfile(package, 'Item.m'), {
  'classdef Item'
  'properties'
  'Value'
  'end'
  'methods'
  'function obj = Item(); obj.Value = 8; end'
  'end'
  'end'});
source = fullfile(callers, 'nc_lazy_scope.m');
destination = fullfile(callers, 'nc_lazy_scope.nbc');
filewrite(source, {
  'function [value, objectValue] = nc_lazy_scope()'
  'value = NcLazyScope.nested.leaf;'
  'object = NcLazyScope.nested.Item;'
  'objectValue = object.Value;'
  'end'});
__bytecode_artifact__('compile', source, destination);
delete(source);
source = fullfile(callers, 'nc_lazy_scalar.m');
destination = fullfile(callers, 'nc_lazy_scalar.nbc');
filewrite(source, {
  'function value = nc_lazy_scalar(x)'
  'object = NcLazyScope.nested.Item;'
  'value = NcLazyScope.nested.leaf + object.Value + x;'
  'end'});
__bytecode_artifact__('compile', source, destination);
delete(source);
addpath(callers, '-frozen');
addpath(packageRoot, '-frozen');
check_caller_scope();
clear restore;
clear cleanup;

function check_caller_scope()
  callerSentinel = 123;
  varlock('local', 'callerSentinel');
  for repetition = 1:3
    scalarValue = nc_lazy_scalar(repetition);
    asserts.istrue(exist('callerSentinel', 'var') == 1);
    asserts.isequal(scalarValue, 25 + repetition);
    [value, objectValue] = nc_lazy_scope();
    asserts.istrue(exist('callerSentinel', 'var') == 1);
    asserts.isequal(callerSentinel, 123);
    asserts.isequal(value, 17);
    asserts.isequal(objectValue, 8);
  end
  varunlock('local', 'callerSentinel');
end
