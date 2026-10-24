%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% Property access resolution is memoized per class: every rule is checked
% several times so that a warm cache is exercised as well as a cold one.
o = ClassdefPropertyAccessBase();
c = ClassdefPropertyAccessChild();
for k = 1:3
  % GetAccess = public, SetAccess = protected
  asserts.isequal(o.Counter, 1);
  asserts.isequal(get(o, 'Counter'), 1);
  assert_error_id(@() set_property(o, 'Counter', 3), 'Nelson:interpreter:cannotSetProperty');
  assert_error_id(@() set(o, 'Counter', 3), 'Nelson:interpreter:cannotSetProperty');
  % Access = private
  assert_error_id(@() o.Secret, 'Nelson:interpreter:cannotGetProperty');
  assert_error_id(@() get(o, 'Secret'), 'Nelson:interpreter:cannotGetProperty');
  assert_error_id(@() set_property(o, 'Secret', 1), 'Nelson:interpreter:cannotSetProperty');
  % Access = protected
  assert_error_id(@() o.Guarded, 'Nelson:interpreter:cannotGetProperty');
  assert_error_id(@() set_property(o, 'Guarded', 1), 'Nelson:interpreter:cannotSetProperty');
  % SetAccess = immutable
  asserts.isequal(o.Id, 5);
  assert_error_id(@() set_property(o, 'Id', 1), 'Nelson:interpreter:cannotSetProperty');
  % unknown property
  assert_error_id(@() o.Nope, 'Nelson:interpreter:noSuchProperty');
  assert_error_id(@() set_property(o, 'Nope', 1), 'Nelson:interpreter:noSuchProperty');
  % subclass: protected members reachable, private ones are not
  asserts.isequal(c.readGuarded(), 7);
  assert_error_id(@() c.readSecretFromChild(), 'Nelson:interpreter:cannotGetProperty');
  assert_error_id(@() c.Guarded, 'Nelson:interpreter:cannotGetProperty');
  assert_error_id(@() c.Secret, 'Nelson:interpreter:cannotGetProperty');
end
%=============================================================================
% private and protected members from inside the class
asserts.isequal(o.readSecret(), 42);
o.writeSecret(9);
asserts.isequal(o.readSecret(), 9);
o.bump();
asserts.isequal(o.Counter, 2);
c.setCounter(8);
asserts.isequal(c.Counter, 8);
%=============================================================================
% Dependent property: getter and setter
asserts.isequal(o.Twice, 4);
o.Twice = 10;
asserts.isequal(o.Counter, 5);
asserts.isequal(o.Twice, 10);
asserts.isequal(c.Twice, 16);
%=============================================================================
% static methods called with an instance of the class as argument
asserts.isequal(ClassdefPropertyAccessBase.callHelper(o), 9);
asserts.isequal(ClassdefPropertyAccessBase.callHelper(c), 42);
assert_error_id(@() ClassdefPropertyAccessBase.helper(o), 'Nelson:interpreter:noSuchProperty');
asserts.isequal(ClassdefPropertyAccessBase.Limit, 10);
%=============================================================================
% Constant property read and written through a handle instance
asserts.isequal(o.Limit, 10);
asserts.isequal(c.Limit, 10);
asserts.isequal(o.('Limit'), 10);
assert_error_id(@() set_limit(o, 1), 'Nelson:interpreter:cannotModifyConstantProperty');
assert_error_id(@() set_property(o, 'Limit', 1), 'Nelson:interpreter:cannotModifyConstantProperty');
identifier = '';
try
  o.Limit = 1;
catch ex
  identifier = ex.identifier;
end
asserts.isequal(identifier, 'Nelson:interpreter:cannotModifyConstantProperty');
asserts.isequal(o.Limit, 10);
asserts.isequal(ClassdefPropertyAccessBase.Limit, 10);
% same identifier on a value class instance
p = ClassdefPoint(1, 2);
assert_error_id(@() set_dimension(p), 'Nelson:interpreter:cannotModifyConstantProperty');
assert_error_id(@() set_property(p, 'Dimension', 3), 'Nelson:interpreter:cannotModifyConstantProperty');
asserts.isequal(p.Dimension, 2);
%=============================================================================
% the memoized access rules follow a class redefinition
directory = tempname();
mkdir(directory);
write_access_classes(directory);
code = ['try, addpath(''', directory, '''); ', ...
        'h = ClassdefPropertyAccessRedefined(); ', ...
        'asserts.isequal(h.Value, 3); ', ...
        'clear h; ', ...
        'copyfile(''', fullfile(directory, 'private.txt'), ''', ''', fullfile(directory, 'ClassdefPropertyAccessRedefined.m'), ''', ''f''); ', ...
        'clear classes; ', ...
        'rehash; ', ...
        'h = ClassdefPropertyAccessRedefined(); ', ...
        'id = ''''; ', ...
        'try, v = h.Value; catch ex, id = ex.identifier; end; ', ...
        'asserts.isequal(id, ''Nelson:interpreter:cannotGetProperty''); ', ...
        'exit(0); catch, exit(1); end'];
[nelson_exe, execute_option] = nelson_cli_command_for_test();
status = system([nelson_exe, execute_option, '"', code, '"'], 60);
rmdir(directory, 's');
asserts.isequal(status, 0);
%=============================================================================
function set_property(obj, name, value)
  obj.(name) = value;
end
%=============================================================================
function set_dimension(obj)
  obj.Dimension = 3;
end
%=============================================================================
function set_limit(obj, value)
  obj.Limit = value;
end
%=============================================================================
function assert_error_id(callback, expected)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, expected);
end
%=============================================================================
function write_access_classes(directory)
  write_class(fullfile(directory, 'ClassdefPropertyAccessRedefined.m'), 'public');
  write_class(fullfile(directory, 'private.txt'), 'private');
end
%=============================================================================
function write_class(filename, access)
  fid = fopen(filename, 'wt');
  fprintf(fid, 'classdef ClassdefPropertyAccessRedefined < handle\n');
  fprintf(fid, '  properties (Access = %s)\n', access);
  fprintf(fid, '    Value = 3\n');
  fprintf(fid, '  end\n');
  fprintf(fid, 'end\n');
  fclose(fid);
end
%=============================================================================
