%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
declarationConflict = sprintf(['function y = vm_semantic_declaration_conflict()\n', ...
  '  global vmSemanticDeclarationConflictName\n', ...
  '  persistent vmSemanticDeclarationConflictName\n', ...
  '  y = 1;\n', ...
  'end']);
assert_isequal(parsestring(declarationConflict), 'function');
path_test = [tempdir(), createGUID()];
mkdir(path_test);
conflict_file = [path_test, '/vm_semantic_declaration_conflict.m'];
fd = fopen(conflict_file, 'wt');
fprintf(fd, '%s', declarationConflict);
fclose(fd);
addpath(path_test);
msg = _('A variable cannot be both global and persistent.');
assert_checkerror('vm_semantic_declaration_conflict()', msg);
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
assert_isequal(struct('Field', 42).Field, 42);
assert_isequal(ones(1, 3)(2), 1);
assert_isequal(num2cell(99){1}, 99);
fieldname = 'Field';
assert_isequal(struct('Field', 123).(fieldname), 123);
%=============================================================================
