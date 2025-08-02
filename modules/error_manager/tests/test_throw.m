%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/error_manager/tests']);
ME = fun_MException_1();
assert_isequal(fieldnames(ME), {'identifier'; 'message'; 'cause'; 'stack'; 'Correction'})
assert_isequal(ME.message, 'Input must be char.');
assert_isequal(ME.identifier, 'sayHello:inputError');
assert_isequal(ME.cause, {})
assert_isequal(size(ME.stack), [0 1])
assert_isequal(class(ME.stack), 'struct')
assert_isequal(fieldnames(ME.stack), {'file'; 'name'; 'line'})
assert_isequal(ME.Correction, {})
%=============================================================================
ME = fun_MException_2();
assert_isequal(fieldnames(ME), {'identifier'; 'message'; 'cause'; 'stack'; 'Correction'})
assert_isequal(ME.message, 'Input must be char.');
assert_isequal(ME.identifier, 'sayHello:inputError');
assert_isequal(ME.cause, {})
assert_isequal(class(ME.stack), 'struct')
n = numel(ME.stack);
if (n == 4 || n == 6 || n == 9)
  assert_isequal(ME.stack(1).name, 'test3')
  assert_isequal(ME.stack(1).line, 28)
  assert_isequal(ME.stack(2).name, 'test2')
  assert_isequal(ME.stack(2).line, 23)
  assert_isequal(ME.stack(3).name, 'test1')
  assert_isequal(ME.stack(3).line, 19)
  assert_isequal(ME.stack(4).name, 'fun_MException_2')
  assert_isequal(ME.stack(4).line, 12)
else
  error('wrong ME.stack size.')
end
assert_isequal(ME.Correction, {})
%=============================================================================
