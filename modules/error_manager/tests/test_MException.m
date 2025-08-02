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
