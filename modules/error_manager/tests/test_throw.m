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
asserts.isequal(fieldnames(ME), {'identifier'; 'message'; 'cause'; 'stack'; 'Correction'})
asserts.isequal(ME.message, 'Input must be char.');
asserts.isequal(ME.identifier, 'sayHello:inputError');
asserts.isequal(ME.cause, {})
asserts.isequal(size(ME.stack), [0 1])
asserts.isequal(class(ME.stack), 'struct')
asserts.isequal(fieldnames(ME.stack), {'file'; 'name'; 'line'})
asserts.isequal(ME.Correction, [])
%=============================================================================
ME = fun_MException_2();
asserts.isequal(fieldnames(ME), {'identifier'; 'message'; 'cause'; 'stack'; 'Correction'})
asserts.isequal(ME.message, 'Input must be char.');
asserts.isequal(ME.identifier, 'sayHello:inputError');
asserts.isequal(ME.cause, {})
asserts.isequal(class(ME.stack), 'struct')
n = numel(ME.stack);
asserts.istrue(n >= 4);
asserts.isequal(ME.stack(1).name, 'test3')
asserts.isequal(ME.stack(1).line, 28)
asserts.istrue(any(strcmp({ME.stack.name}, 'test2')) || all(strcmp({ME.stack.name}, 'test3')))
asserts.istrue(any(strcmp({ME.stack.name}, 'test1')) || all(strcmp({ME.stack.name}, 'test3')))
asserts.istrue(any(strcmp({ME.stack.name}, 'fun_MException_2')) || all(strcmp({ME.stack.name}, 'test3')))
asserts.isequal(ME.Correction, [])
%=============================================================================
