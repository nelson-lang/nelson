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
ME = fun_MException_3(true);
asserts.isequal(ME.message, 'Input must be char.');
asserts.isequal(ME.identifier, 'sayHello:inputError');
asserts.isequal(ME.cause, {})
asserts.isequal(class(ME.stack), 'struct')
n = numel(ME.stack);
asserts.istrue(n >= 1);
asserts.isequal(ME.stack(1).name, 'fun_MException_3')
asserts.isequal(ME.stack(1).line, 12)
asserts.isequal(ME.Correction, [])
%=============================================================================
