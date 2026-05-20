%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
%=============================================================================
clear global bytecodeGlobalA
assert_isfalse(isvar('global', 'bytecodeGlobalA'));
%=============================================================================
global bytecodeGlobalA
bytecodeGlobalA = 41;
assert_istrue(isvar('global', 'bytecodeGlobalA'));
assert_isequal(bytecode_global_reader(), 41);
bytecode_global_writer(99);
assert_isequal(bytecodeGlobalA, 99);
assert_isequal(bytecode_global_reader(), 99);
%=============================================================================
bytecode_global_writer(123);
assert_isequal(bytecodeGlobalA, 123);
%=============================================================================
clear global bytecodeGlobalA
assert_isfalse(isvar('global', 'bytecodeGlobalA'));
%=============================================================================
