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
clear global bytecodeGlobalMultiA bytecodeGlobalMultiB
%=============================================================================
global bytecodeGlobalMultiA bytecodeGlobalMultiB
bytecodeGlobalMultiA = 10;
bytecodeGlobalMultiB = 20;
assert_isequal(bytecode_global_multi_reader(), [10 20]);
%=============================================================================
bytecode_global_multi_writer(30, 40);
assert_isequal(bytecodeGlobalMultiA, 30);
assert_isequal(bytecodeGlobalMultiB, 40);
assert_isequal(bytecode_global_multi_reader(), [30 40]);
%=============================================================================
clear global bytecodeGlobalMultiA bytecodeGlobalMultiB
%=============================================================================
