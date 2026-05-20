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
clear global bytecodeGlobalShadow
global bytecodeGlobalShadow
bytecodeGlobalShadow = 42;
%=============================================================================
assert_isequal(bytecode_global_shadow_local(), 7);
assert_isequal(bytecodeGlobalShadow, 42);
%=============================================================================
clear global bytecodeGlobalShadow
%=============================================================================
