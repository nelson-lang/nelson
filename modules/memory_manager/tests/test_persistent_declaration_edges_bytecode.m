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
clear bytecode_persistent_alpha bytecode_persistent_beta
assert_isequal(bytecode_persistent_alpha(), 1);
assert_isequal(bytecode_persistent_alpha(), 2);
assert_isequal(bytecode_persistent_beta(), 10);
assert_isequal(bytecode_persistent_alpha(), 3);
assert_isequal(bytecode_persistent_beta(), 20);
%=============================================================================
clear bytecode_persistent_alpha
assert_isequal(bytecode_persistent_alpha(), 1);
assert_isequal(bytecode_persistent_beta(), 30);
%=============================================================================
clear bytecode_persistent_beta
assert_isequal(bytecode_persistent_beta(), 10);
%=============================================================================
