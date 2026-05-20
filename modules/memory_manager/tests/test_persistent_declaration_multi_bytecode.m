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
clear bytecode_persistent_pair
assert_isequal(bytecode_persistent_pair(), [1 110]);
assert_isequal(bytecode_persistent_pair(), [2 120]);
assert_isequal(bytecode_persistent_pair(), [3 130]);
%=============================================================================
clear bytecode_persistent_pair
assert_isequal(bytecode_persistent_pair(), [1 110]);
%=============================================================================
