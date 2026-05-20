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
clear bytecode_persistent_counter
%=============================================================================
assert_isequal(bytecode_persistent_counter(), 1);
assert_isequal(bytecode_persistent_counter(), 2);
assert_isequal(bytecode_persistent_counter(), 3);
%=============================================================================
clear bytecode_persistent_counter
assert_isequal(bytecode_persistent_counter(), 1);
%=============================================================================
