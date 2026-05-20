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
clear bytecode_persistent_builtin_after_global_same_scope
clear bytecode_global_builtin_after_persistent_same_scope
clear global bytecodeSharedBuiltinDeclarationName bytecodeSharedBuiltinDeclarationNameReverse
%=============================================================================
msg = _('A variable cannot be both global and persistent.');
assert_checkerror('bytecode_persistent_builtin_after_global_same_scope()', msg);
global bytecodeSharedBuiltinDeclarationName
assert_isequal(bytecodeSharedBuiltinDeclarationName, 5);
assert_checkerror('bytecode_global_builtin_after_persistent_same_scope()', msg);
assert_isfalse(isvar('global', 'bytecodeSharedBuiltinDeclarationNameReverse'));
%=============================================================================
clear bytecode_persistent_builtin_after_global_same_scope
clear bytecode_global_builtin_after_persistent_same_scope
clear global bytecodeSharedBuiltinDeclarationName bytecodeSharedBuiltinDeclarationNameReverse
%=============================================================================
