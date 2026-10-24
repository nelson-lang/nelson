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
clear bytecode_global_after_persistent_same_scope
clear global bytecodeSharedDeclarationNameReverse
%=============================================================================
msg = _('A variable cannot be both global and persistent.');
asserts.checkerror('bytecode_global_after_persistent_same_scope()', msg);
asserts.isfalse(isvar('global', 'bytecodeSharedDeclarationNameReverse'));
%=============================================================================
clear bytecode_global_after_persistent_same_scope
clear global bytecodeSharedDeclarationNameReverse
%=============================================================================
