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
clear bytecode_persistent_after_global_same_scope
clear global bytecodeSharedDeclarationName
asserts.isfalse(isvar('global', 'bytecodeSharedDeclarationName'));
%=============================================================================
global bytecodeSharedDeclarationName
bytecodeSharedDeclarationName = 100;
msg = _('A variable cannot be both global and persistent.');
asserts.checkerror('bytecode_persistent_after_global_same_scope()', msg);
asserts.isequal(bytecodeSharedDeclarationName, 5);
%=============================================================================
clear bytecode_persistent_after_global_same_scope
clear global bytecodeSharedDeclarationName
%=============================================================================
