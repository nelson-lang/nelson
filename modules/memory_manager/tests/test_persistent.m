%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('persistent'), -1);
asserts.isequal(nargout('persistent'), 0);
%=============================================================================
msg = _('A ''persistent'' declaration is only allowed in a script file function.');
asserts.checkerror('persistent nelsonPersistentBaseScope', msg);
%=============================================================================
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
clear bytecode_persistent_counter
asserts.isequal(bytecode_persistent_counter(), 1);
asserts.isequal(bytecode_persistent_counter(), 2);
clear bytecode_persistent_counter
asserts.isequal(bytecode_persistent_counter(), 1);
%=============================================================================
