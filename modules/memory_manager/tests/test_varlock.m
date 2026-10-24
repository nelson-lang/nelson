%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = 33;
varlock('local', 'A');
asserts.isequal(A, 33);
asserts.checkerror('A = 44;', _('Redefining permanent variable.'))
varunlock('local', 'A');
A = 44;
asserts.isequal(A, 44);
%=============================================================================
B = [];
B(2, 3) = 2;
varlock('local', 'B');
asserts.checkerror('B(2, 3) = 44;', _('Redefining permanent variable.'))
%=============================================================================
clear global lockedGlobalValue
global lockedGlobalValue
lockedGlobalValue = 10;
varlock('global', 'lockedGlobalValue');
asserts.istrue(varislock('global', 'lockedGlobalValue'));
asserts.checkerror('bytecode_global_locked_writer(11);', _('Redefining permanent variable.'))
varunlock('global', 'lockedGlobalValue');
asserts.isfalse(varislock('global', 'lockedGlobalValue'));
lockedGlobalValue = 12;
asserts.isequal(lockedGlobalValue, 12);
clear global lockedGlobalValue
%=============================================================================
clear global lockedGlobalVectorValue
global lockedGlobalVectorValue
lockedGlobalVectorValue = [1 2 3];
varlock('global', 'lockedGlobalVectorValue');
asserts.checkerror('bytecode_global_locked_index_writer(99);', _('Redefining permanent variable.'))
asserts.isequal(lockedGlobalVectorValue, [1 2 3]);
varunlock('global', 'lockedGlobalVectorValue');
bytecode_global_locked_index_writer(99);
asserts.isequal(lockedGlobalVectorValue, [1 99 3]);
clear global lockedGlobalVectorValue
%=============================================================================
