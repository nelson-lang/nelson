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
assert_isequal(A, 33);
assert_checkerror('A = 44;', _('Redefining permanent variable.'))
varunlock('local', 'A');
A = 44;
assert_isequal(A, 44);
%=============================================================================
B = [];
B(2, 3) = 2;
varlock('local', 'B');
assert_checkerror('B(2, 3) = 44;', _('Redefining permanent variable.'))
%=============================================================================
