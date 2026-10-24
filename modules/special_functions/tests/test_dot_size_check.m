%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% vectors of different lengths must error
asserts.checkerror('dot([1 2 3], [1 2])', _('A and B must be same size.'));
asserts.checkerror('dot([1 2], [1 2 3])', _('A and B must be same size.'));
% valid cases unchanged
asserts.isequal(dot([1 2 3], [4 5 6]), 32);
asserts.isequal(dot([1 2 3], [4; 5; 6]), 32);
asserts.isequal(dot([1 + 2i 3], [2 4i]), 2 - 4i + 12i);
asserts.isequal(dot(magic(3), magic(3)), [89 107 89]);
%=============================================================================
