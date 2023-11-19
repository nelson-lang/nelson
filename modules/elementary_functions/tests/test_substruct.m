%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = substruct ('()', {1, 2, 3}, '{}', {':'}, '.', 'field');
assert_isequal(size(R), [1 3]);
assert_istrue(isstruct(R));
REF_1 = struct([]);
REF_1.type = '()';
REF_1.subs = {1 2 3};
REF_2 = struct([]);
REF_2.type = '{}';
REF_2.subs = {':'};
REF_3 = struct([]);
REF_3.type = '.';
REF_3.subs = 'field';
assert_isequal(R(1), REF_1)
assert_isequal(R(2), REF_2)
assert_isequal(R(3), REF_3)
%=============================================================================
