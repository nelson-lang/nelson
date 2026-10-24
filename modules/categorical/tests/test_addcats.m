%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'red', 'blue'}, {'red', 'blue'});
B = addcats(A, 'green');
asserts.isequal(categories(B), {'red'; 'blue'; 'green'});
asserts.istrue(iscategory(B, 'green'));
%=============================================================================
