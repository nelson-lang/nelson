%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'a'}, {'a'}, {'a'}, 'Protected', true);
B = categorical({'a'});
asserts.istrue(isprotected(A));
asserts.isfalse(isprotected(B));
asserts.isfalse(isprotected('a'));
%=============================================================================
