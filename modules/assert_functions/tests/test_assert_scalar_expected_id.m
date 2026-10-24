%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
asserts.checkerror('assert_isfalse([false false])', 'A scalar expected.', 'Nelson:asserts:scalarExpected');
asserts.checkerror('assert_istrue([true true])', 'A scalar expected.', 'Nelson:asserts:scalarExpected');
%=============================================================================
