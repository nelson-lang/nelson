%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(dot(true,true), 1);
asserts.isequal(dot([true true],[true true]), 2);
asserts.isequal(dot([true true]',[true true]'), 2);
asserts.isequal(dot(true(2,2),true(2,2)), [2 2]);
asserts.isequal(dot(['ab';'cd'],['ab';'cd']), [19210 19604]);
asserts.isequal(dot(true(2,3),ones(2,3)), [2 2 2]);
%=============================================================================
