%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(trace(true(3)), 3);
asserts.isequal(trace(['ab';'cd']), 197);
asserts.isequal(trace(int8(magic(3))), 15);
asserts.isequal(trace(uint8(200*ones(2))), 400);
asserts.isequal(trace(logical([1 1;1 1])), 2);
asserts.isequal(trace('a'), 97);
asserts.isequal(trace(true), 1);
asserts.isequal(trace(int8(5)), 5);
asserts.isequal(trace(uint8(5)), 5);
asserts.isequal(trace(sparse(true(2))), 2);
asserts.isequal(trace(int16(magic(3))), 15);
asserts.isequal(trace(uint64(5)*eye(2,'uint64')), 10);
%=============================================================================
