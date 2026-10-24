%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nextpow2(true), 0);
asserts.isequal(nextpow2('a'), 7);
asserts.isequal(nextpow2(sparse([1 0 3])), sparse([0 0 2]));
asserts.isequal(nextpow2(0.5), -1);
asserts.isequal(nextpow2([0 1 2 3 4 5 0.5 -3 Inf -Inf NaN]), [0 0 1 2 2 3 -1 2 Inf Inf NaN]);
asserts.isequal(nextpow2(single([0 1 2 3 4 5 0.5 -3 Inf -Inf NaN])), single([0 0 1 2 2 3 -1 2 Inf Inf NaN]));
asserts.isequal(nextpow2(false), 0);
asserts.isequal(nextpow2(sparse(0)), sparse(0));
asserts.isequal(nextpow2(sparse(NaN)), sparse(NaN));
asserts.isequal(nextpow2(sparse(true)), sparse(0));
%=============================================================================
