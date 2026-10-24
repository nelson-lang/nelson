%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('elementary_functions', 'root'), '/examples/classic_test_matrices.m']);
assert(isequal(size(hilbertMatrix), [5 5]));
assert(hilbertInverseError < 1e-9);
assert(all(magicRowSums == magicRowSums(1)));
%=============================================================================
