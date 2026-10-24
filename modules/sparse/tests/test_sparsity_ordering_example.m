%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
sparsityOrderingFigureVisible = 'off';
run([modulepath('sparse', 'root'), '/examples/sparsity_ordering.m']);

assert(issparse(shuffledMatrix) && issparse(reorderedMatrix));
assert(nnz(shuffledMatrix) == nnz(reorderedMatrix));
assert(reorderedBandwidth < shuffledBandwidth);
assert(isgraphics(figureHandle));
close(figureHandle);
%=============================================================================
