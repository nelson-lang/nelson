%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('spalloc'), 3);
asserts.isequal(nargout('spalloc'), 1);
%=============================================================================
R = spalloc(3, 4, 5);
asserts.istrue(issparse(R));
asserts.isequal(size(R), [3 4]);
asserts.isequal(nnz(R), 0);
asserts.isequal(nzmax(R), 5);
%=============================================================================
