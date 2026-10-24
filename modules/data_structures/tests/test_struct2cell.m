%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('struct2cell'), 1);
asserts.isequal(nargout('struct2cell'), 1);
%=============================================================================
S.alpha = 1;
S.beta = 'Nelson';
R = struct2cell(S);
asserts.isequal(R, {1; 'Nelson'});
%=============================================================================
StructVector = struct('a', {1, 2}, 'b', {3, 4});
StructVectorCell = struct2cell(StructVector);
asserts.isequal(size(StructVectorCell), [2 1 2]);
asserts.isequal(StructVectorCell{1, 1, 2}, 2);
asserts.isequal(StructVectorCell{2, 1, 2}, 4);
asserts.isequal(struct2cell(struct('x', 7)), {7});
asserts.isequal(size(struct2cell(struct())), [0 1]);
asserts.isequal(size(struct2cell(struct([]))), [0 0 0]);
%=============================================================================
