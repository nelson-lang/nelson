%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fieldnames'), 1);
asserts.isequal(nargout('fieldnames'), 1);
%=============================================================================
S.alpha = 1;
S.beta = 'Nelson';
R = fieldnames(S);
asserts.isequal(R, {'alpha'; 'beta'});
%=============================================================================
S = struct('b', 2, 'a', 1);
asserts.isequal(fieldnames(S), {'b'; 'a'});
asserts.isequal(size(fieldnames(struct([]))), [0 1]);
%=============================================================================
