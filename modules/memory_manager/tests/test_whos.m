%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('whos'), 1);
asserts.isequal(nargout('whos'), 1);
%=============================================================================
whosValue = ones(2, 3);
S = whos('whosValue');
asserts.istrue(isstruct(S));
asserts.isequal(S.name, 'whosValue');
asserts.isequal(S.size, [2 3]);
asserts.isequal(S.class, 'double');
asserts.isfalse(S.global);
asserts.isfalse(S.sparse);
asserts.isfalse(S.complex);
%=============================================================================
