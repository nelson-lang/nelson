%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('iscolumn'), 1);
asserts.isequal(nargout('iscolumn'), 1);
%=============================================================================
asserts.istrue(iscolumn([1; 2]));
asserts.isfalse(iscolumn([1 2]));
asserts.isfalse(iscolumn(ones(2, 1, 2)));
%=============================================================================
