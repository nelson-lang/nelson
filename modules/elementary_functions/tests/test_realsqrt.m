%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('realsqrt'), 1);
asserts.isequal(nargout('realsqrt'), 1);
%=============================================================================
asserts.isequal(realsqrt(4), 2);
asserts.isequal(realsqrt([0 1 4 9]), [0 1 2 3]);
asserts.checkerror('realsqrt(-1)', sprintf(_('%s must be real.'), 'Result'), 'Nelson:validators:mustBeReal');
%=============================================================================
