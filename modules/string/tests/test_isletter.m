%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isletter'), 1);
asserts.isequal(nargout('isletter'), 1);
%=============================================================================
A = 'Nelson';
R = isletter(A);
REF = logical(ones(1, 6));
asserts.isequal(R, REF);
%=============================================================================
A = "Nelson";
R = isletter(A);
REF = logical(ones(1, 6));
asserts.isequal(R, REF);
%=============================================================================
R = isletter("六書 six writings");
REF = logical([1 1 0 1 1 1 0 1 1 1 1 1 1 1 1]);
asserts.isequal(R, REF);
%=============================================================================

