%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% reductions of an empty complex array give a real array
z = complex([]);
asserts.istrue(isreal(cumsum(z)));
asserts.istrue(isreal(cumprod(z)));
asserts.istrue(isreal(max(z)));
asserts.istrue(isreal(min(z)));
R = min(complex(zeros(0, 3)));
asserts.istrue(isreal(R));
asserts.isequal(size(R), [0 3]);
[M, I] = max(complex(zeros(0, 3)));
asserts.istrue(isreal(M));
asserts.isequal(size(I), [0 3]);
asserts.istrue(isreal(max(z, [], 'all')));
asserts.istrue(isreal(min(z, [], 'all')));
asserts.istrue(isreal(sum(complex(zeros(0, 3)))));
%=============================================================================
