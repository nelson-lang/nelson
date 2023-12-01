%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/259
% <-- Short Description -->
% extraction decomplexify values
%=============================================================================
A = [1 0 0 0;
0 2 0 0;
0 0 3 0;
0 0 0 4];
CX = sparse (complex(A));
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(6)));
%=============================================================================
CX = complex(1);
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(1)));
%=============================================================================
CX = single(complex(1));
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(1)));
%=============================================================================
A = rand(3, 2, 3);
CX = complex(A);
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(1:3)));
assert_istrue(isreal(CX(1, 2, 2)));
%=============================================================================
A = rand(3, 2);
CX = complex(A);
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(2, 1)));
assert_istrue(isreal(CX(1:2, 1:2)));
%=============================================================================
