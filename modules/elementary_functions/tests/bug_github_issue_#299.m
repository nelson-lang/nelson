%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/299
% <-- Short Description -->
% extends complex to manage sparse matrix
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
a = sparse(eye(3,3));
b = complex(a);
assert_istrue(isreal(a));
assert_isfalse(isreal(b));
assert_istrue(isequal(a, b));
%=============================================================================
a = sparse(eye(3,3) + i);
b = complex(a);
assert_isfalse(isreal(a));
assert_isfalse(isreal(b));
assert_isequal(a, b);
%=============================================================================
a = sparse(logical(eye(3,3)));
assert_checkerror('b = complex(a);', 'Undefined function ''sparselogical_complex''');
%=============================================================================