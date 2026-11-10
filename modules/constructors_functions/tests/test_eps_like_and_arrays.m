%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% eps of an array is computed element by element
R = eps([1 2 NaN 4 Inf 0]);
assert_isequal(R, [eps, 2 * eps, NaN, 4 * eps, NaN, 4.9406564584124654e-324]);
R = eps(single([1 2; 4 8]));
assert_isequal(class(R), 'single');
assert_isequal(R, eps(single(1)) * single([1 2; 4 8]));
assert_isequal(eps([-1 -0 realmin realmax]), [eps, 4.9406564584124654e-324, 4.9406564584124654e-324, 2 ^ 971]);
assert_isequal(size(eps(zeros(0, 3))), [0 3]);
%=============================================================================
% complex values use their modulus, sparse values give a full result
assert_isequal(eps([1+2i 3]), [2 * eps, 2 * eps]);
R = eps(single(1+1i));
assert_isequal(class(R), 'single');
assert_istrue(isreal(R));
R = eps(sparse([1 0 2]));
assert_isfalse(issparse(R));
assert_isequal(R, [eps, 4.9406564584124654e-324, 2 * eps]);
%=============================================================================
% class name and 'like' forms
assert_isequal(eps("single"), eps(single(1)));
R = eps('like', single(2i));
assert_isequal(class(R), 'single');
assert_isfalse(isreal(R));
assert_isequal(real(R), eps(single(1)));
R = eps('like', sparse(1));
assert_istrue(issparse(R));
assert_isequal(full(R), eps);
assert_isequal(eps('LIKE', 1), eps);
%=============================================================================
msg = _('Class name must be ''double'' or ''single''.');
assert_checkerror('eps(int8(1))', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('eps(true)', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('eps(''int8'')', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('eps(''Single'')', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('eps({1})', msg, 'Nelson:constants:floatClassExpected');
msg = _('With two inputs, the first one must be ''like''.');
assert_checkerror('eps(''single'', 1)', msg, 'Nelson:constants:firstArgNotLike');
assert_checkerror('eps(1, 2)', msg, 'Nelson:constants:firstArgNotLike');
msg = _('The value after ''like'' must be a double or single array.');
assert_checkerror('eps(''like'', ''a'')', msg, 'Nelson:constants:floatPrototypeExpected');
assert_checkerror('eps(''like'', int8(1))', msg, 'Nelson:constants:floatPrototypeExpected');
%=============================================================================
