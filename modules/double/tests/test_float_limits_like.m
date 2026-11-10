%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% realmax, realmin and flintmax take the class, complexity and sparsity of the 'like' value
R = realmax('like', single(1i));
assert_isequal(class(R), 'single');
assert_isfalse(isreal(R));
assert_isequal(real(R), realmax('single'));
R = realmax('like', sparse(1));
assert_istrue(issparse(R));
assert_isequal(full(R), realmax);
R = realmax('like', sparse(1i));
assert_istrue(issparse(R));
assert_isfalse(isreal(R));
R = realmin('like', 1i);
assert_isequal(class(R), 'double');
assert_isfalse(isreal(R));
assert_isequal(real(R), realmin);
assert_isequal(realmin('like', single(3)), realmin('single'));
R = flintmax('like', 1i);
assert_isfalse(isreal(R));
assert_isequal(real(R), 2 ^ 53);
R = flintmax('like', sparse(1));
assert_istrue(issparse(R));
assert_isequal(flintmax('LIKE', single(1)), flintmax('single'));
assert_isequal(realmax("single"), realmax('single'));
%=============================================================================
msg = _('Class name must be ''double'' or ''single''.');
assert_checkerror('realmax(''int8'')', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('realmax(1)', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('realmin(''like'')', msg, 'Nelson:constants:floatClassExpected');
assert_checkerror('flintmax(''foo'')', msg, 'Nelson:constants:floatClassExpected');
msg = _('With two inputs, the first one must be ''like''.');
assert_checkerror('realmax(''foo'', 1)', msg, 'Nelson:constants:firstArgNotLike');
assert_checkerror('flintmax(1, 1)', msg, 'Nelson:constants:firstArgNotLike');
msg = _('The value after ''like'' must be a double or single array.');
assert_checkerror('realmax(''like'', int8(1))', msg, 'Nelson:constants:floatPrototypeExpected');
assert_checkerror('realmin(''like'', {1})', msg, 'Nelson:constants:floatPrototypeExpected');
assert_checkerror('flintmax(''like'', true)', msg, 'Nelson:constants:floatPrototypeExpected');
%=============================================================================
