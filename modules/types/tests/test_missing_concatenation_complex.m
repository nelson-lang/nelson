%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% a complex operand, even empty, keeps the complex attribute
R = [missing complex([])];
assert_isequal(class(R), 'double');
assert_isfalse(isreal(R));
assert_isequal(size(R), [1 1]);
assert_istrue(isnan(real(R)));
assert_isequal(imag(R), 0);
R = [complex([]) missing];
assert_isfalse(isreal(R));
assert_isequal(size(R), [1 1]);
R = [missing; complex([])];
assert_isfalse(isreal(R));
R = [complex([]); missing];
assert_isfalse(isreal(R));
R = horzcat(missing, complex(zeros(1, 0)));
assert_isfalse(isreal(R));
R = cat(1, missing, complex(zeros(0, 1)));
assert_isfalse(isreal(R));
R = cat(2, complex([]), missing);
assert_isfalse(isreal(R));
%=============================================================================
% single
R = [missing complex(single([]))];
assert_isequal(class(R), 'single');
assert_isfalse(isreal(R));
R = [complex(single([])); missing];
assert_isequal(class(R), 'single');
assert_isfalse(isreal(R));
R = [missing; missing; complex(single([]))];
assert_isequal(class(R), 'single');
assert_isfalse(isreal(R));
assert_isequal(size(R), [2 1]);
%=============================================================================
% every missing value becomes NaN + 0i
R = [missing missing complex([])];
assert_isfalse(isreal(R));
assert_isequal(size(R), [1 2]);
assert_istrue(all(isnan(real(R))));
assert_isequal(imag(R), [0 0]);
R = [complex(1, 2) missing missing];
assert_isfalse(isreal(R));
assert_isequal(real(R(1)), 1);
assert_isequal(imag(R), [2 0 0]);
assert_istrue(all(isnan(real(R(2:3)))));
R = [missing complex([]) 1];
assert_isfalse(isreal(R));
assert_istrue(isnan(real(R(1))));
assert_isequal(R(2), 1);
R = [missing sparse(complex([]))];
assert_istrue(issparse(R));
assert_isfalse(isreal(R));
%=============================================================================
% without missing, an empty complex double operand still makes a double
% result complex
assert_isfalse(isreal([complex([]) 5]));
assert_istrue(isreal([missing 1]));
%=============================================================================
