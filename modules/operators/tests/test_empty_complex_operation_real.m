%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% an operation giving an empty or all zeros result gives a real array
z = complex([]);
asserts.istrue(isreal(1i * []));
asserts.istrue(isreal([] * 1i));
asserts.istrue(isreal(z * z));
asserts.istrue(isreal(z * 2));
asserts.istrue(isreal(2 * z));
asserts.istrue(isreal(single(1i) * []));
asserts.isequal(class(single(1i) * []), 'single');
R = zeros(0, 3) * (1i * ones(3, 2));
asserts.istrue(isreal(R));
asserts.isequal(size(R), [0 2]);
R = zeros(2, 0) * complex(zeros(0, 2));
asserts.istrue(isreal(R));
asserts.isequal(R, zeros(2, 2));
%=============================================================================
asserts.istrue(isreal([] / 1i));
asserts.istrue(isreal(z / 2));
asserts.istrue(isreal(ones(0, 1) / 1i));
R = complex(zeros(2, 0)) / complex(zeros(3, 0));
asserts.istrue(isreal(R));
asserts.isequal(R, zeros(2, 3));
R = 2 \ z;
asserts.istrue(isreal(R));
asserts.isequal(size(R), [0 0]);
R = 1i \ [];
asserts.istrue(isreal(R));
asserts.isequal(size(R), [0 0]);
R = complex(zeros(2, 0)) \ complex(zeros(2, 3));
asserts.istrue(isreal(R));
asserts.isequal(R, zeros(0, 3));
%=============================================================================
asserts.istrue(isreal(-z));
asserts.istrue(isreal(+z));
asserts.istrue(isreal(-zeros(0, 1, 'like', 1i)));
asserts.isfalse(isreal(-complex(1)));
asserts.isfalse(isreal(z'));
asserts.isfalse(isreal(z.'));
%=============================================================================
% empty sparse complex arrays
S = sparse(z);
asserts.isfalse(isreal(S));
R = S * 2;
asserts.istrue(isreal(R));
asserts.istrue(issparse(R));
asserts.istrue(isreal(1i * sparse([])));
asserts.istrue(isreal(sparse([]) * 1i));
asserts.istrue(isreal(sparse(complex(zeros(0, 2))) * 2));
asserts.isequal(size(sparse(complex(zeros(0, 2))) * 2), [0 2]);
asserts.istrue(isreal(S * true));
asserts.istrue(isreal(z * sparse(2)));
asserts.isfalse(isreal(S * S));
asserts.istrue(isreal(-S));
asserts.istrue(isreal(S .* 2));
asserts.istrue(isreal(2 .* S));
asserts.istrue(isreal(S + 1));
asserts.istrue(isreal(S - S));
asserts.isfalse(isreal(S'));
%=============================================================================
