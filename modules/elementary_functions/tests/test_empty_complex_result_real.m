%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% functions giving an empty result from a complex array give a real array
z = complex([]);
asserts.istrue(isreal(conj(z)));
asserts.istrue(isreal(conj(zeros(0, 1, 'like', 1i))));
asserts.istrue(isreal(conj(sparse(z))));
asserts.istrue(isreal(round(z)));
asserts.istrue(isreal(ceil(z)));
asserts.istrue(isreal(floor(single(z))));
asserts.isequal(class(floor(single(z))), 'single');
asserts.istrue(isreal(fix(z)));
%=============================================================================
asserts.istrue(isreal(repmat(1i, 0, 0)));
asserts.istrue(isreal(repmat(1i, 0, 3)));
asserts.isequal(size(repmat(1i, 0, 3)), [0 3]);
asserts.istrue(isreal(repmat(1i, 2, 0)));
asserts.istrue(isreal(repmat(z, 2, 2)));
asserts.istrue(isreal(repmat([1i, 2i], 0, 1)));
asserts.isfalse(isreal(repmat(1i, 1, 2)));
%=============================================================================
asserts.istrue(isreal(fliplr(z)));
asserts.istrue(isreal(flipud(z)));
asserts.istrue(isreal(flip(z)));
asserts.istrue(isreal(flip(z, 2)));
asserts.istrue(isreal(rot90(z)));
asserts.istrue(isreal(circshift(z, 1)));
asserts.isequal(size(circshift(complex(zeros(0, 3)), 1)), [0 3]);
asserts.istrue(isreal(rot90(complex(zeros(0, 3)), 2)));
asserts.isequal(size(rot90(complex(zeros(0, 3)))), [3 0]);
asserts.istrue(isreal(tril(z)));
asserts.istrue(isreal(triu(z)));
%=============================================================================
% storage is kept by reshaping and concatenation
asserts.isfalse(isreal(reshape(z, 0, 1)));
asserts.isfalse(isreal(cat(1, z, [])));
asserts.isfalse(isreal(complex(ones(3, 0))));
%=============================================================================
% flip of a scalar or of an empty array
asserts.isequal(flip(5), 5);
asserts.istrue(flip(complex(1)) == 1);
asserts.isequal(size(flip([])), [0 0]);
asserts.isequal(flip([1 2 3]), [3 2 1]);
asserts.isequal(flip([1; 2; 3]), [3; 2; 1]);
%=============================================================================
