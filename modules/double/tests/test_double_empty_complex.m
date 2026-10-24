%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% converting an empty complex array gives an empty real array
asserts.istrue(isreal(double(complex([]))));
asserts.istrue(isreal(double(complex(single([])))));
asserts.isequal(class(double(complex(single([])))), 'double');
asserts.istrue(isreal(double(complex(zeros(0, 3)))));
asserts.isequal(size(double(complex(zeros(0, 3)))), [0 3]);
asserts.istrue(isreal(double(sparse(complex([])))));
asserts.istrue(issparse(double(sparse(complex([])))));
asserts.isfalse(isreal(double(complex(1))));
%=============================================================================
