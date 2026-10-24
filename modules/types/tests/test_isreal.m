%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.isequal(nargin('isreal'), 1);
asserts.isequal(nargout('isreal'), 1);
%=============================================================================
R = complex(13);
asserts.isfalse(isreal(R));
%=============================================================================
R = complex([]);
asserts.isfalse(isreal(R));
%=============================================================================
asserts.istrue(isreal(ones(3, 0)));
%=============================================================================
R1 = 3 + 14i;
R2 = 5 - 14i;
R = R1 + R2;
asserts.isfalse(isreal(R1));
asserts.isfalse(isreal(R2));
asserts.istrue(isreal(R));
%=============================================================================
R1 = 3 - 14i;
R2 = 5 - 14i;
R = R2 - R1;
asserts.isfalse(isreal(R1));
asserts.isfalse(isreal(R2));
asserts.istrue(isreal(R));
%=============================================================================
R = isreal(1 + 0i);
asserts.istrue(R);
%=============================================================================
R = isreal([]);
asserts.istrue(R);
%=============================================================================
R = isreal(complex([]));
asserts.isfalse(R);
%=============================================================================
R = isreal(complex(ones(3, 0)));
asserts.isfalse(R);
%=============================================================================
R = isreal([1]);
asserts.istrue(R);
%=============================================================================
R = isreal([1, 2, 3]);
asserts.istrue(R);
%=============================================================================
R = isreal([1; 2; 3]);
asserts.istrue(R);
%=============================================================================
R = isreal([1, 2, 3; 4, 5, 6]);
asserts.istrue(R);
%=============================================================================
R = isreal([i]);
asserts.isfalse(R);
%=============================================================================
R = isreal([i, 2i, 3i]);
asserts.isfalse(R);
%=============================================================================
R = isreal([i; 2i; 3i]);
asserts.isfalse(R);
%=============================================================================
R = isreal([i, 2i, 3i; 4i, 5i, 6i]);
asserts.isfalse(R);
%=============================================================================
% an empty array stored as complex is not real
asserts.isfalse(isreal(complex(zeros(0, 1))));
asserts.isfalse(isreal(complex(single([]))));
asserts.isfalse(isreal(complex([], [])));
asserts.isfalse(isreal(complex(zeros(0, 0, 2))));
asserts.isfalse(isreal(sparse(complex([]))));
asserts.isfalse(isreal(ones(0, 0, 'like', 1i)));
asserts.isfalse(isreal(zeros(0, 0, 'like', 1i)));
asserts.isfalse(isreal([1i, []]));
asserts.isfalse(isreal([complex([]), []]));
%=============================================================================
% results of operations on empty arrays are real
asserts.istrue(isreal(zeros(0, 0) + 1i));
asserts.istrue(isreal(1i * []));
x = 1i;
x(1) = [];
asserts.istrue(isreal(x));
x = [1i, 2i];
asserts.istrue(isreal(x([])));
asserts.istrue(isreal(repmat(1i, 0, 0)));
%=============================================================================
