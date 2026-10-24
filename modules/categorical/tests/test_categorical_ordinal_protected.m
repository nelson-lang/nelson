%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical([1 2 3], [1 2 3], {'low', 'mid', 'high'}, 'Ordinal', true);
asserts.istrue(isordinal(A));
asserts.istrue(isprotected(A));
asserts.isequal(A > 'mid', [false false true]);
%=============================================================================
R = false;
try
  categorical([1 2], [1 2], {'a', 'b'}, 'Ordinal', true, 'Protected', false);
catch ex
  R = true;
end
asserts.istrue(R);
%=============================================================================
B = categorical({'x'}, 'Protected', true);
R = false;
try
  B(2) = 'y';
catch ex
  R = true;
end
asserts.istrue(R);
%=============================================================================
C = categorical({'x'});
C(2) = 'y';
asserts.isequal(categories(C), {'x'; 'y'});
asserts.isequal(cellstr(C), {'x', 'y'});
%=============================================================================
