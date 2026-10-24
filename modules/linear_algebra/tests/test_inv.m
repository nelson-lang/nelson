%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('inv'), 1);
asserts.isequal(nargout('inv'), 1);
%=============================================================================
asserts.isequal(inv([]), []);
asserts.isequal(inv(NaN), NaN);
asserts.isequal(inv(Inf), 0);
%=============================================================================
asserts.isapprox(inv([1, 2; 3, 4]), [-2, 1; 1.5, -0.5], 1e-15);
asserts.isapprox(inv(eye(3, 3)), eye(3, 3), 1e-15);
asserts.isapprox(inv(single([1, 2; 3, 4])), single([-2, 1; 1.5, -0.5]), 1e-6);
%=============================================================================
asserts.isapprox(inv([i, 2; i, 4]), [-2i i; -0.5, 0.5], 1e-15);
%=============================================================================
asserts.isequal(inv([0 0; 2 3]), [Inf, Inf; Inf, Inf]);
asserts.isequal(inv([2 NaN; 2 3]), [NaN, NaN; NaN, NaN])
if ispc() && ~strcmp(computer('arch'), 'woa64')
  asserts.isequal(inv([2 Inf; 2 3]), [NaN, NaN; 0 0])
  asserts.isapprox(inv([0 Inf; 2 3]), [0 0.5; 0 0], 1e-15);
end
%=============================================================================
asserts.isequal(inv([0 0; i() 3]), [Inf, Inf; Inf, Inf]);
asserts.isapprox(inv([i Inf; 2 3]), [0, .5; 0, 0], 1e-4);
asserts.isequal(inv([2 NaN; 2i 3]), complex([NaN, NaN; NaN, NaN], [NaN, NaN; NaN, NaN]))
asserts.isapprox(inv([i Inf; 2 3]), [0, .5; 0, 0], 1e-4);
%=============================================================================
A = [4, 2, 2; 4, 6, 8; -2, 2, 4];
REF = [1.0000 -0.5000 0.5000;
  -4.0000 2.5000 -3.0000;
  2.5000 -1.5000 2.0000];
asserts.isapprox(inv(A), REF, 1e-15);
%=============================================================================
asserts.isequal(inv(single([])), single([]));
asserts.isequal(inv(single(NaN)), single(NaN));
asserts.isequal(inv(single(Inf)), single(0));
%=============================================================================
asserts.isapprox(inv(single([1, 2; 3, 4])), single([-2, 1; 1.5, -0.5]), 1e-4);
asserts.isapprox(inv(single(eye(3, 3))), single(eye(3, 3)), 1e-15);
asserts.isapprox(inv(single([1, 2; 3, 4])), single([-2, 1; 1.5, -0.5]), 1e-6);
%=============================================================================
asserts.isapprox(inv(single([i, 2; i, 4])), single([-2i i; -0.5, 0.5]), 1e-15);
%=============================================================================
asserts.isequal(inv(single([0 0; 2 3])), single([Inf, Inf; Inf, Inf]));
if ispc() && ~strcmp(computer('arch'), 'woa64')
  asserts.isequal(inv(single([2 NaN; 2 3])), single([NaN, NaN; NaN, NaN]))
  asserts.isequal(inv(single([2 Inf; 2 3])), single([NaN, NaN; 0 0]))
  asserts.isapprox(inv(single([0 Inf; 2 3])), single([0 0.5; 0 0]), 1e-15);
end
%=============================================================================
asserts.isequal(inv(single([0 0; i() 3])), single([Inf, Inf; Inf, Inf]));
asserts.isapprox(inv(single([i Inf; 2 3])), [0 0.5; 0, 0], 1e-4);
asserts.isequal(inv(single([2 NaN; 2i 3])), complex(single([NaN, NaN; NaN, NaN]), single([NaN, NaN; NaN, NaN])))
%=============================================================================
A = single([4, 2, 2; 4, 6, 8; -2, 2, 4]);
REF = [1.0000 -0.5000 0.5000;
  -4.0000 2.5000 -3.0000;
  2.5000 -1.5000 2.0000];
REF = single(REF);
asserts.isapprox(inv(A), REF, 1e-15);
%=============================================================================
A = diag([1 2 3 4 5]);
R = inv(A);
REF = diag(1 ./ [1 2 3 4 5]);
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('inv(zeros(3,5))', sprintf(_('%s expected.'), 'Square matrix'));
%=============================================================================
asserts.isequal(inv([0 0; i() 3]), [Inf, Inf; Inf, Inf]);
[msg, id] = lastwarn();
asserts.isequal(msg, _('Matrix is singular to working precision.'));
asserts.isequal(id, 'Nelson:singularMatrix');
%=============================================================================
B = [1 2; 3 4];
asserts.isapprox(inv(B), [-2 1; 1.5 -0.5], 1e-12);
%=============================================================================
