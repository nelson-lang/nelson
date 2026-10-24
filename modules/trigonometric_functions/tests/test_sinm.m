%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('sinm'), 1);
asserts.isequal(nargout('sinm'), 1);
%=============================================================================
asserts.isequal(sinm(NaN), NaN);
asserts.isequal(sinm(-NaN), NaN);
asserts.isequal(sinm(Inf), NaN);
asserts.isequal(sinm(-Inf), NaN);
%=============================================================================
x = sinm(0i);
ref = 0;
asserts.isequal(x, ref);
%=============================================================================
X = sinm(zeros(3, 3));
REF = zeros(3, 3);
asserts.isequal(X, REF);
%=============================================================================
asserts.isequal(sinm([]), []);
%=============================================================================
A = [1, 2; 3, 4];
R = sinm(A);
REF = [-0.4656 -0.1484
  -0.2226 -0.6882];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
A = single([1, 2; 3, 4]);
R = sinm(A);
REF = [-0.4656 -0.1484
  -0.2226 -0.6882];
asserts.isapprox(R, REF, 1e-4);
asserts.isequal(class(R), 'single');
%=============================================================================
A = [1, 2; 3, 4] + i;
R = sinm(A);
REF = [-1.0832 + 0.3850i, -1.0001 + 0.5069i;
  -1.2988 + 0.9096i -1.9793 + 1.5933i];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
A = single([1, 2; 3, 4] + i);
R = sinm(A);
REF = [-1.0832 + 0.3850i, -1.0001 + 0.5069i;
  -1.2988 + 0.9096i -1.9793 + 1.5933i];
asserts.isapprox(R, REF, 1e-4);
asserts.isequal(class(R), 'single');
%=============================================================================
asserts.checkerror('sinm([1 , 2])', _('Square matrix expected.'));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sinm');
asserts.checkerror('sinm(''a'')', msg);
%=============================================================================
