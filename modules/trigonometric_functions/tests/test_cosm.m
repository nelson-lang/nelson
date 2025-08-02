%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cosm'), 1);
assert_isequal(nargout('cosm'), 1);
%=============================================================================
assert_isequal(cosm(NaN), NaN);
assert_isequal(cosm(-NaN), NaN);
assert_isequal(cosm(Inf), NaN);
assert_isequal(cosm(-Inf), NaN);
%=============================================================================
x = cosm(0i);
ref = 1;
assert_isequal(x, ref);
assert_isequal(cos(0), 1);
assert_isequal(cos(-0), 1);
%=============================================================================
X = cosm(zeros(3, 3));
REF = eye(3, 3);
assert_isequal(X, REF);
%=============================================================================
assert_isequal(cosm([]), []);
%=============================================================================
A = [1,2;3,4];
R = cosm(A);
REF = [0.8554    -0.1109;
-0.1663     0.6891];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = single([1,2;3,4]);
R = cosm(A);
REF = [0.8554    -0.1109;
-0.1663     0.6891];
assert_isapprox(R, REF, 1e-4);
assert_isequal(class(R), 'single');
%=============================================================================
A = [1,2;3,4] + i;
R = cosm(A);
REF = [1.0181+0.7790i, 0.2173+1.0167i;
0.5076+1.3799i, 1.8889+1.8687i];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = single([1,2;3,4] + i);
R = cosm(A);
REF = [1.0181+0.7790i, 0.2173+1.0167i;
0.5076+1.3799i, 1.8889+1.8687i];
assert_isapprox(R, REF, 1e-4);
assert_isequal(class(R), 'single');
%=============================================================================
assert_checkerror('cosm([1 , 2])', _('Square matrix expected.'));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'cosm');
assert_checkerror('cosm(''a'')', msg);
%=============================================================================
