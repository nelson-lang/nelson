%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('asin'), 1);
assert_isequal(nargout('asin'), 1);
%=============================================================================
assert_isequal(asin(NaN), NaN);
assert_isequal(asin(-NaN), NaN);
if ismac() || ispc()
  % Visual Studio 2019 16.8.1 required
  assert_isequal(asin(Inf), complex(pi * 0.5, Inf));
else
  if ~ispc()
    assert_isequal(real(asin(Inf)), real(pi * 0.5));
  end
end
assert_isequal(asin(-Inf), complex(-pi * 0.5, Inf));
%=============================================================================
A = rand(100, 100);
if ~ismac()
  assert_isapprox(asin(-A), -asin(A));
end
%=============================================================================
A = rand(100, 100);
if ~ismac()
  assert_isapprox(asin(A), acos(-A) - pi * 0.5, 1e-4);
end
%=============================================================================
v = [1.   0.8660254   0.7071068   0.5   0.   0.  -0.5  -0.7071068  -0.8660254  -1];
r = asin(v);
ref = [1.5708    1.0472    0.7854    0.5236         0         0   -0.5236   -0.7854   -1.0472   -1.5708];
assert_isapprox(real(r), ref, 1e-4);
%=============================================================================
x = asin(0i);
ref = 0;
assert_isequal(x, ref);
%=============================================================================
X = asin(zeros(3, 3, 3));
REF = zeros(3, 3, 3);
assert_isequal(X, REF);
%=============================================================================
S = sparse(zeros(3, 3));
X = asin(S);
REF = sparse(3, 3);
assert_isequal(X, REF);
%=============================================================================
assert_isequal(asin([]), []);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'asin');
assert_checkerror('asin(''a'')', msg);
%=============================================================================
