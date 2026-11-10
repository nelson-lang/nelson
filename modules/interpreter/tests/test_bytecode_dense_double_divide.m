%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A \ B and A / B on dense doubles in a loop give the operator results, its
% warnings and errors, and still reach a double overload on the path.
%=============================================================================
A = [4 1 0; 1 3 1; 0 1 2];
C = A + 1i * eye(3);
b = [1; 2; 3];
operands = {{A, b}, {A, [b, 2 * b]}, {C, b + 1i}, {[1 2; 3 4; 5 6], [1; 2; 3]}, ...
  {2, [2 4 6]}, {complex(1, 2), complex([2 4 6])}, {zeros(0, 3), zeros(0, 2)}, {triu(A), b}};
for k = 1:numel(operands)
  a = operands{k}{1};
  y = operands{k}{2};
  assert_isequal(loop_mldivide(a, y), feval('mldivide', a, y));
end
assert_isequal(loop_mrdivide(b', A), feval('mrdivide', b', A));
assert_isequal(loop_mrdivide([b'; 2 * b'], C), feval('mrdivide', [b'; 2 * b'], C));
assert_isequal(loop_mrdivide([2 4 6], 2), feval('mrdivide', [2 4 6], 2));
%=============================================================================
% mixed real and complex operands keep the generic promotion
assert_isapprox(loop_mldivide(complex(1, 2), [2, 4, 6]), [0.4 - 0.8i, 0.8 - 1.6i, 1.2 - 2.4i], 1e-14);
assert_isapprox(loop_mrdivide([2, 4, 6], complex(1, 2)), [0.4 - 0.8i, 0.8 - 1.6i, 1.2 - 2.4i], 1e-14);
assert_isequal(class(loop_mldivide(single(A), b)), 'single');
%=============================================================================
% warnings and errors are the operator ones
n = 60;
K = eye(n) - triu(ones(n), 1);
K(n, 1) = 1e-30;
lastwarn('', '');
x = loop_mldivide(K, ones(n, 1));
[msg, id] = lastwarn();
assert_isequal(id, 'Nelson:nearlySingularMatrix');
lastwarn('', '');
x = loop_mrdivide(ones(1, n), K);
[msg, id] = lastwarn();
assert_isequal(id, 'Nelson:nearlySingularMatrix');
assert_checkerror('loop_mldivide([1 2; 3 4], [1 2 3])', operator_error('mldivide', [1 2; 3 4], [1 2 3]));
assert_checkerror('loop_mrdivide([1 2 3], [1 2; 3 4])', operator_error('mrdivide', [1 2 3], [1 2; 3 4]));
%=============================================================================
% a double overload found on the path is called
d = fullfile(tempdir(), ['nelson_mldivide_', num2str(round(rand() * 1e9))]);
mkdir(fullfile(d, '@double'));
fid = fopen(fullfile(d, '@double', 'mldivide.m'), 'wt');
fprintf(fid, 'function r = mldivide(a, b)\n  r = 42;\nend\n');
fclose(fid);
fid = fopen(fullfile(d, '@double', 'mrdivide.m'), 'wt');
fprintf(fid, 'function r = mrdivide(a, b)\n  r = 43;\nend\n');
fclose(fid);
assert_isequal(loop_mldivide(A, b), A \ b);
addpath(d);
assert_isequal(loop_mldivide(A, b), 42);
assert_isequal(loop_mrdivide(b', A), 43);
rmpath(d);
clear functions
assert_isequal(loop_mldivide(A, b), feval('mldivide', A, b));
rmdir(d, 's');
%=============================================================================
function r = loop_mldivide(a, b)
  r = [];
  for k = 1:2
    r = a \ b;
  end
end
%=============================================================================
function r = loop_mrdivide(a, b)
  r = [];
  for k = 1:2
    r = a / b;
  end
end
%=============================================================================
function msg = operator_error(name, a, b)
  msg = '';
  try
    feval(name, a, b);
  catch e
    msg = e.message;
  end
end
%=============================================================================
