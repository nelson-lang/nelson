%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% well conditioned diagonal: plain quotients, no warning
lastwarn('');
R = diag([3 4]) \ [1; 1];
asserts.isequal(R, [1/3; 1/4]);
[W, ID] = lastwarn();
asserts.isequal(ID, '');
R = [1 1] / diag([3 4]);
asserts.isequal(R, [1/3, 1/4]);
[W, ID] = lastwarn();
asserts.isequal(ID, '');
%=============================================================================
D = diag(1:6);
B = reshape(1:18, 6, 3);
asserts.isequal(D \ B, B ./ (1:6)');
asserts.isequal(B' / D, B' ./ (1:6));
%=============================================================================
R = diag([2+1i, 3-1i]) \ [1; 1i];
asserts.isequal(R, [1 / (2+1i); 1i / (3-1i)]);
R = diag([2+0i, 4]) \ [1; 1];
asserts.isequal(R, [0.5; 0.25]);
asserts.istrue(isreal(R));
%=============================================================================
R = single(diag([2 4])) \ [1; 1];
asserts.isequal(R, single([0.5; 0.25]));
R = diag([2 4]) \ single([1; 1]);
asserts.isequal(R, single([0.5; 0.25]));
%=============================================================================
% badly scaled diagonal: diagonal ratio as RCOND
lastwarn('');
R = diag([1 1e-20]) \ [1; 1];
asserts.isequal(R, [1; 1e20]);
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:nearlySingularMatrix');
lastwarn('');
R = [1 1] / diag([1 1e-20]);
asserts.isequal(R, [1, 1e20]);
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:nearlySingularMatrix');
lastwarn('');
R = single(diag([1 1e-9])) \ single([1; 1]);
asserts.isequal(R, single([1; 1e9]));
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:nearlySingularMatrix');
%=============================================================================
lastwarn('');
R = diag([2 NaN]) \ [1; 1];
asserts.isequal(R(1), 0.5);
asserts.istrue(isnan(R(2)));
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:illConditionedMatrix');
lastwarn('');
R = [1 1] / diag([2 NaN]);
asserts.istrue(all(isnan(R)));
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:illConditionedMatrix');
%=============================================================================
lastwarn('');
R = diag([2 Inf]) \ [1; 1];
asserts.isequal(R, [0.5; 0]);
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:singularMatrix');
lastwarn('');
R = diag([0 2]) \ [1; 1];
asserts.isequal(R(1), Inf);
asserts.istrue(isnan(R(2)));
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:singularMatrix');
%=============================================================================
% Inf or NaN quotients spread as in a triangular substitution
lastwarn('');
R = diag([1 2]) \ [Inf; 1];
asserts.isequal(R(1), Inf);
asserts.istrue(isnan(R(2)));
R = diag([1 2 3]) \ [1; NaN; 1];
asserts.isequal(R(1), 1);
asserts.istrue(all(isnan(R(2:3))));
R = [Inf 1] / diag([1 2]);
asserts.isequal(R, [Inf, 0.5]);
R = [1 1 NaN] / diag([1 2 3]);
asserts.istrue(all(isnan(R)));
R = diag([0.5 1]) \ [1.5e308; 1];
asserts.isequal(R(1), Inf);
asserts.istrue(isnan(R(2)));
[W, ID] = lastwarn();
asserts.isequal(ID, '');
%=============================================================================
% B / D: each column of B divided by its diagonal entry
D = diag([2 4 8]);
B = [1 2 3; 4 5 6];
asserts.isequal(B / D, B ./ [2 4 8]);
asserts.isequal([1 2 3] / D, [1 2 3] ./ [2 4 8]);
R = single(B) / single(D);
asserts.isequal(R, single(B) ./ single([2 4 8]));
asserts.isequal(class(R), 'single');
%=============================================================================
dc = [2+1i, 3-1i, 0.5i];
Bc = [1, 1i, 2-1i; 1-1i, 3, 1i];
asserts.isequal(Bc / diag(dc), Bc ./ dc);
asserts.isequal(Bc(1, :) / diag(dc), Bc(1, :) ./ dc);
asserts.isequal(single(Bc) / single(diag(dc)), single(Bc) ./ single(dc));
R = [1 2] / diag([2+0i, 4]);
asserts.isequal(R, [0.5, 0.5]);
asserts.istrue(isreal(R));
%=============================================================================
lastwarn('');
d = 2 .^ mod(0:199, 9);
B = reshape(1:600, 3, 200);
asserts.isequal(B / diag(d), B ./ d);
d = 1:200;
asserts.isequal(B(2, :) / diag(d), B(2, :) ./ d);
[W, ID] = lastwarn();
asserts.isequal(ID, '');
%=============================================================================
lastwarn('');
R = [1 1; 2 2] / diag([0 1]);
asserts.isequal(R, [Inf 1; Inf 2]);
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:singularMatrix');
lastwarn('');
R = [1 2] / diag([-0 0]);
asserts.istrue(isnan(R(1)));
asserts.isequal(R(2), Inf);
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:singularMatrix');
lastwarn('');
R = [1 1] / diag([1 1e-17]);
asserts.isequal(R, [1, 1e17]);
[W, ID] = lastwarn();
asserts.isequal(ID, 'Nelson:nearlySingularMatrix');
lastwarn('');
R = [1 2; Inf 1] / diag([1 2]);
asserts.isequal(R, [1 1; Inf 0.5]);
R = [1 NaN 2] / diag([1 2 3]);
asserts.istrue(all(isnan(R(1:2))));
asserts.isequal(R(3), 2 / 3);
R = [1e308 1] / diag([0.5 1]);
asserts.isequal(R, [Inf 1]);
[W, ID] = lastwarn();
asserts.isequal(ID, '');
%=============================================================================
