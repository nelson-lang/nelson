%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: append loops on a real double vector and on a cell. v(k) = x,
% v(end + 1) = x and the preallocated store all run as one typed SCALAR_EXPR
% window and growth uses the amortized vectorResize capacity, so every form
% must stay close to the preallocated one (the generic subset assignment was
% ~1.2 us/append). Cell growth pays one empty element construction per spare
% capacity slot on top of that.
N = 200000;
%=============================================================================
v = zeros(1, N);
tic();
for k = 1:N
  v(k) = k;
end
t_prealloc = toc();
asserts.isequal(v, 1:N);
fprintf('  preallocated v(k) = k        : %8.1f ns/op\n', t_prealloc / N * 1e9);
%=============================================================================
v = [];
tic();
for k = 1:N
  v(k) = k * 2; %#ok<NLS0039>
end
t_grow = toc();
asserts.isequal(v, 2 * (1:N));
fprintf('  growth v(k) = k * 2          : %8.1f ns/op\n', t_grow / N * 1e9);
%=============================================================================
v = [];
tic();
for k = 1:N
  v(end + 1) = k; %#ok<NLS0039>
end
t_grow_end = toc();
asserts.isequal(v, 1:N);
fprintf('  growth v(end + 1) = k        : %8.1f ns/op\n', t_grow_end / N * 1e9);
%=============================================================================
c = [1; 2];
tic();
for k = 3:N
  c(k) = k; %#ok<NLS0039>
end
t_grow_col = toc();
asserts.isequal(c, (1:N)');
fprintf('  growth column c(k) = k       : %8.1f ns/op\n', t_grow_col / N * 1e9);
%=============================================================================
ce = {};
tic();
for k = 1:N
  ce{k} = k; %#ok<NLS0039>
end
t_grow_cell = toc();
asserts.isequal(size(ce), [1 N]);
asserts.isequal(ce{N}, N);
fprintf('  growth cell ce{k} = k        : %8.1f ns/op\n', t_grow_cell / N * 1e9);
%=============================================================================
ce = {};
tic();
for k = 1:N
  ce{end + 1} = k; %#ok<NLS0039>
end
t_grow_cell_end = toc();
asserts.isequal(size(ce), [1 N]);
asserts.isequal(ce{N}, N);
fprintf('  growth cell ce{end + 1} = k  : %8.1f ns/op\n', t_grow_cell_end / N * 1e9);
%=============================================================================
v = [];
tic();
for k = 1:N
  v = [v k]; %#ok<NLS0039>
end
t_concat_row = toc();
asserts.isequal(v, 1:N);
fprintf('  growth concat v = [v k]      : %8.1f ns/op\n', t_concat_row / N * 1e9);
%=============================================================================
s = '';
tic();
for k = 1:N %#ok<NLS0039>
  s = [s 'a']; %#ok<NLS0039>
end
t_concat_text = toc();
asserts.isequal(numel(s), N);
fprintf('  growth concat s = [s ''a'']    : %8.1f ns/op\n', t_concat_text / N * 1e9);
%=============================================================================
ce = {};
tic();
for k = 1:N %#ok<NLS0039>
  ce = [ce {k}]; %#ok<NLS0039>
end
t_concat_cell = toc();
asserts.isequal(size(ce), [1 N]);
asserts.isequal(ce{N}, N);
fprintf('  growth concat ce = [ce {k}]  : %8.1f ns/op\n', t_concat_cell / N * 1e9);
%=============================================================================
