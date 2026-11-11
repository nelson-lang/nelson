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
% Overloads and functions loaded from a folder stop resolving after rmpath or cd.
%=============================================================================
d = fullfile(tempdir(), ['nelson_path_ovl_', num2str(round(rand() * 1e9))]);
mkdir(fullfile(d, '@double'));
write_function(fullfile(d, '@double', 'plus.m'), 'plus', '''ovl plus''');
write_function(fullfile(d, '@double', 'mldivide.m'), 'mldivide', '''ovl mldivide''');
write_function(fullfile(d, '@double', 'mrdivide.m'), 'mrdivide', '''ovl mrdivide''');
write_function(fullfile(d, 'path_ovl_plain_fn.m'), 'path_ovl_plain_fn', '7');
A = [2 1; 1 3];
%=============================================================================
addpath(d);
assert_isequal(A + A, 'ovl plus');
assert_isequal(loop_plus(A, A), 'ovl plus');
assert_isequal(A \ A, 'ovl mldivide');
assert_isequal(A / A, 'ovl mrdivide');
assert_isequal(path_ovl_plain_fn(), 7);
%=============================================================================
rmpath(d);
assert_isequal(A + A, 2 * A);
assert_isequal(loop_plus(A, A), 2 * A);
assert_isequal(class(A \ A), 'double');
assert_isequal(class(A / A), 'double');
assert_isfalse(exist('path_ovl_plain_fn') == 2);
assert_checkerror('path_ovl_plain_fn()', _('Undefined variable or function: path_ovl_plain_fn'));
%=============================================================================
addpath(d);
assert_isequal(loop_plus(A, A), 'ovl plus');
assert_isequal(path_ovl_plain_fn(), 7);
rmpath(d);
assert_isequal(loop_plus(A, A), 2 * A);
%=============================================================================
% a function or @class overload of the current folder resolves until we leave it
assert_isequal(A + A, 2 * A);
previous = cd(d);
assert_isequal(path_ovl_plain_fn(), 7);
assert_isequal(A + A, 'ovl plus');
assert_isequal(loop_plus(A, A), 'ovl plus');
assert_isequal(A \ A, 'ovl mldivide');
assert_isequal(plus(A, A), 'ovl plus');
cd(previous);
assert_isfalse(exist('path_ovl_plain_fn') == 2);
assert_isequal(A + A, 2 * A);
assert_isequal(loop_plus(A, A), 2 * A);
assert_isequal(class(A \ A), 'double');
%=============================================================================
% a second visit to the folder reuses its cached index
cd(d);
assert_isequal(A + A, 'ovl plus');
cd(previous);
assert_isequal(A + A, 2 * A);
%=============================================================================
rmdir(d, 's');
%=============================================================================
function r = loop_plus(a, b)
  r = [];
  for k = 1:3
    r = a + b;
  end
end
%=============================================================================
function write_function(filename, name, value)
  fid = fopen(filename, 'wt');
  fprintf(fid, 'function r = %s(varargin)\n  r = %s;\nend\n', name, value);
  fclose(fid);
end
%=============================================================================
