%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Default-constructed empties share one immutable storage block: every write
% must detach, and no read may change the shape seen by the other empties.
%=============================================================================
a = [];
b = [];
a(2) = 1;
assert_isequal(a, [0 1]);
assert_isequal(b, []);
assert_isequal(size(b), [0 0]);
%=============================================================================
x = [];
y = x;
x(1) = 1;
assert_isequal(y, []);
assert_isequal(x, 1);
%=============================================================================
% shape queries beyond the rank are reads
x = [];
assert_isequal(size(x, 3), 1);
assert_isequal(size(x, 5), 1);
assert_isequal(ndims(x), 2);
assert_isequal(size([]), [0 0]);
assert_isequal(ndims([]), 2);
%=============================================================================
% cell elements start as independent empties
c = cell(1, 3);
c{2} = 1;
assert_isequal(c{1}, []);
assert_isequal(c{3}, []);
c{1}(2) = 7;
assert_isequal(c{1}, [0 7]);
assert_isequal(c{3}, []);
d = cell(2, 2);
assert_isequal(size(d{1}), [0 0]);
assert_isequal(class(d{4}), 'double');
d{4}.f = 1;
assert_istrue(isstruct(d{4}));
assert_isequal(d{1}, []);
%=============================================================================
% struct fields and struct arrays
s = struct();
s.a = 1;
t = struct();
assert_isequal(fieldnames(t), cell(0, 1));
u = struct('a', {});
u(2).a = 5;
assert_isequal(u(1).a, []);
assert_isequal(u(2).a, 5);
w = struct('p', [], 'q', []);
w.p(3) = 1;
assert_isequal(w.q, []);
assert_isequal(w.p, [0 0 1]);
%=============================================================================
% class promotion, reshape, transpose, sparse and struct promotion of one
% empty leave the others untouched
e = [];
e(1) = int8(3);
assert_isequal(class(e), 'int8');
assert_isequal(class([]), 'double');
e = [];
e = reshape(e, 0, 1);
assert_isequal(size(e), [0 1]);
assert_isequal(size([]), [0 0]);
e = [];
e = e';
assert_isequal(size(e), [0 0]);
e = [];
sp = sparse(e);
assert_istrue(issparse(sp));
assert_isfalse(issparse([]));
e = [];
e.x = 1;
assert_istrue(isstruct(e));
assert_isfalse(isstruct([]));
e = [];
e(3) = 'a';
assert_isequal(class(e), 'char');
assert_isequal(class([]), 'double');
%=============================================================================
% growth from a fresh empty
m = [];
m(2, 3) = 1;
assert_isequal(size(m), [2 3]);
assert_isequal(size([]), [0 0]);
for k = 1:50
  v = [];
  v(k) = k; %#ok<NLS0039>
  assert_isequal(numel(v), k);
end
assert_isequal(size([]), [0 0]);
%=============================================================================
% operators on empties
r = [] / [];
assert_isequal(size(r), [0 0]);
assert_isequal(size([] + []), [0 0]);
assert_isequal(size([] * []), [0 0]);
assert_istrue(isempty([]));
assert_istrue(isreal([]));
assert_isequal(numel([]), 0);
assert_isequal(zeros(0, 0), []);
assert_isequal(size([]), [0 0]);
%=============================================================================
% empties produced by functions are independent too
p = fresh_empty();
q = fresh_empty();
p(2) = 2;
assert_isequal(q, []);
assert_isequal(fresh_empty(), []);
%=============================================================================
function r = fresh_empty()
  r = [];
end
%=============================================================================
