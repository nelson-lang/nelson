%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Assigning a nested dotted field whose intermediate field does not yet exist
% must auto-vivify that intermediate as a struct:
%   s.newParent.child = value   ->   s.newParent == struct('child', value)
% The get-modify-set read of the (missing) intermediate must not raise
% "Reference to non-existent field". This regressed only when the right-hand
% side was a bare local variable, which routes through the dedicated
% ASGN_DOT_DOT_LOCAL opcode instead of the general chained-assignment path.
%=============================================================================
% New intermediate field, right-hand side is a local variable (the regression).
v = [1 2];
s = struct('domain', 7);
s.xaxis.range = v;
assert_istrue(isstruct(s.xaxis));
assert_isequal(s.xaxis.range, [1 2]);
assert_isequal(s.domain, 7);
%=============================================================================
% Same shape, right-hand side is a literal (general chained path).
s2 = struct('domain', 7);
s2.xaxis.range = [1 2];
assert_isequal(s2.xaxis.range, [1 2]);
assert_isequal(s2.domain, 7);
%=============================================================================
% Same shape, right-hand side is an expression (general chained path).
s3 = struct('domain', 7);
s3.xaxis.range = v + 1;
assert_isequal(s3.xaxis.range, [2 3]);
%=============================================================================
% Several new nested fields in a row keep the earlier ones intact.
xr = [0 1];
yr = [2 3];
zr = [4 5];
scene = struct('domain', 1);
scene.xaxis.range = xr;
scene.yaxis.range = yr;
scene.zaxis.range = zr;
assert_isequal(scene.xaxis.range, [0 1]);
assert_isequal(scene.yaxis.range, [2 3]);
assert_isequal(scene.zaxis.range, [4 5]);
assert_isequal(scene.domain, 1);
%=============================================================================
% Undefined base variable, local right-hand side: build the whole chain.
w = 5;
clear t
t.a.b = w;
assert_istrue(isstruct(t));
assert_istrue(isstruct(t.a));
assert_isequal(t.a.b, 5);
%=============================================================================
% Existing intermediate struct, new child, local right-hand side.
u.a.x = 1;
y = 2;
u.a.z = y;
assert_isequal(u.a.x, 1);
assert_isequal(u.a.z, 2);
%=============================================================================
% Existing intermediate struct, overwrite an existing child, local rhs.
u.a.x = y;
assert_isequal(u.a.x, 2);
%=============================================================================
% The struct-valued right-hand side one level up still works (single dot).
k = struct('range', v);
scene2 = struct('domain', 1);
scene2.xaxis = k;
assert_isequal(scene2.xaxis.range, [1 2]);
%=============================================================================
% Deeper chain (three levels) auto-vivifies both intermediates.
m = 9;
clear deep
deep.a.b.c = m;
assert_isequal(deep.a.b.c, 9);
%=============================================================================
% Dynamic intermediate field name auto-vivifies too.
fn = 'xaxis';
d = struct('domain', 1);
d.(fn).range = v;
assert_isequal(d.xaxis.range, [1 2]);
%=============================================================================
% Same auto-vivification inside a function body (the plotly toolbox context).
r = nested_assign_in_function([10 20]);
assert_istrue(isstruct(r.xaxis));
assert_isequal(r.xaxis.range, [10 20]);
assert_isequal(r.domain, 42);
%=============================================================================
% A nested sub-assignment onto an existing non-struct intermediate still errors.
assert_checkerror('bad = struct(''a'', 5); loc = 3; bad.a.b = loc;', ...
    _('Unable to perform assignment because dot indexing is not supported for variables of this type.'));
assert_checkerror('bad = struct(''a'', 5); loc = 3; bad.a.b = loc;', ...
    _('Unable to perform assignment because dot indexing is not supported for variables of this type.'), 'Nelson:structAssToNonStruct');
%=============================================================================
function out = nested_assign_in_function(rng)
  out = struct('domain', 42);
  local = rng;
  out.xaxis.range = local;
end
%=============================================================================
