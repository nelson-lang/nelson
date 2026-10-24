%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('assert'), -1);
asserts.isequal(nargout('assert'), 2);
%=============================================================================
assert(true);
assert(1);
assert([true true]);
r = assert(true);
asserts.istrue(r);
[r, msg] = assert(false);
asserts.isfalse(r);
asserts.istrue(ischar(msg));
asserts.istrue(~isempty(msg));
%=============================================================================
[r, msg] = assert(false, _('your error message'));
asserts.isfalse(r);
asserts.isequal(msg, _('your error message'));
%=============================================================================
[r, msg] = assert(false, 'value %s', 'bad');
asserts.isfalse(r);
asserts.isequal(msg, 'value bad');
[r, msg] = assert(false, 'value %.2f', 1.234);
asserts.isfalse(r);
asserts.isequal(msg, 'value 1.23');
%=============================================================================
asserts.checkerror('assert()', _('Not enough input arguments.'));
asserts.checkerror('assert(false, ''custom failure'')', 'custom failure');
asserts.checkerror('assert(false, ''custom failure'')', 'custom failure', 'Nelson:asserts:assert');
%=============================================================================
raised = false;
try
  assert(false, 'Nelson:asserts:test', 'bad %d', 7);
catch ME
  raised = true;
  asserts.isequal(ME.identifier, 'Nelson:asserts:test');
  asserts.isequal(ME.message, 'bad 7');
end
asserts.istrue(raised);
%=============================================================================
% a passing assert ignores its message arguments entirely (no formatting)
assert(true, 'no percent here', 'unused extra arg');
assert(true, 'SMTB:pkg.Cls:id', 'a message with (parens) and no percent');
[tf, msg] = assert(true, 'no percent', 'extra');
asserts.istrue(tf);
asserts.isequal(msg, '');
%=============================================================================
