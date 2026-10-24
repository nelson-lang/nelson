%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.contains'), 2);
asserts.isequal(nargout('asserts.contains'), 2);
asserts.isequal(nargin('asserts.match'), 2);
asserts.isequal(nargout('asserts.match'), 2);
asserts.isequal(nargin('asserts.type'), 2);
asserts.isequal(nargout('asserts.type'), 2);
asserts.isequal(nargin('asserts.scalar'), 1);
asserts.isequal(nargout('asserts.scalar'), 2);
asserts.isequal(nargin('asserts.vector'), 1);
asserts.isequal(nargout('asserts.vector'), 2);
asserts.isequal(nargin('asserts.matrix'), 1);
asserts.isequal(nargout('asserts.matrix'), 2);
asserts.isequal(nargin('asserts.real'), 1);
asserts.isequal(nargout('asserts.real'), 2);
asserts.isequal(nargin('asserts.finite'), 1);
asserts.isequal(nargout('asserts.finite'), 2);
asserts.isequal(nargin('asserts.nonNan'), 1);
asserts.isequal(nargout('asserts.nonNan'), 2);
asserts.isequal(nargin('asserts.sameSize'), 2);
asserts.isequal(nargout('asserts.sameSize'), 2);
asserts.isequal(nargin('asserts.numel'), 2);
asserts.isequal(nargout('asserts.numel'), 2);
asserts.isequal(nargin('asserts.throws'), -2);
asserts.isequal(nargout('asserts.throws'), 2);
asserts.isequal(nargin('asserts.warning'), -2);
asserts.isequal(nargout('asserts.warning'), 2);
asserts.isequal(nargin('asserts.warningFree'), 1);
asserts.isequal(nargout('asserts.warningFree'), 2);
asserts.isequal(nargin('asserts.diff'), 2);
asserts.isequal(nargout('asserts.diff'), 2);
%=============================================================================
asserts.contains('Nelson language', 'language');
[r, msg] = asserts.contains('Nelson language', 'toolbox');
asserts.isfalse(r);
asserts.istrue(contains(msg, 'does not contain'));
%=============================================================================
asserts.match('abc123', '^abc[0-9]+$');
[r, msg] = asserts.match('abc', '[0-9]+');
asserts.isfalse(r);
asserts.istrue(contains(msg, 'does not match'));
asserts.checkerror('feval(''asserts.match'', ''abc'', ''['')', _('Invalid regular expression.'));
%=============================================================================
asserts.type(1, 'double');
asserts.type(single(1), {'double', 'single'});
[r, msg] = asserts.type(int32(1), {'double', 'single'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected class one of'));
asserts.istrue(contains(msg, 'int32'));
%=============================================================================
asserts.scalar(1);
[r, msg] = asserts.scalar([1 2]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected scalar'));
%=============================================================================
asserts.vector([1 2]);
[r, msg] = asserts.vector(ones(2, 2));
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected vector'));
%=============================================================================
asserts.matrix(ones(2, 2));
%=============================================================================
asserts.real([1 2]);
asserts.finite([1 2 3]);
[r, msg] = asserts.finite([1 Inf]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'finite'));
%=============================================================================
asserts.nonNan([1 Inf]);
[r, msg] = asserts.nonNan([1 NaN]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'NaN'));
%=============================================================================
asserts.sameSize(ones(2, 3), zeros(2, 3));
[r, msg] = asserts.sameSize(ones(2, 3), zeros(3, 2));
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected same size'));
%=============================================================================
asserts.numel(ones(2, 3), 6);
[r, msg] = asserts.numel(ones(2, 3), 5);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected 5 elements'));
asserts.checkerror('feval(''asserts.numel'', ones(2, 3), NaN)', _('Expected number of elements must be a nonnegative finite integer scalar.'));
%=============================================================================
asserts.throws('cos', _('Not enough input arguments.'));
[r, msg] = asserts.throws('1 + 1', _('Not enough input arguments.'));
asserts.isfalse(r);
asserts.isequal(msg, _('No error was produced while evaluating command.'));
%=============================================================================
stBegin = warning();
warning('on');
warning('on', 'Nelson:asserts:test');
asserts.warningFree('1 + 1');
[r, msg] = asserts.warningFree('warning(''Nelson:asserts:test'', ''expected warning'');');
asserts.isfalse(r);
asserts.istrue(contains(msg, _('Unexpected warning was produced while evaluating command:')));
warning(stBegin);
%=============================================================================
msg = asserts.diff([1 2], [1 3]);
asserts.istrue(contains(msg, 'class double'));
asserts.istrue(contains(msg, 'First different index'));
[r, msg] = asserts.diff([1 2], [1 2]);
asserts.istrue(r);
asserts.isequal(msg, '');
%=============================================================================
