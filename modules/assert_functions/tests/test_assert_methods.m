%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.istrue'), -1);
asserts.isequal(nargout('asserts.istrue'), 2);
asserts.isequal(nargin('asserts.isfalse'), -1);
asserts.isequal(nargout('asserts.isfalse'), 2);
asserts.isequal(nargin('asserts.checkerror'), -2);
asserts.isequal(nargout('asserts.checkerror'), 2);
asserts.istrue(isbuiltin('asserts.istrue'));
%=============================================================================
asserts.istrue(true);
asserts.isfalse(false);
%=============================================================================
[r, msg] = asserts.istrue(false, 'custom true message');
asserts.isfalse(r);
asserts.isequal(msg, 'custom true message');
raised = false;
try
  asserts.istrue(false, 'direct true message');
catch ME
  raised = true;
  asserts.isequal(ME.message, 'direct true message');
end
asserts.istrue(raised);
%=============================================================================
[r, msg] = asserts.isfalse(true, 'custom false message');
asserts.isfalse(r);
asserts.isequal(msg, 'custom false message');
%=============================================================================
asserts.checkerror('cos', _('Not enough input arguments.'));
[r, msg] = asserts.checkerror('1 + 1', _('unused'));
asserts.isfalse(r);
asserts.isequal(msg, _('No error was produced while evaluating command.'));
%=============================================================================
