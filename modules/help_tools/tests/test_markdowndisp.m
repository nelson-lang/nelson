%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
asserts.isequal(nargin('markdowndisp'), 1);
asserts.isequal(nargout('markdowndisp'), 0);
%=============================================================================
mode = getnelsonmode();
if ~contains(mode, {'GUI', 'WEB_GUI', 'WASM_TERMINAL'})
  txt = evalc('markdowndisp(''**bold**'')');
  asserts.istrue(contains(txt, '**bold**'));
end
%=============================================================================
asserts.checkerror('markdowndisp(1)', _('Input argument must be a string scalar or row character vector.'));
asserts.checkerror('markdowndisp(1)', _('Input argument must be a string scalar or row character vector.'), 'Nelson:markdowndisp:invalidInput');
asserts.checkerror('markdowndisp([''a''; ''b''])', _('Input argument must be a string scalar or row character vector.'));
%=============================================================================
if contains(mode, {'GUI', 'WEB_GUI', 'WASM_TERMINAL'})
  asserts.checkerror('markdowndisp([''a'', char(8)])', _('Backspace character is not supported.'));
  asserts.checkerror('markdowndisp([''a'', char(8)])', _('Backspace character is not supported.'), 'Nelson:markdowndisp:backspaceNotSupported');
end
%=============================================================================
