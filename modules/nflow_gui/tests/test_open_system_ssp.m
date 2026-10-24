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
% open_system is an M wrapper that routes an SSP archive (.ssp) through
% NFlow.sspImport before opening it in the editor; models, model files and
% handles pass straight through to the low-level __open_system__ builtin. This
% test checks the routing is installed without opening the editor (which needs a
% GUI): the wrapper is on the path, the builtin exists, and a missing .ssp is
% not imported but passed through to the builtin's own file check.
%=============================================================================
% The low-level opener is a builtin; the routing wrapper is an M function.
assert_isequal(exist('__open_system__'), 5);
w = strrep(which('open_system'), '\', '/');
assert_istrue(~isempty(w));
assert_istrue(~isempty(strfind(w, 'nflow_gui/functions/open_system.m')));
%=============================================================================
% A missing .ssp is not imported (isfile is false), so the argument passes
% through unchanged and the builtin reports the missing file. This exercises the
% wrapper's routing branch without launching the editor.
msg = '';
try
  open_system([tempname(), '.ssp']);
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'does not exist')));
%=============================================================================
% narginchk: open_system requires exactly one argument.
msg2 = '';
try
  open_system();
catch e
  msg2 = e.message;
end
assert_istrue(~isempty(msg2));
%=============================================================================
