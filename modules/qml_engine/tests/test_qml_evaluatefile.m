%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--WITH DISPLAY-->
% linux with qt < 5.4 does not work
%=============================================================================
asserts.isequal(nargin('qml_evaluatefile'), 1);
asserts.isequal(nargout('qml_evaluatefile'), 1);
%=============================================================================
if ismac() || ispc()
  js_file = [modulepath('qml_engine', 'tests'), '/example_no_output.js'];
  r = qml_evaluatefile(js_file);
  asserts.isequal(r, []);
  %=============================================================================
  js_file = [modulepath('qml_engine', 'tests'), '/example_with_output.js'];
  r = qml_evaluatefile(js_file);
  asserts.isequal(r, 56);
  %=============================================================================
end
