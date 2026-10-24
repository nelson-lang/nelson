%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('getnelsonmode'), 0);
asserts.isequal(nargout('getnelsonmode'), 1);
%=============================================================================
mode = getnelsonmode();
asserts.isequal(class(mode), 'char');
asserts.istrue(~isempty(mode));
allowed_modes = {'BASIC_ENGINE', 'ADVANCED_ENGINE', 'BASIC_TERMINAL', ...
  'ADVANCED_TERMINAL', 'GUI', 'WEB_GUI', 'WASM_TERMINAL'};
asserts.istrue(ismember(mode, allowed_modes));
if iswasm()
  asserts.isequal(mode, 'WASM_TERMINAL');
end
%=============================================================================
