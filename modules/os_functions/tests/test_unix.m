%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), 'Requires a native shell and process creation, unavailable in WebAssembly.');
asserts.isequal(nargin('unix'), 1);
asserts.isequal(nargout('unix'), 2);
%=============================================================================
if ismac()
  [a, b] = unix('ls');
else
  [a, b] = unix('dir');
end
asserts.isequal(a, 0);
%=============================================================================
