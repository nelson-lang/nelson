%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'clearfun on a static builtin cannot be isolated in the portable inline runner.');
asserts.isequal(nargin('clearfun'), 1);
asserts.isequal(nargout('clearfun'), 1);
%=============================================================================
asserts.istrue(isbuiltin('cos'));
r = clearfun('cos');
asserts.istrue(r);
asserts.isfalse(isbuiltin('cos'));
%=============================================================================
