%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~ismodule('dynamic_link'), 'dynamic_link module unavailable');
%=============================================================================
includes = dlgetnflowincludes();
asserts.istrue(iscellstr(includes));
asserts.istrue(length(includes) > 0);
asserts.istrue(any(contains(includes, 'nflow_blocks')));
%=============================================================================
