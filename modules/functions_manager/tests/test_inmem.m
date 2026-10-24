%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('inmem'), 0);
asserts.isequal(nargout('inmem'), 2);
%=============================================================================
[functionsList, mexList] = inmem();
asserts.istrue(iscell(functionsList));
asserts.istrue(iscell(mexList));
[functionsList, mexList] = inmem('-completenames');
asserts.istrue(iscell(functionsList));
asserts.istrue(iscell(mexList));
%=============================================================================
[functionsList, mexList] = inmem();
asserts.istrue(iscell(functionsList));
asserts.istrue(iscell(mexList));
[completeFunctionsList, completeMexList] = inmem('-completenames');
asserts.istrue(iscell(completeFunctionsList));
asserts.istrue(iscell(completeMexList));
%=============================================================================
