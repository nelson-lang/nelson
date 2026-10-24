%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
x = ClassdefOverloadDispatch([1, NaN, 3]);
asserts.isequal(isnan(x), [false, true, false]);
printedIsnan = evalc('isnan(x)');
asserts.istrue(contains(printedIsnan, 'ans ='));
asserts.istrue(contains(printedIsnan, 'true'));
%=============================================================================
