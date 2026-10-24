%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = dictionary(["alpha", "beta"], [1 2]);
txt = evalc('disp(d)');
%=============================================================================
asserts.istrue(contains(txt, 'dictionary'));
asserts.istrue(contains(txt, 'alpha'));
asserts.istrue(contains(txt, 'beta'));
%=============================================================================
