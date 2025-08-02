%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/134
% <-- Short Description -->
% Evaluation of Non-Scalar If-Condition Expression was not managed.
%=============================================================================
R = [];
if [false, false], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [true, false], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [false, true], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [true, true], R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
if [], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
if ones(3,0), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([false]), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([true]), R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
R = [];
if sparse([false, false]), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([true, false]), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([true, true]), R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
R = [];
if [true, false, true], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [false, true, false], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if eps, R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
R = [];
cmd = 'if sparse([0+i, 0+i]), R = true; else, R = false; end';
assert_checkerror(cmd, _('Complex cannot be converted to logical.'));
%=============================================================================
R = [];
cmd = 'if [0+i, 0+i], R = true; else, R = false; end';
assert_checkerror(cmd, _('Complex cannot be converted to logical.'));
%=============================================================================

