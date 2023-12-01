%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/176
% <-- Short Description -->
% nfilename did not return canonical path name in some cases.
%=============================================================================
cd([nelsonroot(), '/modules/core/tests']);
run('nfilename_#176.m');
%=============================================================================
REF1 = 'nfilename_#176';
assert_isequal(R1, REF1);
%=============================================================================
REF2 = [pwd(), '/', 'nfilename_#176'];
assert_isequal(R2, REF2);
%=============================================================================
REF3 = [pwd(), '/', 'nfilename_#176.m'];
assert_isequal(R3, REF3);
%=============================================================================
