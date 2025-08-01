%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = ["nel" "son"];
REF = [string('nel') string('son')];
assert_isequal(R, REF);
%=============================================================================
R1 = ["nel" "son"];
R2 = ["nel", "son"];
assert_isequal(R1, R2);
%=============================================================================
R = ["Nelson" "manages"; "string" "array"];
REF = strings(2, 2);
REF(1, 1) = 'Nelson';
REF(1, 2) = 'manages';
REF(2, 1) = 'string';
REF(2, 2) = 'array';
assert_isequal(R, REF);
%=============================================================================
R = { "nel" "son"};
REF = {string('nel') string('son')};
assert_isequal(R, REF);
%=============================================================================
