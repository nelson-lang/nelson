%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% whos('global') listed the current scope instead of the global scope
%=============================================================================
global bug_whos_g
bug_whos_g = int8(3);
bug_whos_x = 1;
s = whos('global');
names = {s.name};
asserts.istrue(any(strcmp(names, 'bug_whos_g')));
asserts.isfalse(any(strcmp(names, 'bug_whos_x')));
idx = find(strcmp(names, 'bug_whos_g'));
asserts.isequal(s(idx).class, 'int8');
clear global bug_whos_g
clear bug_whos_x
%=============================================================================
