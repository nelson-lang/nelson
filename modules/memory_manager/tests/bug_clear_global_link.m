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
% 'clear g' on a linked global from the base scope destroyed the global value
%=============================================================================
global bug_clear_link_g
bug_clear_link_g = 5;
clear bug_clear_link_g
% the link is removed but the global value survives
global bug_clear_link_g
asserts.isequal(bug_clear_link_g, 5);
%=============================================================================
% 'clear global g' destroys the global value
clear global bug_clear_link_g
global bug_clear_link_g
asserts.istrue(isempty(bug_clear_link_g));
clear global bug_clear_link_g
%=============================================================================
% 'clear' (all) still removes local variables
bug_clear_all_a = 1;
bug_clear_all_b = 2;
clear
asserts.isfalse(isvar('bug_clear_all_a'));
asserts.isfalse(isvar('bug_clear_all_b'));
%=============================================================================
