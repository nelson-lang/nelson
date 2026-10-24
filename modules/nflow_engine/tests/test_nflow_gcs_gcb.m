%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Current editing context: gcs (current system) and gcb (current
% block). new_system/load_system set the current system; closing it clears the
% context; with no system active the result is the empty string.
%=============================================================================
bdclose('all');
%=============================================================================
% A freshly created system is the current one; no block is selected yet.
new_system('ctxa');
assert_isequal(gcs(), 'ctxa');
assert_isequal(gcb(), '');
%=============================================================================
% Creating another system moves the current context to it.
new_system('ctxb');
assert_isequal(gcs(), 'ctxb');
%=============================================================================
% Loading a system from disk makes it current.
add_block('nflow/math/gain', 'ctxb/G');
f = [tempname(), '.nflow'];
save_system('ctxb', f);
bdclose('ctxb');
load_system(f);
assert_isequal(gcs(), 'ctxb');
%=============================================================================
% Closing the current system clears the context (no stale value).
bdclose('ctxb');
assert_isequal(gcs(), '');
assert_isequal(gcb(), '');
%=============================================================================
bdclose('all');
rmfile(f);
%=============================================================================
