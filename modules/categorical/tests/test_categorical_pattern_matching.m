%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Cause = categorical({'winter storm'; 'equipment fault'; 'thunder Storm'; ''; 'severe storm'; 'fire'});
%=============================================================================
tf = contains(Cause, "storm");
assert_isequal(tf, [true; false; false; false; true; false]);
tf = contains(Cause, "storm", 'IgnoreCase', true);
assert_isequal(tf, [true; false; true; false; true; false]);
tf = contains(Cause, {'fault', 'fire'});
assert_isequal(tf, [false; true; false; false; false; true]);
tf = contains(Cause, "");
assert_isequal(tf, [true; true; true; false; true; true]);
%=============================================================================
tf = startsWith(Cause, 'thunder');
assert_isequal(tf, [false; false; true; false; false; false]);
tf = startsWith(Cause, ["WINTER", "Fire"], 'IgnoreCase', true);
assert_isequal(tf, [true; false; false; false; false; true]);
%=============================================================================
tf = endsWith(Cause, 'storm');
assert_isequal(tf, [true; false; false; false; true; false]);
tf = endsWith(Cause, pattern('orm'));
assert_isequal(tf, [true; false; true; false; true; false]);
%=============================================================================
tf = matches(Cause, 'fire');
assert_isequal(tf, [false; false; false; false; false; true]);
tf = matches(Cause, {'FIRE', 'severe storm'}, 'IgnoreCase', true);
assert_isequal(tf, [false; false; false; false; true; true]);
tf = matches(Cause, 'storm');
assert_isequal(tf, false(6, 1));
%=============================================================================
C = categorical({'red', 'blue'; '', 'Red'});
tf = contains(C, 'e');
assert_isequal(tf, [true, true; false, true]);
assert_isequal(class(tf), 'logical');
tf = startsWith(C, 'r', 'IgnoreCase', true);
assert_isequal(tf, [true, false; false, true]);
%=============================================================================
C = categorical({'a', 'b', 'c'}, {'a', 'b', 'c', 'unused'});
C = C([3, 1]);
assert_isequal(matches(C, 'a'), [false, true]);
%=============================================================================
C = categorical({'b'; 'a'}, 'Ordinal', true);
assert_isequal(contains(C, 'a'), [false; true]);
%=============================================================================
tf = contains(categorical({}), 'a');
assert_isequal(size(tf), [0, 0]);
assert_isequal(class(tf), 'logical');
C = categorical({''; ''});
assert_isequal(contains(C, ''), [false; false]);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'contains');
assert_checkerror('contains(Cause, categorical({''storm''}))', msg);
assert_checkerror('contains(Cause, categorical({''storm''}))', msg, 'Nelson:UndefinedFunction:incorrectArgumentType');
%=============================================================================
